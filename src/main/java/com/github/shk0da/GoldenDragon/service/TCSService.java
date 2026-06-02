package com.github.shk0da.GoldenDragon.service;

import com.github.shk0da.GoldenDragon.config.MainConfig;
import com.github.shk0da.GoldenDragon.config.MarketConfig;
import com.github.shk0da.GoldenDragon.model.Position;
import com.github.shk0da.GoldenDragon.model.PositionInfo;
import com.github.shk0da.GoldenDragon.model.TickerInfo;
import com.github.shk0da.GoldenDragon.model.TickerType;
import com.github.shk0da.GoldenDragon.repository.FigiRepository;
import com.github.shk0da.GoldenDragon.repository.PricesRepository;
import com.github.shk0da.GoldenDragon.repository.Repository;
import com.github.shk0da.GoldenDragon.repository.TickerRepository;
import java.math.BigDecimal;
import java.time.Instant;
import java.time.OffsetDateTime;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;
import javax.annotation.Nullable;
import ru.tinkoff.piapi.contract.v1.Bond;
import ru.tinkoff.piapi.contract.v1.CandleInterval;
import ru.tinkoff.piapi.contract.v1.Currency;
import ru.tinkoff.piapi.contract.v1.Etf;
import ru.tinkoff.piapi.contract.v1.Future;
import ru.tinkoff.piapi.contract.v1.GetOrderBookResponse;
import ru.tinkoff.piapi.contract.v1.HistoricCandle;
import ru.tinkoff.piapi.contract.v1.Order;
import ru.tinkoff.piapi.contract.v1.OrderDirection;
import ru.tinkoff.piapi.contract.v1.OrderType;
import ru.tinkoff.piapi.contract.v1.PostOrderResponse;
import ru.tinkoff.piapi.contract.v1.Quotation;
import ru.tinkoff.piapi.contract.v1.Share;
import ru.tinkoff.piapi.contract.v1.StopOrderDirection;
import ru.tinkoff.piapi.core.InvestApi;
import ru.tinkoff.piapi.core.models.Money;
import ru.tinkoff.piapi.core.models.Portfolio;
import ru.tinkoff.piapi.core.models.Positions;


import static com.github.shk0da.GoldenDragon.config.MainConfig.dateTimeFormat;
import static com.github.shk0da.GoldenDragon.dictionary.CurrenciesDictionary.getTickerName;
import static com.github.shk0da.GoldenDragon.service.TelegramNotifyService.telegramNotifyService;
import static com.github.shk0da.GoldenDragon.utils.PrintUtils.printGlassOfPrices;
import static com.github.shk0da.GoldenDragon.utils.TimeUtils.sleep;
import static java.lang.Math.round;
import static java.lang.System.out;
import static java.util.stream.Collectors.toCollection;
import static ru.tinkoff.piapi.contract.v1.OrderDirection.ORDER_DIRECTION_BUY;
import static ru.tinkoff.piapi.contract.v1.OrderDirection.ORDER_DIRECTION_SELL;
import static ru.tinkoff.piapi.contract.v1.OrderExecutionReportStatus.EXECUTION_REPORT_STATUS_FILL;
import static ru.tinkoff.piapi.contract.v1.StopOrderDirection.STOP_ORDER_DIRECTION_BUY;
import static ru.tinkoff.piapi.contract.v1.StopOrderDirection.STOP_ORDER_DIRECTION_SELL;
import static ru.tinkoff.piapi.contract.v1.StopOrderType.STOP_ORDER_TYPE_STOP_LOSS;
import static ru.tinkoff.piapi.contract.v1.StopOrderType.STOP_ORDER_TYPE_TAKE_PROFIT;

public class TCSService {

    public static final double FUTURES_MARGIN_RATE = 0.40;

    private final MainConfig mainConfig;
    private final MarketConfig marketConfig;
    private final InvestApi investApi;

    private final Repository<TickerInfo.Key, String> figiRepository = FigiRepository.INSTANCE;
    private final Repository<TickerInfo.Key, TickerInfo> tickerRepository = TickerRepository.INSTANCE;
    private final Repository<TickerInfo.Key, Map<String, Map<Double, Integer>>> pricesRepository = PricesRepository.INSTANCE;
    private final Map<TickerInfo.Key, Double> lastExecutedPriceByTicker = new ConcurrentHashMap<>();
    private final Map<TickerInfo.Key, ProtectiveOrders> protectiveOrdersByTicker = new ConcurrentHashMap<>();

    public TCSService(MainConfig mainConfig, MarketConfig marketConfig) {
        this.mainConfig = mainConfig;
        this.marketConfig = marketConfig;
        this.investApi = mainConfig.isSandbox()
                ? InvestApi.createSandbox(mainConfig.getTcsApiKey())
                : InvestApi.create(mainConfig.getTcsApiKey());

        figiRepository.insert(new TickerInfo.Key("RUB", TickerType.CURRENCY), "RUB000UTSTOM");
        figiRepository.insert(new TickerInfo.Key("USD", TickerType.CURRENCY), "BBG0013HGFT4");
        figiRepository.insert(new TickerInfo.Key("EUR", TickerType.CURRENCY), "BBG0013HJJ31");
    }

    public List<Share> getMoexShares() {
        return investApi.getInstrumentsService().getTradableSharesSync()
                .stream()
                .filter(it -> it.getCurrency().equals("rub"))
                .collect(Collectors.toList());
    }

    public GetOrderBookResponse getOrderBook(String figi, int depth /*1, 10, 20, 30, 40, 50*/) {
        return investApi.getMarketDataService().getOrderBookSync(figi, depth);
    }

    public List<HistoricCandle> getCandles(String figi, Instant start, Instant end, CandleInterval interval) {
        return investApi.getMarketDataService().getCandlesSync(figi, start, end, interval);
    }

    public List<HistoricCandle> getCandles(String figi, OffsetDateTime start, OffsetDateTime end, CandleInterval interval) {
        return investApi.getMarketDataService().getCandlesSync(figi, start.toInstant(), end.toInstant(), interval);
    }

    public void closeAllByMarket(TickerType type) {
        getCurrentPositions(type).values().forEach(ticker -> {
            int count = ticker.getBalance();
            String name = ticker.getTicker();
            String currentDate = dateTimeFormat.format(new Date());
            if (count > 0) {
                var message = "[" + currentDate + "] Sell: " + count + " " + name + " by Market";
                out.println(message);
                if (mainConfig.isTestMode()) return;
                telegramNotifyService.sendMessage(message);
                createOrder(new TickerInfo.Key(name, type), 0.0, count, "Sell");
            }
            if (count < 0) {
                var message = "[" + currentDate + "] Buy: " + count + " " + name + " by Market";
                out.println(message);
                if (mainConfig.isTestMode()) return;
                telegramNotifyService.sendMessage(message);
                createOrder(new TickerInfo.Key(name, type), 0.0, Math.abs(count), "Buy");
            }
            sleep(1_000);
        });
    }

    public boolean closeByMarket(String name, TickerType type) {
        return closeShortByMarket(name, type) || closeLongByMarket(name, type);
    }

    public boolean closeShortByMarket(String name, TickerType type) {
        int count = getCountOfCurrentPositions(type, name);
        if (count < 0) {
            String currentDate = dateTimeFormat.format(new Date());
            out.println("[" + currentDate + "] Buy: " + Math.abs(count) + " " + name + " by Market");

            if (mainConfig.isTestMode()) return true;
            return 1 == createOrder(new TickerInfo.Key(name, type), 0.0, Math.abs(count), "Buy");
        }
        return false;
    }

    public OrderExecutionResult closeShortByMarketWithDetails(String name, TickerType type) {
        int count = getCountOfCurrentPositions(type, name);
        if (count < 0) {
            String currentDate = dateTimeFormat.format(new Date());
            out.println("[" + currentDate + "] Buy: " + Math.abs(count) + " " + name + " by Market");

            if (mainConfig.isTestMode()) {
                return OrderExecutionResult.testSuccess(getAvailablePrice(new TickerInfo.Key(name, type)), Math.abs(count));
            }
            return createOrder(new TickerInfo.Key(name, type), 0.0, Math.abs(count), "Buy", 0.0, 0.0, false);
        }
        return OrderExecutionResult.failed();
    }

    public boolean closeLongByMarket(String name, TickerType type) {
        int count = getCountOfCurrentPositions(type, name);
        if (count > 0) {
            String currentDate = dateTimeFormat.format(new Date());
            out.println("[" + currentDate + "] Sell: " + count + " " + name + " by Market");

            if (mainConfig.isTestMode()) return true;
            return 1 == createOrder(new TickerInfo.Key(name, type), 0.0, count, "Sell");
        }
        return false;
    }

    public OrderExecutionResult closeLongByMarketWithDetails(String name, TickerType type) {
        int count = getCountOfCurrentPositions(type, name);
        if (count > 0) {
            String currentDate = dateTimeFormat.format(new Date());
            out.println("[" + currentDate + "] Sell: " + count + " " + name + " by Market");

            if (mainConfig.isTestMode()) {
                return OrderExecutionResult.testSuccess(getAvailablePrice(new TickerInfo.Key(name, type)), count);
            }
            return createOrder(new TickerInfo.Key(name, type), 0.0, count, "Sell", 0.0, 0.0, false);
        }
        return OrderExecutionResult.failed();
    }

    public boolean sellByMarket(String name, TickerType type, double cashToSell, double takeProfit, double stopLose) {
        return sell(name, type, cashToSell, true, takeProfit, stopLose, false).isSuccess();
    }

    public boolean sellByMarket(String name, TickerType type, double cashToSell, double takeProfit, double stopLose, boolean isFullPrice) {
        return sell(name, type, cashToSell, true, takeProfit, stopLose, isFullPrice).isSuccess();
    }

    public OrderExecutionResult sellLimit(String name, TickerType type, double cashToSell, double limitPrice) {
        if (limitPrice <= 0.0) {
            return OrderExecutionResult.failed();
        }

        var key = new TickerInfo.Key(name, type);
        String basicCurrency = marketConfig.getCurrency();
        String currency = searchTicker(key).getCurrency();
        if (!basicCurrency.equals(currency)) {
            cashToSell = convertCurrencies(currency, basicCurrency, cashToSell);
        }

        int count = calculateTradeCount(key, cashToSell, limitPrice);
        if (count == 0) {
            out.println("Warn: limit short will be skipped - " + name + " with count " + count
                    + ". CashToSell: " + cashToSell + ", price: " + limitPrice);
            return OrderExecutionResult.failed();
        }

        double cost = getRequiredCashForOrder(key, count, limitPrice);
        String currentDate = dateTimeFormat.format(new Date());
        out.println("[" + currentDate + "] Sell: " + count + " " + key.getTicker() + " by "
                + limitPrice + " (" + cost + " " + currency + ")");
        if (mainConfig.isTestMode()) {
            return OrderExecutionResult.testSuccess(limitPrice, count);
        }
        return createOrder(key, limitPrice, count, "Sell", 0.0, 0.0, false);
    }

    public OrderExecutionResult sellByMarketWithDetails(String name,
                                                        TickerType type,
                                                        double cashToSell,
                                                        double takeProfit,
                                                        double stopLose) {
        return sell(name, type, cashToSell, true, takeProfit, stopLose, false);
    }

    public OrderExecutionResult sell(String name, TickerType type, double cashToSell, boolean byMarket, double takeProfit, double stopLose, boolean isFullPrice) {
        var key = new TickerInfo.Key(name, type);

        String basicCurrency = marketConfig.getCurrency();
        String currency = searchTicker(key).getCurrency();
        if (!basicCurrency.equals(currency)) {
            cashToSell = convertCurrencies(currency, basicCurrency, cashToSell);
        }

        int value = 0;
        double tickerPrice = 0.0;
        for (Map.Entry<Double, Integer> bid : getCurrentPrices(key, false).get("bids").entrySet()) {
            tickerPrice = bid.getKey();
            value = value + bid.getValue();
            if (value >= (cashToSell / tickerPrice)) break;
        }

        if (0.0 == tickerPrice) {
            out.println("Warn: short will be skipped - " + name + " by price " + tickerPrice);
            return OrderExecutionResult.failed();
        }

        int count = calculateTradeCount(key, cashToSell, tickerPrice);
        if (count == 0) {
            out.println("Warn: short will be skipped - " + name + " with count " + count + ". CashToSell: " + cashToSell + ", price: " + tickerPrice);
            return OrderExecutionResult.failed();
        }
        double cost = getRequiredCashForOrder(key, count, tickerPrice);

        String currentDate = dateTimeFormat.format(new Date());
        out.println("[" + currentDate + "] Sell: " + count + " " + key.getTicker() + " by " + (byMarket ? "Market" : tickerPrice + " (" + cost + " " + currency + ")"));
        if (mainConfig.isTestMode()) {
            return OrderExecutionResult.testSuccess(tickerPrice, count);
        }
        return createOrder(key, byMarket ? 0.0 : tickerPrice, count, "Sell", takeProfit, stopLose, isFullPrice);
    }

    public boolean sell(String name, TickerType type, double cost) {
        if (cost == 0) {
            out.println("Warn: sale will be skipped - " + name + " with cost " + cost);
            return false;
        }

        var key = new TickerInfo.Key(name, type);

        String basicCurrency = marketConfig.getCurrency();
        String currency = searchTicker(key).getCurrency();
        if (!basicCurrency.equals(currency)) {
            cost = convertCurrencies(currency, basicCurrency, cost);
        }

        int count = 1;
        double availablePrice = getAvailablePrice(key);
        if (availablePrice < cost) {
            count = (int) round(cost / availablePrice);
            int lot = searchTicker(key).getLot();
            while (count % lot != 0 && count > 0) {
                count = count - 1;
            }
        }
        if (count == 0) {
            out.println("Warn: sale will be skipped - " + name + " with count " + count);
            return false;
        }

        double tickerPrice = getAvailablePrice(key, count, true);
        if (0.0 == tickerPrice) {
            out.println("Warn: sale will be used Market Price - " + name);
        }

        String currentDate = dateTimeFormat.format(new Date());
        out.println("[" + currentDate + "] Sell: " + count + " " + key.getTicker() + " by " + tickerPrice + " (" + cost + " " + currency + ")");
        if (mainConfig.isTestMode()) {
            return true;
        }
        return 1 == createOrder(key, tickerPrice, count, "Sell");
    }

    public boolean buyByMarket(String name, TickerType type, double cashToBuy, double takeProfit, double stopLose) {
        return buy(name, type, cashToBuy, true, takeProfit, stopLose, false).isSuccess();
    }

    public boolean buyByMarket(String name, TickerType type, double cashToBuy, double takeProfit, double stopLose, boolean isFullPrice) {
        return buy(name, type, cashToBuy, true, takeProfit, stopLose, isFullPrice).isSuccess();
    }

    public OrderExecutionResult buyByMarketWithDetails(String name,
                                                       TickerType type,
                                                       double cashToBuy,
                                                       double takeProfit,
                                                       double stopLose) {
        return buy(name, type, cashToBuy, true, takeProfit, stopLose, false);
    }

    public boolean buy(String name, TickerType type, double cashToBuy) {
        return buy(name, type, cashToBuy, false, 0.0, 0.0, false).isSuccess();
    }

    public boolean buy(String name, TickerType type, double cashToBuy, boolean isFullPrice) {
        return buy(name, type, cashToBuy, false, 0.0, 0.0, isFullPrice).isSuccess();
    }

    public OrderExecutionResult buyLimit(String name, TickerType type, double cashToBuy, double limitPrice) {
        if (limitPrice <= 0.0) {
            return OrderExecutionResult.failed();
        }

        var key = new TickerInfo.Key(name, type);
        String basicCurrency = marketConfig.getCurrency();
        String currency = searchTicker(key).getCurrency();
        if (!basicCurrency.equals(currency)) {
            cashToBuy = convertCurrencies(currency, basicCurrency, cashToBuy);
        }

        int count = calculateTradeCount(key, cashToBuy, limitPrice);
        if (count == 0) {
            out.println("Warn: limit long will be skipped - " + name + " with count " + count
                    + ". CashToBuy: " + cashToBuy + ", price: " + limitPrice);
            return OrderExecutionResult.failed();
        }

        double cost = getRequiredCashForOrder(key, count, limitPrice);
        String currentDate = dateTimeFormat.format(new Date());
        out.println("[" + currentDate + "] Buy: " + count + " " + key.getTicker() + " by "
                + limitPrice + " (" + cost + " " + currency + ")");
        if (mainConfig.isTestMode()) {
            return OrderExecutionResult.testSuccess(limitPrice, count);
        }
        return createOrder(key, limitPrice, count, "Buy", 0.0, 0.0, false);
    }

    public OrderExecutionResult buy(String name, TickerType type, double cashToBuy, boolean byMarket, double takeProfit, double stopLose, boolean isFullPrice) {
        var key = new TickerInfo.Key(name, type);

        String basicCurrency = marketConfig.getCurrency();
        String currency = searchTicker(key).getCurrency();
        if (!basicCurrency.equals(currency)) {
            cashToBuy = convertCurrencies(currency, basicCurrency, cashToBuy);
        }

        int value = 0;
        double tickerPrice = 0.0;
        for (Map.Entry<Double, Integer> ask : getCurrentPrices(key, false).get("asks").entrySet()) {
            tickerPrice = ask.getKey();
            value = value + ask.getValue();
            if (value >= (cashToBuy / tickerPrice)) break;
        }

        if (0.0 == tickerPrice) {
            out.println("Warn: purchase will be skipped - " + name + " by price " + tickerPrice);
            return OrderExecutionResult.failed();
        }

        int count = calculateTradeCount(key, cashToBuy, tickerPrice);
        if (count == 0) {
            out.println("Warn: long will be skipped - " + name + " with count " + count + ". CashToBuy: " + cashToBuy + ", price: " + tickerPrice);
            return OrderExecutionResult.failed();
        }
        double cost = getRequiredCashForOrder(key, count, tickerPrice);

        String currentDate = dateTimeFormat.format(new Date());
        out.println("[" + currentDate + "] Buy: " + count + " " + key.getTicker() + " by " + (byMarket ? "Market" : tickerPrice + " (" + cost + " " + currency + ")"));
        if (mainConfig.isTestMode()) {
            return OrderExecutionResult.testSuccess(tickerPrice, count);
        }
        return createOrder(key, byMarket ? 0.0 : tickerPrice, count, "Buy", takeProfit, stopLose, isFullPrice);
    }

    public int createOrder(TickerInfo.Key key, double price, int count, String operation) {
        return createOrder(key, price, count, operation, 0.0, 0.0, false).isSuccess() ? 1 : 0;
    }

    public OrderExecutionResult createOrder(TickerInfo.Key key, double price, int count, String operation, double takeProfit, double stopLose, boolean isFullPrice) {
        String figi = figiByName(key);
        TickerInfo tickerInfo = searchTicker(key);
        int lot = tickerInfo.getLot();
        int quantity = (count / lot);
        OrderDirection direction = "Buy".equals(operation) ? ORDER_DIRECTION_BUY : ORDER_DIRECTION_SELL;
        OrderType type = OrderType.ORDER_TYPE_MARKET;
        Quotation orderPrice = Quotation.newBuilder().build();
        if (price > 0.0) {
            type = OrderType.ORDER_TYPE_LIMIT;
            orderPrice = createQuotation(price);
        }

        try {
            PostOrderResponse response = investApi.getOrdersService().postOrderSync(
                    figi, quantity, orderPrice, direction, mainConfig.getTcsAccountId(), type, null
            );
            int executedLots = Math.toIntExact(response.getLotsExecuted());
            int executedCount = executedLots > 0 ? executedLots * lot : count;
            double executedPrice = toDouble(
                    response.getExecutedOrderPrice().getUnits(),
                    response.getExecutedOrderPrice().getNano()
            );
            if (executedPrice <= 0.0) {
                executedPrice = price > 0.0 ? price : getAvailablePrice(key);
            }
            lastExecutedPriceByTicker.put(key, executedPrice);

            String message = String.format(
                    "%s %d %s by %f (%f): %s [order=%s, status=%s, price=%f, commission=%f]\n",
                    operation,
                    executedCount,
                    key.getTicker(),
                    price,
                    executedCount * price,
                    response.getMessage(),
                    response.getOrderId(),
                    response.getExecutionReportStatus(),
                    executedPrice,
                    toDouble(response.getExecutedCommission().getUnits(), response.getExecutedCommission().getNano())
            );
            out.println(message);

            Position bracketPosition = null;
            if (response.getExecutionReportStatus().equals(EXECUTION_REPORT_STATUS_FILL)) {
                bracketPosition = createProtectivePosition(direction, executedPrice, stopLose, takeProfit, isFullPrice, executedCount, tickerInfo);
                placeOrLogProtectiveOrders(figi, executedLots > 0 ? executedLots : quantity, key, direction, bracketPosition);
            }

            telegramNotifyService.sendMessage(message, true);
            return OrderExecutionResult.success(executedPrice, executedCount, bracketPosition);
        } catch (Exception ex) {
            String message = "Failed create order [" + key.getTicker() + "]: " + ex.getMessage();
            out.println(message);
            telegramNotifyService.sendMessage(message);
            return OrderExecutionResult.failed();
        }
    }

    public Double getLastExecutedPrice(String name, TickerType type) {
        return lastExecutedPriceByTicker.get(new TickerInfo.Key(name, type));
    }

    public void syncProtectiveOrders(String name, TickerType type, Position position) {
        if (mainConfig.isSandbox() || position == null || position.quantity <= 0) {
            return;
        }

        TickerInfo.Key key = new TickerInfo.Key(name, type);
        TickerInfo tickerInfo = searchTicker(key);
        int lot = Math.max(1, tickerInfo.getLot());
        int quantity = Math.max(1, position.quantity / lot);
        String figi = figiByName(key);
        OrderDirection direction = "BUY".equals(position.direction) ? ORDER_DIRECTION_BUY : ORDER_DIRECTION_SELL;

        ProtectiveOrders currentOrders = protectiveOrdersByTicker.computeIfAbsent(key, ignored -> new ProtectiveOrders());
        syncStopOrder(figi, key, quantity, direction, position.stopLoss, currentOrders, true);
        syncStopOrder(figi, key, quantity, direction, position.takeProfit, currentOrders, false);
    }

    public void clearProtectiveOrders(String name, TickerType type) {
        if (mainConfig.isSandbox()) {
            return;
        }

        TickerInfo.Key key = new TickerInfo.Key(name, type);
        ProtectiveOrders protectiveOrders = protectiveOrdersByTicker.remove(key);
        if (protectiveOrders == null) {
            return;
        }

        cancelStopOrder(key, protectiveOrders.stopLossOrderId, "StopLose");
        cancelStopOrder(key, protectiveOrders.takeProfitOrderId, "TakeProfit");
    }

    private Position createProtectivePosition(OrderDirection direction,
                                              double executedPrice,
                                              double stopLose,
                                              double takeProfit,
                                              boolean isFullPrice,
                                              int count,
                                              TickerInfo tickerInfo) {
        Double stopLossPrice = null;
        Double takeProfitPrice = null;

        if (stopLose > 0.0) {
            double slPrice = ORDER_DIRECTION_BUY == direction
                    ? (isFullPrice ? stopLose : executedPrice - ((executedPrice / 100) * stopLose))
                    : (isFullPrice ? stopLose : executedPrice + ((executedPrice / 100) * stopLose));
            stopLossPrice = normalizePrice(slPrice, tickerInfo.getMinPriceIncrement());
        }

        if (takeProfit > 0.0) {
            double tpPrice = ORDER_DIRECTION_BUY == direction
                    ? (isFullPrice ? takeProfit : executedPrice + ((executedPrice / 100) * takeProfit))
                    : (isFullPrice ? takeProfit : executedPrice - ((executedPrice / 100) * takeProfit));
            takeProfitPrice = normalizePrice(tpPrice, tickerInfo.getMinPriceIncrement());
        }

        if (stopLossPrice == null && takeProfitPrice == null) {
            return null;
        }

        return new Position(
                ORDER_DIRECTION_BUY == direction ? "BUY" : "SELL",
                executedPrice,
                stopLossPrice,
                takeProfitPrice,
                count,
                0
        );
    }

    private void placeOrLogProtectiveOrders(String figi,
                                            int quantity,
                                            TickerInfo.Key key,
                                            OrderDirection direction,
                                            Position bracketPosition) {
        if (bracketPosition == null) {
            return;
        }

        if (bracketPosition.stopLoss != null) {
            out.println(key.getTicker() + " StopLose target: " + bracketPosition.stopLoss);
            if (!mainConfig.isSandbox()) {
                sleep(1_000);
                try {
                    StopOrderDirection stopOrderDirection = ORDER_DIRECTION_BUY == direction
                            ? STOP_ORDER_DIRECTION_SELL
                            : STOP_ORDER_DIRECTION_BUY;
                    Quotation stopLosePrice = createQuotation(bracketPosition.stopLoss);
                    String stopOrderId = investApi.getStopOrdersService().postStopOrderGoodTillCancelSync(
                            figi,
                            quantity,
                            stopLosePrice,
                            stopLosePrice,
                            stopOrderDirection,
                            mainConfig.getTcsAccountId(),
                            STOP_ORDER_TYPE_STOP_LOSS
                    );
                    protectiveOrdersByTicker.computeIfAbsent(key, ignored -> new ProtectiveOrders()).stopLossOrderId = stopOrderId;
                } catch (Exception ex) {
                    var error = "Failed create StopLose: " + ex.getMessage();
                    out.println(error);
                    telegramNotifyService.sendMessage(error);
                    ex.printStackTrace();
                }
            }
        }

        if (bracketPosition.takeProfit != null) {
            out.println(key.getTicker() + " TakeProfit target: " + bracketPosition.takeProfit);
            if (!mainConfig.isSandbox()) {
                sleep(1_000);
                try {
                    StopOrderDirection stopOrderDirection = ORDER_DIRECTION_BUY == direction
                            ? STOP_ORDER_DIRECTION_SELL
                            : STOP_ORDER_DIRECTION_BUY;
                    Quotation takeProfitPrice = createQuotation(bracketPosition.takeProfit);
                    String stopOrderId = investApi.getStopOrdersService().postStopOrderGoodTillCancelSync(
                            figi,
                            quantity,
                            takeProfitPrice,
                            takeProfitPrice,
                            stopOrderDirection,
                            mainConfig.getTcsAccountId(),
                            STOP_ORDER_TYPE_TAKE_PROFIT
                    );
                    protectiveOrdersByTicker.computeIfAbsent(key, ignored -> new ProtectiveOrders()).takeProfitOrderId = stopOrderId;
                } catch (Exception ex) {
                    var error = "Failed create TakeProfit: " + ex.getMessage();
                    out.println(error);
                    telegramNotifyService.sendMessage(error);
                    ex.printStackTrace();
                }
            }
        }
    }

    public static class OrderExecutionResult {

        private final boolean success;
        private final Double executedPrice;
        private final int executedCount;
        private final Position protectivePosition;

        private OrderExecutionResult(boolean success, Double executedPrice, int executedCount, Position protectivePosition) {
            this.success = success;
            this.executedPrice = executedPrice;
            this.executedCount = executedCount;
            this.protectivePosition = protectivePosition;
        }

        public static OrderExecutionResult success(Double executedPrice, int executedCount, Position protectivePosition) {
            return new OrderExecutionResult(true, executedPrice, executedCount, protectivePosition);
        }

        public static OrderExecutionResult testSuccess(Double executedPrice, int executedCount) {
            return new OrderExecutionResult(true, executedPrice, executedCount, null);
        }

        public static OrderExecutionResult failed() {
            return new OrderExecutionResult(false, null, 0, null);
        }

        public boolean isSuccess() {
            return success;
        }

        public Double getExecutedPrice() {
            return executedPrice;
        }

        public int getExecutedCount() {
            return executedCount;
        }

        public Position getProtectivePosition() {
            return protectivePosition;
        }
    }

    private void syncStopOrder(String figi,
                               TickerInfo.Key key,
                               int quantity,
                               OrderDirection direction,
                               Double price,
                               ProtectiveOrders protectiveOrders,
                               boolean isStopLoss) {
        String currentOrderId = isStopLoss ? protectiveOrders.stopLossOrderId : protectiveOrders.takeProfitOrderId;
        if (price == null || price <= 0.0) {
            cancelStopOrder(key, currentOrderId, isStopLoss ? "StopLose" : "TakeProfit");
            if (isStopLoss) {
                protectiveOrders.stopLossOrderId = null;
            } else {
                protectiveOrders.takeProfitOrderId = null;
            }
            return;
        }

        cancelStopOrder(key, currentOrderId, isStopLoss ? "StopLose" : "TakeProfit");

        sleep(1_000);
        try {
            StopOrderDirection stopOrderDirection = ORDER_DIRECTION_BUY == direction
                    ? STOP_ORDER_DIRECTION_SELL
                    : STOP_ORDER_DIRECTION_BUY;
            Quotation stopPrice = createQuotation(price);
            String stopOrderId = investApi.getStopOrdersService().postStopOrderGoodTillCancelSync(
                    figi,
                    quantity,
                    stopPrice,
                    stopPrice,
                    stopOrderDirection,
                    mainConfig.getTcsAccountId(),
                    isStopLoss ? STOP_ORDER_TYPE_STOP_LOSS : STOP_ORDER_TYPE_TAKE_PROFIT
            );
            if (isStopLoss) {
                protectiveOrders.stopLossOrderId = stopOrderId;
            } else {
                protectiveOrders.takeProfitOrderId = stopOrderId;
            }
            out.println(key.getTicker() + " " + (isStopLoss ? "StopLose" : "TakeProfit") + " synced: " + price);
        } catch (Exception ex) {
            var error = "Failed sync " + (isStopLoss ? "StopLose" : "TakeProfit") + " for " + key.getTicker() + ": " + ex.getMessage();
            out.println(error);
            telegramNotifyService.sendMessage(error);
            if (isStopLoss) {
                protectiveOrders.stopLossOrderId = null;
            } else {
                protectiveOrders.takeProfitOrderId = null;
            }
        }
    }

    private void cancelStopOrder(TickerInfo.Key key, String stopOrderId, String orderTypeName) {
        if (stopOrderId == null || stopOrderId.isBlank()) {
            return;
        }
        try {
            investApi.getStopOrdersService().cancelStopOrderSync(mainConfig.getTcsAccountId(), stopOrderId);
            out.println(key.getTicker() + " " + orderTypeName + " cancelled: " + stopOrderId);
        } catch (Exception ex) {
            var error = "Failed cancel " + orderTypeName + " for " + key.getTicker() + ": " + ex.getMessage();
            out.println(error);
            telegramNotifyService.sendMessage(error);
        }
    }

    private static class ProtectiveOrders {

        private String stopLossOrderId;
        private String takeProfitOrderId;
    }

    public int calculateTradeCount(TickerInfo.Key key, double availableCash, double price) {
        if (availableCash <= 0.0 || price <= 0.0) {
            return 0;
        }

        TickerInfo tickerInfo = searchTicker(key);
        int lot = Math.max(1, tickerInfo.getLot());
        double orderCost = getOrderValue(tickerInfo, lot, price);
        if (availableCash < orderCost) {
            return 0;
        }

        int lots = (int) Math.floor(availableCash / orderCost);
        return lots * lot;
    }

    public double getRequiredCashForOrder(TickerInfo.Key key, int count, double price) {
        if (count <= 0 || price <= 0.0) {
            return 0.0;
        }

        return getOrderValue(searchTicker(key), count, price);
    }

    private double getOrderValue(TickerInfo tickerInfo, int count, double price) {
        double fullValue = count * price;
        if (TickerType.FEATURE == tickerInfo.getType()) {
            return fullValue * FUTURES_MARGIN_RATE;
        }
        return fullValue;
    }

    private static Quotation createQuotation(double price) {
        long units = (long) price;
        double fractional = price - units;
        // nano - это дробная часть, умноженная на 1_000_000_000
        int nano = (int) Math.round(fractional * 1_000_000_000);
        return Quotation.newBuilder()
                .setUnits(units)
                .setNano(nano)
                .build();
    }

    public Map<TickerInfo.Key, TickerInfo> getStockList() {
        out.println("Loading current stocks...");
        List<Share> stocks = investApi.getInstrumentsService().getTradableSharesSync();
        return stocks.stream()
                .map(it -> new TickerInfo(
                        it.getFigi(),
                        it.getTicker(),
                        it.getIsin(),
                        toDouble(it.getMinPriceIncrement()),
                        it.getLot(),
                        it.getCurrency(),
                        it.getName(),
                        TickerType.STOCK.name()
                ))
                .collect(Collectors.toMap(TickerInfo::getKey, it -> it, (o, n) -> n));
    }

    public Map<TickerInfo.Key, TickerInfo> getBondList() {
        out.println("Loading current bonds...");
        List<Bond> bonds = investApi.getInstrumentsService().getTradableBondsSync();
        return bonds.stream()
                .map(it -> new TickerInfo(
                        it.getFigi(),
                        it.getTicker(),
                        it.getIsin(),
                        toDouble(it.getMinPriceIncrement()),
                        it.getLot(),
                        it.getCurrency(),
                        it.getName(),
                        TickerType.BOND.name()
                ))
                .collect(Collectors.toMap(TickerInfo::getKey, it -> it, (o, n) -> n));
    }

    public Map<TickerInfo.Key, TickerInfo> getEtfList() {
        out.println("Loading current etfs...");
        List<Etf> etfs = investApi.getInstrumentsService().getTradableEtfsSync();
        return etfs.stream()
                .map(it -> new TickerInfo(
                        it.getFigi(),
                        it.getTicker(),
                        it.getIsin(),
                        toDouble(it.getMinPriceIncrement()),
                        it.getLot(),
                        it.getCurrency(),
                        it.getName(),
                        TickerType.ETF.name()
                ))
                .collect(Collectors.toMap(TickerInfo::getKey, it -> it, (o, n) -> n));
    }

    public Map<TickerInfo.Key, TickerInfo> getCurrenciesList() {
        out.println("Loading current currencies...");
        List<Currency> currencies = investApi.getInstrumentsService().getTradableCurrenciesSync();
        return currencies.stream()
                .map(it -> new TickerInfo(
                        it.getFigi(),
                        it.getTicker(),
                        it.getIsin(),
                        toDouble(it.getMinPriceIncrement()),
                        it.getLot(),
                        it.getCurrency(),
                        it.getName(),
                        TickerType.CURRENCY.name()
                ))
                .collect(Collectors.toMap(TickerInfo::getKey, it -> it, (o, n) -> n));
    }

    public Map<TickerInfo.Key, TickerInfo> getFuturesList() {
        out.println("Loading current features...");
        List<Future> futures = investApi.getInstrumentsService().getTradableFuturesSync();
        return futures.stream()
                .map(it -> new TickerInfo(
                        it.getFigi(),
                        it.getTicker(),
                        it.getBasicAsset(),
                        toDouble(it.getMinPriceIncrement()),
                        it.getLot(),
                        it.getCurrency(),
                        it.getName(),
                        TickerType.FEATURE.name()
                ))
                .collect(Collectors.toMap(TickerInfo::getKey, it -> it, (o, n) -> n));
    }

    public Double getAvailableCash() {
        sleep(550);
        Positions positions = investApi.getOperationsService().getPositionsSync(mainConfig.getTcsAccountId());
        return positions.getMoney()
                .stream()
                .filter(it -> marketConfig.getCurrency().equalsIgnoreCase(it.getCurrency()))
                .map(Money::getValue)
                .findFirst()
                .orElse(BigDecimal.ZERO)
                .doubleValue();
    }

    public TickerInfo searchTicker(TickerInfo.Key key) {
        if (tickerRepository.containsKey(key)) {
            return tickerRepository.getById(key);
        }
        out.println("Search ticker '" + key.getTicker() + "'...");
        sleep(550);

        TickerInfo tickerInfo;
        switch (key.getType()) {
            case ETF:
                tickerInfo = getEtfList().get(key);
                break;
            case BOND:
                tickerInfo = getBondList().get(key);
                break;
            case CURRENCY:
                tickerInfo = getCurrenciesList().get(key);
                break;
            case STOCK:
                tickerInfo = getStockList().get(key);
                break;
            case FEATURE:
                tickerInfo = getFuturesList().get(key);
                break;
            case UNKNOWN:
            default:
                throw new RuntimeException("Ticker '" + key.getTicker() + "' not found in TCS");
        }

        if (null != tickerInfo) {
            tickerRepository.insert(key, tickerInfo);
            out.println(key.getTicker() + ": " + tickerInfo);
        }
        return tickerInfo;
    }

    public double getTotalPortfolioCost() {
        return investApi.getOperationsService()
                .getPortfolioSync(mainConfig.getTcsAccountId())
                .getTotalAmountPortfolio()
                .getValue()
                .doubleValue();
    }

    public int getCountOfCurrentPositions(TickerType tickerType, String tickerName) {
        out.println("Loading current positions: " + tickerName);
        return getCurrentPositions(tickerType).values()
                .stream()
                .filter(it -> it.getTicker().equalsIgnoreCase(tickerName))
                .map(PositionInfo::getBalance)
                .findFirst()
                .orElse(0);
    }

    public PositionInfo getCurrentPositions(TickerType tickerType, String tickerName) {
        return getCurrentPositions(tickerType).values()
                .stream()
                .filter(it -> it.getTicker().equalsIgnoreCase(tickerName))
                .findFirst()
                .orElse(null);
    }

    public Map<TickerInfo.Key, PositionInfo> getCurrentPositions(@Nullable TickerType tickerType) {
        sleep(550);
        Map<TickerInfo.Key, PositionInfo> positionInfoList = new HashMap<>();
        String type = getTypeFromTickerType(tickerType);
        Portfolio portfolio = investApi.getOperationsService().getPortfolioSync(mainConfig.getTcsAccountId());
        portfolio.getPositions()
                .stream()
                .filter(it -> TickerType.ALL == tickerType || type.equalsIgnoreCase(it.getInstrumentType()))
                .forEach(it -> {
                    AtomicReference<TickerInfo.Key> tickerKey = new AtomicReference<>();
                    figiRepository.getAll().forEach((key, value) -> {
                        if (value.equals(it.getFigi())) {
                            tickerKey.set(key);
                        }
                    });
                    if (null == tickerKey.get()) {
                        tickerRepository.getAll().values()
                                .stream()
                                .filter(ticker -> ticker.getFigi().equals(it.getFigi()))
                                .findFirst()
                                .map(ticker -> new TickerInfo.Key(ticker.getTicker(), ticker.getType()))
                                .ifPresent(tickerKey::set);
                    }
                    if (null == tickerKey.get()) {
                        TickerInfo recoveredTicker = recoverTickerInfoByFigi(it.getFigi(), it.getInstrumentType());
                        if (recoveredTicker != null) {
                            tickerKey.set(recoveredTicker.getKey());
                        }
                    }
                    if (null == tickerKey.get()) {
                        out.println("Warn: position skipped, ticker key not found for figi=" + it.getFigi()
                                + ", instrumentType=" + it.getInstrumentType());
                        return;
                    }
                    TickerInfo tickerInfo = searchTicker(tickerKey.get());
                    if (null != tickerInfo && marketConfig.getCurrency().equals(tickerInfo.getCurrency())) {
                        var expectedYield = it.getExpectedYield().doubleValue();
                        if (0.0 == expectedYield) {
                            var currentPrice = it.getCurrentPrice().getValue().doubleValue();
                            var averagePositionPrice = it.getAveragePositionPriceFifo().getValue().doubleValue();
                            if (it.getQuantity().doubleValue() > 0) {
                                expectedYield = (currentPrice - averagePositionPrice) / averagePositionPrice * 100;
                            } else {
                                expectedYield = (averagePositionPrice - currentPrice) / currentPrice * 100;
                            }
                        }
                        PositionInfo positionInfo = new PositionInfo(
                                tickerInfo.getFigi(),
                                tickerInfo.getTicker(),
                                tickerInfo.getIsin(),
                                tickerInfo.getType().name(),
                                it.getQuantity().intValue(),
                                expectedYield,
                                it.getQuantityLots().intValue(),
                                it.getAveragePositionPriceFifo().getValue().doubleValue(),
                                tickerInfo.getName()
                        );
                        positionInfoList.put(tickerKey.get(), positionInfo);
                    }
                });
        return positionInfoList;
    }

    private TickerInfo recoverTickerInfoByFigi(String figi, String instrumentType) {
        if (figi == null || figi.isBlank()) {
            return null;
        }

        TickerInfo tickerInfo = null;
        if ("share".equalsIgnoreCase(instrumentType)) {
            tickerInfo = getStockList().values().stream()
                    .filter(ticker -> figi.equals(ticker.getFigi()))
                    .findFirst()
                    .orElse(null);
        } else if ("futures".equalsIgnoreCase(instrumentType)) {
            tickerInfo = getFuturesList().values().stream()
                    .filter(ticker -> figi.equals(ticker.getFigi()))
                    .findFirst()
                    .orElse(null);
        } else if ("bond".equalsIgnoreCase(instrumentType)) {
            tickerInfo = getBondList().values().stream()
                    .filter(ticker -> figi.equals(ticker.getFigi()))
                    .findFirst()
                    .orElse(null);
        } else if ("etf".equalsIgnoreCase(instrumentType)) {
            tickerInfo = getEtfList().values().stream()
                    .filter(ticker -> figi.equals(ticker.getFigi()))
                    .findFirst()
                    .orElse(null);
        } else if ("currency".equalsIgnoreCase(instrumentType)) {
            tickerInfo = getCurrenciesList().values().stream()
                    .filter(ticker -> figi.equals(ticker.getFigi()))
                    .findFirst()
                    .orElse(null);
        }

        if (tickerInfo != null) {
            tickerRepository.insert(tickerInfo.getKey(), tickerInfo);
            figiRepository.insert(tickerInfo.getKey(), tickerInfo.getFigi());
            out.println("Recovered ticker by figi: " + tickerInfo);
        }
        return tickerInfo;
    }

    private String getTypeFromTickerType(TickerType tickerType) {
        if (TickerType.STOCK == tickerType) return  "share";
        if (TickerType.FEATURE == tickerType) return "futures";
        return "";
    }

    public String figiByName(TickerInfo.Key key) {
        if (figiRepository.containsKey(key)) {
            return figiRepository.getById(key);
        }
        var ticker = searchTicker(key);
        var figi = ticker.getFigi();
        figiRepository.insert(key, figi);
        return figi;
    }

    public double getPriceInCurrentCurrency(TickerInfo.Key key, int qty, String basicCurrency) {
        double price = getAvailablePrice(key, qty, false);
        String currency = searchTicker(key).getCurrency();
        if (!basicCurrency.equals(currency)) {
            price = convertCurrencies(currency, basicCurrency, price);
        }
        return price;
    }

    public double getAvailablePrice(String name, TickerType type) {
        return getAvailablePrice(new TickerInfo.Key(name, type));
    }

    public double getAvailablePrice(TickerInfo.Key key) {
        return getAvailablePrice(key, 1, false);
    }

    public double getAvailablePrice(String name, TickerType type, int count, String glassType) {
        return getAvailablePrice(new TickerInfo.Key(name, type), count, glassType, true);
    }

    public double getAvailablePrice(String name, TickerType type, int count, String glassType, boolean isPrintGlass) {
        return getAvailablePrice(new TickerInfo.Key(name, type), count, glassType, isPrintGlass);
    }

    public double getAvailablePrice(TickerInfo.Key key, int count, boolean isPrintGlass) {
        return getAvailablePrice(key, count, "bids", isPrintGlass);
    }

    public double getAvailablePrice(TickerInfo.Key key, int count, String type, boolean isPrintGlass) {
        int value = count;
        double tickerPrice = 0.0;

        var glass = getCurrentPrices(key, isPrintGlass).get(type).entrySet();
        if ("asks".equals(type)) {
            glass = glass.stream()
                    .sorted((o1, o2) -> o1.getKey().compareTo(o2.getKey()))
                    .collect(toCollection(LinkedHashSet::new));
        }
        if ("bids".equals(type)) {
            glass = glass.stream()
                    .sorted((o1, o2) -> o2.getKey().compareTo(o1.getKey()))
                    .collect(toCollection(LinkedHashSet::new));
        }
        for (Map.Entry<Double, Integer> bid : glass) {
            tickerPrice = bid.getKey();
            value = value - bid.getValue();
            if (value <= 0) break;
        }
        return tickerPrice;
    }

    public Map<String, Map<Double, Integer>> getCurrentPrices(TickerInfo.Key key) {
        return getCurrentPrices(key, true);
    }

    public Map<String, Map<Double, Integer>> getCurrentPrices(TickerInfo.Key key, boolean isPrintGlass) {
        if (!isPrintGlass && pricesRepository.containsKey(key)) {
            return pricesRepository.getById(key);
        }
        String figi = figiByName(key);
        if (isPrintGlass) {
            out.println("Loading current price '" + key + "'...");
        }
        sleep(550);

        GetOrderBookResponse response = investApi.getMarketDataService().getOrderBookSync(figi, 10);

        Map<String, Map<Double, Integer>> currentPrices = new TreeMap<>();
        Map<Double, Integer> bidsValues = new LinkedHashMap<>(10);
        for (Order bid : response.getBidsList()) {
            bidsValues.put(toDouble(bid.getPrice()), (int) bid.getQuantity());
        }
        currentPrices.put("bids", bidsValues);

        Map<Double, Integer> asksValues = new LinkedHashMap<>(10);
        for (Order ask : response.getAsksList()) {
            asksValues.put(toDouble(ask.getPrice()), (int) ask.getQuantity());
        }
        currentPrices.put("asks", asksValues);

        if (isPrintGlass) {
            synchronized(this) {
                printGlassOfPrices(key.getTicker(), currentPrices);
            }
        }
        pricesRepository.insert(key, currentPrices);
        return currentPrices;
    }

    public double convertCurrencies(String currency, String basicCurrency, double price) {
        if (basicCurrency.equals(currency)) {
            return price;
        }

        String currencyTicker = getTickerName(currency);
        if (currencyTicker.equals(currency)) {
            return round(((price / getAvailablePrice(new TickerInfo.Key(getTickerName(basicCurrency), TickerType.CURRENCY)))) * 1000) / 1000.0;
        }

        var key = new TickerInfo.Key(currencyTicker, TickerType.CURRENCY);
        TickerInfo currencyTickerInfo = searchTicker(key);
        if (basicCurrency.equals(currencyTickerInfo.getCurrency())) {
            return round(((price / getAvailablePrice(key))) * 100000) / 100000.0;
        }
        return price;
    }

    private static Double toDouble(Quotation quotation) {
        return toDouble(quotation.getUnits(), quotation.getNano());
    }

    private static Double toDouble(long units, int nano) {
        double fractional = nano / 1_000_000_000.0;
        return units + fractional;
    }

    private static Double normalizePrice(double price, double priceStep) {
        return Math.round(price / priceStep) * priceStep;
    }
}
