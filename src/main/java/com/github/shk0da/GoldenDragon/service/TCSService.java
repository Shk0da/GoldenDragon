package com.github.shk0da.GoldenDragon.service;

import com.github.shk0da.GoldenDragon.config.MainConfig;
import com.github.shk0da.GoldenDragon.config.MarketConfig;
import com.github.shk0da.GoldenDragon.model.MarketDepthLevel;
import com.github.shk0da.GoldenDragon.model.MarketDepthSnapshot;
import com.github.shk0da.GoldenDragon.model.MarketTickListener;
import com.github.shk0da.GoldenDragon.model.MarketTradeTick;
import com.github.shk0da.GoldenDragon.model.Position;
import com.github.shk0da.GoldenDragon.model.PositionInfo;
import com.github.shk0da.GoldenDragon.model.TickerCandle;
import com.github.shk0da.GoldenDragon.model.TickerInfo;
import com.github.shk0da.GoldenDragon.model.TickerType;
import com.github.shk0da.GoldenDragon.repository.FigiRepository;
import com.github.shk0da.GoldenDragon.repository.PricesRepository;
import com.github.shk0da.GoldenDragon.repository.Repository;
import com.github.shk0da.GoldenDragon.repository.TickerRepository;
import java.io.IOException;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.text.SimpleDateFormat;
import java.time.Duration;
import java.time.Instant;
import java.time.OffsetDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;
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
import ru.tinkoff.piapi.contract.v1.MarketDataResponse;
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
import ru.tinkoff.piapi.core.stream.MarketDataSubscriptionService;


import static com.github.shk0da.GoldenDragon.dictionary.CurrenciesDictionary.getTickerName;
import static com.github.shk0da.GoldenDragon.service.TelegramNotifyService.telegramNotifyService;
import static com.github.shk0da.GoldenDragon.utils.PrintUtils.printGlassOfPrices;
import static com.github.shk0da.GoldenDragon.utils.TimeUtils.sleep;
import static java.lang.Math.max;
import static java.lang.Math.round;
import static java.lang.System.out;
import static java.util.Collections.emptyList;
import static java.util.stream.Collectors.toCollection;
import static ru.tinkoff.piapi.contract.v1.OrderDirection.ORDER_DIRECTION_BUY;
import static ru.tinkoff.piapi.contract.v1.OrderDirection.ORDER_DIRECTION_SELL;
import static ru.tinkoff.piapi.contract.v1.OrderExecutionReportStatus.EXECUTION_REPORT_STATUS_FILL;
import static ru.tinkoff.piapi.contract.v1.StopOrderDirection.STOP_ORDER_DIRECTION_BUY;
import static ru.tinkoff.piapi.contract.v1.StopOrderDirection.STOP_ORDER_DIRECTION_SELL;
import static ru.tinkoff.piapi.contract.v1.StopOrderType.STOP_ORDER_TYPE_STOP_LOSS;
import static ru.tinkoff.piapi.contract.v1.StopOrderType.STOP_ORDER_TYPE_TAKE_PROFIT;

public class TCSService {

    @Deprecated
    public static final double FUTURES_MARGIN_RATE = 0.40;

    private static final int FUTURES_CONTRACT_MULTIPLIER = 1000;
    private static final String MARKET_DEPTH_TICKS_HEADER = "time,best_bid,best_ask,mid_price,bids,asks";
    private static final DateTimeFormatter MARKET_DEPTH_TICKS_TIME_FORMATTER = DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm:ss");
    private static final ThreadLocal<SimpleDateFormat> LOG_TIME_FORMAT =
            ThreadLocal.withInitial(() -> new SimpleDateFormat("dd.MM.yyyy HH:mm:ss"));

    private final MainConfig mainConfig;
    private final MarketConfig marketConfig;
    private final InvestApi investApi;
    private final boolean writeMarketDepthTicks;

    private final Repository<TickerInfo.Key, String> figiRepository = FigiRepository.INSTANCE;
    private final Repository<TickerInfo.Key, TickerInfo> tickerRepository = TickerRepository.INSTANCE;
    private final Repository<TickerInfo.Key, Map<String, Map<Double, Integer>>> pricesRepository = PricesRepository.INSTANCE;
    private final Map<TickerInfo.Key, Double> lastExecutedPriceByTicker = new ConcurrentHashMap<>();
    private final Map<TickerInfo.Key, ProtectiveOrders> protectiveOrdersByTicker = new ConcurrentHashMap<>();
    private final Map<TickerInfo.Key, MarketDepthSnapshot> marketDepthByTicker = new ConcurrentHashMap<>();
    private final Map<TickerInfo.Key, List<MarketTradeTick>> recentTradesByTicker = new ConcurrentHashMap<>();
    private final Map<TickerInfo.Key, CopyOnWriteArrayList<MarketTickListener>> marketTickListenersByTicker = new ConcurrentHashMap<>();
    private final Map<TickerInfo.Key, MarketDataSubscriptionService> marketDataStreamsByTicker = new ConcurrentHashMap<>();
    private final Map<String, Object> marketDepthFileLocks = new ConcurrentHashMap<>();
    private volatile Map<TickerInfo.Key, TickerInfo> cachedStockList;
    private volatile Instant cachedStockListAt;

    public TCSService(MainConfig mainConfig, MarketConfig marketConfig) {
        this.mainConfig = mainConfig;
        this.marketConfig = marketConfig;
        this.investApi = mainConfig.isSandbox()
                ? InvestApi.createSandbox(mainConfig.getTcsApiKey())
                : InvestApi.create(mainConfig.getTcsApiKey());
        this.writeMarketDepthTicks = mainConfig.isWriteMarketDepthTicks();

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

    public void subscribeMarketData(TickerInfo.Key key, int depth, MarketTickListener listener) {
        marketTickListenersByTicker.computeIfAbsent(key, ignored -> new CopyOnWriteArrayList<>()).add(listener);
        marketDataStreamsByTicker.computeIfAbsent(key, ignored -> {
            String streamId = "market-data-" + key.getTicker() + "-" + key.getType().name();
            MarketDataSubscriptionService stream = investApi.getMarketDataStreamService().newStream(
                    streamId,
                    response -> handleMarketDataResponse(key, response),
                    throwable -> notifyMarketDataError(key, throwable)
            );
            String figi = figiByName(key);
            stream.subscribeOrderbook(List.of(figi), depth);
            stream.subscribeTrades(List.of(figi));
            return stream;
        });
    }

    public void unsubscribeMarketData(TickerInfo.Key key, MarketTickListener listener) {
        List<MarketTickListener> listeners = marketTickListenersByTicker.get(key);
        if (listeners == null) {
            return;
        }
        listeners.remove(listener);
        if (!listeners.isEmpty()) {
            return;
        }
        marketTickListenersByTicker.remove(key);
        MarketDataSubscriptionService stream = marketDataStreamsByTicker.remove(key);
        if (stream != null) {
            String figi = figiByName(key);
            stream.unsubscribeOrderbook(List.of(figi));
            stream.unsubscribeTrades(List.of(figi));
            stream.cancel();
        }
    }

    public MarketDepthSnapshot getLastMarketDepth(TickerInfo.Key key) {
        return marketDepthByTicker.get(key);
    }

    public List<MarketTradeTick> getRecentTrades(TickerInfo.Key key, Duration maxAge) {
        List<MarketTradeTick> trades = recentTradesByTicker.get(key);
        if (trades == null || trades.isEmpty()) {
            return List.of();
        }
        Instant threshold = Instant.now().minus(maxAge);
        return trades.stream()
                .filter(it -> !it.getTime().isBefore(threshold))
                .collect(Collectors.toList());
    }

    public List<MarketTradeTick> getLastTrades(TickerInfo.Key key, Instant from, Instant to) {
        String figi = figiByName(key);
        return investApi.getMarketDataService().getLastTradesSync(figi, from, to).stream()
                .map(it -> new MarketTradeTick(
                        figi,
                        Instant.ofEpochSecond(it.getTime().getSeconds(), it.getTime().getNanos()),
                        toDouble(it.getPrice()),
                        it.getQuantity(),
                        it.getDirection().name()
                ))
                .collect(Collectors.toList());
    }

    public List<HistoricCandle> getCandles(String figi, Instant start, Instant end, CandleInterval interval) {
        return investApi.getMarketDataService().getCandlesSync(figi, start, end, interval);
    }

    public List<HistoricCandle> getCandles(String figi, OffsetDateTime start, OffsetDateTime end, CandleInterval interval) {
        return investApi.getMarketDataService().getCandlesSync(figi, start.toInstant(), end.toInstant(), interval);
    }

    public List<HistoricCandle> getLastCandles(String ticker, TickerType type, int size) {
        if (size <= 0) return emptyList();
        TickerInfo.Key key = new TickerInfo.Key(ticker, type);
        String figi = figiByName(key);
        Instant end = Instant.now();
        Instant start = end.minusSeconds(size * 3600L);
        List<HistoricCandle> candles = getCandles(figi, start, end, CandleInterval.CANDLE_INTERVAL_HOUR);
        return candles.stream()
                .sorted((c1, c2) -> Long.compare(c2.getTime().getSeconds(), c1.getTime().getSeconds()))
                .limit(size)
                .sorted(Comparator.comparingLong(c -> c.getTime().getSeconds()))
                .collect(Collectors.toList());
    }

    public List<TickerCandle> getLastCandlesAsTickerCandles(String ticker, TickerType type, int count) {
        if (count <= 0) return emptyList();
        List<HistoricCandle> candles = getLastCandles(ticker, type, count);
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
        return candles.stream()
                .map(c -> new TickerCandle(
                        ticker,
                        formatter.format(Instant.ofEpochSecond(c.getTime().getSeconds(), c.getTime().getNanos())
                                .atZone(ZoneId.systemDefault())),
                        toDouble(c.getOpen()),
                        toDouble(c.getHigh()),
                        toDouble(c.getLow()),
                        toDouble(c.getClose()),
                        toDouble(c.getClose()),
                        (int) c.getVolume()
                ))
                .collect(Collectors.toList());
    }

    public void closeAllByMarket(TickerType type) {
        getCurrentPositions(type).values().forEach(ticker -> {
            int count = ticker.getBalance();
            String name = ticker.getTicker();
            if (count > 0) {
                var message = "Sell: " + count + " " + name + " by Market";
                log(message);
                if (mainConfig.isTestMode()) return;
                telegramNotifyService.sendMessage(message);
                createOrder(new TickerInfo.Key(name, type), 0.0, count, "Sell");
            }
            if (count < 0) {
                var message = "Buy: " + count + " " + name + " by Market";
                log(message);
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

    public boolean closePartiallyByMarket(String name, TickerType type, int count) {
        if (count <= 0) {
            return false;
        }

        int currentCount = getCountOfCurrentPositions(type, name);
        if (currentCount > 0) {
            int quantityToSell = Math.min(currentCount, count);
            return quantityToSell > 0
                    && 1 == createOrder(new TickerInfo.Key(name, type), 0.0, quantityToSell, "Sell");
        }

        if (currentCount < 0) {
            int quantityToBuy = Math.min(Math.abs(currentCount), count);
            return quantityToBuy > 0
                    && 1 == createOrder(new TickerInfo.Key(name, type), 0.0, quantityToBuy, "Buy");
        }
        return false;
    }

    public boolean closeShortByMarket(String name, TickerType type) {
        int count = getCountOfCurrentPositions(type, name);
        if (count < 0) {
            log("Buy: " + Math.abs(count) + " " + name + " by Market");

            if (mainConfig.isTestMode()) return true;
            return 1 == createOrder(new TickerInfo.Key(name, type), 0.0, Math.abs(count), "Buy");
        }
        return false;
    }

    public OrderExecutionResult closeShortByMarketWithDetails(String name, TickerType type) {
        int count = getCountOfCurrentPositions(type, name);
        if (count < 0) {
            log("Buy: " + Math.abs(count) + " " + name + " by Market");

            if (mainConfig.isTestMode()) {
                return OrderExecutionResult.testSuccess(getAvailablePrice(new TickerInfo.Key(name, type)), Math.abs(count));
            }
            return createOrder(new TickerInfo.Key(name, type), 0.0, Math.abs(count), "Buy", 0.0, 0.0, false);
        }
        return OrderExecutionResult.failed();
    }

    public OrderExecutionResult closeShortByMarketWithDetails(String name, TickerType type, int count) {
        int currentCount = getCountOfCurrentPositions(type, name);
        if (currentCount < 0) {
            int quantityToBuy = Math.min(Math.abs(currentCount), count);
            if (quantityToBuy <= 0) {
                return OrderExecutionResult.failed();
            }
            log("Buy: " + quantityToBuy + " " + name + " by Market");

            if (mainConfig.isTestMode()) {
                return OrderExecutionResult.testSuccess(getAvailablePrice(new TickerInfo.Key(name, type)), quantityToBuy);
            }
            return createOrder(new TickerInfo.Key(name, type), 0.0, quantityToBuy, "Buy", 0.0, 0.0, false);
        }
        return OrderExecutionResult.failed();
    }

    public boolean closeLongByMarket(String name, TickerType type) {
        int count = getCountOfCurrentPositions(type, name);
        if (count > 0) {
            log("Sell: " + count + " " + name + " by Market");

            if (mainConfig.isTestMode()) return true;
            return 1 == createOrder(new TickerInfo.Key(name, type), 0.0, count, "Sell");
        }
        return false;
    }

    public OrderExecutionResult closeLongByMarketWithDetails(String name, TickerType type) {
        int count = getCountOfCurrentPositions(type, name);
        if (count > 0) {
            log("Sell: " + count + " " + name + " by Market");

            if (mainConfig.isTestMode()) {
                return OrderExecutionResult.testSuccess(getAvailablePrice(new TickerInfo.Key(name, type)), count);
            }
            return createOrder(new TickerInfo.Key(name, type), 0.0, count, "Sell", 0.0, 0.0, false);
        }
        return OrderExecutionResult.failed();
    }

    public OrderExecutionResult closeLongByMarketWithDetails(String name, TickerType type, int count) {
        int currentCount = getCountOfCurrentPositions(type, name);
        if (currentCount > 0) {
            int quantityToSell = Math.min(currentCount, count);
            if (quantityToSell <= 0) {
                return OrderExecutionResult.failed();
            }
            log("Sell: " + quantityToSell + " " + name + " by Market");

            if (mainConfig.isTestMode()) {
                return OrderExecutionResult.testSuccess(getAvailablePrice(new TickerInfo.Key(name, type)), quantityToSell);
            }
            return createOrder(new TickerInfo.Key(name, type), 0.0, quantityToSell, "Sell", 0.0, 0.0, false);
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
            log("Warn: limit short will be skipped - " + name + " with count " + count
                    + ". CashToSell: " + cashToSell + ", price: " + limitPrice);
            return OrderExecutionResult.failed();
        }

        double cost = getRequiredCashForOrder(key, count, limitPrice);
        log("Sell: " + count + " " + key.getTicker() + " by "
                + limitPrice + " (" + cost + " " + currency + ")");
        if (mainConfig.isTestMode()) {
            return OrderExecutionResult.testSuccess(limitPrice, count);
        }
        return createOrder(key, limitPrice, count, "Sell", 0.0, 0.0, false, cashToSell);
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
            log("Warn: short will be skipped - " + name + " by price " + tickerPrice);
            return OrderExecutionResult.failed();
        }

        int count = calculateTradeCount(key, cashToSell, tickerPrice);
        if (count == 0) {
            log("Warn: short will be skipped - " + name + " with count " + count + ". CashToSell: " + cashToSell + ", price: " + tickerPrice);
            return OrderExecutionResult.failed();
        }
        double cost = getRequiredCashForOrder(key, count, tickerPrice);
        int lot = Math.max(1, searchTicker(key).getLot());
        int quantity = Math.max(1, count / lot);

        log("Sell: " + count + " " + key.getTicker() + " by "
                + (byMarket
                ? String.format("Market [price=%.4f, cost=%.2f %s, lot=%d, lots=%d, cash=%.2f]", tickerPrice, cost, currency, lot, quantity, cashToSell)
                : tickerPrice + " (" + cost + " " + currency + ")"));
        if (mainConfig.isTestMode()) {
            return OrderExecutionResult.testSuccess(tickerPrice, count);
        }
        return createOrder(key, byMarket ? 0.0 : tickerPrice, count, "Sell", takeProfit, stopLose, isFullPrice, cashToSell);
    }

    public boolean sell(String name, TickerType type, double cost) {
        if (cost == 0) {
            log("Warn: sale will be skipped - " + name + " with cost " + cost);
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
            log("Warn: sale will be skipped - " + name + " with count " + count);
            return false;
        }

        double tickerPrice = getAvailablePrice(key, count, true);
        if (0.0 == tickerPrice) {
            log("Warn: sale will be used Market Price - " + name);
        }

        log("Sell: " + count + " " + key.getTicker() + " by " + tickerPrice + " (" + cost + " " + currency + ")");
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
            log("Warn: limit long will be skipped - " + name + " with count " + count
                    + ". CashToBuy: " + cashToBuy + ", price: " + limitPrice);
            return OrderExecutionResult.failed();
        }

        double cost = getRequiredCashForOrder(key, count, limitPrice);
        log("Buy: " + count + " " + key.getTicker() + " by "
                + limitPrice + " (" + cost + " " + currency + ")");
        if (mainConfig.isTestMode()) {
            return OrderExecutionResult.testSuccess(limitPrice, count);
        }
        return createOrder(key, limitPrice, count, "Buy", 0.0, 0.0, false, cashToBuy);
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
            log("Warn: purchase will be skipped - " + name + " by price " + tickerPrice);
            return OrderExecutionResult.failed();
        }

        int count = calculateTradeCount(key, cashToBuy, tickerPrice);
        if (count == 0) {
            log("Warn: long will be skipped - " + name + " with count " + count + ". CashToBuy: " + cashToBuy + ", price: " + tickerPrice);
            return OrderExecutionResult.failed();
        }
        double cost = getRequiredCashForOrder(key, count, tickerPrice);
        int lot = Math.max(1, searchTicker(key).getLot());
        int quantity = Math.max(1, count / lot);

        log("Buy: " + count + " " + key.getTicker() + " by "
                + (byMarket
                ? String.format("Market [price=%.4f, cost=%.2f %s, lot=%d, lots=%d, cash=%.2f]", tickerPrice, cost, currency, lot, quantity, cashToBuy)
                : tickerPrice + " (" + cost + " " + currency + ")"));
        if (mainConfig.isTestMode()) {
            return OrderExecutionResult.testSuccess(tickerPrice, count);
        }
        return createOrder(key, byMarket ? 0.0 : tickerPrice, count, "Buy", takeProfit, stopLose, isFullPrice, cashToBuy);
    }

    public int createOrder(TickerInfo.Key key, double price, int count, String operation) {
        return createOrder(key, price, count, operation, 0.0, 0.0, false).isSuccess() ? 1 : 0;
    }

    public OrderExecutionResult createOrder(TickerInfo.Key key, double price, int count, String operation, double takeProfit, double stopLose, boolean isFullPrice) {
        return createOrder(key, price, count, operation, takeProfit, stopLose, isFullPrice, 0.0);
    }

    public OrderExecutionResult createOrder(TickerInfo.Key key,
                                            double price,
                                            int count,
                                            String operation,
                                            double takeProfit,
                                            double stopLose,
                                            boolean isFullPrice,
                                            double cashToUse) {
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
        double referencePrice = price > 0.0
                ? price
                : getAvailablePrice(key, Math.max(1, count), ORDER_DIRECTION_BUY == direction ? "asks" : "bids", false);
        int contractMultiplier = TickerType.FEATURE == tickerInfo.getType() ? FUTURES_CONTRACT_MULTIPLIER : 1;
        double fullNotional = referencePrice > 0.0 ? count * referencePrice * contractMultiplier : 0.0;
        double estimatedCost = referencePrice > 0.0 ? getRequiredCashForOrder(key, count, referencePrice) : 0.0;
        log(String.format(
                "Create order request [%s]: operation=%s direction=%s type=%s count=%d lot=%d quantity=%d requestedPrice=%.4f referencePrice=%.4f contractMultiplier=%d fullNotional=%.2f cashToUse=%.2f estimatedCost=%.2f takeProfit=%.4f stopLose=%.4f isFullPrice=%s",
                key.getTicker(),
                operation,
                direction,
                type,
                count,
                lot,
                quantity,
                price,
                referencePrice,
                contractMultiplier,
                fullNotional,
                cashToUse,
                estimatedCost,
                takeProfit,
                stopLose,
                isFullPrice
        ));

        try {
            PostOrderResponse response = investApi.getOrdersService().postOrderSync(
                    figi, quantity, orderPrice, direction, mainConfig.getTcsAccountId(), type, null
            );
            int executedLots = Math.toIntExact(response.getLotsExecuted());
            int executedCount = executedLots > 0 ? executedLots * lot : count;
            double rawExecutedPrice = toDouble(
                    response.getExecutedOrderPrice().getUnits(),
                    response.getExecutedOrderPrice().getNano()
            );
            referencePrice = price > 0.0
                    ? price
                    : getAvailablePrice(key, Math.max(1, executedCount), ORDER_DIRECTION_BUY == direction ? "asks" : "bids", false);
            double executedPrice = normalizeExecutedPrice(rawExecutedPrice, executedCount, referencePrice, tickerInfo.getMinPriceIncrement());
            if (executedPrice <= 0.0) {
                executedPrice = referencePrice > 0.0 ? referencePrice : getAvailablePrice(key);
            }
            lastExecutedPriceByTicker.put(key, executedPrice);
            log(String.format(
                    "%s execution price normalized for %s: raw=%f normalized=%f reference=%f executedCount=%d",
                    operation,
                    key.getTicker(),
                    rawExecutedPrice,
                    executedPrice,
                    referencePrice,
                    executedCount
            ));

            double executedCommission = toDouble(response.getExecutedCommission().getUnits(), response.getExecutedCommission().getNano());
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
                    executedCommission
            );
            log(message);

            Position bracketPosition = null;
            if (response.getExecutionReportStatus().equals(EXECUTION_REPORT_STATUS_FILL)) {
                bracketPosition = createProtectivePosition(direction, executedPrice, stopLose, takeProfit, isFullPrice, executedCount, tickerInfo);
                placeOrLogProtectiveOrders(figi, executedLots > 0 ? executedLots : quantity, key, direction, bracketPosition);
            }

            telegramNotifyService.sendMessage(message, true);
            return OrderExecutionResult.success(executedPrice, executedCount, executedCommission, bracketPosition);
        } catch (Exception ex) {
            String message = "Failed create order [" + key.getTicker() + "]: " + ex.getMessage();
            log(message);
            telegramNotifyService.sendMessage(message);
            return OrderExecutionResult.failed();
        }
    }

    private double normalizeExecutedPrice(double rawExecutedPrice,
                                          int executedCount,
                                          double referencePrice,
                                          double minPriceIncrement) {
        if (rawExecutedPrice <= 0.0) {
            return 0.0;
        }
        if (executedCount <= 0) {
            return rawExecutedPrice;
        }

        double perUnitFromRaw = rawExecutedPrice;
        double perUnitFromTotal = rawExecutedPrice / executedCount;
        double tolerance = max(minPriceIncrement * 10, referencePrice * 0.03);

        boolean rawLooksPerUnit = referencePrice > 0.0 && Math.abs(perUnitFromRaw - referencePrice) <= tolerance;
        boolean totalLooksPerUnit = referencePrice > 0.0 && Math.abs(perUnitFromTotal - referencePrice) <= tolerance;

        if (rawLooksPerUnit && !totalLooksPerUnit) {
            return perUnitFromRaw;
        }
        if (!rawLooksPerUnit && totalLooksPerUnit) {
            return perUnitFromTotal;
        }
        if (rawLooksPerUnit) {
            return perUnitFromRaw;
        }
        if (totalLooksPerUnit) {
            return perUnitFromTotal;
        }

        if (referencePrice > 0.0) {
            return Math.abs(perUnitFromTotal - referencePrice) < Math.abs(perUnitFromRaw - referencePrice)
                    ? perUnitFromTotal
                    : perUnitFromRaw;
        }
        return perUnitFromTotal > 0.0 ? perUnitFromTotal : perUnitFromRaw;
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
                    log(error);
                    telegramNotifyService.sendMessage(error);
                    ex.printStackTrace();
                }
            }
        }

        if (bracketPosition.takeProfit != null) {
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
                    log(error);
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
        private final double commission;
        private final Position protectivePosition;

        private OrderExecutionResult(boolean success, Double executedPrice, int executedCount, double commission, Position protectivePosition) {
            this.success = success;
            this.executedPrice = executedPrice;
            this.executedCount = executedCount;
            this.commission = commission;
            this.protectivePosition = protectivePosition;
        }

        public static OrderExecutionResult success(Double executedPrice, int executedCount, double commission, Position protectivePosition) {
            return new OrderExecutionResult(true, executedPrice, executedCount, commission, protectivePosition);
        }

        public static OrderExecutionResult testSuccess(Double executedPrice, int executedCount) {
            return new OrderExecutionResult(true, executedPrice, executedCount, 0.0, null);
        }

        public static OrderExecutionResult failed() {
            return new OrderExecutionResult(false, null, 0, 0.0, null);
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

        public double getCommission() {
            return commission;
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
            log(key.getTicker() + " " + (isStopLoss ? "StopLose" : "TakeProfit") + " synced: " + price);
        } catch (Exception ex) {
            var error = "Failed sync " + (isStopLoss ? "StopLose" : "TakeProfit") + " for " + key.getTicker() + ": " + ex.getMessage();
            log(error);
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
            log(key.getTicker() + " " + orderTypeName + " cancelled: " + stopOrderId);
        } catch (Exception ex) {
            var error = "Failed cancel " + orderTypeName + " for " + key.getTicker() + ": " + ex.getMessage();
            log(error);
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
            return fullValue * FUTURES_CONTRACT_MULTIPLIER;
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
        if (cachedStockList != null && cachedStockListAt != null
                && cachedStockListAt.plus(Duration.ofMinutes(10)).isAfter(Instant.now())) {
            return cachedStockList;
        }

        log("Loading current stocks...");
        List<Share> stocks = investApi.getInstrumentsService().getTradableSharesSync();
        Map<TickerInfo.Key, TickerInfo> loadedStocks = stocks.stream()
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
        cachedStockList = loadedStocks;
        cachedStockListAt = Instant.now();
        return loadedStocks;
    }

    public Map<TickerInfo.Key, TickerInfo> getBondList() {
        log("Loading current bonds...");
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
        log("Loading current etfs...");
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
        log("Loading current currencies...");
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
        log("Loading current features...");
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
        log("Search ticker '" + key.getTicker() + "'...");
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
            log(key.getTicker() + ": " + tickerInfo);
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
                        log("Warn: position skipped, ticker key not found for figi=" + it.getFigi()
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
            log("Recovered ticker by figi: " + tickerInfo);
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
        MarketDepthSnapshot liveSnapshot = marketDepthByTicker.get(key);
        if (liveSnapshot != null && !liveSnapshot.getBids().isEmpty() && !liveSnapshot.getAsks().isEmpty()) {
            return toCurrentPrices(liveSnapshot, key, isPrintGlass);
        }
        if (!isPrintGlass && pricesRepository.containsKey(key)) {
            return pricesRepository.getById(key);
        }
        String figi = figiByName(key);
        if (isPrintGlass) {
            log("Loading current price '" + key + "'...");
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

    private void handleMarketDataResponse(TickerInfo.Key key, MarketDataResponse response) {
        if (response.hasOrderbook()) {
            MarketDepthSnapshot snapshot = toMarketDepthSnapshot(response.getOrderbook());
            marketDepthByTicker.put(key, snapshot);
            pricesRepository.insert(key, toCurrentPrices(snapshot));
            appendMarketDepthSnapshot(key, snapshot);
            notifyOrderBookListeners(key, snapshot);
        }
        if (response.hasTrade()) {
            MarketTradeTick trade = new MarketTradeTick(
                    response.getTrade().getFigi(),
                    Instant.ofEpochSecond(response.getTrade().getTime().getSeconds(), response.getTrade().getTime().getNanos()),
                    toDouble(response.getTrade().getPrice()),
                    response.getTrade().getQuantity(),
                    response.getTrade().getDirection().name()
            );
            recentTradesByTicker.computeIfAbsent(key, ignored -> new CopyOnWriteArrayList<>()).add(trade);
            trimRecentTrades(key);
            notifyTradeListeners(key, trade);
        }
    }

    private void notifyOrderBookListeners(TickerInfo.Key key, MarketDepthSnapshot snapshot) {
        List<MarketTickListener> listeners = marketTickListenersByTicker.get(key);
        if (listeners == null) {
            return;
        }
        listeners.forEach(listener -> listener.onOrderBook(snapshot));
    }

    private void notifyTradeListeners(TickerInfo.Key key, MarketTradeTick trade) {
        List<MarketTickListener> listeners = marketTickListenersByTicker.get(key);
        if (listeners == null) {
            return;
        }
        listeners.forEach(listener -> listener.onTrade(trade));
    }

    private void notifyMarketDataError(TickerInfo.Key key, Throwable throwable) {
        List<MarketTickListener> listeners = marketTickListenersByTicker.get(key);
        if (listeners == null) {
            return;
        }
        listeners.forEach(listener -> listener.onError(throwable));
    }

    private void trimRecentTrades(TickerInfo.Key key) {
        List<MarketTradeTick> trades = recentTradesByTicker.get(key);
        if (trades == null) {
            return;
        }
        Instant threshold = Instant.now().minus(Duration.ofMinutes(10));
        trades.removeIf(it -> it.getTime().isBefore(threshold));
    }

    private MarketDepthSnapshot toMarketDepthSnapshot(ru.tinkoff.piapi.contract.v1.OrderBook orderBook) {
        Instant time = orderBook.hasTime()
                ? Instant.ofEpochSecond(orderBook.getTime().getSeconds(), orderBook.getTime().getNanos())
                : Instant.now();
        return new MarketDepthSnapshot(
                orderBook.getFigi(),
                time,
                orderBook.getIsConsistent(),
                orderBook.getBidsList().stream()
                        .map(it -> new MarketDepthLevel(toDouble(it.getPrice()), (int) it.getQuantity()))
                        .collect(Collectors.toList()),
                orderBook.getAsksList().stream()
                        .map(it -> new MarketDepthLevel(toDouble(it.getPrice()), (int) it.getQuantity()))
                .collect(Collectors.toList())
        );
    }

    private void appendMarketDepthSnapshot(TickerInfo.Key key, MarketDepthSnapshot snapshot) {
        if (!writeMarketDepthTicks) {
            return;
        }
        if (!snapshot.isConsistent()) {
            return;
        }
        Path ticksFilePath = Path.of("data", key.getTicker(), "ticks.txt");
        Object fileLock = marketDepthFileLocks.computeIfAbsent(ticksFilePath.toString(), ignored -> new Object());
        synchronized (fileLock) {
            try {
                Files.createDirectories(ticksFilePath.getParent());
                if (!Files.exists(ticksFilePath)) {
                    Files.write(
                            ticksFilePath,
                            List.of(MARKET_DEPTH_TICKS_HEADER),
                            StandardCharsets.UTF_8,
                            StandardOpenOption.CREATE,
                            StandardOpenOption.APPEND
                    );
                }
                Files.write(
                        ticksFilePath,
                        List.of(toMarketDepthCsvLine(snapshot)),
                        StandardCharsets.UTF_8,
                        StandardOpenOption.CREATE,
                        StandardOpenOption.APPEND
                );
            } catch (IOException ex) {
                log("Warn: failed to append market depth tick for '" + key.getTicker() + "': " + ex.getMessage());
            }
        }
    }

    private String toMarketDepthCsvLine(MarketDepthSnapshot snapshot) {
        return String.join(",",
                formatMarketDepthTime(snapshot.getTime()),
                toCsvValue(snapshot.getBestBid()),
                toCsvValue(snapshot.getBestAsk()),
                toCsvValue(snapshot.getMidPrice()),
                quoteCsv(formatMarketDepthLevels(snapshot.getBids())),
                quoteCsv(formatMarketDepthLevels(snapshot.getAsks()))
        );
    }

    private String formatMarketDepthTime(Instant time) {
        return MARKET_DEPTH_TICKS_TIME_FORMATTER.format(time.atZone(ZoneId.systemDefault()));
    }

    private String toCsvValue(Double value) {
        return value == null ? "" : Double.toString(value);
    }

    private String formatMarketDepthLevels(List<MarketDepthLevel> levels) {
        return levels.stream()
                .map(it -> Double.toString(it.getPrice()) + ":" + it.getQuantity())
                .collect(Collectors.joining("|"));
    }

    private String quoteCsv(String value) {
        return '"' + value.replace("\"", "\"\"") + '"';
    }

    private Map<String, Map<Double, Integer>> toCurrentPrices(MarketDepthSnapshot snapshot, TickerInfo.Key key, boolean isPrintGlass) {
        Map<String, Map<Double, Integer>> currentPrices = toCurrentPrices(snapshot);
        if (isPrintGlass) {
            synchronized (this) {
                printGlassOfPrices(key.getTicker(), currentPrices);
            }
        }
        pricesRepository.insert(key, currentPrices);
        return currentPrices;
    }

    private Map<String, Map<Double, Integer>> toCurrentPrices(MarketDepthSnapshot snapshot) {
        Map<String, Map<Double, Integer>> currentPrices = new TreeMap<>();
        Map<Double, Integer> bidsValues = new LinkedHashMap<>(snapshot.getBids().size());
        snapshot.getBids().forEach(it -> bidsValues.put(it.getPrice(), it.getQuantity()));
        currentPrices.put("bids", bidsValues);

        Map<Double, Integer> asksValues = new LinkedHashMap<>(snapshot.getAsks().size());
        snapshot.getAsks().forEach(it -> asksValues.put(it.getPrice(), it.getQuantity()));
        currentPrices.put("asks", asksValues);
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

    public double convertQuotationToDouble(Quotation quotation) {
        return toDouble(quotation);
    }

    private static Double toDouble(long units, int nano) {
        double fractional = nano / 1_000_000_000.0;
        return units + fractional;
    }

    private static Double normalizePrice(double price, double priceStep) {
        return Math.round(price / priceStep) * priceStep;
    }

    private void log(String message) {
        out.println("[" + LOG_TIME_FORMAT.get().format(new Date()) + "] " + message);
    }
}
