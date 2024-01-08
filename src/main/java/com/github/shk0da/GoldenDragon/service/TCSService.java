package com.github.shk0da.GoldenDragon.service;

import com.github.shk0da.GoldenDragon.config.MainConfig;
import com.github.shk0da.GoldenDragon.config.MarketConfig;
import com.github.shk0da.GoldenDragon.model.PositionInfo;
import com.github.shk0da.GoldenDragon.model.TickerInfo;
import com.github.shk0da.GoldenDragon.model.TickerType;
import com.github.shk0da.GoldenDragon.repository.FigiRepository;
import com.github.shk0da.GoldenDragon.repository.PricesRepository;
import com.github.shk0da.GoldenDragon.repository.Repository;
import com.github.shk0da.GoldenDragon.repository.TickerRepository;
import ru.tinkoff.piapi.contract.v1.Bond;
import ru.tinkoff.piapi.contract.v1.Currency;
import ru.tinkoff.piapi.contract.v1.Etf;
import ru.tinkoff.piapi.contract.v1.GetOrderBookResponse;
import ru.tinkoff.piapi.contract.v1.Order;
import ru.tinkoff.piapi.contract.v1.OrderDirection;
import ru.tinkoff.piapi.contract.v1.OrderType;
import ru.tinkoff.piapi.contract.v1.PostOrderResponse;
import ru.tinkoff.piapi.contract.v1.Quotation;
import ru.tinkoff.piapi.contract.v1.Share;
import ru.tinkoff.piapi.core.InvestApi;
import ru.tinkoff.piapi.core.models.Money;
import ru.tinkoff.piapi.core.models.Portfolio;
import ru.tinkoff.piapi.core.models.Positions;

import java.math.BigDecimal;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

import static com.github.shk0da.GoldenDragon.config.MainConfig.dateFormat;
import static com.github.shk0da.GoldenDragon.dictionary.CurrenciesDictionary.getTickerName;
import static com.github.shk0da.GoldenDragon.utils.PrintUtils.printGlassOfPrices;
import static java.lang.Math.round;
import static java.lang.System.out;
import static ru.tinkoff.piapi.contract.v1.OrderDirection.ORDER_DIRECTION_BUY;
import static ru.tinkoff.piapi.contract.v1.OrderDirection.ORDER_DIRECTION_SELL;

public class TCSService {

    private final MainConfig mainConfig;
    private final MarketConfig marketConfig;
    private final InvestApi investApi;

    private final Repository<TickerInfo.Key, String> figiRepository = FigiRepository.INSTANCE;
    private final Repository<TickerInfo.Key, TickerInfo> tickerRepository = TickerRepository.INSTANCE;
    private final Repository<TickerInfo.Key, Map<String, Map<Double, Integer>>> pricesRepository = PricesRepository.INSTANCE;

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

    public boolean sellAllByMarket(String name, TickerType type) {
        int count = getCountOfCurrentPositions(type, name);
        if (count > 0) {
            String currentDate = dateFormat.format(new Date());
            out.println("[" + currentDate + "] Sell: " + count + " " + name + " by Market");
            return 1 == createOrder(new TickerInfo.Key(name, type), 0.0, count, "Sell");
        }
        return true;
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

        String currentDate = dateFormat.format(new Date());
        out.println("[" + currentDate + "] Sell: " + count + " " + key.getTicker() + " by " + tickerPrice + " (" + cost + " " + currency + ")");
        if (mainConfig.isTestMode()) {
            return true;
        }
        return 1 == createOrder(key, tickerPrice, count, "Sell");
    }

    public boolean buy(String name, TickerType type, double cashToBuy) {
        var key = new TickerInfo.Key(name, type);

        String basicCurrency = marketConfig.getCurrency();
        String currency = searchTicker(key).getCurrency();
        if (!basicCurrency.equals(currency)) {
            cashToBuy = convertCurrencies(currency, basicCurrency, cashToBuy);
        }

        int value = 0;
        double tickerPrice = 0.0;
        for (Map.Entry<Double, Integer> ask : getCurrentPrices(key).get("asks").entrySet()) {
            tickerPrice = ask.getKey();
            value = value + ask.getValue();
            if (value >= (cashToBuy / tickerPrice)) break;
        }

        if (0.0 == tickerPrice) {
            out.println("Warn: purchase will be skipped - " + name + " by price " + tickerPrice);
            return false;
        }

        int count = 0;
        if (cashToBuy >= tickerPrice) {
            int lot = searchTicker(key).getLot();
            count = (int) (cashToBuy / tickerPrice);
            while (count % lot != 0 && count > 0) {
                count = count - 1;
            }
        }
        if (count == 0) {
            out.println("Warn: purchase will be skipped - " + name + " with count " + count);
            return false;
        }
        double cost = count * tickerPrice;

        String currentDate = dateFormat.format(new Date());
        out.println("[" + currentDate + "] Buy: " + count + " " + key.getTicker() + " by " + tickerPrice + " (" + cost + " " + currency + ")");
        if (mainConfig.isTestMode()) {
            return true;
        }
        return 1 == createOrder(key, tickerPrice, count, "Buy");
    }

    public int createOrder(TickerInfo.Key key, double price, int count, String operation) {
        String figi = figiByName(key);
        int lot = searchTicker(key).getLot();
        int quantity = (count / lot);
        OrderDirection direction = "Buy".equals(operation) ? ORDER_DIRECTION_BUY : ORDER_DIRECTION_SELL;
        OrderType type = OrderType.ORDER_TYPE_MARKET;
        Quotation orderPrice = Quotation.newBuilder().build();
        if (price > 0.0) {
            type = OrderType.ORDER_TYPE_LIMIT;
            orderPrice = Quotation.newBuilder()
                    .setUnits(Math.round((price - (price % 1))))
                    .setNano((int) (Math.round((price % 1) * 100)))
                    .build();
        }
        PostOrderResponse response = investApi.getOrdersService().postOrderSync(
                figi, quantity, orderPrice, direction, mainConfig.getTcsAccountId(), type, null
        );

        switch (response.getExecutionReportStatus()) {
            case EXECUTION_REPORT_STATUS_NEW:
            case EXECUTION_REPORT_STATUS_FILL:
            case EXECUTION_REPORT_STATUS_PARTIALLYFILL: {
                out.printf(
                        "Created %s order=%s [status=%s, price=%f, commission=%f]: %s\n",
                        response.getDirection().name(),
                        response.getOrderId(),
                        response.getExecutionReportStatus(),
                        toDouble(response.getExecutedOrderPrice().getUnits(), response.getExecutedOrderPrice().getNano()),
                        toDouble(response.getExecutedCommission().getUnits(), response.getExecutedCommission().getNano()),
                        response.getMessage()
                );
                return 1;
            }
            default: {
                out.println("Failed create order: " + response.getMessage());
                return 0;
            }
        }
    }

    public Map<TickerInfo.Key, TickerInfo> getStockList() {
        out.println("Loading current stocks from TCS...");
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
        out.println("Loading current bonds from TCS...");
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
        out.println("Loading current etfs from TCS...");
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
        out.println("Loading current currencies from TCS...");
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

    public Double getAvailableCash() {
        out.println("Loading currencies from TCS...");
        try {
            TimeUnit.MILLISECONDS.sleep(550);
        } catch (InterruptedException ex) {
            out.println("Error: " + ex.getMessage());
        }

        Positions positions = investApi.getOperationsService().getPositionsSync(mainConfig.getTcsAccountId());
        double availableCash = positions.getMoney()
                .stream()
                .filter(it -> marketConfig.getCurrency().equalsIgnoreCase(it.getCurrency()))
                .map(Money::getValue)
                .findFirst()
                .orElse(BigDecimal.ZERO)
                .doubleValue();
        out.println(mainConfig.getTcsAccountId() + ": " + availableCash + " " + marketConfig.getCurrency());
        out.println();
        return availableCash;
    }

    public TickerInfo searchTicker(TickerInfo.Key key) {
        if (tickerRepository.containsKey(key)) {
            return tickerRepository.getById(key);
        }
        out.println("Search ticker '" + key.getTicker() + "' from TCS...");
        try {
            TimeUnit.MILLISECONDS.sleep(550);
        } catch (InterruptedException ex) {
            out.println("Error: " + ex.getMessage());
        }

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
            case UNKNOWN:
            default:
                throw new RuntimeException("Ticker '" + key.getTicker() + "' not found in TCS");
        }

        tickerRepository.insert(key, tickerInfo);
        out.println(key.getTicker() + ": " + tickerInfo);
        return tickerInfo;
    }

    public double getTotalPortfolioCost() {
        return investApi.getOperationsService()
                .getPortfolioSync(mainConfig.getTcsAccountId())
                .getTotalAmountPortfolio()
                .getValue()
                .doubleValue();
    }

    public int getCountOfCurrentPositions() {
        return investApi.getOperationsService()
                .getPortfolioSync(mainConfig.getTcsAccountId())
                .getPositions()
                .size();
    }

    public int getCountOfCurrentPositions(TickerType tickerType, String tickerName) {
        return getCurrentPositions(tickerType).values()
                .stream()
                .filter(it -> it.getTicker().equalsIgnoreCase(tickerName))
                .map(it -> it.getLots())
                .findFirst()
                .orElse(0);
    }

    public Map<TickerInfo.Key, PositionInfo> getCurrentPositions(TickerType tickerType) {
        out.println("Loading current positions from TCS...");
        try {
            TimeUnit.MILLISECONDS.sleep(550);
        } catch (InterruptedException ex) {
            out.println("Error: " + ex.getMessage());
        }

        Map<TickerInfo.Key, PositionInfo> positionInfoList = new HashMap<>();
        String type = TickerType.STOCK == tickerType ? "share" : tickerType.name();
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
                    TickerInfo tickerInfo = searchTicker(tickerKey.get());
                    if (marketConfig.getCurrency().equals(tickerInfo.getCurrency())) {
                        PositionInfo positionInfo = new PositionInfo(
                                tickerInfo.getFigi(),
                                tickerInfo.getTicker(),
                                tickerInfo.getIsin(),
                                tickerInfo.getType().name(),
                                it.getQuantity().intValue(),
                                0,
                                it.getQuantityLots().intValue(),
                                tickerInfo.getName()
                        );
                        positionInfoList.put(tickerKey.get(), positionInfo);
                    }
                });
        return positionInfoList;
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

    public double getAvailablePrice(TickerInfo.Key key) {
        return getAvailablePrice(key, 1, false);
    }

    public double getAvailablePrice(TickerInfo.Key key, int count, boolean isPrintGlass) {
        int value = count;
        double tickerPrice = 0.0;
        for (Map.Entry<Double, Integer> bid : getCurrentPrices(key, isPrintGlass).get("bids").entrySet()) {
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
            out.println("Loading current price '" + key + "' from TCS...");
        }
        try {
            TimeUnit.MILLISECONDS.sleep(550);
        } catch (InterruptedException ex) {
            out.println("Error: " + ex.getMessage());
        }

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
            printGlassOfPrices(key.getTicker(), currentPrices);
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

    private Double toDouble(Quotation quotation) {
        return toDouble(quotation.getUnits(), quotation.getNano());
    }

    private Double toDouble(long units, int nano) {
        return units + Double.parseDouble("0." + nano);
    }
}
