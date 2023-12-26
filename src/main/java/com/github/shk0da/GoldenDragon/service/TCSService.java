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
import ru.tinkoff.piapi.core.models.Position;
import ru.tinkoff.piapi.core.models.Positions;

import java.math.BigDecimal;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

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

    public int createOrder(TickerInfo.Key key, double price, int count, String operation) {
        String figi = figiByName(key);
        int lot = searchTicker(key).getLot();
        int quantity = (count / lot);
        Quotation orderPrice = Quotation.newBuilder()
                .setUnits((long) (price - (price % 1)))
                .setNano((int) (price % 1))
                .build();
        OrderDirection direction = "Buy".equals(operation) ? ORDER_DIRECTION_BUY : ORDER_DIRECTION_SELL;
        OrderType type = price >= 0.0 ? OrderType.ORDER_TYPE_LIMIT : OrderType.ORDER_TYPE_MARKET;
        PostOrderResponse response = investApi.getOrdersService().postOrderSync(
                figi, quantity, orderPrice, direction, mainConfig.getTcsAccountId(), type, null
        );

        switch (response.getExecutionReportStatus()) {
            case EXECUTION_REPORT_STATUS_NEW:
            case EXECUTION_REPORT_STATUS_FILL:
            case EXECUTION_REPORT_STATUS_PARTIALLYFILL: {
                out.println("Created order: " + response.getOrderId());
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

    public Map<TickerInfo.Key, PositionInfo> getCurrentPositions(TickerType tickerType) {
        out.println("Loading current positions from TCS...");
        try {
            TimeUnit.MILLISECONDS.sleep(550);
        } catch (InterruptedException ex) {
            out.println("Error: " + ex.getMessage());
        }

        Map<TickerInfo.Key, PositionInfo> positionInfoList = new HashMap<>();
        Portfolio portfolio = investApi.getOperationsService().getPortfolioSync(mainConfig.getTcsAccountId());
        portfolio.getPositions()
                .stream()
                .filter(it -> TickerType.ALL == tickerType || tickerType.name().equalsIgnoreCase(it.getInstrumentType()))
                .forEach(it -> {
                    AtomicReference<TickerInfo.Key> tickerKey = new AtomicReference<>();
                    figiRepository.getAll().forEach((key, value) -> {
                        if (value.equals(it.getFigi())) {
                            tickerKey.set(key);
                        }
                    });
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
        return quotation.getUnits() + Double.parseDouble("0." + quotation.getNano());
    }
}
