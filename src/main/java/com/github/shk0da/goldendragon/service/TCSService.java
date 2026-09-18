package com.github.shk0da.goldendragon.service;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.shk0da.goldendragon.config.MainConfig;
import com.github.shk0da.goldendragon.model.Candle;
import com.github.shk0da.goldendragon.model.MarketDepthLevel;
import com.github.shk0da.goldendragon.model.MarketDepthSnapshot;
import com.github.shk0da.goldendragon.model.OrderExecutionResult;
import com.github.shk0da.goldendragon.model.Position;
import com.github.shk0da.goldendragon.model.PositionInfo;
import com.github.shk0da.goldendragon.model.TickerInfo;
import com.github.shk0da.goldendragon.model.TickerType;
import com.github.shk0da.goldendragon.repository.FigiRepository;
import com.github.shk0da.goldendragon.repository.PricesRepository;
import com.github.shk0da.goldendragon.repository.Repository;
import com.github.shk0da.goldendragon.repository.TickerRepository;
import com.github.shk0da.goldendragon.utils.MoneyUtils;
import com.github.shk0da.goldendragon.utils.TinkoffApiUrlResolver;
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
import ru.tinkoff.piapi.contract.v1.SecurityTradingStatus;
import ru.tinkoff.piapi.contract.v1.Share;
import ru.tinkoff.piapi.contract.v1.StopOrderDirection;
import ru.tinkoff.piapi.contract.v1.StopOrderType;
import ru.tinkoff.piapi.core.InvestApi;
import ru.tinkoff.piapi.core.models.Money;
import ru.tinkoff.piapi.core.models.Portfolio;
import ru.tinkoff.piapi.core.models.Positions;
import ru.tinkoff.piapi.core.stream.MarketDataSubscriptionService;

import java.io.IOException;
import java.math.BigDecimal;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.time.Duration;
import java.time.Instant;
import java.time.OffsetDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

import static com.github.shk0da.goldendragon.dictionary.CurrenciesDictionary.getTickerName;
import static com.github.shk0da.goldendragon.utils.LoggingUtils.log;
import static com.github.shk0da.goldendragon.utils.PrintUtils.printGlassOfPrices;
import static com.github.shk0da.goldendragon.utils.TimeUtils.sleep;
import static java.lang.Math.max;
import static java.lang.Math.round;
import static java.util.Collections.emptyList;
import static java.util.stream.Collectors.toCollection;
import static ru.tinkoff.piapi.contract.v1.OrderDirection.ORDER_DIRECTION_BUY;
import static ru.tinkoff.piapi.contract.v1.OrderDirection.ORDER_DIRECTION_SELL;
import static ru.tinkoff.piapi.contract.v1.OrderExecutionReportStatus.EXECUTION_REPORT_STATUS_FILL;
import static ru.tinkoff.piapi.contract.v1.StopOrderDirection.STOP_ORDER_DIRECTION_BUY;
import static ru.tinkoff.piapi.contract.v1.StopOrderDirection.STOP_ORDER_DIRECTION_SELL;
import static ru.tinkoff.piapi.contract.v1.StopOrderType.STOP_ORDER_TYPE_STOP_LOSS;
import static ru.tinkoff.piapi.contract.v1.StopOrderType.STOP_ORDER_TYPE_TAKE_PROFIT;

/**
 * Service for interacting with the Tinkoff Client Solution (TCS) Invest API.
 *
 * <p>Provides methods for retrieving market data, managing orders, tracking positions, subscribing
 * to real-time market streams, and currency conversion.
 */
public class TCSService implements TradingService {

    private static final int MAX_INSTRUMENTS_PER_MARKET_DATA_STREAM = 250;
    private static final long MARKET_DATA_RECOVERY_MIN_INTERVAL_MS = 30_000L;
    private static final String MARKET_DEPTH_TICKS_HEADER =
            "time,best_bid,best_ask,mid_price,bids,asks";
    private static final DateTimeFormatter MARKET_DEPTH_TICKS_TIME_FORMATTER =
            DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm:ss");

    private final MainConfig mainConfig;
    private final InvestApi investApi;
    private final boolean writeMarketDepthTicks;

    private final Repository<TickerInfo.Key, String> figiRepository = FigiRepository.INSTANCE;
    private final Repository<TickerInfo.Key, TickerInfo> tickerRepository =
            TickerRepository.INSTANCE;
    private final Repository<TickerInfo.Key, Map<String, Map<Double, Long>>> pricesRepository =
            PricesRepository.INSTANCE;
    private final Map<TickerInfo.Key, Double> lastExecutedPriceByTicker = new ConcurrentHashMap<>();
    private final Map<TickerInfo.Key, ProtectiveOrders> protectiveOrdersByTicker =
            new ConcurrentHashMap<>();
    private final Map<TickerInfo.Key, MarketDepthSnapshot> marketDepthByTicker =
            new ConcurrentHashMap<>();
    private final Map<String, TickerInfo.Key> marketDataKeyByFigi = new ConcurrentHashMap<>();
    private final Map<String, MarketDataStreamShard> marketDataShardByFigi =
            new ConcurrentHashMap<>();
    private final List<MarketDataStreamShard> marketDataStreamShards = new CopyOnWriteArrayList<>();
    private final AtomicInteger marketDataStreamShardCounter = new AtomicInteger();
    private volatile long lastMarketDataRecoveryMs;
    private final Map<String, Object> marketDepthFileLocks = new ConcurrentHashMap<>();
    private volatile Map<TickerInfo.Key, TickerInfo> cachedStockList;
    private volatile Instant cachedStockListAt;
    private final Map<String, Long> throttledLogLastTime = new ConcurrentHashMap<>();

    /**
     * Creates a new {@code TCSService} initialized with the given configurations.
     *
     * <p>Connects to the TCS Invest API (sandbox or production) based on {@link
     * MainConfig#isSandbox()} and pre-loads common currency FIGI mappings.
     *
     * @param mainConfig application configuration containing API credentials and account settings
     */
    public TCSService(MainConfig mainConfig) {
        this.mainConfig = mainConfig;
        this.investApi =
                mainConfig.isSandbox()
                        ? InvestApi.createSandbox(mainConfig.getTcsApiKey())
                        : InvestApi.create(mainConfig.getTcsApiKey());
        this.writeMarketDepthTicks = mainConfig.isWriteMarketDepthTicks();

        figiRepository.insert(new TickerInfo.Key("RUB", TickerType.CURRENCY), "RUB000UTSTOM");
        figiRepository.insert(new TickerInfo.Key("USD", TickerType.CURRENCY), "BBG0013HGFT4");
        figiRepository.insert(new TickerInfo.Key("EUR", TickerType.CURRENCY), "BBG0013HJJ31");
    }

    /**
     * Logs message with throttling to prevent spam of repeated warnings.
     * Only logs if more than {@code throttleMinutes} have passed since the last log for this key.
     *
     * @param key unique identifier for the log category (e.g., "TMON@_empty_orderbook")
     * @param message message to log
     * @param throttleMinutes minutes to wait between logs for the same key
     */
    private void logThrottled(String key, String message, long throttleMinutes) {
        long now = System.currentTimeMillis();
        long throttleMs = throttleMinutes * 60 * 1000L;
        Long lastTime = throttledLogLastTime.get(key);
        if (lastTime == null || (now - lastTime) >= throttleMs) {
            throttledLogLastTime.put(key, now);
            log(message);
        }
    }

    /**
     * Retrieves historical candles for the given FIGI identifier and time range.
     *
     * @param figi FIGI identifier of the instrument
     * @param start start of the time range (inclusive)
     * @param end end of the time range (inclusive)
     * @param interval candle interval (e.g. "1_MIN", "5_MIN", "HOUR", "1_DAY")
     * @return list of {@link Candle} within the given range
     */
    public List<Candle> getCandles(
            String figi, Instant start, Instant end, String interval) {
        CandleInterval candleInterval = mapInterval(interval);
        return getCandlesWithRetry(figi, start, end, candleInterval)
                .stream()
                .map(TCSService::mapHistoricCandle)
                .collect(Collectors.toList());
    }

    /**
     * Retrieves historical candles using {@link OffsetDateTime} parameters.
     *
     * <p>The offsets are converted to {@link Instant} before calling the API.
     *
     * @param figi FIGI identifier of the instrument
     * @param start start of the time range (inclusive)
     * @param end end of the time range (inclusive)
     * @param interval candle interval (e.g. "1_MIN", "5_MIN", "HOUR", "1_DAY")
     * @return list of {@link Candle} within the given range
     */
    public List<Candle> getCandles(
            String figi, OffsetDateTime start, OffsetDateTime end, String interval) {
        CandleInterval candleInterval = mapInterval(interval);
        return getCandlesWithRetry(figi, start.toInstant(), end.toInstant(), candleInterval)
                .stream()
                .map(TCSService::mapHistoricCandle)
                .collect(Collectors.toList());
    }

    /**
     * Maps a {@link HistoricCandle} from Tinkoff API to a domain {@link Candle}.
     *
     * @param hc the Tinkoff historic candle
     * @return domain candle object
     */
    private static Candle mapHistoricCandle(HistoricCandle hc) {
        return new Candle(
                java.time.Instant.ofEpochSecond(hc.getTime().getSeconds(), hc.getTime().getNanos())
                    .atOffset(java.time.ZoneOffset.UTC)
                    .format(java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")),
                toDouble(hc.getOpen()),
                toDouble(hc.getHigh()),
                toDouble(hc.getLow()),
                toDouble(hc.getClose()),
                hc.getVolume());
    }

    /**
     * Maps a string interval to a {@link CandleInterval} enum.
     *
     * @param interval string interval (e.g. "HOUR", "5_MIN", "1_MIN", "15_MIN", "1_DAY")
     * @return corresponding {@link CandleInterval} enum value
     * @throws IllegalArgumentException if interval is not recognized
     */
    private static CandleInterval mapInterval(String interval) {
        if (interval == null) {
            throw new IllegalArgumentException("Interval cannot be null");
        }
        switch (interval) {
            case "HOUR":
                return CandleInterval.CANDLE_INTERVAL_HOUR;
            case "5_MIN":
                return CandleInterval.CANDLE_INTERVAL_5_MIN;
            case "1_MIN":
                return CandleInterval.CANDLE_INTERVAL_1_MIN;
            case "15_MIN":
                return CandleInterval.CANDLE_INTERVAL_15_MIN;
            case "1_DAY":
                return CandleInterval.CANDLE_INTERVAL_DAY;
            default:
                throw new IllegalArgumentException("Unknown interval: " + interval);
        }
    }

    @Override
    public List<Candle> getCandles(String figi, String interval, int count) {
        CandleInterval candleInterval = mapInterval(interval);
        // Calculate duration based on interval and count
        long durationMinutes;
        switch (candleInterval) {
            case CANDLE_INTERVAL_1_MIN:
                durationMinutes = (count + 1);
                break;
            case CANDLE_INTERVAL_5_MIN:
                durationMinutes = (count + 1) * 5;
                break;
            case CANDLE_INTERVAL_15_MIN:
                durationMinutes = (count + 1) * 15;
                break;
            case CANDLE_INTERVAL_HOUR:
                durationMinutes = (count + 1) * 60;
                break;
            case CANDLE_INTERVAL_DAY:
                durationMinutes = (count + 1) * 24 * 60;
                break;
            default:
                durationMinutes = (count + 1) * 60;
        }

        Instant end = Instant.now();
        Instant start = end.minus(durationMinutes, java.time.temporal.ChronoUnit.MINUTES);

        List<HistoricCandle> candles = getCandlesWithRetry(figi, start, end, candleInterval);

        return candles.stream()
                .map(TCSService::mapHistoricCandle)
                .collect(Collectors.toList());
    }

    /**
     * Returns the last {@code size} hourly candles as domain objects, sorted chronologically.
     *
     * <p>Requests a wider time range than needed and then trims to exactly {@code size} candles
     * sorted from oldest to newest.
     *
     * @param ticker ticker symbol
     * @param type instrument type
     * @param size number of candles to return (must be positive)
     * @return chronologically sorted list of {@link Candle}, or empty list if {@code size <= 0}
     */
    public List<Candle> getLastCandles(String ticker, TickerType type, int size) {
        if (size <= 0) {
            return emptyList();
        }
        TickerInfo.Key key = new TickerInfo.Key(ticker, type);
        String figi = figiByName(key);
        Instant end = Instant.now();
        Instant start = end.minusSeconds(size * 3600L);
        List<HistoricCandle> candles =
                investApi.getMarketDataService()
                        .getCandlesSync(figi, start, end, CandleInterval.CANDLE_INTERVAL_HOUR);
        return candles.stream()
                .sorted(
                        (c1, c2) ->
                                Long.compare(c2.getTime().getSeconds(), c1.getTime().getSeconds()))
                .limit(size)
                .sorted(Comparator.comparingLong(c -> c.getTime().getSeconds()))
                .map(TCSService::mapHistoricCandle)
                .collect(Collectors.toList());
    }

    /**
     * Closes all positions of the given ticker type by market orders.
     *
     * <p>Iterates over all current positions and sends market sell orders for long positions and
     * market buy orders for short positions. Notifies via Telegram in non-test mode.
     *
     * @param type instrument type of positions to close
     */
    public void closeAllByMarket(TickerType type) {
        getCurrentPositions(type)
                .values()
                .forEach(
                        ticker -> {
                            int count = ticker.getBalance();
                            String name = ticker.getTicker();
                            if (count > 0) {
                                String message = formatTradeLog("Sell", name, count, type, "Market");
                                log(message);
                                if (mainConfig.isTestMode()) {
                                    return;
                                }
                                createOrder(new TickerInfo.Key(name, type), 0.0, count, "Sell");
                            }
                            if (count < 0) {
                                String message =
                                        formatTradeLog(
                                                "Buy", name, Math.abs(count), type, "Market");
                                log(message);
                                if (mainConfig.isTestMode()) {
                                    return;
                                }
                                createOrder(
                                        new TickerInfo.Key(name, type),
                                        0.0,
                                        Math.abs(count),
                                        "Buy");
                            }
                            sleep(1_000);
                        });
    }

    /**
     * Closes the entire short position for the given ticker by market order.
     *
     * @param name ticker symbol
     * @param type instrument type
     * @return {@code true} if the short position was closed successfully
     */
    public boolean closeShortByMarket(String name, TickerType type) {
        int count = getCountOfCurrentPositions(type, name);
        if (count < 0) {
            log(formatTradeLog("Buy", name, Math.abs(count), type, "Market"));

            if (mainConfig.isTestMode()) {
                return true;
            }
            return 1 == createOrder(new TickerInfo.Key(name, type), 0.0, Math.abs(count), "Buy");
        }
        return false;
    }

    /**
     * Closes the entire short position and returns execution details including price and count.
     *
     * @param name ticker symbol
     * @param type instrument type
     * @return {@link OrderExecutionResult} with execution details, or a failed result if no short
     *     position exists
     */
    @Override
    public OrderExecutionResult closeShortByMarketWithDetails(String name, TickerType type) {
        int count = getCountOfCurrentPositions(type, name);
        if (count < 0) {
            log(formatTradeLog("Buy", name, Math.abs(count), type, "Market"));

            if (mainConfig.isTestMode()) {
                return OrderExecutionResult.testSuccess(
                        getAvailablePrice(new TickerInfo.Key(name, type)), Math.abs(count));
            }
            return createOrder(
                    new TickerInfo.Key(name, type), 0.0, Math.abs(count), "Buy", 0.0, 0.0, false);
        }
        return OrderExecutionResult.failed();
    }

    @Override
    public OrderExecutionResult closeShortByMarketWithDetails(String name, TickerType type, int quantity) {
        if (quantity > 0) {
            log(formatTradeLog("Buy", name, quantity, type, "Market (partial)"));

            if (mainConfig.isTestMode()) {
                return OrderExecutionResult.testSuccess(
                        getAvailablePrice(new TickerInfo.Key(name, type)), quantity);
            }
            return createOrder(
                    new TickerInfo.Key(name, type), 0.0, quantity, "Buy", 0.0, 0.0, false);
        }
        return OrderExecutionResult.failed();
    }

    /**
     * Closes the entire long position for the given ticker by market order.
     *
     * @param name ticker symbol
     * @param type instrument type
     * @return {@code true} if the long position was closed successfully
     */
    public boolean closeLongByMarket(String name, TickerType type) {
        int count = getCountOfCurrentPositions(type, name);
        if (count > 0) {
            log(formatTradeLog("Sell", name, count, type, "Market"));

            if (mainConfig.isTestMode()) {
                return true;
            }
            return 1 == createOrder(new TickerInfo.Key(name, type), 0.0, count, "Sell");
        }
        return false;
    }

    /**
     * Closes the entire long position and returns execution details including price and count.
     *
     * @param name ticker symbol
     * @param type instrument type
     * @return {@link OrderExecutionResult} with execution details, or a failed result if no long
     *     position exists
     */
    @Override
    public OrderExecutionResult closeLongByMarketWithDetails(String name, TickerType type) {
        int count = getCountOfCurrentPositions(type, name);
        if (count > 0) {
            log(formatTradeLog("Sell", name, count, type, "Market"));

            if (mainConfig.isTestMode()) {
                return OrderExecutionResult.testSuccess(
                        getAvailablePrice(new TickerInfo.Key(name, type)), count);
            }
            return createOrder(new TickerInfo.Key(name, type), 0.0, count, "Sell", 0.0, 0.0, false);
        }
        return OrderExecutionResult.failed();
    }

    @Override
    public OrderExecutionResult closeLongByMarketWithDetails(String name, TickerType type, int quantity) {
        if (quantity > 0) {
            log(formatTradeLog("Sell", name, quantity, type, "Market (partial)"));

            if (mainConfig.isTestMode()) {
                return OrderExecutionResult.testSuccess(
                        getAvailablePrice(new TickerInfo.Key(name, type)), quantity);
            }
            return createOrder(new TickerInfo.Key(name, type), 0.0, quantity, "Sell", 0.0, 0.0, false);
        }
        return OrderExecutionResult.failed();
    }

    /**
     * Sells by market price and returns execution details including price and count.
     *
     * @param name ticker symbol
     * @param type instrument type
     * @param cashToSell amount of cash to sell in the instrument's currency
     * @param takeProfit take-profit distance as percentage, or 0 to skip
     * @param stopLose stop-loss distance as percentage, or 0 to skip
     * @return {@link OrderExecutionResult} with execution details
     */
    public OrderExecutionResult sellByMarketWithDetails(
            String name, TickerType type, double cashToSell, double takeProfit, double stopLose) {
        return sell(name, type, cashToSell, true, takeProfit, stopLose, false);
    }

    /**
     * Sells the given cash amount with the option to use market or limit price, and optional
     * protective orders.
     *
     * <p>Walks through the order book to determine a realistic execution price. Converts currency
     * if the instrument trades in a different currency than the base.
     *
     * @param name ticker symbol
     * @param type instrument type
     * @param cashToSell amount of cash to sell in the instrument's currency
     * @param byMarket if {@code true}, submits a market order; otherwise uses the calculated price
     * @param takeProfit take-profit distance as percentage, or 0 to skip
     * @param stopLose stop-loss distance as percentage, or 0 to skip
     * @param isFullPrice if {@code true}, take-profit and stop-loss are interpreted as absolute
     *     prices
     * @return {@link OrderExecutionResult} with execution details
     */
    public OrderExecutionResult sell(
            String name,
            TickerType type,
            double cashToSell,
            boolean byMarket,
            double takeProfit,
            double stopLose,
            boolean isFullPrice) {
        TickerInfo.Key key = new TickerInfo.Key(name, type);

        String basicCurrency = "RUB";
        String currency = searchTicker(key).getCurrency();
        if (!basicCurrency.equals(currency)) {
            cashToSell = convertCurrencies(currency, basicCurrency, cashToSell);
        }

        long value = 0;
        double tickerPrice = 0.0;
        for (Map.Entry<Double, Long> bid : getCurrentPrices(key, false).get("bids").entrySet()) {
            tickerPrice = bid.getKey();
            value = value + bid.getValue();
            if (value >= (cashToSell / tickerPrice)) {
                break;
            }
        }

        if (0.0 == tickerPrice) {
            // Fallback to portfolio average price if order book is empty (e.g., illiquid ETF like TMON@)
            PositionInfo positionInfo = getCurrentPositions(type, name);
            if (positionInfo != null && positionInfo.getAveragePositionPrice() != null && positionInfo.getAveragePositionPrice() > 0) {
                tickerPrice = positionInfo.getAveragePositionPrice();
                log("Using portfolio average price for " + name + ": " + tickerPrice);
            }
        }

        if (0.0 == tickerPrice) {
            log("Warn: short will be skipped - " + name + " by price " + tickerPrice);
            return OrderExecutionResult.failed();
        }

        int unitsCount = calculateTradeCount(key, cashToSell, tickerPrice);
        if (unitsCount == 0) {
            log(
                    "Warn: short will be skipped - "
                            + name
                            + " with count "
                            + unitsCount
                            + ". CashToSell: "
                            + cashToSell
                            + ", price: "
                            + tickerPrice);
            return OrderExecutionResult.failed();
        }
        double cost = getRequiredCashForOrder(key, unitsCount, tickerPrice);

        log(
                formatTradeLog(
                        "Sell",
                        key.getTicker(),
                        unitsCount,
                        key.getType(),
                        byMarket
                                ? String.format(
                                        "Market [price=%.4f, cost=%.2f %s, cash=%.2f]",
                                        tickerPrice, cost, currency, cashToSell)
                                : tickerPrice + " (" + cost + " " + currency + ")"));
        if (mainConfig.isTestMode()) {
            return OrderExecutionResult.testSuccess(tickerPrice, unitsCount);
        }
        return createOrder(
                key,
                byMarket ? 0.0 : tickerPrice,
                unitsCount,
                "Sell",
                takeProfit,
                stopLose,
                isFullPrice,
                cashToSell);
    }

    /**
     * Sells instruments worth the specified cost, calculating quantity based on available price.
     *
     * <p>Rounds the count down to a multiple of the lot size. Converts currency if needed.
     *
     * @param name ticker symbol
     * @param type instrument type
     * @param cost total cost in the instrument's currency
     * @return {@code true} if the sell order was executed successfully
     */
    public boolean sell(String name, TickerType type, double cost) {
        if (cost == 0) {
            log("Warn: sale will be skipped - " + name + " with cost " + cost);
            return false;
        }

        TickerInfo.Key key = new TickerInfo.Key(name, type);

        String basicCurrency = "RUB";
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

        log(
                formatTradeLog(
                        "Sell",
                        key.getTicker(),
                        count,
                        key.getType(),
                        tickerPrice + " (" + cost + " " + currency + ")"));
        if (mainConfig.isTestMode()) {
            return true;
        }
        return 1 == createOrder(key, tickerPrice, count, "Sell");
    }

    /**
     * Buys by market price and returns execution details including price and count.
     *
     * @param name ticker symbol
     * @param type instrument type
     * @param cashToBuy amount of cash to spend in the instrument's currency
     * @param takeProfit take-profit distance as percentage, or 0 to skip
     * @param stopLose stop-loss distance as percentage, or 0 to skip
     * @return {@link OrderExecutionResult} with execution details
     */
    public OrderExecutionResult buyByMarketWithDetails(
            String name, TickerType type, double cashToBuy, double takeProfit, double stopLose) {
        return buy(name, type, cashToBuy, true, takeProfit, stopLose, false);
    }

    /**
     * Buys the given cash amount at the best available limit price without protective orders.
     *
     * @param name ticker symbol
     * @param type instrument type
     * @param cashToBuy amount of cash to spend in the instrument's currency
     * @return {@code true} if the buy order was executed successfully
     */
    public boolean buy(String name, TickerType type, double cashToBuy) {
        return buy(name, type, cashToBuy, false, 0.0, 0.0, false).isSuccess();
    }

    /**
     * Buys the given cash amount with the option to use market or limit price, and optional
     * protective orders.
     *
     * <p>Walks through the order book to determine a realistic execution price. Converts currency
     * if the instrument trades in a different currency than the base.
     *
     * @param name ticker symbol
     * @param type instrument type
     * @param cashToBuy amount of cash to spend in the instrument's currency
     * @param byMarket if {@code true}, submits a market order; otherwise uses the calculated price
     * @param takeProfit take-profit distance as percentage, or 0 to skip
     * @param stopLose stop-loss distance as percentage, or 0 to skip
     * @param isFullPrice if {@code true}, take-profit and stop-loss are interpreted as absolute
     *     prices
     * @return {@link OrderExecutionResult} with execution details
     */
    public OrderExecutionResult buy(
            String name,
            TickerType type,
            double cashToBuy,
            boolean byMarket,
            double takeProfit,
            double stopLose,
            boolean isFullPrice) {
        TickerInfo.Key key = new TickerInfo.Key(name, type);

        String basicCurrency = "RUB";
        String currency = searchTicker(key).getCurrency();
        if (!basicCurrency.equals(currency)) {
            double convertedCashToBuy = convertCurrencies(basicCurrency, currency, cashToBuy);
            log(
                    String.format(
                            "Buy cash conversion [%s]: baseCurrency=%s instrumentCurrency=%s before=%.5f after=%.5f",
                            name, basicCurrency, currency, cashToBuy, convertedCashToBuy));
            cashToBuy = convertedCashToBuy;
        }

        Map<String, Map<Double, Long>> currentPrices = getCurrentPrices(key, false);
        long value = 0;
        double tickerPrice = 0.0;
        for (Map.Entry<Double, Long> ask : currentPrices.get("asks").entrySet()) {
            tickerPrice = ask.getKey();
            value = value + ask.getValue();
            if (value >= (cashToBuy / tickerPrice)) {
                break;
            }
        }

        if (0.0 == tickerPrice) {
            logThrottled(
                    name + "_empty_orderbook",
                    "Warn: purchase will be skipped - "
                            + name
                            + " due to empty asks in order book"
                            + " [asks="
                            + currentPrices.get("asks").size()
                            + ", bids="
                            + currentPrices.get("bids").size()
                            + ", cashToBuy="
                            + cashToBuy
                            + "]",
                    5);
            return OrderExecutionResult.failed();
        }

        int unitsCount = calculateTradeCount(key, cashToBuy, tickerPrice);
        if (unitsCount == 0) {
            log(
                    "Warn: long will be skipped - "
                            + name
                            + " with count "
                            + unitsCount
                            + ". CashToBuy: "
                            + cashToBuy
                            + ", price: "
                            + tickerPrice);
            return OrderExecutionResult.failed();
        }
        double cost = getRequiredCashForOrder(key, unitsCount, tickerPrice);

        log(
                formatTradeLog(
                        "Buy",
                        key.getTicker(),
                        unitsCount,
                        key.getType(),
                        byMarket
                                ? String.format(
                                        "Market [price=%.4f, cost=%.2f %s, cash=%.2f]",
                                        tickerPrice, cost, currency, cashToBuy)
                                : tickerPrice + " (" + cost + " " + currency + ")"));
        if (mainConfig.isTestMode()) {
            return OrderExecutionResult.testSuccess(tickerPrice, unitsCount);
        }
        return createOrder(
                key,
                byMarket ? 0.0 : tickerPrice,
                unitsCount,
                "Buy",
                takeProfit,
                stopLose,
                isFullPrice,
                cashToBuy);
    }

    /**
     * Creates an order and returns {@code 1} on success or {@code 0} on failure.
     *
     * <p>Convenience method that delegates to the full {@link #createOrder} overload without
     * protective orders or cash tracking.
     *
     * @param key ticker key identifying the instrument
     * @param price order price, or 0 for a market order
     * @param count number of instruments to trade
     * @param operation "Buy" or "Sell"
     * @return {@code 1} if the order was filled, {@code 0} otherwise
     */
    public int createOrder(TickerInfo.Key key, double price, int count, String operation) {
        return createOrder(key, price, count, operation, 0.0, 0.0, false).isSuccess() ? 1 : 0;
    }

    /**
     * Creates an order with optional protective (bracket) orders.
     *
     * <p>Convenience method that delegates to the full overload without cash tracking.
     *
     * @param key ticker key identifying the instrument
     * @param price order price, or 0 for a market order
     * @param count number of instruments to trade
     * @param operation "Buy" or "Sell"
     * @param takeProfit take-profit distance as percentage, or 0 to skip
     * @param stopLose stop-loss distance as percentage, or 0 to skip
     * @param isFullPrice if {@code true}, take-profit and stop-loss are interpreted as absolute
     *     prices
     * @return {@link OrderExecutionResult} with execution details
     */
    public OrderExecutionResult createOrder(
            TickerInfo.Key key,
            double price,
            int count,
            String operation,
            double takeProfit,
            double stopLose,
            boolean isFullPrice) {
        return createOrder(key, price, count, operation, takeProfit, stopLose, isFullPrice, 0.0);
    }

    /**
     * Creates an order with the full set of parameters including cash tracking and protective
     * orders.
     *
     * <p>Submits the order to the TCS API, normalizes the execution price, places take-profit and
     * stop-loss bracket orders if specified, and notifies via Telegram.
     *
     * @param key ticker key identifying the instrument
     * @param price order price, or 0 for a market order
     * @param count number of instruments to trade
     * @param operation "Buy" or "Sell"
     * @param takeProfit take-profit distance as percentage, or 0 to skip
     * @param stopLose stop-loss distance as percentage, or 0 to skip
     * @param isFullPrice if {@code true}, take-profit and stop-loss are interpreted as absolute
     *     prices
     * @param cashToUse intended cash amount for the order (used for logging)
     * @return {@link OrderExecutionResult} with execution price, count, commission, and protective
     *     position
     */
    public OrderExecutionResult createOrder(
            TickerInfo.Key key,
            double price,
            int count,
            String operation,
            double takeProfit,
            double stopLose,
            boolean isFullPrice,
            double cashToUse) {
        String figi = figiByName(key);
        TickerInfo tickerInfo = searchTicker(key);
        int lotSize = tickerInfo.getLot();
        int normalizedCount = normalizeOrderCount(count, lotSize);
        int contractUnits = getContractUnits();
        if (normalizedCount <= 0 || count <= 0) {
            log(
                    String.format(
                            "Skip create order [%s]: invalid normalizedCount=%d, lot=%d, contractUnits=%d, quantity=%d",
                            key.getTicker(), normalizedCount, lotSize, contractUnits, count));
            return OrderExecutionResult.failed();
        }
        OrderDirection direction =
                "Buy".equals(operation) ? ORDER_DIRECTION_BUY : ORDER_DIRECTION_SELL;
        OrderType type = OrderType.ORDER_TYPE_MARKET;
        Quotation orderPrice = Quotation.newBuilder().build();
        if (price > 0.0) {
            type = OrderType.ORDER_TYPE_LIMIT;
            orderPrice = createQuotation(price);
        }
        double referencePrice =
                price > 0.0
                        ? price
                        : getAvailablePrice(
                                key,
                                Math.max(1, normalizedCount),
                                ORDER_DIRECTION_BUY == direction ? "asks" : "bids",
                                false);
        double fullNotional = referencePrice > 0.0 ? normalizedCount * referencePrice : 0.0;
        double estimatedCost =
                referencePrice > 0.0
                        ? getRequiredCashForOrder(key, normalizedCount, referencePrice)
                        : 0.0;
        log(
                String.format(
                        "Create order request [%s]: figi=%s accountId=%s operation=%s direction=%s type=%s quantity=%d lot=%d lots=%d requestedPrice=%.4f referencePrice=%.4f fullNotional=%.2f cashToUse=%.2f estimatedCost=%.2f takeProfit=%.4f stopLose=%.4f isFullPrice=%s",
                        key.getTicker(),
                        figi,
                        mainConfig.getTcsAccountId(),
                        operation,
                        direction,
                        type,
                        normalizedCount,
                        lotSize,
                        count,
                        price,
                        referencePrice,
                        fullNotional,
                        cashToUse,
                        estimatedCost,
                        takeProfit,
                        stopLose,
                        isFullPrice));

        try {
            PostOrderResponse response =
                    investApi
                            .getOrdersService()
                            .postOrderSync(
                                    figi,
                                    normalizedCount,
                                    orderPrice,
                                    direction,
                                    mainConfig.getTcsAccountId(),
                                    type,
                                    null);
            int executedLots = Math.toIntExact(response.getLotsExecuted());
            int executedCount =
                    executedLots > 0 ? executedLots * lotSize * contractUnits : normalizedCount;
            double rawExecutedPrice =
                    toDouble(
                            response.getExecutedOrderPrice().getUnits(),
                            response.getExecutedOrderPrice().getNano());
            referencePrice =
                    price > 0.0
                            ? price
                            : getAvailablePrice(
                                    key,
                                    Math.max(1, executedCount),
                                    ORDER_DIRECTION_BUY == direction ? "asks" : "bids",
                                    false);
            double executedPrice =
                    normalizeExecutedPrice(
                            rawExecutedPrice,
                            executedCount,
                            lotSize,
                            referencePrice,
                            tickerInfo.getMinPriceIncrement());
            if (executedPrice <= 0.0) {
                executedPrice = referencePrice > 0.0 ? referencePrice : getAvailablePrice(key);
            }
            lastExecutedPriceByTicker.put(key, executedPrice);
            log(
                    String.format(
                            "%s execution price normalized for %s: raw=%f normalized=%f reference=%f executedCount=%d lot=%d lots=%d",
                            operation,
                            key.getTicker(),
                            rawExecutedPrice,
                            executedPrice,
                            referencePrice,
                            executedCount,
                            lotSize,
                            Math.max(1, executedCount / lotSize)));

            double executedCommission =
                    toDouble(
                            response.getExecutedCommission().getUnits(),
                            response.getExecutedCommission().getNano());
            String executionType =
                    price > 0.0
                            ? String.format("%.6f (%.6f)", price, executedCount * price)
                            : "Market";
            String message =
                    String.format(
                            "%s %s count=%d lot=%d lots=%d by %s: %s [order=%s, status=%s, price=%f, commission=%f]\n",
                            operation,
                            key.getTicker(),
                            executedCount,
                            lotSize,
                            Math.max(1, executedCount / lotSize),
                            executionType,
                            response.getMessage(),
                            response.getOrderId(),
                            response.getExecutionReportStatus(),
                            executedPrice,
                            executedCommission);
            log(message);

            Position bracketPosition = null;
            if (response.getExecutionReportStatus().equals(EXECUTION_REPORT_STATUS_FILL)) {
                bracketPosition =
                        createProtectivePosition(
                                direction,
                                executedPrice,
                                stopLose,
                                takeProfit,
                                isFullPrice,
                                executedCount,
                                tickerInfo);
                int lots = Math.max(1, executedCount / lotSize);
                placeOrLogProtectiveOrders(figi, response.getOrderId(), lots, key, direction, bracketPosition);
            }

            return OrderExecutionResult.success(
                    executedPrice, executedCount, executedCommission, bracketPosition);
        } catch (Exception ex) {
            String errorDetail = ex.getClass().getSimpleName();
            int errorCode = 0;
            if (ex.getMessage() != null && !ex.getMessage().isEmpty()) {
                errorDetail += ": " + ex.getMessage();
            }
            if (ex.getCause() != null) {
                errorDetail += " | cause=" + ex.getCause().getClass().getSimpleName()
                        + (ex.getCause().getMessage() != null ? ": " + ex.getCause().getMessage() : "");
            }
            if (ex instanceof ru.tinkoff.piapi.core.exception.ApiRuntimeException) {
                ru.tinkoff.piapi.core.exception.ApiRuntimeException apiEx =
                        (ru.tinkoff.piapi.core.exception.ApiRuntimeException) ex;
                try {
                    errorCode = Integer.parseInt(apiEx.getCode());
                } catch (NumberFormatException ignored) {
                    // code is not numeric, leave errorCode as 0
                }
                errorDetail += " [code=" + errorCode;
                if (apiEx.getMessage() != null && !apiEx.getMessage().isEmpty()) {
                    errorDetail += ", message=" + apiEx.getMessage();
                }
                if (apiEx.getTrackingId() != null) {
                    errorDetail += ", trackingId=" + apiEx.getTrackingId();
                }
                Throwable grpcThrowable = apiEx.getThrowable();
                if (grpcThrowable != null) {
                    io.grpc.Status grpcStatus = io.grpc.Status.fromThrowable(grpcThrowable);
                    if (grpcStatus.getDescription() != null) {
                        errorDetail += ", detail=" + grpcStatus.getDescription();
                    }
                    if (grpcThrowable.getMessage() != null && !grpcThrowable.getMessage().equals(grpcStatus.getDescription())) {
                        errorDetail += ", grpc=" + grpcThrowable.getMessage();
                    }
                }
                errorDetail += "]";
            }
            String message = "Failed create order [" + key.getTicker() + "]: " + errorDetail;
            log(message);
            return errorCode != 0
                    ? OrderExecutionResult.failed(errorCode, errorDetail)
                    : OrderExecutionResult.failed(errorDetail);
        }
    }

    private double normalizeExecutedPrice(
            double rawExecutedPrice,
            int executedCount,
            int lotSize,
            double referencePrice,
            double minPriceIncrement) {
        if (rawExecutedPrice <= 0.0) {
            return 0.0;
        }
        if (executedCount <= 0) {
            return rawExecutedPrice;
        }

        double perUnitFromRaw = rawExecutedPrice;
        double perUnitFromLot = lotSize > 0 ? rawExecutedPrice / lotSize : perUnitFromRaw;
        double perUnitFromTotal = rawExecutedPrice / executedCount;
        double tolerance = max(minPriceIncrement * 10, referencePrice * 0.03);

        boolean rawLooksPerUnit =
                referencePrice > 0.0 && Math.abs(perUnitFromRaw - referencePrice) <= tolerance;
        boolean lotLooksPerUnit =
                referencePrice > 0.0 && Math.abs(perUnitFromLot - referencePrice) <= tolerance;
        boolean totalLooksPerUnit =
                referencePrice > 0.0 && Math.abs(perUnitFromTotal - referencePrice) <= tolerance;

        if (rawLooksPerUnit && !lotLooksPerUnit && !totalLooksPerUnit) {
            return perUnitFromRaw;
        }
        if (!rawLooksPerUnit && lotLooksPerUnit && !totalLooksPerUnit) {
            return perUnitFromLot;
        }
        if (!rawLooksPerUnit && !lotLooksPerUnit && totalLooksPerUnit) {
            return perUnitFromTotal;
        }
        if (rawLooksPerUnit) {
            return perUnitFromRaw;
        }
        if (lotLooksPerUnit) {
            return perUnitFromLot;
        }
        if (totalLooksPerUnit) {
            return perUnitFromTotal;
        }

        if (referencePrice > 0.0) {
            // wide band tolerates illiquid spreads but rejects values that are
            // clearly not per-unit prices (e.g. futures margin totals)
            double wideTolerance = referencePrice * 0.10;
            if (Math.abs(perUnitFromRaw - referencePrice) <= wideTolerance) {
                return perUnitFromRaw;
            }
            if (Math.abs(perUnitFromTotal - referencePrice) <= wideTolerance) {
                return perUnitFromTotal;
            }
            // no candidate resembles the order book price; the reference price
            // is the closest available estimate of the true execution price
            return referencePrice;
        }
        return perUnitFromTotal > 0.0 ? perUnitFromTotal : perUnitFromRaw;
    }

    @Override
    public Double getSingleContractGo(String figi) {
        try {
            String url = TinkoffApiUrlResolver.buildRestUrl(mainConfig, "InstrumentsService/GetFuturesMargin");
            String json = post(url, "{\"figi\": \"" + figi + "\"}", 2, true);
            return json == null ? null : extractMoney(json, "initialMarginOnSell");
        } catch (Exception ex) {
            log("Failed to get margin for futures " + figi + ": " + ex.getMessage());
            return null;
        }
    }

    private String post(String url, String body, int retries, boolean silent) {
        try {
            HttpClient client = HttpClient.newBuilder()
                .connectTimeout(java.time.Duration.ofSeconds(30))
                .build();
            HttpRequest request = HttpRequest.newBuilder()
                .uri(java.net.URI.create(url))
                .timeout(java.time.Duration.ofSeconds(300))
                .header("Content-Type", "application/json")
                .header("Authorization", "Bearer " + mainConfig.getTcsApiKey())
                .POST(HttpRequest.BodyPublishers.ofString(body))
                .build();

            for (int attempt = 0; attempt < retries; attempt++) {
                try {
                    java.net.http.HttpResponse<String> response = client.send(request, java.net.http.HttpResponse.BodyHandlers.ofString());
                    if (response.statusCode() == 200) {
                        return response.body();
                    }
                    if (!silent) {
                        log("HTTP " + response.statusCode() + " attempt " + (attempt + 1));
                    }
                } catch (Exception e) {
                    if (!silent) {
                        log("HTTP request failed (attempt " + (attempt + 1) + "): " + e.getMessage());
                    }
                }
                Thread.sleep(1000);
            }
            return null;
        } catch (Exception ex) {
            if (!silent) {
                log("POST failed to " + url + ": " + ex.getMessage());
            }
            return null;
        }
    }

    private Double extractMoney(String json, String key) {
        if (json == null || json.isEmpty()) {
            return null;
        }
        try {
            com.google.gson.JsonObject obj = com.google.gson.JsonParser.parseString(json).getAsJsonObject();
            if (obj.has(key)) {
                com.google.gson.JsonElement val = obj.get(key);
                if (val.isJsonObject()) {
                    com.google.gson.JsonObject moneyObj = val.getAsJsonObject();
                    long units = moneyObj.has("units") ? moneyObj.get("units").getAsLong() : 0;
                    int nano = moneyObj.has("nano") ? moneyObj.get("nano").getAsInt() : 0;
                    return MoneyUtils.parseUnitsNano(units, nano);
                }
            }
            return null;
        } catch (Exception ex) {
            log("Failed to extract " + key + " from JSON: " + ex.getMessage());
            return null;
        }
    }

    @Override
    public Position restoreProtectivePosition(String name, TickerType type, Position position) {
        boolean sandbox = mainConfig != null && mainConfig.isSandbox();

        if (position == null || position.quantity <= 0) {
            log("restoreProtectivePosition " + name + ": returning position (null/zero qty)");
            return position;
        }

        // In sandbox mode, just return the position without searching for stop orders
        if (sandbox) {
            log("restoreProtectivePosition " + name + ": sandbox=true, returning position qty=" + position.quantity);
            return position;
        }

        TickerInfo.Key key = new TickerInfo.Key(name, type);
        String figi = figiByName(key);
        if (figi == null || figi.isEmpty()) {
            return position;
        }

        try {
            ProtectiveOrders protectiveOrders = new ProtectiveOrders();
            Double stopLoss = null;
            Double takeProfit = null;

            List<ru.tinkoff.piapi.contract.v1.StopOrder> stopOrders =
                    investApi
                            .getStopOrdersService()
                            .getStopOrdersSync(mainConfig.getTcsAccountId());
            for (ru.tinkoff.piapi.contract.v1.StopOrder stopOrder : stopOrders) {
                if (!figi.equals(stopOrder.getFigi())) {
                    continue;
                }

                // Use stopPrice (activation price) instead of price for market stop orders
                double stopPrice =
                        toDouble(stopOrder.getStopPrice().getUnits(), stopOrder.getStopPrice().getNano());
                if ("BUY".equals(position.direction)) {
                    if (position.entryPrice != null && stopPrice <= position.entryPrice) {
                        if (stopLoss == null || stopPrice > stopLoss) {
                            stopLoss = stopPrice;
                            protectiveOrders.stopLossOrderId = stopOrder.getStopOrderId();
                        }
                    } else {
                        if (takeProfit == null || stopPrice < takeProfit) {
                            takeProfit = stopPrice;
                            protectiveOrders.takeProfitOrderId = stopOrder.getStopOrderId();
                        }
                    }
                } else if ("SELL".equals(position.direction)) {
                    if (position.entryPrice != null && stopPrice >= position.entryPrice) {
                        if (stopLoss == null || stopPrice < stopLoss) {
                            stopLoss = stopPrice;
                            protectiveOrders.stopLossOrderId = stopOrder.getStopOrderId();
                        }
                    } else {
                        if (takeProfit == null || stopPrice > takeProfit) {
                            takeProfit = stopPrice;
                            protectiveOrders.takeProfitOrderId = stopOrder.getStopOrderId();
                        }
                    }
                }
            }

            if (protectiveOrders.stopLossOrderId != null
                    || protectiveOrders.takeProfitOrderId != null) {
                protectiveOrdersByTicker.put(key, protectiveOrders);
            }

            if (stopLoss == null && takeProfit == null) {
                return position;
            }

            return new Position(
                    position.direction,
                    position.entryPrice,
                    stopLoss,
                    takeProfit,
                    position.quantity,
                    position.candlesHeld,
                    position.cooldownRemaining);
        } catch (Exception ex) {
            log("Failed to restore protective orders for " + name + ": " + ex.getMessage());
            return position;
        }
    }

    @Override
    public void syncProtectiveOrders(String name, TickerType type, Position position) {
        if (position == null || position.quantity <= 0) {
            return;
        }

        TickerInfo.Key key = new TickerInfo.Key(name, type);
        String figi = figiByName(key);
        if (figi == null || figi.isEmpty()) {
            log("WARN: syncProtectiveOrders - figi not found for " + name);
            return;
        }

        // Get all stop orders from API and find existing for this FIGI
        List<ru.tinkoff.piapi.contract.v1.StopOrder> allStopOrders;
        try {
            allStopOrders = investApi.getStopOrdersService().getStopOrdersSync(mainConfig.getTcsAccountId());
        } catch (Exception ex) {
            log("WARN: syncProtectiveOrders - failed to get stop orders: " + ex.getMessage());
            return;
        }

        // Find existing SL/TP orders for this FIGI
        String existingSlOrderId = null;
        String existingTpOrderId = null;

        for (ru.tinkoff.piapi.contract.v1.StopOrder order : allStopOrders) {
            if (!figi.equals(order.getFigi())) {
                continue;
            }

// Check order type
            if (order.getOrderType() == STOP_ORDER_TYPE_STOP_LOSS) {
                existingSlOrderId = order.getStopOrderId();
                log(name + " | Found existing SL order: " + existingSlOrderId);
            } else if (order.getOrderType() == STOP_ORDER_TYPE_TAKE_PROFIT) {
                existingTpOrderId = order.getStopOrderId();
                log(name + " | Found existing TP order: " + existingTpOrderId);
            }
        }

        // Calculate quantity in lots
        TickerInfo tickerInfo = searchTicker(key);
        int lot = tickerInfo != null && tickerInfo.getLot() != null ? Math.max(1, tickerInfo.getLot()) : 1;
        int quantityLots = position.quantity / lot;

        // Cancel old SL order
        if (existingSlOrderId != null) {
            cancelStopOrder(key, existingSlOrderId, "SL");
            sleep(100);
            if (!verifyOrderCancelled(key, existingSlOrderId, "SL")) {
                log("WARN: " + name + " | SL order cancellation NOT confirmed via API");
            }
        }

        // Cancel old TP order
        if (existingTpOrderId != null) {
            cancelStopOrder(key, existingTpOrderId, "TP");
            sleep(100);
            if (!verifyOrderCancelled(key, existingTpOrderId, "TP")) {
                log("WARN: " + name + " | TP order cancellation NOT confirmed via API");
            }
        }

        // Place new SL order
        if (position.stopLoss != null && position.stopLoss > 0) {
            StopOrderDirection stopOrderDirection = "BUY".equals(position.direction)
                    ? STOP_ORDER_DIRECTION_SELL
                    : STOP_ORDER_DIRECTION_BUY;

            String stopOrderId = postMarketStopOrder(
                    figi,
                    name,
                    quantityLots,
                    position.stopLoss,
                    stopOrderDirection,
                    STOP_ORDER_TYPE_STOP_LOSS);

            if (stopOrderId != null) {
                sleep(100);
                if (verifyOrderExists(key, stopOrderId, "SL")) {
                    log(name + " | SL order UPDATED: lots=" + quantityLots + ", price=" + position.stopLoss);
                } else {
                    log("WARN: " + name + " | SL order placement NOT confirmed via API, orderId=" + stopOrderId);
                }
            } else {
                log("WARN: " + name + " | SL order UPDATE FAILED, lots=" + quantityLots + " left unprotected");
            }
        }

        // Place new TP order
        if (position.takeProfit != null && position.takeProfit > 0) {
            StopOrderDirection stopOrderDirection = "BUY".equals(position.direction)
                    ? STOP_ORDER_DIRECTION_SELL
                    : STOP_ORDER_DIRECTION_BUY;

            String tpOrderId = postMarketStopOrder(
                    figi,
                    name,
                    quantityLots,
                    position.takeProfit,
                    stopOrderDirection,
                    STOP_ORDER_TYPE_TAKE_PROFIT);

            if (tpOrderId != null) {
                sleep(100);
                if (verifyOrderExists(key, tpOrderId, "TP")) {
                    log(name + " | TP order UPDATED: lots=" + quantityLots + ", price=" + position.takeProfit);
                } else {
                    log("WARN: " + name + " | TP order placement NOT confirmed via API, orderId=" + tpOrderId);
                }
            } else {
                log("WARN: " + name + " | TP order UPDATE FAILED, lots=" + quantityLots + " left unprotected");
            }
        }
    }

    /**
     * Verify that a stop order was cancelled by checking GetStopOrders API.
     */
    private boolean verifyOrderCancelled(TickerInfo.Key key, String orderId, String orderTypeName) {
        try {
            List<ru.tinkoff.piapi.contract.v1.StopOrder> stopOrders =
                    investApi.getStopOrdersService().getStopOrdersSync(mainConfig.getTcsAccountId());

            for (ru.tinkoff.piapi.contract.v1.StopOrder order : stopOrders) {
                if (orderId.equals(order.getStopOrderId())) {
                    log("WARN: verifyOrderCancelled - " + orderTypeName + " " + orderId + " still active");
                    return false;
                }
            }
            log(orderTypeName + " " + orderId + " confirmed cancelled via API");
            return true;
        } catch (Exception ex) {
            log("WARN: verifyOrderCancelled failed for " + orderTypeName + ": " + ex.getMessage());
            return false;
        }
    }

    /**
     * Verify that a stop order exists by checking GetStopOrders API.
     */
    private boolean verifyOrderExists(TickerInfo.Key key, String orderId, String orderTypeName) {
        try {
            List<ru.tinkoff.piapi.contract.v1.StopOrder> stopOrders =
                    investApi.getStopOrdersService().getStopOrdersSync(mainConfig.getTcsAccountId());

            for (ru.tinkoff.piapi.contract.v1.StopOrder order : stopOrders) {
                if (orderId.equals(order.getStopOrderId())) {
                    double stopPrice = toDouble(order.getStopPrice().getUnits(), order.getStopPrice().getNano());
                    log(orderTypeName + " " + orderId + " confirmed active via API, price=" + stopPrice);
                    return true;
                }
            }
            log("WARN: verifyOrderExists - " + orderTypeName + " " + orderId + " NOT found in active orders");
            return false;
        } catch (Exception ex) {
            log("WARN: verifyOrderExists failed for " + orderTypeName + ": " + ex.getMessage());
            return false;
        }
    }

    private Position createProtectivePosition(
            OrderDirection direction,
            double executedPrice,
            double stopLose,
            double takeProfit,
            boolean isFullPrice,
            int count,
            TickerInfo tickerInfo) {
        Double stopLossPrice = null;
        Double takeProfitPrice = null;

        if (stopLose > 0.0) {
            double slPrice =
                    ORDER_DIRECTION_BUY == direction
                            ? isFullPrice
                                    ? stopLose
                                    : executedPrice - (executedPrice / 100 * stopLose)
                            : isFullPrice
                                    ? stopLose
                                    : executedPrice + (executedPrice / 100 * stopLose);
            stopLossPrice = normalizePrice(slPrice, tickerInfo.getMinPriceIncrement());
        }

        if (takeProfit > 0.0) {
            double tpPrice =
                    ORDER_DIRECTION_BUY == direction
                            ? isFullPrice
                                    ? takeProfit
                                    : executedPrice + executedPrice / 100 * takeProfit
                            : isFullPrice
                                    ? takeProfit
                                    : executedPrice - executedPrice / 100 * takeProfit;
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
                0);
    }

    private void placeOrLogProtectiveOrders(
        String figi,
        String orderId,
        int quantity,
        TickerInfo.Key key,
        OrderDirection direction,
        Position bracketPosition
    ) {
        if (bracketPosition == null) {
            return;
        }

        if (bracketPosition.stopLoss != null) {
            sleep(1_000);
            StopOrderDirection stopOrderDirection =
                ORDER_DIRECTION_BUY == direction
                    ? STOP_ORDER_DIRECTION_SELL
                    : STOP_ORDER_DIRECTION_BUY;
            double slPrice = bracketPosition.stopLoss;
            String stopOrderId = postMarketStopOrder(
                figi,
                orderId,
                quantity,
                slPrice,
                stopOrderDirection,
                STOP_ORDER_TYPE_STOP_LOSS);
            if (stopOrderId != null) {
                ProtectiveOrders protectiveOrders = protectiveOrdersByTicker.computeIfAbsent(key, ignored -> new ProtectiveOrders());
                protectiveOrders.stopLossOrderId = stopOrderId;
                log(key.getTicker() + " | SL order placed: lots=" + quantity + ", price=" + slPrice);
            } else {
                log("WARN: " + key.getTicker() + " | SL order FAILED, lots=" + quantity + " left unprotected");
            }
        }

        if (bracketPosition.takeProfit != null) {
            StopOrderDirection stopOrderDirection =
                ORDER_DIRECTION_BUY == direction
                    ? STOP_ORDER_DIRECTION_SELL
                    : STOP_ORDER_DIRECTION_BUY;
            if (quantity > 0 && bracketPosition.entryPrice != null) {
                double tpPrice = bracketPosition.takeProfit;
                String tpOrderId = postMarketStopOrder(
                    figi,
                    orderId,
                    quantity,
                    tpPrice,
                    stopOrderDirection,
                    STOP_ORDER_TYPE_TAKE_PROFIT);
                if (tpOrderId != null) {
                    ProtectiveOrders orders = protectiveOrdersByTicker.computeIfAbsent(key, ignored -> new ProtectiveOrders());
                    orders.takeProfitOrderId = tpOrderId;
                    log(key.getTicker() + " | TP order placed: lots=" + quantity + ", price=" + tpPrice);
                } else {
                    log("WARN: " + key.getTicker() + " | TP order FAILED, lots=" + quantity + " left unprotected");
                }
            }
        }
    }

    /**
     * Posts a stop order via REST API that, when triggered, executes a MARKET order.
     * Returns the stop order id or null on failure.
     */
    private String postMarketStopOrder(
            String figi,
            String orderId,
            int quantity,
            double triggerPrice,
            StopOrderDirection direction,
            StopOrderType stopOrderType) {
        try {
            String url = TinkoffApiUrlResolver.buildRestUrl(mainConfig, "StopOrdersService/PostStopOrder");

            String body =
                    "{\"figi\":\"" + figi
                            + "\",\"quantity\":\"" + quantity
                            + "\",\"stopPrice\":"
                            + quotationJson(triggerPrice)
                            + ",\"direction\":\"" + direction.name()
                            + "\",\"accountId\":\"" + mainConfig.getTcsAccountId()
                            + "\",\"expirationType\":\"STOP_ORDER_EXPIRATION_TYPE_GOOD_TILL_CANCEL\""
                            + ",\"stopOrderType\":\"" + stopOrderType.name()
                            + "\",\"instrumentId\":\"" + figi
                            + "\",\"exchangeOrderType\":\"EXCHANGE_ORDER_TYPE_MARKET\""
                            + ",\"orderId\":\"" + UUID.randomUUID() + "\"}";

            HttpRequest request =
                    HttpRequest.newBuilder()
                            .uri(URI.create(url))
                            .header("Authorization", "Bearer " + mainConfig.getTcsApiKey())
                            .header("Content-Type", "application/json")
                            .POST(HttpRequest.BodyPublishers.ofString(body))
                            .build();

            HttpResponse<String> response =
                    MainConfig.httpClient.send(request, HttpResponse.BodyHandlers.ofString());

            if (response.statusCode() != 200) {
                log(
                    "Failed to post market stop order for " + orderId
                        + ": status=" + response.statusCode()
                        + " figi=" + figi
                        + " quantity=" + quantity
                        + " stopPrice=" + String.format("%.4f", triggerPrice)
                        + " direction=" + direction.name()
                        + " type=" + stopOrderType.name()
                        + " body=" + response.body());
                return null;
            }

            ObjectMapper objectMapper = new ObjectMapper();
            JsonNode json = objectMapper.readTree(response.body());
            String stopOrderId = json.path("stopOrderId").asText(null);
            if (stopOrderId == null || stopOrderId.isEmpty()) {
                log(
                    "Missing stopOrderId in response for "
                        + orderId + " figi=" + figi
                        + " quantity=" + quantity
                        + " stopPrice=" + String.format("%.4f", triggerPrice)
                        + " direction=" + direction.name()
                        + " type=" + stopOrderType.name()
                        + ": " + response.body());
                return null;
            }
            return stopOrderId;
        } catch (Exception ex) {
            log(
                "Failed to post market stop order for " + orderId
                    + " figi=" + figi
                    + " quantity=" + quantity
                    + " stopPrice=" + String.format("%.4f", triggerPrice)
                    + " direction=" + direction.name()
                    + " type=" + stopOrderType.name()
                    + ": " + ex.getMessage());
            return null;
        }
    }

    private String quotationJson(double price) {
        long units = (long) price;
        int nano = (int) Math.round((price - units) * 1_000_000_000);
        return "{\"units\":\"" + units + "\",\"nano\":\"" + nano + "\"}";
    }

    public void cancelStopOrder(TickerInfo.Key key, String stopOrderId, String orderTypeName) {
        if (stopOrderId == null || stopOrderId.isBlank()) {
            return;
        }
        if (key == null || key.getTicker() == null) {
            log("WARN: cancelStopOrder called with null key, skipping cancel for " + stopOrderId);
            return;
        }
        if (orderTypeName == null || orderTypeName.isBlank()) {
            log("WARN: cancelStopOrder called with null orderTypeName for " + stopOrderId);
            return;
        }
        try {
            investApi
                    .getStopOrdersService()
                    .cancelStopOrderSync(mainConfig.getTcsAccountId(), stopOrderId);
            log(key.getTicker() + " " + orderTypeName + " cancelled: " + stopOrderId);
        } catch (Exception ex) {
            String error =
                    "Failed cancel "
                            + orderTypeName
                            + " for "
                            + key.getTicker()
                            + ": "
                            + ex.getMessage();
            log(error);
        }
    }

    private static class ProtectiveOrders {

        private String stopLossOrderId;
        private String takeProfitOrderId;
    }

    /**
     * Calculates the maximum number of instruments that can be traded with the given cash at the
     * given price.
     *
     * <p>Rounds down to a multiple of the lot size. For futures, the cost calculation includes the
     * contract multiplier.
     *
     * @param key ticker key identifying the instrument
     * @param availableCash amount of cash available for trading
     * @param price price per instrument
     * @return number of instruments to trade, or 0 if insufficient funds
     */
    public int calculateTradeCount(TickerInfo.Key key, double availableCash, double price) {
        if (availableCash <= 0.0 || price <= 0.0) {
            return 0;
        }

        TickerInfo tickerInfo = searchTicker(key);
        int lot = Math.max(1, tickerInfo.getLot());
        // Safety margin (1%) for market order slippage to avoid INSUFFICIENT_FUNDS (error 30049)
        double effectivePrice = price * 1.01;
        double tradeUnitCost = effectivePrice * lot;

        // For futures (FEATURE), use margin requirement instead of full notional
        // MOEX futures typically require 20-25% margin
        // price is per unit, lot is units per lot — price * lot = notional per lot
        if (tickerInfo.getType() == TickerType.FEATURE) {
            double futuresMarginRate = 0.25; // Conservative 25% margin
            tradeUnitCost = effectivePrice * lot * futuresMarginRate;
        }

        if (availableCash < tradeUnitCost) {
            return 0;
        }

        // Return quantity in instrument
        return (int) Math.floor(availableCash / tradeUnitCost);
    }

    /**
     * Calculates the total cash required to trade the given count of instruments at the given
     * price.
     *
     * <p>For futures, the cost includes the contract unit multiplier.
     *
     * @param key ticker key identifying the instrument
     * @param count number of instruments
     * @param price price per instrument
     * @return total cash required for the order
     */
    public double getRequiredCashForOrder(TickerInfo.Key key, int count, double price) {
        if (count <= 0 || price <= 0.0) {
            return 0.0;
        }

        return getOrderValue(count, price);
    }

    private double getOrderValue(int count, double price) {
        return count * price;
    }

    private int getContractUnits() {
        return 1;
    }

    private String formatTradeLog(
            String operation, String ticker, int count, TickerType type, String details) {
        if (operation == null || operation.isEmpty()) {
            throw new IllegalArgumentException("Operation cannot be null or empty");
        }
        if (ticker == null || ticker.isEmpty()) {
            throw new IllegalArgumentException("Ticker cannot be null or empty");
        }
        if (type == null) {
            throw new IllegalArgumentException("TickerType cannot be null");
        }

        int normalizedCount = Math.abs(count);
        int lot = 1;
        try {
            lot = Math.max(1, searchTicker(new TickerInfo.Key(ticker, type)).getLot());
        } catch (Exception e) {
            log("WARN: Failed to get lot size for " + ticker + ", using default lot=1: " + e.getMessage());
        }
        int quantity = normalizedCount * lot;
        return String.format(
                "%s: %s count=%d lot=%d quantity=%d by %s",
                operation, ticker, normalizedCount, lot, quantity, details);
    }

    private int normalizeOrderCount(int count, int lot) {
        if (count <= 0) {
            return count;
        }
        // count is already in lots (from API position balance or calculateTradeCount)
        // no need to multiply by lot size
        return count;
    }

    private static Quotation createQuotation(double price) {
        long units = (long) price;
        double fractional = price - units;
        // nano - это дробная часть, умноженная на 1_000_000_000
        int nano = (int) Math.round(fractional * 1_000_000_000);
        return Quotation.newBuilder().setUnits(units).setNano(nano).build();
    }

    /**
     * Returns the list of all tradable shares on MOEX, cached for 10 minutes.
     *
     * @return map of ticker key to {@link TickerInfo} for all tradable shares
     */
    public Map<TickerInfo.Key, TickerInfo> getStockList() {
        if (cachedStockList != null
                && cachedStockListAt != null
                && cachedStockListAt.plus(Duration.ofMinutes(10)).isAfter(Instant.now())) {
            return cachedStockList;
        }

        log("Loading current stocks...");
        List<Share> stocks = investApi.getInstrumentsService().getTradableSharesSync();
        Map<TickerInfo.Key, TickerInfo> loadedStocks =
                stocks.stream()
                        .map(
                                it ->
                                        new TickerInfo(
                                                it.getFigi(),
                                                it.getTicker(),
                                                it.getIsin(),
                                                toDouble(it.getMinPriceIncrement()),
                                                it.getLot(),
                                                it.getCurrency(),
                                                it.getName(),
                                                TickerType.STOCK.name()))
                        .collect(Collectors.toMap(TickerInfo::getKey, it -> it, TCSService::preferRuble));
        cachedStockList = loadedStocks;
        cachedStockListAt = Instant.now();
        return loadedStocks;
    }

    /**
     * Returns the list of all tradable bonds.
     *
     * @return map of ticker key to {@link TickerInfo} for all tradable bonds
     */
    public Map<TickerInfo.Key, TickerInfo> getBondList() {
        log("Loading current bonds...");
        List<Bond> bonds = investApi.getInstrumentsService().getTradableBondsSync();
        return bonds.stream()
                .map(
                        it ->
                                new TickerInfo(
                                        it.getFigi(),
                                        it.getTicker(),
                                        it.getIsin(),
                                        toDouble(it.getMinPriceIncrement()),
                                        it.getLot(),
                                        it.getCurrency(),
                                        it.getName(),
                                        TickerType.BOND.name()))
                .collect(Collectors.toMap(TickerInfo::getKey, it -> it, TCSService::preferRuble));
    }

    /**
     * Returns the list of all tradable ETFs.
     *
     * @return map of ticker key to {@link TickerInfo} for all tradable ETFs
     */
    public Map<TickerInfo.Key, TickerInfo> getEtfList() {
        log("Loading current etfs...");
        List<Etf> etfs = investApi.getInstrumentsService().getTradableEtfsSync();
        return etfs.stream()
                .map(
                        it ->
                                new TickerInfo(
                                        it.getFigi(),
                                        it.getTicker(),
                                        it.getIsin(),
                                        toDouble(it.getMinPriceIncrement()),
                                        it.getLot(),
                                        it.getCurrency(),
                                        it.getName(),
                                        TickerType.ETF.name()))
                .collect(Collectors.toMap(TickerInfo::getKey, it -> it, TCSService::preferRuble));
    }

    /**
     * Returns the list of all tradable currencies.
     *
     * @return map of ticker key to {@link TickerInfo} for all tradable currencies
     */
    public Map<TickerInfo.Key, TickerInfo> getCurrenciesList() {
        log("Loading current currencies...");
        List<Currency> currencies = investApi.getInstrumentsService().getTradableCurrenciesSync();
        return currencies.stream()
                .map(
                        it ->
                                new TickerInfo(
                                        it.getFigi(),
                                        it.getTicker(),
                                        it.getIsin(),
                                        toDouble(it.getMinPriceIncrement()),
                                        it.getLot(),
                                        it.getCurrency(),
                                        it.getName(),
                                        TickerType.CURRENCY.name()))
                .collect(Collectors.toMap(TickerInfo::getKey, it -> it, TCSService::preferRuble));
    }

    /**
     * Returns the list of all tradable futures.
     *
     * @return map of ticker key to {@link TickerInfo} for all tradable futures
     */
    public Map<TickerInfo.Key, TickerInfo> getFuturesList() {
        log("Loading current features...");
        List<Future> futures = fetchTradableFuturesWithRetry();
        return futures.stream()
                .map(this::toFutureTickerInfo)
                .collect(Collectors.toMap(TickerInfo::getKey, it -> it, TCSService::preferRuble));
    }

    private List<Future> fetchTradableFuturesWithRetry() {
        final int maxAttempts = 3;
        for (int attempt = 1; ; attempt++) {
            try {
                return investApi.getInstrumentsService().getTradableFuturesSync();
            } catch (RuntimeException ex) {
                if (attempt >= maxAttempts) {
                    throw ex;
                }
                log(
                        "Failed to load futures (attempt "
                                + attempt
                                + "/"
                                + maxAttempts
                                + "): "
                                + ex.getMessage());
                sleep(attempt * 1_000L);
            }
        }
    }

    private static TickerInfo preferRuble(TickerInfo first, TickerInfo second) {
        if (isRuble(first)) {
            return first;
        }
        return second;
    }

    private static boolean isRuble(TickerInfo info) {
        return info != null && "RUB".equalsIgnoreCase(info.getCurrency());
    }

    private List<HistoricCandle> getCandlesWithRetry(
            String figi, Instant start, Instant end, CandleInterval candleInterval) {
        final int maxAttempts = 3;
        for (int attempt = 1; ; attempt++) {
            try {
                return investApi.getMarketDataService().getCandlesSync(figi, start, end, candleInterval);
            } catch (RuntimeException ex) {
                if (attempt >= maxAttempts) {
                    throw ex;
                }
                // Silent retry for transient API errors (e.g., "unknown error" from gRPC)
                sleep(attempt * 500L);
            }
        }
    }

    private TickerInfo toFutureTickerInfo(Future future) {
        TickerInfo info =
                new TickerInfo(
                        future.getFigi(),
                        future.getTicker(),
                        future.getBasicAsset(),
                        toDouble(future.getMinPriceIncrement()),
                        future.getLot(),
                        future.getCurrency(),
                        future.getName(),
                        TickerType.FEATURE.name());
        info.setForQualInvestorFlag(future.getForQualInvestorFlag());
        info.setApiTradeAvailableFlag(future.getApiTradeAvailableFlag());
        info.setNormalTradingStatus(isNormalTradingStatus(future.getTradingStatus()));
        info.setBasicAsset(future.getBasicAsset());
        info.setAssetType(future.getAssetType());
        info.setBasicAssetSize(toDouble(future.getBasicAssetSize()));
        if (future.hasExpirationDate()) {
            info.setExpirationDate(
                    Instant.ofEpochSecond(
                            future.getExpirationDate().getSeconds(),
                            future.getExpirationDate().getNanos()));
        }
        return info;
    }

    private static void copyFutureMetadata(TickerInfo source, TickerInfo target) {
        target.setForQualInvestorFlag(source.isForQualInvestorFlag());
        target.setApiTradeAvailableFlag(source.isApiTradeAvailableFlag());
        target.setNormalTradingStatus(source.isNormalTradingStatus());
        target.setBasicAsset(source.getBasicAsset());
        target.setAssetType(source.getAssetType());
        target.setExpirationDate(source.getExpirationDate());
    }

    private static boolean isNormalTradingStatus(SecurityTradingStatus status) {
        return SecurityTradingStatus.SECURITY_TRADING_STATUS_NORMAL_TRADING == status;
    }

    /**
     * Returns the available cash balance in RUB.
     *
     * @return available cash amount
     */
    public Double getAvailableCash() {
        sleep(550);
        Positions positions =
                investApi.getOperationsService().getPositionsSync(mainConfig.getTcsAccountId());
        return positions.getMoney().stream()
                .filter(it -> "RUB".equalsIgnoreCase(it.getCurrency()))
                .map(Money::getValue)
                .findFirst()
                .orElse(BigDecimal.ZERO)
                .doubleValue();
    }

    @Override
    public double getInitialBalance() {
        // Live trading: use current balance as there's no fixed initial balance
        Double cash = getAvailableCash();
        return cash != null ? cash : 0.0;
    }

    /** Applies ticker lot override from config if present. */
    private TickerInfo applyTickerLotOverride(TickerInfo tickerInfo) {
        Integer overrideLot = mainConfig.getTickerLotOverrides().get(tickerInfo.getTicker());
        if (overrideLot != null) {
            TickerInfo overridden =
                    new TickerInfo(
                            tickerInfo.getFigi(),
                            tickerInfo.getTicker(),
                            tickerInfo.getIsin(),
                            tickerInfo.getMinPriceIncrement(),
                            overrideLot,
                            tickerInfo.getCurrency(),
                            tickerInfo.getName(),
                            tickerInfo.getType().name());
            copyFutureMetadata(tickerInfo, overridden);
            return overridden;
        }
        return tickerInfo;
    }

    /**
     * Searches for a ticker by its key, using the cache or fetching from the appropriate instrument
     * list.
     *
     * <p>Results are cached in {@link TickerRepository} for subsequent lookups.
     *
     * @param key ticker key (symbol + type) to search for
     * @return {@link TickerInfo} if found, or throws {@link RuntimeException} if the ticker type is
     *     unknown
     */
    public TickerInfo searchTicker(TickerInfo.Key key) {
        if (tickerRepository.containsKey(key)) {
            return applyTickerLotOverride(tickerRepository.getById(key));
        }
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
            tickerInfo = applyTickerLotOverride(tickerInfo);
            tickerRepository.insert(key, tickerInfo);
            log(key.getTicker() + ": " + tickerInfo);
        }
        return tickerInfo;
    }

    /**
     * Returns the total portfolio value in the base currency.
     *
     * @return total portfolio cost
     */
    @Override
    public double getTotalPortfolioValue() {
        return getTotalPortfolioCost();
    }

    @Override
    public double getGlobalPeakEquity() {
        return getTotalPortfolioCost();
    }

    /**
     * Returns the total portfolio value in the base currency.
     *
     * @return total portfolio cost
     */
    public double getTotalPortfolioCost() {
        return investApi
                .getOperationsService()
                .getPortfolioSync(mainConfig.getTcsAccountId())
                .getTotalAmountPortfolio()
                .getValue()
                .doubleValue();
    }

    /**
     * Returns the current balance (quantity) of the position for the given ticker.
     *
     * <p>A positive value indicates a long position, negative indicates a short position.
     *
     * @param tickerType instrument type to filter positions
     * @param tickerName ticker symbol
     * @return position balance, or 0 if no position is found
     */
    public int getCountOfCurrentPositions(TickerType tickerType, String tickerName) {
        return getCurrentPositions(tickerType).values().stream()
                .filter(it -> it.getTicker().equalsIgnoreCase(tickerName))
                .map(PositionInfo::getLots)
                .findFirst()
                .orElse(0);
    }

    /**
     * Returns the position info for the given ticker name, filtered by instrument type.
     *
     * @param tickerType instrument type to filter positions
     * @param tickerName ticker symbol
     * @return {@link PositionInfo} if a matching position is found, or {@code null} otherwise
     */
    public PositionInfo getCurrentPositions(TickerType tickerType, String tickerName) {
        return getCurrentPositions(tickerType).values().stream()
                .filter(it -> it.getTicker().equalsIgnoreCase(tickerName))
                .findFirst()
                .orElse(null);
    }

    /**
     * Returns all current positions, optionally filtered by instrument type.
     *
     * <p>Maps portfolio positions to {@link PositionInfo} using ticker lookups. Positions in
     * currencies other than the base currency are excluded. Attempts to recover ticker info for
     * positions with unknown FIGI mappings.
     *
     * @param tickerType instrument type to filter by, or {@code null} for all positions
     * @return map of ticker key to {@link PositionInfo} for all matching positions
     */
    public Map<TickerInfo.Key, PositionInfo> getCurrentPositions(TickerType tickerType) {
        sleep(550);
        Map<TickerInfo.Key, PositionInfo> positionInfoList = new HashMap<>();
        String type = getTypeFromTickerType(tickerType);
        Portfolio portfolio =
                investApi.getOperationsService().getPortfolioSync(mainConfig.getTcsAccountId());
        portfolio.getPositions().stream()
                .filter(
                        it ->
                                TickerType.ALL == tickerType
                                        || type.equalsIgnoreCase(it.getInstrumentType()))
                .forEach(
                        it -> {
                            AtomicReference<TickerInfo.Key> tickerKey = new AtomicReference<>();
                            figiRepository
                                    .getAll()
                                    .forEach(
                                            (key, value) -> {
                                                if (value.equals(it.getFigi())) {
                                                    tickerKey.set(key);
                                                }
                                            });
                            if (null == tickerKey.get()) {
                                tickerRepository.getAll().values().stream()
                                        .filter(ticker -> ticker.getFigi().equals(it.getFigi()))
                                        .findFirst()
                                        .map(
                                                ticker ->
                                                        new TickerInfo.Key(
                                                                ticker.getTicker(),
                                                                ticker.getType()))
                                        .ifPresent(tickerKey::set);
                            }
                            if (null == tickerKey.get()) {
                                TickerInfo recoveredTicker =
                                        recoverTickerInfoByFigi(
                                                it.getFigi(), it.getInstrumentType());
                                if (recoveredTicker != null) {
                                    tickerKey.set(recoveredTicker.getKey());
                                }
                            }
                            if (null == tickerKey.get()) {
                                log(
                                        "Warn: position skipped, ticker key not found for figi="
                                                + it.getFigi()
                                                + ", instrumentType="
                                                + it.getInstrumentType());
                                return;
                            }
                            TickerInfo tickerInfo = searchTicker(tickerKey.get());
                            if (null != tickerInfo
                                    && "RUB".equals(tickerInfo.getCurrency())) {
                                Double expectedYield = it.getExpectedYield().doubleValue();
                                if (0.0 == expectedYield) {
                                    Double currentPrice =
                                            it.getCurrentPrice().getValue().doubleValue();
                                    Double averagePositionPrice =
                                            it.getAveragePositionPriceFifo()
                                                    .getValue()
                                                    .doubleValue();
                                    if ((long) it.getQuantity().doubleValue() > 0) {
                                        expectedYield =
                                                (currentPrice - averagePositionPrice)
                                                        / averagePositionPrice
                                                        * 100;
                                    } else {
                                        expectedYield =
                                                (averagePositionPrice - currentPrice)
                                                        / currentPrice
                                                        * 100;
                                    }
                                }
                                PositionInfo positionInfo =
                                        new PositionInfo(
                                                tickerInfo.getFigi(),
                                                tickerInfo.getTicker(),
                                                tickerInfo.getIsin(),
                                                tickerInfo.getType().name(),
                                                it.getQuantity().intValue(),
                                                expectedYield,
                                                it.getQuantityLots().intValue(),
                                                it.getAveragePositionPriceFifo()
                                                        .getValue()
                                                        .doubleValue(),
                                                tickerInfo.getName());
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
            tickerInfo =
                    getStockList().values().stream()
                            .filter(ticker -> figi.equals(ticker.getFigi()))
                            .findFirst()
                            .orElse(null);
        } else if ("futures".equalsIgnoreCase(instrumentType)) {
            tickerInfo =
                    getFuturesList().values().stream()
                            .filter(ticker -> figi.equals(ticker.getFigi()))
                            .findFirst()
                            .orElse(null);
        } else if ("bond".equalsIgnoreCase(instrumentType)) {
            tickerInfo =
                    getBondList().values().stream()
                            .filter(ticker -> figi.equals(ticker.getFigi()))
                            .findFirst()
                            .orElse(null);
        } else if ("etf".equalsIgnoreCase(instrumentType)) {
            tickerInfo =
                    getEtfList().values().stream()
                            .filter(ticker -> figi.equals(ticker.getFigi()))
                            .findFirst()
                            .orElse(null);
        } else if ("currency".equalsIgnoreCase(instrumentType)) {
            tickerInfo =
                    getCurrenciesList().values().stream()
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
        if (TickerType.STOCK == tickerType) {
            return "share";
        }
        if (TickerType.FEATURE == tickerType) {
            return "futures";
        }
        if (TickerType.ETF == tickerType) {
            return "etf";
        }
        if (TickerType.BOND == tickerType) {
            return "bond";
        }
        if (TickerType.CURRENCY == tickerType) {
            return "currency";
        }
        return "";
    }

    /**
     * Returns the FIGI identifier for the given ticker key.
     *
     * <p>Uses the cached FIGI repository first; if not found, searches for the ticker and caches
     * the result.
     *
     * @param key ticker key (symbol + type) to look up
     * @return FIGI identifier string
     */
    public String figiByName(TickerInfo.Key key) {
        if (key == null || key.getTicker() == null || key.getType() == null) {
            throw new IllegalArgumentException("TickerInfo.Key cannot be null, ticker or type must not be null");
        }
        if (figiRepository.containsKey(key)) {
            return figiRepository.getById(key);
        }
        TickerInfo ticker = searchTicker(key);
        if (ticker == null || ticker.getFigi() == null) {
            throw new IllegalArgumentException("Ticker not found or missing FIGI: " + key.getTicker());
        }
        String figi = ticker.getFigi();
        figiRepository.insert(key, figi);
        return figi;
    }

    /**
     * Returns the best available price for a single instrument without printing the glass.
     *
     * @param key ticker key identifying the instrument
     * @return best available price from the bids side of the glass
     */
    public double getAvailablePrice(TickerInfo.Key key) {
        return getAvailablePrice(key, 1, false);
    }

    /**
     * Returns the best available price for a given quantity from the bids side of the glass.
     *
     * @param key ticker key identifying the instrument
     * @param count number of instruments to cover
     * @param isPrintGlass if {@code true}, prints the glass of prices to the console
     * @return best available price from the bids side for the given quantity
     */
    public double getAvailablePrice(TickerInfo.Key key, int count, boolean isPrintGlass) {
        return getAvailablePrice(key, count, "bids", isPrintGlass);
    }

    /**
     * Returns the best available price for a given quantity from the specified side of the glass.
     *
     * <p>Walks through the order book levels until the requested quantity is covered. For "asks"
     * the price is sorted ascending, for "bids" descending.
     *
     * @param key ticker key identifying the instrument
     * @param count number of instruments to cover
     * @param type "bids" or "asks" side of the glass
     * @param isPrintGlass if {@code true}, prints the glass of prices to the console
     * @return best available price for the given quantity, or 0.0 if the glass is empty
     */
    public double getAvailablePrice(
            TickerInfo.Key key, int count, String type, boolean isPrintGlass) {
        long value = count;
        double tickerPrice = 0.0;

        Set<Map.Entry<Double, Long>> glass = getCurrentPrices(key, isPrintGlass).get(type).entrySet();
        if ("asks".equals(type)) {
            glass =
                    glass.stream()
                            .sorted((o1, o2) -> o1.getKey().compareTo(o2.getKey()))
                            .collect(toCollection(LinkedHashSet::new));
        }
        if ("bids".equals(type)) {
            glass =
                    glass.stream()
                            .sorted((o1, o2) -> o2.getKey().compareTo(o1.getKey()))
                            .collect(toCollection(LinkedHashSet::new));
        }
        for (Map.Entry<Double, Long> bid : glass) {
            tickerPrice = bid.getKey();
            value = value - bid.getValue();
            if (value <= 0) break;
        }
        return tickerPrice;
    }

    /**
     * Returns the best (lowest) live ask price from the orderbook without printing the glass.
     *
     * @param key ticker key identifying the instrument
     * @return the best ask price, or {@code 0.0} if the orderbook is empty or unavailable
     */
    public double getLiveAskPrice(TickerInfo.Key key) {
        Map<String, Map<Double, Long>> prices = getCurrentPrices(key, false);
        Map<Double, Long> asks = prices.get("asks");
        if (asks == null || asks.isEmpty()) {
            return 0.0;
        }
        return asks.keySet().iterator().next();
    }

    /**
     * Returns the best bid price from the current order book for the given ticker.
     *
     * @param key ticker key identifying the instrument
     * @return the best bid price, or {@code 0.0} if the orderbook is empty or unavailable
     */
    public double getLiveBidPrice(TickerInfo.Key key) {
        Map<String, Map<Double, Long>> prices = getCurrentPrices(key, false);
        Map<Double, Long> bids = prices.get("bids");
        if (bids == null || bids.isEmpty()) {
            return 0.0;
        }
        return bids.keySet().iterator().next();
    }

    /**
     * Returns the current prices (bids and asks) for the given ticker, optionally printing the
     * glass.
     *
     * <p>Uses the real-time snapshot if available, otherwise falls back to the cached repository,
     * and as a last resort fetches the order book from the API.
     *
     * @param key ticker key identifying the instrument
     * @param isPrintGlass if {@code true}, prints the glass of prices to the console
     * @return a map with "bids" and "asks" keys, each containing a price-to-quantity mapping
     */
    public Map<String, Map<Double, Long>> getCurrentPrices(
            TickerInfo.Key key, boolean isPrintGlass) {
        MarketDepthSnapshot liveSnapshot = marketDepthByTicker.get(key);
        if (liveSnapshot != null
                && !liveSnapshot.getBids().isEmpty()
                && !liveSnapshot.getAsks().isEmpty()) {
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

        Map<String, Map<Double, Long>> currentPrices = new TreeMap<>();
        Map<Double, Long> bidsValues = new LinkedHashMap<>(10);
        for (Order bid : response.getBidsList()) {
            bidsValues.put(toDouble(bid.getPrice()), bid.getQuantity());
        }
        currentPrices.put("bids", bidsValues);

        Map<Double, Long> asksValues = new LinkedHashMap<>(10);
        for (Order ask : response.getAsksList()) {
            asksValues.put(toDouble(ask.getPrice()), ask.getQuantity());
        }
        currentPrices.put("asks", asksValues);

        if (isPrintGlass) {
            synchronized (this) {
                printGlassOfPrices(key.getTicker(), currentPrices);
            }
        }
        pricesRepository.insert(key, currentPrices);
        return currentPrices;
    }

    private MarketDataStreamShard findOrCreateMarketDataShard(int depth) {
        for (MarketDataStreamShard shard : marketDataStreamShards) {
            if (shard.depth == depth
                    && shard.figis.size() < MAX_INSTRUMENTS_PER_MARKET_DATA_STREAM) {
                return shard;
            }
        }
        int shardId = marketDataStreamShardCounter.incrementAndGet();
        MarketDataStreamShard[] shardHolder = new MarketDataStreamShard[1];
        MarketDataStreamShard shard =
                new MarketDataStreamShard(
                        investApi
                                .getMarketDataStreamService()
                                .newStream(
                                        "market-data-shard-" + shardId,
                                        this::handleMarketDataResponse,
                                        error ->
                                                handleMarketDataStreamError(shardHolder[0], error)),
                        depth);
        shardHolder[0] = shard;
        marketDataStreamShards.add(shard);
        return shard;
    }

    private void handleMarketDataStreamError(
            MarketDataStreamShard failedShard, Throwable throwable) {
        if (failedShard == null) {
            return;
        }
        long now = System.currentTimeMillis();
        boolean resubscribeAllowed =
                now - lastMarketDataRecoveryMs >= MARKET_DATA_RECOVERY_MIN_INTERVAL_MS;
        synchronized (marketDataStreamShards) {
            // always evict the failed shard so it is never reused for new
            // subscriptions, even when resubscription is throttled
            marketDataStreamShards.remove(failedShard);
            Set<String> figis = Set.copyOf(failedShard.figis);
            for (String figi : figis) {
                marketDataShardByFigi.remove(figi);
            }
            failedShard.figis.clear();
            try {
                failedShard.stream.cancel();
            } catch (Exception ignored) {
                // best-effort cleanup of the dead stream
            }
            if (!resubscribeAllowed) {
                // throttled: engine-level stale-data check will recover subscriptions
                return;
            }
            lastMarketDataRecoveryMs = now;
            if (!figis.isEmpty()) {
                log("recovering market data stream, resubscribing " + figis.size() + " figis");
                resubscribeMarketData(figis, failedShard.depth);
            }
        }
    }

    private void resubscribeMarketData(Set<String> figis, int depth) {
        MarketDataStreamShard shard = findOrCreateMarketDataShard(depth);
        List<String> figiList = List.copyOf(figis);
        shard.stream.subscribeOrderbook(figiList, depth);
        shard.stream.subscribeTrades(figiList);
        for (String figi : figiList) {
            shard.figis.add(figi);
            marketDataShardByFigi.put(figi, shard);
        }
    }

    private void handleMarketDataResponse(MarketDataResponse response) {
        if (response.hasOrderbook()) {
            TickerInfo.Key key = marketDataKeyByFigi.get(response.getOrderbook().getFigi());
            if (key == null) {
                return;
            }
            MarketDepthSnapshot snapshot = toMarketDepthSnapshot(response.getOrderbook());
            marketDepthByTicker.put(key, snapshot);
            pricesRepository.insert(key, toCurrentPrices(snapshot));
            appendMarketDepthSnapshot(key, snapshot);
        }
    }

    private static final class MarketDataStreamShard {

        private final MarketDataSubscriptionService stream;
        private final int depth;
        private final Set<String> figis = ConcurrentHashMap.newKeySet();

        private MarketDataStreamShard(MarketDataSubscriptionService stream, int depth) {
            this.stream = stream;
            this.depth = depth;
        }
    }

    private MarketDepthSnapshot toMarketDepthSnapshot(
            ru.tinkoff.piapi.contract.v1.OrderBook orderBook) {
        Instant time =
                orderBook.hasTime()
                        ? Instant.ofEpochSecond(
                                orderBook.getTime().getSeconds(), orderBook.getTime().getNanos())
                        : Instant.now();
        return new MarketDepthSnapshot(
                orderBook.getFigi(),
                time,
                orderBook.getIsConsistent(),
                orderBook.getBidsList().stream()
                        .map(
                                it ->
                                        new MarketDepthLevel(
                                                toDouble(it.getPrice()), (int) (long) it.getQuantity()))
                        .collect(Collectors.toList()),
                orderBook.getAsksList().stream()
                        .map(
                                it ->
                                        new MarketDepthLevel(
                                                toDouble(it.getPrice()), (int) (long) it.getQuantity()))
                        .collect(Collectors.toList()));
    }

    private void appendMarketDepthSnapshot(TickerInfo.Key key, MarketDepthSnapshot snapshot) {
        if (!writeMarketDepthTicks) {
            return;
        }
        if (!snapshot.isConsistent()) {
            return;
        }
        Path ticksFilePath = Path.of("data", key.getTicker(), "ticks.txt");
        Object fileLock =
                marketDepthFileLocks.computeIfAbsent(
                        ticksFilePath.toString(), ignored -> new Object());
        synchronized (fileLock) {
            try {
                Files.createDirectories(ticksFilePath.getParent());
                if (!Files.exists(ticksFilePath)) {
                    Files.write(
                            ticksFilePath,
                            List.of(MARKET_DEPTH_TICKS_HEADER),
                            StandardCharsets.UTF_8,
                            StandardOpenOption.CREATE,
                            StandardOpenOption.APPEND);
                }
                Files.write(
                        ticksFilePath,
                        List.of(toMarketDepthCsvLine(snapshot)),
                        StandardCharsets.UTF_8,
                        StandardOpenOption.CREATE,
                        StandardOpenOption.APPEND);
            } catch (IOException ex) {
                log(
                        "Warn: failed to append market depth tick for '"
                                + key.getTicker()
                                + "': "
                                + ex.getMessage());
            }
        }
    }

    private String toMarketDepthCsvLine(MarketDepthSnapshot snapshot) {
        return String.join(
                ",",
                formatMarketDepthTime(snapshot.getTime()),
                toCsvValue(snapshot.getBestBid()),
                toCsvValue(snapshot.getBestAsk()),
                toCsvValue(snapshot.getMidPrice()),
                quoteCsv(formatMarketDepthLevels(snapshot.getBids())),
                quoteCsv(formatMarketDepthLevels(snapshot.getAsks())));
    }

    private String formatMarketDepthTime(Instant time) {
        return MARKET_DEPTH_TICKS_TIME_FORMATTER.format(time.atZone(ZoneId.systemDefault()));
    }

    private String toCsvValue(Double value) {
        return value == null ? "" : Double.toString(value);
    }

    private String formatMarketDepthLevels(List<MarketDepthLevel> levels) {
        return levels.stream()
                .map(it -> Double.toString(it.getPrice()) + ":" + (long) it.getQuantity())
                .collect(Collectors.joining("|"));
    }

    private String quoteCsv(String value) {
        return '"' + value.replace("\"", "\"\"") + '"';
    }

    private Map<String, Map<Double, Long>> toCurrentPrices(
            MarketDepthSnapshot snapshot, TickerInfo.Key key, boolean isPrintGlass) {
        Map<String, Map<Double, Long>> currentPrices = toCurrentPrices(snapshot);
        if (isPrintGlass) {
            synchronized (this) {
                printGlassOfPrices(key.getTicker(), currentPrices);
            }
        }
        pricesRepository.insert(key, currentPrices);
        return currentPrices;
    }

    private Map<String, Map<Double, Long>> toCurrentPrices(MarketDepthSnapshot snapshot) {
        Map<String, Map<Double, Long>> currentPrices = new TreeMap<>();
        Map<Double, Long> bidsValues = new LinkedHashMap<>(snapshot.getBids().size());
        snapshot.getBids().forEach(it -> bidsValues.put(it.getPrice(), (long) it.getQuantity()));
        currentPrices.put("bids", bidsValues);

        Map<Double, Long> asksValues = new LinkedHashMap<>(snapshot.getAsks().size());
        snapshot.getAsks().forEach(it -> asksValues.put(it.getPrice(), (long) it.getQuantity()));
        currentPrices.put("asks", asksValues);
        return currentPrices;
    }

    /**
     * Converts a price from one currency to another using the current exchange rate.
     *
     * <p>Returns the price unchanged if both currencies are the same.
     *
     * @param currency source currency code
     * @param basicCurrency target currency code
     * @param price price in the source currency
     * @return price converted to the target currency
     */
    public double convertCurrencies(String currency, String basicCurrency, double price) {
        if (basicCurrency.equals(currency)) {
            return price;
        }

        String currencyTicker = getTickerName(currency);
        if (currencyTicker.equals(currency)) {
            return round(
                            (price
                                            / getAvailablePrice(
                                                    new TickerInfo.Key(
                                                            getTickerName(basicCurrency),
                                                            TickerType.CURRENCY)))
                                    * 1000)
                    / 1000.0;
        }

        TickerInfo.Key key = new TickerInfo.Key(currencyTicker, com.github.shk0da.goldendragon.model.TickerType.CURRENCY);
        TickerInfo currencyTickerInfo = searchTicker(key);
        if (basicCurrency.equals(currencyTickerInfo.getCurrency())) {
            return round((price / getAvailablePrice(key)) * 100000) / 100000.0;
        }
        return price;
    }

    private static Double toDouble(Quotation quotation) {
        return toDouble(quotation.getUnits(), quotation.getNano());
    }

    /**
     * Converts a {@link Quotation} to a {@code double} value.
     *
     * @return double representation of the quotation
     */
    private static Double toDouble(long units, int nano) {
        double fractional = nano / 1_000_000_000.0;
        return units + fractional;
    }

    private static Double normalizePrice(double price, double priceStep) {
        return Math.round(price / priceStep) * priceStep;
    }

    /**
     * Returns trade history from broker operations since the given timestamp.
     * Uses REST API with retry logic for rate limiting.
     */
    @Override
    public List<Map<String, Object>> getTradeHistory(Instant since) {
        if (since == null) {
            log("WARN: getTradeHistory called with null since parameter, using 7 days ago");
            since = Instant.now().minus(java.time.Duration.ofDays(7));
        }
        try {
            // Retry with exponential backoff for 429 errors
            int maxRetries = 3;
            int retryDelayMs = 1000;

            for (int attempt = 0; attempt < maxRetries; attempt++) {
                List<Map<String, Object>> result = fetchTradeHistory(since);
                if (result != null) {
                    return result;
                }
                log("Trade history rate limited (429), retry " + (attempt + 1) + "/" + maxRetries);
                if (attempt < maxRetries - 1) {
                    Thread.sleep(retryDelayMs * (attempt + 1));
                }
            }
            return emptyList();
        } catch (InterruptedException ie) {
            Thread.currentThread().interrupt();
            log("Trade history fetch interrupted");
            return emptyList();
        } catch (Exception ex) {
            log("Failed to get trade history after retries: " + ex.getMessage());
            return emptyList();
        }
    }

    private List<Map<String, Object>> fetchTradeHistory(Instant since) throws Exception {
        if (since == null) {
            throw new IllegalArgumentException("since parameter cannot be null");
        }
        Instant now = Instant.now();
        String fromParam = java.time.format.DateTimeFormatter.ISO_INSTANT.format(since);
        String toParam = java.time.format.DateTimeFormatter.ISO_INSTANT.format(now);

        String url = TinkoffApiUrlResolver.buildRestUrl(mainConfig, "OperationsService/GetOperationsByCursor");

        // All Tinkoff gRPC-to-REST bridge endpoints use POST with a JSON body.
        String body =
                "{\"accountId\":\"" + mainConfig.getTcsAccountId()
                        + "\",\"from\":\"" + fromParam
                        + "\",\"to\":\"" + toParam
                        + "\",\"limit\":\"1000\""
                        + ",\"operationTypes\":[\"OPERATION_TYPE_BUY\",\"OPERATION_TYPE_SELL\"]"
                        + ",\"state\":\"OPERATION_STATE_EXECUTED\"}";

        HttpRequest request =
                HttpRequest.newBuilder()
                        .uri(URI.create(url))
                        .header("Authorization", "Bearer " + mainConfig.getTcsApiKey())
                        .header("Content-Type", "application/json")
                        .POST(HttpRequest.BodyPublishers.ofString(body))
                        .build();

        HttpResponse<String> response =
                MainConfig.httpClient.send(request, HttpResponse.BodyHandlers.ofString());

        if (response.statusCode() == 429) {
            return null; // signal to retry
        }

        if (response.statusCode() != 200) {
            log("Failed to get trade history: status=" + response.statusCode() + " body=" + response.body());
            return emptyList();
        }

        ObjectMapper objectMapper = new ObjectMapper();
        JsonNode json = objectMapper.readTree(response.body());
        // GetOperationsByCursor returns 'items' array, not 'operations'
        JsonNode operationsNode = json.has("operations") ? json.path("operations") : json.path("items");

        if (!operationsNode.isArray()) {
            log("No 'items' array in response: " + response.body());
            return emptyList();
        }

        List<Map<String, Object>> trades = new ArrayList<>();

        for (JsonNode op : operationsNode) {
            String type = op.path("type").asText("");
            if ("OPERATION_TYPE_BUY".equals(type) || "OPERATION_TYPE_SELL".equals(type)) {
                Map<String, Object> trade = new LinkedHashMap<>();

                // Convert broker time (UTC) to Moscow timezone (UTC+3) as ISO 8601 with offset
                String timeStr = op.path("date").asText("");
                if (!timeStr.isEmpty()) {
                    try {
                        timeStr = Instant.parse(timeStr)
                                .atZone(ZoneId.of("Europe/Moscow"))
                                .format(DateTimeFormatter.ISO_OFFSET_DATE_TIME);
                    } catch (Exception ex) {
                        timeStr = timeStr.replace("T", " ").substring(0, 19);
                    }
                }
                trade.put("time", timeStr);

                // Use 'ticker' field directly (e.g., "GMKN"), fallback to figi if missing
                String ticker = op.path("ticker").asText("");
                if (ticker.isEmpty()) {
                    ticker = op.path("instrumentUid").asText("");
                }
                if (ticker.isEmpty()) {
                    ticker = op.path("figi").asText("");
                }
                trade.put("ticker", ticker.isEmpty() ? "UNKNOWN" : ticker);

                trade.put("type", type.replace("OPERATION_TYPE_", ""));
                trade.put("quantity", op.path("quantity").asInt(0));

                // Parse price from {units, nano} object
                JsonNode priceNode = op.path("price");
                double price = 0.0;
                if (priceNode.has("units")) {
                    long units = priceNode.path("units").asLong(0);
                    int nano = priceNode.path("nano").asInt(0);
                    price = MoneyUtils.parseUnitsNano(units, nano);
                }
                trade.put("price", price);
                trade.put("description", op.path("description").asText(""));

                // Use 'yield' field from API - this is the actual PnL in RUB
                double pnl = parseYieldFromJson(op);
                trade.put("pnl", pnl);

                trades.add(trade);
            }
        }

        return trades;
    }

    private double parseYieldFromJson(JsonNode op) {
        // Use 'yield' field from API - this is the actual PnL in RUB
        JsonNode yieldNode = op.path("yield");
        if (yieldNode.has("units")) {
            long units = yieldNode.path("units").asLong(0);
            int nano = yieldNode.path("nano").asInt(0);
            return MoneyUtils.parseUnitsNano(units, nano);
        }
        return 0.0;
    }
}
