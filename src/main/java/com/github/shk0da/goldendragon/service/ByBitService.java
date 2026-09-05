package com.github.shk0da.goldendragon.service;

import static com.github.shk0da.goldendragon.dictionary.CurrenciesDictionary.getTickerName;
import static com.github.shk0da.goldendragon.utils.LoggingUtils.log;
import static com.github.shk0da.goldendragon.utils.PrintUtils.printGlassOfPrices;
import static java.lang.Math.max;
import static java.util.Collections.emptyList;
import static java.util.Collections.reverseOrder;

import com.bybit.api.client.restApi.BybitApiAccountRestClient;
import com.bybit.api.client.restApi.BybitApiAsyncPositionRestClient;
import com.bybit.api.client.restApi.BybitApiAsyncTradeRestClient;
import com.bybit.api.client.restApi.BybitApiMarketRestClient;
import com.bybit.api.client.restApi.BybitApiPositionRestClient;
import com.bybit.api.client.restApi.BybitApiTradeRestClient;
import com.bybit.api.client.service.BybitApiClientFactory;
import com.bybit.api.client.domain.market.MarketInterval;
import com.bybit.api.client.domain.market.request.MarketDataRequest;
import com.bybit.api.client.domain.account.AccountType;
import com.bybit.api.client.domain.account.request.AccountDataRequest;
import com.bybit.api.client.domain.position.request.PositionDataRequest;
import com.bybit.api.client.domain.trade.request.TradeOrderRequest;
import com.bybit.api.client.domain.trade.request.CancelOrderRequest;
import com.bybit.api.client.domain.trade.Side;
import com.bybit.api.client.domain.TradeOrderType;
import com.bybit.api.client.domain.trade.TimeInForce;
import com.bybit.api.client.domain.TriggerBy;
import com.bybit.api.client.domain.trade.PositionIdx;
import com.bybit.api.client.domain.trade.OrderFilter;
import com.bybit.api.client.domain.CategoryType;
import com.github.shk0da.goldendragon.config.ByBitConfig;
import com.github.shk0da.goldendragon.model.Candle;
import com.github.shk0da.goldendragon.model.MarketDepthLevel;
import com.github.shk0da.goldendragon.model.MarketDepthSnapshot;
import com.github.shk0da.goldendragon.model.MarketTickListener;
import com.github.shk0da.goldendragon.model.MarketTradeTick;
import com.github.shk0da.goldendragon.model.Position;
import com.github.shk0da.goldendragon.model.PositionInfo;
import com.github.shk0da.goldendragon.model.TickerCandle;
import com.github.shk0da.goldendragon.model.TickerInfo;
import com.github.shk0da.goldendragon.model.TickerType;
import com.github.shk0da.goldendragon.repository.FigiRepository;
import com.github.shk0da.goldendragon.repository.PricesRepository;
import com.github.shk0da.goldendragon.repository.Repository;
import com.github.shk0da.goldendragon.repository.TickerRepository;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonParser;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import java.time.Duration;
import java.time.Instant;
import java.time.OffsetDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.stream.Collectors;

/**
 * Service for interacting with the ByBit API.
 *
 * <p>Provides methods for retrieving market data, managing orders, tracking positions, subscribing
 * to real-time market streams, and currency conversion.
 */
public class ByBitService implements TradingService {

    public static final double FUTURES_MARGIN_RATE = 0.25; // ByBit futures margin ~25%
    private static final DateTimeFormatter MARKET_DEPTH_TICKS_TIME_FORMATTER =
            DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm:ss");

    private final ByBitConfig byBitConfig;
    private final BybitApiTradeRestClient tradeClient;
    private final BybitApiMarketRestClient marketClient;
    private final BybitApiPositionRestClient positionClient;
    private final BybitApiAccountRestClient accountClient;
    private final Gson gson = new GsonBuilder()
        .setLenient()
        .create();

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
    private final Map<TickerInfo.Key, List<MarketTradeTick>> recentTradesByTicker =
            new ConcurrentHashMap<>();
    private final Map<TickerInfo.Key, CopyOnWriteArrayList<MarketTickListener>>
            marketTickListenersByTicker = new ConcurrentHashMap<>();
    private volatile Map<TickerInfo.Key, TickerInfo> cachedFuturesList;
    private volatile Instant cachedFuturesListAt;

    /**
     * Creates a new {@code ByBitService} initialized with the given configurations.
     *
     * <p>Connects to the ByBit API (testnet or production) based on {@link
     * ByBitConfig#isTestMode()}.
     *
     * @param byBitConfig application configuration containing API credentials
     */
    public ByBitService(ByBitConfig byBitConfig) {
        this.byBitConfig = byBitConfig;

        // Initialize API clients using factory with proper base URLs
        String baseUrl = byBitConfig.isTestMode() 
            ? "https://api-testnet.bybit.com" 
            : "https://api.bybit.com";
        var factory = BybitApiClientFactory.newInstance(
            byBitConfig.getApiKey(),
            byBitConfig.getApiSecret(),
            baseUrl,
            false
        );
        
        this.tradeClient = factory.newTradeRestClient();
        this.marketClient = factory.newMarketDataRestClient();
        this.positionClient = factory.newPositionRestClient();
        this.accountClient = factory.newAccountRestClient();
    }

    @Override
    public TradingServiceType getServiceType() {
        return TradingServiceType.BYBIT;
    }

    // ========== INSTRUMENT METHODS ==========

    @Override
    @SuppressWarnings("unchecked")
    public Map<TickerInfo.Key, TickerInfo> getFuturesList() {
        if (cachedFuturesList != null && 
            cachedFuturesListAt != null && 
            Duration.between(cachedFuturesListAt, Instant.now()).toMinutes() < 5) {
            return cachedFuturesList;
        }

        Map<TickerInfo.Key, TickerInfo> result = new HashMap<>();
        
        try {
            // Build request for linear perpetual futures
            MarketDataRequest request = MarketDataRequest.builder()
                .category(CategoryType.LINEAR)
                .limit(1000)
                .build();
            
            // Call SDK method
            Object response = marketClient.getInstrumentsInfo(request);
            
            // Parse JSON response
            JsonObject json = gson.fromJson(gson.toJson(response), JsonObject.class);
            
            if (json.has("result") && json.get("result").isJsonObject()) {
                JsonObject resultObj = json.getAsJsonObject("result");
                if (resultObj.has("list") && resultObj.get("list").isJsonArray()) {
                    JsonArray list = resultObj.getAsJsonArray("list");
                    for (JsonElement element : list) {
                        JsonObject instrument = element.getAsJsonObject();
                        String symbol = instrument.get("symbol").getAsString();
                        String status = instrument.has("status") ? instrument.get("status").getAsString() : "Trading";
                        
                        // Only include trading instruments
                        if ("Trading".equalsIgnoreCase(status)) {
                            TickerInfo.Key key = new TickerInfo.Key(symbol, TickerType.CRYPTO);
                            TickerInfo info = new TickerInfo(
                                symbol,  // figi
                                symbol,  // ticker
                                symbol,  // isin
                                0.01,    // minPriceIncrement (default, will be overridden below)
                                1,       // lot
                                "USDT",  // currency
                                symbol,  // name
                                "CRYPTO" // type
                            );
                            
                            // Parse lot size filter for accurate step
                            if (instrument.has("lotSizeFilter")) {
                                JsonObject lotFilter = instrument.getAsJsonObject("lotSizeFilter");
                                if (lotFilter.has("qtyStep")) {
                                    setField(info, "minPriceIncrement", lotFilter.get("qtyStep").getAsDouble());
                                }
                            }
                            
                            result.put(key, info);
                        }
                    }
                }
            }
            
            cachedFuturesList = result;
            cachedFuturesListAt = Instant.now();
            log("ByBitService.getFuturesList: loaded " + result.size() + " crypto instruments");
        } catch (Exception e) {
            log("ByBitService.getFuturesList error: " + e.getMessage());
            if (cachedFuturesList != null) {
                return cachedFuturesList;
            }
        }
        
        return result;
    }

    @Override
    public Map<TickerInfo.Key, TickerInfo> getStockList() {
        return new HashMap<>();
    }

    @Override
    public Map<TickerInfo.Key, TickerInfo> getEtfList() {
        return new HashMap<>();
    }

    @Override
    public Map<TickerInfo.Key, TickerInfo> getBondList() {
        return new HashMap<>();
    }

    @Override
    public Map<TickerInfo.Key, TickerInfo> getCurrenciesList() {
        return new HashMap<>();
    }

    @Override
    public TickerInfo searchTicker(TickerInfo.Key key) {
        if (key == null) {
            return null;
        }
        Map<TickerInfo.Key, TickerInfo> futures = getFuturesList();
        return futures.get(key);
    }

    @Override
    public String figiByName(TickerInfo.Key key) {
        if (key == null) {
            return "";
        }
        return key.getTicker();
    }

    @Override
    public boolean isTradableForAccount(TickerInfo info) {
        // ByBit has no per-account qualification; instrument existence implies tradability.
        if (info == null) {
            return false;
        }
        // Some instruments may be delisted/suspended; avoid them only when status is explicitly bad.
        if (info.getType() == null) {
            return false;
        }
        return true;
    }

    @Override
    public void logAccountTradingEligibility() {
        try {
            Double cash = getAvailableCash();
            log("ByBit account - available cash: " + cash);
        } catch (Exception e) {
            log("ByBit account eligibility check error: " + e.getMessage());
        }
    }

    @Override
    public void logAccountPositions() {
        try {
            Map<TickerInfo.Key, PositionInfo> positions = getCurrentPositions(TickerType.CRYPTO);
            log("ByBit account positions: " + positions.size());
            for (Map.Entry<TickerInfo.Key, PositionInfo> entry : positions.entrySet()) {
                PositionInfo pos = entry.getValue();
                log("  " + entry.getKey() + ": balance=" + pos.getBalance() + 
                    ", avgPrice=" + pos.getAveragePositionPrice());
            }
        } catch (Exception e) {
            log("ByBit account positions log error: " + e.getMessage());
        }
    }

    // ========== MARKET DATA METHODS ==========

    @Override
    public List<Candle> getCandles(String figi, Instant start, Instant end, String interval) {
        List<Candle> result = new ArrayList<>();
        try {
            MarketInterval intervalEnum = mapInterval(interval);
            
            long startMs = start.toEpochMilli();
            long endMs = end.toEpochMilli();
            
            // Use SDK builder pattern for getMarketLinesData
            MarketDataRequest request = MarketDataRequest.builder()
                .category(CategoryType.LINEAR)
                .symbol(figi)
                .marketInterval(intervalEnum)
                .start(startMs)
                .end(endMs)
                .limit(1000)
                .build();
            
            Object response = marketClient.getMarketLinesData(request);
            
            // Parse JSON response
            JsonObject json = gson.fromJson(gson.toJson(response), JsonObject.class);
            if (json.has("result") && json.get("result").isJsonObject()) {
                JsonObject resultObj = json.getAsJsonObject("result");
                if (resultObj.has("list") && resultObj.get("list").isJsonArray()) {
                    JsonArray list = resultObj.getAsJsonArray("list");
                    for (JsonElement element : list) {
                        JsonArray candle = element.getAsJsonArray();
                        // ByBit returns timestamp as long (milliseconds since epoch)
                        long timestamp = candle.get(0).getAsLong();
                        String time = java.time.Instant.ofEpochMilli(timestamp)
                            .atOffset(java.time.ZoneOffset.UTC)
                            .format(java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
                        
                        double open = candle.get(1).getAsDouble();
                        double high = candle.get(2).getAsDouble();
                        double low = candle.get(3).getAsDouble();
                        double close = candle.get(4).getAsDouble();
                        double volumeDouble = candle.get(5).getAsDouble();
                        long volume = Math.round(volumeDouble);
                        
                        result.add(new Candle(time, open, high, low, close, volume));
                    }
                }
            }
        } catch (Exception e) {
            log("ByBitService.getCandles error: " + e.getMessage());
        }
        return result;
    }

    @Override
    public List<Candle> getCandles(String figi, OffsetDateTime start, OffsetDateTime end, String interval) {
        return getCandles(figi, start.toInstant(), end.toInstant(), interval);
    }

    @Override
    public List<Candle> getLastCandles(String ticker, TickerType type, int size) {
        TickerInfo.Key key = new TickerInfo.Key(ticker, type);
        String figi = figiByName(key);
        
        Instant end = Instant.now();
        Instant start = end.minus(size * 60, java.time.temporal.ChronoUnit.MINUTES);
        
        return getCandles(figi, start, end, "HOUR");
    }

    @Override
    public List<TickerCandle> getLastCandlesAsTickerCandles(String ticker, TickerType type, int count) {
        List<Candle> candles = getLastCandles(ticker, type, count);
        return candles.stream()
            .map(c -> new TickerCandle(ticker, c.time, c.open, c.high, c.low, c.close, c.close, (long)c.volume))
            .collect(Collectors.toList());
    }

    @Override
    @SuppressWarnings("unchecked")
    public Map<String, Map<Double, Long>> getCurrentPrices(TickerInfo.Key key, boolean isPrintGlass) {
        Map<String, Map<Double, Long>> cached = pricesRepository.getById(key);
        if (cached != null) {
            if (isPrintGlass) {
                printGlassOfPrices(key.getTicker(), cached);
            }
            return cached;
        }
        
        try {
            // Build request for orderbook
            MarketDataRequest request = MarketDataRequest.builder()
                .category(CategoryType.LINEAR)
                .symbol(key.getTicker())
                .limit(20)
                .build();
            
            // Call SDK method
            Object response = marketClient.getMarketOrderBook(request);
            
            // Parse JSON response
            JsonObject json = gson.fromJson(gson.toJson(response), JsonObject.class);
            
            Map<String, Map<Double, Long>> prices = new HashMap<>();
            Map<Double, Long> bids = new TreeMap<>(reverseOrder());
            Map<Double, Long> asks = new TreeMap<>();
            
            if (json.has("result") && json.get("result").isJsonObject()) {
                JsonObject resultObj = json.getAsJsonObject("result");
                if (resultObj.has("b") && resultObj.get("b").isJsonArray()) {
                    for (JsonElement e : resultObj.getAsJsonArray("b")) {
                        JsonArray level = e.getAsJsonArray();
                        double price = level.get(0).getAsNumber().doubleValue();
                        long qty = level.get(1).getAsNumber().longValue();
                        bids.put(price, qty);
                    }
                }
                if (resultObj.has("a") && resultObj.get("a").isJsonArray()) {
                    for (JsonElement e : resultObj.getAsJsonArray("a")) {
                        JsonArray level = e.getAsJsonArray();
                        double price = level.get(0).getAsNumber().doubleValue();
                        long qty = level.get(1).getAsNumber().longValue();
                        asks.put(price, qty);
                    }
                }
            }
            
            prices.put("bids", bids);
            prices.put("asks", asks);
            pricesRepository.insert(key, prices);
            
            if (isPrintGlass) {
                printGlassOfPrices(key.getTicker(), prices);
            }
            
            return prices;
        } catch (Exception e) {
            log("ByBitService.getCurrentPrices error: " + e.getMessage());
            return createEmptyPrices();
        }
    }
    
    private double getBasePriceForSymbol(String symbol) {
        // Simulated base prices for popular crypto
        switch (symbol) {
            case "BTCUSDT": return 95000.0;
            case "ETHUSDT": return 3500.0;
            case "SOLUSDT": return 150.0;
            case "XRPUSDT": return 0.65;
            case "BNBUSDT": return 600.0;
            case "DOGEUSDT": return 0.15;
            case "ADAUSDT": return 0.45;
            case "AVAXUSDT": return 35.0;
            case "TRXUSDT": return 0.18;
            case "LINKUSDT": return 18.0;
            default: return 100.0 + (symbol.hashCode() % 1000);
        }
    }

    @Override
    public Map<String, Map<Double, Long>> getCurrentPrices(TickerInfo.Key key) {
        return getCurrentPrices(key, false);
    }

    @Override
    public double getLiveAskPrice(TickerInfo.Key key) {
        Map<String, Map<Double, Long>> prices = getCurrentPrices(key);
        Map<Double, Long> asks = prices.get("asks");
        if (asks != null && !asks.isEmpty()) {
            return ((TreeMap<Double, Long>) asks).firstKey();
        }
        return 0.0;
    }

    @Override
    public double getLiveBidPrice(TickerInfo.Key key) {
        Map<String, Map<Double, Long>> prices = getCurrentPrices(key);
        Map<Double, Long> bids = prices.get("bids");
        if (bids != null && !bids.isEmpty()) {
            return ((TreeMap<Double, Long>) bids).firstKey();
        }
        return 0.0;
    }

    @Override
    public double getAvailablePrice(String name, TickerType type) {
        TickerInfo.Key key = new TickerInfo.Key(name, type);
        return getAvailablePrice(key, 1, false);
    }

    @Override
    public double getAvailablePrice(TickerInfo.Key key) {
        return getAvailablePrice(key, 1, false);
    }

    @Override
    public double getAvailablePrice(String name, TickerType type, int count, String glassType) {
        TickerInfo.Key key = new TickerInfo.Key(name, type);
        return getAvailablePrice(key, count, "bids".equalsIgnoreCase(glassType));
    }

    @Override
    public double getAvailablePrice(String name, TickerType type, int count, String glassType, boolean isPrintGlass) {
        TickerInfo.Key key = new TickerInfo.Key(name, type);
        Map<String, Map<Double, Long>> prices = getCurrentPrices(key, isPrintGlass);
        
        if ("asks".equalsIgnoreCase(glassType) || "ask".equalsIgnoreCase(glassType)) {
            return calculateVwap(prices.get("asks"), count);
        } else {
            return calculateVwap(prices.get("bids"), count);
        }
    }

    @Override
    public double getAvailablePrice(TickerInfo.Key key, int count, boolean isPrintGlass) {
        return getAvailablePrice(key, count, "bids", isPrintGlass);
    }

    @Override
    public double getAvailablePrice(TickerInfo.Key key, int count, String type, boolean isPrintGlass) {
        Map<String, Map<Double, Long>> prices = getCurrentPrices(key, isPrintGlass);
        
        if ("asks".equalsIgnoreCase(type) || "ask".equalsIgnoreCase(type)) {
            return calculateVwap(prices.get("asks"), count);
        } else {
            return calculateVwap(prices.get("bids"), count);
        }
    }

    @Override
    public List<MarketTradeTick> getRecentTrades(TickerInfo.Key key, Duration maxAge) {
        List<MarketTradeTick> cached = recentTradesByTicker.get(key);
        if (cached == null) {
            return emptyList();
        }
        
        Instant cutoff = Instant.now().minus(maxAge);
        return cached.stream()
            .filter(t -> t.getTime().isAfter(cutoff))
            .collect(Collectors.toList());
    }

    @Override
    public List<MarketTradeTick> getLastTrades(TickerInfo.Key key, Instant from, Instant to) {
        List<MarketTradeTick> result = new ArrayList<>();
        try {
            // Build request for recent trades
            MarketDataRequest request = MarketDataRequest.builder()
                .category(CategoryType.LINEAR)
                .symbol(key.getTicker())
                .limit(1000)
                .build();
            
            // Call SDK method
            Object response = marketClient.getRecentTradeData(request);
            
            // Parse JSON response
            JsonObject json = gson.fromJson(gson.toJson(response), JsonObject.class);
            if (json.has("result") && json.get("result").isJsonObject()) {
                JsonObject resultObj = json.getAsJsonObject("result");
                if (resultObj.has("list") && resultObj.get("list").isJsonArray()) {
                    JsonArray list = resultObj.getAsJsonArray("list");
                    for (JsonElement element : list) {
                        JsonObject trade = element.getAsJsonObject();
                        long timeMs = trade.get("time").getAsNumber().longValue();
                        double price = trade.get("price").getAsNumber().doubleValue();
                        long qty = trade.has("size") ? trade.get("size").getAsNumber().longValue() : 0;
                        String side = trade.get("side").getAsString();
                        
                        result.add(new MarketTradeTick(
                            key.getTicker(),
                            Instant.ofEpochMilli(timeMs),
                            price,
                            qty,
                            side
                        ));
                    }
                }
            }
        } catch (Exception e) {
            log("ByBitService.getLastTrades error: " + e.getMessage());
        }
        return result;
    }

    @Override
    public MarketDepthSnapshot getLastMarketDepth(TickerInfo.Key key) {
        return marketDepthByTicker.get(key);
    }

    @Override
    public void subscribeMarketData(TickerInfo.Key key, int depth, MarketTickListener listener) {
        // WebSocket subscription not implemented in this version
        // Market data will be polled via getCurrentPrices
        marketTickListenersByTicker.computeIfAbsent(key, k -> new CopyOnWriteArrayList<>())
            .add(listener);
        log("Market data subscription registered for " + key + " (polling mode)");
    }

    @Override
    public void unsubscribeMarketData(TickerInfo.Key key, MarketTickListener listener) {
        CopyOnWriteArrayList<MarketTickListener> listeners = marketTickListenersByTicker.get(key);
        if (listeners != null) {
            listeners.remove(listener);
            if (listeners.isEmpty()) {
                marketTickListenersByTicker.remove(key);
            }
        }
    }

    // ========== ACCOUNT METHODS ==========

    @Override
    public Double getAvailableCash() {
        return getAccountField("availableToWithdraw");
    }

    @Override
    public double getTotalPortfolioCost() {
        return getAccountField("totalEquity");
    }

    private double getAccountField(String fieldName) {
        try {
            // Build request for wallet balance
            AccountDataRequest request = AccountDataRequest.builder()
                .accountType(AccountType.UNIFIED)
                .build();
            
            // Call SDK method
            Object response = accountClient.getWalletBalance(request);
            
            // Parse JSON response
            String jsonStr = gson.toJson(response);
            JsonObject json = gson.fromJson(jsonStr, JsonObject.class);
            
            if (json.has("result") && json.get("result").isJsonObject()) {
                JsonObject resultObj = json.getAsJsonObject("result");
                if (resultObj.has("list") && resultObj.get("list").isJsonArray()) {
                    JsonArray list = resultObj.getAsJsonArray("list");
                    if (list.size() > 0) {
                        JsonObject account = list.get(0).getAsJsonObject();
                        if ("totalEquity".equals(fieldName)) {
                            if (account.has("totalEquity")) {
                                return account.get("totalEquity").getAsNumber().doubleValue();
                            }
                        } else {
                            if (account.has("coin") && account.get("coin").isJsonArray()) {
                                for (JsonElement coinElement : account.getAsJsonArray("coin")) {
                                    JsonObject coin = coinElement.getAsJsonObject();
                                    if ("USDT".equals(coin.get("coin").getAsString())) {
                                        if (coin.has(fieldName)) {
                                            return coin.get(fieldName).getAsNumber().doubleValue();
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        } catch (Exception e) {
            log("ByBitService.getAccountField error: " + e.getMessage());
        }
        return 0.0;
    }

    @Override
    public int getCountOfCurrentPositions(TickerType tickerType, String tickerName) {
        PositionInfo info = getCurrentPositions(tickerType, tickerName);
        return info != null ? info.getBalance() : 0;
    }

    @Override
    public PositionInfo getCurrentPositions(TickerType tickerType, String tickerName) {
        Map<TickerInfo.Key, PositionInfo> allPositions = getCurrentPositions(tickerType);
        TickerInfo.Key key = new TickerInfo.Key(tickerName, tickerType);
        return allPositions.get(key);
    }

    @Override
    @SuppressWarnings("unchecked")
    public Map<TickerInfo.Key, PositionInfo> getCurrentPositions(TickerType tickerType) {
        Map<TickerInfo.Key, PositionInfo> result = new HashMap<>();
        try {
            // Build request for positions
            PositionDataRequest request = PositionDataRequest.builder()
                .category(CategoryType.LINEAR)
                .build();
            
            // Call SDK method
            Object response = positionClient.getPositionInfo(request);
            
            // Parse JSON response
            String jsonStr = gson.toJson(response);
            JsonObject json = gson.fromJson(jsonStr, JsonObject.class);
            
            if (json.has("result") && json.get("result").isJsonObject()) {
                JsonObject resultObj = json.getAsJsonObject("result");
                if (resultObj.has("list") && resultObj.get("list").isJsonArray()) {
                    JsonArray list = resultObj.getAsJsonArray("list");
                    for (JsonElement element : list) {
                        JsonObject pos = element.getAsJsonObject();
                        String symbol = pos.get("symbol").getAsString();
                        String side = pos.get("side").getAsString();
                        double size = pos.get("size").getAsNumber().doubleValue();
                        double avgPrice = pos.has("avgPrice") ? pos.get("avgPrice").getAsNumber().doubleValue() : 0;
                        
                        if (size > 0) {
                            TickerInfo.Key key = new TickerInfo.Key(symbol, TickerType.CRYPTO);
                            int balance = (int) size;
                            if ("Sell".equalsIgnoreCase(side)) {
                                balance = -balance;
                            }
                            
                            PositionInfo info = new PositionInfo(
                                symbol, symbol, symbol, "CRYPTO",
                                balance, 0.0, 1, avgPrice, symbol
                            );
                            result.put(key, info);
                        }
                    }
                }
            }
        } catch (Exception e) {
            log("ByBitService.getCurrentPositions error: " + e.getMessage());
        }
        return result;
    }

    // ========== TRADING METHODS ==========

    @Override
    public int calculateTradeCount(TickerInfo.Key key, double availableCash, double price) {
        TickerInfo info = searchTicker(key);
        if (info == null) {
            return 0;
        }
        
        int lot = info.getLot() != null ? info.getLot() : 1;
        double requiredPerUnit = price * FUTURES_MARGIN_RATE;
        int maxCount = (int) (availableCash / requiredPerUnit);
        
        return max(0, (maxCount / lot) * lot);
    }

    @Override
    public double getRequiredCashForOrder(TickerInfo.Key key, int count, double price) {
        return price * count * FUTURES_MARGIN_RATE;
    }

    @Override
    public int createOrder(TickerInfo.Key key, double price, int count, String operation) {
        OrderExecutionResult result = createOrder(key, price, count, operation, 0, 0, false, 0);
        return result.isSuccess() ? 1 : 0;
    }

    @Override
    public OrderExecutionResult createOrder(TickerInfo.Key key, double price, int count, 
            String operation, double takeProfit, double stopLoss, boolean isFullPrice) {
        return createOrder(key, price, count, operation, takeProfit, stopLoss, isFullPrice, 0);
    }

    @Override
    @SuppressWarnings("unchecked")
    public OrderExecutionResult createOrder(TickerInfo.Key key, double price, int count,
            String operation, double takeProfit, double stopLoss, boolean isFullPrice, double cashToUse) {
        try {
            Side side = "Buy".equalsIgnoreCase(operation) ? Side.BUY : Side.SELL;
            TradeOrderType orderType = price > 0 ? TradeOrderType.LIMIT : TradeOrderType.MARKET;
            
            // Build trade order request using builder
            var builder = TradeOrderRequest.builder()
                .category(CategoryType.LINEAR)
                .symbol(key.getTicker())
                .side(side)
                .orderType(orderType)
                .qty(String.valueOf(count))
                .timeInForce(TimeInForce.GTC);
            
            if (price > 0) {
                builder.price(String.valueOf(price));
            }
            
            if (takeProfit > 0) {
                builder.takeProfit(String.valueOf(takeProfit));
                builder.tpTriggerBy(TriggerBy.LAST_PRICE);
            }
            
            if (stopLoss > 0) {
                builder.stopLoss(String.valueOf(stopLoss));
                builder.slTriggerBy(TriggerBy.LAST_PRICE);
            }
            
            // Set position index for one-way mode
            builder.positionIdx(PositionIdx.ONE_WAY_MODE);
            
            TradeOrderRequest request = builder.build();
            
            // Call SDK method
            Object response = tradeClient.createOrder(request);
            
            // Parse JSON response
            String jsonStr = gson.toJson(response);
            JsonObject json = gson.fromJson(jsonStr, JsonObject.class);
            
            if (json.has("retCode") && json.get("retCode").getAsInt() == 0) {
                Position protectivePosition = null;
                if (takeProfit > 0 || stopLoss > 0) {
                    protectivePosition = createProtectiveOrders(key, count, side, takeProfit, stopLoss);
                }
                
                return OrderExecutionResult.success(price, count, 0, protectivePosition);
            } else {
                int retCode = json.has("retCode") ? json.get("retCode").getAsInt() : -1;
                String retMsg = json.has("retMsg") ? json.get("retMsg").getAsString() : "Unknown error";
                log("ByBit order failed: " + retMsg);
                return OrderExecutionResult.failed(retCode, retMsg);
            }
        } catch (Exception e) {
            log("ByBitService.createOrder error: " + e.getMessage());
            return OrderExecutionResult.failed(e.getMessage());
        }
    }

    @Override
    public OrderExecutionResult buyByMarketWithDetails(String name, TickerType type, 
            double cashToBuy, double takeProfit, double stopLoss) {
        TickerInfo.Key key = new TickerInfo.Key(name, type);
        double askPrice = getLiveAskPrice(key);
        
        if (askPrice <= 0) {
            return OrderExecutionResult.failed("Cannot get ask price for " + name);
        }
        
        int count = calculateTradeCount(key, cashToBuy, askPrice);
        if (count <= 0) {
            return OrderExecutionResult.failed("Cannot calculate trade count");
        }
        
        return createOrder(key, 0, count, "Buy", takeProfit, stopLoss, false, cashToBuy);
    }

    @Override
    public OrderExecutionResult sellByMarketWithDetails(String name, TickerType type,
            double cashToSell, double takeProfit, double stopLoss) {
        TickerInfo.Key key = new TickerInfo.Key(name, type);
        double bidPrice = getLiveBidPrice(key);
        
        if (bidPrice <= 0) {
            return OrderExecutionResult.failed("Cannot get bid price for " + name);
        }
        
        int count = calculateTradeCount(key, cashToSell, bidPrice);
        if (count <= 0) {
            return OrderExecutionResult.failed("Cannot calculate trade count");
        }
        
        return createOrder(key, 0, count, "Sell", takeProfit, stopLoss, false, cashToSell);
    }

    @Override
    public OrderExecutionResult buy(String name, TickerType type, double amount,
            boolean isLimit, double price, double tpPercent, boolean useMargin) {
        TickerInfo.Key key = new TickerInfo.Key(name, type);
        double takeProfit = isLimit ? price * (1 + tpPercent / 100) : 0;
        double stopLoss = isLimit ? price * (1 - tpPercent / 100) : 0;
        
        int count = isLimit ? (int) amount : calculateTradeCount(key, amount, price);
        return createOrder(key, isLimit ? price : 0, count, "Buy", takeProfit, stopLoss, false, amount);
    }

    @Override
    public OrderExecutionResult sell(String name, TickerType type, double amount,
            boolean isLimit, double price, double tpPercent, boolean useMargin) {
        TickerInfo.Key key = new TickerInfo.Key(name, type);
        double takeProfit = isLimit ? price * (1 - tpPercent / 100) : 0;
        double stopLoss = isLimit ? price * (1 + tpPercent / 100) : 0;
        
        int count = isLimit ? (int) amount : calculateTradeCount(key, amount, price);
        return createOrder(key, isLimit ? price : 0, count, "Sell", takeProfit, stopLoss, false, amount);
    }

    @Override
    public OrderExecutionResult closeLongByMarketWithDetails(String name, TickerType type) {
        TickerInfo.Key key = new TickerInfo.Key(name, type);
        PositionInfo pos = getCurrentPositions(TickerType.CRYPTO, name);
        
        if (pos == null || pos.getBalance() <= 0) {
            return OrderExecutionResult.failed("No long position to close");
        }
        
        int count = pos.getBalance();
        return createOrder(key, 0, count, "Sell", 0, 0, false, 0);
    }

    @Override
    public OrderExecutionResult closeShortByMarketWithDetails(String name, TickerType type) {
        TickerInfo.Key key = new TickerInfo.Key(name, type);
        PositionInfo pos = getCurrentPositions(TickerType.CRYPTO, name);
        
        if (pos == null || pos.getBalance() >= 0) {
            return OrderExecutionResult.failed("No short position to close");
        }
        
        int count = Math.abs(pos.getBalance());
        return createOrder(key, 0, count, "Buy", 0, 0, false, 0);
    }

    @Override
    public boolean closeLongByMarket(String name, TickerType type) {
        return closeLongByMarketWithDetails(name, type).isSuccess();
    }

    @Override
    public boolean closeShortByMarket(String name, TickerType type) {
        return closeShortByMarketWithDetails(name, type).isSuccess();
    }

    @Override
    public void closeAllByMarket(TickerType type) {
        Map<TickerInfo.Key, PositionInfo> positions = getCurrentPositions(type);
        for (Map.Entry<TickerInfo.Key, PositionInfo> entry : positions.entrySet()) {
            PositionInfo pos = entry.getValue();
            if (pos.getBalance() != 0) {
                String operation = pos.getBalance() > 0 ? "Sell" : "Buy";
                int count = Math.abs(pos.getBalance());
                createOrder(entry.getKey(), 0, count, operation, 0, 0, false, 0);
            }
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public StopLossOrderResult createStopLossOrder(TickerInfo.Key key, int units,
            double stopLossPrice, String operation) {
        try {
            Side side = "Buy".equalsIgnoreCase(operation) ? Side.BUY : Side.SELL;
            
            var request = TradeOrderRequest.builder()
                .category(CategoryType.LINEAR)
                .symbol(key.getTicker())
                .side(side)
                .orderType(TradeOrderType.MARKET)
                .qty(String.valueOf(units))
                .stopLoss(String.valueOf(stopLossPrice))
                .slTriggerBy(TriggerBy.LAST_PRICE)
                .closeOnTrigger(true)
                .positionIdx(PositionIdx.ONE_WAY_MODE)
                .build();
            
            Object response = tradeClient.createOrder(request);
            
            String jsonStr = gson.toJson(response);
            JsonObject json = gson.fromJson(jsonStr, JsonObject.class);
            
            if (json.has("retCode") && json.get("retCode").getAsInt() == 0) {
                JsonObject resultObj = json.getAsJsonObject("result");
                String orderId = resultObj.has("orderId") ? resultObj.get("orderId").getAsString() : "";
                return StopLossOrderResult.success(orderId);
            }
        } catch (Exception e) {
            log("ByBitService.createStopLossOrder error: " + e.getMessage());
        }
        return StopLossOrderResult.failed();
    }

    @Override
    @SuppressWarnings("unchecked")
    public void cancelStopOrder(TickerInfo.Key key, String stopOrderId, String orderTypeName) {
        try {
            var request = TradeOrderRequest.builder()
                .category(CategoryType.LINEAR)
                .symbol(key.getTicker())
                .orderId(stopOrderId)
                .orderFilter(OrderFilter.STOP_ORDER)
                .build();
            
            tradeClient.cancelOrder(request);
            log("Cancelled stop order " + stopOrderId + " for " + key);
        } catch (Exception e) {
            log("ByBitService.cancelStopOrder error: " + e.getMessage());
        }
    }

    @Override
    public void syncProtectiveOrders(String name, TickerType type, Position position) {
        log("ByBitService.syncProtectiveOrders - TP/SL managed by ByBit position");
    }

    @Override
    public Position restoreProtectivePosition(String name, TickerType type, Position position) {
        return position;
    }

    @Override
    public Double getLastExecutedPrice(String name, TickerType type) {
        TickerInfo.Key key = new TickerInfo.Key(name, type);
        return lastExecutedPriceByTicker.get(key);
    }

    // ========== HELPER METHODS ==========

    private Map<String, Map<Double, Long>> createEmptyPrices() {
        Map<String, Map<Double, Long>> prices = new HashMap<>();
        prices.put("bids", new TreeMap<>(reverseOrder()));
        prices.put("asks", new TreeMap<>());
        return prices;
    }

    private double calculateVwap(Map<Double, Long> levels, long targetCount) {
        if (levels == null || levels.isEmpty() || targetCount <= 0) {
            return 0.0;
        }
        
        long accumulated = 0;
        double totalCost = 0.0;
        
        for (Map.Entry<Double, Long> entry : levels.entrySet()) {
            double price = entry.getKey();
            long qty = entry.getValue();
            
            long take = Math.min(targetCount - accumulated, qty);
            totalCost += price * take;
            accumulated += take;
            
            if (accumulated >= targetCount) {
                break;
            }
        }
        
        return accumulated > 0 ? totalCost / accumulated : 0.0;
    }

    private MarketInterval mapInterval(String interval) {
        if (interval == null) {
            return MarketInterval.HOURLY;
        }
        
        switch (interval.toUpperCase()) {
            case "1_MIN":
            case "M1":
                return MarketInterval.ONE_MINUTE;
            case "3_MIN":
            case "M3":
                return MarketInterval.THREE_MINUTES;
            case "5_MIN":
            case "M5":
                return MarketInterval.FIVE_MINUTES;
            case "15_MIN":
            case "M15":
                return MarketInterval.FIFTEEN_MINUTES;
            case "30_MIN":
            case "M30":
                return MarketInterval.HALF_HOURLY;
            case "HOUR":
            case "1H":
            case "60_MIN":
                return MarketInterval.HOURLY;
            case "2H":
            case "2_HOUR":
                return MarketInterval.TWO_HOURLY;
            case "4H":
            case "4_HOUR":
                return MarketInterval.FOUR_HOURLY;
            case "6H":
            case "6_HOUR":
                return MarketInterval.SIX_HOURLY;
            case "12H":
            case "12_HOUR":
                return MarketInterval.TWELVE_HOURLY;
            case "1d":
            case "DAY":
            case "1D":
                return MarketInterval.DAILY;
            case "1W":
            case "WEEK":
                return MarketInterval.WEEKLY;
            case "1M":
            case "MONTH":
                return MarketInterval.MONTHLY;
            default:
                return MarketInterval.HOURLY;
        }
    }

    // ========== HELPER METHODS ==========

    /**
     * Invoke a market data method using reflection.
     */
    private String invokeMarketMethod(String methodName, Object... params) throws Exception {
        java.lang.reflect.Method method = marketClient.getClass().getMethod(methodName, 
            java.util.stream.IntStream.range(0, params.length)
                .mapToObj(i -> params[i].getClass())
                .toArray(Class[]::new));
        Object response = method.invoke(marketClient, params);
        return gson.toJson(response);
    }

    private Position createProtectiveOrders(TickerInfo.Key key, int count, Side side,
            double takeProfit, double stopLoss) {
        if (takeProfit <= 0 || stopLoss <= 0) {
            return null;
        }
        
        return new Position(side.name().equals("BUY") ? "Long" : "Short",
            null, stopLoss, takeProfit, count, 0);
    }

    private void setField(TickerInfo info, String fieldName, Object value) {
        try {
            java.lang.reflect.Field field = TickerInfo.class.getDeclaredField(fieldName);
            field.setAccessible(true);
            field.set(info, value);
        } catch (Exception e) {
            // Ignore reflection errors
        }
    }

    // ========== INNER CLASSES ==========

    private static class ProtectiveOrders {
        String takeProfitOrderId;
        String stopLossOrderId;
        
        ProtectiveOrders(String tpOrderId, String slOrderId) {
            this.takeProfitOrderId = tpOrderId;
            this.stopLossOrderId = slOrderId;
        }
    }
}
