package com.github.shk0da.goldendragon.service;

import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;
import com.github.shk0da.goldendragon.model.Candle;
import com.github.shk0da.goldendragon.model.OrderExecutionResult;
import com.github.shk0da.goldendragon.model.Position;
import com.github.shk0da.goldendragon.model.PositionInfo;
import com.github.shk0da.goldendragon.model.TickerInfo;
import com.github.shk0da.goldendragon.model.TickerType;

import java.time.Duration;
import java.time.Instant;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Callable;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;

/**
 * Cache wrapper for TradingService to prevent duplicate API calls.
 *
 * <p>Usage:
 * <pre>
 * TradingService cached = new TradingServiceCache(delegate);
 * cached.getAvailableCash();  // First call: API
 * cached.getAvailableCash();  // Second call: cache (if &lt; 5s)
 * </pre>
 *
 * TTL configuration:
 * <ul>
 *   <li>Cash: 1 second</li>
 *   <li>Prices: 1 second</li>
 *   <li>Positions: 5 seconds</li>
 *   <li>Portfolio: 5 seconds</li>
 * </ul>
 */
public class TradingServiceCache implements TradingService {

    private final TradingService delegate;
    private final Cache<String, Object> cache;
    private final Map<String, Object> pendingLoads = new ConcurrentHashMap<>();

    // TTL для разных типов данных
    private static final Duration CASH_TTL = Duration.ofSeconds(1);
    private static final Duration PRICE_TTL = Duration.ofSeconds(1);
    private static final Duration POSITIONS_TTL = Duration.ofSeconds(5);
    private static final Duration PORTFOLIO_TTL = Duration.ofSeconds(5);

    public TradingServiceCache(TradingService delegate) {
        this.delegate = delegate;
        // No global expiration - TTL is managed per-key by getWithTTL()
        this.cache = Caffeine.newBuilder()
            .maximumSize(100)
            .build();
    }

    @Override
    public Double getAvailableCash() {
        return getWithTTL("cash:RUB", delegate::getAvailableCash, CASH_TTL);
    }

    @Override
    public double getInitialBalance() {
        // Delegate to avoid double-call - the delegate will use cached value
        return delegate.getInitialBalance();
    }

    @Override
    public double getLiveAskPrice(TickerInfo.Key key) {
        return getWithTTL("price:" + key + ":ask",
            () -> delegate.getLiveAskPrice(key), PRICE_TTL);
    }

    @Override
    public double getLiveBidPrice(TickerInfo.Key key) {
        return getWithTTL("price:" + key + ":bid",
            () -> delegate.getLiveBidPrice(key), PRICE_TTL);
    }

    @Override
    public PositionInfo getCurrentPositions(TickerType tickerType, String tickerName) {
        return getWithTTL("position:" + tickerType + ":" + tickerName,
            () -> delegate.getCurrentPositions(tickerType, tickerName), POSITIONS_TTL);
    }

    @Override
    public Map<TickerInfo.Key, PositionInfo> getCurrentPositions(TickerType tickerType) {
        return getWithTTL("positions:" + (tickerType != null ? tickerType : "all"),
            () -> delegate.getCurrentPositions(tickerType), POSITIONS_TTL);
    }

    @Override
    public int getCountOfCurrentPositions(TickerType tickerType, String tickerName) {
        PositionInfo info = getCurrentPositions(tickerType, tickerName);
        return info != null ? info.getBalance() : 0;
    }

    @Override
    public List<Map<String, Object>> getTradeHistory(Instant since) {
        return delegate.getTradeHistory(since);
    }

    @Override
    public OrderExecutionResult buyByMarketWithDetails(String name, TickerType type,
            double cashToBuy, double takeProfit, double stopLose) {
        // Invalidate cash cache BEFORE order to get fresh balance
        invalidateCash();
        return delegate.buyByMarketWithDetails(name, type, cashToBuy, takeProfit, stopLose);
    }

    @Override
    public OrderExecutionResult sellByMarketWithDetails(String name, TickerType type,
            double cashToSell, double takeProfit, double stopLose) {
        // Invalidate cash cache BEFORE order to get fresh balance
        invalidateCash();
        return delegate.sellByMarketWithDetails(name, type, cashToSell, takeProfit, stopLose);
    }

    @Override
    public OrderExecutionResult closeLongByMarketWithDetails(String name, TickerType type) {
        // Invalidate cash cache BEFORE order to get fresh balance
        invalidateCash();
        return delegate.closeLongByMarketWithDetails(name, type);
    }

    @Override
    public OrderExecutionResult closeShortByMarketWithDetails(String name, TickerType type) {
        // Invalidate cash cache BEFORE order to get fresh balance
        invalidateCash();
        return delegate.closeShortByMarketWithDetails(name, type);
    }

    @Override
    public Double getSingleContractGo(String figi) {
        // Delegate to underlying service - no caching for margin data
        return delegate.getSingleContractGo(figi);
    }

    /**
     * Get value with TTL-based caching and per-key locking to prevent cache stampede.
     * If loader fails and stale cache exists, returns stale value instead of throwing.
     */
    @SuppressWarnings("unchecked")
    private <T> T getWithTTL(String key, Callable<T> loader, Duration ttl) {
        Object cached = cache.getIfPresent(key);
        boolean hasStaleCache = false;
        T staleValue = null;
        
        // Check if we have cached value (fresh or stale)
        if (cached instanceof CachedValue) {
            CachedValue<T> cv = (CachedValue<T>) cached;
            if (!cv.isStale(ttl)) {
                // Fresh cache - return immediately
                return cv.value;
            }
            // Stale cache - save it as fallback
            hasStaleCache = true;
            staleValue = cv.value;
        }

        // Per-key locking: only one thread loads per key, others wait
        synchronized (pendingLoads.computeIfAbsent(key, k -> new Object())) {
            try {
                // Double-check: maybe another thread already updated cache while we waited
                Object rechecked = cache.getIfPresent(key);
                if (rechecked instanceof CachedValue) {
                    CachedValue<T> cv = (CachedValue<T>) rechecked;
                    if (!cv.isStale(ttl)) {
                        return cv.value;
                    }
                }

                T value = loader.call();
                cache.put(key, new CachedValue<>(value));
                return value;
            } catch (Exception e) {
                // If we have stale cache, return it instead of throwing
                if (hasStaleCache) {
                    return staleValue;
                }
                // No cache and loader failed - throw
                throw new RuntimeException("Failed to load " + key + " (no cached value available)", e);
            } finally {
                pendingLoads.remove(key);
            }
        }
    }

    @Override
    public List<Candle> getCandles(String figi, String interval, int count) {
        // No caching for candles - delegate directly to avoid stale data
        return delegate.getCandles(figi, interval, count);
    }

    @Override
    public List<Candle> getCandles(String figi, java.time.Instant start, java.time.Instant end, String interval) {
        // No caching for candles - delegate directly
        return delegate.getCandles(figi, start, end, interval);
    }

    @Override
    public List<Candle> getCandles(String figi, java.time.OffsetDateTime start, java.time.OffsetDateTime end, String interval) {
        // No caching for candles - delegate directly
        return delegate.getCandles(figi, start, end, interval);
    }

    @Override
    public List<Candle> getLastCandles(String ticker, TickerType type, int size) {
        // No caching for last candles - delegate directly
        return delegate.getLastCandles(ticker, type, size);
    }

    @Override
    public Map<String, Map<Double, Long>> getCurrentPrices(TickerInfo.Key key, boolean isPrintGlass) {
        return delegate.getCurrentPrices(key, isPrintGlass);
    }

    @Override
    public double getTotalPortfolioCost() {
        return getWithTTL(
                "portfolio:total", delegate::getTotalPortfolioCost, PORTFOLIO_TTL);
    }

    @Override
    public double getTotalPortfolioValue() {
        return getWithTTL(
                "portfolio:total", delegate::getTotalPortfolioValue, PORTFOLIO_TTL);
    }

    @Override
    public double getAvailablePrice(TickerInfo.Key key) {
        return delegate.getAvailablePrice(key);
    }

    @Override
    public double getAvailablePrice(TickerInfo.Key key, int count, String type, boolean isPrintGlass) {
        return delegate.getAvailablePrice(key, count, type, isPrintGlass);
    }

    @Override
    public int calculateTradeCount(TickerInfo.Key key, double availableCash, double price) {
        return delegate.calculateTradeCount(key, availableCash, price);
    }

    @Override
    public TickerInfo searchTicker(TickerInfo.Key key) {
        return delegate.searchTicker(key);
    }

    @Override
    public void closeAllByMarket(TickerType tickerType) {
        delegate.closeAllByMarket(tickerType);
    }

    @Override
    public boolean closeLongByMarket(String name, TickerType type) {
        return delegate.closeLongByMarket(name, type);
    }

    @Override
    public OrderExecutionResult closeLongByMarketWithDetails(String name, TickerType type, int quantity) {
        return delegate.closeLongByMarketWithDetails(name, type, quantity);
    }

    @Override
    public boolean closeShortByMarket(String name, TickerType type) {
        return delegate.closeShortByMarket(name, type);
    }

    @Override
    public OrderExecutionResult closeShortByMarketWithDetails(String name, TickerType type, int quantity) {
        return delegate.closeShortByMarketWithDetails(name, type, quantity);
    }

    @Override
    public OrderExecutionResult closeLong(String ticker) {
        return delegate.closeLong(ticker);
    }

    @Override
    public OrderExecutionResult closeShort(String ticker) {
        return delegate.closeShort(ticker);
    }

    @Override
    public double getGlobalPeakEquity() {
        return delegate.getGlobalPeakEquity();
    }

    @Override
    public Position restoreProtectivePosition(String name, TickerType type, Position position) {
        // No caching for protective positions - delegate directly
        return delegate.restoreProtectivePosition(name, type, position);
    }

    @Override
    public void syncProtectiveOrders(String name, TickerType type, Position position) {
        // No caching for protective orders - delegate directly
        delegate.syncProtectiveOrders(name, type, position);
    }

    /**
     * Invalidate specific cache key.
     */
    public void invalidate(String key) {
        cache.invalidate(key);
    }

    /**
     * Invalidate all cash-related keys.
     */
    public void invalidateCash() {
        cache.asMap().keySet().removeIf(k -> k.startsWith("cash:"));
    }

    /**
     * Invalidate all price-related keys.
     */
    public void invalidatePrices() {
        cache.asMap().keySet().removeIf(k -> k.startsWith("price:"));
    }

    /**
     * Get cache statistics for monitoring.
     */
    public long getCacheSize() {
        return cache.estimatedSize();
    }

    private static class CachedValue<T> {
        final T value;
        final Instant timestamp;

        CachedValue(T value) {
            this.value = value;
            this.timestamp = Instant.now();
        }

        boolean isStale(Duration ttl) {
            return Duration.between(timestamp, Instant.now()).compareTo(ttl) > 0;
        }
    }
}
