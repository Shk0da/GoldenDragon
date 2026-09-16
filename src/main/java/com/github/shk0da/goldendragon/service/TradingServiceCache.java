package com.github.shk0da.goldendragon.service;

import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;
import com.github.shk0da.goldendragon.model.PositionInfo;
import com.github.shk0da.goldendragon.model.TickerInfo;
import com.github.shk0da.goldendragon.model.TickerType;

import java.time.Duration;
import java.time.Instant;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Callable;
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
 *   <li>Cash: 5 seconds</li>
 *   <li>Prices: 1 second</li>
 *   <li>Positions: 10 seconds</li>
 * </ul>
 */
public class TradingServiceCache implements TradingService {
    
    private final TradingService delegate;
    private final Cache<String, Object> cache;
    
    // TTL для разных типов данных
    private static final Duration CASH_TTL = Duration.ofSeconds(5);
    private static final Duration PRICE_TTL = Duration.ofSeconds(1);
    private static final Duration POSITIONS_TTL = Duration.ofSeconds(10);
    
    public TradingServiceCache(TradingService delegate) {
        this.delegate = delegate;
        this.cache = Caffeine.newBuilder()
            .maximumSize(100)
            .expireAfterWrite(1, TimeUnit.SECONDS)
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
        // Invalidate cash cache after order
        invalidateCash();
        return delegate.buyByMarketWithDetails(name, type, cashToBuy, takeProfit, stopLose);
    }
    
    @Override
    public OrderExecutionResult sellByMarketWithDetails(String name, TickerType type, 
            double cashToSell, double takeProfit, double stopLose) {
        // Invalidate cash cache after order
        invalidateCash();
        return delegate.sellByMarketWithDetails(name, type, cashToSell, takeProfit, stopLose);
    }
    
    @Override
    public OrderExecutionResult closeLongByMarketWithDetails(String name, TickerType type) {
        // Invalidate cash cache after order
        invalidateCash();
        return delegate.closeLongByMarketWithDetails(name, type);
    }
    
    @Override
    public OrderExecutionResult closeShortByMarketWithDetails(String name, TickerType type) {
        // Invalidate cash cache after order
        invalidateCash();
        return delegate.closeShortByMarketWithDetails(name, type);
    }
    
    @Override
    public Double getSingleContractGo(String figi) {
        // Delegate to underlying service - no caching for margin data
        return delegate.getSingleContractGo(figi);
    }
    
    /**
     * Get value with TTL-based caching.
     */
    @SuppressWarnings("unchecked")
    private <T> T getWithTTL(String key, Callable<T> loader, Duration ttl) {
        Object cached = cache.getIfPresent(key);
        if (cached instanceof CachedValue) {
            CachedValue<T> cv = (CachedValue<T>) cached;
            if (!cv.isStale(ttl)) {
                return cv.value;
            }
        }
        
        try {
            T value = loader.call();
            cache.put(key, new CachedValue<>(value));
            return value;
        } catch (Exception e) {
            throw new RuntimeException("Failed to load " + key, e);
        }
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
