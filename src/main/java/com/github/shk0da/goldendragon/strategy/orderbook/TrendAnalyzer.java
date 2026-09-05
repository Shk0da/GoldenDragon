package com.github.shk0da.goldendragon.strategy.orderbook;

import com.github.shk0da.goldendragon.model.Candle;
import com.github.shk0da.goldendragon.model.TickerInfo;
import com.github.shk0da.goldendragon.service.TradingService;
import com.github.shk0da.goldendragon.utils.IndicatorsUtil;
import com.github.shk0da.goldendragon.utils.LoggingUtils;

import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Analyzes trend on higher timeframes (5-min, 15-min) for multi-timeframe context.
 * 
 * <p>Key features:
 * <ul>
 *   <li>Determines trend direction: UP, DOWN, SIDEWAYS</li>
 *   <li>Identifies higher highs and lower lows</li>
 *   <li>Caches results to avoid excessive API calls</li>
 * </ul>
 */
public final class TrendAnalyzer {

    public enum Trend {
        UP, DOWN, SIDEWAYS
    }

    private final TradingService tradingService;
    private final int timeframeMinutes;
    private final int lookbackCandles;
    private final long cacheTtlMs;

    // Cache trend analysis results
    private final Map<String, TrendResult> trendCache = new ConcurrentHashMap<>();

    public TrendAnalyzer(TradingService tradingService, int timeframeMinutes, int lookbackCandles, long cacheTtlMs) {
        this.tradingService = tradingService;
        this.timeframeMinutes = timeframeMinutes;
        this.lookbackCandles = lookbackCandles;
        this.cacheTtlMs = cacheTtlMs;
    }

    /**
     * Get current trend for a ticker.
     * Returns cached result if available and fresh, otherwise recalculates.
     */
    public Trend getTrend(TickerInfo.Key key) {
        String ticker = key.getTicker();
        TrendResult cached = trendCache.get(ticker);
        if (cached != null && !cached.isExpired(cacheTtlMs)) {
            return cached.trend;
        }
        
        // Recalculate trend
        Trend trend = calculateTrend(key);
        trendCache.put(ticker, new TrendResult(trend, System.currentTimeMillis()));
        return trend;
    }

    /**
     * Check if entry direction is aligned with higher timeframe trend.
     */
    public boolean isAlignedWithTrend(TickerInfo.Key key, String direction) {
        Trend trend = getTrend(key);
        if (trend == Trend.SIDEWAYS) {
            return true; // Sideways allows both directions
        }
        if ("LONG".equals(direction)) {
            return trend == Trend.UP;
        }
        if ("SHORT".equals(direction)) {
            return trend == Trend.DOWN;
        }
        return false;
    }

    /**
     * Check if entry direction is counter-trend.
     */
    public boolean isCounterTrend(TickerInfo.Key key, String direction) {
        Trend trend = getTrend(key);
        if (trend == Trend.SIDEWAYS) {
            return false;
        }
        if ("LONG".equals(direction)) {
            return trend == Trend.DOWN;
        }
        if ("SHORT".equals(direction)) {
            return trend == Trend.UP;
        }
        return false;
    }

    /**
     * Calculate trend from candle data.
     */
    private Trend calculateTrend(TickerInfo.Key key) {
        try {
            List<Candle> candles = loadCandles(key);
            if (candles == null || candles.size() < 3) {
                return Trend.SIDEWAYS; // Not enough data
            }
            
            // Analyze price structure
            double firstClose = candles.get(0).close;
            double lastClose = candles.get(candles.size() - 1).close;
            
            // Find swing highs and lows
            List<Double> swingHighs = new ArrayList<>();
            List<Double> swingLows = new ArrayList<>();
            
            for (int i = 1; i < candles.size() - 1; i++) {
                double prevHigh = candles.get(i - 1).high;
                double currHigh = candles.get(i).high;
                double nextHigh = candles.get(i + 1).high;
                
                double prevLow = candles.get(i - 1).low;
                double currLow = candles.get(i).low;
                double nextLow = candles.get(i + 1).low;
                
                // Swing high: higher than neighbors
                if (currHigh > prevHigh && currHigh > nextHigh) {
                    swingHighs.add(currHigh);
                }
                
                // Swing low: lower than neighbors
                if (currLow < prevLow && currLow < nextLow) {
                    swingLows.add(currLow);
                }
            }
            
            // Determine trend from swing structure
            boolean higherHighs = false;
            boolean lowerLows = false;
            
            if (swingHighs.size() >= 2) {
                double lastHigh = swingHighs.get(swingHighs.size() - 1);
                double prevHigh = swingHighs.get(swingHighs.size() - 2);
                higherHighs = lastHigh > prevHigh;
            }
            
            if (swingLows.size() >= 2) {
                double lastLow = swingLows.get(swingLows.size() - 1);
                double prevLow = swingLows.get(swingLows.size() - 2);
                lowerLows = lastLow < prevLow;
            }
            
            // Also consider overall price change
            double priceChange = (lastClose - firstClose) / firstClose;
            
            // Trend determination
            if (higherHighs && priceChange > 0.002) { // 0.2% threshold
                return Trend.UP;
            }
            if (lowerLows && priceChange < -0.002) {
                return Trend.DOWN;
            }
            
            // Check for strong directional move even without clear swing structure
            if (priceChange > 0.005) { // 0.5% threshold
                return Trend.UP;
            }
            if (priceChange < -0.005) {
                return Trend.DOWN;
            }
            
            return Trend.SIDEWAYS;
            
        } catch (Exception e) {
            LoggingUtils.log("TrendAnalyzer error for " + key.getTicker() + ": " + e.getMessage());
            return Trend.SIDEWAYS;
        }
    }

    /**
     * Load candles from API.
     */
    private List<Candle> loadCandles(TickerInfo.Key key) {
        try {
            Instant now = Instant.now();
            Instant from = now.minus(lookbackCandles * timeframeMinutes, ChronoUnit.MINUTES);
            
            String interval = timeframeMinutes <= 5 ? "5_MIN" : "15_MIN";
            
            String figi = tradingService.figiByName(key);
            return tradingService.getCandles(figi, from, now, interval);
            
        } catch (Exception e) {
            LoggingUtils.log("Failed to load candles for " + key.getTicker() + ": " + e.getMessage());
            return null;
        }
    }

    /**
     * Log current trend status.
     */
    public void logStatus() {
        LoggingUtils.log("TrendAnalyzer status (" + timeframeMinutes + "-min):");
        for (Map.Entry<String, TrendResult> entry : trendCache.entrySet()) {
            LoggingUtils.log("  " + entry.getKey() + ": " + entry.getValue().trend);
        }
    }

    /**
     * Clear trend cache.
     */
    public void reset() {
        trendCache.clear();
    }

    /**
     * Cached trend result with timestamp.
     */
    private static final class TrendResult {
        final Trend trend;
        final long timestampMs;

        TrendResult(Trend trend, long timestampMs) {
            this.trend = trend;
            this.timestampMs = timestampMs;
        }

        boolean isExpired(long ttlMs) {
            return System.currentTimeMillis() - timestampMs > ttlMs;
        }
    }
}
