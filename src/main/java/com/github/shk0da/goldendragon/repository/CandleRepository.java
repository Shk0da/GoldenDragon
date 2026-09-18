package com.github.shk0da.goldendragon.repository;

import com.github.shk0da.goldendragon.model.Candle;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * In-memory cache for candle data.
 * Stores candles by ticker and interval to avoid repeated API calls.
 * Thread-safe implementation using ConcurrentHashMap.
 */
public class CandleRepository {

    private static final CandleRepository INSTANCE = new CandleRepository();

    // Key: ticker, Value: Map<interval, List<candles>>
    private final Map<String, Map<String, List<Candle>>> candleCache = new ConcurrentHashMap<>();

    private CandleRepository() {
    }

    public static CandleRepository getInstance() {
        return INSTANCE;
    }

    /**
     * Store candles for a ticker and interval.
     * Merges new candles with existing ones, deduplicates by time (last one wins).
     * Preserves chronological order.
     *
     * @param ticker ticker symbol
     * @param interval candle interval (HOUR, 5_MIN, etc.)
     * @param candles list of candles to store
     */
    public void putCandles(String ticker, String interval, List<Candle> candles) {
        if (candles == null || candles.isEmpty()) {
            return;
        }

        // Get existing candles
        List<Candle> existing = candleCache
            .computeIfAbsent(ticker, k -> new ConcurrentHashMap<>())
            .get(interval);

        // Merge with existing (deduplicate by time, last wins)
        Map<String, Candle> merged = new LinkedHashMap<>();
        if (existing != null) {
            for (Candle c : existing) {
                merged.put(c.time, c);
            }
        }
        for (Candle c : candles) {
            merged.put(c.time, c);  // New candles overwrite duplicates
        }

        candleCache.get(ticker).put(interval, new ArrayList<>(merged.values()));
    }

    /**
     * Get candles for a ticker and interval.
     * Returns empty list if no data available.
     *
     * @param ticker ticker symbol
     * @param interval candle interval
     * @return list of candles, or empty list if not found
     */
    public List<Candle> getCandles(String ticker, String interval) {
        Map<String, List<Candle>> tickerCandles = candleCache.get(ticker);
        if (tickerCandles == null) {
            return Collections.emptyList();
        }
        List<Candle> candles = tickerCandles.get(interval);
        return candles != null ? candles : Collections.emptyList();
    }

    /**
     * Get last N candles for a ticker and interval.
     *
     * @param ticker ticker symbol
     * @param interval candle interval
     * @param count number of candles to return
     * @return list of last N candles, or empty list if not found
     */
    public List<Candle> getLastCandles(String ticker, String interval, int count) {
        List<Candle> all = getCandles(ticker, interval);
        if (all.isEmpty()) {
            return Collections.emptyList();
        }
        int size = all.size();
        if (count >= size) {
            return new ArrayList<>(all);
        }
        return all.subList(size - count, size);
    }

    /**
     * Get cache statistics.
     *
     * @return map with ticker counts
     */
    public Map<String, Integer> getStats() {
        Map<String, Integer> stats = new HashMap<>();
        candleCache.forEach((ticker, intervals) ->
                stats.put(ticker, intervals.values().stream()
                        .mapToInt(List::size)
                        .sum()));
        return stats;
    }
}
