package com.github.shk0da.goldendragon.strategy.orderbook;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.Map;
import java.util.NavigableMap;
import java.util.TreeMap;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Tracks volume distribution by price levels and computes VWAP, POC, and Value Area.
 * Uses tick-sized buckets for the volume histogram and supports a rolling time window
 * that automatically evicts expired observations.
 * Maintains separate profiles per ticker.
 */
public final class VolumeProfileTracker {

    private final double tickSize;
    private final long windowMillis;
    private final Map<String, TickerProfile> profilesByTicker = new ConcurrentHashMap<>();

    /**
     * Internal class holding volume profile data for a single ticker.
     */
    private final class TickerProfile {
        final TreeMap<Long, Long> volumeByBucket = new TreeMap<>();
        final Deque<TradeRecord> records = new ArrayDeque<>();
        double sumPriceVolume;
        long sumVolume;
    }

    // single trade observation stored for rolling-window eviction
    private static final class TradeRecord {

        final long bucket;
        final double price;
        final long volume;
        final long timestampMillis;

        TradeRecord(long bucket, double price, long volume, long timestampMillis) {
            this.bucket = bucket;
            this.price = price;
            this.volume = volume;
            this.timestampMillis = timestampMillis;
        }
    }

    /**
     * Creates a tracker with the given tick size and rolling window duration.
     *
     * @param tickSize     minimum price step (e.g. 0.01)
     * @param windowMillis rolling window duration in milliseconds
     */
    public VolumeProfileTracker(double tickSize, long windowMillis) {
        if (tickSize <= 0) {
            throw new IllegalArgumentException("tickSize must be positive");
        }
        if (windowMillis <= 0) {
            throw new IllegalArgumentException("windowMillis must be positive");
        }
        this.tickSize = tickSize;
        this.windowMillis = windowMillis;
    }

    /**
     * Adds a trade observation to the volume profile for a specific ticker.
     *
     * @param ticker          ticker symbol
     * @param price           trade price
     * @param volume          trade volume (quantity)
     * @param timestampMillis trade timestamp in milliseconds
     */
    public void addTrade(String ticker, double price, long volume, long timestampMillis) {
        if (volume <= 0) {
            return;
        }
        TickerProfile profile = profilesByTicker.computeIfAbsent(ticker, k -> new TickerProfile());
        long bucket = toBucket(price);
        profile.volumeByBucket.merge(bucket, volume, Long::sum);
        profile.records.addLast(new TradeRecord(bucket, price, volume, timestampMillis));
        profile.sumPriceVolume += price * volume;
        profile.sumVolume += volume;
        evictExpired(profile, timestampMillis);
    }

    /**
     * Returns the total volume at the given price level (tick-sized bucket) for a specific ticker.
     *
     * @param ticker ticker symbol
     * @param price price to query
     * @return volume at that price level, or 0 if no data
     */
    public long getVolumeAtPrice(String ticker, double price) {
        TickerProfile profile = profilesByTicker.get(ticker);
        if (profile == null) {
            return 0L;
        }
        long bucket = toBucket(price);
        return profile.volumeByBucket.getOrDefault(bucket, 0L);
    }

    /**
     * Calculates VWAP (Volume Weighted Average Price) over the current window for a specific ticker.
     *
     * @param ticker ticker symbol
     * @return VWAP, or 0 if no data
     */
    public double getVwap(String ticker) {
        TickerProfile profile = profilesByTicker.get(ticker);
        if (profile == null || profile.sumVolume <= 0) {
            return 0.0;
        }
        return profile.sumPriceVolume / profile.sumVolume;
    }

    /**
     * Returns the Point of Control — the price level with the highest traded volume for a specific ticker.
     *
     * @param ticker ticker symbol
     * @return POC price, or 0 if no data
     */
    public double getPoc(String ticker) {
        TickerProfile profile = profilesByTicker.get(ticker);
        if (profile == null || profile.volumeByBucket.isEmpty()) {
            return 0.0;
        }
        return fromBucket(findPocBucket(profile));
    }

    /**
     * Returns the Value Area High — upper boundary of the narrowest price range
     * around POC containing at least 70% of total volume for a specific ticker.
     *
     * @param ticker ticker symbol
     * @return Value Area High price, or 0 if no data
     */
    public double getValueAreaHigh(String ticker) {
        return computeValueArea(ticker)[1];
    }

    /**
     * Returns the Value Area Low — lower boundary of the narrowest price range
     * around POC containing at least 70% of total volume for a specific ticker.
     *
     * @param ticker ticker symbol
     * @return Value Area Low price, or 0 if no data
     */
    public double getValueAreaLow(String ticker) {
        return computeValueArea(ticker)[0];
    }

    /**
     * Returns the total volume across all price levels in the current window for a specific ticker.
     */
    public long getTotalVolume(String ticker) {
        TickerProfile profile = profilesByTicker.get(ticker);
        return profile != null ? profile.sumVolume : 0L;
    }

    /**
     * Returns the number of distinct price levels with non-zero volume for a specific ticker.
     */
    public int getLevelCount(String ticker) {
        TickerProfile profile = profilesByTicker.get(ticker);
        return profile != null ? profile.volumeByBucket.size() : 0;
    }

    /**
     * Returns a snapshot of the volume profile as a map from price to volume for a specific ticker.
     */
    public NavigableMap<Double, Long> getVolumeProfile(String ticker) {
        TreeMap<Double, Long> result = new TreeMap<>();
        TickerProfile profile = profilesByTicker.get(ticker);
        if (profile == null) {
            return result;
        }
        for (Map.Entry<Long, Long> entry : profile.volumeByBucket.entrySet()) {
            result.put(fromBucket(entry.getKey()), entry.getValue());
        }
        return result;
    }

    // --- private helpers ---

    private long toBucket(double price) {
        return Math.round(price / tickSize);
    }

    private double fromBucket(long bucket) {
        return bucket * tickSize;
    }

    private void evictExpired(TickerProfile profile, long nowMillis) {
        long cutoff = nowMillis - windowMillis;
        while (!profile.records.isEmpty() && profile.records.peekFirst().timestampMillis < cutoff) {
            TradeRecord expired = profile.records.removeFirst();
            long remaining = profile.volumeByBucket.get(expired.bucket);
            long newVolume = remaining - expired.volume;
            if (newVolume <= 0) {
                profile.volumeByBucket.remove(expired.bucket);
            } else {
                profile.volumeByBucket.put(expired.bucket, newVolume);
            }
            profile.sumPriceVolume -= expired.price * expired.volume;
            profile.sumVolume -= expired.volume;
        }
        // reset to zero when all data evicted to avoid floating-point drift
        if (profile.sumVolume <= 0) {
            profile.sumPriceVolume = 0;
            profile.sumVolume = 0;
        }
    }

    private long findPocBucket(TickerProfile profile) {
        long maxVol = 0;
        long pocBucket = 0;
        for (Map.Entry<Long, Long> entry : profile.volumeByBucket.entrySet()) {
            if (entry.getValue() > maxVol) {
                maxVol = entry.getValue();
                pocBucket = entry.getKey();
            }
        }
        return pocBucket;
    }

    /**
     * Computes Value Area [low, high] — the narrowest price range around POC
     * that contains at least 70% of total volume for a specific ticker.
     * Expands outward from POC one occupied bucket at a time,
     * always choosing the side with the larger volume contribution.
     */
    private double[] computeValueArea(String ticker) {
        TickerProfile profile = profilesByTicker.get(ticker);
        if (profile == null || profile.volumeByBucket.isEmpty()) {
            return new double[]{0.0, 0.0};
        }
        long totalVolume = profile.sumVolume;
        if (totalVolume <= 0) {
            return new double[]{0.0, 0.0};
        }

        long targetVolume = (long) Math.ceil(totalVolume * 0.7);
        long pocBucket = findPocBucket(profile);

        long accumulatedVolume = profile.volumeByBucket.getOrDefault(pocBucket, 0L);
        long lowBucket = pocBucket;
        long highBucket = pocBucket;

        while (accumulatedVolume < targetVolume) {
            Long nextLow = profile.volumeByBucket.lowerKey(lowBucket);
            Long nextHigh = profile.volumeByBucket.higherKey(highBucket);

            if (nextLow == null && nextHigh == null) {
                break;
            }

            long volLow = (nextLow != null) ? profile.volumeByBucket.get(nextLow) : 0;
            long volHigh = (nextHigh != null) ? profile.volumeByBucket.get(nextHigh) : 0;

            if (volLow >= volHigh && nextLow != null) {
                lowBucket = nextLow;
                accumulatedVolume += volLow;
            } else if (nextHigh != null) {
                highBucket = nextHigh;
                accumulatedVolume += volHigh;
            } else {
                // nextLow is the only remaining option
                lowBucket = nextLow;
                accumulatedVolume += volLow;
            }
        }

        return new double[]{fromBucket(lowBucket), fromBucket(highBucket)};
    }
}
