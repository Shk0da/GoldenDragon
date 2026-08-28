package com.github.shk0da.goldendragon.strategy.orderbook;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.Map;
import java.util.NavigableMap;
import java.util.TreeMap;

/**
 * Tracks volume distribution by price levels and computes VWAP, POC, and Value Area.
 * Uses tick-sized buckets for the volume histogram and supports a rolling time window
 * that automatically evicts expired observations.
 */
public final class VolumeProfileTracker {

    private final double tickSize;
    private final long windowMillis;
    private final TreeMap<Long, Long> volumeByBucket = new TreeMap<>();
    private final Deque<TradeRecord> records = new ArrayDeque<>();
    private double sumPriceVolume;
    private long sumVolume;

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
     * Adds a trade observation to the volume profile.
     *
     * @param price           trade price
     * @param volume          trade volume (quantity)
     * @param timestampMillis trade timestamp in milliseconds
     */
    public void addTrade(double price, long volume, long timestampMillis) {
        if (volume <= 0) {
            return;
        }
        long bucket = toBucket(price);
        volumeByBucket.merge(bucket, volume, Long::sum);
        records.addLast(new TradeRecord(bucket, price, volume, timestampMillis));
        sumPriceVolume += price * volume;
        sumVolume += volume;
        evictExpired(timestampMillis);
    }

    /**
     * Returns the total volume at the given price level (tick-sized bucket).
     *
     * @param price price to query
     * @return volume at that price level, or 0 if no data
     */
    public long getVolumeAtPrice(double price) {
        long bucket = toBucket(price);
        return volumeByBucket.getOrDefault(bucket, 0L);
    }

    /**
     * Calculates VWAP (Volume Weighted Average Price) over the current window.
     *
     * @return VWAP, or 0 if no data
     */
    public double getVwap() {
        if (sumVolume <= 0) {
            return 0.0;
        }
        return sumPriceVolume / sumVolume;
    }

    /**
     * Returns the Point of Control — the price level with the highest traded volume.
     *
     * @return POC price, or 0 if no data
     */
    public double getPoc() {
        if (volumeByBucket.isEmpty()) {
            return 0.0;
        }
        return fromBucket(findPocBucket());
    }

    /**
     * Returns the Value Area High — upper boundary of the narrowest price range
     * around POC containing at least 70% of total volume.
     *
     * @return Value Area High price, or 0 if no data
     */
    public double getValueAreaHigh() {
        return computeValueArea()[1];
    }

    /**
     * Returns the Value Area Low — lower boundary of the narrowest price range
     * around POC containing at least 70% of total volume.
     *
     * @return Value Area Low price, or 0 if no data
     */
    public double getValueAreaLow() {
        return computeValueArea()[0];
    }

    /**
     * Returns the total volume across all price levels in the current window.
     */
    public long getTotalVolume() {
        return sumVolume;
    }

    /**
     * Returns the number of distinct price levels with non-zero volume.
     */
    public int getLevelCount() {
        return volumeByBucket.size();
    }

    /**
     * Returns a snapshot of the volume profile as a map from price to volume.
     */
    public NavigableMap<Double, Long> getVolumeProfile() {
        TreeMap<Double, Long> result = new TreeMap<>();
        for (Map.Entry<Long, Long> entry : volumeByBucket.entrySet()) {
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

    private void evictExpired(long nowMillis) {
        long cutoff = nowMillis - windowMillis;
        while (!records.isEmpty() && records.peekFirst().timestampMillis < cutoff) {
            TradeRecord expired = records.removeFirst();
            long remaining = volumeByBucket.get(expired.bucket);
            long newVolume = remaining - expired.volume;
            if (newVolume <= 0) {
                volumeByBucket.remove(expired.bucket);
            } else {
                volumeByBucket.put(expired.bucket, newVolume);
            }
            sumPriceVolume -= expired.price * expired.volume;
            sumVolume -= expired.volume;
        }
        // reset to zero when all data evicted to avoid floating-point drift
        if (sumVolume <= 0) {
            sumPriceVolume = 0;
            sumVolume = 0;
        }
    }

    private long findPocBucket() {
        long maxVol = 0;
        long pocBucket = 0;
        for (Map.Entry<Long, Long> entry : volumeByBucket.entrySet()) {
            if (entry.getValue() > maxVol) {
                maxVol = entry.getValue();
                pocBucket = entry.getKey();
            }
        }
        return pocBucket;
    }

    /**
     * Computes Value Area [low, high] — the narrowest price range around POC
     * that contains at least 70% of total volume.
     * Expands outward from POC one occupied bucket at a time,
     * always choosing the side with the larger volume contribution.
     */
    private double[] computeValueArea() {
        if (volumeByBucket.isEmpty()) {
            return new double[]{0.0, 0.0};
        }
        long totalVolume = sumVolume;
        if (totalVolume <= 0) {
            return new double[]{0.0, 0.0};
        }

        long targetVolume = (long) Math.ceil(totalVolume * 0.7);
        long pocBucket = findPocBucket();

        long accumulatedVolume = volumeByBucket.getOrDefault(pocBucket, 0L);
        long lowBucket = pocBucket;
        long highBucket = pocBucket;

        while (accumulatedVolume < targetVolume) {
            Long nextLow = volumeByBucket.lowerKey(lowBucket);
            Long nextHigh = volumeByBucket.higherKey(highBucket);

            if (nextLow == null && nextHigh == null) {
                break;
            }

            long volLow = (nextLow != null) ? volumeByBucket.get(nextLow) : 0;
            long volHigh = (nextHigh != null) ? volumeByBucket.get(nextHigh) : 0;

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
