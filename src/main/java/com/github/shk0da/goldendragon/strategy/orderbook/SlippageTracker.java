package com.github.shk0da.goldendragon.strategy.orderbook;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Deque;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Tracks execution quality by measuring slippage per trade.
 *
 * <p>Slippage is the adverse difference between the expected price and the actual fill price,
 * measured in ticks for normalization across instruments. Positive values indicate worse execution
 * than expected.
 *
 * <p>Maintains rolling statistics per direction (LONG/SHORT):
 * <ul>
 *   <li>Average slippage</li>
 *   <li>Maximum slippage</li>
 *   <li>Slippage distribution (percentiles)</li>
 * </ul>
 *
 * <p>Thread-safe via synchronized access to per-direction deques.
 */
public final class SlippageTracker {

    private static final int DEFAULT_WINDOW_SIZE = 200;
    private static final double DEFAULT_WARNING_THRESHOLD_TICKS = 5.0;

    private final int windowSize;
    private final double warningThresholdTicks;
    private final double tickSize;

    private final Map<String, Deque<SlippageRecord>> samplesByDirection = new ConcurrentHashMap<>();

    public SlippageTracker(double tickSize) {
        this(tickSize, DEFAULT_WINDOW_SIZE, DEFAULT_WARNING_THRESHOLD_TICKS);
    }

    public SlippageTracker(double tickSize, int windowSize, double warningThresholdTicks) {
        if (tickSize <= 0.0) {
            throw new IllegalArgumentException("tickSize must be positive, got " + tickSize);
        }
        if (windowSize <= 0) {
            throw new IllegalArgumentException("windowSize must be positive, got " + windowSize);
        }
        this.tickSize = tickSize;
        this.windowSize = windowSize;
        this.warningThresholdTicks = warningThresholdTicks;
    }

    /**
     * Record a trade execution and compute its slippage.
     *
     * <p>For LONG direction: slippage = (actualPrice - expectedPrice) / tickSize.
     * For SHORT direction: slippage = (expectedPrice - actualPrice) / tickSize.
     * Positive values mean worse execution than expected.
     *
     * @param direction trade direction ("LONG" or "SHORT")
     * @param expectedPrice anticipated fill price
     * @param actualPrice actual fill price
     */
    public void recordTrade(String direction, double expectedPrice, double actualPrice) {
        if (direction == null || direction.isEmpty()) {
            return;
        }
        if (expectedPrice <= 0.0 || actualPrice <= 0.0) {
            return;
        }

        double slippageTicks;
        if ("LONG".equalsIgnoreCase(direction)) {
            slippageTicks = (actualPrice - expectedPrice) / tickSize;
        } else {
            slippageTicks = (expectedPrice - actualPrice) / tickSize;
        }

        String normalizedDirection = direction.toUpperCase();
        Deque<SlippageRecord> deque = samplesByDirection.computeIfAbsent(
                normalizedDirection, k -> new ArrayDeque<>());
        synchronized (deque) {
            deque.addLast(new SlippageRecord(slippageTicks, System.currentTimeMillis()));
            while (deque.size() > windowSize) {
                deque.removeFirst();
            }
        }

        if (slippageTicks > warningThresholdTicks) {
            System.out.println(
                    "WARN: high slippage detected direction=" + normalizedDirection
                            + " slippageTicks=" + String.format("%.1f", slippageTicks)
                            + " expected=" + String.format("%.4f", expectedPrice)
                            + " actual=" + String.format("%.4f", actualPrice)
                            + " threshold=" + String.format("%.1f", warningThresholdTicks));
        }
    }

    /**
     * Average slippage in ticks across all directions.
     *
     * @return average slippage in ticks, or 0.0 if no trades recorded
     */
    public double getAverageSlippage() {
        double totalSum = 0.0;
        int totalCount = 0;
        for (Deque<SlippageRecord> deque : samplesByDirection.values()) {
            synchronized (deque) {
                for (SlippageRecord record : deque) {
                    totalSum += record.slippageTicks;
                    totalCount++;
                }
            }
        }
        return totalCount == 0 ? 0.0 : totalSum / totalCount;
    }

    /**
     * Average slippage in ticks for a specific direction.
     *
     * @param direction "LONG" or "SHORT"
     * @return average slippage in ticks, or 0.0 if no trades recorded
     */
    public double getAverageSlippage(String direction) {
        Deque<SlippageRecord> deque = samplesByDirection.get(direction.toUpperCase());
        if (deque == null) {
            return 0.0;
        }
        double sum = 0.0;
        int count;
        synchronized (deque) {
            count = deque.size();
            if (count == 0) {
                return 0.0;
            }
            for (SlippageRecord record : deque) {
                sum += record.slippageTicks;
            }
        }
        return sum / count;
    }

    /**
     * Maximum slippage in ticks across all directions.
     *
     * @return max slippage in ticks, or 0.0 if no trades recorded
     */
    public double getMaxSlippage() {
        double max = 0.0;
        for (Deque<SlippageRecord> deque : samplesByDirection.values()) {
            synchronized (deque) {
                for (SlippageRecord record : deque) {
                    if (record.slippageTicks > max) {
                        max = record.slippageTicks;
                    }
                }
            }
        }
        return max;
    }

    /**
     * Maximum slippage in ticks for a specific direction.
     *
     * @param direction "LONG" or "SHORT"
     * @return max slippage in ticks, or 0.0 if no trades recorded
     */
    public double getMaxSlippage(String direction) {
        Deque<SlippageRecord> deque = samplesByDirection.get(direction.toUpperCase());
        if (deque == null) {
            return 0.0;
        }
        double max = 0.0;
        synchronized (deque) {
            for (SlippageRecord record : deque) {
                if (record.slippageTicks > max) {
                    max = record.slippageTicks;
                }
            }
        }
        return max;
    }

    /**
     * Slippage percentile across all directions.
     *
     * <p>Returns the slippage value at the given percentile of the distribution.
     * For example, percentile 95 returns the value below which 95% of observations fall.
     *
     * @param percentile percentile to compute (0 to 100)
     * @return slippage in ticks at the given percentile, or 0.0 if no data
     */
    public double getSlippagePercentile(double percentile) {
        if (percentile < 0.0 || percentile > 100.0) {
            throw new IllegalArgumentException("percentile must be between 0 and 100, got " + percentile);
        }
        List<Double> allValues = collectAllValues();
        if (allValues.isEmpty()) {
            return 0.0;
        }
        return computePercentile(allValues, percentile);
    }

    /**
     * Slippage percentile for a specific direction.
     *
     * @param direction "LONG" or "SHORT"
     * @param percentile percentile to compute (0 to 100)
     * @return slippage in ticks at the given percentile, or 0.0 if no data
     */
    public double getSlippagePercentile(String direction, double percentile) {
        if (percentile < 0.0 || percentile > 100.0) {
            throw new IllegalArgumentException("percentile must be between 0 and 100, got " + percentile);
        }
        Deque<SlippageRecord> deque = samplesByDirection.get(direction.toUpperCase());
        if (deque == null) {
            return 0.0;
        }
        List<Double> values;
        synchronized (deque) {
            if (deque.isEmpty()) {
                return 0.0;
            }
            values = new ArrayList<>(deque.size());
            for (SlippageRecord record : deque) {
                values.add(record.slippageTicks);
            }
        }
        Collections.sort(values);
        return computePercentile(values, percentile);
    }

    /**
     * Total number of slippage samples across all directions.
     *
     * @return sample count
     */
    public int getSampleCount() {
        int total = 0;
        for (Deque<SlippageRecord> deque : samplesByDirection.values()) {
            synchronized (deque) {
                total += deque.size();
            }
        }
        return total;
    }

    /**
     * Number of slippage samples for a specific direction.
     *
     * @param direction "LONG" or "SHORT"
     * @return sample count
     */
    public int getSampleCount(String direction) {
        Deque<SlippageRecord> deque = samplesByDirection.get(direction.toUpperCase());
        if (deque == null) {
            return 0;
        }
        synchronized (deque) {
            return deque.size();
        }
    }

    /**
     * Whether enough samples have been collected for reliable statistics.
     *
     * @return true if at least 20 samples exist
     */
    public boolean isReady() {
        return getSampleCount() >= Math.min(20, windowSize / 2);
    }

    /** Reset all tracked data. */
    public synchronized void reset() {
        samplesByDirection.clear();
    }

    private List<Double> collectAllValues() {
        List<Double> values = new ArrayList<>();
        for (Deque<SlippageRecord> deque : samplesByDirection.values()) {
            synchronized (deque) {
                for (SlippageRecord record : deque) {
                    values.add(record.slippageTicks);
                }
            }
        }
        Collections.sort(values);
        return values;
    }

    private static double computePercentile(List<Double> sortedValues, double percentile) {
        if (sortedValues.size() == 1) {
            return sortedValues.get(0);
        }
        double rank = (percentile / 100.0) * (sortedValues.size() - 1);
        int lower = (int) Math.floor(rank);
        int upper = (int) Math.ceil(rank);
        if (lower == upper) {
            return sortedValues.get(lower);
        }
        double fraction = rank - lower;
        return sortedValues.get(lower) + fraction * (sortedValues.get(upper) - sortedValues.get(lower));
    }

    /** Single slippage observation record. */
    private static final class SlippageRecord {

        final double slippageTicks;
        final long timestampMs;

        SlippageRecord(double slippageTicks, long timestampMs) {
            this.slippageTicks = slippageTicks;
            this.timestampMs = timestampMs;
        }
    }
}
