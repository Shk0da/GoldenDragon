package com.github.shk0da.goldendragon.strategy.orderbook;

import com.github.shk0da.goldendragon.model.MarketTradeTick;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Calculates Volume-Synchronized Probability of Informed Trading (VPIN).
 *
 * <p>VPIN measures order flow toxicity by classifying trades as buyer- or seller-initiated,
 * accumulating volume into fixed-size buckets, and computing the average order flow imbalance
 * across recent buckets. Higher VPIN values indicate more informed (toxic) trading flow,
 * which often precedes large price moves.
 *
 * <p>Algorithm:
 * <ol>
 *   <li>Classify each trade as buy-initiated or sell-initiated using trade direction</li>
 *   <li>Accumulate volume into fixed-size volume buckets</li>
 *   <li>For each completed bucket, compute imbalance: |buy_volume - sell_volume| / bucket_size</li>
 *   <li>Return VPIN as the rolling average of imbalance over the last N buckets</li>
 * </ol>
 *
 * <p>Reference: Easley, Lopez de Prado, O'Hara (2012) "Flow Toxicity and Liquidity in a
 * High-Frequency World"
 */
public final class VpinCalculator {

    private static final int DEFAULT_BUCKET_COUNT = 20;

    private final int bucketSize;
    private final int bucketHistorySize;
    private final Map<String, TickerVpinState> statesByTicker = new ConcurrentHashMap<>();

    /**
     * Creates a VPIN calculator with the specified bucket configuration.
     *
     * @param bucketSize volume threshold to close a single bucket (must be positive)
     * @param bucketHistorySize number of completed buckets to keep for rolling average (must be positive)
     */
    public VpinCalculator(int bucketSize, int bucketHistorySize) {
        if (bucketSize <= 0) {
            throw new IllegalArgumentException("bucketSize must be positive, got: " + bucketSize);
        }
        if (bucketHistorySize <= 0) {
            throw new IllegalArgumentException("bucketHistorySize must be positive, got: " + bucketHistorySize);
        }
        this.bucketSize = bucketSize;
        this.bucketHistorySize = bucketHistorySize;
    }

    /**
     * Creates a VPIN calculator with default history size.
     *
     * @param bucketSize volume threshold to close a single bucket (must be positive)
     */
    public VpinCalculator(int bucketSize) {
        this(bucketSize, DEFAULT_BUCKET_COUNT);
    }

    /**
     * State for a single ticker's VPIN tracking.
     */
    private static final class TickerVpinState {

        final Deque<Double> completedBucketImbalances = new ArrayDeque<>();
        long currentBucketBuyVolume;
        long currentBucketSellVolume;
        long currentBucketFilledVolume;

        TickerVpinState() {
            this.currentBucketBuyVolume = 0;
            this.currentBucketSellVolume = 0;
            this.currentBucketFilledVolume = 0;
        }
    }

    /**
     * Processes a trade tick and updates VPIN state for the ticker.
     *
     * <p>A single trade may partially fill a bucket or close one or more buckets
     * if the trade volume exceeds the remaining bucket capacity.
     *
     * @param ticker ticker symbol
     * @param trade trade tick with direction and quantity
     */
    public void onTrade(String ticker, MarketTradeTick trade) {
        TickerVpinState state = statesByTicker.computeIfAbsent(ticker, k -> new TickerVpinState());

        long volume = trade.getQuantity();
        boolean isBuy = isBuyDirection(trade.getDirection());

        addVolumeToBuckets(state, volume, isBuy);
    }

    /**
     * Adds volume to the current bucket, closing and recording buckets as they fill.
     *
     * <p>If the incoming volume exceeds the remaining capacity of the current bucket,
     * the bucket is closed with its imbalance recorded, and the remaining volume starts
     * filling the next bucket. This repeats until all volume is consumed.
     */
    private void addVolumeToBuckets(TickerVpinState state, long volume, boolean isBuy) {
        long remaining = volume;

        while (remaining > 0) {
            long spaceInBucket = bucketSize - state.currentBucketFilledVolume;
            long fillAmount = Math.min(remaining, spaceInBucket);

            if (isBuy) {
                state.currentBucketBuyVolume += fillAmount;
            } else {
                state.currentBucketSellVolume += fillAmount;
            }
            state.currentBucketFilledVolume += fillAmount;
            remaining -= fillAmount;

            if (state.currentBucketFilledVolume >= bucketSize) {
                closeCurrentBucket(state);
            }
        }
    }

    /**
     * Closes the current bucket, records its imbalance, and resets for the next bucket.
     */
    private void closeCurrentBucket(TickerVpinState state) {
        long totalFilled = state.currentBucketFilledVolume;
        if (totalFilled > 0) {
            double imbalance = Math.abs(
                    (double) state.currentBucketBuyVolume - state.currentBucketSellVolume
            ) / totalFilled;
            state.completedBucketImbalances.addLast(imbalance);
        }

        // Trim history to configured window
        while (state.completedBucketImbalances.size() > bucketHistorySize) {
            state.completedBucketImbalances.removeFirst();
        }

        // Reset current bucket
        state.currentBucketBuyVolume = 0;
        state.currentBucketSellVolume = 0;
        state.currentBucketFilledVolume = 0;
    }

    /**
     * Returns the current VPIN value for a ticker.
     *
     * <p>VPIN is the simple average of order flow imbalance across all completed buckets
     * in the rolling window. The value is in range [0.0, 1.0] where:
     * <ul>
     *   <li>0.0 = perfectly balanced flow (no informed trading)</li>
     *   <li>1.0 = entirely one-sided flow (maximum toxicity)</li>
     * </ul>
     *
     * @param ticker ticker symbol
     * @return VPIN value in [0.0, 1.0], or 0.0 if no buckets have been completed
     */
    public double getVpin(String ticker) {
        TickerVpinState state = statesByTicker.get(ticker);
        if (state == null || state.completedBucketImbalances.isEmpty()) {
            return 0.0;
        }

        double sum = 0.0;
        for (double imbalance : state.completedBucketImbalances) {
            sum += imbalance;
        }
        double vpin = sum / state.completedBucketImbalances.size();

        // Clamp to [0.0, 1.0] for safety against floating-point drift
        return Math.max(0.0, Math.min(1.0, vpin));
    }

    /**
     * Returns the number of completed buckets available for a ticker.
     *
     * @param ticker ticker symbol
     * @return number of completed buckets in the rolling window
     */
    public int getCompletedBucketCount(String ticker) {
        TickerVpinState state = statesByTicker.get(ticker);
        if (state == null) {
            return 0;
        }
        return state.completedBucketImbalances.size();
    }

    /**
     * Resets VPIN tracking for a ticker.
     *
     * @param ticker ticker symbol
     */
    public void reset(String ticker) {
        statesByTicker.remove(ticker);
    }

    private static boolean isBuyDirection(String direction) {
        if (direction == null) {
            return false;
        }
        // Tinkoff API returns exact values: "BUY" or "SELL" for aggressor side
        // "BID" check kept for legacy compatibility
        String upper = direction.toUpperCase();
        return "BUY".equals(upper) || "BID".equals(upper);
    }
}
