package com.github.shk0da.goldendragon.strategy.orderbook;

import java.util.ArrayDeque;
import java.util.Deque;

/**
 * Tracks spread and price volatility for position sizing and dynamic TP/SL.
 *
 * <p>Uses a rolling window of spread samples to compute:
 * <ul>
 *   <li>Average spread (baseline liquidity measure)</li>
 *   <p>Spread volatility (standard deviation — proxy for ATR)</li>
 *   <li>Volatility ratio (current vs target — for position sizing)</li>
 * </ul>
 */
public final class VolatilityTracker {

    private static final int DEFAULT_WINDOW_SIZE = 60;

    private final int windowSize;
    private final Deque<Double> spreadSamples = new ArrayDeque<>();
    private final Deque<Double> midPriceSamples = new ArrayDeque<>();

    private double spreadSum = 0.0;
    private double spreadSqSum = 0.0;
    private double midPriceSum = 0.0;
    private double midPriceSqSum = 0.0;

    public VolatilityTracker() {
        this(DEFAULT_WINDOW_SIZE);
    }

    public VolatilityTracker(int windowSize) {
        this.windowSize = Math.max(10, windowSize);
    }

    /**
     * Record a new spread and mid-price observation.
     *
     * @param spread current spread in price units
     * @param midPrice current mid-price
     */
    public synchronized void update(double spread, double midPrice) {
        if (spread <= 0.0 || midPrice <= 0.0) {
            return;
        }

        // Add new sample
        spreadSamples.addLast(spread);
        spreadSum += spread;
        spreadSqSum += spread * spread;

        midPriceSamples.addLast(midPrice);
        midPriceSum += midPrice;
        midPriceSqSum += midPrice * midPrice;

        // Remove oldest if over window
        if (spreadSamples.size() > windowSize) {
            double oldSpread = spreadSamples.removeFirst();
            spreadSum -= oldSpread;
            spreadSqSum -= oldSpread * oldSpread;

            double oldMid = midPriceSamples.removeFirst();
            midPriceSum -= oldMid;
            midPriceSqSum -= oldMid * oldMid;
        }
    }

    /** Average spread over the window. */
    public synchronized double getAverageSpread() {
        if (spreadSamples.isEmpty()) {
            return 0.0;
        }
        return spreadSum / spreadSamples.size();
    }

    /**
     * Spread volatility (standard deviation) — proxy for ATR.
     *
     * <p>Higher volatility means less predictable spreads, requiring smaller positions.
     */
    public synchronized double getSpreadVolatility() {
        int n = spreadSamples.size();
        if (n < 2) {
            return 0.0;
        }
        double mean = spreadSum / n;
        double variance = (spreadSqSum / n) - (mean * mean);
        return Math.sqrt(Math.max(0.0, variance));
    }

    /**
     * Mid-price volatility — another ATR proxy based on price movements.
     */
    public synchronized double getMidPriceVolatility() {
        int n = midPriceSamples.size();
        if (n < 2) {
            return 0.0;
        }
        double mean = midPriceSum / n;
        double variance = (midPriceSqSum / n) - (mean * mean);
        return Math.sqrt(Math.max(0.0, variance));
    }

    /**
     * Combined volatility estimate (spread vol + price vol).
     *
     * <p>This is the primary metric for position sizing.
     */
    public double getCombinedVolatility() {
        return getSpreadVolatility() + getMidPriceVolatility();
    }

    /**
     * Average spread in basis points.
     *
     * @param midPrice current mid-price for conversion
     */
    public double getAverageSpreadBps(double midPrice) {
        if (midPrice <= 0.0) {
            return 0.0;
        }
        return getAverageSpread() / midPrice * 10_000.0;
    }

    /**
     * Volatility ratio: current volatility vs target.
     *
     * <p>Used for position sizing in scalping:
     * <ul>
     *   <p>> 1.0: high volatility → more profit potential → can increase position</li>
     *   <p>< 1.0: low volatility → less movement → should decrease position</li>
     * </ul>
     *
     * @param targetSpreadBps target spread in basis points
     */
    public double getVolatilityRatio(double targetSpreadBps, double currentMidPrice) {
        double currentSpreadBps = getAverageSpreadBps(currentMidPrice);
        if (currentSpreadBps <= 0.0 || targetSpreadBps <= 0.0) {
            return 1.0;
        }
        // For scalping: higher volatility = more profit potential = larger position
        return currentSpreadBps / targetSpreadBps;
    }

    /** Number of samples in the window. */
    public synchronized int getSampleCount() {
        return spreadSamples.size();
    }

    /** Whether we have enough samples for reliable statistics. */
    public boolean isReady() {
        return getSampleCount() >= Math.min(20, windowSize / 2);
    }

    /** Reset all tracked data. */
    public synchronized void reset() {
        spreadSamples.clear();
        midPriceSamples.clear();
        spreadSum = 0.0;
        spreadSqSum = 0.0;
        midPriceSum = 0.0;
        midPriceSqSum = 0.0;
    }
}
