package com.github.shk0da.GoldenDragon.strategy.turtle;

import java.util.concurrent.atomic.AtomicLong;

/**
 * Turtle drawdown manager.
 * Tracks equity drawdown and adjusts risk accordingly.
 */
public class TurtleDrawdownManager {

    private final double riskReduceThreshold;
    private final double stopThreshold;
    private final AtomicLong peakEquity = new AtomicLong(0);
    private final AtomicLong currentEquity = new AtomicLong(0);
    private volatile boolean tradingAllowed = true;

    /**
     * Create Turtle drawdown manager.
     *
     * @param riskReduceThreshold reduce risk at this drawdown (e.g., 0.05 for 5%)
     * @param stopThreshold stop trading at this drawdown (e.g., 0.10 for 10%)
     */
    public TurtleDrawdownManager(double riskReduceThreshold, double stopThreshold) {
        this.riskReduceThreshold = riskReduceThreshold;
        this.stopThreshold = stopThreshold;
    }

    /**
     * Update equity and track peak.
     *
     * @param equity current equity in cents (to avoid floating point)
     */
    public void updateEquity(long equity) {
        currentEquity.set(equity);
        peakEquity.updateAndGet(peak -> Math.max(peak, equity));

        // Check if should stop trading
        double drawdown = calculateDrawdown();
        if (drawdown >= stopThreshold) {
            tradingAllowed = false;
        }
    }

    /**
     * Update equity with double value.
     */
    public void updateEquity(double equity) {
        updateEquity((long) (equity * 100));
    }

    /**
     * Calculate current drawdown.
     *
     * @return drawdown as decimal (0.0 to 1.0)
     */
    public double calculateDrawdown() {
        long peak = peakEquity.get();
        long current = currentEquity.get();

        if (peak <= 0 || current <= 0) {
            return 0.0;
        }

        return (double) (peak - current) / peak;
    }

    /**
     * Get risk multiplier based on drawdown.
     *
     * @return risk multiplier (1.0 = full risk, < 1.0 = reduced risk)
     */
    public double getRiskMultiplier() {
        double drawdown = calculateDrawdown();

        if (drawdown >= stopThreshold) {
            return 0.0; // Stop trading
        }

        if (drawdown >= riskReduceThreshold) {
            // Linear reduction from 1.0 to 0.5 as drawdown increases
            double reduction = (drawdown - riskReduceThreshold) / (stopThreshold - riskReduceThreshold);
            return Math.max(0.25, 1.0 - (reduction * 0.75));
        }

        return 1.0; // Full risk
    }

    /**
     * Check if trading is allowed.
     */
    public boolean canTrade() {
        return tradingAllowed && getRiskMultiplier() > 0;
    }

    /**
     * Reset drawdown tracking.
     */
    public void reset() {
        peakEquity.set(0);
        currentEquity.set(0);
        tradingAllowed = true;
    }

    /**
     * Get current drawdown percentage.
     */
    public double getDrawdownPercent() {
        return calculateDrawdown() * 100;
    }

    /**
     * Get peak equity.
     */
    public long getPeakEquity() {
        return peakEquity.get();
    }

    /**
     * Get current equity.
     */
    public long getCurrentEquity() {
        return currentEquity.get();
    }
}
