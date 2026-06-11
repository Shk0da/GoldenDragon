package com.github.shk0da.GoldenDragon.money;

import java.util.concurrent.atomic.AtomicInteger;

/**
 * Adaptive capital manager implementing anti-martingale position sizing.
 * Reduces risk after losses, restores after wins.
 * Never uses martingale (no doubling down on losses).
 */
public class AdaptiveCapital {

    private final int lossesToReduce;
    private final int winsToRestore;
    private final double riskReductionFactor;
    private final double baseRiskPercent;

    private final AtomicInteger consecutiveWins = new AtomicInteger(0);
    private final AtomicInteger consecutiveLosses = new AtomicInteger(0);
    private volatile double currentRiskPercent;

    /**
     * Create adaptive capital manager.
     *
     * @param baseRiskPercent base risk per trade as decimal (e.g., 0.01 for 1%)
     * @param lossesToReduce consecutive losses before risk reduction (e.g., 3)
     * @param winsToRestore consecutive wins to restore base risk (e.g., 5)
     * @param riskReductionFactor risk reduction factor (e.g., 0.5 for 50% reduction)
     */
    public AdaptiveCapital(
            double baseRiskPercent,
            int lossesToReduce,
            int winsToRestore,
            double riskReductionFactor) {
        this.baseRiskPercent = baseRiskPercent;
        this.lossesToReduce = lossesToReduce;
        this.winsToRestore = winsToRestore;
        this.riskReductionFactor = riskReductionFactor;
        this.currentRiskPercent = baseRiskPercent;
    }

    /**
     * Register winning trade.
     * Restores risk after consecutive wins.
     */
    public void registerWin() {
        consecutiveWins.incrementAndGet();
        consecutiveLosses.set(0);

        // Restore base risk after consecutive wins
        if (consecutiveWins.get() >= winsToRestore) {
            currentRiskPercent = baseRiskPercent;
            consecutiveWins.set(0);
        }
    }

    /**
     * Register losing trade.
     * Reduces risk after consecutive losses.
     */
    public void registerLoss() {
        consecutiveLosses.incrementAndGet();
        consecutiveWins.set(0);

        // Reduce risk after consecutive losses
        if (consecutiveLosses.get() >= lossesToReduce) {
            currentRiskPercent = baseRiskPercent * riskReductionFactor;
        }
    }

    /**
     * Get current risk multiplier.
     *
     * @return risk multiplier (1.0 = base risk, < 1.0 = reduced risk)
     */
    public double getRiskMultiplier() {
        return currentRiskPercent / baseRiskPercent;
    }

    /**
     * Get current risk percent.
     *
     * @return current risk per trade as decimal
     */
    public double getCurrentRiskPercent() {
        return currentRiskPercent;
    }

    /**
     * Reset adaptive capital state.
     */
    public void reset() {
        consecutiveWins.set(0);
        consecutiveLosses.set(0);
        currentRiskPercent = baseRiskPercent;
    }
}
