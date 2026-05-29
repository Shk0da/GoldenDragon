package com.github.shk0da.GoldenDragon.strategy.gerchik;

import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

/**
 * Gerchik risk manager.
 * Controls daily trade limits and loss limits.
 */
public class GerchikRiskManager {

    private final int maxTradesPerDay;
    private final double maxDailyLoss;
    private final int maxConsecutiveLosses;

    private final AtomicInteger tradesToday = new AtomicInteger(0);
    private final AtomicReference<Double> dailyPnL = new AtomicReference<>(0.0);
    private final AtomicInteger consecutiveLosses = new AtomicInteger(0);

    /**
     * Create Gerchik risk manager.
     *
     * @param maxTradesPerDay maximum trades per day (e.g., 5)
     * @param maxDailyLoss maximum daily loss in currency (e.g., 10000)
     * @param maxConsecutiveLosses max consecutive losses (e.g., 3)
     */
    public GerchikRiskManager(
            int maxTradesPerDay,
            double maxDailyLoss,
            int maxConsecutiveLosses) {
        this.maxTradesPerDay = maxTradesPerDay;
        this.maxDailyLoss = maxDailyLoss;
        this.maxConsecutiveLosses = maxConsecutiveLosses;
    }

    /**
     * Check if can trade.
     *
     * @param ticker ticker symbol
     * @return true if can trade
     */
    public boolean canTrade(String ticker) {
        // Check daily trade limit
        if (tradesToday.get() >= maxTradesPerDay) {
            return false;
        }

        // Check daily loss limit
        if (Math.abs(dailyPnL.get()) >= maxDailyLoss && dailyPnL.get() < 0) {
            return false;
        }

        // Check consecutive losses
        if (consecutiveLosses.get() >= maxConsecutiveLosses) {
            return false;
        }

        return true;
    }

    /**
     * Register trade result.
     *
     * @param pnl trade PnL
     */
    public void registerTrade(double pnl) {
        tradesToday.incrementAndGet();
        dailyPnL.updateAndGet(current -> current + pnl);

        if (pnl >= 0) {
            consecutiveLosses.set(0);
        } else {
            consecutiveLosses.incrementAndGet();
        }
    }

    /**
     * Reset daily limits.
     */
    public void resetDaily() {
        tradesToday.set(0);
        dailyPnL.set(0.0);
        consecutiveLosses.set(0);
    }

    /**
     * Get trades today count.
     */
    public int getTradesToday() {
        return tradesToday.get();
    }

    /**
     * Get daily PnL.
     */
    public double getDailyPnL() {
        return dailyPnL.get();
    }

    /**
     * Get consecutive losses count.
     */
    public int getConsecutiveLosses() {
        return consecutiveLosses.get();
    }

    /**
     * Get max trades per day.
     */
    public int getMaxTradesPerDay() {
        return maxTradesPerDay;
    }

    /**
     * Get max daily loss.
     */
    public double getMaxDailyLoss() {
        return maxDailyLoss;
    }
}
