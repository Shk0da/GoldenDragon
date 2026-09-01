package com.github.shk0da.goldendragon.strategy.orderbook;

import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

/**
 * Circuit breaker that stops trading after hitting a daily loss limit.
 *
 * <p>Tracks cumulative P&L for the current trading day. When the loss exceeds
 * the configured limit, trading is blocked until the next day.
 *
 * <p>Thread-safe for concurrent access from multiple strategy threads.
 */
public final class DailyLossLimit {

    private final double dailyLimitRub;
    private final AtomicReference<Double> cumulativePnl = new AtomicReference<>(0.0);
    private final AtomicReference<Long> currentDay = new AtomicReference<>(System.currentTimeMillis() / 86400000L);
    private final AtomicBoolean tradingStopped = new AtomicBoolean(false);

    public DailyLossLimit(double dailyLimitRub) {
        if (dailyLimitRub >= 0) {
            throw new IllegalArgumentException("dailyLimitRub must be negative (loss limit), got " + dailyLimitRub);
        }
        this.dailyLimitRub = dailyLimitRub;
    }

    /**
     * Add P&L from a completed trade.
     *
     * @param pnl trade P&L (positive for wins, negative for losses)
     * @return true if trading is still allowed after this trade
     */
    public boolean addPnl(double pnl) {
        resetIfNewDay();
        cumulativePnl.accumulateAndGet(pnl, Double::sum);
        return canTrade();
    }

    /**
     * Check if trading is currently allowed.
     *
     * @return true if cumulative loss has not exceeded the daily limit
     */
    public boolean canTrade() {
        resetIfNewDay();
        if (tradingStopped.get()) {
            return false;
        }
        Double pnl = cumulativePnl.get();
        return pnl == null || pnl > dailyLimitRub;
    }

    /**
     * Get current cumulative P&L for today.
     *
     * @return cumulative P&L in RUB
     */
    public double getCumulativePnl() {
        resetIfNewDay();
        Double pnl = cumulativePnl.get();
        return pnl != null ? pnl : 0.0;
    }

    /**
     * Get the configured daily loss limit.
     *
     * @return loss limit (negative value, e.g. -500.0 for -500 RUB)
     */
    public double getDailyLimit() {
        return dailyLimitRub;
    }

    /**
     * Manually stop trading (e.g. from UI or emergency stop).
     */
    public void stopTrading() {
        tradingStopped.set(true);
    }

    /**
     * Reset state for a new day.
     */
    private void resetIfNewDay() {
        long today = System.currentTimeMillis() / 86400000L;
        Long lastDay = currentDay.get();
        if (lastDay != null && !lastDay.equals(today)) {
            synchronized (this) {
                lastDay = currentDay.get();
                if (lastDay != null && !lastDay.equals(today)) {
                    currentDay.set(today);
                    cumulativePnl.set(0.0);
                    tradingStopped.set(false);
                }
            }
        }
    }
}