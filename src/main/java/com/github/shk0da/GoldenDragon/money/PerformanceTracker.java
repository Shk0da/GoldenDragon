package com.github.shk0da.goldendragon.money;

import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;

/**
 * Performance tracker for trade statistics and PnL tracking. Thread-safe implementation using
 * atomic variables.
 */
public class PerformanceTracker {

    private final AtomicReference<SessionStats> sessionStats =
            new AtomicReference<>(new SessionStats());
    private final AtomicLong peakEquity = new AtomicLong(0);
    private final AtomicLong currentEquity = new AtomicLong(0);

    /** Create performance tracker. */
    public PerformanceTracker() {}

    /**
     * Update equity and track peak.
     *
     * @param equity current equity value
     */
    public void updateEquity(long equity) {
        currentEquity.set(equity);
        peakEquity.updateAndGet(peak -> Math.max(peak, equity));
    }

    /**
     * Update equity and track peak (double version).
     *
     * @param equity current equity value
     */
    public void updateEquity(double equity) {
        updateEquity((long) (equity * 100));
    }

    /**
     * Register trade result.
     *
     * @param pnl trade PnL
     * @param ticker ticker symbol
     * @param direction trade direction ("BUY" or "SELL")
     * @param entryPrice entry price
     * @param exitPrice exit price
     */
    public void registerTrade(
            double pnl, String ticker, String direction, double entryPrice, double exitPrice) {
        SessionStats oldStats = sessionStats.get();
        int newWins = pnl >= 0 ? oldStats.wins + 1 : oldStats.wins;
        int newLosses = pnl < 0 ? oldStats.losses + 1 : oldStats.losses;
        double newTotalPnL = oldStats.totalPnL + pnl;
        int newTrades = oldStats.trades + 1;
        double newLargestWin = Math.max(oldStats.largestWin, pnl >= 0 ? pnl : 0);
        double newLargestLoss = Math.min(oldStats.largestLoss, pnl < 0 ? pnl : 0);

        SessionStats newStats =
                new SessionStats(
                        newWins, newLosses, newTotalPnL, newTrades, newLargestWin, newLargestLoss);
        sessionStats.set(newStats);
    }

    /**
     * Get current drawdown.
     *
     * @return current drawdown as decimal (0.0 to 1.0)
     */
    public double getCurrentDrawdown() {
        long current = currentEquity.get();
        long peak = peakEquity.get();

        if (peak <= 0 || current <= 0) {
            return 0.0;
        }

        return (double) (peak - current) / peak;
    }

    /** Reset session statistics. */
    public void resetSession() {
        sessionStats.set(new SessionStats());
    }

    /** Immutable session statistics holder. */
    public static class SessionStats {
        public final int wins;
        public final int losses;
        public final double totalPnL;
        public final int trades;
        public final double largestWin;
        public final double largestLoss;

        SessionStats() {
            this(0, 0, 0.0, 0, 0.0, 0.0);
        }

        SessionStats(
                int wins,
                int losses,
                double totalPnL,
                int trades,
                double largestWin,
                double largestLoss) {
            this.wins = wins;
            this.losses = losses;
            this.totalPnL = totalPnL;
            this.trades = trades;
            this.largestWin = largestWin;
            this.largestLoss = largestLoss;
        }
    }
}
