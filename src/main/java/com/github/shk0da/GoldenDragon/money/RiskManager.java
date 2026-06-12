package com.github.shk0da.goldendragon.money;

import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

/**
 * Risk manager for trading limits and daily loss control. Thread-safe implementation using atomic
 * variables.
 */
public class RiskManager {

  private final double riskPerTradePercent;
  private final double maxDailyLossPercent;
  private final int maxConsecutiveLosses;

  // Daily statistics (reset at session start)
  private final AtomicReference<DailyStats> dailyStats = new AtomicReference<>(new DailyStats());
  private final AtomicInteger consecutiveLosses = new AtomicInteger(0);

  /**
   * Create risk manager with specified parameters.
   *
   * @param riskPerTradePercent risk per trade as decimal (e.g., 0.01 for 1%)
   * @param maxDailyLossPercent maximum daily loss as decimal (e.g., 0.03 for 3%)
   * @param maxConsecutiveLosses maximum consecutive losing trades before halt
   */
  public RiskManager(
      double riskPerTradePercent, double maxDailyLossPercent, int maxConsecutiveLosses) {
    this.riskPerTradePercent = riskPerTradePercent;
    this.maxDailyLossPercent = maxDailyLossPercent;
    this.maxConsecutiveLosses = maxConsecutiveLosses;
  }

  /**
   * Check if trading is allowed based on current limits.
   *
   * @param equity current equity/balance
   * @return true if trading is allowed, false if limits reached
   */
  public boolean canTrade(double equity) {
    if (equity <= 0) {
      return false;
    }

    // Check consecutive losses
    if (consecutiveLosses.get() >= maxConsecutiveLosses) {
      return false;
    }

    // Check daily loss
    DailyStats stats = dailyStats.get();
    double dailyLossPercent = Math.abs(stats.dailyPnL) / equity;
    if (stats.dailyPnL < 0 && dailyLossPercent >= maxDailyLossPercent) {
      return false;
    }

    return true;
  }

  /**
   * Register trade result and update statistics.
   *
   * @param pnl trade PnL (positive for win, negative for loss)
   */
  public void registerTrade(double pnl) {
    DailyStats oldStats = dailyStats.get();
    DailyStats newStats;

    if (pnl >= 0) {
      consecutiveLosses.set(0);
      newStats =
          new DailyStats(
              oldStats.wins + 1, oldStats.losses, oldStats.dailyPnL + pnl, oldStats.trades + 1);
    } else {
      consecutiveLosses.incrementAndGet();
      newStats =
          new DailyStats(
              oldStats.wins, oldStats.losses + 1, oldStats.dailyPnL + pnl, oldStats.trades + 1);
    }
    dailyStats.set(newStats);
  }

  /** Reset daily statistics (called at session start). */
  public void resetDailyLimits() {
    dailyStats.set(new DailyStats());
    consecutiveLosses.set(0);
  }

  /**
   * Get current consecutive loss count.
   *
   * @return number of consecutive losses
   */
  public int getConsecutiveLosses() {
    return consecutiveLosses.get();
  }

  /**
   * Get current day PnL.
   *
   * @return daily PnL
   */
  public double getDailyPnL() {
    return dailyStats.get().dailyPnL;
  }

  /** Immutable daily statistics holder. */
  private static class DailyStats {
    final int wins;
    final int losses;
    final double dailyPnL;
    final int trades;

    DailyStats() {
      this(0, 0, 0.0, 0);
    }

    DailyStats(int wins, int losses, double dailyPnL, int trades) {
      this.wins = wins;
      this.losses = losses;
      this.dailyPnL = dailyPnL;
      this.trades = trades;
    }
  }
}
