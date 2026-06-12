package com.github.shk0da.GoldenDragon.money;

import com.github.shk0da.GoldenDragon.model.Candle;
import com.github.shk0da.GoldenDragon.model.Position;

/**
 * Stop loss manager with multiple stop types and trailing logic. Supports fixed, ATR-based, and
 * trailing stops.
 */
public class StopLossManager {

  private final double atrStopMultiplier;
  private final double trailingActivationR;
  private final double trailingMultiplier;
  private final double breakevenActivationR;
  private final double breakevenBuffer;

  /**
   * Create stop loss manager with specified parameters.
   *
   * @param atrStopMultiplier ATR multiplier for initial stop (e.g., 2.0)
   * @param trailingActivationR activate trailing after X R profit (e.g., 1.0)
   * @param trailingMultiplier trailing stop distance in ATR (e.g., 1.0)
   * @param breakevenActivationR move to breakeven after X R profit (e.g., 0.5)
   * @param breakevenBuffer buffer above entry for breakeven (e.g., 0.001)
   */
  public StopLossManager(
      double atrStopMultiplier,
      double trailingActivationR,
      double trailingMultiplier,
      double breakevenActivationR,
      double breakevenBuffer) {
    this.atrStopMultiplier = atrStopMultiplier;
    this.trailingActivationR = trailingActivationR;
    this.trailingMultiplier = trailingMultiplier;
    this.breakevenActivationR = breakevenActivationR;
    this.breakevenBuffer = breakevenBuffer;
  }

  /**
   * Calculate initial stop loss level.
   *
   * @param entry entry price
   * @param atr current ATR value
   * @param direction position direction ("BUY" or "SELL")
   * @return stop loss price
   */
  public double calculateInitialStop(double entry, double atr, String direction) {
    if (entry <= 0 || atr <= 0) {
      return entry;
    }

    double stopDistance = atr * atrStopMultiplier;
    if ("BUY".equals(direction)) {
      return entry - stopDistance;
    } else {
      return entry + stopDistance;
    }
  }

  /**
   * Update stop loss for existing position. Only moves stop in favorable direction (up for long,
   * down for short).
   *
   * @param position current position
   * @param candle current candle
   * @param atr current ATR value
   * @param initialRisk initial risk (entry - initial SL for long)
   * @return new stop loss level or null if unchanged
   */
  public Double updateStopLoss(Position position, Candle candle, double atr, double initialRisk) {
    if (position == null || position.quantity <= 0 || position.entryPrice == null) {
      return null;
    }

    String direction = position.direction;
    double currentStop = position.stopLoss != null ? position.stopLoss : position.entryPrice;
    double entry = position.entryPrice;

    // Calculate current PnL in R units
    double pnlR = calculatePnLInR(position, candle.close, initialRisk);

    // Check for breakeven move
    if (pnlR >= breakevenActivationR) {
      double breakevenStop =
          "BUY".equals(direction) ? entry + breakevenBuffer : entry - breakevenBuffer;

      if (isBetterStop(breakevenStop, currentStop, direction)) {
        return breakevenStop;
      }
    }

    // Check for trailing stop activation
    if (pnlR >= trailingActivationR && trailingMultiplier > 0 && atr > 0) {
      double trailDistance = atr * trailingMultiplier;
      double trailingStop =
          "BUY".equals(direction) ? candle.close - trailDistance : candle.close + trailDistance;

      if (isBetterStop(trailingStop, currentStop, direction)) {
        return trailingStop;
      }
    }

    return null;
  }

  /**
   * Calculate PnL in R units (multiples of initial risk).
   *
   * @param position current position
   * @param currentPrice current price
   * @param initialRisk initial risk per unit
   * @return PnL in R units
   */
  private double calculatePnLInR(Position position, double currentPrice, double initialRisk) {
    if (position.entryPrice == null || initialRisk <= 0) {
      return 0;
    }

    double pnlPerUnit =
        "BUY".equals(position.direction)
            ? currentPrice - position.entryPrice
            : position.entryPrice - currentPrice;

    return pnlPerUnit / initialRisk;
  }

  /**
   * Check if new stop is better than current stop. For long: higher is better. For short: lower is
   * better.
   *
   * @param newStop new stop level
   * @param currentStop current stop level
   * @param direction position direction
   * @return true if new stop is better
   */
  private boolean isBetterStop(double newStop, double currentStop, String direction) {
    if ("BUY".equals(direction)) {
      return newStop > currentStop;
    } else {
      return newStop < currentStop;
    }
  }

  /**
   * Get ATR stop multiplier.
   *
   * @return ATR multiplier for initial stop
   */
  public double getAtrStopMultiplier() {
    return atrStopMultiplier;
  }

  /**
   * Get trailing activation R.
   *
   * @return R multiple to activate trailing
   */
  public double getTrailingActivationR() {
    return trailingActivationR;
  }

  /**
   * Get trailing multiplier.
   *
   * @return ATR multiplier for trailing stop
   */
  public double getTrailingMultiplier() {
    return trailingMultiplier;
  }
}
