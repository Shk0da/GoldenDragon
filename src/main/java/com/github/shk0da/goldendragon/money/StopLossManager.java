package com.github.shk0da.goldendragon.money;

import com.github.shk0da.goldendragon.model.Candle;
import com.github.shk0da.goldendragon.model.Position;

/**
 * Stop loss manager with trailing logic. Supports breakeven move and step-based trailing
 * for both stop loss and take profit.
 *
 * Trailing activation logic:
 * 1. When position reaches breakeven (including commission), SL moves to entry + buffer
 * 2. After breakeven, trailing activates and moves both SL and TP as price moves favorably
 * 3. Trailing uses step-based approach: SL/TP move only when price moves by stepPercent
 */
public class StopLossManager {

    private final double trailingActivationR;
    private final double trailingMultiplier;
    private final double breakevenActivationR;
    private final double breakevenBuffer;

    // New trailing parameters
    private final boolean trailingEnabled;
    private final double trailingStepPercent;
    private final double trailingDeltaPercent;
    private final int trailingCheckInterval;
    private final double trailingVolumePercent;

    // Commission for breakeven calculation
    private final double commission;

    /**
     * Create stop loss manager with specified parameters.
     *
     * @param trailingActivationR activate trailing after X R profit (e.g., 1.0)
     * @param trailingMultiplier trailing stop distance in ATR (e.g., 1.0)
     * @param breakevenActivationR move to breakeven after X R profit (e.g., 0.5)
     * @param breakevenBuffer buffer above entry for breakeven (e.g., 0.001)
     * @param trailingEnabled enable/disable trailing
     * @param trailingStepPercent step size as % of price (e.g., 0.005 = 0.5%)
     * @param trailingDeltaPercent minimum price movement to trigger trail (e.g., 0.003 = 0.3%)
     * @param trailingCheckInterval check trailing every N candles
     * @param trailingVolumePercent volume percentage for partial trailing (e.g., 0.5 = 50%)
     * @param commission commission rate for breakeven calculation (e.g., 0.0005)
     */
    public StopLossManager(
            double trailingActivationR,
            double trailingMultiplier,
            double breakevenActivationR,
            double breakevenBuffer,
            boolean trailingEnabled,
            double trailingStepPercent,
            double trailingDeltaPercent,
            int trailingCheckInterval,
            double trailingVolumePercent,
            double commission) {
        this.trailingActivationR = trailingActivationR;
        this.trailingMultiplier = trailingMultiplier;
        this.breakevenActivationR = breakevenActivationR;
        this.breakevenBuffer = breakevenBuffer;
        this.trailingEnabled = trailingEnabled;
        this.trailingStepPercent = trailingStepPercent;
        this.trailingDeltaPercent = trailingDeltaPercent;
        this.trailingCheckInterval = trailingCheckInterval;
        this.trailingVolumePercent = trailingVolumePercent;
        this.commission = commission;
    }

    /**
     * Result of stop loss update containing new SL and TP levels.
     */
    public static class TrailingResult {
        public final Double newStopLoss;
        public final Double newTakeProfit;
        public final boolean trailingActivated;

        public TrailingResult(Double newStopLoss, Double newTakeProfit, boolean trailingActivated) {
            this.newStopLoss = newStopLoss;
            this.newTakeProfit = newTakeProfit;
            this.trailingActivated = trailingActivated;
        }
    }

    /**
     * Update stop loss and take profit for existing position.
     * Only moves stops in favorable direction (up for long, down for short).
     *
     * @param position current position
     * @param candle current candle
     * @param atr current ATR value
     * @param initialRisk initial risk (entry - initial SL for long)
     * @param candlesHeld number of candles held (for check interval)
     * @return TrailingResult with new SL/TP levels or null if unchanged
     */
    public TrailingResult updateStopLoss(
            Position position,
            Candle candle,
            double atr,
            double initialRisk,
            int candlesHeld) {

        if (position == null || position.quantity <= 0 || position.entryPrice == null) {
            return new TrailingResult(null, null, false);
        }

        String direction = position.direction;
        Double currentStop = position.stopLoss != null ? position.stopLoss : position.entryPrice;
        Double currentTP = position.takeProfit;
        double entry = position.entryPrice;
        double close = candle.close;

        // Calculate current PnL in R units
        double pnlR = calculatePnLInR(position, close, initialRisk);

        // Calculate breakeven price including commission
        double breakevenPrice = calculateBreakevenPrice(entry, direction);

        // Check if price reached breakeven
        boolean reachedBreakeven = false;
        if ("BUY".equals(direction)) {
            reachedBreakeven = close >= breakevenPrice;
        } else {
            reachedBreakeven = close <= breakevenPrice;
        }

        // Step 1: Move to breakeven when reached
        if (reachedBreakeven && pnlR >= breakevenActivationR) {
            double breakevenStop = "BUY".equals(direction)
                    ? entry + breakevenBuffer
                    : entry - breakevenBuffer;

            if (isBetterStop(breakevenStop, currentStop, direction)) {
                return new TrailingResult(breakevenStop, currentTP, false);
            }
        }

        // Step 2: Trailing after breakeven (if enabled)
        if (trailingEnabled && reachedBreakeven && pnlR >= trailingActivationR) {
            // Check interval
            if (candlesHeld % trailingCheckInterval != 0) {
                return new TrailingResult(null, null, false);
            }

            // Calculate trailing levels
            double trailDistance = atr * trailingMultiplier;
            double stepDistance = entry * trailingStepPercent;

            // Calculate new trailing stop
            double trailingStop = "BUY".equals(direction)
                    ? close - trailDistance
                    : close + trailDistance;

            // Calculate new trailing TP (maintain R:R ratio or use step-based)
            Double trailingTP = null;
            if (currentTP != null) {
                if ("BUY".equals(direction)) {
                    double priceMove = close - entry;
                    if (priceMove >= stepDistance) {
                        int steps = (int) (priceMove / stepDistance);
                        double tpMove = steps * stepDistance;
                        trailingTP = entry + (currentTP - entry) + tpMove * 0.5;
                    }
                } else {
                    double priceMove = entry - close;
                    if (priceMove >= stepDistance) {
                        int steps = (int) (priceMove / stepDistance);
                        double tpMove = steps * stepDistance;
                        trailingTP = entry - (entry - currentTP) - tpMove * 0.5;
                    }
                }
            }

            // Apply trailing if better than current
            Double newStop = null;
            if (isBetterStop(trailingStop, currentStop, direction)) {
                newStop = trailingStop;
            }

            Double newTP = null;
            if (trailingTP != null && isBetterTP(trailingTP, currentTP, direction)) {
                newTP = trailingTP;
            }

            if (newStop != null || newTP != null) {
                return new TrailingResult(
                        newStop != null ? newStop : currentStop,
                        newTP != null ? newTP : currentTP,
                        true);
            }
        }

        return new TrailingResult(null, null, false);
    }

    /**
     * Calculate breakeven price including commission costs.
     */
    private double calculateBreakevenPrice(double entry, String direction) {
        double totalCommission = 2.0 * commission;
        if ("BUY".equals(direction)) {
            return entry * (1.0 + totalCommission);
        } else {
            return entry * (1.0 - totalCommission);
        }
    }

    /**
     * Calculate PnL in R units (multiples of initial risk).
     */
    private double calculatePnLInR(Position position, double currentPrice, double initialRisk) {
        if (position.entryPrice == null || initialRisk <= 0) {
            return 0;
        }

        double pnlPerUnit = "BUY".equals(position.direction)
                ? currentPrice - position.entryPrice
                : position.entryPrice - currentPrice;

        return pnlPerUnit / initialRisk;
    }

    /**
     * Check if new stop is better than current stop.
     */
    private boolean isBetterStop(double newStop, Double currentStop, String direction) {
        if (currentStop == null) {
            return true;
        }
        if ("BUY".equals(direction)) {
            return newStop > currentStop;
        } else {
            return newStop < currentStop;
        }
    }

    /**
     * Check if new TP is better than current TP.
     */
    private boolean isBetterTP(double newTP, Double currentTP, String direction) {
        if (currentTP == null) {
            return true;
        }
        if ("BUY".equals(direction)) {
            return newTP > currentTP;
        } else {
            return newTP < currentTP;
        }
    }
}
