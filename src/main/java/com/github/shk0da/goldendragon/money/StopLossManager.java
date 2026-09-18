package com.github.shk0da.goldendragon.money;

import com.github.shk0da.goldendragon.model.Candle;
import com.github.shk0da.goldendragon.model.Position;

/**
 * Stop loss manager with trailing logic.
 *
 * Trailing Stop:
 * 1. When price reaches Entry + Initial Risk * trailingActivationR, trailing activates
 * 2. SL is set to Market Price - ATR * trailingMultiplier
 * 3. SL only moves in profit direction (never down for LONG)
 *
 * Trailing Take Profit:
 * 1. When price reaches Initial TP (or close), TP is removed
 * 2. Highest price is tracked
 * 3. Execution Price = Highest Price * (1 - callbackPercent)
 * 4. Position closes when price <= executionPrice
 */
public class StopLossManager {

    private final double trailingActivationR;
    private final double trailingMultiplier;
    private final double breakevenActivationR;
    private final double breakevenBuffer;

    // Trailing TP parameters
    private final boolean trailingTpEnabled;
    private final double trailingTpCallbackPercent;

    // Trailing control
    private final boolean trailingEnabled;
    private final int trailingCheckInterval;

    // Commission for breakeven calculation
    private final double commission;

    /**
     * Create stop loss manager.
     *
     * @param trailingActivationR activate trailing after X R profit (e.g. 1.0)
     * @param trailingMultiplier trailing stop distance in ATR (e.g. 1.0)
     * @param breakevenActivationR move to breakeven after X R profit (e.g. 0.5)
     * @param breakevenBuffer buffer above entry for breakeven (e.g. 0.001)
     * @param trailingTpEnabled enable trailing take profit
     * @param trailingTpCallbackPercent callback % from highest price to close (e.g. 0.005)
     * @param trailingEnabled enable trailing stop loss
     * @param trailingCheckInterval check trailing every N candles
     * @param commission commission rate for breakeven calculation (e.g. 0.0005)
     */
    public StopLossManager(
            double trailingActivationR,
            double trailingMultiplier,
            double breakevenActivationR,
            double breakevenBuffer,
            boolean trailingTpEnabled,
            double trailingTpCallbackPercent,
            boolean trailingEnabled,
            int trailingCheckInterval,
            double commission) {
        this.trailingActivationR = trailingActivationR;
        this.trailingMultiplier = trailingMultiplier;
        this.breakevenActivationR = breakevenActivationR;
        this.breakevenBuffer = breakevenBuffer;
        this.trailingTpEnabled = trailingTpEnabled;
        this.trailingTpCallbackPercent = trailingTpCallbackPercent;
        this.trailingEnabled = trailingEnabled;
        this.trailingCheckInterval = trailingCheckInterval;
        this.commission = commission;
    }

    /**
     * Result of stop loss update with trailing close signal.
     */
    public static class TrailingResult {
        public final Double newStopLoss;
        public final Double newTakeProfit;
        public final boolean trailingActivated;
        public final boolean shouldClose;
        public final Double highestPrice;
        public final Double executionPrice;

        public TrailingResult(
                Double newStopLoss,
                Double newTakeProfit,
                boolean trailingActivated) {
            this(newStopLoss, newTakeProfit, trailingActivated, false, null, null);
        }

        public TrailingResult(
                Double newStopLoss,
                Double newTakeProfit,
                boolean trailingActivated,
                boolean shouldClose,
                Double highestPrice,
                Double executionPrice) {
            this.newStopLoss = newStopLoss;
            this.newTakeProfit = newTakeProfit;
            this.trailingActivated = trailingActivated;
            this.shouldClose = shouldClose;
            this.highestPrice = highestPrice;
            this.executionPrice = executionPrice;
        }
    }

    /**
     * Update stop loss and take profit for existing position.
     *
     * Trailing Stop:
     * 1. When price reaches Entry + InitialRisk * trailingActivationR, trailing activates
     * 2. SL = MarketPrice - ATR * trailingMultiplier
     * 3. SL only moves in profit direction
     *
     * Trailing Take Profit:
     * 1. When price reaches Initial TP, TP is removed
     * 2. Track highestPrice
     * 3. executionPrice = highestPrice * (1 - callbackPercent)
     * 4. Close when price <= executionPrice
     *
     * @param position current position
     * @param candle current candle
     * @param atr current ATR
     * @param initialRisk initial risk (entry - initial SL for long)
     * @param candlesHeld number of candles held (for check interval)
     * @return TrailingResult with new SL/TP and close signal
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
        boolean isBuy = "BUY".equals(direction);

        // Calculate current PnL in R units
        double pnlR = calculatePnLInR(position, close, initialRisk);

        // Calculate breakeven price including commission
        double breakevenPrice = calculateBreakevenPrice(entry, direction);
        boolean reachedBreakeven = isBuy ? close >= breakevenPrice : close <= breakevenPrice;

        // Step 1: Move to breakeven when reached
        if (reachedBreakeven && pnlR >= breakevenActivationR) {
            double breakevenStop = isBuy ? entry + breakevenBuffer : entry - breakevenBuffer;
            if (isBetterStop(breakevenStop, currentStop, direction)) {
                return new TrailingResult(breakevenStop, currentTP, false);
            }
        }

        // Step 2: Trailing Take Profit — when price reaches Initial TP,
        // TP is removed, track highestPrice, close on callback from peak
        if (trailingTpEnabled && currentTP != null && reachedTakeProfit(close, currentTP, direction)) {
            // Determine new highest price (LONG: max, SHORT: min)
            Double prevHighest = position.highestPrice;
            double newHighest;
            if (isBuy) {
                newHighest = prevHighest != null ? Math.max(prevHighest, close) : close;
            } else {
                newHighest = prevHighest != null ? Math.min(prevHighest, close) : close;
            }

            // Execution price = highestPrice * (1 - callback%)
            double newExecution;
            if (isBuy) {
                newExecution = newHighest * (1.0 - trailingTpCallbackPercent);
            } else {
                newExecution = newHighest * (1.0 + trailingTpCallbackPercent);
            }

            // Close position when price reaches executionPrice
            boolean shouldClose = isBuy
                    ? close <= newExecution
                    : close >= newExecution;

            return new TrailingResult(
                    currentStop,
                    null, // TP removed
                    true,
                    shouldClose,
                    newHighest,
                    newExecution);
        }

        // Step 2b: If TP already removed and highestPrice is tracked, check for close signal
        if (trailingTpEnabled && position.highestPrice != null && position.executionPrice != null) {
            boolean shouldClose = isBuy
                    ? close <= position.executionPrice
                    : close >= position.executionPrice;

            if (shouldClose) {
                return new TrailingResult(currentStop, null, true, true, position.highestPrice, position.executionPrice);
            }
        }

        // Step 3: Trailing Stop Loss after breakeven (if enabled)
        if (trailingEnabled && reachedBreakeven && pnlR >= trailingActivationR) {
            // Check interval
            if (candlesHeld % trailingCheckInterval != 0) {
                return new TrailingResult(null, null, false);
            }

            // Trailing stop follows price at fixed ATR distance
            double trailDistance = atr * trailingMultiplier;
            double trailingStop = isBuy ? close - trailDistance : close + trailDistance;

            // Stop must stay below TP (LONG) / above TP (SHORT)
            double stopLimit = currentTP != null ? currentTP : (isBuy ? Double.MAX_VALUE : -Double.MAX_VALUE);
            boolean stopBelowTp = isBuy ? trailingStop < stopLimit : trailingStop > stopLimit;

            Double newStop = null;
            if (isBetterStop(trailingStop, currentStop, direction) && stopBelowTp) {
                newStop = trailingStop;
            }

            if (newStop != null) {
                return new TrailingResult(newStop, currentTP, true);
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
     * Check if price has reached take profit level.
     */
    private boolean reachedTakeProfit(double close, Double takeProfit, String direction) {
        if (takeProfit == null) {
            return false;
        }
        if ("BUY".equals(direction)) {
            return close >= takeProfit;
        } else {
            return close <= takeProfit;
        }
    }
}
