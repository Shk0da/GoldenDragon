package com.github.shk0da.GoldenDragon.strategy.gerchik;

import com.github.shk0da.GoldenDragon.model.Candle;
import com.github.shk0da.GoldenDragon.model.Position;

/**
 * Gerchik exit manager.
 * Handles stops, take profits, and EOD close.
 */
public class GerchikExitManager {

    private final double breakevenAtr;
    private final double trailAtr;
    private final boolean useTrailing;

    /**
     * Create Gerchik exit manager.
     *
     * @param breakevenAtr move to BE after X ATR profit (e.g., 1.0)
     * @param trailAtr trailing stop distance in ATR (e.g., 1.5)
     * @param useTrailing use trailing stop
     */
    public GerchikExitManager(
            double breakevenAtr,
            double trailAtr,
            boolean useTrailing) {
        this.breakevenAtr = breakevenAtr;
        this.trailAtr = trailAtr;
        this.useTrailing = useTrailing;
    }

    /**
     * Update stop loss for existing position.
     *
     * @param position current position
     * @param candle current candle
     * @param atr current ATR
     * @param initialRisk initial risk per unit
     * @return new stop loss or null
     */
    public Double updateStop(Position position, Candle candle, double atr, double initialRisk) {
        if (position == null || position.quantity <= 0 || position.direction == null || atr <= 0) {
            return null;
        }

        if (!"BUY".equals(position.direction)) {
            return null;
        }

        double currentPrice = candle.close;
        double entry = position.entryPrice != null ? position.entryPrice : currentPrice;
        double currentStop = position.stopLoss != null ? position.stopLoss : 0.0;

        // Calculate PnL in ATR units
        double pnlAtr = (currentPrice - entry) / atr;

        // Check for breakeven move
        if (pnlAtr >= breakevenAtr) {
            double breakevenStop = entry + (atr * 0.1); // Small buffer
            if (breakevenStop > currentStop) {
                return breakevenStop;
            }
        }

        // Check for trailing stop
        if (useTrailing && pnlAtr >= breakevenAtr) {
            double trailingStop = candle.low - (atr * trailAtr);
            if (trailingStop > currentStop) {
                return trailingStop;
            }
        }

        return null;
    }

    /**
     * Check if should take profit.
     *
     * @param position current position
     * @param currentPrice current price
     * @param takeProfit take profit level
     * @return true if TP hit
     */
    public boolean shouldTakeProfit(Position position, double currentPrice, double takeProfit) {
        if (position == null || position.quantity <= 0 || takeProfit <= 0) {
            return false;
        }

        if ("BUY".equals(position.direction)) {
            return currentPrice >= takeProfit;
        }

        return false;
    }

    /**
     * Check if should close EOD.
     *
     * @param currentTime current time
     * @param eodCloseTime EOD close time
     * @return true if should close
     */
    public boolean shouldCloseEOD(java.time.LocalTime currentTime, java.time.LocalTime eodCloseTime) {
        return !currentTime.isBefore(eodCloseTime);
    }

    /**
     * Get breakeven ATR.
     */
    public double getBreakevenAtr() {
        return breakevenAtr;
    }

    /**
     * Get trailing ATR.
     */
    public double getTrailAtr() {
        return trailAtr;
    }
}
