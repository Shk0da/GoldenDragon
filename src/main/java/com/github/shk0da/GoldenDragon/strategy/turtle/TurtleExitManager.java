package com.github.shk0da.GoldenDragon.strategy.turtle;

import com.github.shk0da.GoldenDragon.model.Candle;
import com.github.shk0da.GoldenDragon.model.Position;
import java.util.List;

/**
 * Turtle exit manager.
 * Handles trailing stops and Donchian exit signals.
 */
public class TurtleExitManager {

    private final double trailingMultiplier;
    private final int exitLookback;
    private final boolean useDonchianExit;

    /**
     * Create Turtle exit manager.
     *
     * @param trailingMultiplier trailing stop ATR multiplier (e.g., 2.0)
     * @param exitLookback Donchian exit lookback (e.g., 10)
     * @param useDonchianExit use Donchian exit signal
     */
    public TurtleExitManager(
            double trailingMultiplier,
            int exitLookback,
            boolean useDonchianExit) {
        this.trailingMultiplier = trailingMultiplier;
        this.exitLookback = exitLookback;
        this.useDonchianExit = useDonchianExit;
    }

    /**
     * Update trailing stop for existing position.
     *
     * @param position current position
     * @param candle current candle
     * @param atr current ATR value
     * @return new stop loss level or null if unchanged
     */
    public Double updateTrailingStop(Position position, Candle candle, double atr) {
        if (position == null || position.quantity <= 0 || position.direction == null) {
            return null;
        }

        if (!"BUY".equals(position.direction)) {
            return null; // Only long positions for now
        }

        if (atr <= 0) {
            return null;
        }

        // Calculate trailing stop: low - (ATR × multiplier)
        double trailingStop = candle.low - (atr * trailingMultiplier);

        // Only move stop up (for long positions)
        double currentStop = position.stopLoss != null ? position.stopLoss : 0.0;
        if (trailingStop > currentStop) {
            return trailingStop;
        }

        return null;
    }

    /**
     * Check if should exit based on Donchian low break.
     *
     * @param candles hourly candles
     * @param position current position
     * @param currentPrice current price
     * @return true if exit signal
     */
    public boolean shouldExitDonchian(List<Candle> candles, Position position, double currentPrice) {
        if (!useDonchianExit || candles == null || candles.size() < exitLookback) {
            return false;
        }

        // Calculate Donchian low (lowest low over lookback period)
        double lowestLow = Double.MAX_VALUE;
        int startIndex = Math.max(0, candles.size() - exitLookback);
        for (int i = startIndex; i < candles.size(); i++) {
            lowestLow = Math.min(lowestLow, candles.get(i).low);
        }

        // Exit if price breaks below Donchian low
        return "BUY".equals(position.direction) && currentPrice < lowestLow;
    }

    /**
     * Get exit lookback period.
     */
    public int getExitLookback() {
        return exitLookback;
    }

    /**
     * Check if using Donchian exit.
     */
    public boolean isUseDonchianExit() {
        return useDonchianExit;
    }
}
