package com.github.shk0da.GoldenDragon.strategy.turtle;

/**
 * Turtle position sizer.
 * Calculates position size based on ATR and risk per trade.
 * Capped by maxPositionSize to prevent oversized positions.
 */
public class TurtlePositionSizer {

    private final double riskPercent;
    private final double atrStopMultiplier;
    private final int minLotSize;
    private final int lotStep;
    private final double maxPositionSize; // Max % of balance per position

    /**
     * Create Turtle position sizer.
     *
     * @param riskPercent risk per trade as decimal (e.g., 0.01 for 1%)
     * @param atrStopMultiplier ATR multiplier for stop distance (e.g., 2.0)
     * @param minLotSize minimum position size (default: 1)
     * @param lotStep position size step (default: 1)
     */
    public TurtlePositionSizer(
            double riskPercent,
            double atrStopMultiplier,
            int minLotSize,
            int lotStep) {
        this(riskPercent, atrStopMultiplier, minLotSize, lotStep, 0.25); // Default max 25%
    }

    /**
     * Create Turtle position sizer with position size limit.
     *
     * @param riskPercent risk per trade as decimal (e.g., 0.01 for 1%)
     * @param atrStopMultiplier ATR multiplier for stop distance (e.g., 2.0)
     * @param minLotSize minimum position size (default: 1)
     * @param lotStep position size step (default: 1)
     * @param maxPositionSize max % of balance per position (e.g., 0.25 for 25%)
     */
    public TurtlePositionSizer(
            double riskPercent,
            double atrStopMultiplier,
            int minLotSize,
            int lotStep,
            double maxPositionSize) {
        this.riskPercent = riskPercent;
        this.atrStopMultiplier = atrStopMultiplier;
        this.minLotSize = minLotSize;
        this.lotStep = lotStep;
        this.maxPositionSize = maxPositionSize;
    }

    /**
     * Calculate position size.
     *
     * @param entry entry price
     * @param atr current ATR value
     * @param balance available balance
     * @return position size in units (0 if calculation fails)
     */
    public int calculateSize(double entry, double atr, double balance) {
        if (entry <= 0 || atr <= 0 || balance <= 0) {
            return 0;
        }

        // Calculate stop distance
        double stopDistance = atr * atrStopMultiplier;

        // Calculate risk amount
        double riskAmount = balance * riskPercent;

        // Calculate position size: qty = riskAmount / stopDistance
        double qty = riskAmount / stopDistance;

        // Cap by max position size (% of capital)
        double maxQtyBySize = (balance * maxPositionSize) / entry;
        qty = Math.min(qty, maxQtyBySize);

        // Cap by available capital (safety check)
        double maxQtyByCapital = balance / entry;
        qty = Math.min(qty, maxQtyByCapital);

        // Round down to lot step
        int finalQty = (int) Math.floor(qty);
        if (finalQty < minLotSize) {
            return 0;
        }

        return (finalQty / lotStep) * lotStep;
    }

    /**
     * Calculate stop loss price.
     *
     * @param entry entry price
     * @param atr current ATR value
     * @return stop loss price
     */
    public double calculateStopLoss(double entry, double atr) {
        if (entry <= 0 || atr <= 0) {
            return entry * 0.95; // Default 5% stop
        }
        return entry - (atr * atrStopMultiplier);
    }

    /**
     * Get risk percent.
     */
    public double getRiskPercent() {
        return riskPercent;
    }

    /**
     * Get ATR stop multiplier.
     */
    public double getAtrStopMultiplier() {
        return atrStopMultiplier;
    }
}
