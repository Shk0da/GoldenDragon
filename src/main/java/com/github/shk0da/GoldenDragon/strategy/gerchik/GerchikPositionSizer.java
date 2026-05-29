package com.github.shk0da.GoldenDragon.strategy.gerchik;

/**
 * Gerchik position sizer.
 * Calculates position size based on OR range or ATR.
 * Capped by maxPositionSize to prevent oversized positions.
 */
public class GerchikPositionSizer {

    private final double riskPercent;
    private final double atrStopMultiplier;
    private final double tpRewardRatio;
    private final double maxPositionSize; // Max % of balance per position

    /**
     * Create Gerchik position sizer.
     *
     * @param riskPercent risk per trade as decimal (e.g., 0.01)
     * @param atrStopMultiplier ATR multiplier for stop (e.g., 2.0)
     * @param tpRewardRatio take profit as multiple of risk (e.g., 2.0 for 2R)
     */
    public GerchikPositionSizer(
            double riskPercent,
            double atrStopMultiplier,
            double tpRewardRatio) {
        this(riskPercent, atrStopMultiplier, tpRewardRatio, 0.25); // Default max 25%
    }

    /**
     * Create Gerchik position sizer with position size limit.
     *
     * @param riskPercent risk per trade as decimal (e.g., 0.01)
     * @param atrStopMultiplier ATR multiplier for stop (e.g., 2.0)
     * @param tpRewardRatio take profit as multiple of risk (e.g., 2.0 for 2R)
     * @param maxPositionSize max % of balance per position (e.g., 0.25 for 25%)
     */
    public GerchikPositionSizer(
            double riskPercent,
            double atrStopMultiplier,
            double tpRewardRatio,
            double maxPositionSize) {
        this.riskPercent = riskPercent;
        this.atrStopMultiplier = atrStopMultiplier;
        this.tpRewardRatio = tpRewardRatio;
        this.maxPositionSize = maxPositionSize;
    }

    /**
     * Calculate position size.
     *
     * @param entry entry price
     * @param stopLoss stop loss price
     * @param balance available balance
     * @return position size in units
     */
    public int calculateSize(double entry, double stopLoss, double balance) {
        if (entry <= 0 || stopLoss <= 0 || balance <= 0) {
            return 0;
        }

        double stopDistance = Math.abs(entry - stopLoss);
        if (stopDistance <= 0) {
            return 0;
        }

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

        return (int) Math.floor(qty);
    }

    /**
     * Calculate stop loss based on OR range.
     *
     * @param entry entry price
     * @param orHigh opening range high
     * @param orLow opening range low
     * @param atr current ATR
     * @param isLong true for long position
     * @return stop loss price
     */
    public double calculateStopLoss(double entry, double orHigh, double orLow, double atr, boolean isLong) {
        if (isLong) {
            // For long: stop below OR low
            double orStop = orLow - (atr * 0.5); // Buffer below OR
            double atrStop = entry - (atr * atrStopMultiplier);
            return Math.max(orStop, atrStop); // Use wider stop
        } else {
            // For short: stop above OR high
            double orStop = orHigh + (atr * 0.5);
            double atrStop = entry + (atr * atrStopMultiplier);
            return Math.min(orStop, atrStop);
        }
    }

    /**
     * Calculate take profit.
     *
     * @param entry entry price
     * @param stopLoss stop loss price
     * @param isLong true for long position
     * @return take profit price
     */
    public double calculateTakeProfit(double entry, double stopLoss, boolean isLong) {
        double risk = Math.abs(entry - stopLoss);
        if (isLong) {
            return entry + (risk * tpRewardRatio);
        } else {
            return entry - (risk * tpRewardRatio);
        }
    }

    /**
     * Get risk percent.
     */
    public double getRiskPercent() {
        return riskPercent;
    }

    /**
     * Get take profit reward ratio.
     */
    public double getTpRewardRatio() {
        return tpRewardRatio;
    }
}
