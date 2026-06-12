package com.github.shk0da.goldendragon.money;

/**
 * Fixed risk position sizing strategy. Calculates position size based on fixed risk percentage per
 * trade.
 *
 * <p>Formula: qty = (balance × riskPercent) / (entry - stopLoss) Capped by maxPositionSize to
 * prevent oversized positions.
 */
public class FixedRiskSizing implements SizingStrategy {

    private final double riskPercent;
    private final double maxPositionSize; // Max % of balance per position

    /**
     * Create fixed risk sizing strategy.
     *
     * @param riskPercent risk per trade as decimal (e.g., 0.005 for 0.5%)
     */
    public FixedRiskSizing(double riskPercent) {
        this(riskPercent, 0.15); // Default max 15% of capital per position
    }

    /**
     * Create fixed risk sizing strategy with position size limit.
     *
     * @param riskPercent risk per trade as decimal (e.g., 0.01 for 1%)
     * @param maxPositionSize max % of balance per position (e.g., 0.25 for 25%)
     */
    public FixedRiskSizing(double riskPercent, double maxPositionSize) {
        this.riskPercent = riskPercent;
        this.maxPositionSize = maxPositionSize;
    }

    @Override
    public int calculateSize(
            String ticker, double entry, double stopLoss, double balance, double atr) {
        if (entry <= 0 || stopLoss <= 0 || balance <= 0) {
            return 0;
        }

        double stopDistance = Math.abs(entry - stopLoss);
        if (stopDistance <= 0) {
            return 0;
        }

        // Calculate position size based on risk
        double riskAmount = balance * riskPercent;
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
     * Get risk percent parameter.
     *
     * @return risk per trade as decimal
     */
    public double getRiskPercent() {
        return riskPercent;
    }
}
