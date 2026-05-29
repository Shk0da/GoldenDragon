package com.github.shk0da.GoldenDragon.money;

/**
 * Fixed risk position sizing strategy.
 * Calculates position size based on fixed risk percentage per trade.
 *
 * Formula: qty = (balance × riskPercent) / (entry - stopLoss)
 */
public class FixedRiskSizing implements SizingStrategy {

    private final double riskPercent;

    /**
     * Create fixed risk sizing strategy.
     *
     * @param riskPercent risk per trade as decimal (e.g., 0.01 for 1%)
     */
    public FixedRiskSizing(double riskPercent) {
        this.riskPercent = riskPercent;
    }

    @Override
    public int calculateSize(String ticker, double entry, double stopLoss, double balance, double atr) {
        if (entry <= 0 || stopLoss <= 0 || balance <= 0) {
            return 0;
        }

        double stopDistance = Math.abs(entry - stopLoss);
        if (stopDistance <= 0) {
            return 0;
        }

        double riskAmount = balance * riskPercent;
        double qty = riskAmount / stopDistance;

        // Cap by available capital
        double maxQty = balance / entry;
        qty = Math.min(qty, maxQty);

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
