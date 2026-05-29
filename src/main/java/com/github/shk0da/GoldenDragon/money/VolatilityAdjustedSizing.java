package com.github.shk0da.GoldenDragon.money;

/**
 * Volatility-adjusted position sizing strategy.
 * Reduces position size when volatility (ATR) is high.
 *
 * Formula: qty = (balance × riskPercent × volatilityAdjustment) / (entry - stopLoss)
 * where volatilityAdjustment = baseVolatility / currentVolatility
 */
public class VolatilityAdjustedSizing implements SizingStrategy {

    private final double riskPercent;
    private final double baseVolatility;
    private final double minVolatilityAdjustment;
    private final double maxVolatilityAdjustment;

    /**
     * Create volatility-adjusted sizing strategy.
     *
     * @param riskPercent risk per trade as decimal (e.g., 0.01 for 1%)
     * @param baseVolatility baseline ATR for volatility adjustment
     * @param minVolatilityAdjustment minimum volatility adjustment factor (e.g., 0.5)
     * @param maxVolatilityAdjustment maximum volatility adjustment factor (e.g., 1.5)
     */
    public VolatilityAdjustedSizing(
            double riskPercent,
            double baseVolatility,
            double minVolatilityAdjustment,
            double maxVolatilityAdjustment) {
        this.riskPercent = riskPercent;
        this.baseVolatility = baseVolatility;
        this.minVolatilityAdjustment = minVolatilityAdjustment;
        this.maxVolatilityAdjustment = maxVolatilityAdjustment;
    }

    @Override
    public int calculateSize(String ticker, double entry, double stopLoss, double balance, double atr) {
        if (entry <= 0 || stopLoss <= 0 || balance <= 0 || atr <= 0) {
            return 0;
        }

        double stopDistance = Math.abs(entry - stopLoss);
        if (stopDistance <= 0) {
            return 0;
        }

        // Calculate volatility adjustment
        double volatilityAdjustment = baseVolatility / atr;
        volatilityAdjustment = Math.max(minVolatilityAdjustment,
                Math.min(maxVolatilityAdjustment, volatilityAdjustment));

        double riskAmount = balance * riskPercent * volatilityAdjustment;
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

    /**
     * Get base volatility parameter.
     *
     * @return baseline ATR for volatility adjustment
     */
    public double getBaseVolatility() {
        return baseVolatility;
    }
}
