package com.github.shk0da.GoldenDragon.strategy.turtle;

import com.github.shk0da.GoldenDragon.model.Candle;
import java.util.List;

/**
 * Turtle breakout signal detector.
 * Identifies Donchian channel breakouts with adaptive lookback and quality filters.
 */
public class TurtleBreakoutSignal {

    private final int defaultLookback;
    private final int minLookback;
    private final int maxLookback;
    private final double breakoutBufferAtr;
    private final double volatilityLowThreshold;
    private final double volatilityHighThreshold;
    private final double minAdx; // Minimum ADX for trend confirmation

    /**
     * Create Turtle breakout signal detector.
     *
     * @param defaultLookback default Donchian lookback period (e.g., 20)
     * @param minLookback minimum adaptive lookback (e.g., 10)
     * @param maxLookback maximum adaptive lookback (e.g., 55)
     * @param breakoutBufferAtr breakout buffer in ATR units (e.g., 0.5)
     * @param volatilityLowThreshold low volatility threshold (e.g., 0.5)
     * @param volatilityHighThreshold high volatility threshold (e.g., 2.0)
     */
    public TurtleBreakoutSignal(
            int defaultLookback,
            int minLookback,
            int maxLookback,
            double breakoutBufferAtr,
            double volatilityLowThreshold,
            double volatilityHighThreshold) {
        this(defaultLookback, minLookback, maxLookback, breakoutBufferAtr,
             volatilityLowThreshold, volatilityHighThreshold, 20.0); // Default ADX 20
    }

    /**
     * Create Turtle breakout signal detector with ADX filter.
     *
     * @param defaultLookback default Donchian lookback period (e.g., 20)
     * @param minLookback minimum adaptive lookback (e.g., 10)
     * @param maxLookback maximum adaptive lookback (e.g., 55)
     * @param breakoutBufferAtr breakout buffer in ATR units (e.g., 0.5)
     * @param volatilityLowThreshold low volatility threshold (e.g., 0.5)
     * @param volatilityHighThreshold high volatility threshold (e.g., 2.0)
     * @param minAdx minimum ADX for trend confirmation (e.g., 20.0)
     */
    public TurtleBreakoutSignal(
            int defaultLookback,
            int minLookback,
            int maxLookback,
            double breakoutBufferAtr,
            double volatilityLowThreshold,
            double volatilityHighThreshold,
            double minAdx) {
        this.defaultLookback = defaultLookback;
        this.minLookback = minLookback;
        this.maxLookback = maxLookback;
        this.breakoutBufferAtr = breakoutBufferAtr;
        this.volatilityLowThreshold = volatilityLowThreshold;
        this.volatilityHighThreshold = volatilityHighThreshold;
        this.minAdx = minAdx;
    }

    /**
     * Detect breakout signal.
     *
     * @param candles hourly candles
     * @param atr current ATR value
     * @param currentPrice current price
     * @return breakout signal string or null if no breakout
     */
    public String detectSignal(List<Candle> candles, double atr, double currentPrice) {
        if (candles == null || candles.size() < minLookback || atr <= 0) {
            return null;
        }

        // Check ADX for trend strength (filter sideways market)
        double adx = calculateAdx(candles);
        if (adx < minAdx) {
            return null; // No trend, skip breakout
        }

        // Calculate adaptive lookback based on volatility
        int lookback = calculateAdaptiveLookback(candles, atr);

        // Calculate Donchian levels
        double[] donchianLevels = calculateDonchianLevels(candles, lookback);
        double donchianHigh = donchianLevels[0];
        double donchianLow = donchianLevels[1];

        // Check for breakout with buffer
        double breakoutLevel = donchianHigh + (atr * breakoutBufferAtr);

        if (currentPrice > breakoutLevel) {
            double breakoutStrength = (currentPrice - donchianHigh) / atr;
            return "TURTLE_B_" + lookback + "_" + formatStrength(breakoutStrength) + "_ADX" + (int)adx;
        }

        return null;
    }

    /**
     * Calculate ADX (14-period) for trend strength.
     * Simplified calculation for performance.
     */
    private double calculateAdx(List<Candle> candles) {
        if (candles.size() < 30) {
            return 0.0;
        }

        int period = 14;
        double plusDM = 0.0;
        double minusDM = 0.0;
        double trSum = 0.0;

        for (int i = candles.size() - period - 1; i < candles.size() - 1; i++) {
            Candle current = candles.get(i);
            Candle prev = candles.get(i - 1);

            double highDiff = current.high - prev.high;
            double lowDiff = prev.low - current.low;

            if (highDiff > lowDiff && highDiff > 0) {
                plusDM += highDiff;
            } else if (lowDiff > highDiff && lowDiff > 0) {
                minusDM += lowDiff;
            }

            double tr = Math.max(current.high - current.low,
                    Math.max(Math.abs(current.high - prev.close),
                            Math.abs(current.low - prev.close)));
            trSum += tr;
        }

        if (trSum == 0) return 0.0;

        double plusDI = (plusDM / trSum) * 100;
        double minusDI = (minusDM / trSum) * 100;
        double dx = Math.abs(plusDI - minusDI) / (plusDI + minusDI) * 100;

        return dx;
    }

    /**
     * Calculate adaptive lookback based on volatility regime.
     */
    private int calculateAdaptiveLookback(List<Candle> candles, double currentAtr) {
        if (candles.size() < 50) {
            return defaultLookback;
        }

        // Calculate average ATR over recent period
        double avgAtr = 0.0;
        int count = Math.min(20, candles.size() - 1);
        for (int i = 0; i < count; i++) {
            Candle c = candles.get(candles.size() - 2 - i);
            Candle p = candles.get(candles.size() - 3 - i);
            double tr = Math.max(
                    Math.max(c.high - c.low, Math.abs(c.high - p.close)),
                    Math.abs(c.low - p.close)
            );
            avgAtr += tr;
        }
        avgAtr /= count;

        if (avgAtr <= 0) {
            return defaultLookback;
        }

        // Calculate volatility ratio
        double volRatio = currentAtr / avgAtr;

        // Adjust lookback based on volatility
        if (volRatio > volatilityHighThreshold) {
            // High volatility: use longer lookback
            return Math.min(maxLookback, defaultLookback + 10);
        } else if (volRatio < volatilityLowThreshold) {
            // Low volatility: use shorter lookback
            return Math.max(minLookback, defaultLookback - 5);
        }

        return defaultLookback;
    }

    /**
     * Calculate Donchian channel levels (high and low).
     */
    private double[] calculateDonchianLevels(List<Candle> candles, int lookback) {
        double highestHigh = Double.MIN_VALUE;
        double lowestLow = Double.MAX_VALUE;

        int startIndex = Math.max(0, candles.size() - lookback);
        for (int i = startIndex; i < candles.size(); i++) {
            Candle c = candles.get(i);
            highestHigh = Math.max(highestHigh, c.high);
            lowestLow = Math.min(lowestLow, c.low);
        }

        return new double[]{highestHigh, lowestLow};
    }

    /**
     * Format breakout strength.
     */
    private String formatStrength(double strength) {
        if (strength >= 2.0) return "STRONG";
        if (strength >= 1.0) return "MODERATE";
        return "WEAK";
    }

    /**
     * Get default lookback period.
     */
    public int getDefaultLookback() {
        return defaultLookback;
    }

    /**
     * Get Donchian high level for given lookback.
     */
    public double getDonchianHigh(List<Candle> candles, int lookback) {
        return calculateDonchianLevels(candles, lookback)[0];
    }

    /**
     * Get Donchian low level for given lookback.
     */
    public double getDonchianLow(List<Candle> candles, int lookback) {
        return calculateDonchianLevels(candles, lookback)[1];
    }
}
