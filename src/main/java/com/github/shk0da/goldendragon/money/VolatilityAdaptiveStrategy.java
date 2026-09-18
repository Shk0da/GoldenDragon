package com.github.shk0da.goldendragon.money;

import com.github.shk0da.goldendragon.model.Candle;

import java.util.List;

/**
 * Volatility-adaptive SL/TP algorithm.
 * Adjusts SL/TP based on current ATR relative to historical ATR range.
 *
 * <p>Calculates ATR percentile over last N periods:
 * - High percentile (>80%): high volatility → wider SL/TP
 * - Low percentile (<20%): low volatility → tighter SL/TP
 * - Normal percentile: standard SL/TP
 *
 * <p>Config: slMult and tpMult are base multipliers, adjusted by volatility percentile.
 */
public class VolatilityAdaptiveStrategy implements StopLossTakeProfitStrategy {

    private static final int ATR_HISTORY_PERIOD = 50;
    private static final int ATR_PERIOD = 14;

    @Override
    public SLTPResult calculate(
            double entry,
            boolean isBuy,
            List<Candle> hourCandles,
            double dAtr,
            double avgAtr,
            double adx,
            double slMult,
            double tpMult,
            int atrPeriod,
            double commission) {
        if (entry <= 0.0 || hourCandles == null || hourCandles.size() < ATR_HISTORY_PERIOD + ATR_PERIOD) {
            return null;
        }

        // Calculate ATR history to determine percentile
        double[] atrHistory = new double[ATR_HISTORY_PERIOD];
        for (int i = 0; i < ATR_HISTORY_PERIOD; i++) {
            int idx = hourCandles.size() - 1 - i;
            if (idx >= ATR_PERIOD) {
                atrHistory[i] = calculateSimpleAtr(hourCandles, idx, ATR_PERIOD);
            } else {
                atrHistory[i] = dAtr;
            }
        }

        // Calculate ATR percentile
        double atrPercentile = calculatePercentile(atrHistory, dAtr);

        // Base SL/TP distances
        double slDist = dAtr * slMult;
        double tpDist = dAtr * tpMult;

        // Adjust based on ATR percentile
        double volatilityFactor;
        if (atrPercentile >= 80.0) {
            // High volatility: wider stops
            volatilityFactor = 1.3;
        } else if (atrPercentile >= 60.0) {
            // Above average volatility
            volatilityFactor = 1.15;
        } else if (atrPercentile >= 40.0) {
            // Normal volatility
            volatilityFactor = 1.0;
        } else if (atrPercentile >= 20.0) {
            // Below average volatility
            volatilityFactor = 0.85;
        } else {
            // Low volatility: tighter stops
            volatilityFactor = 0.7;
        }

        slDist *= volatilityFactor;
        tpDist *= volatilityFactor;

        // Adjust for trend strength
        if (adx >= 30.0) {
            slDist *= 1.10;
            tpDist *= (adx >= 45.0) ? 1.35 : 1.20;
        }

        // Adjust for range regime
        if (adx > 0.0 && adx <= 15.0) {
            slDist *= 0.90;
            tpDist *= 0.85;
        }

        return new SLTPResult(slDist, tpDist);
    }

    /**
     * Calculate simple ATR for a given period ending at index.
     */
    private double calculateSimpleAtr(List<Candle> candles, int endIndex, int period) {
        if (endIndex < period - 1) {
            return 0.0;
        }

        double sum = 0.0;
        for (int i = 0; i < period; i++) {
            int idx = endIndex - i;
            if (idx <= 0) {
                break;
            }
            Candle current = candles.get(idx);
            Candle previous = candles.get(idx - 1);

            double highLow = current.high - current.low;
            double highClose = Math.abs(current.high - previous.close);
            double lowClose = Math.abs(current.low - previous.close);

            double trueRange = Math.max(highLow, Math.max(highClose, lowClose));
            sum += trueRange;
        }

        return sum / period;
    }

    /**
     * Calculate percentile of value in array.
     * @return percentile (0-100)
     */
    private double calculatePercentile(double[] values, double value) {
        if (values == null || values.length == 0) {
            return 50.0;
        }

        int count = 0;
        for (double v : values) {
            if (v <= value) {
                count++;
            }
        }

        return (double) count / values.length * 100.0;
    }
}
