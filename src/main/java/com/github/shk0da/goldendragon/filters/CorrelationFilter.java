package com.github.shk0da.goldendragon.filters;

import com.github.shk0da.goldendragon.model.Candle;
import com.github.shk0da.goldendragon.utils.LoggingUtils;
import java.util.List;
import java.util.Map;

/**
 * Portfolio-level correlation filter. Blocks new entries when the candidate asset is highly
 * correlated with existing open positions, preventing over-concentration in correlated instruments.
 */
public class CorrelationFilter {

    private final boolean enabled;
    private final double correlationThreshold;
    private final int returnWindow;

    public CorrelationFilter(boolean enabled) {
        this(enabled, 0.8, 20);
    }

    public CorrelationFilter(boolean enabled, double correlationThreshold, int returnWindow) {
        this.enabled = enabled;
        this.correlationThreshold = correlationThreshold;
        this.returnWindow = returnWindow;
    }

    /**
     * Check if a new position is allowed based on correlation with existing open positions.
     *
     * @param newTicker ticker of the candidate asset
     * @param newCandles candle history of the candidate asset
     * @param openPositionCandles map of ticker to candles for currently open positions
     * @return true if entry is allowed, false if blocked by high correlation
     */
    public boolean canTrade(
            String newTicker,
            List<Candle> newCandles,
            Map<String, List<Candle>> openPositionCandles) {
        if (!enabled) {
            return true;
        }
        if (openPositionCandles == null || openPositionCandles.isEmpty()) {
            return true;
        }
        if (newCandles == null || newCandles.size() < returnWindow + 1) {
            return true;
        }

        double[] newReturns = calculateReturns(newCandles);

        for (Map.Entry<String, List<Candle>> entry : openPositionCandles.entrySet()) {
            String existingTicker = entry.getKey();
            List<Candle> existingCandles = entry.getValue();

            if (existingCandles == null || existingCandles.size() < returnWindow + 1) {
                continue;
            }

            double[] existingReturns = calculateReturns(existingCandles);
            double correlation = calculateCorrelation(newReturns, existingReturns);

            if (correlation > correlationThreshold) {
                LoggingUtils.log(
                        String.format(
                                "CorrelationFilter: BLOCKED entry to %s — correlation with %s = %.3f (threshold %.3f)",
                                newTicker, existingTicker, correlation, correlationThreshold));
                return false;
            }
        }

        return true;
    }

    /**
     * Returns the block reason if trade is blocked, or null if allowed.
     *
     * @param newTicker ticker of the candidate asset
     * @param newCandles candle history of the candidate asset
     * @param openPositionCandles map of ticker to candles for currently open positions
     * @return block reason string or null
     */
    public String getBlockReason(
            String newTicker,
            List<Candle> newCandles,
            Map<String, List<Candle>> openPositionCandles) {
        if (!enabled) {
            return null;
        }
        if (openPositionCandles == null || openPositionCandles.isEmpty()) {
            return null;
        }
        if (newCandles == null || newCandles.size() < returnWindow + 1) {
            return null;
        }

        double[] newReturns = calculateReturns(newCandles);

        for (Map.Entry<String, List<Candle>> entry : openPositionCandles.entrySet()) {
            String existingTicker = entry.getKey();
            List<Candle> existingCandles = entry.getValue();

            if (existingCandles == null || existingCandles.size() < returnWindow + 1) {
                continue;
            }

            double[] existingReturns = calculateReturns(existingCandles);
            double correlation = calculateCorrelation(newReturns, existingReturns);

            if (correlation > correlationThreshold) {
                return String.format("HIGH_CORRELATION_%s_%.3f", existingTicker, correlation);
            }
        }

        return null;
    }

    /**
     * Calculate simple returns from candle close prices over the configured window.
     */
    private double[] calculateReturns(List<Candle> candles) {
        int size = candles.size();
        int start = Math.max(0, size - returnWindow - 1);
        int count = size - start - 1;
        double[] returns = new double[count];
        for (int i = 0; i < count; i++) {
            double prevClose = candles.get(start + i).close;
            double currClose = candles.get(start + i + 1).close;
            returns[i] = (prevClose != 0.0) ? (currClose - prevClose) / prevClose : 0.0;
        }
        return returns;
    }

    /**
     * Calculate Pearson correlation coefficient between two return series. Uses the overlapping
     * tail aligned to the most recent data.
     */
    private double calculateCorrelation(double[] returnsA, double[] returnsB) {
        int len = Math.min(returnsA.length, returnsB.length);
        if (len < 2) {
            return 0.0;
        }

        int offsetA = returnsA.length - len;
        int offsetB = returnsB.length - len;

        double sumA = 0.0;
        double sumB = 0.0;
        for (int i = 0; i < len; i++) {
            sumA += returnsA[offsetA + i];
            sumB += returnsB[offsetB + i];
        }
        double meanA = sumA / len;
        double meanB = sumB / len;

        double covAB = 0.0;
        double varA = 0.0;
        double varB = 0.0;
        for (int i = 0; i < len; i++) {
            double dA = returnsA[offsetA + i] - meanA;
            double dB = returnsB[offsetB + i] - meanB;
            covAB += dA * dB;
            varA += dA * dA;
            varB += dB * dB;
        }

        double denom = Math.sqrt(varA * varB);
        if (denom < 1e-12) {
            return 0.0;
        }
        return covAB / denom;
    }

    public boolean isEnabled() {
        return enabled;
    }

    public double getCorrelationThreshold() {
        return correlationThreshold;
    }

    public int getReturnWindow() {
        return returnWindow;
    }
}
