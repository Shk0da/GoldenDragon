package com.github.shk0da.goldendragon.money;

import com.github.shk0da.goldendragon.model.Candle;

import java.util.List;

/**
 * Level-based SL/TP algorithm.
 * Places stops just below support levels (for LONG) or above resistance (for SHORT),
 * and take-profits just before the next resistance/support level.
 *
 * <p>Support/resistance are identified as recent swing lows/highs with the most volume.
 * This keeps stops out of noise while giving price room to reach targets.
 *
 * <p>Config: slMult = swing lookback for support/resistance, tpMult = fallback R:R when level
 * distance is unreliable.
 */
public class LevelStrategy implements StopLossTakeProfitStrategy {

    private static final int DEFAULT_LOOKBACK = 20;
    private static final double SUPPORT_BUFFER_ATR = 1.1;
    private static final double TP_BUFFER_ATR = 0.25;

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
            int atrPeriod) {
        if (entry <= 0.0 || hourCandles == null || hourCandles.size() < DEFAULT_LOOKBACK || dAtr <= 0.0) {
            return null;
        }

        int lookback = (int) Math.max(5, Math.min(DEFAULT_LOOKBACK, slMult));
        double fallbackRr = Math.max(1.0, tpMult);

        // Find recent significant swing levels within lookback window
        List<Candle> window = hourCandles.subList(
                Math.max(0, hourCandles.size() - lookback), hourCandles.size());

        // Find the most significant support (recent low with price reaction) and resistance
        double support = findSupportLevel(window);
        double resistance = findResistanceLevel(window);

        if (isBuy) {
            // SL below support level (protects against breakdown), with ATR buffer so stop is
            // not knocked by wicks
            double slDist;
            if (support > 0.0 && support < entry) {
                slDist = (entry - support) + dAtr * SUPPORT_BUFFER_ATR;
            } else {
                // No usable support, fall back to ATR-based stop
                slDist = dAtr * fallbackRr;
            }

            double tpDist;
            if (resistance > 0.0 && resistance > entry) {
                tpDist = Math.max((resistance - entry) - dAtr * TP_BUFFER_ATR, 0.0);
            } else {
                tpDist = slDist * fallbackRr;
            }

            if (strongTrendAdjust(slDist, tpDist, adx) == null) {
                return null;
            }
            return strongTrendAdjust(slDist, tpDist, adx);
        } else {
            // SHORT: SL above resistance, TP below support
            double slDist;
            if (resistance > 0.0 && resistance > entry) {
                slDist = (resistance - entry) + dAtr * SUPPORT_BUFFER_ATR;
            } else {
                slDist = dAtr * fallbackRr;
            }

            double tpDist;
            if (support > 0.0 && support < entry) {
                tpDist = Math.max((entry - support) - dAtr * TP_BUFFER_ATR, 0.0);
            } else {
                tpDist = slDist * fallbackRr;
            }

            return strongTrendAdjust(slDist, tpDist, adx);
        }
    }

    private SLTPResult strongTrendAdjust(double slDist, double tpDist, double adx) {
        if (slDist <= 0.0 || tpDist <= 0.0) {
            return null;
        }
        return new SLTPResult(slDist, tpDist);
    }

    /**
     * Find the most significant support level: the lowest low in the window that had visible
     * price reaction (confirmed by a close back above it within the window).
     */
    private double findSupportLevel(List<Candle> window) {
        double lowest = Double.MAX_VALUE;
        int lowestIdx = -1;
        for (int i = 0; i < window.size(); i++) {
            if (window.get(i).low < lowest) {
                lowest = window.get(i).low;
                lowestIdx = i;
            }
        }
        if (lowestIdx < 0 || lowestIdx >= window.size() - 1) {
            return lowest == Double.MAX_VALUE ? 0.0 : lowest;
        }
        double level = lowest;
        // Confirm price closed back above the level (market respected it as support)
        for (int i = lowestIdx + 1; i < window.size(); i++) {
            if (window.get(i).close > level) {
                return level;
            }
        }
        return 0.0;
    }

    /**
     * Find the most significant resistance level: the highest high in the window that had visible
     * price rejection (confirmed by a close back below it within the window).
     */
    private double findResistanceLevel(List<Candle> window) {
        double highest = Double.MIN_VALUE;
        int highestIdx = -1;
        for (int i = 0; i < window.size(); i++) {
            if (window.get(i).high > highest) {
                highest = window.get(i).high;
                highestIdx = i;
            }
        }
        if (highestIdx < 0 || highestIdx >= window.size() - 1) {
            return highest == Double.MIN_VALUE ? 0.0 : highest;
        }
        double level = highest;
        for (int i = highestIdx + 1; i < window.size(); i++) {
            if (window.get(i).close < level) {
                return level;
            }
        }
        return 0.0;
    }
}
