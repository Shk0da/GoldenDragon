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
 *
 * <p>Maximum SL/TP distance is capped at 2 ATR to prevent excessive risk when levels are far.
 * Maximum TP distance is capped at 1.5% of entry price to prevent unrealistic targets.
 */
public class LevelStrategy implements StopLossTakeProfitStrategy {

    private static final int DEFAULT_LOOKBACK = 20;
    private static final double SUPPORT_BUFFER_ATR = 1.1;
    private static final double TP_BUFFER_ATR = 0.25;
    private static final double MAX_ATR_MULT = 2.0;
    private static final double MAX_TP_PERCENT = 0.015;

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

        List<Candle> window = hourCandles.subList(
                Math.max(0, hourCandles.size() - lookback), hourCandles.size());

        double support = findSupportLevel(window);
        double resistance = findResistanceLevel(window);

        double slDist;
        double tpDist;

        if (isBuy) {
            if (support > 0.0 && support < entry && (entry - support) <= dAtr * MAX_ATR_MULT) {
                slDist = (entry - support) + dAtr * SUPPORT_BUFFER_ATR;
            } else {
                slDist = dAtr * fallbackRr;
            }

            if (resistance > 0.0 && resistance > entry && (resistance - entry) <= entry * MAX_TP_PERCENT) {
                tpDist = (resistance - entry) - dAtr * TP_BUFFER_ATR;
            } else {
                tpDist = slDist * fallbackRr;
            }
        } else {
            if (resistance > 0.0 && resistance > entry && (resistance - entry) <= dAtr * MAX_ATR_MULT) {
                slDist = (resistance - entry) + dAtr * SUPPORT_BUFFER_ATR;
            } else {
                slDist = dAtr * fallbackRr;
            }

            if (support > 0.0 && support < entry && (entry - support) <= entry * MAX_TP_PERCENT) {
                tpDist = (entry - support) - dAtr * TP_BUFFER_ATR;
            } else {
                tpDist = slDist * fallbackRr;
            }
        }

        double maxSl = dAtr * MAX_ATR_MULT;
        double maxTp = Math.min(dAtr * MAX_ATR_MULT, entry * MAX_TP_PERCENT);

        if (slDist > maxSl || tpDist > maxTp) {
            double ratio = tpDist / slDist;
            slDist = Math.min(slDist, maxSl);
            tpDist = slDist * ratio;

            if (tpDist > maxTp) {
                tpDist = maxTp;
                slDist = tpDist / ratio;
            }
        }

        if (strongTrendAdjust(slDist, tpDist, adx) == null) {
            return null;
        }
        return strongTrendAdjust(slDist, tpDist, adx);
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
