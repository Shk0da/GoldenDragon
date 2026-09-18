package com.github.shk0da.goldendragon.money;

import com.github.shk0da.goldendragon.model.Candle;

import java.util.List;

/**
 * Wave ATR SL/TP algorithm.
 * Uses recent swing high/low as reference point, then normalizes with ATR to calculate distances.
 *
 * <p>For LONG: SL = recent swing low (or near), TP = distance from swing to entry × R:R ratio.
 * For SHORT: SL = recent swing high (or near), TP = distance from swing to entry × R:R ratio.
 *
 * <p>Config: slMult = swing lookback period, tpMult = risk:reward multiplier.
 */
public class WaveAtrStrategy implements StopLossTakeProfitStrategy {

    private static final int DEFAULT_SWING_LOOKBACK = 20;

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
        if (entry <= 0.0 || hourCandles == null || hourCandles.size() < DEFAULT_SWING_LOOKBACK) {
            return null;
        }

        int lookback = (int) Math.max(5, Math.min(DEFAULT_SWING_LOOKBACK, slMult));
        double rrMultiplier = Math.max(1.5, tpMult);

        if (isBuy) {
            // Find recent swing low (last N candles minimum)
            double swingLow = entry;
            for (int i = hourCandles.size() - 1; i >= 0 && i >= hourCandles.size() - lookback; i--) {
                double candleLow = hourCandles.get(i).low;
                if (candleLow < swingLow) {
                    swingLow = candleLow;
                }
            }

            // SL = distance from entry to swing low minus ATR buffer (safety margin)
            double slDist = Math.max(entry - swingLow - dAtr * 0.5, dAtr * 0.5);

            // TP = swing distance × R:R multiplier
            double swingDistance = entry - swingLow;
            double tpDist = swingDistance * rrMultiplier;

            // Ensure minimum TP distance
            tpDist = Math.max(tpDist, dAtr * 0.5);

            // Adjust for trend strength
            if (adx >= 30.0) {
                tpDist *= 1.25;
            }

            return new SLTPResult(slDist, tpDist);

        } else {
            // SHORT position
            // Find recent swing high (last N candles maximum)
            double swingHigh = entry;
            for (int i = hourCandles.size() - 1; i >= 0 && i >= hourCandles.size() - lookback; i--) {
                double candleHigh = hourCandles.get(i).high;
                if (candleHigh > swingHigh) {
                    swingHigh = candleHigh;
                }
            }

            // SL = distance from entry to swing high minus ATR buffer
            double slDist = Math.max(swingHigh - entry - dAtr * 0.5, dAtr * 0.5);

            // TP = swing distance × R:R multiplier
            double swingDistance = swingHigh - entry;
            double tpDist = swingDistance * rrMultiplier;

            // Ensure minimum TP distance
            tpDist = Math.max(tpDist, dAtr * 0.5);

            // Adjust for trend strength
            if (adx >= 30.0) {
                tpDist *= 1.25;
            }

            return new SLTPResult(slDist, tpDist);
        }
    }
}
