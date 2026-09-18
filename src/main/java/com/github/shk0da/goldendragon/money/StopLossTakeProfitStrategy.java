package com.github.shk0da.goldendragon.money;

import com.github.shk0da.goldendragon.model.Candle;

import java.util.List;

/**
 * Strategy interface for calculating stop-loss and take-profit levels.
 * Implementations provide different algorithms for determining optimal SL/TP distances.
 */
public interface StopLossTakeProfitStrategy {

    /**
     * Calculate stop-loss and take-profit distances from entry price.
     *
     * @param entry entry price
     * @param isBuy true for LONG position, false for SHORT
     * @param hourCandles hourly candles for indicator calculation
     * @param dAtr daily ATR value
     * @param avgAtr average ATR (EMA of ATR)
     * @param adx ADX value for trend strength
     * @param slMult stop-loss multiplier from config
     * @param tpMult take-profit multiplier from config
     * @param atrPeriod ATR period
     * @return SLTPResult with calculated distances, or null if calculation fails
     */
    SLTPResult calculate(
            double entry,
            boolean isBuy,
            List<Candle> hourCandles,
            double dAtr,
            double avgAtr,
            double adx,
            double slMult,
            double tpMult,
            int atrPeriod);

    /**
     * Result of SL/TP calculation containing distances from entry price.
     */
    class SLTPResult {
        public final double slDistance;
        public final double tpDistance;

        public SLTPResult(double slDistance, double tpDistance) {
            this.slDistance = slDistance;
            this.tpDistance = tpDistance;
        }
    }
}
