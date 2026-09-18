package com.github.shk0da.goldendragon.money;

import com.github.shk0da.goldendragon.model.Candle;

import java.util.List;

/**
 * Current baseline percentage-based SL/TP algorithm.
 * Uses fixed percentage of entry price for SL and TP distances.
 *
 * <p>Config: slMult and tpMult from UnifiedTraderConfig.TickerParams.
 *
 * <p>Maximum SL/TP distance is capped at 2 ATR to prevent excessive risk when ATR is low.
 */
public class PercentageStrategy implements StopLossTakeProfitStrategy {

    private static final double MAX_ATR_MULT = 2.0;

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
        if (entry <= 0.0 || slMult <= 0.0 || tpMult <= 0.0) {
            return null;
        }

        double stopPct = slMult / 100.0;
        double takeProfitPct = tpMult / 100.0;
        double slDist = entry * stopPct;
        double tpDist = entry * takeProfitPct;

        // Adjust for strong trend (ADX >= 30)
        if (adx >= 30.0) {
            slDist *= 1.10;
            tpDist *= (adx >= 45.0) ? 1.35 : 1.20;
        }

        // Adjust for range regime (ADX <= 15)
        if (adx > 0.0 && adx <= 15.0) {
            slDist *= 0.90;
            tpDist *= 0.85;
        }

        slDist = Math.min(slDist, dAtr * MAX_ATR_MULT);
        tpDist = Math.min(tpDist, dAtr * MAX_ATR_MULT);

        return new SLTPResult(slDist, tpDist);
    }
}
