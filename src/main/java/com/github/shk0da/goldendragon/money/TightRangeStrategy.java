package com.github.shk0da.goldendragon.money;

import com.github.shk0da.goldendragon.model.Candle;

import java.util.List;

/**
 * Tight-range SL/TP algorithm.
 * Uses very short stops and take-profits, capped at a small percentage of entry price.
 *
 * <p>SL and TP distances are both kept <= maxDistPercent (default 0.4%) measured from entry,
 * targeting quick, high-frequency scalps. ATR is only used as a floor when price volatility is
 * so low that a sub-tick stop is meaningless.
 *
 * <p>Config: slMult = SL percent (0.4 = 0.4%), tpMult relative to slMult for R:R (e.g. 1.0 => 1:1,
 * 1.5 => 1:1.5).
 */
public class TightRangeStrategy implements StopLossTakeProfitStrategy {

    private static final double MAX_SL_PERCENT = 0.004;
    private static final double ATR_FLOOR = 0.25;

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
        if (entry <= 0.0 || dAtr <= 0.0) {
            return null;
        }

        double slPercent = Math.max(0.001, Math.min(MAX_SL_PERCENT, slMult / 100.0));
        double rr = Math.max(0.5, tpMult);

        // SL is a strict percentage of entry, capped at max
        double slDist = entry * slPercent;

        // Ensure stop is at least a fraction of ATR to avoid being triggered by single-tick noise
        double atrFloor = dAtr * ATR_FLOOR;
        slDist = Math.max(slDist, atrFloor);

        // TP has fixed multiple of SL distance, still tight by construction
        double tpDist = slDist * rr;

        // Extremely tight cap on TP too (never more than small % from entry)
        double maxTpDist = entry * (MAX_SL_PERCENT * 2.0);
        tpDist = Math.min(tpDist, maxTpDist);

        return new SLTPResult(slDist, tpDist);
    }
}