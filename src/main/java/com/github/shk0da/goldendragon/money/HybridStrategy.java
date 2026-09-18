package com.github.shk0da.goldendragon.money;

import com.github.shk0da.goldendragon.model.Candle;

import java.util.List;

/**
 * Hybrid SL/TP algorithm.
 * Combines ATR-based stop-loss with percentage-based take-profit.
 *
 * <p>SL = ATR × slMult (adapts to volatility)
 * TP = entry × tpMult / 100 (fixed percentage)
 *
 * <p>This approach uses ATR for SL to adapt to market conditions,
 * but uses percentage-based TP for predictable reward targets.
 *
 * <p>Maximum SL/TP distance is capped at 2 ATR to prevent excessive risk.
 */
public class HybridStrategy implements StopLossTakeProfitStrategy {

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
        if (entry <= 0.0 || dAtr <= 0.0 || slMult <= 0.0 || tpMult <= 0.0) {
            return null;
        }

        // ATR-based stop-loss (adapts to volatility)
        double slDist = dAtr * slMult;

        // Percentage-based take-profit (fixed target)
        double tpDist = entry * (tpMult / 100.0);

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

        // Cap SL/TP distance at 2 ATR to prevent excessive risk
        // Cap SL/TP distance at 2 ATR to prevent excessive risk
        slDist = Math.min(slDist, dAtr * MAX_ATR_MULT);
        tpDist = Math.min(tpDist, dAtr * MAX_ATR_MULT);

        if (!StopLossTakeProfitStrategy.tpDistanceCoversCommissions(entry, isBuy, commission, tpDist)) {
            return null;
        }
        return new SLTPResult(slDist, tpDist);
    }
}
