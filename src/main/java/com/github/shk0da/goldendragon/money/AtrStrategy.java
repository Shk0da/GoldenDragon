package com.github.shk0da.goldendragon.money;

import com.github.shk0da.goldendragon.model.Candle;

import java.util.List;

/**
 * ATR-based SL/TP algorithm.
 * Uses ATR (Average True Range) for volatility-adjusted stop-loss and take-profit.
 *
 * <p>SL = ATR × slMult, TP = ATR × tpMult.
 * This adapts to market volatility: wider stops in volatile markets, tighter in calm markets.
 */
public class AtrStrategy implements StopLossTakeProfitStrategy {

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
        if (entry <= 0.0 || dAtr <= 0.0 || slMult <= 0.0 || tpMult <= 0.0) {
            return null;
        }

        // Use daily ATR for SL/TP calculation
        double slDist = dAtr * slMult;
        double tpDist = dAtr * tpMult;

        // Adjust for trend strength: wider TP in strong trends
        if (adx >= 30.0) {
            slDist *= 1.10;
            tpDist *= (adx >= 45.0) ? 1.35 : 1.20;
        }

        // Adjust for range regime: tighter stops in ranging markets
        if (adx > 0.0 && adx <= 15.0) {
            slDist *= 0.90;
            tpDist *= 0.85;
        }

        return new SLTPResult(slDist, tpDist);
    }
}
