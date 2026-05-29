package com.github.shk0da.GoldenDragon.strategy.gerchik;

import com.github.shk0da.GoldenDragon.model.Candle;
import java.util.List;

/**
 * Range breakout detector for Gerchik strategy.
 * Detects ORB, narrow range, and inside bar breakouts.
 */
public class RangeBreakoutDetector {

    private final double breakoutBufferAtr;
    private final int breakoutLookback;
    private final boolean useInsideBar;
    private final boolean useNarrowRange;

    /**
     * Create range breakout detector.
     *
     * @param breakoutBufferAtr breakout buffer in ATR (e.g., 0.5)
     * @param breakoutLookback lookback for range (e.g., 20)
     * @param useInsideBar detect inside bar breakouts
     * @param useNarrowRange detect narrow range breakouts
     */
    public RangeBreakoutDetector(
            double breakoutBufferAtr,
            int breakoutLookback,
            boolean useInsideBar,
            boolean useNarrowRange) {
        this.breakoutBufferAtr = breakoutBufferAtr;
        this.breakoutLookback = breakoutLookback;
        this.useInsideBar = useInsideBar;
        this.useNarrowRange = useNarrowRange;
    }

    /**
     * Detect breakout signal.
     *
     * @param candles 5-minute candles
     * @param orCandle opening range candle
     * @param atr current ATR
     * @param currentPrice current price
     * @return breakout signal string or null
     */
    public String detectBreakout(List<Candle> candles, Candle orCandle, double atr, double currentPrice) {
        if (candles == null || candles.isEmpty() || atr <= 0) {
            return null;
        }

        // Check Opening Range Breakout
        if (orCandle != null) {
            double orHigh = orCandle.high;
            double orLow = orCandle.low;
            double buffer = atr * breakoutBufferAtr;

            if (currentPrice > (orHigh + buffer)) {
                return "GERCHIK_ORB_LONG_STRONG";
            }
            if (currentPrice < (orLow - buffer)) {
                return "GERCHIK_ORB_SHORT_STRONG";
            }
        }

        // Check Previous Range Breakout
        if (candles.size() >= breakoutLookback) {
            double[] rangeLevels = calculateRangeLevels(candles, breakoutLookback);
            double rangeHigh = rangeLevels[0];
            double rangeLow = rangeLevels[1];
            double buffer = atr * breakoutBufferAtr;

            if (currentPrice > (rangeHigh + buffer)) {
                return "GERCHIK_RANGE_LONG";
            }
            if (currentPrice < (rangeLow - buffer)) {
                return "GERCHIK_RANGE_SHORT";
            }
        }

        // Check Inside Bar Breakout
        if (useInsideBar && candles.size() >= 3) {
            String insideBarSignal = detectInsideBarBreakout(candles, atr);
            if (insideBarSignal != null) {
                return insideBarSignal;
            }
        }

        // Check Narrow Range Breakout
        if (useNarrowRange && candles.size() >= 7) {
            String nrSignal = detectNarrowRangeBreakout(candles, atr);
            if (nrSignal != null) {
                return nrSignal;
            }
        }

        return null;
    }

    /**
     * Calculate range levels (high and low) over lookback period.
     */
    private double[] calculateRangeLevels(List<Candle> candles, int lookback) {
        double highestHigh = Double.MIN_VALUE;
        double lowestLow = Double.MAX_VALUE;

        int startIndex = Math.max(0, candles.size() - lookback);
        for (int i = startIndex; i < candles.size(); i++) {
            Candle c = candles.get(i);
            highestHigh = Math.max(highestHigh, c.high);
            lowestLow = Math.min(lowestLow, c.low);
        }

        return new double[]{highestHigh, lowestLow};
    }

    /**
     * Detect inside bar breakout.
     */
    private String detectInsideBarBreakout(List<Candle> candles, double atr) {
        Candle current = candles.get(candles.size() - 1);
        Candle previous = candles.get(candles.size() - 2);

        // Check if current is inside bar
        boolean isInsideBar = current.high <= previous.high && current.low >= previous.low;

        if (!isInsideBar) {
            return null;
        }

        // Check breakout direction (would need next candle, so check current momentum)
        double body = Math.abs(current.close - current.open);
        double range = current.high - current.low;

        if (body > range * 0.7) {
            if (current.close > current.open) {
                return "GERCHIK_INSIDE_BAR_LONG";
            } else {
                return "GERCHIK_INSIDE_BAR_SHORT";
            }
        }

        return null;
    }

    /**
     * Detect narrow range breakout.
     */
    private String detectNarrowRangeBreakout(List<Candle> candles, double atr) {
        Candle current = candles.get(candles.size() - 1);
        double currentRange = current.high - current.low;

        // Check if current range is smallest of last N bars
        int lookback = Math.min(7, candles.size() - 1);
        boolean isNarrowest = true;

        for (int i = 1; i <= lookback; i++) {
            Candle c = candles.get(candles.size() - 1 - i);
            double range = c.high - c.low;
            if (range <= currentRange) {
                isNarrowest = false;
                break;
            }
        }

        if (!isNarrowest) {
            return null;
        }

        // Check breakout direction
        if (current.close > current.open) {
            return "GERCHIK_NR7_LONG";
        } else {
            return "GERCHIK_NR7_SHORT";
        }
    }

    /**
     * Get breakout buffer in ATR.
     */
    public double getBreakoutBufferAtr() {
        return breakoutBufferAtr;
    }
}
