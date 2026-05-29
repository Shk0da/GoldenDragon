package com.github.shk0da.GoldenDragon.strategy.gerchik;

import com.github.shk0da.GoldenDragon.model.Candle;
import java.util.List;

/**
 * Opening range calculator for Gerchik strategy.
 * Calculates high/low of first N minutes of session.
 */
public class OpeningRangeCalculator {

    private final int openingRangeMinutes;
    private final boolean useBarCount;
    private final int openingRangeBars;

    /**
     * Create opening range calculator.
     *
     * @param openingRangeMinutes OR duration in minutes (e.g., 30)
     * @param useBarCount use bar count instead of time
     * @param openingRangeBars number of bars for OR (if useBarCount=true)
     */
    public OpeningRangeCalculator(
            int openingRangeMinutes,
            boolean useBarCount,
            int openingRangeBars) {
        this.openingRangeMinutes = openingRangeMinutes;
        this.useBarCount = useBarCount;
        this.openingRangeBars = openingRangeBars;
    }

    /**
     * Calculate opening range from candles.
     *
     * @param candles 5-minute candles
     * @return OR candle (high = OR high, low = OR low)
     */
    public Candle calculateOpeningRange(List<Candle> candles) {
        if (candles == null || candles.isEmpty()) {
            return null;
        }

        int barsToUse = useBarCount ? openingRangeBars : (openingRangeMinutes / 5);
        int startIndex = Math.max(0, candles.size() - barsToUse);

        double highestHigh = Double.MIN_VALUE;
        double lowestLow = Double.MAX_VALUE;
        double firstOpen = candles.get(startIndex).open;
        double lastClose = candles.get(candles.size() - 1).close;

        for (int i = startIndex; i < candles.size(); i++) {
            Candle c = candles.get(i);
            highestHigh = Math.max(highestHigh, c.high);
            lowestLow = Math.min(lowestLow, c.low);
        }

        return new Candle("OR", highestHigh, highestHigh, lowestLow, lastClose, 0);
    }

    /**
     * Check if opening range is complete.
     *
     * @param candles 5-minute candles
     * @return true if OR period completed
     */
    public boolean isOpeningRangeComplete(List<Candle> candles) {
        if (candles == null || candles.isEmpty()) {
            return false;
        }

        int barsToUse = useBarCount ? openingRangeBars : (openingRangeMinutes / 5);
        return candles.size() >= barsToUse;
    }

    /**
     * Get OR high level.
     */
    public double getORHigh(List<Candle> candles) {
        Candle or = calculateOpeningRange(candles);
        return or != null ? or.high : 0.0;
    }

    /**
     * Get OR low level.
     */
    public double getORLow(List<Candle> candles) {
        Candle or = calculateOpeningRange(candles);
        return or != null ? or.low : 0.0;
    }

    /**
     * Get OR range size.
     */
    public double getORRange(List<Candle> candles) {
        Candle or = calculateOpeningRange(candles);
        return or != null ? (or.high - or.low) : 0.0;
    }

    /**
     * Get opening range minutes.
     */
    public int getOpeningRangeMinutes() {
        return openingRangeMinutes;
    }
}
