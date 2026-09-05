package com.github.shk0da.goldendragon.strategy.candle;

import com.github.shk0da.goldendragon.model.Candle;
import java.util.List;

/**
 * Market context for candle-based signal evaluation.
 * Provides access to recent candles and computed indicators.
 */
public class CandleMarketContext {
    private final List<Candle> candles;
    private final double latestClose;
    private final double latestOpen;
    private final double latestHigh;
    private final double latestLow;
    private final double latestVolume;
    private final boolean isBullish;

    public CandleMarketContext(List<Candle> candles) {
        this.candles = candles;
        if (candles != null && !candles.isEmpty()) {
            Candle latest = candles.get(candles.size() - 1);
            this.latestClose = latest.close;
            this.latestOpen = latest.open;
            this.latestHigh = latest.high;
            this.latestLow = latest.low;
            this.latestVolume = latest.volume;
            this.isBullish = latestClose >= latestOpen;
        } else {
            this.latestClose = 0.0;
            this.latestOpen = 0.0;
            this.latestHigh = 0.0;
            this.latestLow = 0.0;
            this.latestVolume = 0.0;
            this.isBullish = false;
        }
    }

    /** Get recent candles (most recent last). */
    public List<Candle> getCandles() {
        return candles;
    }

    /** Get latest candle close price. */
    public double getLatestClose() {
        return latestClose;
    }

    /** Get latest candle open price. */
    public double getLatestOpen() {
        return latestOpen;
    }

    /** Get latest candle high price. */
    public double getLatestHigh() {
        return latestHigh;
    }

    /** Get latest candle low price. */
    public double getLatestLow() {
        return latestLow;
    }

    /** Get latest candle volume. */
    public double getLatestVolume() {
        return latestVolume;
    }

    /** Check if latest candle is bullish (close >= open). */
    public boolean isBullish() {
        return isBullish;
    }

    /** Check if latest candle is bearish (close < open). */
    public boolean isBearish() {
        return !isBullish;
    }
}
