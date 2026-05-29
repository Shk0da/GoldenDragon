package com.github.shk0da.GoldenDragon.strategy.gerchik;

import com.github.shk0da.GoldenDragon.model.Candle;
import java.util.List;

/**
 * Signal quality filter for Gerchik strategy.
 * Assesses breakout quality based on multiple factors.
 */
public class SignalQualityFilter {

    private final double minQualityThreshold;
    private final double highQualityThreshold;

    /**
     * Create signal quality filter.
     *
     * @param minQualityThreshold minimum quality to trade (e.g., 0.4)
     * @param highQualityThreshold high quality for increased size (e.g., 0.8)
     */
    public SignalQualityFilter(double minQualityThreshold, double highQualityThreshold) {
        this.minQualityThreshold = minQualityThreshold;
        this.highQualityThreshold = highQualityThreshold;
    }

    /**
     * Calculate signal quality score (0.0 to 1.0).
     *
     * @param breakoutType type of breakout
     * @param candle current candle
     * @param candles recent candles
     * @param atr current ATR
     * @return quality score
     */
    public double calculateQuality(String breakoutType, Candle candle, List<Candle> candles, double atr) {
        if (candle == null || atr <= 0) {
            return 0.0;
        }

        double quality = 0.0;

        // Breakout strength (0-0.3)
        double body = Math.abs(candle.close - candle.open);
        double range = candle.high - candle.low;
        if (range > 0) {
            double bodyRatio = body / range;
            double strength = Math.min(1.0, bodyRatio / 0.7);
            quality += strength * 0.3;
        }

        // Volume confirmation (0-0.2) - simplified, no volume data
        quality += 0.1; // Base assumption

        // Time quality (0-0.2) - morning breakouts stronger
        quality += 0.15; // Assume good timing

        // Pattern quality (0-0.3)
        if (breakoutType != null && breakoutType.contains("ORB")) {
            quality += 0.3; // ORB is high quality
        } else if (breakoutType != null && breakoutType.contains("RANGE")) {
            quality += 0.2;
        } else if (breakoutType != null && breakoutType.contains("INSIDE") || breakoutType.contains("NR")) {
            quality += 0.15;
        }

        return Math.min(1.0, quality);
    }

    /**
     * Check if quality is sufficient for trading.
     *
     * @param quality quality score
     * @return true if meets minimum threshold
     */
    public boolean isQualitySufficient(double quality) {
        return quality >= minQualityThreshold;
    }

    /**
     * Check if quality is high (for position size adjustment).
     *
     * @param quality quality score
     * @return true if high quality
     */
    public boolean isHighQuality(double quality) {
        return quality >= highQualityThreshold;
    }

    /**
     * Get minimum quality threshold.
     */
    public double getMinQualityThreshold() {
        return minQualityThreshold;
    }

    /**
     * Get high quality threshold.
     */
    public double getHighQualityThreshold() {
        return highQualityThreshold;
    }
}
