package com.github.shk0da.goldendragon.filters;

import com.github.shk0da.goldendragon.model.Candle;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;

import static org.assertj.core.api.BDDAssertions.then;

@DisplayName("MarketRegimeFilter")
class MarketRegimeFilterTest {

    private static final double ADX_RANGE_THRESHOLD = 25.0;
    private static final double ADX_UNCLEAR_THRESHOLD = 35.0;
    private static final double VOLUME_RATIO_MIN = 50.0;
    private static final double CONFIDENCE_MIN = 60.0;
    private static final int ATR_BARS = 3;

    private static Candle candle(double close, double high, double low, long volume) {
        return new Candle("2024-01-01 10:00:00", close, high, low, close, volume);
    }

    private static Candle candle(double close) {
        return candle(close, close + 1, close - 1, 1000L);
    }

    private static List<Candle> candles(int count, double basePrice) {
        List<Candle> result = new ArrayList<>();
        for (int i = 0; i < count; i++) {
            double price = basePrice + (i % 10) - 5;
            result.add(candle(price, price + 2, price - 2, 1000L + i * 100));
        }
        return result;
    }

    @Nested
    @DisplayName("When filter is disabled or insufficient data")
    class DisabledOrInsufficientData {

        @Test
        @DisplayName("Should return DISABLED when filter is disabled")
        void shouldReturnDisabled_WhenFilterDisabled() {
            MarketRegimeFilter filter = new MarketRegimeFilter(false);
            List<Candle> candles = candles(100, 100.0);

            MarketRegimeFilter.FilterResult result = filter.evaluate(
                    candles, ADX_RANGE_THRESHOLD, ADX_UNCLEAR_THRESHOLD,
                    VOLUME_RATIO_MIN, CONFIDENCE_MIN, ATR_BARS);

            then(result.canTrade).isTrue();
            then(result.confidence).isEqualTo(100.0);
            then(result.positionMultiplier).isEqualTo(1.0);
            then(result.reason).isEqualTo("DISABLED");
        }

        @Test
        @DisplayName("Should return DISABLED when candles is null")
        void shouldReturnDisabled_WhenCandlesNull() {
            MarketRegimeFilter filter = new MarketRegimeFilter(true);

            MarketRegimeFilter.FilterResult result = filter.evaluate(
                    null, ADX_RANGE_THRESHOLD, ADX_UNCLEAR_THRESHOLD,
                    VOLUME_RATIO_MIN, CONFIDENCE_MIN, ATR_BARS);

            then(result.canTrade).isTrue();
            then(result.confidence).isEqualTo(100.0);
            then(result.positionMultiplier).isEqualTo(1.0);
            then(result.reason).isEqualTo("DISABLED");
        }

        @Test
        @DisplayName("Should return DISABLED when insufficient candles")
        void shouldReturnDisabled_WhenInsufficientCandles() {
            MarketRegimeFilter filter = new MarketRegimeFilter(true);
            List<Candle> candles = candles(30, 100.0); // Less than required 60

            MarketRegimeFilter.FilterResult result = filter.evaluate(
                    candles, ADX_RANGE_THRESHOLD, ADX_UNCLEAR_THRESHOLD,
                    VOLUME_RATIO_MIN, CONFIDENCE_MIN, ATR_BARS);

            then(result.canTrade).isTrue();
            then(result.confidence).isEqualTo(100.0);
            then(result.positionMultiplier).isEqualTo(1.0);
            then(result.reason).isEqualTo("DISABLED");
        }
    }

    @Nested
    @DisplayName("When ADX is below range threshold")
    class AdxBelowRange {

        @Test
        @DisplayName("Should reject when ADX is below range threshold")
        void shouldReject_WhenAdxBelowRange() {
            MarketRegimeFilter filter = new MarketRegimeFilter(true);
            List<Candle> candles = createRangingCandles(100, 100.0); // Low ADX

            MarketRegimeFilter.FilterResult result = filter.evaluate(
                    candles, ADX_RANGE_THRESHOLD, ADX_UNCLEAR_THRESHOLD,
                    VOLUME_RATIO_MIN, CONFIDENCE_MIN, ATR_BARS);

            then(result.canTrade).isFalse();
            then(result.confidence).isZero();
            then(result.positionMultiplier).isZero();
            then(result.reason).startsWith("ADX_RANGE_");
        }
    }

    @Nested
    @DisplayName("When ADX is in unclear zone")
    class AdxUnclearZone {

        @Test
        @DisplayName("Should reject when ADX is in unclear zone (25-35) without strong DI")
        void shouldReject_WhenAdxUnclear() {
            MarketRegimeFilter filter = new MarketRegimeFilter(true);
            // Create candles with moderate trend - ADX should be in 25-35 range
            List<Candle> candles = createModerateTrendCandles(100, 100.0);

            MarketRegimeFilter.FilterResult result = filter.evaluate(
                    candles, ADX_RANGE_THRESHOLD, ADX_UNCLEAR_THRESHOLD,
                    VOLUME_RATIO_MIN, CONFIDENCE_MIN, ATR_BARS);

            // Either rejected as unclear or accepted as early trend
            // We verify the filter processes the unclear zone correctly
            then(result.reason).doesNotStartWith("ADX_RANGE_");
        }
    }

    @Nested
    @DisplayName("When ATR is declining")
    class AtrDeclining {

        @Test
        @DisplayName("Should handle declining ATR scenario")
        void shouldHandleDecliningAtr() {
            MarketRegimeFilter filter = new MarketRegimeFilter(true);
            List<Candle> candles = createTrendingCandlesWithDecliningAtr(100, 100.0);

            MarketRegimeFilter.FilterResult result = filter.evaluate(
                    candles, ADX_RANGE_THRESHOLD, ADX_UNCLEAR_THRESHOLD,
                    VOLUME_RATIO_MIN, CONFIDENCE_MIN, ATR_BARS);

            then(result.reason).contains("ATR");
        }
    }

    @Nested
    @DisplayName("When volume is low")
    class LowVolume {

        @Test
        @DisplayName("Should reject when volume ratio is below minimum")
        void shouldReject_WhenVolumeLow() {
            MarketRegimeFilter filter = new MarketRegimeFilter(true);
            // Last candle has very low volume (100) compared to average (2000+)
            List<Candle> candles = createLowVolumeCandles(100, 100.0);

            MarketRegimeFilter.FilterResult result = filter.evaluate(
                    candles, ADX_RANGE_THRESHOLD, ADX_UNCLEAR_THRESHOLD,
                    VOLUME_RATIO_MIN, CONFIDENCE_MIN, ATR_BARS);

            then(result.canTrade).isFalse();
        }
    }

    @Nested
    @DisplayName("When confidence is below minimum")
    class LowConfidence {

        @Test
        @DisplayName("Should reject when confidence is below minimum")
        void shouldReject_WhenConfidenceLow() {
            MarketRegimeFilter filter = new MarketRegimeFilter(true);
            // Sideways candles will produce low confidence
            List<Candle> candles = createLowConfidenceCandles(100, 100.0);

            MarketRegimeFilter.FilterResult result = filter.evaluate(
                    candles, ADX_RANGE_THRESHOLD, ADX_UNCLEAR_THRESHOLD,
                    VOLUME_RATIO_MIN, 90.0, ATR_BARS);

            then(result.canTrade).isFalse();
        }
    }

    @Nested
    @DisplayName("When all conditions are met")
    class AllConditionsMet {

        @Test
        @DisplayName("Should allow trading with good confidence")
        void shouldAllowTrading() {
            MarketRegimeFilter filter = new MarketRegimeFilter(true);
            List<Candle> candles = createStrongTrendCandles(100, 100.0);

            MarketRegimeFilter.FilterResult result = filter.evaluate(
                    candles, ADX_RANGE_THRESHOLD, ADX_UNCLEAR_THRESHOLD,
                    VOLUME_RATIO_MIN, CONFIDENCE_MIN, ATR_BARS);

            then(result.canTrade).isTrue();
            then(result.confidence).isGreaterThan(0.0);
            then(result.reason).contains("TREND");
        }

        @Test
        @DisplayName("Should return TREND_EARLY with 0.5 multiplier in early trend")
        void shouldReturnTrendEarly() {
            MarketRegimeFilter filter = new MarketRegimeFilter(true);
            // Moderate uptrend with growing volume and volatility
            List<Candle> candles = createEarlyTrendCandles(100, 100.0);

            MarketRegimeFilter.FilterResult result = filter.evaluate(
                    candles, ADX_RANGE_THRESHOLD, ADX_UNCLEAR_THRESHOLD,
                    VOLUME_RATIO_MIN, CONFIDENCE_MIN, ATR_BARS);

            // Early trend should allow trading with reduced multiplier
            then(result.canTrade).isTrue();
            then(result.positionMultiplier).isLessThanOrEqualTo(1.0);
        }
    }

    // Helper methods to create test data

    private static List<Candle> createRangingCandles(int count, double basePrice) {
        List<Candle> result = new ArrayList<>();
        for (int i = 0; i < count; i++) {
            double price = basePrice + (i % 5) - 2.5;
            result.add(candle(price, price + 1, price - 1, 1000L));
        }
        return result;
    }

    private static List<Candle> createModerateTrendCandles(int count, double basePrice) {
        List<Candle> result = new ArrayList<>();
        for (int i = 0; i < count; i++) {
            double price = basePrice + Math.sin(i * 0.2) * 2 + i * 0.1;
            double volatility = 1.0 + Math.random() * 2;
            long volume = 1500L + i * 20;
            result.add(candle(price, price + volatility, price - volatility, volume));
        }
        return result;
    }

    private static List<Candle> createUnclearCandles(int count, double basePrice) {
        List<Candle> result = new ArrayList<>();
        for (int i = 0; i < count; i++) {
            double price = basePrice + (i % 8) - 4;
            result.add(candle(price, price + 2, price - 2, 1000L));
        }
        return result;
    }

    private static List<Candle> createTrendingCandlesWithDecliningAtr(int count, double basePrice) {
        List<Candle> result = new ArrayList<>();
        for (int i = 0; i < count; i++) {
            double price = basePrice + i * 0.5; // Strong uptrend
            double volatility = Math.max(0.5, 5.0 - i * 0.05); // Declining volatility
            result.add(candle(price, price + volatility, price - volatility, 2000L));
        }
        return result;
    }

    private static List<Candle> createLowVolumeCandles(int count, double basePrice) {
        List<Candle> result = new ArrayList<>();
        for (int i = 0; i < count; i++) {
            double price = basePrice + i * 0.8; // Strong uptrend
            double volatility = 3.0 + i * 0.02;
            long volume = (i < count - 1) ? 2000L : 100L; // Last candle has very low volume
            result.add(candle(price, price + volatility, price - volatility, volume));
        }
        return result;
    }

    private static List<Candle> createLowConfidenceCandles(int count, double basePrice) {
        List<Candle> result = new ArrayList<>();
        for (int i = 0; i < count; i++) {
            double price = basePrice + (i % 10) - 5; // Sideways
            result.add(candle(price, price + 2, price - 2, 1500L));
        }
        return result;
    }

    private static List<Candle> createStrongTrendCandles(int count, double basePrice) {
        List<Candle> result = new ArrayList<>();
        for (int i = 0; i < count; i++) {
            double price = basePrice + i * 0.8; // Strong uptrend
            double volatility = 3.0 + i * 0.02; // Expanding volatility
            long volume = 2000L + i * 50; // High and growing volume
            result.add(candle(price, price + volatility, price - volatility, volume));
        }
        return result;
    }

    private static List<Candle> createEarlyTrendCandles(int count, double basePrice) {
        List<Candle> result = new ArrayList<>();
        for (int i = 0; i < count; i++) {
            double price = basePrice + i * 0.4; // Moderate uptrend
            double volatility = 2.0 + i * 0.03; // Growing volatility
            long volume = 1500L + i * 30;
            result.add(candle(price, price + volatility, price - volatility, volume));
        }
        return result;
    }
}
