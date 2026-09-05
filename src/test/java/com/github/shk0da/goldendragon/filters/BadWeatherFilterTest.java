package com.github.shk0da.goldendragon.filters;

import com.github.shk0da.goldendragon.model.Candle;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;

import static org.assertj.core.api.BDDAssertions.then;

/**
 * BDD-style tests for {@link BadWeatherFilter}.
 *
 * Filter checks in order:
 * 1. INSUFFICIENT_DATA (null or < 30 candles)
 * 2. LOW_ACTIVITY (low last candle volume < 50% avg, low ATR < 70% avg, small range < 0.5%)
 * 3. CHAOTIC_ACTIVITY (high ATR > 2x avg, large wick > 40%, panic volume > 3x avg + small body)
 * 4. POOR_LIQUIDITY (avg daily volume < 100k, or spread > 1%)
 * 5. TURBULENT_REGIME (ATR spike > 2.5x prev, panic volume > 3x avg)
 */
@DisplayName("Bad Weather Filter - Market Conditions Check")
class BadWeatherFilterTest {

    private static final BadWeatherFilter.Params DEFAULT_PARAMS = new BadWeatherFilter.Params();

    @Nested
    @DisplayName("When filter is disabled")
    class FilterDisabled {

        @Test
        @DisplayName("Should always allow trading")
        void shouldAlwaysAllowTrading_WhenFilterDisabled() {
            // Given
            BadWeatherFilter filter = new BadWeatherFilter(false);
            List<Candle> candles = buildCandles(50);

            // When
            boolean canTrade = filter.canTrade(candles, 100.0, DEFAULT_PARAMS);

            // Then
            then(canTrade).isTrue()
                    .as("Disabled filter should always allow trading");
        }
    }

    @Nested
    @DisplayName("When insufficient data")
    class InsufficientData {

        @Test
        @DisplayName("Should block trading with null candles")
        void shouldBlockTrading_WhenNullCandles() {
            // Given
            BadWeatherFilter filter = new BadWeatherFilter(true);

            // When
            boolean canTrade = filter.canTrade(null, 100.0, DEFAULT_PARAMS);
            String reason = filter.getBlockReason(null, 100.0, DEFAULT_PARAMS);

            // Then
            then(canTrade).isFalse();
            then(reason).isEqualTo("INSUFFICIENT_DATA");
        }

        @Test
        @DisplayName("Should block trading with less than 30 candles")
        void shouldBlockTrading_WhenLessThan30Candles() {
            // Given
            BadWeatherFilter filter = new BadWeatherFilter(true);
            List<Candle> candles = buildCandles(20);

            // When
            boolean canTrade = filter.canTrade(candles, 100.0, DEFAULT_PARAMS);
            String reason = filter.getBlockReason(candles, 100.0, DEFAULT_PARAMS);

            // Then
            then(canTrade).isFalse();
            then(reason).isEqualTo("INSUFFICIENT_DATA");
        }
    }

    @Nested
    @DisplayName("When low activity detected")
    class LowActivity {

        @Test
        @DisplayName("Should block trading with very low volume on last candle")
        void shouldBlockTrading_WhenLowLastCandleVolume() {
            // Given - 50 candles with avg vol 500k, but last has 10k (< 50% = 25k)
            BadWeatherFilter filter = new BadWeatherFilter(true);
            List<Candle> candles = buildCandlesWithLowLastVolume(50);

            // When
            boolean canTrade = filter.canTrade(candles, 100.0, DEFAULT_PARAMS);
            String reason = filter.getBlockReason(candles, 100.0, DEFAULT_PARAMS);

            // Then
            then(canTrade).isFalse();
            then(reason).isEqualTo("LOW_ACTIVITY");
        }
    }

    @Nested
    @DisplayName("When chaotic activity detected")
    class ChaoticActivity {

        @Test
        @DisplayName("Should block trading with large wick candle (> 40%)")
        void shouldBlockTrading_WhenLargeWickRatio() {
            // Given - candles with normal activity, last has large wick
            BadWeatherFilter filter = new BadWeatherFilter(true);
            List<Candle> candles = buildCandlesWithLargeWick(50);

            // When
            boolean canTrade = filter.canTrade(candles, 100.0, DEFAULT_PARAMS);
            String reason = filter.getBlockReason(candles, 100.0, DEFAULT_PARAMS);

            // Then
            then(canTrade).isFalse();
            then(reason).isEqualTo("CHAOTIC_ACTIVITY");
        }
    }

    @Nested
    @DisplayName("When poor liquidity detected")
    class PoorLiquidity {

        @Test
        @DisplayName("Should block trading with low average volume")
        void shouldBlockTrading_WhenLowAvgVolume() {
            // Given - avg daily volume < 100k threshold
            BadWeatherFilter filter = new BadWeatherFilter(true);
            List<Candle> candles = buildCandlesWithLowAvgVolume(50);

            // When
            boolean canTrade = filter.canTrade(candles, 100.0, DEFAULT_PARAMS);
            String reason = filter.getBlockReason(candles, 100.0, DEFAULT_PARAMS);

            // Then
            then(canTrade).isFalse();
            then(reason).isEqualTo("POOR_LIQUIDITY");
        }
    }

    @Nested
    @DisplayName("When turbulent regime detected")
    class TurbulentRegime {

        @Test
        @DisplayName("Should block trading with panic volume (> 3x avg)")
        void shouldBlockTrading_WhenPanicVolume() {
            // Given - panic volume > 3x avg, but body > 0.5% (to avoid CHAOTIC check)
            BadWeatherFilter filter = new BadWeatherFilter(true);
            List<Candle> candles = buildCandlesWithPanicVolume(50);

            // When
            boolean canTrade = filter.canTrade(candles, 100.0, DEFAULT_PARAMS);
            String reason = filter.getBlockReason(candles, 100.0, DEFAULT_PARAMS);

            // Then
            then(canTrade).isFalse();
            then(reason).isEqualTo("TURBULENT_REGIME");
        }
    }

    @Nested
    @DisplayName("When good market conditions")
    class GoodConditions {

        @Test
        @DisplayName("Should allow trading with normal candles")
        void shouldAllowTrading_WhenNormalConditions() {
            // Given - normal candles with good volume, spread < 1%, range > 0.5%
            BadWeatherFilter filter = new BadWeatherFilter(true);
            List<Candle> candles = buildNormalCandles(50);

            // When
            boolean canTrade = filter.canTrade(candles, 100.0, DEFAULT_PARAMS);
            String reason = filter.getBlockReason(candles, 100.0, DEFAULT_PARAMS);

            // Then
            then(canTrade).isTrue();
            then(reason).isNull();
        }
    }

    // ===== Helper methods =====

    /** Normal candles: avg vol 500k, spread < 1%, range > 0.5% */
    private List<Candle> buildCandles(int count) {
        return buildNormalCandles(count);
    }

    /** Good conditions: avg vol 500k, wick ratio < 0.4, spread < 1% */
    private List<Candle> buildNormalCandles(int count) {
        List<Candle> candles = new ArrayList<>();
        for (int i = 0; i < count; i++) {
            double price = 100.0;
            // Wick ratio = max(upper, lower) / range
            // Need wick < 0.4, so body should be > 60% of range
            // spread = (high-low)/close < 0.01 (1%)
            // Example: open=99.5, close=100.5, high=100.6, low=99.4
            // range = 1.2, body = 1.0, upper wick = 0.1, lower wick = 0.1
            // wickRatio = 0.1/1.2 = 0.083 < 0.4 ✓
            // spread = 1.2/100.5 = 0.012 > 0.01 ✗
            // Try: open=99.7, close=100.3, high=100.4, low=99.6
            // range = 0.8, body = 0.6, upper wick = 0.1, lower wick = 0.1
            // wickRatio = 0.1/0.8 = 0.125 < 0.4 ✓
            // spread = 0.8/100.3 = 0.008 < 0.01 ✓
            candles.add(new Candle(
                    "2024-01-01 10:00:00",
                    price - 0.3,  // open = 99.7
                    price + 0.4,  // high = 100.4
                    price - 0.4,  // low = 99.6
                    price + 0.3,  // close = 100.3
                    500000L
            ));
        }
        return candles;
    }

    /** Low last candle volume: avg vol 500k, last candle 10k (< 50% = 25k) */
    private List<Candle> buildCandlesWithLowLastVolume(int count) {
        List<Candle> candles = new ArrayList<>();
        for (int i = 0; i < count - 1; i++) {
            candles.add(new Candle(
                    "2024-01-01 10:00:00",
                    100.0, 100.5, 99.5, 100.0, 500000L
            ));
        }
        candles.add(new Candle(
                "2024-01-01 10:00:00",
                100.0, 100.5, 99.5, 100.0, 10000L // Very low volume
        ));
        return candles;
    }

    /** Large wick: last candle wickRatio > 40%, body small */
    private List<Candle> buildCandlesWithLargeWick(int count) {
        List<Candle> candles = new ArrayList<>();
        for (int i = 0; i < count - 1; i++) {
            candles.add(new Candle(
                    "2024-01-01 10:00:00",
                    100.0, 100.5, 99.5, 100.0, 500000L
            ));
        }
        // Large wick: body = |close - open|, range = high - low
        // wickRatio = max(upperWick, lowerWick) / range > 0.4
        candles.add(new Candle(
                "2024-01-01 10:00:00",
                100.0, // open
                105.0, // high
                100.0, // low (close = low → no lower wick)
                100.0, // close
                500000L // Normal volume
        ));
        return candles;
    }

    /** Low avg daily volume < 100k (minAvgDailyVolume) */
    private List<Candle> buildCandlesWithLowAvgVolume(int count) {
        List<Candle> candles = new ArrayList<>();
        for (int i = 0; i < count; i++) {
            // Same structure as good candles but lower volume
            // wickRatio < 0.4, spread < 0.01 to avoid CHAOTIC/Poor-spread checks
            candles.add(new Candle(
                    "2024-01-01 10:00:00",
                    99.7, 100.4, 99.6, 100.3, 50000L // 50k < 100k threshold
            ));
        }
        return candles;
    }

    /** Panic volume > 3x avg (> 300k when avg = 100k) */
    private List<Candle> buildCandlesWithPanicVolume(int count) {
        List<Candle> candles = new ArrayList<>();
        for (int i = 0; i < count; i++) {
            // Normal candles with avg volume = 100k
            // wickRatio < 0.4, spread < 0.01, body > 0.5% (to avoid CHAOTIC check)
            candles.add(new Candle(
                    "2024-01-01 10:00:00",
                    99.7, 100.4, 99.6, 100.3, 100000L
            ));
        }
        // Last candle: panic volume > 3x avg (300k+) but body > 0.5% to avoid CHAOTIC
        // spread < 0.01 to avoid POOR_LIQUIDITY check
        // range = 0.8, spread = 0.8/100.3 = 0.008 < 0.01 ✓
        candles.add(new Candle(
                "2024-01-01 10:00:00",
                99.9, 100.4, 99.6, 100.8, 500000L // 5x avg = panic, body = 0.9/100 = 0.9% > 0.5%
        ));
        return candles;
    }
}
