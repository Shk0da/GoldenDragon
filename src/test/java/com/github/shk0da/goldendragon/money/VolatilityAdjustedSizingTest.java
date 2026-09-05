package com.github.shk0da.goldendragon.money;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.BDDAssertions.then;

@DisplayName("Volatility-Adjusted Position Sizing")
class VolatilityAdjustedSizingTest {

    private static final double RISK_PERCENT = 0.01; // 1%
    private static final double BASE_VOLATILITY = 2.0; // Base ATR
    private static final double MIN_ADJUSTMENT = 0.5;
    private static final double MAX_ADJUSTMENT = 1.5;
    private static final double MAX_POSITION_SIZE = 0.25; // 25%
    private static final double BALANCE = 100_000.0;

    @Nested
    @DisplayName("When volatility is normal")
    class NormalVolatility {

        @Test
        @DisplayName("Should use base risk when ATR equals base volatility")
        void shouldUseBaseRisk_WhenAtrEqualsBase() {
            // Given
            VolatilityAdjustedSizing sizing = new VolatilityAdjustedSizing(
                    RISK_PERCENT, BASE_VOLATILITY, MIN_ADJUSTMENT, MAX_ADJUSTMENT, MAX_POSITION_SIZE);
            double entry = 100.0;
            double stopLoss = 95.0;
            double atr = BASE_VOLATILITY; // ATR = base

            // When
            int size = sizing.calculateSize("TEST", entry, stopLoss, BALANCE, atr);

            // Then
            // volatilityAdjustment = 2.0 / 2.0 = 1.0
            // riskAmount = 100_000 * 0.01 * 1.0 = 1_000
            // qty = 1_000 / 5 = 200
            then(size).isEqualTo(200);
        }
    }

    @Nested
    @DisplayName("When volatility is low")
    class LowVolatility {

        @Test
        @DisplayName("Should increase position size when ATR is below base")
        void shouldIncreaseSize_WhenAtrIsLow() {
            // Given
            VolatilityAdjustedSizing sizing = new VolatilityAdjustedSizing(
                    RISK_PERCENT, BASE_VOLATILITY, MIN_ADJUSTMENT, MAX_ADJUSTMENT, MAX_POSITION_SIZE);
            double entry = 100.0;
            double stopLoss = 95.0;
            double atr = 1.0; // Low volatility (half of base)

            // When
            int size = sizing.calculateSize("TEST", entry, stopLoss, BALANCE, atr);

            // Then
            // volatilityAdjustment = 2.0 / 1.0 = 2.0, clamped to 1.5
            // riskAmount = 100_000 * 0.01 * 1.5 = 1_500
            // qty = 1_500 / 5 = 300
            // maxQtyBySize = 100_000 * 0.25 / 100 = 250 (capped!)
            then(size).isEqualTo(250);
        }

        @Test
        @DisplayName("Should apply max adjustment cap")
        void shouldApplyMaxAdjustmentCap() {
            // Given
            VolatilityAdjustedSizing sizing = new VolatilityAdjustedSizing(
                    RISK_PERCENT, BASE_VOLATILITY, MIN_ADJUSTMENT, MAX_ADJUSTMENT, MAX_POSITION_SIZE);
            double entry = 100.0;
            double stopLoss = 98.0;
            double atr = 0.5; // Very low volatility

            // When
            int size = sizing.calculateSize("TEST", entry, stopLoss, BALANCE, atr);

            // Then
            // volatilityAdjustment = 2.0 / 0.5 = 4.0, clamped to 1.5
            // riskAmount = 100_000 * 0.01 * 1.5 = 1_500
            // qty = 1_500 / 2 = 750
            // maxQtyBySize = 100_000 * 0.25 / 100 = 250 (capped!)
            then(size).isEqualTo(250);
        }
    }

    @Nested
    @DisplayName("When volatility is high")
    class HighVolatility {

        @Test
        @DisplayName("Should reduce position size when ATR is above base")
        void shouldReduceSize_WhenAtrIsHigh() {
            // Given
            VolatilityAdjustedSizing sizing = new VolatilityAdjustedSizing(
                    RISK_PERCENT, BASE_VOLATILITY, MIN_ADJUSTMENT, MAX_ADJUSTMENT, MAX_POSITION_SIZE);
            double entry = 100.0;
            double stopLoss = 95.0;
            double atr = 4.0; // High volatility (2x base)

            // When
            int size = sizing.calculateSize("TEST", entry, stopLoss, BALANCE, atr);

            // Then
            // volatilityAdjustment = 2.0 / 4.0 = 0.5
            // riskAmount = 100_000 * 0.01 * 0.5 = 500
            // qty = 500 / 5 = 100
            then(size).isEqualTo(100);
        }

        @Test
        @DisplayName("Should apply min adjustment floor")
        void shouldApplyMinAdjustmentFloor() {
            // Given
            VolatilityAdjustedSizing sizing = new VolatilityAdjustedSizing(
                    RISK_PERCENT, BASE_VOLATILITY, MIN_ADJUSTMENT, MAX_ADJUSTMENT, MAX_POSITION_SIZE);
            double entry = 100.0;
            double stopLoss = 95.0;
            double atr = 10.0; // Very high volatility

            // When
            int size = sizing.calculateSize("TEST", entry, stopLoss, BALANCE, atr);

            // Then
            // volatilityAdjustment = 2.0 / 10.0 = 0.2, clamped to 0.5
            // riskAmount = 100_000 * 0.01 * 0.5 = 500
            // qty = 500 / 5 = 100
            then(size).isEqualTo(100);
        }
    }

    @Nested
    @DisplayName("When invalid parameters provided")
    class InvalidParameters {

        @Test
        @DisplayName("Should return 0 when entry <= 0")
        void shouldReturnZero_WhenEntryIsZero() {
            // Given
            VolatilityAdjustedSizing sizing = new VolatilityAdjustedSizing(
                    RISK_PERCENT, BASE_VOLATILITY, MIN_ADJUSTMENT, MAX_ADJUSTMENT, MAX_POSITION_SIZE);

            // When
            int size = sizing.calculateSize("TEST", 0.0, 95.0, BALANCE, 2.0);

            // Then
            then(size).isZero();
        }

        @Test
        @DisplayName("Should return 0 when stopLoss <= 0")
        void shouldReturnZero_WhenStopLossIsZero() {
            // Given
            VolatilityAdjustedSizing sizing = new VolatilityAdjustedSizing(
                    RISK_PERCENT, BASE_VOLATILITY, MIN_ADJUSTMENT, MAX_ADJUSTMENT, MAX_POSITION_SIZE);

            // When
            int size = sizing.calculateSize("TEST", 100.0, 0.0, BALANCE, 2.0);

            // Then
            then(size).isZero();
        }

        @Test
        @DisplayName("Should return 0 when balance <= 0")
        void shouldReturnZero_WhenBalanceIsZero() {
            // Given
            VolatilityAdjustedSizing sizing = new VolatilityAdjustedSizing(
                    RISK_PERCENT, BASE_VOLATILITY, MIN_ADJUSTMENT, MAX_ADJUSTMENT, MAX_POSITION_SIZE);

            // When
            int size = sizing.calculateSize("TEST", 100.0, 95.0, 0.0, 2.0);

            // Then
            then(size).isZero();
        }

        @Test
        @DisplayName("Should return 0 when ATR <= 0")
        void shouldReturnZero_WhenAtrIsZero() {
            // Given
            VolatilityAdjustedSizing sizing = new VolatilityAdjustedSizing(
                    RISK_PERCENT, BASE_VOLATILITY, MIN_ADJUSTMENT, MAX_ADJUSTMENT, MAX_POSITION_SIZE);

            // When
            int size = sizing.calculateSize("TEST", 100.0, 95.0, BALANCE, 0.0);

            // Then
            then(size).isZero();
        }

        @Test
        @DisplayName("Should return 0 when stop distance is zero")
        void shouldReturnZero_WhenStopDistanceIsZero() {
            // Given
            VolatilityAdjustedSizing sizing = new VolatilityAdjustedSizing(
                    RISK_PERCENT, BASE_VOLATILITY, MIN_ADJUSTMENT, MAX_ADJUSTMENT, MAX_POSITION_SIZE);

            // When
            int size = sizing.calculateSize("TEST", 100.0, 100.0, BALANCE, 2.0);

            // Then
            then(size).isZero();
        }
    }

    @Nested
    @DisplayName("When position size exceeds limits")
    class SizeLimits {

        @Test
        @DisplayName("Should cap by max position size")
        void shouldCapByMaxPositionSize() {
            // Given
            VolatilityAdjustedSizing sizing = new VolatilityAdjustedSizing(
                    0.05, BASE_VOLATILITY, MIN_ADJUSTMENT, MAX_ADJUSTMENT, MAX_POSITION_SIZE); // 5% risk
            double entry = 100.0;
            double stopLoss = 99.0;
            double atr = 1.0; // Low vol, adjustment = 2.0 clamped to 1.5

            // When
            int size = sizing.calculateSize("TEST", entry, stopLoss, BALANCE, atr);

            // Then
            // riskAmount = 100_000 * 0.05 * 1.5 = 7_500
            // qty = 7_500 / 1 = 7_500
            // maxQtyBySize = 100_000 * 0.25 / 100 = 250 (capped!)
            then(size).isEqualTo(250);
        }
    }
}
