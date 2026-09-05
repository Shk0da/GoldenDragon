package com.github.shk0da.goldendragon.money;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.BDDAssertions.then;

@DisplayName("Fixed Risk Position Sizing")
class FixedRiskSizingTest {

    private static final double RISK_PERCENT = 0.01; // 1%
    private static final double MAX_POSITION_SIZE = 0.25; // 25%
    private static final double BALANCE = 100_000.0;

    @Nested
    @DisplayName("When calculating position size")
    class NormalCalculation {

        @Test
        @DisplayName("Should calculate correct size with normal parameters")
        void shouldCalculateCorrectSize() {
            // Given
            FixedRiskSizing sizing = new FixedRiskSizing(RISK_PERCENT, MAX_POSITION_SIZE);
            double entry = 100.0;
            double stopLoss = 95.0; // 5% stop distance

            // When
            int size = sizing.calculateSize("TEST", entry, stopLoss, BALANCE, 0.0);

            // Then
            // riskAmount = 100_000 * 0.01 = 1_000
            // qty = 1_000 / 5 = 200
            then(size).isEqualTo(200);
        }

        @Test
        @DisplayName("Should calculate size with tight stop loss")
        void shouldCalculateSize_WithTightStop() {
            // Given
            FixedRiskSizing sizing = new FixedRiskSizing(RISK_PERCENT, MAX_POSITION_SIZE);
            double entry = 100.0;
            double stopLoss = 99.0; // 1% stop distance

            // When
            int size = sizing.calculateSize("TEST", entry, stopLoss, BALANCE, 0.0);

            // Then
            // riskAmount = 1_000, qty = 1_000 / 1 = 1_000
            // maxQtyBySize = 100_000 * 0.25 / 100 = 250 (capped!)
            then(size).isEqualTo(250);
        }

        @Test
        @DisplayName("Should calculate size with wide stop loss")
        void shouldCalculateSize_WithWideStop() {
            // Given
            FixedRiskSizing sizing = new FixedRiskSizing(RISK_PERCENT, MAX_POSITION_SIZE);
            double entry = 100.0;
            double stopLoss = 90.0; // 10% stop distance

            // When
            int size = sizing.calculateSize("TEST", entry, stopLoss, BALANCE, 0.0);

            // Then
            // riskAmount = 1_000, qty = 1_000 / 10 = 100
            then(size).isEqualTo(100);
        }
    }

    @Nested
    @DisplayName("When position size exceeds limits")
    class SizeLimits {

        @Test
        @DisplayName("Should cap by max position size")
        void shouldCapByMaxPositionSize() {
            // Given
            FixedRiskSizing sizing = new FixedRiskSizing(RISK_PERCENT, MAX_POSITION_SIZE);
            double entry = 100.0;
            double stopLoss = 99.5; // Very tight stop: 0.5%
            // riskAmount = 1_000, qty = 1_000 / 0.5 = 2_000
            // maxQtyBySize = 100_000 * 0.25 / 100 = 250

            // When
            int size = sizing.calculateSize("TEST", entry, stopLoss, BALANCE, 0.0);

            // Then
            then(size).isEqualTo(250);
        }

        @Test
        @DisplayName("Should cap by available capital")
        void shouldCapByAvailableCapital() {
            // Given
            FixedRiskSizing sizing = new FixedRiskSizing(0.5, 1.0); // 50% risk, 100% max position
            double entry = 100.0;
            double stopLoss = 90.0;
            // riskAmount = 100_000 * 0.5 = 50_000, qty = 50_000 / 10 = 5_000
            // maxQtyByCapital = 100_000 / 100 = 1_000

            // When
            int size = sizing.calculateSize("TEST", entry, stopLoss, BALANCE, 0.0);

            // Then
            then(size).isEqualTo(1_000);
        }
    }

    @Nested
    @DisplayName("When invalid parameters provided")
    class InvalidParameters {

        @Test
        @DisplayName("Should return 0 when entry <= 0")
        void shouldReturnZero_WhenEntryIsZero() {
            // Given
            FixedRiskSizing sizing = new FixedRiskSizing(RISK_PERCENT, MAX_POSITION_SIZE);

            // When
            int size = sizing.calculateSize("TEST", 0.0, 95.0, BALANCE, 0.0);

            // Then
            then(size).isZero();
        }

        @Test
        @DisplayName("Should return 0 when stopLoss <= 0")
        void shouldReturnZero_WhenStopLossIsZero() {
            // Given
            FixedRiskSizing sizing = new FixedRiskSizing(RISK_PERCENT, MAX_POSITION_SIZE);

            // When
            int size = sizing.calculateSize("TEST", 100.0, 0.0, BALANCE, 0.0);

            // Then
            then(size).isZero();
        }

        @Test
        @DisplayName("Should return 0 when balance <= 0")
        void shouldReturnZero_WhenBalanceIsZero() {
            // Given
            FixedRiskSizing sizing = new FixedRiskSizing(RISK_PERCENT, MAX_POSITION_SIZE);

            // When
            int size = sizing.calculateSize("TEST", 100.0, 95.0, 0.0, 0.0);

            // Then
            then(size).isZero();
        }

        @Test
        @DisplayName("Should return 0 when stop distance is zero")
        void shouldReturnZero_WhenStopDistanceIsZero() {
            // Given
            FixedRiskSizing sizing = new FixedRiskSizing(RISK_PERCENT, MAX_POSITION_SIZE);

            // When
            int size = sizing.calculateSize("TEST", 100.0, 100.0, BALANCE, 0.0);

            // Then
            then(size).isZero();
        }

        @Test
        @DisplayName("Should return 0 when negative parameters")
        void shouldReturnZero_WhenNegativeParameters() {
            // Given
            FixedRiskSizing sizing = new FixedRiskSizing(RISK_PERCENT, MAX_POSITION_SIZE);

            // When
            int size = sizing.calculateSize("TEST", -100.0, -95.0, -BALANCE, 0.0);

            // Then
            then(size).isZero();
        }
    }

    @Nested
    @DisplayName("When stop loss is above entry (short position)")
    class ShortPosition {

        @Test
        @DisplayName("Should calculate correct size for short")
        void shouldCalculateSize_ForShortPosition() {
            // Given
            FixedRiskSizing sizing = new FixedRiskSizing(RISK_PERCENT, MAX_POSITION_SIZE);
            double entry = 100.0;
            double stopLoss = 105.0; // Stop above entry for short

            // When
            int size = sizing.calculateSize("TEST", entry, stopLoss, BALANCE, 0.0);

            // Then
            // riskAmount = 1_000, qty = 1_000 / 5 = 200
            then(size).isEqualTo(200);
        }
    }
}
