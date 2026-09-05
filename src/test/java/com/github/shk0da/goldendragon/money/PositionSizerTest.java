package com.github.shk0da.goldendragon.money;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.BDDAssertions.then;

@DisplayName("Position Sizer")
class PositionSizerTest {

    private static final double BALANCE = 100_000.0;

    // Dummy sizing strategy for isolated PositionSizer testing
    private static final class StubSizingStrategy implements SizingStrategy {
        private final int result;

        StubSizingStrategy(int result) {
            this.result = result;
        }

        @Override
        public int calculateSize(
                String ticker, double entry, double stopLoss, double balance, double atr) {
            return result;
        }
    }

    @Nested
    @DisplayName("When applying lot step rounding")
    class LotStepRounding {

        @Test
        @DisplayName("Should round down to lot step")
        void shouldRoundDownToLotStep() {
            // Given
            SizingStrategy strategy = new StubSizingStrategy(95);
            PositionSizer sizer = new PositionSizer(strategy, 1, 10);

            // When
            int size = sizer.calculateSize("TEST", 100.0, 95.0, BALANCE, 0.0);

            // Then
            // 95 / 10 * 10 = 90
            then(size).isEqualTo(90);
        }

        @Test
        @DisplayName("Should return exact value when already a multiple of lot step")
        void shouldReturnExactValue_WhenAlreadyMultiple() {
            // Given
            SizingStrategy strategy = new StubSizingStrategy(100);
            PositionSizer sizer = new PositionSizer(strategy, 1, 10);

            // When
            int size = sizer.calculateSize("TEST", 100.0, 95.0, BALANCE, 0.0);

            // Then
            then(size).isEqualTo(100);
        }

        @Test
        @DisplayName("Should use minLotSize = 1 by default")
        void shouldUseMinLotSizeOne_WhenDefaultConstructor() {
            // Given
            SizingStrategy strategy = new StubSizingStrategy(42);
            PositionSizer sizer = new PositionSizer(strategy);

            // When
            int size = sizer.calculateSize("TEST", 100.0, 95.0, BALANCE, 0.0);

            // Then
            then(size).isEqualTo(42);
        }
    }

    @Nested
    @DisplayName("When size is below minimum lot")
    class BelowMinimumLot {

        @Test
        @DisplayName("Should return 0 when raw size is below minLotSize")
        void shouldReturnZero_WhenBelowMinLotSize() {
            // Given
            SizingStrategy strategy = new StubSizingStrategy(3);
            PositionSizer sizer = new PositionSizer(strategy, 5, 1);

            // When
            int size = sizer.calculateSize("TEST", 100.0, 95.0, BALANCE, 0.0);

            // Then
            then(size).isZero();
        }

        @Test
        @DisplayName("Should return 0 when raw size equals zero")
        void shouldReturnZero_WhenRawSizeIsZero() {
            // Given
            SizingStrategy strategy = new StubSizingStrategy(0);
            PositionSizer sizer = new PositionSizer(strategy);

            // When
            int size = sizer.calculateSize("TEST", 100.0, 95.0, BALANCE, 0.0);

            // Then
            then(size).isZero();
        }
    }

    @Nested
    @DisplayName("When size equals minimum lot")
    class AtMinimumLot {

        @Test
        @DisplayName("Should allow size equal to minLotSize")
        void shouldAllowSize_EqualToMinLotSize() {
            // Given
            SizingStrategy strategy = new StubSizingStrategy(5);
            PositionSizer sizer = new PositionSizer(strategy, 5, 1);

            // When
            int size = sizer.calculateSize("TEST", 100.0, 95.0, BALANCE, 0.0);

            // Then
            then(size).isEqualTo(5);
        }
    }

    @Nested
    @DisplayName("When combining min lot and lot step")
    class MinLotAndStep {

        @Test
        @DisplayName("Should apply min lot check first, then round to step")
        void shouldApplyMinLotThenRoundToStep() {
            // Given
            SizingStrategy strategy = new StubSizingStrategy(17);
            PositionSizer sizer = new PositionSizer(strategy, 10, 5);

            // When
            int size = sizer.calculateSize("TEST", 100.0, 95.0, BALANCE, 0.0);

            // Then
            // 17 >= 10 (min lot), 17 / 5 * 5 = 15
            then(size).isEqualTo(15);
        }

        @Test
        @DisplayName("Should round to zero when below min lot after rounding")
        void shouldHandleRawBelowMinLot() {
            // Given
            SizingStrategy strategy = new StubSizingStrategy(7);
            PositionSizer sizer = new PositionSizer(strategy, 10, 5);

            // When
            int size = sizer.calculateSize("TEST", 100.0, 95.0, BALANCE, 0.0);

            // Then
            then(size).isZero();
        }
    }
}