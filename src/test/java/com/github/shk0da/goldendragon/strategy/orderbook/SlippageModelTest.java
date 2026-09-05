package com.github.shk0da.goldendragon.strategy.orderbook;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.assertj.core.api.BDDAssertions.then;
import static org.assertj.core.api.BDDAssertions.thenThrownBy;

/**
 * BDD-style tests for {@link SlippageModel} predictive order-book slippage estimation.
 */
@DisplayName("Slippage Model - Predictive Slippage Estimation")
class SlippageModelTest {

    private static final double DELTA = 0.01;

    @Nested
    @DisplayName("When calculating slippage for small orders")
    class SmallOrders {

        @Test
        @DisplayName("Should return zero slippage when order fits at best level")
        void shouldReturnZeroSlippage_WhenOrderFitsAtBestLevel() {
            // Given
            SlippageModel model = new SlippageModel();
            List<Integer> depth = Arrays.asList(50, 100); // 50 at best, 100 at next

            // When
            double slippage = model.calculateSlippage(10, depth);

            // Then
            then(slippage).isEqualTo(0.0)
                    .as("10 contracts fits at best level (50 available) → 0 slippage");
        }
    }

    @Nested
    @DisplayName("When calculating slippage for medium orders")
    class MediumOrders {

        @Test
        @DisplayName("Should calculate 1 tick slippage when order spans two levels")
        void shouldCalculateOneTickSlippage_WhenOrderSpansTwoLevels() {
            // Given
            SlippageModel model = new SlippageModel();
            List<Integer> depth = Arrays.asList(50, 50);

            // When
            double slippage = model.calculateSlippage(100, depth);

            // Then
            then(slippage).isEqualTo(1.0)
                    .as("100 contracts: 50 at best + 50 at next → 1 tick slippage");
        }

        @Test
        @DisplayName("Should return zero slippage when order exactly fills best level")
        void shouldReturnZeroSlippage_WhenOrderExactlyFillsBestLevel() {
            // Given
            SlippageModel model = new SlippageModel();
            List<Integer> depth = Arrays.asList(50, 100);

            // When
            double slippage = model.calculateSlippage(50, depth);

            // Then
            then(slippage).isEqualTo(0.0)
                    .as("50 contracts exactly fill best level → 0 slippage");
        }
    }

    @Nested
    @DisplayName("When calculating slippage for large orders")
    class LargeOrders {

        @Test
        @DisplayName("Should calculate slippage when order exceeds all levels")
        void shouldCalculateSlippage_WhenOrderExceedsAllLevels() {
            // Given
            SlippageModel model = new SlippageModel();
            List<Integer> depth = Arrays.asList(50, 50, 50); // 150 total across 3 levels

            // When
            double slippage = model.calculateSlippage(500, depth);

            // Then
            then(slippage).isEqualTo(2.0)
                    .as("500 contracts: walks all 3 levels (150 total) → 2 ticks slippage");
        }

        @Test
        @DisplayName("Should cap slippage when max levels is configured")
        void shouldCapSlippage_WhenMaxLevelsReached() {
            // Given
            SlippageModel model = new SlippageModel(3); // max 3 levels
            List<Integer> depth = Arrays.asList(10, 10, 10, 10, 10); // 5 levels, 50 total

            // When
            double slippage = model.calculateSlippage(1000, depth);

            // Then
            then(slippage).isEqualTo(2.0)
                    .as("Only 3 levels considered → max 2 ticks slippage");
        }
    }

    @Nested
    @DisplayName("When handling edge cases")
    class EdgeCases {

        @Test
        @DisplayName("Should return zero slippage for zero quantity order")
        void shouldReturnZeroSlippage_ForZeroQuantity() {
            // Given
            SlippageModel model = new SlippageModel();
            List<Integer> depth = Arrays.asList(50, 100);

            // When
            double slippage = model.calculateSlippage(0, depth);

            // Then
            then(slippage).isEqualTo(0.0);
        }

        @Test
        @DisplayName("Should return zero slippage for negative quantity order")
        void shouldReturnZeroSlippage_ForNegativeQuantity() {
            // Given
            SlippageModel model = new SlippageModel();
            List<Integer> depth = Arrays.asList(50);

            // When
            double slippage = model.calculateSlippage(-5, depth);

            // Then
            then(slippage).isEqualTo(0.0);
        }

        @Test
        @DisplayName("Should return zero slippage for null depth")
        void shouldReturnZeroSlippage_ForNullDepth() {
            // Given
            SlippageModel model = new SlippageModel();

            // When
            double slippage = model.calculateSlippage(10, null);

            // Then
            then(slippage).isEqualTo(0.0);
        }

        @Test
        @DisplayName("Should return zero slippage for empty depth")
        void shouldReturnZeroSlippage_ForEmptyDepth() {
            // Given
            SlippageModel model = new SlippageModel();

            // When
            double slippage = model.calculateSlippage(10, Collections.<Integer>emptyList());

            // Then
            then(slippage).isEqualTo(0.0);
        }
    }

    @Nested
    @DisplayName("When checking fillability")
    class Fillability {

        @Test
        @DisplayName("Should be fillable when slippage within limit")
        void shouldBeFillable_WhenSlippageWithinLimit() {
            // Given/When/Then
            // 10 contracts fits at best level (0 ticks) → OK with 1 tick limit
            then(new SlippageModel().isFillable(10, Arrays.asList(50, 100), 1.0)).isTrue()
                    .as("10 contracts fits at best level (50 available) → 0 ticks, within 1 tick limit");
            
            // 100 contracts spans 2 levels (1 tick) → OK with 1 tick limit
            then(new SlippageModel().isFillable(100, Arrays.asList(50, 50), 1.0)).isTrue()
                    .as("100 contracts spans 2 levels → 1 tick, within 1 tick limit");
            
            // 500 contracts exceeds 3 levels (2 ticks) → NOT OK with 1 tick limit
            then(new SlippageModel().isFillable(500, Arrays.asList(50, 50, 50, 50), 1.0)).isFalse()
                    .as("500 contracts exceeds 3 levels → 2 ticks, exceeds 1 tick limit");
        }
    }

    @Nested
    @DisplayName("When constructing model")
    class Construction {

        @Test
        @DisplayName("Should reject zero max levels")
        void shouldRejectZeroMaxLevels() {
            // Given/When/Then
            thenThrownBy(() -> new SlippageModel(0))
                    .isInstanceOf(IllegalArgumentException.class)
                    .hasMessageContaining("maxLevels must be positive");
        }

        @Test
        @DisplayName("Should reject negative max levels")
        void shouldRejectNegativeMaxLevels() {
            // Given/When/Then
            thenThrownBy(() -> new SlippageModel(-5))
                    .isInstanceOf(IllegalArgumentException.class)
                    .hasMessageContaining("maxLevels must be positive");
        }
    }
}