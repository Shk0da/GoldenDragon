package com.github.shk0da.goldendragon.strategy.orderbook;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.BDDAssertions.then;
import static org.assertj.core.api.BDDAssertions.thenThrownBy;

/**
 * BDD-style tests for {@link DailyLossLimit} circuit breaker.
 */
@DisplayName("Daily Loss Limit - Circuit Breaker")
class DailyLossLimitTest {

    @Nested
    @DisplayName("When trading within loss limit")
    class WithinLimit {

        @Test
        @DisplayName("Should allow trading when within daily loss limit")
        void shouldAllowTrading_WhenWithinDailyLimit() {
            // Given
            DailyLossLimit limit = new DailyLossLimit(-500.0);

            // When
            boolean canTrade1 = limit.canTrade();
            boolean trade1 = limit.addPnl(-200.0);
            boolean trade2 = limit.addPnl(-200.0);

            // Then
            then(canTrade1).isTrue();
            then(trade1).isTrue();
            then(trade2).isTrue();
            then(limit.getCumulativePnl()).isEqualTo(-400.0)
                    .as("Cumulative PnL should reflect all losses");
        }

        @Test
        @DisplayName("Should allow trading when PnL starts at zero")
        void shouldAllowTrading_WhenPnLStartsAtZero() {
            // Given
            DailyLossLimit limit = new DailyLossLimit(-500.0);

            // When/Then
            then(limit.canTrade()).isTrue();
            then(limit.getCumulativePnl()).isEqualTo(0.0)
                    .as("Cumulative PnL should start at zero");
        }
    }

    @Nested
    @DisplayName("When trading exceeds loss limit")
    class ExceedsLimit {

        @Test
        @DisplayName("Should stop trading after hitting daily loss limit")
        void shouldStopTrading_WhenLimitHit() {
            // Given
            DailyLossLimit limit = new DailyLossLimit(-500.0);
            limit.addPnl(-300.0);

            // When
            boolean beforeLimit = limit.canTrade();
            boolean tradeResult = limit.addPnl(-250.0); // Cumulative: -550
            boolean afterLimit = limit.canTrade();

            // Then
            then(beforeLimit).isTrue()
                    .as("Trading allowed before limit reached");
            then(afterLimit).isFalse()
                    .as("Trading should stop after limit hit");
            then(limit.getCumulativePnl()).isEqualTo(-550.0)
                    .as("PnL should still be recorded even after limit hit");
        }
    }

    @Nested
    @DisplayName("When PnL changes")
    class PnLChanges {

        @Test
        @DisplayName("Should allow trading when wins offset losses")
        void shouldAllowTrading_WhenWinsOffsetLosses() {
            // Given
            DailyLossLimit limit = new DailyLossLimit(-500.0);
            limit.addPnl(-300.0);

            // When
            boolean canTrade = limit.addPnl(150.0); // Cumulative: -150

            // Then
            then(canTrade).isTrue()
                    .as("Wins reduce cumulative loss, trading should be allowed");
        }

        @Test
        @DisplayName("Should return correct daily loss limit")
        void shouldReturnDailyLimit() {
            // Given
            DailyLossLimit limit = new DailyLossLimit(-1000.0);

            // When/Then
            then(limit.getDailyLimit()).isEqualTo(-1000.0);
        }
    }

    @Nested
    @DisplayName("When manually stopping trading")
    class ManualStop {

        @Test
        @DisplayName("Should stop trading after stopTrading()")
        void shouldStopTrading_AfterManualStop() {
            // Given
            DailyLossLimit limit = new DailyLossLimit(-500.0);

            // When
            limit.stopTrading();

            // Then
            then(limit.canTrade()).isFalse();
        }
    }

    @Nested
    @DisplayName("When constructing limit")
    class Construction {

        @Test
        @DisplayName("Should reject positive loss limit")
        void shouldRejectPositiveLimit() {
            // Given/When/Then
            thenThrownBy(() -> new DailyLossLimit(500.0))
                    .isInstanceOf(IllegalArgumentException.class);
        }

        @Test
        @DisplayName("Should reject zero loss limit")
        void shouldRejectZeroLimit() {
            // Given/When/Then
            thenThrownBy(() -> new DailyLossLimit(0.0))
                    .isInstanceOf(IllegalArgumentException.class);
        }
    }
}