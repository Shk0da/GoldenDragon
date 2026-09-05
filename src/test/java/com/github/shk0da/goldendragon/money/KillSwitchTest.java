package com.github.shk0da.goldendragon.money;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.BDDAssertions.then;

@DisplayName("Kill Switch")
class KillSwitchTest {

    private static final double CRITICAL_DRAWDOWN = 0.10; // 10%

    @Nested
    @DisplayName("When in initial state")
    class InitialState {

        @Test
        @DisplayName("Should allow trading by default")
        void shouldAllowTrading_ByDefault() {
            // Given
            KillSwitch killSwitch = new KillSwitch(CRITICAL_DRAWDOWN);

            // When
            boolean allowed = killSwitch.isTradingAllowed();

            // Then
            then(allowed).isTrue();
        }

        @Test
        @DisplayName("Should have null trigger reason initially")
        void shouldHaveNullTriggerReason_Initially() {
            // Given
            KillSwitch killSwitch = new KillSwitch(CRITICAL_DRAWDOWN);

            // When
            String reason = killSwitch.getTriggerReason();

            // Then
            then(reason).isNull();
        }
    }

    @Nested
    @DisplayName("When manually triggering")
    class ManualTrigger {

        @Test
        @DisplayName("Should block trading with provided reason")
        void shouldBlockTrading_WithReason() {
            // Given
            KillSwitch killSwitch = new KillSwitch(CRITICAL_DRAWDOWN);

            // When
            killSwitch.trigger("CONNECTION_LOST");

            // Then
            then(killSwitch.isTradingAllowed()).isFalse();
            then(killSwitch.getTriggerReason()).isEqualTo("CONNECTION_LOST");
        }

        @Test
        @DisplayName("Should keep latest trigger reason")
        void shouldKeepLatestTriggerReason() {
            // Given
            KillSwitch killSwitch = new KillSwitch(CRITICAL_DRAWDOWN);

            // When
            killSwitch.trigger("CONNECTION_LOST");
            killSwitch.trigger("ABNORMAL_SPREAD");

            // Then
            then(killSwitch.getTriggerReason()).isEqualTo("ABNORMAL_SPREAD");
        }
    }

    @Nested
    @DisplayName("When checking drawdown")
    class DrawdownCheck {

        @Test
        @DisplayName("Should trigger when drawdown exceeds critical threshold")
        void shouldTrigger_WhenDrawdownExceedsThreshold() {
            // Given
            KillSwitch killSwitch = new KillSwitch(CRITICAL_DRAWDOWN);

            // When
            killSwitch.checkDrawdown(0.15); // 15% drawdown

            // Then
            then(killSwitch.isTradingAllowed()).isFalse();
            then(killSwitch.getTriggerReason()).isEqualTo("CRITICAL_DD_15%");
        }

        @Test
        @DisplayName("Should trigger at exact threshold")
        void shouldTrigger_AtExactThreshold() {
            // Given
            KillSwitch killSwitch = new KillSwitch(CRITICAL_DRAWDOWN);

            // When
            killSwitch.checkDrawdown(0.10); // exactly 10%

            // Then
            then(killSwitch.isTradingAllowed()).isFalse();
        }

        @Test
        @DisplayName("Should not trigger below threshold")
        void shouldNotTrigger_BelowThreshold() {
            // Given
            KillSwitch killSwitch = new KillSwitch(CRITICAL_DRAWDOWN);

            // When
            killSwitch.checkDrawdown(0.05); // 5% < 10%

            // Then
            then(killSwitch.isTradingAllowed()).isTrue();
            then(killSwitch.getTriggerReason()).isNull();
        }

        @Test
        @DisplayName("Should not trigger when zero drawdown")
        void shouldNotTrigger_WhenZeroDrawdown() {
            // Given
            KillSwitch killSwitch = new KillSwitch(CRITICAL_DRAWDOWN);

            // When
            killSwitch.checkDrawdown(0.0);

            // Then
            then(killSwitch.isTradingAllowed()).isTrue();
        }
    }

    @Nested
    @DisplayName("When resetting")
    class Reset {

        @Test
        @DisplayName("Should restore trading after reset")
        void shouldRestoreTrading_AfterReset() {
            // Given
            KillSwitch killSwitch = new KillSwitch(CRITICAL_DRAWDOWN);
            killSwitch.trigger("MANUAL");

            // When
            killSwitch.reset();

            // Then
            then(killSwitch.isTradingAllowed()).isTrue();
            then(killSwitch.getTriggerReason()).isNull();
        }

        @Test
        @DisplayName("Should allow trigger again after reset")
        void shouldAllowTrigger_AfterReset() {
            // Given
            KillSwitch killSwitch = new KillSwitch(CRITICAL_DRAWDOWN);
            killSwitch.trigger("FIRST");
            killSwitch.reset();

            // When
            killSwitch.trigger("SECOND");

            // Then
            then(killSwitch.isTradingAllowed()).isFalse();
            then(killSwitch.getTriggerReason()).isEqualTo("SECOND");
        }
    }

    @Nested
    @DisplayName("When drawdown triggers after previous trigger")
    class RepeatedTrigger {

        @Test
        @DisplayName("Should not re-trigger when already stopped")
        void shouldNotRetrigger_WhenAlreadyStopped() {
            // Given
            KillSwitch killSwitch = new KillSwitch(CRITICAL_DRAWDOWN);
            killSwitch.trigger("CONNECTION_LOST");

            // When
            killSwitch.checkDrawdown(0.20);

            // Then
            // Trigger reason should remain the manual one
            then(killSwitch.getTriggerReason()).isEqualTo("CONNECTION_LOST");
        }
    }
}