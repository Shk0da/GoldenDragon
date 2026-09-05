package com.github.shk0da.goldendragon.money;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.BDDAssertions.then;

@DisplayName("Risk Manager")
class RiskManagerTest {

    private static final double MAX_DAILY_LOSS_PERCENT = 0.03; // 3%
    private static final int MAX_CONSECUTIVE_LOSSES = 3;
    private static final double EQUITY = 100_000.0;

    @Nested
    @DisplayName("When checking initial state")
    class InitialState {

        @Test
        @DisplayName("Should allow trading by default")
        void shouldAllowTrading_ByDefault() {
            // Given
            RiskManager riskManager = new RiskManager(MAX_DAILY_LOSS_PERCENT, MAX_CONSECUTIVE_LOSSES);

            // When
            boolean canTrade = riskManager.canTrade(EQUITY);

            // Then
            then(canTrade).isTrue();
        }

        @Test
        @DisplayName("Should have zero daily PnL initially")
        void shouldHaveZeroDailyPnl_Initially() {
            // Given
            RiskManager riskManager = new RiskManager(MAX_DAILY_LOSS_PERCENT, MAX_CONSECUTIVE_LOSSES);

            // When
            double dailyPnL = riskManager.getDailyPnL();

            // Then
            then(dailyPnL).isZero();
        }

        @Test
        @DisplayName("Should have zero consecutive losses initially")
        void shouldHaveZeroConsecutiveLosses_Initially() {
            // Given
            RiskManager riskManager = new RiskManager(MAX_DAILY_LOSS_PERCENT, MAX_CONSECUTIVE_LOSSES);

            // When
            int consecutiveLosses = riskManager.getConsecutiveLosses();

            // Then
            then(consecutiveLosses).isZero();
        }
    }

    @Nested
    @DisplayName("When registering winning trades")
    class WinningTrades {

        @Test
        @DisplayName("Should reset consecutive losses")
        void shouldResetConsecutiveLosses_OnWin() {
            // Given
            RiskManager riskManager = new RiskManager(MAX_DAILY_LOSS_PERCENT, MAX_CONSECUTIVE_LOSSES);
            riskManager.registerTrade(-100.0);
            riskManager.registerTrade(-100.0);

            // When
            riskManager.registerTrade(200.0);

            // Then
            then(riskManager.getConsecutiveLosses()).isZero();
            then(riskManager.getDailyPnL()).isEqualTo(0.0);
        }

        @Test
        @DisplayName("Should accumulate positive daily PnL")
        void shouldAccumulateDailyPnl_OnWins() {
            // Given
            RiskManager riskManager = new RiskManager(MAX_DAILY_LOSS_PERCENT, MAX_CONSECUTIVE_LOSSES);

            // When
            riskManager.registerTrade(100.0);
            riskManager.registerTrade(50.0);

            // Then
            then(riskManager.getDailyPnL()).isEqualTo(150.0);
        }

        @Test
        @DisplayName("Should allow trading after wins")
        void shouldAllowTrading_AfterWins() {
            // Given
            RiskManager riskManager = new RiskManager(MAX_DAILY_LOSS_PERCENT, MAX_CONSECUTIVE_LOSSES);
            riskManager.registerTrade(5000.0);

            // When
            boolean canTrade = riskManager.canTrade(EQUITY);

            // Then
            then(canTrade).isTrue();
        }
    }

    @Nested
    @DisplayName("When registering losing trades")
    class LosingTrades {

        @Test
        @DisplayName("Should count consecutive losses")
        void shouldCountConsecutiveLosses() {
            // Given
            RiskManager riskManager = new RiskManager(MAX_DAILY_LOSS_PERCENT, MAX_CONSECUTIVE_LOSSES);

            // When
            riskManager.registerTrade(-100.0);
            riskManager.registerTrade(-50.0);

            // Then
            then(riskManager.getConsecutiveLosses()).isEqualTo(2);
        }

        @Test
        @DisplayName("Should block trading when consecutive loss limit reached")
        void shouldBlockTrading_WhenConsecutiveLossLimitReached() {
            // Given
            RiskManager riskManager = new RiskManager(MAX_DAILY_LOSS_PERCENT, MAX_CONSECUTIVE_LOSSES);

            // When
            riskManager.registerTrade(-100.0);
            riskManager.registerTrade(-100.0);
            riskManager.registerTrade(-100.0); // Reaches limit of 3

            // Then
            boolean canTrade = riskManager.canTrade(EQUITY);
            then(canTrade).isFalse();
        }

        @Test
        @DisplayName("Should not reach limit before consecutive count")
        void shouldAllowTrading_BeforeLimitReached() {
            // Given
            RiskManager riskManager = new RiskManager(MAX_DAILY_LOSS_PERCENT, MAX_CONSECUTIVE_LOSSES);

            // When
            riskManager.registerTrade(-100.0);
            riskManager.registerTrade(-100.0); // Only 2 losses, limit is 3

            // Then
            boolean canTrade = riskManager.canTrade(EQUITY);
            then(canTrade).isTrue();
        }
    }

    @Nested
    @DisplayName("When daily loss limit is reached")
    class DailyLossLimit {

        @Test
        @DisplayName("Should block trading when loss >= max daily loss")
        void shouldBlockTrading_WhenDailyLossLimitReached() {
            // Given
            RiskManager riskManager = new RiskManager(MAX_DAILY_LOSS_PERCENT, MAX_CONSECUTIVE_LOSSES);

            // When
            riskManager.registerTrade(-3000.0); // 3% loss = 3_000 / 100_000
            boolean canTrade = riskManager.canTrade(EQUITY);

            // Then
            then(canTrade).isFalse();
        }

        @Test
        @DisplayName("Should allow trading when loss is below daily limit")
        void shouldAllowTrading_WhenLossBelowLimit() {
            // Given
            RiskManager riskManager = new RiskManager(MAX_DAILY_LOSS_PERCENT, MAX_CONSECUTIVE_LOSSES);

            // When
            riskManager.registerTrade(-2000.0); // 2% loss < 3%
            boolean canTrade = riskManager.canTrade(EQUITY);

            // Then
            then(canTrade).isTrue();
        }

        @Test
        @DisplayName("Should block at exact threshold")
        void shouldBlock_AtExactThreshold() {
            // Given
            RiskManager riskManager = new RiskManager(MAX_DAILY_LOSS_PERCENT, MAX_CONSECUTIVE_LOSSES);

            // When
            riskManager.registerTrade(-3000.0); // exactly 3%
            boolean canTrade = riskManager.canTrade(EQUITY);

            // Then
            then(canTrade).isFalse();
        }
    }

    @Nested
    @DisplayName("When combining wins and losses")
    class MixedResults {

        @Test
        @DisplayName("Should not block trading when wins offset losses")
        void shouldNotBlock_WhenWinsOffsetLosses() {
            // Given
            RiskManager riskManager = new RiskManager(MAX_DAILY_LOSS_PERCENT, MAX_CONSECUTIVE_LOSSES);

            // When
            riskManager.registerTrade(-2000.0);
            riskManager.registerTrade(2500.0); // Net +500
            boolean canTrade = riskManager.canTrade(EQUITY);

            // Then
            then(canTrade).isTrue();
            then(riskManager.getDailyPnL()).isEqualTo(500.0);
        }

        @Test
        @DisplayName("Should reset consecutive losses on win after losses")
        void shouldResetLosses_OnWinAfterLosses() {
            // Given
            RiskManager riskManager = new RiskManager(MAX_DAILY_LOSS_PERCENT, MAX_CONSECUTIVE_LOSSES);

            // When
            riskManager.registerTrade(-100.0);
            riskManager.registerTrade(-100.0);
            riskManager.registerTrade(300.0);

            // Then
            then(riskManager.getConsecutiveLosses()).isZero();
            then(riskManager.getDailyPnL()).isEqualTo(100.0);
        }
    }

    @Nested
    @DisplayName("When resetting daily limits")
    class ResetLimits {

        @Test
        @DisplayName("Should clear daily PnL and consecutive losses")
        void shouldClearState_OnReset() {
            // Given
            RiskManager riskManager = new RiskManager(MAX_DAILY_LOSS_PERCENT, MAX_CONSECUTIVE_LOSSES);
            riskManager.registerTrade(-100.0);
            riskManager.registerTrade(-100.0);

            // When
            riskManager.resetDailyLimits();

            // Then
            then(riskManager.getDailyPnL()).isZero();
            then(riskManager.getConsecutiveLosses()).isZero();
            then(riskManager.canTrade(EQUITY)).isTrue();
        }

        @Test
        @DisplayName("Should reset after reaching limit")
        void shouldEnableTrading_AfterReset() {
            // Given
            RiskManager riskManager = new RiskManager(MAX_DAILY_LOSS_PERCENT, MAX_CONSECUTIVE_LOSSES);
            riskManager.registerTrade(-3000.0);
            then(riskManager.canTrade(EQUITY)).isFalse();

            // When
            riskManager.resetDailyLimits();

            // Then
            then(riskManager.canTrade(EQUITY)).isTrue();
        }
    }

    @Nested
    @DisplayName("When equity is invalid")
    class InvalidEquity {

        @Test
        @DisplayName("Should block trading when equity <= 0")
        void shouldBlockTrading_WhenEquityIsZero() {
            // Given
            RiskManager riskManager = new RiskManager(MAX_DAILY_LOSS_PERCENT, MAX_CONSECUTIVE_LOSSES);

            // When
            boolean canTrade = riskManager.canTrade(0.0);

            // Then
            then(canTrade).isFalse();
        }

        @Test
        @DisplayName("Should block trading when equity is negative")
        void shouldBlockTrading_WhenEquityIsNegative() {
            // Given
            RiskManager riskManager = new RiskManager(MAX_DAILY_LOSS_PERCENT, MAX_CONSECUTIVE_LOSSES);

            // When
            boolean canTrade = riskManager.canTrade(-1000.0);

            // Then
            then(canTrade).isFalse();
        }
    }
}