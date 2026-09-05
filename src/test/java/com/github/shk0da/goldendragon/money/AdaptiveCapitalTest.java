package com.github.shk0da.goldendragon.money;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.BDDAssertions.then;

@DisplayName("Adaptive Capital")
class AdaptiveCapitalTest {

    private static final double BASE_RISK = 0.01; // 1%
    private static final int LOSSES_TO_REDUCE = 3;
    private static final int WINS_TO_RESTORE = 5;
    private static final double RISK_REDUCTION_FACTOR = 0.5;

    @Nested
    @DisplayName("When in initial state")
    class InitialState {

        @Test
        @DisplayName("Should start with base risk")
        void shouldStartWithBaseRisk() {
            // Given
            AdaptiveCapital capital = new AdaptiveCapital(
                    BASE_RISK, LOSSES_TO_REDUCE, WINS_TO_RESTORE, RISK_REDUCTION_FACTOR);

            // When
            double riskPercent = capital.getCurrentRiskPercent();
            double multiplier = capital.getRiskMultiplier();

            // Then
            then(riskPercent).isEqualTo(BASE_RISK);
            then(multiplier).isEqualTo(1.0);
        }
    }

    @Nested
    @DisplayName("When registering losses")
    class LosingStreak {

        @Test
        @DisplayName("Should not reduce risk below threshold")
        void shouldNotReduceRisk_BelowLossThreshold() {
            // Given
            AdaptiveCapital capital = new AdaptiveCapital(
                    BASE_RISK, LOSSES_TO_REDUCE, WINS_TO_RESTORE, RISK_REDUCTION_FACTOR);

            // When
            capital.registerLoss();
            capital.registerLoss(); // Only 2 losses, threshold is 3

            // Then
            then(capital.getCurrentRiskPercent()).isEqualTo(BASE_RISK);
            then(capital.getRiskMultiplier()).isEqualTo(1.0);
        }

        @Test
        @DisplayName("Should reduce risk after consecutive losses reach threshold")
        void shouldReduceRisk_WhenLossesReachThreshold() {
            // Given
            AdaptiveCapital capital = new AdaptiveCapital(
                    BASE_RISK, LOSSES_TO_REDUCE, WINS_TO_RESTORE, RISK_REDUCTION_FACTOR);

            // When
            capital.registerLoss();
            capital.registerLoss();
            capital.registerLoss(); // Reaches threshold of 3

            // Then
            then(capital.getCurrentRiskPercent()).isEqualTo(BASE_RISK * RISK_REDUCTION_FACTOR);
            then(capital.getRiskMultiplier()).isEqualTo(0.5);
        }

        @Test
        @DisplayName("Should not reduce below minimum after more losses")
        void shouldNotReduceFurther_AfterMoreLosses() {
            // Given
            AdaptiveCapital capital = new AdaptiveCapital(
                    BASE_RISK, LOSSES_TO_REDUCE, WINS_TO_RESTORE, RISK_REDUCTION_FACTOR);

            // When
            for (int i = 0; i < 10; i++) {
                capital.registerLoss();
            }

            // Then
            // Risk stays at reduced level (no martingale - never reduces below one step)
            then(capital.getCurrentRiskPercent()).isEqualTo(BASE_RISK * RISK_REDUCTION_FACTOR);
        }

        @Test
        @DisplayName("Should reset loss count after a win")
        void shouldResetLosses_OnWin() {
            // Given
            AdaptiveCapital capital = new AdaptiveCapital(
                    BASE_RISK, LOSSES_TO_REDUCE, WINS_TO_RESTORE, RISK_REDUCTION_FACTOR);
            capital.registerLoss();
            capital.registerLoss();

            // When
            capital.registerWin(); // Breaks losing streak

            // Then
            // Loss count reset, but risk reduction only happens after LOSSES_TO_REDUCE consecutive losses
            then(capital.getCurrentRiskPercent()).isEqualTo(BASE_RISK);
        }
    }

    @Nested
    @DisplayName("When registering wins")
    class WinningStreak {

        @Test
        @DisplayName("Should restore base risk after enough consecutive wins")
        void shouldRestoreRisk_AfterEnoughWins() {
            // Given
            AdaptiveCapital capital = new AdaptiveCapital(
                    BASE_RISK, LOSSES_TO_REDUCE, WINS_TO_RESTORE, RISK_REDUCTION_FACTOR);
            // Reduce risk first
            for (int i = 0; i < LOSSES_TO_REDUCE; i++) {
                capital.registerLoss();
            }
            then(capital.getCurrentRiskPercent()).isEqualTo(BASE_RISK * RISK_REDUCTION_FACTOR);

            // When
            for (int i = 0; i < WINS_TO_RESTORE; i++) {
                capital.registerWin();
            }

            // Then
            then(capital.getCurrentRiskPercent()).isEqualTo(BASE_RISK);
            then(capital.getRiskMultiplier()).isEqualTo(1.0);
        }

        @Test
        @DisplayName("Should not restore risk before enough wins")
        void shouldNotRestoreRisk_BeforeEnoughWins() {
            // Given
            AdaptiveCapital capital = new AdaptiveCapital(
                    BASE_RISK, LOSSES_TO_REDUCE, WINS_TO_RESTORE, RISK_REDUCTION_FACTOR);
            for (int i = 0; i < LOSSES_TO_REDUCE; i++) {
                capital.registerLoss();
            }

            // When
            capital.registerWin();
            capital.registerWin(); // Only 2 wins out of 5 needed

            // Then
            then(capital.getCurrentRiskPercent()).isEqualTo(BASE_RISK * RISK_REDUCTION_FACTOR);
        }

        @Test
        @DisplayName("Should reset win count after a loss")
        void shouldResetWins_OnLoss() {
            // Given
            AdaptiveCapital capital = new AdaptiveCapital(
                    BASE_RISK, LOSSES_TO_REDUCE, WINS_TO_RESTORE, RISK_REDUCTION_FACTOR);
            for (int i = 0; i < LOSSES_TO_REDUCE; i++) {
                capital.registerLoss();
            }

            // When
            for (int i = 0; i < 4; i++) {
                capital.registerWin(); // 4 wins
            }
            capital.registerLoss(); // Break winning streak before restore

            // Then
            // Win count reset, still reduced risk
            then(capital.getCurrentRiskPercent()).isEqualTo(BASE_RISK * RISK_REDUCTION_FACTOR);
        }
    }

    @Nested
    @DisplayName("When resetting")
    class Reset {

        @Test
        @DisplayName("Should restore base risk")
        void shouldRestoreBaseRisk() {
            // Given
            AdaptiveCapital capital = new AdaptiveCapital(
                    BASE_RISK, LOSSES_TO_REDUCE, WINS_TO_RESTORE, RISK_REDUCTION_FACTOR);
            for (int i = 0; i < LOSSES_TO_REDUCE; i++) {
                capital.registerLoss();
            }
            then(capital.getCurrentRiskPercent()).isEqualTo(BASE_RISK * RISK_REDUCTION_FACTOR);

            // When
            capital.reset();

            // Then
            then(capital.getCurrentRiskPercent()).isEqualTo(BASE_RISK);
            then(capital.getRiskMultiplier()).isEqualTo(1.0);
        }
    }
}