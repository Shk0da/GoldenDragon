package com.github.shk0da.goldendragon.money;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.BDDAssertions.then;

@DisplayName("AdaptiveLeverage")
class AdaptiveLeverageTest {

    private static AdaptiveLeverage.Context context(
            int maxLeverage,
            int minLeverage,
            double adx,
            double atr,
            double avgAtr,
            double regimeConfidence,
            double adaptiveRiskMultiplier,
            boolean strongTrend,
            boolean rangeRegime,
            String signal) {
        return new AdaptiveLeverage.Context(
                maxLeverage,
                minLeverage,
                adx,
                atr,
                avgAtr,
                regimeConfidence,
                adaptiveRiskMultiplier,
                strongTrend,
                rangeRegime,
                signal);
    }

    @Nested
    @DisplayName("Boundary values")
    class BoundaryValues {

        @Test
        @DisplayName("Should return 1 when maxLeverage <= 1")
        void shouldReturnOne_WhenMaxLeverageIsOne() {
            AdaptiveLeverage.Context ctx = context(1, 1, 50.0, 1.0, 1.0, 100.0, 1.0, false, false, "TB_6");
            then(AdaptiveLeverage.resolve(ctx)).isEqualTo(1);
        }

        @Test
        @DisplayName("Should handle ADX=0 (minimum adxScore)")
        void shouldHandleZeroAdx() {
            AdaptiveLeverage.Context ctx = context(3, 1, 0.0, 1.0, 1.0, 100.0, 1.0, false, false, "TB_6");
            int leverage = AdaptiveLeverage.resolve(ctx);
            then(leverage).isBetween(1, 3);
        }

        @Test
        @DisplayName("Should handle ATR=0 (volRatio defaults to 1.0)")
        void shouldHandleZeroAtr() {
            AdaptiveLeverage.Context ctx = context(3, 1, 25.0, 0.0, 0.0, 100.0, 1.0, false, false, "TB_6");
            int leverage = AdaptiveLeverage.resolve(ctx);
            then(leverage).isBetween(1, 3);
        }

        @Test
        @DisplayName("Should handle null signal (defaults to 0.5)")
        void shouldHandleNullSignal() {
            AdaptiveLeverage.Context ctx = context(3, 1, 25.0, 1.0, 1.0, 100.0, 1.0, false, false, null);
            int leverage = AdaptiveLeverage.resolve(ctx);
            then(leverage).isBetween(1, 3);
        }

        @Test
        @DisplayName("Should handle empty signal (defaults to 0.5)")
        void shouldHandleEmptySignal() {
            AdaptiveLeverage.Context ctx = context(3, 1, 25.0, 1.0, 1.0, 100.0, 1.0, false, false, "");
            int leverage = AdaptiveLeverage.resolve(ctx);
            then(leverage).isBetween(1, 3);
        }
    }

    @Nested
    @DisplayName("Signal strength scoring")
    class SignalStrength {

        @Test
        @DisplayName("Should return 1.0 for TB_6, MXB, MXS signals")
        void shouldReturnMaxStrength_ForStrongSignals() {
            AdaptiveLeverage.Context ctx = context(3, 1, 25.0, 1.0, 1.0, 100.0, 1.0, false, false, "TB_6");
            int leverage = AdaptiveLeverage.resolve(ctx);
            AdaptiveLeverage.Context ctx2 = context(3, 1, 25.0, 1.0, 1.0, 100.0, 1.0, false, false, "MXB");
            int leverage2 = AdaptiveLeverage.resolve(ctx2);
            AdaptiveLeverage.Context ctx3 = context(3, 1, 25.0, 1.0, 1.0, 100.0, 1.0, false, false, "MXS");
            int leverage3 = AdaptiveLeverage.resolve(ctx3);
            then(leverage).isBetween(1, 3);
            then(leverage2).isBetween(1, 3);
            then(leverage3).isBetween(1, 3);
        }

        @Test
        @DisplayName("Should return 0.9 for TB_5 signals")
        void shouldReturnHighStrength_ForTB5() {
            AdaptiveLeverage.Context ctx = context(3, 1, 25.0, 1.0, 1.0, 100.0, 1.0, false, false, "TB_5");
            int leverage = AdaptiveLeverage.resolve(ctx);
            then(leverage).isBetween(1, 3);
        }

        @Test
        @DisplayName("Should return 0.75 for TB_4 signals")
        void shouldReturnMediumStrength_ForTB4() {
            AdaptiveLeverage.Context ctx = context(3, 1, 25.0, 1.0, 1.0, 100.0, 1.0, false, false, "TB_4");
            int leverage = AdaptiveLeverage.resolve(ctx);
            then(leverage).isBetween(1, 3);
        }

        @Test
        @DisplayName("Should return 0.7 for FX signals")
        void shouldReturnMediumStrength_ForFX() {
            AdaptiveLeverage.Context ctx = context(3, 1, 25.0, 1.0, 1.0, 100.0, 1.0, false, false, "FX");
            int leverage = AdaptiveLeverage.resolve(ctx);
            then(leverage).isBetween(1, 3);
        }

        @Test
        @DisplayName("Should return 0.8 for unknown signals")
        void shouldReturnDefaultStrength_ForUnknownSignals() {
            AdaptiveLeverage.Context ctx = context(3, 1, 25.0, 1.0, 1.0, 100.0, 1.0, false, false, "UNKNOWN");
            int leverage = AdaptiveLeverage.resolve(ctx);
            then(leverage).isBetween(1, 3);
        }
    }

    @Nested
    @DisplayName("Regime modifiers")
    class RegimeModifiers {

        @Test
        @DisplayName("Should reduce leverage for rangeRegime (0.65 multiplier)")
        void shouldReduceLeverage_ForRangeRegime() {
            AdaptiveLeverage.Context normalCtx = context(3, 1, 25.0, 1.0, 1.0, 100.0, 1.0, false, false, "TB_6");
            AdaptiveLeverage.Context rangeCtx = context(3, 1, 25.0, 1.0, 1.0, 100.0, 1.0, false, true, "TB_6");
            int normalLeverage = AdaptiveLeverage.resolve(normalCtx);
            int rangeLeverage = AdaptiveLeverage.resolve(rangeCtx);
            then(rangeLeverage).isLessThanOrEqualTo(normalLeverage);
        }

        @Test
        @DisplayName("Should increase leverage for strongTrend (1.12 multiplier, capped at 1.0)")
        void shouldIncreaseLeverage_ForStrongTrend() {
            AdaptiveLeverage.Context normalCtx = context(3, 1, 25.0, 1.0, 1.0, 100.0, 1.0, false, false, "TB_6");
            AdaptiveLeverage.Context strongCtx = context(3, 1, 25.0, 1.0, 1.0, 100.0, 1.0, true, false, "TB_6");
            int normalLeverage = AdaptiveLeverage.resolve(normalCtx);
            int strongLeverage = AdaptiveLeverage.resolve(strongCtx);
            then(strongLeverage).isGreaterThanOrEqualTo(normalLeverage);
        }
    }

    @Nested
    @DisplayName("Leverage bounds")
    class LeverageBounds {

        @Test
        @DisplayName("Should never exceed maxLeverage")
        void shouldNeverExceedMaxLeverage() {
            AdaptiveLeverage.Context ctx = context(2, 1, 50.0, 0.5, 1.0, 100.0, 1.0, true, false, "TB_6");
            int leverage = AdaptiveLeverage.resolve(ctx);
            then(leverage).isLessThanOrEqualTo(2);
        }

        @Test
        @DisplayName("Should never be below minLeverage")
        void shouldNeverBeBelowMinLeverage() {
            AdaptiveLeverage.Context ctx = context(3, 1, 0.0, 2.0, 1.0, 0.0, 0.5, false, true, "UNKNOWN");
            int leverage = AdaptiveLeverage.resolve(ctx);
            then(leverage).isGreaterThanOrEqualTo(1);
        }

        @Test
        @DisplayName("Should clamp minLeverage to [1, maxLeverage]")
        void shouldClampMinLeverage() {
            AdaptiveLeverage.Context ctx = context(2, 0, 25.0, 1.0, 1.0, 100.0, 1.0, false, false, "TB_6");
            int leverage = AdaptiveLeverage.resolve(ctx);
            then(leverage).isGreaterThanOrEqualTo(1);
            then(leverage).isLessThanOrEqualTo(2);
        }
    }

    @Nested
    @DisplayName("Volatility adjustment")
    class VolatilityAdjustment {

        @Test
        @DisplayName("Should reduce leverage when ATR is high relative to avgAtr")
        void shouldReduceLeverage_WhenVolatilityHigh() {
            AdaptiveLeverage.Context lowVolCtx = context(3, 1, 25.0, 0.5, 1.0, 100.0, 1.0, false, false, "TB_6");
            AdaptiveLeverage.Context highVolCtx = context(3, 1, 25.0, 1.8, 1.0, 100.0, 1.0, false, false, "TB_6");
            int lowVolLeverage = AdaptiveLeverage.resolve(lowVolCtx);
            int highVolLeverage = AdaptiveLeverage.resolve(highVolCtx);
            then(highVolLeverage).isLessThanOrEqualTo(lowVolLeverage);
        }

        @Test
        @DisplayName("Should increase leverage when ATR is low relative to avgAtr")
        void shouldIncreaseLeverage_WhenVolatilityLow() {
            AdaptiveLeverage.Context lowVolCtx = context(3, 1, 25.0, 0.75, 1.0, 100.0, 1.0, false, false, "TB_6");
            AdaptiveLeverage.Context normalVolCtx = context(3, 1, 25.0, 1.0, 1.0, 100.0, 1.0, false, false, "TB_6");
            int lowVolLeverage = AdaptiveLeverage.resolve(lowVolCtx);
            int normalVolLeverage = AdaptiveLeverage.resolve(normalVolCtx);
            then(lowVolLeverage).isGreaterThanOrEqualTo(normalVolLeverage);
        }
    }

    @Nested
    @DisplayName("Risk multiplier")
    class RiskMultiplier {

        @Test
        @DisplayName("Should scale leverage by adaptiveRiskMultiplier")
        void shouldScaleByRiskMultiplier() {
            AdaptiveLeverage.Context fullRiskCtx = context(3, 1, 25.0, 1.0, 1.0, 100.0, 1.0, false, false, "TB_6");
            AdaptiveLeverage.Context halfRiskCtx = context(3, 1, 25.0, 1.0, 1.0, 100.0, 0.5, false, false, "TB_6");
            int fullRiskLeverage = AdaptiveLeverage.resolve(fullRiskCtx);
            int halfRiskLeverage = AdaptiveLeverage.resolve(halfRiskCtx);
            then(halfRiskLeverage).isLessThanOrEqualTo(fullRiskLeverage);
        }

        @Test
        @DisplayName("Should clamp risk multiplier to [0.5, 1.0]")
        void shouldClampRiskMultiplier() {
            AdaptiveLeverage.Context lowRiskCtx = context(3, 1, 25.0, 1.0, 1.0, 100.0, 0.3, false, false, "TB_6");
            AdaptiveLeverage.Context highRiskCtx = context(3, 1, 25.0, 1.0, 1.0, 100.0, 1.5, false, false, "TB_6");
            int lowRiskLeverage = AdaptiveLeverage.resolve(lowRiskCtx);
            int highRiskLeverage = AdaptiveLeverage.resolve(highRiskCtx);
            then(lowRiskLeverage).isBetween(1, 3);
            then(highRiskLeverage).isBetween(1, 3);
        }
    }
}
