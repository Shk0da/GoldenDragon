package com.github.shk0da.goldendragon.model;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.BDDAssertions.then;

@DisplayName("Config")
class ConfigTest {

    @Nested
    @DisplayName("When using default constructor")
    class DefaultConstructor {

        @Test
        @DisplayName("Should initialize with default values and shortsEnabled=true")
        void shouldInitializeWithDefaults() {
            Config config = new Config();

            // EMA parameters
            then(config.emaTrend).isEqualTo(24);
            then(config.emaFast).isEqualTo(3);
            then(config.emaSlow).isEqualTo(7);

            // RSI parameters
            then(config.rsiPeriod).isEqualTo(14);
            then(config.rsiOversold).isEqualTo(25.0);
            then(config.rsiOverbought).isEqualTo(75.0);

            // ADX/ATR parameters
            then(config.adxPeriod).isEqualTo(14);
            then(config.atrPeriod).isEqualTo(14);
            then(config.adxMin).isEqualTo(20.0);

            // Commission and hold
            then(config.commission).isEqualTo(0.0005);
            then(config.maxCandlesHold).isEqualTo(24);
            then(config.maxCandlesHoldFx).isEqualTo(12);

            // ATR spike
            then(config.atrSpikeThreshold).isEqualTo(3.0);
            then(config.atrSpikeWindow).isEqualTo(10);
            then(config.cooldownCandles).isEqualTo(3);

            // Market Regime Filter
            then(config.marketRegimeFilterEnabled).isFalse();

            // Bad Weather Filter
            then(config.badWeatherFilterEnabled).isFalse();
            then(config.badWeatherLowVolumeThreshold).isEqualTo(0.5);
            then(config.badWeatherLowAtrThreshold).isEqualTo(0.7);
            then(config.badWeatherMinRangePercent).isEqualTo(0.005);
            then(config.badWeatherHighAtrThreshold).isEqualTo(2.0);
            then(config.badWeatherMaxSpreadPercent).isEqualTo(0.01);
            then(config.badWeatherMaxWickRatio).isEqualTo(0.4);
            then(config.badWeatherPanicVolumeThreshold).isEqualTo(3.0);
            then(config.badWeatherMinAvgDailyVolume).isEqualTo(100000);
            then(config.badWeatherAtrSpikeThreshold).isEqualTo(2.5);

            // Money Management
            then(config.mmEnabled).isTrue();
            then(config.mmRiskPercent).isEqualTo(0.006);
            then(config.mmMaxDailyLossPercent).isEqualTo(0.02);
            then(config.mmMaxConsecutiveLosses).isEqualTo(3);
            then(config.mmSizingStrategy).isEqualTo("FIXED");
            then(config.mmVolatilityBaseAtr).isEqualTo(1.0);
            then(config.mmVolatilityMinAdjustment).isEqualTo(0.5);
            then(config.mmVolatilityMaxAdjustment).isEqualTo(1.5);
            then(config.mmAtrStopMultiplier).isEqualTo(1.75);
            then(config.mmTrailingActivationR).isEqualTo(0.85);
            then(config.mmTrailingMultiplier).isEqualTo(0.85);
            then(config.mmBreakevenActivationR).isEqualTo(0.4);
            then(config.mmBreakevenBuffer).isEqualTo(0.001);
            then(config.mmAdaptiveEnabled).isFalse();
            then(config.mmLossesToReduce).isEqualTo(3);
            then(config.mmWinsToRestore).isEqualTo(5);
            then(config.mmRiskReductionFactor).isEqualTo(0.5);
            then(config.mmCriticalDrawdownPercent).isEqualTo(0.15);
            then(config.mmMaxPositionSize).isEqualTo(0.15);
            then(config.shortsEnabled).isTrue();

            // Trailing Stop
            then(config.mmTrailingEnabled).isTrue();
            then(config.mmTrailingStepPercent).isEqualTo(0.005);
            then(config.mmTrailingDeltaPercent).isEqualTo(0.003);
            then(config.mmTrailingCheckInterval).isEqualTo(1);
        }
    }

    @Nested
    @DisplayName("When using shortsEnabled constructor")
    class ShortsEnabledConstructor {

        @Test
        @DisplayName("Should initialize with shortsEnabled=true")
        void shouldInitializeWithShortsEnabledTrue() {
            Config config = new Config(true);

            then(config.shortsEnabled).isTrue();
            // Verify other defaults are still set
            then(config.emaTrend).isEqualTo(24);
            then(config.mmEnabled).isTrue();
        }

        @Test
        @DisplayName("Should initialize with shortsEnabled=false")
        void shouldInitializeWithShortsEnabledFalse() {
            Config config = new Config(false);

            then(config.shortsEnabled).isFalse();
            // Verify other defaults are still set
            then(config.emaTrend).isEqualTo(24);
            then(config.mmEnabled).isTrue();
        }
    }

    @Nested
    @DisplayName("When using Bad Weather constructor")
    class BadWeatherConstructor {

        @Test
        @DisplayName("Should initialize with custom bad weather parameters")
        void shouldInitializeWithCustomBadWeatherParams() {
            Config config = new Config(
                    true,   // badWeatherFilterEnabled
                    0.3,    // badWeatherLowVolumeThreshold
                    0.5,    // badWeatherLowAtrThreshold
                    0.01,   // badWeatherMinRangePercent
                    3.0,    // badWeatherHighAtrThreshold
                    0.02,   // badWeatherMaxSpreadPercent
                    0.5,    // badWeatherMaxWickRatio
                    5.0,    // badWeatherPanicVolumeThreshold
                    200000, // badWeatherMinAvgDailyVolume
                    3.0     // badWeatherAtrSpikeThreshold
            );

            // Bad Weather Filter
            then(config.badWeatherFilterEnabled).isTrue();
            then(config.badWeatherLowVolumeThreshold).isEqualTo(0.3);
            then(config.badWeatherLowAtrThreshold).isEqualTo(0.5);
            then(config.badWeatherMinRangePercent).isEqualTo(0.01);
            then(config.badWeatherHighAtrThreshold).isEqualTo(3.0);
            then(config.badWeatherMaxSpreadPercent).isEqualTo(0.02);
            then(config.badWeatherMaxWickRatio).isEqualTo(0.5);
            then(config.badWeatherPanicVolumeThreshold).isEqualTo(5.0);
            then(config.badWeatherMinAvgDailyVolume).isEqualTo(200000);
            then(config.badWeatherAtrSpikeThreshold).isEqualTo(3.0);

            // Market Regime Filter should be false
            then(config.marketRegimeFilterEnabled).isFalse();

            // Money Management defaults for this constructor
            then(config.mmEnabled).isTrue();
            then(config.mmRiskPercent).isEqualTo(0.01);
            then(config.mmMaxDailyLossPercent).isEqualTo(0.03);
            then(config.mmAdaptiveEnabled).isTrue();
            then(config.mmCriticalDrawdownPercent).isEqualTo(0.10);
            then(config.mmMaxPositionSize).isEqualTo(0.15);
            then(config.shortsEnabled).isFalse();
        }

        @Test
        @DisplayName("Should initialize with bad weather disabled")
        void shouldInitializeWithBadWeatherDisabled() {
            Config config = new Config(
                    false,  // badWeatherFilterEnabled
                    0.5, 0.7, 0.005, 2.0, 0.01, 0.4, 3.0, 100000, 2.5
            );

            then(config.badWeatherFilterEnabled).isFalse();
        }
    }

    @Nested
    @DisplayName("When using Money Management constructor")
    class MoneyManagementConstructor {

        @Test
        @DisplayName("Should initialize with custom money management parameters")
        void shouldInitializeWithCustomMmParams() {
            Config config = new Config(
                    true,   // mmEnabled
                    0.02,   // mmRiskPercent
                    0.05,   // mmMaxDailyLossPercent
                    5,      // mmMaxConsecutiveLosses
                    "VOLATILITY", // mmSizingStrategy
                    2.0,    // mmVolatilityBaseAtr
                    0.3,    // mmVolatilityMinAdjustment
                    2.0,    // mmVolatilityMaxAdjustment
                    2.5,    // mmAtrStopMultiplier
                    0.9,    // mmTrailingActivationR
                    0.9,    // mmTrailingMultiplier
                    0.5,    // mmBreakevenActivationR
                    0.002,  // mmBreakevenBuffer
                    true,   // mmAdaptiveEnabled
                    2,      // mmLossesToReduce
                    3,      // mmWinsToRestore
                    0.3,    // mmRiskReductionFactor
                    0.20,   // mmCriticalDrawdownPercent
                    0.50,   // mmMaxPositionSize
                    true    // shortsEnabled
            );

            // Money Management
            then(config.mmEnabled).isTrue();
            then(config.mmRiskPercent).isEqualTo(0.02);
            then(config.mmMaxDailyLossPercent).isEqualTo(0.05);
            then(config.mmMaxConsecutiveLosses).isEqualTo(5);
            then(config.mmSizingStrategy).isEqualTo("VOLATILITY");
            then(config.mmVolatilityBaseAtr).isEqualTo(2.0);
            then(config.mmVolatilityMinAdjustment).isEqualTo(0.3);
            then(config.mmVolatilityMaxAdjustment).isEqualTo(2.0);
            then(config.mmAtrStopMultiplier).isEqualTo(2.5);
            then(config.mmTrailingActivationR).isEqualTo(0.9);
            then(config.mmTrailingMultiplier).isEqualTo(0.9);
            then(config.mmBreakevenActivationR).isEqualTo(0.5);
            then(config.mmBreakevenBuffer).isEqualTo(0.002);
            then(config.mmAdaptiveEnabled).isTrue();
            then(config.mmLossesToReduce).isEqualTo(2);
            then(config.mmWinsToRestore).isEqualTo(3);
            then(config.mmRiskReductionFactor).isEqualTo(0.3);
            then(config.mmCriticalDrawdownPercent).isEqualTo(0.20);
            then(config.mmMaxPositionSize).isEqualTo(0.50);
            then(config.shortsEnabled).isTrue();

            // Bad Weather should be disabled
            then(config.badWeatherFilterEnabled).isFalse();
            then(config.marketRegimeFilterEnabled).isFalse();
        }

        @Test
        @DisplayName("Should initialize with money management disabled")
        void shouldInitializeWithMmDisabled() {
            Config config = new Config(
                    false, 0.01, 0.02, 3, "FIXED", 1.0, 0.5, 1.5, 1.75,
                    0.85, 0.85, 0.4, 0.001, false, 3, 5, 0.5, 0.15, 0.20, false
            );

            then(config.mmEnabled).isFalse();
        }
    }

    @Nested
    @DisplayName("When using TrailingParams constructor")
    class TrailingParamsConstructor {

        @Test
        @DisplayName("Should initialize with custom trailing parameters")
        void shouldInitializeWithCustomTrailingParams() {
            Config.TrailingParams trailingParams = new Config.TrailingParams(
                    true,   // enabled
                    0.01,   // stepPercent
                    0.005,  // deltaPercent
                    5,      // checkInterval
                    0.8     // volumePercent
            );

            Config config = new Config(trailingParams);

            // Trailing parameters should be set from params
            then(config.mmTrailingEnabled).isTrue();
            then(config.mmTrailingStepPercent).isEqualTo(0.01);
            then(config.mmTrailingDeltaPercent).isEqualTo(0.005);
            then(config.mmTrailingCheckInterval).isEqualTo(5);

            // Other defaults should be set
            then(config.emaTrend).isEqualTo(24);
            then(config.mmEnabled).isTrue();
        }

        @Test
        @DisplayName("Should use defaults when trailing params is null")
        void shouldUseDefaultsWhenTrailingParamsNull() {
            Config config = new Config((Config.TrailingParams) null);

            // Should use default trailing values
            then(config.mmTrailingEnabled).isTrue();
            then(config.mmTrailingStepPercent).isEqualTo(0.005);
            then(config.mmTrailingDeltaPercent).isEqualTo(0.003);
            then(config.mmTrailingCheckInterval).isEqualTo(1);
        }

        @Test
        @DisplayName("Should initialize with trailing disabled")
        void shouldInitializeWithTrailingDisabled() {
            Config.TrailingParams trailingParams = new Config.TrailingParams(
                    false, 0.01, 0.005, 5, 0.8
            );

            Config config = new Config(trailingParams);

            then(config.mmTrailingEnabled).isFalse();
        }
    }

    @Nested
    @DisplayName("TrailingParams")
    class TrailingParamsTest {

        @Test
        @DisplayName("Should store trailing parameters")
        void shouldStoreTrailingParameters() {
            Config.TrailingParams params = new Config.TrailingParams(
                    true, 0.01, 0.005, 5, 0.8
            );

            then(params.enabled).isTrue();
            then(params.stepPercent).isEqualTo(0.01);
            then(params.deltaPercent).isEqualTo(0.005);
            then(params.checkInterval).isEqualTo(5);
        }

        @Test
        @DisplayName("Should allow disabled trailing")
        void shouldAllowDisabledTrailing() {
            Config.TrailingParams params = new Config.TrailingParams(
                    false, 0.0, 0.0, 0, 0.0
            );

            then(params.enabled).isFalse();
            then(params.stepPercent).isEqualTo(0.0);
        }
    }
}
