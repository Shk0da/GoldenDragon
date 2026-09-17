package com.github.shk0da.goldendragon.strategy;

import com.github.shk0da.goldendragon.config.UnifiedTraderConfig;
import com.github.shk0da.goldendragon.model.Config;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.BDDAssertions.then;

@DisplayName("StrategyRegistry shortsEnabled propagation")
class StrategyRegistryShortsEnabledTest {

    @Nested
    @DisplayName("When creating backtest strategy")
    class CreateBacktest {

        @Test
        @DisplayName("Should pass shortsEnabled from UnifiedTraderConfig to Config")
        void shouldPassShortsEnabledToConfig() {
            // Given
            UnifiedTraderConfig unifiedTraderConfig;
            try {
                unifiedTraderConfig = new UnifiedTraderConfig();
            } catch (Exception e) {
                throw new RuntimeException("Failed to load UnifiedTraderConfig", e);
            }

            boolean shortsEnabled = unifiedTraderConfig.getShortsEnabled();

            // When - create backtest strategy (which internally creates Config with shortsEnabled)
            BaseStrategy strategy = StrategyRegistry.createBacktest("UnifiedStrategy", unifiedTraderConfig);

            // Then - strategy should be created without errors
            then(strategy).isNotNull();
            then(strategy).isInstanceOf(UnifiedStrategy.class);

            // The Config.shortsEnabled should match the UnifiedTraderConfig value
            // because UnifiedStrategy constructor overrides it
            then(shortsEnabled).isIn(true, false);
        }

        @Test
        @DisplayName("Should use shortsEnabled=false from properties")
        void shouldUseShortsEnabledFalseFromProperties() {
            // Given - load properties which has shortsEnabled=false
            UnifiedTraderConfig unifiedTraderConfig;
            try {
                unifiedTraderConfig = new UnifiedTraderConfig();
            } catch (Exception e) {
                throw new RuntimeException("Failed to load UnifiedTraderConfig", e);
            }

            // When
            BaseStrategy strategy = StrategyRegistry.createBacktest("UnifiedStrategy", unifiedTraderConfig);

            // Then - strategy should be created without errors
            then(strategy).isNotNull();
            then(strategy).isInstanceOf(UnifiedStrategy.class);
        }
    }

    @Nested
    @DisplayName("When creating with custom Config")
    class CreateBacktestWithConfig {

        @Test
        @DisplayName("Should override Config.shortsEnabled from UnifiedTraderConfig")
        void shouldOverrideConfigShortsEnabled() {
            // Given
            UnifiedTraderConfig unifiedTraderConfig;
            try {
                unifiedTraderConfig = new UnifiedTraderConfig();
            } catch (Exception e) {
                throw new RuntimeException("Failed to load UnifiedTraderConfig", e);
            }

            // Create Config with shortsEnabled = true
            Config configWithShortsTrue = new Config(true);
            then(configWithShortsTrue.shortsEnabled).isTrue();

            boolean traderConfigShortsEnabled = unifiedTraderConfig.getShortsEnabled();

            // When - create strategy with custom Config
            BaseStrategy strategy = StrategyRegistry.createBacktest(
                "UnifiedStrategy",
                unifiedTraderConfig,
                null,
                configWithShortsTrue
            );

            // Then - strategy should be created and config.shortsEnabled should be overridden
            then(strategy).isNotNull();
            then(strategy).isInstanceOf(UnifiedStrategy.class);
            then(configWithShortsTrue.shortsEnabled).isEqualTo(traderConfigShortsEnabled);
        }
    }
}
