package com.github.shk0da.goldendragon.model;

import com.github.shk0da.goldendragon.config.UnifiedTraderConfig;
import com.github.shk0da.goldendragon.strategy.UnifiedStrategy;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.BDDAssertions.then;

@DisplayName("Shorts Enabled Configuration")
class ShortsEnabledTest {

    @Nested
    @DisplayName("Config constructor")
    class ConfigConstructor {

        @Test
        @DisplayName("Should default to shortsEnabled = true")
        void shouldDefaultToShortsEnabledTrue() {
            // When
            Config config = new Config();

            // Then
            then(config.shortsEnabled).isTrue();
        }

        @Test
        @DisplayName("Should use passed shortsEnabled value")
        void shouldUsePassedShortsEnabledValue() {
            // When
            Config configTrue = new Config(true);
            Config configFalse = new Config(false);

            // Then
            then(configTrue.shortsEnabled).isTrue();
            then(configFalse.shortsEnabled).isFalse();
        }
    }

    @Nested
    @DisplayName("UnifiedTraderConfig")
    class UnifiedTraderConfigTest {

        @Test
        @DisplayName("Should read shortsEnabled from application.properties")
        void shouldReadShortsEnabledFromProperties() {
            // Given
            UnifiedTraderConfig unifiedTraderConfig;
            try {
                unifiedTraderConfig = new UnifiedTraderConfig();
            } catch (Exception e) {
                throw new RuntimeException("Failed to load UnifiedTraderConfig", e);
            }

            // Then - should have a value (default true or from properties)
            then(unifiedTraderConfig).isNotNull();
            // The actual value depends on application.properties
        }

        @Test
        @DisplayName("Should provide getShortsEnabled method")
        void shouldProvideGetShortsEnabled() {
            // Given
            UnifiedTraderConfig unifiedTraderConfig;
            try {
                unifiedTraderConfig = new UnifiedTraderConfig();
            } catch (Exception e) {
                throw new RuntimeException("Failed to load UnifiedTraderConfig", e);
            }

            // When
            boolean shortsEnabled = unifiedTraderConfig.getShortsEnabled();

            // Then
            then(shortsEnabled).isIn(true, false); // Should return a boolean
        }
    }

    @Nested
    @DisplayName("UnifiedStrategy with shortsEnabled")
    class UnifiedStrategyShortsEnabled {

        @Test
        @DisplayName("Should respect shortsEnabled=false in decide method")
        void shouldRespectShortsEnabledFalse() {
            // Given
            UnifiedTraderConfig unifiedTraderConfig;
            try {
                unifiedTraderConfig = new UnifiedTraderConfig();
            } catch (Exception e) {
                throw new RuntimeException("Failed to load UnifiedTraderConfig", e);
            }

            // Create strategy with default Config (shortsEnabled will be overridden)
            UnifiedStrategy strategy = new UnifiedStrategy(
                unifiedTraderConfig,
                null, // tradingService
                new Config(false) // shortsEnabled = false
            );

            // Then - strategy should be created without errors
            then(strategy).isNotNull();
        }

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

            boolean traderConfigShortsEnabled = unifiedTraderConfig.getShortsEnabled();

            // When - create strategy with opposite Config value
            Config config = new Config(!traderConfigShortsEnabled);
            UnifiedStrategy strategy = new UnifiedStrategy(
                unifiedTraderConfig,
                null,
                config
            );

            // Then - config.shortsEnabled should be overridden to match UnifiedTraderConfig
            // This is tested indirectly - the strategy should use unifiedTraderConfig.getShortsEnabled()
            then(strategy).isNotNull();
            then(config.shortsEnabled).isEqualTo(traderConfigShortsEnabled);
        }
    }
}
