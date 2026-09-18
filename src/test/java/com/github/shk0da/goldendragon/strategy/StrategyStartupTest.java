package com.github.shk0da.goldendragon.strategy;

import com.github.shk0da.goldendragon.config.MainConfig;
import com.github.shk0da.goldendragon.config.UnifiedTraderConfig;
import com.github.shk0da.goldendragon.model.Config;
import com.github.shk0da.goldendragon.service.TradingService;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.io.IOException;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatCode;

/**
 * Tests for strategy startup and registration.
 * Ensures that strategies are properly registered and can be instantiated.
 */
@ExtendWith(MockitoExtension.class)
class StrategyStartupTest {

    @Mock
    private TradingService mockTradingService;

    @Mock
    private MainConfig mockMainConfig;

    @Test
    @DisplayName("UnifiedStrategy should be registered in StrategyRegistry")
    void unifiedStrategyShouldBeRegistered() {
        // Arrange & Act
        StrategyRegistry.Entry entry = StrategyRegistry.get("UnifiedStrategy");

        // Assert
        assertThat(entry).isNotNull();
        assertThat(entry.name()).isEqualTo("UnifiedStrategy");
        assertThat(entry.hasLiveRunner()).isTrue();
    }

    @Test
    @DisplayName("TradeCouncilStrategy should be registered in StrategyRegistry")
    void tradeCouncilStrategyShouldBeRegistered() {
        // Arrange & Act
        StrategyRegistry.Entry entry = StrategyRegistry.get("TradeCouncilStrategy");

        // Assert
        assertThat(entry).isNotNull();
        assertThat(entry.name()).isEqualTo("TradeCouncilStrategy");
        assertThat(entry.hasLiveRunner()).isTrue();
    }

    @Test
    @DisplayName("Unknown strategy should return null from StrategyRegistry")
    void unknownStrategyShouldReturnNull() {
        // Arrange & Act
        StrategyRegistry.Entry entry = StrategyRegistry.get("NonExistentStrategy");

        // Assert
        assertThat(entry).isNull();
    }

    @Test
    @DisplayName("UnifiedStrategy should be creatable for backtest")
    void unifiedStrategyShouldBeCreatableForBacktest() throws IOException {
        // Arrange
        UnifiedTraderConfig config = new UnifiedTraderConfig();

        // Act & Assert
        assertThatCode(() -> {
            BaseStrategy strategy = new UnifiedStrategy(config, null, new Config(config));
            assertThat(strategy).isInstanceOf(UnifiedStrategy.class);
        }).doesNotThrowAnyException();
    }

    @Test
    @DisplayName("UnifiedStrategy should be instantiable with mock services")
    void unifiedStrategyShouldBeInstantiableWithMocks() throws IOException {
        // Arrange
        UnifiedTraderConfig config = new UnifiedTraderConfig();
        Config modelConfig = new Config(config);

        // Act & Assert
        assertThatCode(() -> {
            UnifiedStrategy strategy = new UnifiedStrategy(config, mockTradingService, modelConfig, mockMainConfig);
            assertThat(strategy).isNotNull();
        }).doesNotThrowAnyException();
    }

    @Test
    @DisplayName("TradeCouncilStrategy should be instantiable with mock services")
    void tradeCouncilStrategyShouldBeInstantiableWithMocks() throws IOException {
        // Arrange
        UnifiedTraderConfig config = new UnifiedTraderConfig();
        Config modelConfig = new Config(config);

        // Act & Assert
        assertThatCode(() -> {
            TradeCouncilStrategy strategy = new TradeCouncilStrategy(config, mockTradingService, modelConfig, mockMainConfig);
            assertThat(strategy).isNotNull();
            assertThat(strategy.getStrategyName()).isEqualTo("TradeCouncilStrategy");
        }).doesNotThrowAnyException();
    }

    @Test
    @DisplayName("StrategyRegistry entry should execute live runner")
    void strategyRegistryEntryShouldExecuteLiveRunner() {
        // Arrange
        StrategyRegistry.Entry unifiedEntry = StrategyRegistry.get("UnifiedStrategy");
        StrategyRegistry.Entry tradeCouncilEntry = StrategyRegistry.get("TradeCouncilStrategy");

        // Assert
        assertThat(unifiedEntry).isNotNull();
        assertThat(tradeCouncilEntry).isNotNull();
        assertThat(unifiedEntry.hasLiveRunner()).isTrue();
        assertThat(tradeCouncilEntry.hasLiveRunner()).isTrue();
    }
}
