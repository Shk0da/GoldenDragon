package com.github.shk0da.goldendragon.strategy;

import com.github.shk0da.goldendragon.config.UnifiedTraderConfig;
import com.github.shk0da.goldendragon.model.Config;
import com.github.shk0da.goldendragon.model.TickerInfo;
import com.github.shk0da.goldendragon.model.TickerType;
import com.github.shk0da.goldendragon.repository.TickerRepository;
import com.github.shk0da.goldendragon.service.TradingService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.Map;

import static org.assertj.core.api.BDDAssertions.then;

@DisplayName("TradeCouncilStrategy Futures Integration")
class TradeCouncilStrategyFuturesIntegrationTest {

    private static final String GYENF = "GYENF";
    private static final int FUTURES_LOT = 1;

    @BeforeEach
    void setUp() throws Exception {
        TickerRepository.INSTANCE.putAll(
            Map.of(
                new TickerInfo.Key(GYENF, TickerType.FEATURE),
                new TickerInfo("GYENF_FIGI", GYENF, "GYENF_ISIN", 0.01, FUTURES_LOT, "RUB", GYENF, "FEATURE")
            )
        );
    }

    @Nested
    @DisplayName("calculateQuantityFromDepositPercent with futures")
    class FuturesQuantityCalculation {

        @Test
        @DisplayName("Should use margin-based calculation for futures when margin is available")
        void shouldUseMarginCalculationForFutures() throws Exception {
            // Given: futures with margin = 5000 RUB, deposit = 10%, balance = 100_000
            // Expected: 100_000 * 0.10 = 10_000 / 5500 (with 10% buffer) = 1.81 -> 1 lot
            TradeCouncilStrategy strategy = new TradeCouncilStrategy(
                new UnifiedTraderConfig(),
                new TradingService() {
                    @Override
                    public Double getSingleContractGo(String figi) {
                        return 5000.0; // margin = 5000 RUB
                    }
                },
                new Config()
            );

            int quantity = strategy.calculateQuantityFromDepositPercent(
                GYENF, 10.0, 100.0, 100_000.0, "GYENF_FIGI");

            then(quantity).isEqualTo(1); // 10_000 / 5500 = 1.81 -> 1
        }

        @Test
        @DisplayName("Should use fallback 25% of price for futures when margin unavailable")
        void shouldUseFallbackForFutures() throws Exception {
            // Given: futures price = 100_000, fallback margin = 25_000, deposit = 10%, balance = 100_000
            // Expected: 100_000 * 0.10 = 10_000 / 27_500 (with 10% buffer) = 0.36 -> 0 lots
            TradeCouncilStrategy strategy = new TradeCouncilStrategy(
                new UnifiedTraderConfig(),
                new TradingService() {
                    @Override
                    public Double getSingleContractGo(String figi) {
                        return null; // API unavailable, use fallback
                    }
                },
                new Config()
            );

            int quantity = strategy.calculateQuantityFromDepositPercent(
                GYENF, 10.0, 100_000.0, 100_000.0, "GYENF_FIGI");

            then(quantity).isZero(); // 10_000 / 27_500 = 0.36 -> 0
        }

        @Test
        @DisplayName("Should use stock calculation for non-futures instruments")
        void shouldUseStockCalculationForNonFutures() throws Exception {
            // Given: stock with lot = 100, price = 100, deposit = 1%, balance = 100_000
            // Expected: 100_000 * 0.01 = 1_000 / 100 = 10 units / 100 lot = 0.1 -> 0 lots
            TickerRepository.INSTANCE.putAll(
                Map.of(
                    new TickerInfo.Key("STOCK", TickerType.STOCK),
                    new TickerInfo("STOCK_FIGI", "STOCK", "STOCK_ISIN", 0.01, 100, "RUB", "STOCK", "STOCK")
                )
            );

            TradeCouncilStrategy strategy = new TradeCouncilStrategy(
                new UnifiedTraderConfig(),
                new TradingService() {},
                new Config()
            );

            int quantity = strategy.calculateQuantityFromDepositPercent(
                "STOCK", 1.0, 100.0, 100_000.0, "STOCK_FIGI");

            then(quantity).isZero(); // 1_000 / 100 = 10 units / 100 lot = 0.1 -> 0
        }
    }
}
