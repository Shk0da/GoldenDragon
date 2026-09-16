package com.github.shk0da.goldendragon.strategy;

import com.github.shk0da.goldendragon.config.UnifiedTraderConfig;
import com.github.shk0da.goldendragon.model.Candle;
import com.github.shk0da.goldendragon.model.Config;
import com.github.shk0da.goldendragon.model.TickerInfo;
import com.github.shk0da.goldendragon.model.TickerType;
import com.github.shk0da.goldendragon.repository.TickerRepository;
import com.github.shk0da.goldendragon.service.TradingService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;

import static org.assertj.core.api.BDDAssertions.then;

@DisplayName("TradeCouncil quantity calculation with lot size")
class TradeCouncilQuantityTest {

    private static final String NLMK = "NLMK";
    private static final int NLMK_LOT = 10;
    private static final String T = "T";
    private static final int T_LOT = 1;

    private TradeCouncilStrategy strategy;

    @BeforeEach
    void setUp() throws Exception {
        TickerRepository.INSTANCE.putAll(
                Map.of(
                        new TickerInfo.Key(NLMK, TickerType.STOCK),
                        new TickerInfo("FIGI_NLMK", NLMK, "ISIN_NLMK", 0.01, NLMK_LOT, "RUB", NLMK, "STOCK"),
                        new TickerInfo.Key(T, TickerType.STOCK),
                        new TickerInfo("FIGI_T", T, "ISIN_T", 0.01, T_LOT, "RUB", T, "STOCK")));
        strategy = new TradeCouncilStrategy(new UnifiedTraderConfig(), new TradingService() {
            @Override
            public TickerInfo searchTicker(TickerInfo.Key key) {
                return TickerRepository.INSTANCE.getByName(key.getTicker());
            }

            @Override
            public Double getSingleContractGo(String figi) {
                return 0.0;
            }

            @Override
            public List<Candle> getCandles(String figi, String interval, int count) {
                return List.of();
            }
        }, new Config());
    }

    @Nested
    @DisplayName("calculateQuantityFromDepositPercent")
    class DepositPercent {

        @Test
        @DisplayName("Should return quantity in lots, not units")
        void shouldReturnLots() {
            // Given: 1% of 100_000 = 1000, / 71.2 = 14.04 units, / 10 lot = 1.4 -> 1 lot
            // When
            int quantity = strategy.calculateQuantityFromDepositPercent(NLMK, 1.0, 71.2, 100_000.0);

            // Then
            then(quantity).isEqualTo(1);
        }

        @Test
        @DisplayName("Should return 0 when raw quantity is below one lot")
        void shouldReturnZero_WhenBelowMinLot() {
            // Given: 0.5% of 100_000 = 500, / 71.2 = 7.02 units < 10 lot
            // When
            int quantity = strategy.calculateQuantityFromDepositPercent(NLMK, 0.5, 71.2, 100_000.0);

            // Then
            then(quantity).isZero();
        }

        @Test
        @DisplayName("Should return 0 for lot=1 instrument when deposit buys less than one unit")
        void shouldReturnZero_WhenLotOneAndBelowOneUnit() {
            // Given: 1% of 100_000 = 1000, / 3794 = 0.26 units < 1 lot (YDEX case)
            // When
            int quantity = strategy.calculateQuantityFromDepositPercent(T, 1.0, 3794.0, 100_000.0);

            // Then
            then(quantity).isZero();
        }

        @Test
        @DisplayName("Should return 0 for null or non-positive deposit percent")
        void shouldReturnZero_WhenInvalidDepositPercent() {
            then(strategy.calculateQuantityFromDepositPercent(NLMK, null, 71.2, 100_000.0)).isZero();
            then(strategy.calculateQuantityFromDepositPercent(NLMK, 0.0, 71.2, 100_000.0)).isZero();
            then(strategy.calculateQuantityFromDepositPercent(NLMK, -1.0, 71.2, 100_000.0)).isZero();
        }

        @Test
        @DisplayName("Should return 0 for non-positive entry price or balance")
        void shouldReturnZero_WhenInvalidPriceOrBalance() {
            then(strategy.calculateQuantityFromDepositPercent(NLMK, 1.0, 0.0, 100_000.0)).isZero();
            then(strategy.calculateQuantityFromDepositPercent(NLMK, 1.0, 71.2, 0.0)).isZero();
        }

        @Test
        @DisplayName("Should return 0 when ticker info is missing")
        void shouldReturnZero_WhenTickerNotFound() {
            then(strategy.calculateQuantityFromDepositPercent("UNKNOWN", 1.0, 71.2, 100_000.0)).isZero();
        }
    }

    @Nested
    @DisplayName("positionSizeToDepositPercent")
    class PositionSizeToDepositPercent {

        @Test
        @DisplayName("Should map FullCapital to 100% of deposit")
        void shouldMap_FullCapital() {
            then(strategy.positionSizeToDepositPercent("FullCapital")).isEqualTo(100.0);
        }

        @Test
        @DisplayName("Should map HalfCapital to 50% of deposit")
        void shouldMap_HalfCapital() {
            then(strategy.positionSizeToDepositPercent("HalfCapital")).isEqualTo(50.0);
        }

        @Test
        @DisplayName("Should map SmallPosition to 30% of deposit")
        void shouldMap_SmallPosition() {
            then(strategy.positionSizeToDepositPercent("SmallPosition")).isEqualTo(30.0);
        }

        @Test
        @DisplayName("Should default to 30% for missing or unknown labels")
        void shouldDefault_ToSmallPosition() {
            then(strategy.positionSizeToDepositPercent(null)).isEqualTo(30.0);
            then(strategy.positionSizeToDepositPercent("Unknown")).isEqualTo(30.0);
        }
    }

    @Nested
    @DisplayName("calculateFuturesQuantity with margin (ГО)")
    class FuturesMargin {

        @Test
        @DisplayName("Should use margin from broker API when available")
        void shouldUseMarginFromApi() throws Exception {
            // Given: futures with margin = 5000 RUB, deposit = 2%, balance = 100_000
            // Expected: 100_000 * 0.02 = 2000 / 5500 (with 10% buffer) = 0.36 -> 0 lots
            TradeCouncilStrategy strategyWithMargin = new TradeCouncilStrategy(
                new UnifiedTraderConfig(),
                new TradingService() {
                    @Override
                    public Double getSingleContractGo(String figi) {
                        return 5000.0; // margin = 5000 RUB
                    }
                },
                new Config()
            );

            int quantity = strategyWithMargin.calculateFuturesQuantity("GYENF", 2.0, 100.0, 100_000.0, "GYENF");
            then(quantity).isEqualTo(0);
        }

        @Test
        @DisplayName("Should use fallback 25% of price when margin unavailable")
        void shouldUseFallback_WhenMarginNull() throws Exception {
            // Given: futures price = 100_000, fallback margin = 25_000, deposit = 10%, balance = 100_000
            // Expected: 100_000 * 0.10 = 10_000 / 27_500 (with 10% buffer) = 0.36 -> 0 lots
            TradeCouncilStrategy strategyWithFallback = new TradeCouncilStrategy(
                new UnifiedTraderConfig(),
                new TradingService() {
                    @Override
                    public Double getSingleContractGo(String figi) {
                        return null; // API unavailable
                    }
                },
                new Config()
            );

            int quantity = strategyWithFallback.calculateFuturesQuantity("TESTF", 10.0, 100_000.0, 100_000.0, "TESTF");
            then(quantity).isZero();
        }

        @Test
        @DisplayName("Should apply 10% buffer to margin for conservative risk")
        void shouldApplyBuffer() throws Exception {
            // Given: margin = 1000, deposit = 5%, balance = 100_000
            // Expected: 100_000 * 0.05 = 5000 / 1100 (with 10% buffer) = 4.54 -> 4 lots
            TradeCouncilStrategy strategyWithBuffer = new TradeCouncilStrategy(
                new UnifiedTraderConfig(),
                new TradingService() {
                    @Override
                    public Double getSingleContractGo(String figi) {
                        return 1000.0;
                    }
                },
                new Config()
            );

            int quantity = strategyWithBuffer.calculateFuturesQuantity("TESTF", 5.0, 100.0, 100_000.0, "TESTF");
            then(quantity).isEqualTo(4);
        }

        @Test
        @DisplayName("Should return 0 for invalid parameters")
        void shouldReturnZero_WhenInvalidParams() throws Exception {
            TradeCouncilStrategy strategyWithMargin = new TradeCouncilStrategy(
                new UnifiedTraderConfig(),
                new TradingService() {
                    @Override
                    public Double getSingleContractGo(String figi) {
                        return 1000.0;
                    }
                },
                new Config()
            );

            then(strategyWithMargin.calculateFuturesQuantity("TESTF", null, 100.0, 100_000.0, "TESTF")).isZero();
            then(strategyWithMargin.calculateFuturesQuantity("TESTF", 0.0, 100.0, 100_000.0, "TESTF")).isZero();
            then(strategyWithMargin.calculateFuturesQuantity("TESTF", -1.0, 100.0, 100_000.0, "TESTF")).isZero();
            then(strategyWithMargin.calculateFuturesQuantity("TESTF", 1.0, 0.0, 100_000.0, "TESTF")).isZero();
            then(strategyWithMargin.calculateFuturesQuantity("TESTF", 1.0, 100.0, 0.0, "TESTF")).isZero();
        }

        @Test
        @DisplayName("Should calculate meaningful quantity with sufficient capital")
        void shouldCalculatePositiveQuantity() throws Exception {
            // Given: margin = 1000, deposit = 10%, balance = 500_000
            // Expected: 500_000 * 0.10 = 50_000 / 1100 (with 10% buffer) = 45.45 -> 45 lots
            TradeCouncilStrategy strategyWithMargin = new TradeCouncilStrategy(
                new UnifiedTraderConfig(),
                new TradingService() {
                    @Override
                    public Double getSingleContractGo(String figi) {
                        return 1000.0;
                    }
                },
                new Config()
            );

            int quantity = strategyWithMargin.calculateFuturesQuantity("TESTF", 10.0, 100.0, 500_000.0, "TESTF");
            then(quantity).isEqualTo(45);
        }
    }
}