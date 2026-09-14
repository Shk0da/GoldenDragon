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
        strategy = new TradeCouncilStrategy(new UnifiedTraderConfig(), new TradingService() {}, new Config());
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
}