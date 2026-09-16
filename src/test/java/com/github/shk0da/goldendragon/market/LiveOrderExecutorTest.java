package com.github.shk0da.goldendragon.market;

import com.github.shk0da.goldendragon.model.OrderExecutionResult;
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

@DisplayName("LiveOrderExecutor order creation with lot size")
class LiveOrderExecutorTest {

    private static final String NLMK = "NLMK";
    private static final int NLMK_LOT = 10;
    private static final double ASK_PRICE = 100.0;

    private FakeTradingService tradingService;
    private LiveOrderExecutor executor;

    @BeforeEach
    void setUp() {
        TickerRepository.INSTANCE.putAll(
                Map.of(
                        new TickerInfo.Key(NLMK, TickerType.STOCK),
                        new TickerInfo("FIGI_NLMK", NLMK, "ISIN_NLMK", 0.01, NLMK_LOT, "RUB", NLMK, "STOCK")));
        tradingService = new FakeTradingService();
        tradingService.cash = 1_000_000.0;
        tradingService.askPrice = ASK_PRICE;
        executor = new LiveOrderExecutor(tradingService);
    }

    @Nested
    @DisplayName("When buying")
    class Buy {

        @Test
        @DisplayName("Should compute order value as quantity * ask * lot * safety margin")
        void shouldComputeValueWithLotSize() {
            // Given: 10 lots * 100 * 10 lot size * 1.01 = 10100
            // When
            OrderExecutor.ExecutionResult result = executor.buy(NLMK, 10, 2.0, 4.0);

            // Then
            then(result.isSuccess()).isTrue();
            then(tradingService.lastBuyValue).isEqualTo(10 * ASK_PRICE * NLMK_LOT * 1.01);
            then(tradingService.lastBuySl).isEqualTo(2.0);
            then(tradingService.lastBuyTp).isEqualTo(4.0);
        }

        @Test
        @DisplayName("Should cap quantity when cash is insufficient")
        void shouldCapQuantity_WhenInsufficientCash() {
            // Given: cash 5000 < 10100 -> maxQuantity = 5000 / (100 * 10 * 1.01) = 4 lots
            tradingService.cash = 5000.0;

            // When
            OrderExecutor.ExecutionResult result = executor.buy(NLMK, 10, 2.0, 4.0);

            // Then
            then(result.isSuccess()).isTrue();
            then(tradingService.lastBuyValue).isEqualTo(4 * ASK_PRICE * NLMK_LOT * 1.01);
        }

        @Test
        @DisplayName("Should fail when cash cannot afford even one lot")
        void shouldFail_WhenCashBelowOneLot() {
            // Given: cash 500 < 1010 (one lot with margin)
            tradingService.cash = 500.0;

            // When
            OrderExecutor.ExecutionResult result = executor.buy(NLMK, 10, 2.0, 4.0);

            // Then
            then(result.isSuccess()).isFalse();
        }

        @Test
        @DisplayName("Should fail when ticker is not found")
        void shouldFail_WhenTickerNotFound() {
            // When
            OrderExecutor.ExecutionResult result = executor.buy("UNKNOWN", 10, 2.0, 4.0);

            // Then
            then(result.isSuccess()).isFalse();
        }
    }

    @Nested
    @DisplayName("When selling")
    class Sell {

        @Test
        @DisplayName("Should compute position value as quantity * ask * lot")
        void shouldComputeValueWithLotSize() {
            // Given: 10 lots * 100 * 10 lot size = 10000
            // When
            OrderExecutor.ExecutionResult result = executor.sell(NLMK, 10, 2.0, 4.0);

            // Then
            then(result.isSuccess()).isTrue();
            then(tradingService.lastSellValue).isEqualTo(10 * ASK_PRICE * NLMK_LOT);
            then(tradingService.lastSellSl).isEqualTo(2.0);
            then(tradingService.lastSellTp).isEqualTo(4.0);
        }

        @Test
        @DisplayName("Should cap quantity when cash is insufficient")
        void shouldCapQuantity_WhenInsufficientCash() {
            // Given: cash 5000 < 10000 -> maxQuantity = 5000 / (100 * 10) = 5 lots
            tradingService.cash = 5000.0;

            // When
            OrderExecutor.ExecutionResult result = executor.sell(NLMK, 10, 2.0, 4.0);

            // Then
            then(result.isSuccess()).isTrue();
            then(tradingService.lastSellValue).isEqualTo(5 * ASK_PRICE * NLMK_LOT);
        }

        @Test
        @DisplayName("Should fail when cash cannot afford even one lot")
        void shouldFail_WhenCashBelowOneLot() {
            // Given: cash 500 < 1000 (one lot)
            tradingService.cash = 500.0;

            // When
            OrderExecutor.ExecutionResult result = executor.sell(NLMK, 10, 2.0, 4.0);

            // Then
            then(result.isSuccess()).isFalse();
        }

        @Test
        @DisplayName("Should fail when ticker is not found")
        void shouldFail_WhenTickerNotFound() {
            // When
            OrderExecutor.ExecutionResult result = executor.sell("UNKNOWN", 10, 2.0, 4.0);

            // Then
            then(result.isSuccess()).isFalse();
        }
    }

    /** Minimal TradingService fake recording order values and SL/TP percents. */
    private static class FakeTradingService implements TradingService {

        double cash;
        double askPrice;
        double lastBuyValue;
        Double lastBuySl;
        Double lastBuyTp;
        double lastSellValue;
        Double lastSellSl;
        Double lastSellTp;

        @Override
        public Double getAvailableCash() {
            return cash;
        }

        @Override
        public double getLiveAskPrice(TickerInfo.Key key) {
            return askPrice;
        }

        @Override
        public OrderExecutionResult buyByMarketWithDetails(
                String name, TickerType type, double cashToBuy, double takeProfit, double stopLose) {
            lastBuyValue = cashToBuy;
            lastBuyTp = takeProfit;
            lastBuySl = stopLose;
            return OrderExecutionResult.testSuccess(ASK_PRICE, 10);
        }

        @Override
        public OrderExecutionResult sellByMarketWithDetails(
                String name, TickerType type, double cashToSell, double takeProfit, double stopLose) {
            lastSellValue = cashToSell;
            lastSellTp = takeProfit;
            lastSellSl = stopLose;
            return OrderExecutionResult.testSuccess(ASK_PRICE, 10);
        }
    }
}