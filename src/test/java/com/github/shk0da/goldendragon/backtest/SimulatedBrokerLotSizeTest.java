package com.github.shk0da.goldendragon.backtest;

import com.github.shk0da.goldendragon.market.OrderExecutor;
import com.github.shk0da.goldendragon.model.Candle;
import com.github.shk0da.goldendragon.model.TickerInfo;
import com.github.shk0da.goldendragon.model.TickerType;
import com.github.shk0da.goldendragon.repository.TickerRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.within;
import static org.assertj.core.api.BDDAssertions.then;

@DisplayName("SimulatedBroker order creation with lot size")
class SimulatedBrokerLotSizeTest {

    private static final String NLMK = "NLMK";
    private static final int NLMK_LOT = 10;
    private static final double PRICE = 100.0;
    private static final double COMMISSION_RATE = 0.0005;
    private static final DateTimeFormatter FMT = DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm:ss");

    private SimulatedBroker broker;

    @BeforeEach
    void setUp() {
        TickerRepository.INSTANCE.putAll(
                Map.of(
                        new TickerInfo.Key(NLMK, TickerType.STOCK),
                        new TickerInfo("FIGI_NLMK", NLMK, "ISIN_NLMK", 0.01, NLMK_LOT, "RUB", NLMK, "STOCK")));
        broker = new SimulatedBroker(1_000_000.0, COMMISSION_RATE, 0.0);
        broker.loadCandles(
                NLMK,
                "5_MIN",
                List.of(new Candle("01.01.2026 10:00:00", PRICE, PRICE + 1, PRICE - 1, PRICE, 1000)));
        broker.setCurrentTime(LocalDateTime.parse("01.01.2026 10:00:00", FMT));
    }

    @Nested
    @DisplayName("When buying")
    class Buy {

        @Test
        @DisplayName("Should charge notional = quantity * lotSize * price and store quantity in lots")
        void shouldChargeNotionalWithLotSize() {
            // Given: 5 lots * 10 lot size * 100 = 5000, commission = 5050 (with margin) * 0.0005 = 2.525
            // When
            OrderExecutor.ExecutionResult result = broker.buy(NLMK, 5, null, null);

            // Then
            then(result.isSuccess()).isTrue();
            then(broker.getSharedCash()).isCloseTo(1_000_000.0 - 5000.0 - 2.525, within(0.001));
            then(broker.getPositionState(NLMK).position.quantity).isEqualTo(5);
        }

        @Test
        @DisplayName("Should fail when cash cannot cover notional plus commission")
        void shouldFail_WhenInsufficientCash() {
            // Given: 5 lots need 5050 + 2.525, cash only 4000
            broker = new SimulatedBroker(4000.0, COMMISSION_RATE, 0.0);
            broker.loadCandles(
                    NLMK,
                    "5_MIN",
                    List.of(new Candle("01.01.2026 10:00:00", PRICE, PRICE + 1, PRICE - 1, PRICE, 1000)));
            broker.setCurrentTime(LocalDateTime.parse("01.01.2026 10:00:00", FMT));

            // When
            OrderExecutor.ExecutionResult result = broker.buy(NLMK, 5, null, null);

            // Then
            then(result.isSuccess()).isFalse();
        }

        @Test
        @DisplayName("Should apply default SL 2% and TP 4% when none provided")
        void shouldApplyDefaultSlTp() {
            // When
            OrderExecutor.ExecutionResult result = broker.buy(NLMK, 5, null, null);

            // Then
            then(result.isSuccess()).isTrue();
            then(broker.getPositionState(NLMK).position.stopLoss).isCloseTo(PRICE * 0.98, within(0.001));
            then(broker.getPositionState(NLMK).position.takeProfit).isCloseTo(PRICE * 1.04, within(0.001));
        }

        @Test
        @DisplayName("Should store provided SL/TP prices")
        void shouldStoreProvidedSlTp() {
            // When
            OrderExecutor.ExecutionResult result = broker.buy(NLMK, 5, 95.0, 110.0);

            // Then
            then(result.isSuccess()).isTrue();
            then(broker.getPositionState(NLMK).position.stopLoss).isEqualTo(95.0);
            then(broker.getPositionState(NLMK).position.takeProfit).isEqualTo(110.0);
        }
    }

    @Nested
    @DisplayName("When selling")
    class Sell {

        @Test
        @DisplayName("Should charge margin + commission from notional = quantity * lotSize * price")
        void shouldChargeMarginAndCommissionWithLotSize() {
            // Given: 5 lots * 10 lot size * 100 = 5000
            // entryNotional (with safety) = 5050, then adjusted back to 5000
            // marginRequired = 5000 * 0.30 = 1500
            // commission = 5050 * 0.0005 = 2.525
            // sharedCash = 1_000_000 - 1500 - 2.525
            // When
            OrderExecutor.ExecutionResult result = broker.sell(NLMK, 5, null, null);

            // Then
            then(result.isSuccess()).isTrue();
            then(broker.getSharedCash()).isCloseTo(1_000_000.0 - 1500.0 - 2.525, within(0.001));
            then(broker.getPositionState(NLMK).position.quantity).isEqualTo(5);
        }

        @Test
        @DisplayName("Should fail when margin plus commission exceeds available cash")
        void shouldFail_WhenInsufficientCash() {
            // Given: marginRequired + commission = 5050*0.30 + 2.525 = 1517.525, cash only 1000
            broker = new SimulatedBroker(1000.0, COMMISSION_RATE, 0.0);
            broker.loadCandles(
                    NLMK,
                    "5_MIN",
                    List.of(new Candle("01.01.2026 10:00:00", PRICE, PRICE + 1, PRICE - 1, PRICE, 1000)));
            broker.setCurrentTime(LocalDateTime.parse("01.01.2026 10:00:00", FMT));

            // When
            OrderExecutor.ExecutionResult result = broker.sell(NLMK, 5, null, null);

            // Then
            then(result.isSuccess()).isFalse();
        }

        @Test
        @DisplayName("Should apply default SL 2% above and TP 4% below for short when none provided")
        void shouldApplyDefaultSlTp() {
            // When
            OrderExecutor.ExecutionResult result = broker.sell(NLMK, 5, null, null);

            // Then
            then(result.isSuccess()).isTrue();
            then(broker.getPositionState(NLMK).position.stopLoss).isCloseTo(PRICE * 1.02, within(0.001));
            then(broker.getPositionState(NLMK).position.takeProfit).isCloseTo(PRICE * 0.96, within(0.001));
        }

        @Test
        @DisplayName("Should store provided SL/TP prices")
        void shouldStoreProvidedSlTp() {
            // When
            OrderExecutor.ExecutionResult result = broker.sell(NLMK, 5, 105.0, 90.0);

            // Then
            then(result.isSuccess()).isTrue();
            then(broker.getPositionState(NLMK).position.stopLoss).isEqualTo(105.0);
            then(broker.getPositionState(NLMK).position.takeProfit).isEqualTo(90.0);
        }
    }
}
