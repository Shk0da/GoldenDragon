package com.github.shk0da.goldendragon.money;

import com.github.shk0da.goldendragon.model.PositionInfo;
import com.github.shk0da.goldendragon.model.TickerInfo;
import com.github.shk0da.goldendragon.model.TickerType;
import com.github.shk0da.goldendragon.repository.TickerRepository;
import com.github.shk0da.goldendragon.service.TradingService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import static org.assertj.core.api.BDDAssertions.then;
import static org.assertj.core.api.Assertions.within;

@DisplayName("CashParkingManager.sellParkingToFreeCash with lot size")
class CashParkingManagerTest {

    private static final String TMON = "TMON@";
    private static final int TMON_LOT = 1;
    private static final double TMON_PRICE = 100.0;

    private FakeTradingService tradingService;
    private CashParkingManager manager;

    @BeforeEach
    void setUp() {
        TickerRepository.INSTANCE.putAll(
                Map.of(
                        new TickerInfo.Key(TMON, TickerType.ETF),
                        new TickerInfo("FIGI_TMON", TMON, "ISIN_TMON", 0.01, TMON_LOT, "RUB", TMON, "ETF")));
        tradingService = new FakeTradingService();
        manager = new CashParkingManager(tradingService, null, new ConcurrentHashMap<>());
    }

    @Nested
    @DisplayName("When parking position exists")
    class ParkingExists {

        @Test
        @DisplayName("Should sell exact lots to cover requested cash amount")
        void shouldSellExactLots() {
            // Given: 100 lots at 100 each, request 5000 → 50 lots * 100 = 5000
            tradingService.parkingInfo = new PositionInfo(
                    "FIGI", TMON, "ISIN", "ETF", 100, 0.0, 100, TMON_PRICE, TMON);

            manager.sellParkingToFreeCash(5000, "NLMK");

            then(tradingService.lastSellValue).isCloseTo(5000.0, within(0.001));
            then(tradingService.lastSellTicker).isEqualTo(TMON);
        }

        @Test
        @DisplayName("Should cap to available parking when requested exceeds available")
        void shouldCapToAvailable() {
            // Given: 100 lots at 100, request 20000 → max 100 lots * 100 = 10000
            tradingService.parkingInfo = new PositionInfo(
                    "FIGI", TMON, "ISIN", "ETF", 100, 0.0, 100, TMON_PRICE, TMON);

            manager.sellParkingToFreeCash(20000, "NLMK");

            then(tradingService.lastSellValue).isCloseTo(10000.0, within(0.001));
        }
    }

    @Nested
    @DisplayName("When no parking position")
    class NoParking {

        @Test
        @DisplayName("Should not call sell when parking balance is zero")
        void shouldNotSell() {
            // Given: no parking position
            tradingService.parkingInfo = null;

            manager.sellParkingToFreeCash(5000, "NLMK");

            then(tradingService.lastSellValue).isZero();
        }
    }

    private static class FakeTradingService implements TradingService {

        PositionInfo parkingInfo;
        double lastSellValue;
        String lastSellTicker;

        @Override
        public PositionInfo getCurrentPositions(TickerType tickerType, String tickerName) {
            return parkingInfo;
        }

        @Override
        public OrderExecutionResult sellByMarketWithDetails(
                String name,
                TickerType type,
                double cashToSell,
                double takeProfit,
                double stopLose) {
            lastSellValue = cashToSell;
            lastSellTicker = name;
            return OrderExecutionResult.testSuccess(0.0, 0);
        }
    }
}
