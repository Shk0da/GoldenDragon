package com.github.shk0da.goldendragon.money;

import com.github.shk0da.goldendragon.model.OrderExecutionResult;
import com.github.shk0da.goldendragon.model.Position;
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

import static org.assertj.core.api.Assertions.within;
import static org.assertj.core.api.BDDAssertions.then;

@DisplayName("CashParkingManager")
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

            tradingService.parkingInfo = new PositionInfo(
                    "FIGI", TMON, "ISIN", "ETF", 100, 0.0, 100, TMON_PRICE, TMON);

            manager.sellParkingToFreeCash(5000, "NLMK");

            then(tradingService.lastSellValue).isCloseTo(5000.0, within(0.001));
            then(tradingService.lastSellTicker).isEqualTo(TMON);
        }

        @Test
        @DisplayName("Should cap to available parking when requested exceeds available")
        void shouldCapToAvailable() {

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

            tradingService.parkingInfo = null;

            manager.sellParkingToFreeCash(5000, "NLMK");

            then(tradingService.lastSellValue).isZero();
        }
    }

    @Nested
    @DisplayName("getParkingPosition")
    class GetParkingPosition {

        @Test
        @DisplayName("Should return parking position when exists")
        void shouldReturnParkingPosition() {

            tradingService.parkingInfo = new PositionInfo(
                    "FIGI", TMON, "ISIN", "ETF", 100, 0.0, 100, TMON_PRICE, TMON);


            PositionInfo result = manager.getParkingPosition();


            then(result).isNotNull();
            then(result.getTicker()).isEqualTo(TMON);
            then(result.getBalance()).isEqualTo(100);
        }

        @Test
        @DisplayName("Should return null when parking disabled")
        void shouldReturnNull_WhenParkingDisabled() {

            CashParkingManager disabledManager = new CashParkingManager(tradingService, null, new ConcurrentHashMap<>()) {
                @Override
                public boolean isParkingEnabled() {
                    return false;
                }
            };


            PositionInfo result = disabledManager.getParkingPosition();


            then(result).isNull();
        }

        @Test
        @DisplayName("Should return null when exception occurs")
        void shouldReturnNull_WhenException() {

            tradingService.throwException = true;


            PositionInfo result = manager.getParkingPosition();


            then(result).isNull();
        }
    }

    @Nested
    @DisplayName("getParkingValue")
    class GetParkingValue {

        @Test
        @DisplayName("Should calculate parking value correctly")
        void shouldCalculateParkingValue() {

            tradingService.parkingInfo = new PositionInfo(
                    "FIGI", TMON, "ISIN", "ETF", 100, 0.0, 100, TMON_PRICE, TMON);


            double result = manager.getParkingValue();


            then(result).isEqualTo(10000.0);
        }

        @Test
        @DisplayName("Should return 0 when no parking position")
        void shouldReturnZero_WhenNoParking() {

            tradingService.parkingInfo = null;


            double result = manager.getParkingValue();


            then(result).isZero();
        }

        @Test
        @DisplayName("Should return 0 when price is null")
        void shouldReturnZero_WhenPriceNull() {

            tradingService.parkingInfo = new PositionInfo(
                    "FIGI", TMON, "ISIN", "ETF", 100, 0.0, 100, null, TMON);


            double result = manager.getParkingValue();


            then(result).isZero();
        }
    }

    @Nested
    @DisplayName("closeParkingPosition")
    class CloseParkingPosition {

        @Test
        @DisplayName("Should close long position")
        void shouldCloseLong() {

            tradingService.parkingInfo = new PositionInfo(
                    "FIGI", TMON, "ISIN", "ETF", 100, 0.0, 100, TMON_PRICE, TMON);


            manager.closeParkingPosition();


            then(tradingService.closedLong).isTrue();
        }

        @Test
        @DisplayName("Should close parking position when enabled")
        void shouldCloseParkingPosition() {

            tradingService.parkingInfo = new PositionInfo(
                    "FIGI", TMON, "ISIN", "ETF", 100, 0.0, 100, TMON_PRICE, TMON);


            manager.closeParkingPosition();


            then(tradingService.closedLong).isTrue();
        }

        @Test
        @DisplayName("Should handle exception gracefully")
        void shouldHandleException() {

            tradingService.throwExceptionOnClose = true;
            tradingService.parkingInfo = new PositionInfo(
                    "FIGI", TMON, "ISIN", "ETF", 100, 0.0, 100, TMON_PRICE, TMON);


            manager.closeParkingPosition();


            then(tradingService.closedLong).isFalse();
        }
    }

    private static class FakeTradingService implements TradingService {

        PositionInfo parkingInfo;
        double lastSellValue;
        String lastSellTicker;
        TickerInfo tickerInfo;
        boolean throwException = false;
        boolean throwExceptionOnClose = false;
        boolean closedLong = false;

        FakeTradingService() {
            this.tickerInfo = new TickerInfo("FIGI", "TICKER", "ISIN", 0.01, 1, "RUB", "TICKER", "ETF");
        }

        @Override
        public PositionInfo getCurrentPositions(TickerType tickerType, String tickerName) {
            if (throwException) {
                throw new RuntimeException("Test exception");
            }
            return parkingInfo;
        }

        @Override
        public TickerInfo searchTicker(TickerInfo.Key key) {
            return tickerInfo;
        }

        @Override
        public OrderExecutionResult sellByMarketWithDetails(
                String name,
                TickerType type,
                double cashToSell,
                double takeProfit,
                double stopLose) {
            if (throwException) {
                throw new RuntimeException("Test exception");
            }
            lastSellValue = cashToSell;
            lastSellTicker = name;
            return OrderExecutionResult.testSuccess(0.0, 0);
        }

        @Override
        public boolean closeLongByMarket(String ticker, TickerType type) {
            if (throwExceptionOnClose) {
                throw new RuntimeException("Test exception");
            }
            closedLong = true;
            return true;
        }
    }
}
