package com.github.shk0da.goldendragon.money;

import com.github.shk0da.goldendragon.market.MarketDataProvider;
import com.github.shk0da.goldendragon.market.MarketPrices;
import com.github.shk0da.goldendragon.model.Candle;
import com.github.shk0da.goldendragon.model.PositionInfo;
import com.github.shk0da.goldendragon.model.TickerInfo;
import com.github.shk0da.goldendragon.model.TickerType;
import com.github.shk0da.goldendragon.service.TradingService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import static org.assertj.core.api.BDDAssertions.then;
import static org.assertj.core.api.Assertions.within;

@DisplayName("TmonCashParkingMonitor order creation with lot size")
class TmonCashParkingMonitorTest {

    private static final String TMON = "TMON@";
    private static final int TMON_LOT = 1;
    private static final double TMON_PRICE = 100.0;

    private FakeTradingService tradingService;
    private FakeMarketDataProvider marketDataProvider;
    private TmonCashParkingMonitor monitor;

    @BeforeEach
    void setUp() {
        tradingService = new FakeTradingService();
        marketDataProvider = new FakeMarketDataProvider();
        CashParkingManager cashParkingManager = new CashParkingManager(
                tradingService, marketDataProvider, new ConcurrentHashMap<>());
        monitor = new TmonCashParkingMonitor(
                tradingService, marketDataProvider, cashParkingManager, new ConcurrentHashMap<>());
    }

    @Nested
    @DisplayName("When buying TMON@")
    class Buy {

        @Test
        @DisplayName("Should compute buyLots = floor(usableCash / effectiveCostPerLot) and pass totalCost")
        void shouldComputeBuyLotsWithLotSize() {
            // Given: usableCash = 100_000 * 0.95 = 95_000
            // effectiveCostPerLot = 100 * 1.01 * 1 = 101
            // buyLots = floor(95000 / 101) = 940
            // totalCost = 940 * 100 * 1 = 94_000
            tradingService.cash = 100_000.0;
            marketDataProvider.askPrice = TMON_PRICE;

            monitor.monitorAndBuyTmon();

            then(tradingService.lastBuyValue).isCloseTo(94_000.0, within(0.01));
            then(tradingService.lastBuyTicker).isEqualTo(TMON);
        }

        @Test
        @DisplayName("Should skip when available cash is too low for even one lot")
        void shouldSkipWhenInsufficientCash() {
            // Given: usableCash = 50 * 0.95 = 47.5, effectiveCostPerLot = 101 → skip
            tradingService.cash = 50.0;
            marketDataProvider.askPrice = TMON_PRICE;

            monitor.monitorAndBuyTmon();

            then(tradingService.lastBuyValue).isZero();
        }

        @Test
        @DisplayName("Should skip when parking position already exists")
        void shouldSkipWhenPositionExists() {
            // Given: parkingInfo has balance > 0
            tradingService.cash = 100_000.0;
            tradingService.parkingInfo = new PositionInfo(
                    "FIGI", TMON, "ISIN", "ETF", 100, 0.0, 100, TMON_PRICE, TMON);
            marketDataProvider.askPrice = TMON_PRICE;

            monitor.monitorAndBuyTmon();

            then(tradingService.lastBuyValue).isZero();
        }
    }

    private static class FakeTradingService implements TradingService {

        double cash;
        double lastBuyValue;
        String lastBuyTicker;
        PositionInfo parkingInfo;

        @Override
        public Double getAvailableCash() {
            return cash;
        }

        @Override
        public PositionInfo getCurrentPositions(TickerType tickerType, String tickerName) {
            return parkingInfo;
        }

        @Override
        public TickerInfo searchTicker(TickerInfo.Key key) {
            return new TickerInfo(
                    "FIGI_TMON", TMON, "ISIN_TMON", 0.01, TMON_LOT, "RUB", TMON, "ETF");
        }

        @Override
        public OrderExecutionResult buyByMarketWithDetails(
                String name,
                TickerType type,
                double cashToBuy,
                double takeProfit,
                double stopLose) {
            lastBuyValue = cashToBuy;
            lastBuyTicker = name;
            return OrderExecutionResult.testSuccess(0.0, 0);
        }
    }

    private static class FakeMarketDataProvider implements MarketDataProvider {

        Double askPrice;

        @Override
        public List<Candle> getCandles(String ticker, String interval) {
            return List.of();
        }

        @Override
        public MarketPrices getLivePrices(String ticker) {
            if (askPrice != null) {
                return new MarketPrices(askPrice - 0.1, askPrice);
            }
            return null;
        }
    }
}
