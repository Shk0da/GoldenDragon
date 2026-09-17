package com.github.shk0da.goldendragon.money;

import com.github.shk0da.goldendragon.market.MarketDataProvider;
import com.github.shk0da.goldendragon.market.MarketPrices;
import com.github.shk0da.goldendragon.model.Candle;
import com.github.shk0da.goldendragon.model.OrderExecutionResult;
import com.github.shk0da.goldendragon.model.Position;
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
    private ConcurrentHashMap<String, Position> positionStore;

    @BeforeEach
    void setUp() {
        tradingService = new FakeTradingService();
        tradingService.tickerInfo = new TickerInfo(
                "FIGI_TMON", TMON, "ISIN_TMON", 0.01, TMON_LOT, "RUB", TMON, "ETF");
        marketDataProvider = new FakeMarketDataProvider();
        positionStore = new ConcurrentHashMap<>();
        CashParkingManager cashParkingManager = new CashParkingManager(
                tradingService, marketDataProvider, positionStore);
        monitor = new TmonCashParkingMonitor(
                tradingService, marketDataProvider, cashParkingManager, positionStore);
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
            tradingService.askPrice = TMON_PRICE;

            monitor.monitorAndBuyTmon();

            then(tradingService.lastBuyValue).isCloseTo(94_000.0, within(0.01));
            then(tradingService.lastBuyTicker).isEqualTo(TMON);
        }

        @Test
        @DisplayName("Should skip when available cash is too low for even one lot")
        void shouldSkipWhenInsufficientCash() {
            // Given: usableCash = 50 * 0.95 = 47.5, effectiveCostPerLot = 101 → skip
            tradingService.cash = 50.0;
            tradingService.askPrice = TMON_PRICE;

            monitor.monitorAndBuyTmon();

            then(tradingService.lastBuyValue).isZero();
        }

        @Test
        @DisplayName("Should skip when trading is in progress")
        void shouldSkipWhenTradingInProgress() {
            tradingService.cash = 100_000.0;
            tradingService.askPrice = TMON_PRICE;
            monitor.setTradingInProgress(true);

            monitor.monitorAndBuyTmon();

            then(tradingService.lastBuyValue).isZero();
        }

        @Test
        @DisplayName("Should skip when parking is disabled")
        void shouldSkipWhenParkingDisabled() {
            CashParkingManager disabledManager = new CashParkingManager(
                    tradingService, marketDataProvider, positionStore) {
                @Override
                public boolean isParkingEnabled() {
                    return false;
                }
            };
            TmonCashParkingMonitor disabledMonitor = new TmonCashParkingMonitor(
                    tradingService, marketDataProvider, disabledManager, positionStore);
            tradingService.cash = 100_000.0;
            tradingService.askPrice = TMON_PRICE;

            disabledMonitor.monitorAndBuyTmon();

            then(tradingService.lastBuyValue).isZero();
        }

        @Test
        @DisplayName("Should skip when ticker info is not found")
        void shouldSkipWhenTickerInfoNotFound() {
            tradingService.cash = 100_000.0;
            tradingService.askPrice = TMON_PRICE;
            tradingService.tickerInfo = null;

            monitor.monitorAndBuyTmon();

            then(tradingService.lastBuyValue).isZero();
        }

        @Test
        @DisplayName("Should skip when price is not available")
        void shouldSkipWhenPriceNotAvailable() {
            tradingService.cash = 100_000.0;
            tradingService.askPrice = null;

            monitor.monitorAndBuyTmon();

            then(tradingService.lastBuyValue).isZero();
        }

        @Test
        @DisplayName("Should respect running state")
        void shouldRespectRunningState() {
            then(monitor.isRunning()).isTrue();
            monitor.stop();
            then(monitor.isRunning()).isFalse();
        }

        @Test
        @DisplayName("Should skip parking when active non-parking positions exist")
        void shouldSkipWhenActiveNonParkingPositionsExist() {
            // Given: cash available but an active non-parking position exists in store
            tradingService.cash = 100_000.0;
            tradingService.askPrice = TMON_PRICE;
            positionStore.put("NLMK", new Position(
                    "SELL", 73.7, null, null, 5310, 0, 0, 1));

            monitor.monitorAndBuyTmon();

            then(tradingService.lastBuyValue).isZero();
        }

        @Test
        @DisplayName("Should buy more TMON when parking position exists and free cash available")
        void shouldBuyMoreWhenPositionExists() {
            // Given: parking position exists but there is free cash to top up
            tradingService.cash = 100_000.0;
            tradingService.parkingInfo = new PositionInfo(
                    "FIGI", TMON, "ISIN", "ETF", 100, 0.0, 100, TMON_PRICE, TMON);
            tradingService.askPrice = TMON_PRICE;

            monitor.monitorAndBuyTmon();

            then(tradingService.lastBuyValue).isCloseTo(94_000.0, within(0.01));
            then(tradingService.lastBuyTicker).isEqualTo(TMON);
        }
    }

    private static class FakeTradingService implements TradingService {

        double cash;
        Double askPrice;
        double lastBuyValue;
        String lastBuyTicker;
        PositionInfo parkingInfo;
        TickerInfo tickerInfo;

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
            return tickerInfo;
        }

        @Override
        public double getAvailablePrice(
                TickerInfo.Key key, int count, String type, boolean isPrintGlass) {
            if ("asks".equals(type) && askPrice != null) {
                return askPrice;
            }
            return 0.0;
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
