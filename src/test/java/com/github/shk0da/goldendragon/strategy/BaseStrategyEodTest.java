package com.github.shk0da.goldendragon.strategy;

import com.github.shk0da.goldendragon.config.UnifiedTraderConfig;
import com.github.shk0da.goldendragon.model.Candle;
import com.github.shk0da.goldendragon.model.Config;
import com.github.shk0da.goldendragon.model.Position;
import com.github.shk0da.goldendragon.model.PositionInfo;
import com.github.shk0da.goldendragon.model.TickerInfo;
import com.github.shk0da.goldendragon.model.TickerType;
import com.github.shk0da.goldendragon.money.CashParkingManager;
import com.github.shk0da.goldendragon.service.TradingService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import static org.assertj.core.api.BDDAssertions.then;
import static org.assertj.core.api.Assertions.within;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@DisplayName("BaseStrategy End-of-Day (EOD) logic")
class BaseStrategyEodTest {

    private static final String NLMK = "NLMK";
    private static final String VTBR = "VTBR";
    private static final String TMON = "TMON@";
    private static final double ASK_PRICE = 100.0;
    private static final double TMON_PRICE = 50.0;

    private FakeTradingService tradingService;
    private TestEodStrategy strategy;
    private UnifiedTraderConfig config;
    private CashParkingManager cashParkingManager;

    @BeforeEach
    void setUp() throws Exception {
        // Populate TickerRepository for findTickerInfo()
        com.github.shk0da.goldendragon.repository.TickerRepository.INSTANCE.putAll(
                Map.of(
                        new com.github.shk0da.goldendragon.model.TickerInfo.Key(NLMK, TickerType.STOCK),
                        new com.github.shk0da.goldendragon.model.TickerInfo("FIGI_NLMK", NLMK, "ISIN_NLMK", 0.01, 1, "RUB", NLMK, "STOCK"),
                        new com.github.shk0da.goldendragon.model.TickerInfo.Key(VTBR, TickerType.STOCK),
                        new com.github.shk0da.goldendragon.model.TickerInfo("FIGI_VTBR", VTBR, "ISIN_VTBR", 0.01, 1, "RUB", VTBR, "STOCK"),
                        new com.github.shk0da.goldendragon.model.TickerInfo.Key(TMON, TickerType.ETF),
                        new com.github.shk0da.goldendragon.model.TickerInfo("FIGI_TMON", TMON, "ISIN_TMON", 0.01, 1, "RUB", TMON, "ETF")));
        
        config = new UnifiedTraderConfig();
        tradingService = new FakeTradingService();
        tradingService.cash = 100_000.0;
        tradingService.askPrice = ASK_PRICE;
        
        cashParkingManager = Mockito.mock(CashParkingManager.class);
        when(cashParkingManager.isParkingEnabled()).thenReturn(true);
        when(cashParkingManager.getParkingTicker()).thenReturn(TMON);
        when(cashParkingManager.getParkingTickerType()).thenReturn(TickerType.ETF);
        when(cashParkingManager.isParkingTicker(TMON)).thenReturn(true);
        when(cashParkingManager.isParkingTicker(NLMK)).thenReturn(false);
        when(cashParkingManager.isParkingTicker(VTBR)).thenReturn(false);
        
        strategy = new TestEodStrategy(config, tradingService, new Config(), cashParkingManager);
    }

    private static List<Candle> candles() {
        return List.of(new Candle("01.01.2026 10:00:00", 100.0, 101.0, 99.0, 100.0, 1000));
    }

    @Nested
    @DisplayName("syncPositionStoreWithBroker")
    class SyncPositionStore {

        @Test
        @DisplayName("Should remove positions that don't exist on broker (closed by stop-loss)")
        void shouldRemoveStalePositions() {
            // Given: Local positionStore has VTBR, but broker doesn't (closed by SL)
            strategy.getStrategyPositionStore().put(VTBR, new Position("BUY", 50.0, 48.0, 55.0, 100, 0));
            strategy.getStrategyPositionStore().put(NLMK, new Position("BUY", 100.0, 98.0, 105.0, 50, 0));
            // Broker: VTBR already closed (null), NLMK still open
            tradingService.brokerPositions.remove(VTBR);
            tradingService.brokerPositions.put(NLMK, new PositionInfo("FIGI", NLMK, "ISIN", "STOCK", 50, 0.0, 1, 100.0, NLMK));

            // When
            strategy.syncPositionStoreWithBroker();

            // Then: VTBR removed, NLMK kept
            then(strategy.getStrategyPositionStore()).doesNotContainKey(VTBR);
            then(strategy.getStrategyPositionStore()).containsKey(NLMK);
        }

        @Test
        @DisplayName("Should update position quantity when broker quantity differs")
        void shouldUpdateQuantityMismatch() {
            // Given: Local qty=100, broker qty=50 (partial close)
            strategy.getStrategyPositionStore().put(NLMK, new Position("BUY", 100.0, 98.0, 105.0, 100, 0));
            tradingService.brokerPositions.put(NLMK, new PositionInfo("FIGI", NLMK, "ISIN", "STOCK", 50, 0.0, 1, 100.0, NLMK));

            // When
            strategy.syncPositionStoreWithBroker();

            // Then: Quantity updated to match broker
            then(strategy.getStrategyPositionStore().get(NLMK).quantity).isEqualTo(50);
        }

        @Test
        @DisplayName("Should skip TMON@ parking ticker")
        void shouldSkipParkingTicker() {
            // Given: TMON@ in positionStore
            strategy.getStrategyPositionStore().put(TMON, new Position("BUY", 50.0, null, null, 1000, 0));
            // Broker doesn't have TMON (not yet bought)
            tradingService.brokerPositions.remove(TMON);

            // When
            strategy.syncPositionStoreWithBroker();

            // Then: TMON@ kept in positionStore (not removed)
            then(strategy.getStrategyPositionStore()).containsKey(TMON);
        }

        @Test
        @DisplayName("Should handle exception gracefully")
        void shouldHandleException() {
            // Given: Exception when getting broker position
            tradingService.throwExceptionOnGetPosition = true;
            strategy.getStrategyPositionStore().put(NLMK, new Position("BUY", 100.0, 98.0, 105.0, 50, 0));

            // When
            strategy.syncPositionStoreWithBroker();

            // Then: Position kept, no crash
            then(strategy.getStrategyPositionStore()).containsKey(NLMK);
        }
    }

    @Nested
    @DisplayName("buyTmonWithAllCash")
    class BuyTmonEod {

        @Test
        @DisplayName("Should buy TMON@ with 95% of available cash")
        void shouldBuyTmonWithMostCash() {
            // Given: 100_000 cash, TMON@ price=50, lot=1
            // Expected: usableCash = 95_000, effectivePrice = 50.5, effectiveCostPerLot = 50.5
            // buyLots = floor(95_000 / 50.5) = 1881 lots
            // totalCost = 1881 * 50 * 1 = 94_050
            tradingService.cash = 100_000.0;
            tradingService.askPrice = TMON_PRICE;
            tradingService.executedCount = 1881;

            // When
            strategy.buyTmonWithAllCash();

            // Then: Bought ~1881 lots for ~94_050
            then(tradingService.lastBuyValue).isCloseTo(94_050.0, within(1_000.0));
            then(tradingService.lastBuyTicker).isEqualTo(TMON);
        }

        @Test
        @DisplayName("Should skip if no cash available")
        void shouldSkipIfNoCash() {
            // Given: Zero cash
            tradingService.cash = 0.0;

            // When
            strategy.buyTmonWithAllCash();

            // Then: No buy executed
            then(tradingService.lastBuyTicker).isNull();
        }

        @Test
        @DisplayName("Should skip if parking disabled")
        void shouldSkipIfParkingDisabled() {
            when(cashParkingManager.isParkingEnabled()).thenReturn(false);

            // When
            strategy.buyTmonWithAllCash();

            // Then: No buy executed
            then(tradingService.lastBuyTicker).isNull();
        }

        @Test
        @DisplayName("Should handle insufficient cash for 1 lot")
        void shouldSkipIfInsufficientForOneLot() {
            // Given: Very little cash (not enough for 1 lot)
            tradingService.cash = 50.0;
            tradingService.askPrice = 100.0;

            // When
            strategy.buyTmonWithAllCash();

            // Then: No buy executed
            then(tradingService.lastBuyTicker).isNull();
        }
    }

    @Nested
    @DisplayName("closeAllPositions")
    class CloseAllPositionsEod {

        @Test
        @DisplayName("Should skip TMON@ parking ticker")
        void shouldSkipParkingTicker() {
            // Given: TMON@ and NLMK in positionStore
            strategy.getStrategyPositionStore().put(TMON, new Position("BUY", 50.0, null, null, 1000, 0));
            strategy.getStrategyPositionStore().put(NLMK, new Position("BUY", 100.0, 98.0, 105.0, 50, 0));

            // When
            strategy.closeAllPositionsEod();

            // Then: Only NLMK closed, TMON@ skipped
            then(tradingService.closedPositions).containsKey(NLMK);
            then(tradingService.closedPositions).doesNotContainKey(TMON);
        }

        @Test
        @DisplayName("Should close all non-parking positions")
        void shouldCloseAllNonParking() {
            // Given: Multiple positions
            strategy.getStrategyPositionStore().put(NLMK, new Position("BUY", 100.0, 98.0, 105.0, 50, 0));
            strategy.getStrategyPositionStore().put(VTBR, new Position("BUY", 50.0, 48.0, 55.0, 100, 0));

            // When
            strategy.closeAllPositionsEod();

            // Then: Both closed
            then(tradingService.closedPositions).containsKey(NLMK);
            then(tradingService.closedPositions).containsKey(VTBR);
        }
    }

    @Nested
    @DisplayName("processTicker EOD race condition prevention")
    class ProcessTickerEod {

        @Test
        @DisplayName("Should exit early if isWorkingHours() returns false")
        void shouldExitIfNotWorkingHours() {
            // Given: Outside working hours
            strategy.setWorkingHours(false);
            strategy.getStrategyPositionStore().put(NLMK, new Position("BUY", 100.0, 98.0, 105.0, 50, 0));

            // When
            strategy.processTickerEod(NLMK);

            // Then: No decision made, no position opened
            then(strategy.decideCalls).isZero();
            then(tradingService.lastBuyTicker).isNull();
        }

        @Test
        @DisplayName("Should proceed if isWorkingHours() returns true")
        void shouldProceedIfWorkingHours() {
            // Given: Within working hours + candles in repository
            strategy.setWorkingHours(true);
            strategy.setNextDecision(new com.github.shk0da.goldendragon.model.TradingDecision("OPEN", "TEST", 0.0, 5, null, null, ASK_PRICE, new Position("BUY", ASK_PRICE, null, null, 5, 0)));
            // Add candles to CandleRepository
            com.github.shk0da.goldendragon.repository.CandleRepository.getInstance().putCandles(NLMK, "HOUR", candles());

            // When
            strategy.processTickerEod(NLMK);

            // Then: Decision called, position opened
            then(strategy.decideCalls).isOne();
            then(tradingService.lastBuyTicker).isEqualTo(NLMK);
        }
    }

    @Nested
    @DisplayName("Integration: Full EOD flow (regression test for VTBR duplicate close)")
    class FullEodFlow {

        @Test
        @DisplayName("Should sync positions BEFORE closing to prevent duplicate closes")
        void shouldSyncBeforeClosing() {
            // Given: VTBR closed by stop-loss on broker, but still in positionStore
            strategy.getStrategyPositionStore().put(VTBR, new Position("BUY", 50.0, 48.0, 55.0, 100, 0));
            strategy.getStrategyPositionStore().put(NLMK, new Position("BUY", 100.0, 98.0, 105.0, 50, 0));
            // Broker: VTBR already closed (null), NLMK still open
            tradingService.brokerPositions.remove(VTBR);
            tradingService.brokerPositions.put(NLMK, new PositionInfo("FIGI", NLMK, "ISIN", "STOCK", 50, 0.0, 1, 100.0, NLMK));

            // When: Full EOD flow (sync -> close -> buy TMON)
            strategy.syncPositionStoreWithBroker();
            strategy.closeAllPositionsEod();

            // Then: VTBR NOT closed twice (already removed by sync), NLMK closed once
            then(tradingService.closedPositions).containsKey(NLMK);
            then(tradingService.closedPositions).doesNotContainKey(VTBR);
            then(strategy.getStrategyPositionStore()).doesNotContainKey(VTBR);
        }

        @Test
        @DisplayName("Should NOT attempt to close positions already closed by broker")
        void shouldNotCloseAlreadyClosedPositions() {
            // Given: All positions closed by broker (stop-losses hit)
            strategy.getStrategyPositionStore().put(VTBR, new Position("BUY", 50.0, 48.0, 55.0, 100, 0));
            strategy.getStrategyPositionStore().put(NLMK, new Position("BUY", 100.0, 98.0, 105.0, 50, 0));
            tradingService.brokerPositions.remove(VTBR);
            tradingService.brokerPositions.remove(NLMK);

            // When
            strategy.syncPositionStoreWithBroker();
            strategy.closeAllPositionsEod();

            // Then: No close attempts (all positions already closed)
            then(tradingService.closedPositions).isEmpty();
            then(strategy.getStrategyPositionStore()).isEmpty();
        }

        @Test
        @DisplayName("Should buy TMON@ AFTER closing positions (cash settlement)")
        void shouldBuyTmonAfterClosing() {
            // Given: Positions to close + cash to park
            strategy.getStrategyPositionStore().put(NLMK, new Position("BUY", 100.0, 98.0, 105.0, 50, 0));
            tradingService.brokerPositions.put(NLMK, new PositionInfo("FIGI", NLMK, "ISIN", "STOCK", 50, 0.0, 1, 100.0, NLMK));
            tradingService.cash = 50_000.0;

            // When: Full EOD flow
            strategy.syncPositionStoreWithBroker();
            strategy.closeAllPositionsEod();
            strategy.buyTmonWithAllCash();

            // Then: TMON@ bought with cash
            then(tradingService.lastBuyTicker).isEqualTo(TMON);
            then(tradingService.lastBuyValue).isGreaterThan(0);
        }

        @Test
        @DisplayName("Should preserve TMON@ position during EOD (not sell it)")
        void shouldPreserveTmonPosition() {
            // Given: TMON@ parking position + other positions
            strategy.getStrategyPositionStore().put(TMON, new Position("BUY", 50.0, null, null, 1000, 0));
            strategy.getStrategyPositionStore().put(NLMK, new Position("BUY", 100.0, 98.0, 105.0, 50, 0));
            tradingService.brokerPositions.put(TMON, new PositionInfo("FIGI", TMON, "ISIN", "ETF", 1000, 0.0, 1, 50.0, TMON));
            tradingService.brokerPositions.put(NLMK, new PositionInfo("FIGI", NLMK, "ISIN", "STOCK", 50, 0.0, 1, 100.0, NLMK));

            // When
            strategy.syncPositionStoreWithBroker();
            strategy.closeAllPositionsEod();

            // Then: TMON@ NOT closed, NLMK closed
            then(tradingService.closedPositions).containsKey(NLMK);
            then(tradingService.closedPositions).doesNotContainKey(TMON);
            then(strategy.getStrategyPositionStore()).containsKey(TMON);
        }
    }

    @Nested
    @DisplayName("Regression: VTBR duplicate close scenario")
    class VtbrDuplicateCloseRegression {

        @Test
        @DisplayName("Should handle position closed by stop-loss before EOD")
        void shouldHandleStopLossCloseBeforeEod() {
            // Given: VTBR closed by stop-loss at 18:45, EOD starts at 18:50
            strategy.getStrategyPositionStore().put(VTBR, new Position("BUY", 50.0, 48.0, 55.0, 87, 0));
            // Broker: VTBR already closed (stop-loss executed)
            tradingService.brokerPositions.remove(VTBR);

            // When: EOD sync happens FIRST (new behavior)
            strategy.syncPositionStoreWithBroker();

            // Then: VTBR removed from positionStore BEFORE close attempt
            then(strategy.getStrategyPositionStore()).doesNotContainKey(VTBR);

            // When: EOD close happens (VTBR no longer in positionStore)
            strategy.closeAllPositionsEod();

            // Then: No attempt to close VTBR (no error)
            then(tradingService.closedPositions).doesNotContainKey(VTBR);
        }

        @Test
        @DisplayName("Should handle partial close (quantity mismatch)")
        void shouldHandlePartialClose() {
            // Given: Local qty=87, broker qty=37 (50 closed by stop-loss)
            strategy.getStrategyPositionStore().put(VTBR, new Position("BUY", 50.0, 48.0, 55.0, 87, 0));
            tradingService.brokerPositions.put(VTBR, new PositionInfo("FIGI", VTBR, "ISIN", "STOCK", 37, 0.0, 1, 50.0, VTBR));

            // When
            strategy.syncPositionStoreWithBroker();

            // Then: Quantity updated to match broker
            then(strategy.getStrategyPositionStore().get(VTBR).quantity).isEqualTo(37);
        }

        @Test
        @DisplayName("Should prevent race condition: processTicker exits when EOD starts")
        void shouldPreventRaceCondition() {
            // Given: EOD starts (isWorkingHours = false), ticker thread still running
            strategy.setWorkingHours(false);
            strategy.getStrategyPositionStore().put(VTBR, new Position("BUY", 50.0, 48.0, 55.0, 87, 0));

            // When: Ticker thread tries to process
            strategy.processTickerEod(VTBR);

            // Then: Ticker thread exits immediately (no position opened)
            then(strategy.decideCalls).isZero();
            then(tradingService.lastBuyTicker).isNull();
        }
    }

    /** Test strategy exposing EOD methods. */
    private static class TestEodStrategy extends BaseStrategy {

        boolean workingHours = true;
        com.github.shk0da.goldendragon.model.TradingDecision nextDecision;
        int decideCalls = 0;

        TestEodStrategy(
                UnifiedTraderConfig unifiedTraderConfig,
                TradingService tradingService,
                Config config,
                CashParkingManager cashParkingManager) {
            super(unifiedTraderConfig, tradingService, config);
            // Inject cashParkingManager into parent class field
            try {
                java.lang.reflect.Field field = BaseStrategy.class.getDeclaredField("cashParkingManager");
                field.setAccessible(true);
                field.set(this, cashParkingManager);
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        }

        @Override
        protected String getStrategyName() {
            return "TestEodStrategy";
        }

        @Override
        public com.github.shk0da.goldendragon.model.TradingDecision decide(
                String ticker,
                List<Candle> hourCandles,
                List<Candle> minuteCandles,
                Position position,
                double balance,
                boolean incrementCandlesHeld) {
            decideCalls++;
            return nextDecision != null ? nextDecision : new com.github.shk0da.goldendragon.model.TradingDecision("HOLD", "test");
        }

        @Override
        protected boolean isWorkingHours() {
            return workingHours;
        }

        @Override
        protected boolean isTradingDay() {
            return true;
        }

        void setCashParkingManager(CashParkingManager cashParkingManager) {
            try {
                java.lang.reflect.Field field = BaseStrategy.class.getDeclaredField("cashParkingManager");
                field.setAccessible(true);
                field.set(this, cashParkingManager);
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        }

        @Override
        protected void syncPositionStoreWithBroker() {
            super.syncPositionStoreWithBroker();
        }

        @Override
        protected void buyTmonWithAllCash() {
            super.buyTmonWithAllCash();
        }

        protected void closeAllPositionsEod() {
            super.closeAllPositions(tradingService, unifiedTraderConfig);
        }

        protected void processTickerEod(String ticker) {
            super.processTicker(ticker, tradingService, unifiedTraderConfig, 0.0);
        }

        protected void setWorkingHours(boolean workingHours) {
            this.workingHours = workingHours;
        }

        protected void setNextDecision(com.github.shk0da.goldendragon.model.TradingDecision nextDecision) {
            this.nextDecision = nextDecision;
        }

        protected Map<String, Position> getStrategyPositionStore() {
            try {
                java.lang.reflect.Field field = BaseStrategy.class.getDeclaredField("positionStore");
                field.setAccessible(true);
                return (Map<String, Position>) field.get(this);
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        }
    }

    /** Fake TradingService for EOD tests. */
    private static class FakeTradingService implements TradingService {

        double cash;
        double askPrice;
        int executedCount = 1;
        double lastBuyValue;
        String lastBuyTicker;
        boolean throwExceptionOnGetPosition = false;
        Map<String, PositionInfo> brokerPositions = new ConcurrentHashMap<>();
        Map<String, Boolean> closedPositions = new ConcurrentHashMap<>();

        FakeTradingService() {
            brokerPositions = new ConcurrentHashMap<>();
            closedPositions = new ConcurrentHashMap<>();
        }

        @Override
        public Double getAvailableCash() {
            return cash;
        }

        @Override
        public double getLiveAskPrice(TickerInfo.Key key) {
            return askPrice;
        }

        @Override
        public PositionInfo getCurrentPositions(TickerType tickerType, String tickerName) {
            if (throwExceptionOnGetPosition) {
                throw new RuntimeException("Test exception");
            }
            return brokerPositions.get(tickerName);
        }

        @Override
        public TickerInfo searchTicker(TickerInfo.Key key) {
            if (TMON.equals(key.getTicker())) {
                return new TickerInfo("FIGI_TMON", TMON, "ISIN_TMON", 0.01, 1, "RUB", TMON, "ETF");
            }
            return new TickerInfo("FIGI", key.getTicker(), "ISIN", 0.01, 1, "RUB", key.getTicker(), key.getType().name());
        }

        @Override
        public com.github.shk0da.goldendragon.model.OrderExecutionResult buyByMarketWithDetails(
                String name, TickerType type, double cashToBuy, double takeProfit, double stopLose) {
            lastBuyValue = cashToBuy;
            lastBuyTicker = name;
            return com.github.shk0da.goldendragon.model.OrderExecutionResult.testSuccess(askPrice, 1);
        }

        @Override
        public boolean closeLongByMarket(String ticker, TickerType type) {
            closedPositions.put(ticker, true);
            return true;
        }

        @Override
        public boolean closeShortByMarket(String ticker, TickerType type) {
            closedPositions.put(ticker, true);
            return true;
        }

        // Stub other required methods
        @Override public double getLiveBidPrice(TickerInfo.Key key) { return askPrice * 0.99; }
        @Override public void closeAllByMarket(TickerType type) {}
        @Override public com.github.shk0da.goldendragon.model.OrderExecutionResult sellByMarketWithDetails(String name, TickerType type, double cashToSell, double takeProfit, double stopLose) { return null; }
        @Override public double getAvailablePrice(TickerInfo.Key key) { return askPrice; }
        @Override public double getAvailablePrice(TickerInfo.Key key, int count, String type, boolean isPrintGlass) { return askPrice; }
    }
}
