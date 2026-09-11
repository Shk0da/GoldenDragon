package com.github.shk0da.goldendragon.strategy;

import com.github.shk0da.goldendragon.config.UnifiedTraderConfig;
import com.github.shk0da.goldendragon.model.Candle;
import com.github.shk0da.goldendragon.model.Config;
import com.github.shk0da.goldendragon.model.Position;
import com.github.shk0da.goldendragon.model.PositionInfo;
import com.github.shk0da.goldendragon.model.TickerInfo;
import com.github.shk0da.goldendragon.model.TickerType;
import com.github.shk0da.goldendragon.model.TradingDecision;
import com.github.shk0da.goldendragon.repository.CandleRepository;
import com.github.shk0da.goldendragon.repository.TickerRepository;
import com.github.shk0da.goldendragon.service.TradingService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;

import static org.assertj.core.api.BDDAssertions.then;
import static org.assertj.core.api.Assertions.within;

@DisplayName("BaseStrategy.openPosition with lot size, SL and TP")
class BaseStrategyOpenPositionTest {

    private static final String NLMK = "NLMK";
    private static final int NLMK_LOT = 10;
    private static final String TMON = "TMON@";
    private static final int TMON_LOT = 1;
    private static final double ASK_PRICE = 100.0;
    private static final double BID_PRICE = 99.9;

    private FakeTradingService tradingService;
    private TestStrategy strategy;

    @BeforeEach
    void setUp() throws Exception {
        TickerRepository.INSTANCE.putAll(
                Map.of(
                        new TickerInfo.Key(NLMK, TickerType.STOCK),
                        new TickerInfo("FIGI_NLMK", NLMK, "ISIN_NLMK", 0.01, NLMK_LOT, "RUB", NLMK, "STOCK"),
                        new TickerInfo.Key(TMON, TickerType.ETF),
                        new TickerInfo("FIGI_TMON", TMON, "ISIN_TMON", 0.01, TMON_LOT, "RUB", TMON, "ETF")));
        tradingService = new FakeTradingService();
        tradingService.cash = 100_000.0;
        tradingService.askPrice = ASK_PRICE;
        tradingService.bidPrice = BID_PRICE;
        strategy = new TestStrategy(new UnifiedTraderConfig(), tradingService, new Config());
    }

    private static List<Candle> candles() {
        return List.of(new Candle("01.01.2026 10:00:00", 100.0, 101.0, 99.0, 100.0, 1000));
    }

    private static Position position(String direction) {
        return new Position(direction, ASK_PRICE, null, null, 5, 0);
    }

    @Nested
    @DisplayName("When quantity is provided by the decision")
    class ProvidedQuantity {

        @Test
        @DisplayName("Should pass quantity in lots to the order executor")
        void shouldPassLotsToOrderExecutor() {
            // Given: 5 lots * 100 * 10 lot size * 1.01 = 5050
            tradingService.executedCount = 5;
            TradingDecision decision =
                    new TradingDecision("OPEN", "TEST", 0.0, 5, null, null, ASK_PRICE, position("BUY"));

            // When
            strategy.open(NLMK, TickerRepository.INSTANCE.getByName(NLMK), candles(), decision);

            // Then
            then(tradingService.lastBuyValue).isEqualTo(5 * ASK_PRICE * NLMK_LOT * 1.01);
            then(strategy.getPositionStore().get(NLMK).quantity).isEqualTo(5);
        }

        @Test
        @DisplayName("Should compute default SL 2% and TP 4% for BUY when none provided")
        void shouldComputeDefaultSlTp_ForBuy() {
            // Given: entry 100 -> default SL 98 (2%), TP 104 (4%)
            tradingService.executedCount = 5;
            TradingDecision decision =
                    new TradingDecision("OPEN", "TEST", 0.0, 5, null, null, ASK_PRICE, position("BUY"));

            // When
            strategy.open(NLMK, TickerRepository.INSTANCE.getByName(NLMK), candles(), decision);

            // Then
            then(tradingService.lastBuySl).isEqualTo(2.0);
            then(tradingService.lastBuyTp).isEqualTo(4.0);
        }

        @Test
        @DisplayName("Should compute default SL 2% and TP 4% for SELL when none provided")
        void shouldComputeDefaultSlTp_ForSell() {
            // Given: entry 100 -> default SL 102 (2%), TP 96 (4%)
            tradingService.executedCount = 5;
            TradingDecision decision =
                    new TradingDecision("OPEN", "TEST", 0.0, 5, null, null, ASK_PRICE, position("SELL"));

            // When
            strategy.open(NLMK, TickerRepository.INSTANCE.getByName(NLMK), candles(), decision);

            // Then
            then(tradingService.lastSellSl).isEqualTo(2.0);
            then(tradingService.lastSellTp).isEqualTo(4.0);
        }

        @Test
        @DisplayName("Should compute SL/TP percents from provided stop and take profit prices")
        void shouldComputeSlTp_FromProvidedPrices() {
            // Given: entry 100, SL 95 (5%), TP 110 (10%)
            tradingService.executedCount = 5;
            TradingDecision decision =
                    new TradingDecision("OPEN", "TEST", 0.0, 5, 95.0, 110.0, ASK_PRICE, position("BUY"));

            // When
            strategy.open(NLMK, TickerRepository.INSTANCE.getByName(NLMK), candles(), decision);

            // Then
            then(tradingService.lastBuySl).isEqualTo(5.0);
            then(tradingService.lastBuyTp).isEqualTo(10.0);
        }
    }

    @Nested
    @DisplayName("When opening TMON@ cash parking")
    class TmonCashParking {

        @Test
        @DisplayName("Should compute qty = floor(cash / (price * lot)) and buy with full cash, zero SL/TP")
        void shouldComputeParkingQuantityWithLotSize() {
            // Given: cash 100_000, ask 100, lot 1 -> qty = floor(100000 / 100) = 1000
            tradingService.executedCount = 1000;
            TradingDecision decision =
                    new TradingDecision("OPEN", "TEST", 0.0, 1000, null, null, ASK_PRICE, position("BUY"));

            // When
            strategy.open(TMON, TickerRepository.INSTANCE.getByName(TMON), candles(), decision);

            // Then
            then(tradingService.lastBuyValue).isEqualTo(100_000.0);
            then(tradingService.lastBuySl).isEqualTo(0.0);
            then(tradingService.lastBuyTp).isEqualTo(0.0);
            then(strategy.getPositionStore().get(TMON).quantity).isEqualTo(1000);
        }
    }

    @Nested
    @DisplayName("When freeing cash from parking for an OPEN (PARTIALFREE)")
    class PartialFree {

        @Test
        @DisplayName("Should sell parking lots to cover the missing amount using lot size")
        void shouldFreeCashFromParkingUsingLotSize() {
            // Given: NLMK lot=10, qty=5, entry=100 -> positionValue=5000, cash=1000, missing=4000
            // Parking: TMON@ lot=1, price=100 -> parkingLotCost=100, neededLots=40, cashToFree=4000
            tradingService.cash = 1000.0;
            tradingService.parkingInfo = new PositionInfo(
                    "FIGI", TMON, "ISIN", "ETF", 100, 0.0, 100, 100.0, TMON);
            tradingService.tickerInfo = new TickerInfo(
                    "FIGI_TMON", TMON, "ISIN_TMON", 0.01, TMON_LOT, "RUB", TMON, "ETF");
            strategy.nextDecision =
                    new TradingDecision("OPEN", "TEST", 0.0, 5, null, null, ASK_PRICE, position("BUY"));
            CandleRepository.getInstance().putCandles(NLMK, "HOUR", candles());

            // When
            strategy.processTick(NLMK);

            // Then
            then(tradingService.lastSellValue).isCloseTo(4000.0, within(0.001));
            then(tradingService.lastSellTicker).isEqualTo(TMON);
        }
    }

    @Nested
    @DisplayName("When decision is invalid")
    class InvalidDecision {

        @Test
        @DisplayName("Should skip when updated position is missing")
        void shouldSkip_WhenNoUpdatedPosition() {
            // When
            strategy.open(
                    NLMK,
                    TickerRepository.INSTANCE.getByName(NLMK),
                    candles(),
                    new TradingDecision("OPEN", "TEST", 0.0, 5, null, null, ASK_PRICE, null));

            // Then
            then(tradingService.lastBuyValue).isZero();
            then(strategy.getPositionStore().get(NLMK)).isNull();
        }

        @Test
        @DisplayName("Should skip when quantity is negative")
        void shouldSkip_WhenNegativeQuantity() {
            // When
            strategy.open(
                    NLMK,
                    TickerRepository.INSTANCE.getByName(NLMK),
                    candles(),
                    new TradingDecision("OPEN", "TEST", 0.0, -1, null, null, ASK_PRICE, position("BUY")));

            // Then
            then(tradingService.lastBuyValue).isZero();
            then(strategy.getPositionStore().get(NLMK)).isNull();
        }
    }

    @Nested
    @DisplayName("When trading is halted by loss streak")
    class TradingHalted {

        @Test
        @DisplayName("Should not call decide() in processTicker")
        void shouldNotCallDecide_WhenTradingHalted() {
            strategy.tradingHalted = true;

            strategy.processTick(NLMK);

            then(strategy.decideCalls).isZero();
        }
    }

    /** Minimal strategy exposing openPosition and processTicker for testing. */
    private static class TestStrategy extends BaseStrategy {

        TradingDecision nextDecision;
        int decideCalls = 0;

        TestStrategy(UnifiedTraderConfig unifiedTraderConfig, TradingService tradingService, Config config) {
            super(unifiedTraderConfig, tradingService, config);
        }

        @Override
        protected String getStrategyName() {
            return "TestStrategy";
        }

        @Override
        public TradingDecision decide(
                String ticker,
                List<Candle> hourCandles,
                List<Candle> minuteCandles,
                Position position,
                double balance,
                boolean incrementCandlesHeld) {
            decideCalls++;
            return nextDecision != null ? nextDecision : new TradingDecision("HOLD", "test");
        }

        @Override
        protected boolean isWorkingHours() {
            return true;
        }

        @Override
        protected boolean isTradingDay() {
            return true;
        }

        void open(String name, TickerInfo ticker, List<Candle> candles, TradingDecision decision) {
            openPosition(name, ticker, candles, decision);
        }

        void processTick(String name) {
            processTicker(name, tradingService, unifiedTraderConfig, 0.0);
        }
    }

    /** Minimal TradingService fake recording order values and SL/TP percents. */
    private static class FakeTradingService implements TradingService {

        double cash;
        double askPrice;
        double bidPrice;
        int executedCount;
        double lastBuyValue;
        Double lastBuySl;
        Double lastBuyTp;
        double lastSellValue;
        Double lastSellSl;
        Double lastSellTp;
        String lastSellTicker;
        PositionInfo parkingInfo;
        TickerInfo tickerInfo;

        @Override
        public Double getAvailableCash() {
            return cash;
        }

        @Override
        public double getLiveAskPrice(TickerInfo.Key key) {
            return askPrice;
        }

        @Override
        public double getLiveBidPrice(TickerInfo.Key key) {
            return bidPrice;
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
        public OrderExecutionResult buyByMarketWithDetails(
                String name, TickerType type, double cashToBuy, double takeProfit, double stopLose) {
            lastBuyValue = cashToBuy;
            lastBuyTp = takeProfit;
            lastBuySl = stopLose;
            return OrderExecutionResult.testSuccess(askPrice, executedCount);
        }

        @Override
        public OrderExecutionResult sellByMarketWithDetails(
                String name, TickerType type, double cashToSell, double takeProfit, double stopLose) {
            lastSellValue = cashToSell;
            lastSellTp = takeProfit;
            lastSellSl = stopLose;
            lastSellTicker = name;
            return OrderExecutionResult.testSuccess(askPrice, executedCount);
        }
    }
}