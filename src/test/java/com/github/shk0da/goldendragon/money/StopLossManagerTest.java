package com.github.shk0da.goldendragon.money;

import com.github.shk0da.goldendragon.model.Candle;
import com.github.shk0da.goldendragon.model.Position;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.BDDAssertions.then;

@DisplayName("Stop Loss Manager")
class StopLossManagerTest {

    private static final double TRAILING_ACTIVATION_R = 1.0;
    private static final double TRAILING_MULTIPLIER = 1.0;
    private static final double BREAKEVEN_ACTIVATION_R = 0.5;
    private static final double BREAKEVEN_BUFFER = 0.001;
    private static final boolean TRAILING_ENABLED = true;
    private static final double TRAILING_STEP_PERCENT = 0.005;
    private static final double TRAILING_DELTA_PERCENT = 0.003;
    private static final int TRAILING_CHECK_INTERVAL = 1;
    private static final double TRAILING_VOLUME_PERCENT = 0.5;
    private static final double COMMISSION = 0.0005;

    private static final double INITIAL_RISK = 10.0;
    private static final double ATR = 5.0;

    private static StopLossManager createManager() {
        return new StopLossManager(
                TRAILING_ACTIVATION_R, TRAILING_MULTIPLIER, BREAKEVEN_ACTIVATION_R, BREAKEVEN_BUFFER,
                TRAILING_ENABLED, TRAILING_STEP_PERCENT, TRAILING_DELTA_PERCENT,
                TRAILING_CHECK_INTERVAL, COMMISSION);
    }

    private static Position longPosition(double entry, Double stopLoss) {
        return new Position("BUY", entry, stopLoss, null, 100, 0, 0);
    }

    private static Position shortPosition(double entry, Double stopLoss) {
        return new Position("SELL", entry, stopLoss, null, 100, 0, 0);
    }

    private static Candle candle(double close) {
        return new Candle("2024-01-01 10:00:00", close, close, close, close, 1000L);
    }

    @Nested
    @DisplayName("When position is invalid")
    class InvalidPosition {

        @Test
        @DisplayName("Should return null result for null position")
        void shouldReturnNullResult_WhenPositionIsNull() {
            StopLossManager manager = createManager();
            StopLossManager.TrailingResult result = manager.updateStopLoss(
                    null, candle(110.0), ATR, INITIAL_RISK, 0);
            then(result).isNotNull();
            then(result.newStopLoss).isNull();
            then(result.newTakeProfit).isNull();
            then(result.trailingActivated).isFalse();
        }

        @Test
        @DisplayName("Should return null result when quantity is zero")
        void shouldReturnNullResult_WhenQuantityIsZero() {
            StopLossManager manager = createManager();
            Position position = new Position("BUY", 100.0, 90.0, null, 0, 0, 0);
            StopLossManager.TrailingResult result = manager.updateStopLoss(
                    position, candle(110.0), ATR, INITIAL_RISK, 0);
            then(result).isNotNull();
            then(result.newStopLoss).isNull();
        }
    }

    @Nested
    @DisplayName("When breakeven activation")
    class BreakevenActivation {

        @Test
        @DisplayName("Should move to breakeven when PnL >= breakeven trigger")
        void shouldMoveToBreakeven_WhenPnLReached() {
            StopLossManager manager = createManager();
            Position position = longPosition(100.0, 90.0);
            StopLossManager.TrailingResult result = manager.updateStopLoss(
                    position, candle(105.0), ATR, INITIAL_RISK, 0);
            then(result).isNotNull();
            then(result.newStopLoss).isNotNull();
            then(result.newStopLoss).isEqualTo(100.001);
            then(result.trailingActivated).isFalse();
        }
    }

    @Nested
    @DisplayName("When trailing stop activation")
    class TrailingActivation {

        @Test
        @DisplayName("Should set breakeven when PnL reaches breakeven threshold first")
        void shouldSetBreakeven_WhenPnLReached() {
            StopLossManager manager = createManager();
            Position position = longPosition(100.0, 90.0);
            StopLossManager.TrailingResult result = manager.updateStopLoss(
                    position, candle(105.0), ATR, INITIAL_RISK, 0);
            then(result.newStopLoss).isEqualTo(100.001);
        }

        @Test
        @DisplayName("Should set trailing stop when trailing threshold is lower than breakeven")
        void shouldSetTrailingStop_WhenTrailingBelowBreakeven() {
            StopLossManager manager = new StopLossManager(
                    0.3, 1.0, 0.5, BREAKEVEN_BUFFER,
                    true, TRAILING_STEP_PERCENT, TRAILING_DELTA_PERCENT,
                    TRAILING_CHECK_INTERVAL, COMMISSION);
            Position position = longPosition(100.0, 90.0);
            StopLossManager.TrailingResult result = manager.updateStopLoss(
                    position, candle(103.5), ATR, INITIAL_RISK, 0);
            then(result.newStopLoss).isEqualTo(98.5);
        }

        @Test
        @DisplayName("Should move to breakeven first, then trailing on next call")
        void shouldMoveToBreakevenFirst_ThenTrailOnNextCall() {
            StopLossManager manager = createManager();
            Position position = longPosition(100.0, 90.0);
            
            // First call: move to breakeven
            StopLossManager.TrailingResult result = manager.updateStopLoss(
                    position, candle(115.0), ATR, INITIAL_RISK, 0);
            then(result.newStopLoss).isEqualTo(100.001);
            then(result.trailingActivated).isFalse();
        }
    }

    @Nested
    @DisplayName("When trailing after breakeven (multi-step)")
    class TrailingAfterBreakeven {

        @Test
        @DisplayName("Should activate trailing after breakeven on next candle")
        void shouldActivateTrailing_AfterBreakevenOnNextCandle() {
            StopLossManager manager = createManager();
            Position position = longPosition(100.0, 90.0);

            // Step 1: Move to breakeven
            StopLossManager.TrailingResult result1 = manager.updateStopLoss(
                    position, candle(105.0), ATR, INITIAL_RISK, 0);
            then(result1.newStopLoss).isEqualTo(100.001);
            then(result1.trailingActivated).isFalse();

            
            Position positionWithBreakeven = new Position("BUY", 100.0, 100.001, null, 100, 10, 0);
            StopLossManager.TrailingResult result2 = manager.updateStopLoss(
                    positionWithBreakeven, candle(115.0), ATR, INITIAL_RISK, 10);
            then(result2.newStopLoss).isNotNull();
            then(result2.newStopLoss).isGreaterThan(100.0);
            then(result2.trailingActivated).isTrue();
        }
    }

    @Nested
    @DisplayName("When SHORT position")
    class ShortPosition {

        @Test
        @DisplayName("Should move to breakeven for SHORT when PnL >= breakeven trigger")
        void shouldMoveToBreakeven_Short() {
            StopLossManager manager = createManager();
            Position position = shortPosition(100.0, 110.0);
            StopLossManager.TrailingResult result = manager.updateStopLoss(
                    position, candle(95.0), ATR, INITIAL_RISK, 0);
            then(result).isNotNull();
            then(result.newStopLoss).isNotNull();
            then(result.newStopLoss).isEqualTo(99.999);
            then(result.trailingActivated).isFalse();
        }

        @Test
        @DisplayName("Should calculate breakeven price correctly for SHORT")
        void shouldCalculateBreakevenPrice_Short() {
            StopLossManager manager = createManager();
            Position position = shortPosition(100.0, 110.0);
            
            StopLossManager.TrailingResult result = manager.updateStopLoss(
                    position, candle(95.0), ATR, INITIAL_RISK, 0);
            then(result.newStopLoss).isEqualTo(99.999);
        }

        @Test
        @DisplayName("Should set trailing stop for SHORT when trailing threshold is lower")
        void shouldSetTrailingStop_Short() {
            StopLossManager manager = new StopLossManager(
                    0.3, 1.0, 0.5, BREAKEVEN_BUFFER,
                    true, TRAILING_STEP_PERCENT, TRAILING_DELTA_PERCENT,
                    TRAILING_CHECK_INTERVAL, COMMISSION);
            Position position = shortPosition(100.0, 110.0);
            StopLossManager.TrailingResult result = manager.updateStopLoss(
                    position, candle(96.5), ATR, INITIAL_RISK, 0);
            then(result.newStopLoss).isEqualTo(101.5);
        }

        @Test
        @DisplayName("Should activate trailing after breakeven for SHORT")
        void shouldActivateTrailing_Short() {
            StopLossManager manager = createManager();
            Position position = shortPosition(100.0, 110.0);

            // Step 1: Move to breakeven
            StopLossManager.TrailingResult result1 = manager.updateStopLoss(
                    position, candle(95.0), ATR, INITIAL_RISK, 0);
            then(result1.newStopLoss).isEqualTo(99.999);
            then(result1.trailingActivated).isFalse();

            
            Position positionWithBreakeven = new Position("SELL", 100.0, 99.999, null, 100, 10, 0);
            StopLossManager.TrailingResult result2 = manager.updateStopLoss(
                    positionWithBreakeven, candle(85.0), ATR, INITIAL_RISK, 10);
            then(result2.newStopLoss).isEqualTo(90.0);
            then(result2.trailingActivated).isTrue();
        }
    }

    @Nested
    @DisplayName("When trailing TP activation")
    class TrailingTP {

        @Test
        @DisplayName("Should move TP for LONG when price moves favorably")
        void shouldMoveTP_Long() {
            StopLossManager manager = createManager();
            Position position = new Position("BUY", 100.0, 100.001, 120.0, 100, 10, 0);

            
            StopLossManager.TrailingResult result = manager.updateStopLoss(
                    position, candle(115.0), ATR, INITIAL_RISK, 9);
            then(result).isNotNull();
            then(result.trailingActivated).isTrue();
            then(result.newTakeProfit).isNotNull();
            then(result.newTakeProfit).isEqualTo(127.5);
        }

        @Test
        @DisplayName("Should move TP for SHORT when price moves favorably")
        void shouldMoveTP_Short() {
            StopLossManager manager = createManager();
            Position position = new Position("SELL", 100.0, 99.999, 80.0, 100, 10, 0);

            
            StopLossManager.TrailingResult result = manager.updateStopLoss(
                    position, candle(85.0), ATR, INITIAL_RISK, 9);
            then(result).isNotNull();
            then(result.trailingActivated).isTrue();
            then(result.newTakeProfit).isNotNull();
            then(result.newTakeProfit).isEqualTo(72.5);
        }

        @Test
        @DisplayName("Should not move TP when current TP is null")
        void shouldNotMoveTP_WhenNull() {
            StopLossManager manager = createManager();
            Position position = new Position("BUY", 100.0, 90.0, null, 100, 10, 0);

            // When
            StopLossManager.TrailingResult result = manager.updateStopLoss(
                    position, candle(115.0), ATR, INITIAL_RISK, 10);
            then(result).isNotNull();
            then(result.newTakeProfit).isNull();
        }
    }

    @Nested
    @DisplayName("When edge cases")
    class EdgeCases {

        @Test
        @DisplayName("Should handle null entryPrice gracefully")
        void shouldHandleNullEntryPrice() {
            StopLossManager manager = createManager();
            Position position = new Position("BUY", null, 90.0, null, 100, 0, 0);

            // When
            StopLossManager.TrailingResult result = manager.updateStopLoss(
                    position, candle(110.0), ATR, INITIAL_RISK, 0);
            then(result).isNotNull();
            then(result.newStopLoss).isNull();
        }

        @Test
        @DisplayName("Should handle zero initialRisk gracefully")
        void shouldHandleZeroInitialRisk() {
            StopLossManager manager = createManager();
            Position position = longPosition(100.0, 90.0);

            // When
            StopLossManager.TrailingResult result = manager.updateStopLoss(
                    position, candle(110.0), ATR, 0.0, 0);
            then(result).isNotNull();
        }

        @Test
        @DisplayName("Should return true for isBetterStop when currentStop is null")
        void shouldReturnTrue_WhenCurrentStopNull() {
            
            StopLossManager manager = createManager();
            Position position = longPosition(100.0, null);
            StopLossManager.TrailingResult result = manager.updateStopLoss(
                    position, candle(105.0), ATR, INITIAL_RISK, 0);
            then(result.newStopLoss).isNotNull();
        }
    }
}
