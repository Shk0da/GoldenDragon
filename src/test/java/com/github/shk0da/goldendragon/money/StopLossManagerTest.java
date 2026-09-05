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

    private static final double INITIAL_RISK = 10.0;
    private static final double ATR = 5.0;

    // Helper to build a long position
    private static Position longPosition(double entry, Double stopLoss) {
        return new Position("BUY", entry, stopLoss, null, 100, 0, 0);
    }

    // Helper to build a short position
    private static Position shortPosition(double entry, Double stopLoss) {
        return new Position("SELL", entry, stopLoss, null, 100, 0, 0);
    }

    // Helper to build a candle with given close
    private static Candle candle(double close) {
        return new Candle("2024-01-01 10:00:00", close, close, close, close, 1000L);
    }

    @Nested
    @DisplayName("When position is invalid")
    class InvalidPosition {

        @Test
        @DisplayName("Should return null for null position")
        void shouldReturnNull_WhenPositionIsNull() {
            // Given
            StopLossManager manager = new StopLossManager(
                    TRAILING_ACTIVATION_R, TRAILING_MULTIPLIER, BREAKEVEN_ACTIVATION_R, BREAKEVEN_BUFFER);

            // When
            Double newStop = manager.updateStopLoss(null, candle(110.0), ATR, INITIAL_RISK);

            // Then
            then(newStop).isNull();
        }

        @Test
        @DisplayName("Should return null when quantity is zero")
        void shouldReturnNull_WhenQuantityIsZero() {
            // Given
            StopLossManager manager = new StopLossManager(
                    TRAILING_ACTIVATION_R, TRAILING_MULTIPLIER, BREAKEVEN_ACTIVATION_R, BREAKEVEN_BUFFER);
            Position position = new Position("BUY", 100.0, 90.0, null, 0, 0, 0);

            // When
            Double newStop = manager.updateStopLoss(position, candle(110.0), ATR, INITIAL_RISK);

            // Then
            then(newStop).isNull();
        }

        @Test
        @DisplayName("Should return null when entry price is null")
        void shouldReturnNull_WhenEntryIsNull() {
            // Given
            StopLossManager manager = new StopLossManager(
                    TRAILING_ACTIVATION_R, TRAILING_MULTIPLIER, BREAKEVEN_ACTIVATION_R, BREAKEVEN_BUFFER);
            Position position = new Position("BUY", null, 90.0, null, 100, 0, 0);

            // When
            Double newStop = manager.updateStopLoss(position, candle(110.0), ATR, INITIAL_RISK);

            // Then
            then(newStop).isNull();
        }
    }

    @Nested
    @DisplayName("When breakeven activation")
    class BreakevenActivation {

        @Test
        @DisplayName("Should move to breakeven when PnL >= breakeven trigger")
        void shouldMoveToBreakeven_WhenPnLReached() {
            // Given
            StopLossManager manager = new StopLossManager(
                    TRAILING_ACTIVATION_R, TRAILING_MULTIPLIER, BREAKEVEN_ACTIVATION_R, BREAKEVEN_BUFFER);
            // entry = 100, initial stop = 90 => risk = 10
            // close = 105 => PnL in R = 5/10 = 0.5 (meets breakevenActivationR=0.5)
            Position position = longPosition(100.0, 90.0);

            // When
            Double newStop = manager.updateStopLoss(position, candle(105.0), ATR, INITIAL_RISK);

            // Then
            then(newStop).isNotNull();
            then(newStop).isEqualTo(100.001); // entry + buffer
        }

        @Test
        @DisplayName("Should not move to breakeven below activation threshold")
        void shouldNotMoveToBreakeven_BelowThreshold() {
            // Given
            StopLossManager manager = new StopLossManager(
                    TRAILING_ACTIVATION_R, TRAILING_MULTIPLIER, BREAKEVEN_ACTIVATION_R, BREAKEVEN_BUFFER);
            // close = 102 => PnL in R = 2/10 = 0.2 < 0.5
            Position position = longPosition(100.0, 90.0);

            // When
            Double newStop = manager.updateStopLoss(position, candle(102.0), ATR, INITIAL_RISK);

            // Then
            then(newStop).isNull();
        }

        @Test
        @DisplayName("Should move down to breakeven for short position")
        void shouldMoveDownToBreakeven_ForShort() {
            // Given
            StopLossManager manager = new StopLossManager(
                    TRAILING_ACTIVATION_R, TRAILING_MULTIPLIER, BREAKEVEN_ACTIVATION_R, BREAKEVEN_BUFFER);
            // short entry = 100, initial stop = 110 => risk = 10
            // close = 95 => PnL in R = (100-95)/10 = 0.5 (meets threshold)
            Position position = shortPosition(100.0, 110.0);

            // When
            Double newStop = manager.updateStopLoss(position, candle(95.0), ATR, INITIAL_RISK);

            // Then
            then(newStop).isNotNull();
            then(newStop).isEqualTo(99.999); // entry - buffer
        }
    }

    @Nested
    @DisplayName("When trailing stop activation")
    class TrailingActivation {

        @Test
        @DisplayName("Should set breakeven when PnL reaches breakeven threshold first")
        void shouldSetBreakeven_WhenPnLReached() {
            // Given
            StopLossManager manager = new StopLossManager(
                    TRAILING_ACTIVATION_R, TRAILING_MULTIPLIER, BREAKEVEN_ACTIVATION_R, BREAKEVEN_BUFFER);
            // entry = 100, initial stop = 90 (risk = 10)
            // close = 105 => PnL in R = 5/10 = 0.5 (meets breakevenActivationR=0.5)
            // breakeven returns BEFORE trailing is checked
            Position position = longPosition(100.0, 90.0);

            // When
            Double newStop = manager.updateStopLoss(position, candle(105.0), ATR, INITIAL_RISK);

            // Then
            // breakeven activated first (pnlR=0.5 >= 0.5), returns 100.001
            // trailing is never reached
            then(newStop).isNotNull();
            then(newStop).isEqualTo(100.001);
        }

        @Test
        @DisplayName("Should set trailing stop when trailing threshold is lower than breakeven")
        void shouldSetTrailingStop_WhenTrailingBelowBreakeven() {
            // Given — trailing activation lower than breakeven
            StopLossManager manager = new StopLossManager(
                    0.3,  // trailing activates at 0.3R
                    1.0,  // trailing multiplier
                    0.5,  // breakeven activates at 0.5R
                    BREAKEVEN_BUFFER);
            // entry = 100, initial stop = 90 (risk = 10)
            // close = 103.5 => PnL in R = 3.5/10 = 0.35
            // trailing: 0.35 >= 0.3 => trailingStop = 103.5 - 5 = 98.5 > currentStop 90
            // breakeven: 0.35 < 0.5 => no breakeven
            Position position = longPosition(100.0, 90.0);

            // When
            Double newStop = manager.updateStopLoss(position, candle(103.5), ATR, INITIAL_RISK);

            // Then
            then(newStop).isEqualTo(98.5);
        }

        @Test
        @DisplayName("Should not move trailing stop for short when worse")
        void shouldNotMoveTrailingStop_ForShort_WhenWorse() {
            // Given
            StopLossManager manager = new StopLossManager(
                    0.3, TRAILING_MULTIPLIER, 0.5, BREAKEVEN_BUFFER);
            // short entry = 100, current stop = 90 (already trailed)
            Position position = shortPosition(100.0, 90.0);
            // close = 88 => pnlR = (100-88)/10 = 1.2 >= 0.3 (trailing threshold)
            // trailingStop = 88 + 5 = 93, but for short: isBetterStop(93, 90, "SELL") = 93 < 90 = FALSE
            // breakeven: pnlR (1.2) >= 0.5 => breakevenStop = 99.999, 99.999 < 90 = FALSE
            
            // When
            Double newStop = manager.updateStopLoss(position, candle(88.0), ATR, INITIAL_RISK);

            // Then — trailing is worse (93 > 90), breakeven is also worse (99.999 > 90)
            then(newStop).isNull();
        }
    }

    @Nested
    @DisplayName("When stop should not move backward")
    class NoBackwardMove {

        @Test
        @DisplayName("Should not lower trailing stop for long position")
        void shouldNotLowerStop_ForLong() {
            // Given
            StopLossManager manager = new StopLossManager(
                    TRAILING_ACTIVATION_R, TRAILING_MULTIPLIER, BREAKEVEN_ACTIVATION_R, BREAKEVEN_BUFFER);
            // Already have trailing stop at 110
            Position position = longPosition(100.0, 110.0);
            // close = 112 => PnL in R = 1.2, trailing = 107 < current 110
            // breakeven = 100.001 < current 110

            // When
            Double newStop = manager.updateStopLoss(position, candle(112.0), ATR, INITIAL_RISK);

            // Then
            then(newStop).isNull();
        }

        @Test
        @DisplayName("Should not raise trailing stop for short position")
        void shouldNotRaiseStop_ForShort() {
            // Given
            StopLossManager manager = new StopLossManager(
                    TRAILING_ACTIVATION_R, TRAILING_MULTIPLIER, BREAKEVEN_ACTIVATION_R, BREAKEVEN_BUFFER);
            // Short with trailing stop at 95
            Position position = shortPosition(100.0, 95.0);
            // close = 88 => pnlR = 1.2 >= 0.5 (breakeven), but currentStop=95 > breakevenStop=99.999 is FALSE
            // Wait: for short, isBetterStop(newStop, currentStop, "SELL") = newStop < currentStop
            // breakevenStop = 99.999, currentStop = 95 => 99.999 < 95 = FALSE => no breakeven
            // trailingStop = 88 + 5 = 93, currentStop = 95 => 93 < 95 = TRUE => trailing activates!
            // Actually trailing should work here since 93 < 95
            
            // When
            Double newStop = manager.updateStopLoss(position, candle(88.0), ATR, INITIAL_RISK);

            // Then — trailing IS better (93 < 95), so it should activate
            then(newStop).isEqualTo(93.0);
        }
    }

    @Nested
    @DisplayName("When no stop loss set")
    class NoCurrentStop {

        @Test
        @DisplayName("Should use entry price as current stop fallback")
        void shouldUseEntryAsCurrentStop() {
            // Given
            StopLossManager manager = new StopLossManager(
                    TRAILING_ACTIVATION_R, TRAILING_MULTIPLIER, BREAKEVEN_ACTIVATION_R, BREAKEVEN_BUFFER);
            // No stop loss, entry = 100
            Position position = longPosition(100.0, null);

            // When
            Double newStop = manager.updateStopLoss(position, candle(112.0), ATR, INITIAL_RISK);

            // Then
            // breakeven = 100.001 > currentStop (defaults to entry 100) => move to breakeven
            then(newStop).isEqualTo(100.001);
        }
    }
}