package com.github.shk0da.goldendragon.money;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.BDDAssertions.then;
import static org.assertj.core.api.Assertions.within;

@DisplayName("PerformanceTracker")
class PerformanceTrackerTest {

    @Nested
    @DisplayName("registerTrade")
    class RegisterTrade {

        @Test
        @DisplayName("Should record a winning trade")
        void shouldRecordWinningTrade() {
            PerformanceTracker tracker = new PerformanceTracker();
            tracker.registerTrade(100.0);

            PerformanceTracker.SessionStats stats = tracker.getSessionStats();
            then(stats.wins).isEqualTo(1);
            then(stats.losses).isZero();
            then(stats.trades).isEqualTo(1);
            then(stats.totalPnL).isEqualTo(100.0);
            then(stats.largestWin).isEqualTo(100.0);
        }

        @Test
        @DisplayName("Should record a losing trade")
        void shouldRecordLosingTrade() {
            PerformanceTracker tracker = new PerformanceTracker();
            tracker.registerTrade(-50.0);

            PerformanceTracker.SessionStats stats = tracker.getSessionStats();
            then(stats.wins).isZero();
            then(stats.losses).isEqualTo(1);
            then(stats.trades).isEqualTo(1);
            then(stats.totalPnL).isEqualTo(-50.0);
            then(stats.largestLoss).isEqualTo(-50.0);
        }

        @Test
        @DisplayName("Should track largest win across multiple trades")
        void shouldTrackLargestWin() {
            PerformanceTracker tracker = new PerformanceTracker();
            tracker.registerTrade(100.0);
            tracker.registerTrade(150.0);
            tracker.registerTrade(75.0);

            PerformanceTracker.SessionStats stats = tracker.getSessionStats();
            then(stats.largestWin).isEqualTo(150.0);
        }

        @Test
        @DisplayName("Should track largest loss across multiple trades")
        void shouldTrackLargestLoss() {
            PerformanceTracker tracker = new PerformanceTracker();
            tracker.registerTrade(-100.0);
            tracker.registerTrade(-200.0);
            tracker.registerTrade(-50.0);

            PerformanceTracker.SessionStats stats = tracker.getSessionStats();
            then(stats.largestLoss).isEqualTo(-200.0);
        }

        @Test
        @DisplayName("Should count zero-PnL trade as a win (>= 0)")
        void shouldCountZeroPnlAsWin() {
            PerformanceTracker tracker = new PerformanceTracker();
            tracker.registerTrade(100.0);
            tracker.registerTrade(0.0);
            tracker.registerTrade(-50.0);

            PerformanceTracker.SessionStats stats = tracker.getSessionStats();
            then(stats.wins).isEqualTo(2);
            then(stats.losses).isEqualTo(1);
            then(stats.trades).isEqualTo(3);
        }
    }

    @Nested
    @DisplayName("updateEquity")
    class UpdateEquity {

        @Test
        @DisplayName("Should update current equity and track peak")
        void shouldTrackPeakEquity() {
            PerformanceTracker tracker = new PerformanceTracker();
            tracker.updateEquity(100_000.0);
            tracker.updateEquity(150_000.0);
            tracker.updateEquity(130_000.0);

            then(tracker.getPeakEquity()).isEqualTo(150_000);
            then(tracker.getGlobalPeakEquity()).isEqualTo(150_000);
            then(tracker.getCurrentDrawdown()).isCloseTo(0.1333, within(0.001));
        }

        @Test
        @DisplayName("Should track global peak across resets")
        void shouldTrackGlobalPeakAcrossResets() {
            PerformanceTracker tracker = new PerformanceTracker();
            tracker.updateEquity(100_000.0);
            tracker.updateEquity(150_000.0);
            tracker.resetSession();
            tracker.updateEquity(120_000.0);

            then(tracker.getGlobalPeakEquity()).isEqualTo(150_000);
            then(tracker.getPeakEquity()).isEqualTo(120_000);
        }

        @Test
        @DisplayName("Should return zero drawdown when equity equals peak")
        void shouldReturnZeroDrawdown_WhenEquityEqualsPeak() {
            PerformanceTracker tracker = new PerformanceTracker();
            tracker.updateEquity(100_000.0);

            then(tracker.getCurrentDrawdown()).isZero();
        }

        @Test
        @DisplayName("Should return zero drawdown when equity is zero")
        void shouldReturnZeroDrawdown_WhenEquityZero() {
            PerformanceTracker tracker = new PerformanceTracker();

            then(tracker.getCurrentDrawdown()).isZero();
        }
    }

    @Nested
    @DisplayName("getPositionSizeMultiplier")
    class GetPositionSizeMultiplier {

        @Test
        @DisplayName("Should return 1.0 when drawdown is zero")
        void shouldReturnOne_WhenNoDrawdown() {
            PerformanceTracker tracker = new PerformanceTracker();
            double multiplier = tracker.getPositionSizeMultiplier(100_000.0, 100_000.0);
            then(multiplier).isEqualTo(1.0);
        }

        @Test
        @DisplayName("Should return 1.0 when drawdown is within 10%")
        void shouldReturnOne_WhenDrawdownUnder10Percent() {
            PerformanceTracker tracker = new PerformanceTracker();
            double multiplier = tracker.getPositionSizeMultiplier(100_000.0, 91_000.0);
            then(multiplier).isEqualTo(1.0);
        }

        @Test
        @DisplayName("Should return 0.5 when drawdown is between 10% and 20%")
        void shouldReturnHalf_WhenDrawdownBetween10And20Percent() {
            PerformanceTracker tracker = new PerformanceTracker();
            double multiplier = tracker.getPositionSizeMultiplier(100_000.0, 85_000.0);
            then(multiplier).isEqualTo(0.5);
        }

        @Test
        @DisplayName("Should return 0.25 when drawdown is between 20% and 30%")
        void shouldReturnQuarter_WhenDrawdownBetween20And30Percent() {
            PerformanceTracker tracker = new PerformanceTracker();
            double multiplier = tracker.getPositionSizeMultiplier(100_000.0, 75_000.0);
            then(multiplier).isEqualTo(0.25);
        }

        @Test
        @DisplayName("Should return 0.1 when drawdown exceeds 30%")
        void shouldReturnPointOne_WhenDrawdownExceeds30Percent() {
            PerformanceTracker tracker = new PerformanceTracker();
            double multiplier = tracker.getPositionSizeMultiplier(100_000.0, 60_000.0);
            then(multiplier).isEqualTo(0.1);
        }

        @Test
        @DisplayName("Should return 1.0 when peak equity is zero")
        void shouldReturnOne_WhenPeakEquityZero() {
            PerformanceTracker tracker = new PerformanceTracker();
            double multiplier = tracker.getPositionSizeMultiplier(0.0, 100_000.0);
            then(multiplier).isEqualTo(1.0);
        }

        @Test
        @DisplayName("Should return 1.0 when current equity is zero")
        void shouldReturnOne_WhenCurrentEquityZero() {
            PerformanceTracker tracker = new PerformanceTracker();
            double multiplier = tracker.getPositionSizeMultiplier(100_000.0, 0.0);
            then(multiplier).isEqualTo(1.0);
        }

        @Test
        @DisplayName("Should handle negative drawdown (new high)")
        void shouldReturnOne_WhenNewHigh() {
            PerformanceTracker tracker = new PerformanceTracker();
            double multiplier = tracker.getPositionSizeMultiplier(100_000.0, 110_000.0);
            then(multiplier).isEqualTo(1.0);
        }
    }

    @Nested
    @DisplayName("reset")
    class Reset {

        @Test
        @DisplayName("Should reset session stats and peak equity")
        void shouldResetSession() {
            PerformanceTracker tracker = new PerformanceTracker();
            tracker.registerTrade(100.0);
            tracker.registerTrade(-50.0);
            tracker.updateEquity(150_000.0);
            tracker.resetSession();

            PerformanceTracker.SessionStats stats = tracker.getSessionStats();
            then(stats.wins).isZero();
            then(stats.losses).isZero();
            then(stats.trades).isZero();
            then(tracker.getPeakEquity()).isZero();
        }

        @Test
        @DisplayName("Should preserve global peak after session reset")
        void shouldPreserveGlobalPeak() {
            PerformanceTracker tracker = new PerformanceTracker();
            tracker.updateEquity(150_000.0);
            tracker.resetSession();

            then(tracker.getGlobalPeakEquity()).isEqualTo(150_000);
        }

        @Test
        @DisplayName("Should reset only stats without affecting global peak")
        void shouldResetStatsOnly() {
            PerformanceTracker tracker = new PerformanceTracker();
            tracker.updateEquity(150_000.0);
            tracker.registerTrade(100.0);
            tracker.resetStatsOnly();

            PerformanceTracker.SessionStats stats = tracker.getSessionStats();
            then(stats.wins).isZero();
            then(tracker.getPeakEquity()).isEqualTo(150_000);
            then(tracker.getGlobalPeakEquity()).isEqualTo(150_000);
        }
    }
}
