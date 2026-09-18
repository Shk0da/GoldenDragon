package com.github.shk0da.goldendragon.money;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.within;
import static org.assertj.core.api.BDDAssertions.then;

@DisplayName("PerformanceTracker")
class PerformanceTrackerTest {

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
        void shouldTrackGlobalPeak() {
            PerformanceTracker tracker = new PerformanceTracker();
            tracker.updateEquity(100_000.0);
            tracker.updateEquity(150_000.0);
            tracker.resetSession();
            tracker.updateEquity(120_000.0);

            then(tracker.getGlobalPeakEquity()).isEqualTo(150_000);
            then(tracker.getPeakEquity()).isEqualTo(120_000);
        }
    }

    @Nested
    @DisplayName("getCurrentDrawdown")
    class GetCurrentDrawdown {

        @Test
        @DisplayName("Should calculate drawdown from peak")
        void shouldCalculateDrawdown() {
            PerformanceTracker tracker = new PerformanceTracker();
            tracker.updateEquity(100_000.0);
            tracker.updateEquity(150_000.0);
            tracker.updateEquity(120_000.0);

            then(tracker.getCurrentDrawdown()).isEqualTo(0.20);
        }

        @Test
        @DisplayName("Should return 0 when equity equals peak")
        void shouldReturnZeroAtPeak() {
            PerformanceTracker tracker = new PerformanceTracker();
            tracker.updateEquity(100_000.0);

            then(tracker.getCurrentDrawdown()).isZero();
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
    }
}
