package com.github.shk0da.goldendragon.model;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.BDDAssertions.then;

@DisplayName("PendingOrder entry condition from LLM debate")
class PendingOrderTest {

    private static final int TTL_MINUTES = 30;

    private static PendingOrder order(String direction, Double entryPrice, double priceAtCreation) {
        return new PendingOrder(
                "TEST", direction, entryPrice, 0.0, 0.0, 0.5,
                "test reasoning", TTL_MINUTES, priceAtCreation);
    }

    @Nested
    @DisplayName("Breakdown short (OZON case: entry below creation price)")
    class BreakdownShort {

        private final PendingOrder pending = order("SELL", 2590.5, 2600.5);

        @Test
        @DisplayName("Should not enter at creation price above breakdown target")
        void shouldNotEnter_AtCreationPrice() {
            // Given: council rejected immediate short at 2600.5, waiting for breakdown to 2590.5
            // When / Then
            then(pending.shouldEnter(2600.5)).isFalse();
        }

        @Test
        @DisplayName("Should not enter while price is above touch band")
        void shouldNotEnter_AboveTouchBand() {
            // Given: touch band upper bound is 2590.5 * 1.001 = 2593.09
            // When / Then
            then(pending.shouldEnter(2594.0)).isFalse();
        }

        @Test
        @DisplayName("Should enter when price touches breakdown target")
        void shouldEnter_OnTouch() {
            // When / Then
            then(pending.shouldEnter(2593.0)).isTrue();
            then(pending.shouldEnter(2590.5)).isTrue();
        }

        @Test
        @DisplayName("Should enter on overshoot within drift guard")
        void shouldEnter_OnOvershootWithinDrift() {
            // Given: drift lower bound is 2590.5 * 0.995 = 2577.55
            // When / Then
            then(pending.shouldEnter(2580.0)).isTrue();
        }

        @Test
        @DisplayName("Should not chase price gapped far below target")
        void shouldNotChase_GapBelowTarget() {
            // When / Then
            then(pending.shouldEnter(2570.0)).isFalse();
        }
    }

    @Nested
    @DisplayName("Pullback long (SNGSP case: entry below creation price)")
    class PullbackLong {

        private final PendingOrder pending = order("BUY", 41.88, 41.935);

        @Test
        @DisplayName("Should not enter at creation price above limit target")
        void shouldNotEnter_AtCreationPrice() {
            // Given: touch band upper bound is 41.88 * 1.001 = 41.92
            // When / Then
            then(pending.shouldEnter(41.935)).isFalse();
        }

        @Test
        @DisplayName("Should enter when price falls to limit target")
        void shouldEnter_OnPullback() {
            // When / Then
            then(pending.shouldEnter(41.92)).isTrue();
            then(pending.shouldEnter(41.88)).isTrue();
        }

        @Test
        @DisplayName("Should not chase price fallen far below target")
        void shouldNotChase_DeepFall() {
            // Given: drift lower bound is 41.88 * 0.995 = 41.67
            // When / Then
            then(pending.shouldEnter(41.60)).isFalse();
        }
    }

    @Nested
    @DisplayName("Retest short (GMKN case: entry above creation price)")
    class RetestShort {

        private final PendingOrder pending = order("SELL", 130.5, 130.2);

        @Test
        @DisplayName("Should not enter at creation price below retest target")
        void shouldNotEnter_AtCreationPrice() {
            // Given: touch band lower bound is 130.5 * 0.999 = 130.37
            // When / Then
            then(pending.shouldEnter(130.2)).isFalse();
        }

        @Test
        @DisplayName("Should enter when price rises to retest target")
        void shouldEnter_OnRetest() {
            // When / Then
            then(pending.shouldEnter(130.40)).isTrue();
            then(pending.shouldEnter(130.50)).isTrue();
        }

        @Test
        @DisplayName("Should not chase price risen far above target")
        void shouldNotChase_RunawayUp() {
            // Given: drift upper bound is 130.5 * 1.005 = 131.15
            // When / Then
            then(pending.shouldEnter(131.50)).isFalse();
        }
    }

    @Nested
    @DisplayName("Breakout long (entry above creation price)")
    class BreakoutLong {

        private final PendingOrder pending = order("BUY", 101.0, 100.0);

        @Test
        @DisplayName("Should not enter before breakout")
        void shouldNotEnter_BeforeBreakout() {
            // Given: touch band lower bound is 101.0 * 0.999 = 100.899
            // When / Then
            then(pending.shouldEnter(100.0)).isFalse();
        }

        @Test
        @DisplayName("Should enter on breakout touch")
        void shouldEnter_OnBreakout() {
            // When / Then
            then(pending.shouldEnter(100.95)).isTrue();
            then(pending.shouldEnter(101.0)).isTrue();
        }

        @Test
        @DisplayName("Should not chase price far above breakout level")
        void shouldNotChase_RunawayUp() {
            // Given: drift upper bound is 101.0 * 1.005 = 101.505
            // When / Then
            then(pending.shouldEnter(102.0)).isFalse();
        }
    }

    @Nested
    @DisplayName("Immediate entry (entry within touch tolerance of creation price)")
    class ImmediateEntry {

        private final PendingOrder pending = order("BUY", 100.05, 100.0);

        @Test
        @DisplayName("Should enter while price stays within drift guard")
        void shouldEnter_WithinDrift() {
            // Given: 100.05 is within 0.1% of 100.0, drift band is +/-0.5% of entry
            // When / Then
            then(pending.shouldEnter(100.05)).isTrue();
            then(pending.shouldEnter(100.0)).isTrue();
        }

        @Test
        @DisplayName("Should not enter when price drifted away before execution")
        void shouldNotEnter_AfterDrift() {
            // When / Then
            then(pending.shouldEnter(101.0)).isFalse();
        }
    }

    @Nested
    @DisplayName("Edge cases")
    class EdgeCases {

        @Test
        @DisplayName("Should not enter with null entry price")
        void shouldNotEnter_NullEntry() {
            then(order("BUY", null, 100.0).shouldEnter(100.0)).isFalse();
        }

        @Test
        @DisplayName("Should not enter with non-positive prices")
        void shouldNotEnter_NonPositivePrices() {
            PendingOrder pending = order("BUY", 100.0, 101.0);
            then(pending.shouldEnter(0.0)).isFalse();
            then(pending.shouldEnter(-1.0)).isFalse();
            then(order("BUY", 0.0, 100.0).shouldEnter(100.0)).isFalse();
        }

        @Test
        @DisplayName("Should not be expired within TTL")
        void shouldNotBeExpired_WithinTtl() {
            then(order("BUY", 100.0, 100.0).isExpired()).isFalse();
        }
    }

    @Nested
    @DisplayName("Entry mode description")
    class EntryModeDescription {

        @Test
        @DisplayName("Should describe mode from direction and target side")
        void shouldDescribeMode() {
            then(order("SELL", 2590.5, 2600.5).describeEntryMode())
                    .isEqualTo("wait for breakdown down to 2590.5");
            then(order("BUY", 41.88, 41.935).describeEntryMode())
                    .isEqualTo("wait for pullback down to 41.88");
            then(order("SELL", 130.5, 130.2).describeEntryMode())
                    .isEqualTo("wait for retest up to 130.5");
            then(order("BUY", 101.0, 100.0).describeEntryMode())
                    .isEqualTo("wait for breakout up to 101.0");
            then(order("BUY", 100.05, 100.0).describeEntryMode())
                    .isEqualTo("market entry");
        }
    }
}
