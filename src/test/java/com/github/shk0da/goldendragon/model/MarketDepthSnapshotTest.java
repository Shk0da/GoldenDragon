package com.github.shk0da.goldendragon.model;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.time.Instant;
import java.util.List;

import static org.assertj.core.api.BDDAssertions.then;

@DisplayName("MarketDepthSnapshot")
class MarketDepthSnapshotTest {

    private final Instant testTime = Instant.parse("2024-01-01T10:00:00Z");

    private MarketDepthLevel bid(double price, int quantity) {
        return new MarketDepthLevel(price, quantity);
    }

    private MarketDepthLevel ask(double price, int quantity) {
        return new MarketDepthLevel(price, quantity);
    }

    @Nested
    @DisplayName("Constructor")
    class Constructor {

        @Test
        @DisplayName("Should initialize with all fields")
        void shouldInitializeWithAllFields() {
            List<MarketDepthLevel> bids = List.of(bid(100.0, 100), bid(99.0, 200));
            List<MarketDepthLevel> asks = List.of(bid(101.0, 150), bid(102.0, 250));

            MarketDepthSnapshot snapshot = new MarketDepthSnapshot(
                    "FIGI001", testTime, true, bids, asks
            );

            then(snapshot.getFigi()).isEqualTo("FIGI001");
            then(snapshot.getTime()).isEqualTo(testTime);
            then(snapshot.isConsistent()).isTrue();
            then(snapshot.getBids()).hasSize(2);
            then(snapshot.getAsks()).hasSize(2);
        }

        @Test
        @DisplayName("Should handle null bids as empty list")
        void shouldHandleNullBidsAsEmptyList() {
            MarketDepthSnapshot snapshot = new MarketDepthSnapshot(
                    "FIGI001", testTime, true, null, null
            );

            then(snapshot.getBids()).isEmpty();
            then(snapshot.getAsks()).isEmpty();
        }
    }

    @Nested
    @DisplayName("getBestBid()")
    class GetBestBid {

        @Test
        @DisplayName("Should return best bid from non-empty bids")
        void shouldReturnBestBidFromNonEmptyBids() {
            MarketDepthSnapshot snapshot = new MarketDepthSnapshot(
                    "FIGI001", testTime, true,
                    List.of(bid(100.0, 100), bid(99.0, 200), bid(98.0, 300)),
                    List.of()
            );

            then(snapshot.getBestBid()).isEqualTo(100.0);
        }

        @Test
        @DisplayName("Should return null when bids are empty")
        void shouldReturnNullWhenBidsEmpty() {
            MarketDepthSnapshot snapshot = new MarketDepthSnapshot(
                    "FIGI001", testTime, true, List.of(), List.of()
            );

            then(snapshot.getBestBid()).isNull();
        }

        @Test
        @DisplayName("Should return null when bids are null")
        void shouldReturnNullWhenBidsNull() {
            MarketDepthSnapshot snapshot = new MarketDepthSnapshot(
                    "FIGI001", testTime, true, null, List.of()
            );

            then(snapshot.getBestBid()).isNull();
        }
    }

    @Nested
    @DisplayName("getBestAsk()")
    class GetBestAsk {

        @Test
        @DisplayName("Should return best ask from non-empty asks")
        void shouldReturnBestAskFromNonEmptyAsks() {
            MarketDepthSnapshot snapshot = new MarketDepthSnapshot(
                    "FIGI001", testTime, true,
                    List.of(),
                    List.of(ask(101.0, 150), ask(102.0, 250), ask(103.0, 350))
            );

            then(snapshot.getBestAsk()).isEqualTo(101.0);
        }

        @Test
        @DisplayName("Should return null when asks are empty")
        void shouldReturnNullWhenAsksEmpty() {
            MarketDepthSnapshot snapshot = new MarketDepthSnapshot(
                    "FIGI001", testTime, true, List.of(), List.of()
            );

            then(snapshot.getBestAsk()).isNull();
        }

        @Test
        @DisplayName("Should return null when asks are null")
        void shouldReturnNullWhenAsksNull() {
            MarketDepthSnapshot snapshot = new MarketDepthSnapshot(
                    "FIGI001", testTime, true, List.of(), null
            );

            then(snapshot.getBestAsk()).isNull();
        }
    }

    @Nested
    @DisplayName("getMidPrice()")
    class GetMidPrice {

        @Test
        @DisplayName("Should return mid price from valid bid and ask")
        void shouldReturnMidPriceFromValidBidAndAsk() {
            MarketDepthSnapshot snapshot = new MarketDepthSnapshot(
                    "FIGI001", testTime, true,
                    List.of(bid(100.0, 100)),
                    List.of(ask(102.0, 150))
            );

            then(snapshot.getMidPrice()).isEqualTo(101.0);
        }

        @Test
        @DisplayName("Should return null when bids are empty")
        void shouldReturnNullWhenBidsEmpty() {
            MarketDepthSnapshot snapshot = new MarketDepthSnapshot(
                    "FIGI001", testTime, true, List.of(),
                    List.of(ask(102.0, 150))
            );

            then(snapshot.getMidPrice()).isNull();
        }

        @Test
        @DisplayName("Should return null when asks are empty")
        void shouldReturnNullWhenAsksEmpty() {
            MarketDepthSnapshot snapshot = new MarketDepthSnapshot(
                    "FIGI001", testTime, true,
                    List.of(bid(100.0, 100)), List.of()
            );

            then(snapshot.getMidPrice()).isNull();
        }

        @Test
        @DisplayName("Should return null when both are empty")
        void shouldReturnNullWhenBothEmpty() {
            MarketDepthSnapshot snapshot = new MarketDepthSnapshot(
                    "FIGI001", testTime, true, List.of(), List.of()
            );

            then(snapshot.getMidPrice()).isNull();
        }

        @Test
        @DisplayName("Should return null when both are null")
        void shouldReturnNullWhenBothNull() {
            MarketDepthSnapshot snapshot = new MarketDepthSnapshot(
                    "FIGI001", testTime, true, null, null
            );

            then(snapshot.getMidPrice()).isNull();
        }

        @Test
        @DisplayName("Should calculate mid price with uneven numbers")
        void shouldCalculateMidPriceWithEvenNumbers() {
            MarketDepthSnapshot snapshot = new MarketDepthSnapshot(
                    "FIGI001", testTime, true,
                    List.of(bid(99.0, 100)),
                    List.of(ask(101.0, 150))
            );

            then(snapshot.getMidPrice()).isEqualTo(100.0);
        }

        @Test
        @DisplayName("Should calculate mid price with odd numbers")
        void shouldCalculateMidPriceWithOddBidAndAsk() {
            MarketDepthSnapshot snapshot = new MarketDepthSnapshot(
                    "FIGI001", testTime, true,
                    List.of(bid(99.5, 100)),
                    List.of(ask(100.5, 150))
            );

            then(snapshot.getMidPrice()).isEqualTo(100.0);
        }
    }

    @Nested
    @DisplayName("IsConsistent")
    class IsConsistent {

        @Test
        @DisplayName("Should return consistent=true when passed")
        void shouldReturnConsistentTrue() {
            MarketDepthSnapshot snapshot = new MarketDepthSnapshot(
                    "FIGI001", testTime, true, List.of(), List.of()
            );

            then(snapshot.isConsistent()).isTrue();
        }

        @Test
        @DisplayName("Should return consistent=false when passed")
        void shouldReturnConsistentFalse() {
            MarketDepthSnapshot snapshot = new MarketDepthSnapshot(
                    "FIGI001", testTime, false, List.of(), List.of()
            );

            then(snapshot.isConsistent()).isFalse();
        }
    }

    @Nested
    @DisplayName("getBids()/getAsks()")
    class GetBidsAndAsks {

        @Test
        @DisplayName("Should return unmodifiable lists")
        void shouldReturnUnmodifiableLists() {
            List<MarketDepthLevel> bids = List.of(bid(100.0, 100));
            MarketDepthSnapshot snapshot = new MarketDepthSnapshot(
                    "FIGI001", testTime, true, bids, List.of()
            );

            then(snapshot.getBids()).isNotSameAs(bids);
            then(snapshot.getBids()).hasSize(1);

            try {
                snapshot.getBids().add(bid(99.0, 200));
                then(false).isTrue();
            } catch (UnsupportedOperationException e) {
                then(true).isTrue();
            }
        }

        @Test
        @DisplayName("Should return snapshot from empty lists")
        void shouldReturnSnapshotFromEmptyLists() {
            MarketDepthSnapshot snapshot = new MarketDepthSnapshot(
                    "FIGI001", testTime, true, List.of(), List.of()
            );

            then(snapshot.getBids()).isEmpty();
            then(snapshot.getAsks()).isEmpty();
        }
    }
}
