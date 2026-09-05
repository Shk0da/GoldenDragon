package com.github.shk0da.goldendragon.strategy.orderbook;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.BDDAssertions.then;

@DisplayName("OrderBookMath")
class OrderBookMathTest {

    @Nested
    @DisplayName("When calculating OBI (Order Book Imbalance)")
    class ObiCalculation {

        @Test
        @DisplayName("Should calculate positive OBI when bids dominate")
        void shouldCalculatePositiveObi_WhenBidsDominate() {
            // Given
            var bids = createLevels(100, 100, 100);
            var asks = createLevels(50, 50, 50);

            // When
            double obi = OrderBookMath.calculateObi(bids, asks, 3);

            // Then
            // (300 - 150) / (300 + 150) = 150 / 450 = 0.333
            then(obi).isCloseTo(0.333, within(0.001));
        }

        @Test
        @DisplayName("Should calculate negative OBI when asks dominate")
        void shouldCalculateNegativeObi_WhenAsksDominate() {
            // Given
            var bids = createLevels(50, 50, 50);
            var asks = createLevels(100, 100, 100);

            // When
            double obi = OrderBookMath.calculateObi(bids, asks, 3);

            // Then
            // (150 - 300) / (150 + 300) = -150 / 450 = -0.333
            then(obi).isCloseTo(-0.333, within(0.001));
        }

        @Test
        @DisplayName("Should calculate zero OBI when balanced")
        void shouldCalculateZeroObi_WhenBalanced() {
            // Given
            var bids = createLevels(100, 100, 100);
            var asks = createLevels(100, 100, 100);

            // When
            double obi = OrderBookMath.calculateObi(bids, asks, 3);

            // Then
            then(obi).isZero();
        }

        @Test
        @DisplayName("Should handle empty levels")
        void shouldHandleEmptyLevels() {
            // When
            double obi = OrderBookMath.calculateObi(null, null, 3);

            // Then
            then(obi).isZero();
        }

        @Test
        @DisplayName("Should handle zero total volume")
        void shouldHandleZeroTotalVolume() {
            // Given
            var bids = createLevels(0, 0, 0);
            var asks = createLevels(0, 0, 0);

            // When
            double obi = OrderBookMath.calculateObi(bids, asks, 3);

            // Then
            then(obi).isZero();
        }
    }

    @Nested
    @DisplayName("When calculating microprice edge")
    class MicropriceEdge {

        @Test
        @DisplayName("Should calculate zero edge when balanced")
        void shouldCalculateZeroEdge_WhenBalanced() {
            // Given
            double bestBid = 100.0;
            double bestAsk = 102.0;
            int bidQty = 100;
            int askQty = 100;

            // When
            double edge = OrderBookMath.calculateMicroEdge(bestBid, bestAsk, bidQty, askQty);

            // Then
            // micro = (100*100 + 102*100) / 200 = 101
            // mid = (100 + 102) / 2 = 101
            // edge = 101 - 101 = 0
            then(edge).isZero();
        }

        @Test
        @DisplayName("Should calculate positive edge when bid qty is higher")
        void shouldCalculatePositiveEdge_WhenBidQtyHigher() {
            // Given
            double bestBid = 100.0;
            double bestAsk = 102.0;
            int bidQty = 200;
            int askQty = 100;

            // When
            double edge = OrderBookMath.calculateMicroEdge(bestBid, bestAsk, bidQty, askQty);

            // Then
            // micro = (100*100 + 102*200) / 300 = 101.33
            // mid = 101
            // edge = 0.33
            then(edge).isCloseTo(0.333, within(0.001));
        }

        @Test
        @DisplayName("Should calculate negative edge when ask qty is higher")
        void shouldCalculateNegativeEdge_WhenAskQtyHigher() {
            // Given
            double bestBid = 100.0;
            double bestAsk = 102.0;
            int bidQty = 100;
            int askQty = 200;

            // When
            double edge = OrderBookMath.calculateMicroEdge(bestBid, bestAsk, bidQty, askQty);

            // Then
            // micro = (100*200 + 102*100) / 300 = 100.67
            // mid = 101
            // edge = -0.33
            then(edge).isCloseTo(-0.333, within(0.001));
        }

        @Test
        @DisplayName("Should handle zero quantities")
        void shouldHandleZeroQuantities() {
            // Given
            double bestBid = 100.0;
            double bestAsk = 102.0;
            int bidQty = 0;
            int askQty = 0;

            // When
            double edge = OrderBookMath.calculateMicroEdge(bestBid, bestAsk, bidQty, askQty);

            // Then
            then(edge).isZero();
        }
    }

    @Nested
    @DisplayName("When calculating trade delta")
    class TradeDelta {

        @Test
        @DisplayName("Should calculate positive delta for buy volume")
        void shouldCalculatePositiveDelta_ForBuyVolume() {
            // Given
            var trades = java.util.List.of(
                    createTrade("Buy", 100),
                    createTrade("Buy", 50)
            );

            // When
            double delta = OrderBookMath.calculateTradeDelta(trades);

            // Then
            then(delta).isEqualTo(150.0);
        }

        @Test
        @DisplayName("Should calculate negative delta for sell volume")
        void shouldCalculateNegativeDelta_ForSellVolume() {
            // Given
            var trades = java.util.List.of(
                    createTrade("Sell", 100),
                    createTrade("Sell", 50)
            );

            // When
            double delta = OrderBookMath.calculateTradeDelta(trades);

            // Then
            then(delta).isEqualTo(-150.0);
        }

        @Test
        @DisplayName("Should calculate net delta for mixed trades")
        void shouldCalculateNetDelta_ForMixedTrades() {
            // Given
            var trades = java.util.List.of(
                    createTrade("Buy", 100),
                    createTrade("Sell", 60)
            );

            // When
            double delta = OrderBookMath.calculateTradeDelta(trades);

            // Then
            then(delta).isEqualTo(40.0);
        }

        @Test
        @DisplayName("Should handle empty trades list")
        void shouldHandleEmptyTradesList() {
            // When
            double delta = OrderBookMath.calculateTradeDelta(java.util.List.of());

            // Then
            then(delta).isZero();
        }
    }

    @Nested
    @DisplayName("When calculating weighted depth imbalance")
    class WeightedDepthImbalance {

        @Test
        @DisplayName("Should calculate positive imbalance when bids are weighted heavier")
        void shouldCalculatePositiveImbalance_WhenBidsHeavier() {
            // Given - bids have more volume at closer levels (higher weight)
            var bids = createLevels(100, 50);
            var asks = createLevels(50, 100);

            // When
            double imbalance = OrderBookMath.calculateWeightedDepthImbalance(bids, asks, 2);

            // Then
            // bid: 100*1.0 + 50*0.5 = 125
            // ask: 50*1.0 + 100*0.5 = 100
            // imbalance: (125 - 100) / 225 = 0.111
            then(imbalance).isCloseTo(0.111, within(0.001));
        }

        @Test
        @DisplayName("Should handle null levels")
        void shouldHandleNullLevels() {
            // When
            double imbalance = OrderBookMath.calculateWeightedDepthImbalance(null, null, 3);

            // Then
            then(imbalance).isZero();
        }
    }

    @Nested
    @DisplayName("When calculating depth gradient")
    class DepthGradient {

        @Test
        @DisplayName("Should calculate gradient for normal book")
        void shouldCalculateGradient_ForNormalBook() {
            // Given
            var bids = createLevels(100, 150); // increasing away from mid
            var asks = createLevels(100, 150); // increasing away from mid

            // When
            double gradient = OrderBookMath.calculateDepthGradient(bids, asks, 2);

            // Then
            // bidGradient: (150 - 100) / 100 = 0.5
            // askGradient: (150 - 100) / 100 = 0.5
            // gradient: 0.5 - 0.5 = 0
            then(gradient).isZero();
        }

        @Test
        @DisplayName("Should handle insufficient levels")
        void shouldHandleInsufficientLevels() {
            // Given
            var bids = createLevels(100);
            var asks = createLevels(100);

            // When
            double gradient = OrderBookMath.calculateDepthGradient(bids, asks, 2);

            // Then
            then(gradient).isZero();
        }
    }

    @Nested
    @DisplayName("When calculating absorption score")
    class AbsorptionScore {

        @Test
        @DisplayName("Should calculate positive score for bid absorption")
        void shouldCalculatePositiveScore_ForBidAbsorption() {
            // Given - large bid at top
            var bids = createLevels(500, 100, 100);
            var asks = createLevels(100, 100, 100);

            // When
            double score = OrderBookMath.calculateAbsorptionScore(bids, asks);

            // Then
            then(score).isPositive();
        }

        @Test
        @DisplayName("Should calculate negative score for ask absorption")
        void shouldCalculateNegativeScore_ForAskAbsorption() {
            // Given - large ask at top
            var bids = createLevels(100, 100, 100);
            var asks = createLevels(500, 100, 100);

            // When
            double score = OrderBookMath.calculateAbsorptionScore(bids, asks);

            // Then
            then(score).isNegative();
        }

        @Test
        @DisplayName("Should handle empty levels")
        void shouldHandleEmptyLevels() {
            // When
            double score = OrderBookMath.calculateAbsorptionScore(null, null);

            // Then
            then(score).isZero();
        }
    }

    // Helper methods
    private static java.util.List<com.github.shk0da.goldendragon.model.MarketDepthLevel> createLevels(int... quantities) {
        var levels = new java.util.ArrayList<com.github.shk0da.goldendragon.model.MarketDepthLevel>();
        for (int qty : quantities) {
            levels.add(new com.github.shk0da.goldendragon.model.MarketDepthLevel(100.0, qty));
        }
        return levels;
    }

    private static com.github.shk0da.goldendragon.model.MarketTradeTick createTrade(String direction, long quantity) {
        return new com.github.shk0da.goldendragon.model.MarketTradeTick(
                "TEST", java.time.Instant.now(), 100.0, quantity, direction);
    }

    private static org.assertj.core.data.Offset<Double> within(double value) {
        return org.assertj.core.data.Offset.offset(value);
    }
}
