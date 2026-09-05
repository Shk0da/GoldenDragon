package com.github.shk0da.goldendragon.strategy.orderbook;

import com.github.shk0da.goldendragon.model.MarketTradeTick;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.time.Instant;

import static org.assertj.core.api.BDDAssertions.then;
import static org.assertj.core.api.BDDAssertions.thenThrownBy;

/**
 * BDD-style tests for {@link VpinCalculator} trade direction classification.
 */
@DisplayName("VPIN Calculator - Trade Direction Classification")
class VpinCalculatorTest {

    private static final double DELTA = 0.01;

    private MarketTradeTick trade(String direction, int quantity) {
        return new MarketTradeTick("TEST-FIGI", Instant.EPOCH, 5100.0, quantity, direction);
    }

    @Nested
    @DisplayName("When classifying trades")
    class TradeClassification {

        @Test
        @DisplayName("Should classify all BUY trades as maximum VPIN (1.0)")
        void shouldReturnMaxVpin_WhenAllTradesAreBuy() {
            // Given
            VpinCalculator calc = new VpinCalculator(100);
            MarketTradeTick buyTrade = trade("BUY", 100);

            // When
            calc.onTrade("TEST", buyTrade);

            // Then
            then(calc.getCompletedBucketCount("TEST")).isEqualTo(1);
            then(calc.getVpin("TEST")).isEqualTo(1.0)
                    .as("All buy trades should result in maximum VPIN of 1.0");
        }

        @Test
        @DisplayName("Should classify all SELL trades as maximum VPIN (1.0)")
        void shouldReturnMaxVpin_WhenAllTradesAreSell() {
            // Given
            VpinCalculator calc = new VpinCalculator(100);
            MarketTradeTick sellTrade = trade("SELL", 100);

            // When
            calc.onTrade("TEST", sellTrade);

            // Then
            then(calc.getCompletedBucketCount("TEST")).isEqualTo(1);
            then(calc.getVpin("TEST")).isEqualTo(1.0)
                    .as("All sell trades should result in maximum VPIN of 1.0");
        }

        @Test
        @DisplayName("Should treat BID direction as BUY")
        void shouldTreatBidAsBuy() {
            // Given
            VpinCalculator calc = new VpinCalculator(100);
            MarketTradeTick bidTrade = trade("BID", 100);

            // When
            calc.onTrade("TEST", bidTrade);

            // Then
            then(calc.getCompletedBucketCount("TEST")).isEqualTo(1);
            then(calc.getVpin("TEST")).isEqualTo(1.0);
        }

        @Test
        @DisplayName("Should return zero VPIN for balanced buy/sell volume")
        void shouldReturnZeroVpin_WhenVolumeIsBalanced() {
            // Given
            VpinCalculator calc = new VpinCalculator(100);

            // When
            calc.onTrade("TEST", trade("BUY", 50));
            calc.onTrade("TEST", trade("SELL", 50));

            // Then
            then(calc.getCompletedBucketCount("TEST")).isEqualTo(1);
            then(calc.getVpin("TEST")).isEqualTo(0.0)
                    .as("Balanced buy/sell volume should result in zero VPIN");
        }
    }

    @Nested
    @DisplayName("When handling null/empty directions")
    class NullEmptyDirection {

        @Test
        @DisplayName("Should treat null direction as SELL")
        void shouldTreatNullDirectionAsSell() {
            // Given
            VpinCalculator calc = new VpinCalculator(100);

            // When
            calc.onTrade("TEST", trade(null, 100));

            // Then
            then(calc.getCompletedBucketCount("TEST")).isEqualTo(1);
            then(calc.getVpin("TEST")).isEqualTo(1.0)
                    .as("Null direction should be treated as sell");
        }

        @Test
        @DisplayName("Should treat empty direction as SELL")
        void shouldTreatEmptyDirectionAsSell() {
            // Given
            VpinCalculator calc = new VpinCalculator(100);

            // When
            calc.onTrade("TEST", trade("", 100));

            // Then
            then(calc.getCompletedBucketCount("TEST")).isEqualTo(1);
            then(calc.getVpin("TEST")).isEqualTo(1.0)
                    .as("Empty direction should be treated as sell");
        }
    }

    @Nested
    @DisplayName("When calculating partial bucket imbalance")
    class PartialBucket {

        @Test
        @DisplayName("Should calculate correct VPIN for mixed buy/sell in single bucket")
        void shouldCalculatePartialImbalance() {
            // Given
            VpinCalculator calc = new VpinCalculator(100);

            // When
            calc.onTrade("TEST", trade("BUY", 60));
            calc.onTrade("TEST", trade("SELL", 40));

            // Then
            then(calc.getCompletedBucketCount("TEST")).isEqualTo(1);
            then(calc.getVpin("TEST")).isEqualTo(0.2)
                    .as("|60-40| / 100 = 0.2");
        }
    }

    @Nested
    @DisplayName("When processing multiple buckets")
    class MultipleBuckets {

        @Test
        @DisplayName("Should average VPIN across completed buckets")
        void shouldAverageVpinAcrossBuckets() {
            // Given
            VpinCalculator calc = new VpinCalculator(100, 10);

            // When
            // Bucket 1: all buy
            calc.onTrade("TEST", trade("BUY", 100));
            // Bucket 2: all sell
            calc.onTrade("TEST", trade("SELL", 100));

            // Then
            then(calc.getCompletedBucketCount("TEST")).isEqualTo(2);
            then(calc.getVpin("TEST")).isEqualTo(1.0)
                    .as("Average of [1.0, 1.0] = 1.0");
        }
    }

    @Nested
    @DisplayName("When no trades processed")
    class NoTrades {

        @Test
        @DisplayName("Should return zero VPIN")
        void shouldReturnZeroVpin() {
            // Given
            VpinCalculator calc = new VpinCalculator(100);

            // When/Then
            then(calc.getVpin("TEST")).isEqualTo(0.0);
            then(calc.getCompletedBucketCount("TEST")).isEqualTo(0);
        }
    }

    @Nested
    @DisplayName("When constructor validation")
    class ConstructorValidation {

        @Test
        @DisplayName("Should reject zero bucket size")
        void shouldRejectZeroBucketSize() {
            // Given/When/Then
            thenThrownBy(() -> new VpinCalculator(0))
                    .isInstanceOf(IllegalArgumentException.class)
                    .hasMessageContaining("bucketSize must be positive");
        }

        @Test
        @DisplayName("Should reject negative bucket size")
        void shouldRejectNegativeBucketSize() {
            // Given/When/Then
            thenThrownBy(() -> new VpinCalculator(-10))
                    .isInstanceOf(IllegalArgumentException.class)
                    .hasMessageContaining("bucketSize must be positive");
        }
    }

    @Nested
    @DisplayName("When resetting state")
    class Reset {

        @Test
        @DisplayName("Should clear all VPIN state")
        void shouldClearState() {
            // Given
            VpinCalculator calc = new VpinCalculator(100);
            calc.onTrade("TEST", trade("BUY", 100));

            // When
            calc.reset("TEST");

            // Then
            then(calc.getCompletedBucketCount("TEST")).isEqualTo(0);
            then(calc.getVpin("TEST")).isEqualTo(0.0);
        }
    }
}
