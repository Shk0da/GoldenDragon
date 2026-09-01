package com.github.shk0da.goldendragon.strategy.orderbook;

import com.github.shk0da.goldendragon.model.MarketTradeTick;

import org.junit.jupiter.api.Test;

import java.time.Instant;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * Tests for {@link VpinCalculator} trade direction classification.
 */
class VpinCalculatorTest {

    private static final double DELTA = 0.01;

    private MarketTradeTick trade(String direction, int quantity) {
        return new MarketTradeTick("TEST-FIGI", Instant.EPOCH, 5100.0, quantity, direction);
    }

    @Test
    void testBuyDirectionClassifiesBuyTrade() {
        VpinCalculator calc = new VpinCalculator(100);
        calc.onTrade("TEST", trade("BUY", 100));

        assertEquals(1, calc.getCompletedBucketCount("TEST"));
        // All buy = max imbalance 1.0
        assertEquals(1.0, calc.getVpin("TEST"), DELTA);
    }

    @Test
    void testSellDirectionClassifiesSellTrade() {
        VpinCalculator calc = new VpinCalculator(100);
        calc.onTrade("TEST", trade("SELL", 100));

        assertEquals(1, calc.getCompletedBucketCount("TEST"));
        // All sell = max imbalance 1.0
        assertEquals(1.0, calc.getVpin("TEST"), DELTA);
    }

    @Test
    void testBidTreatedAsBuyLegacy() {
        VpinCalculator calc = new VpinCalculator(100);
        calc.onTrade("TEST", trade("BID", 100));

        assertEquals(1, calc.getCompletedBucketCount("TEST"));
        assertEquals(1.0, calc.getVpin("TEST"), DELTA);
    }

    @Test
    void testBalancedVolumeGivesZeroImbalance() {
        VpinCalculator calc = new VpinCalculator(100);
        calc.onTrade("TEST", trade("BUY", 50));
        calc.onTrade("TEST", trade("SELL", 50));

        assertEquals(1, calc.getCompletedBucketCount("TEST"));
        // 50 buy + 50 sell = 0 imbalance
        assertEquals(0.0, calc.getVpin("TEST"), DELTA);
    }

    @Test
    void testNullDirectionTreatedAsSell() {
        VpinCalculator calc = new VpinCalculator(100);
        calc.onTrade("TEST", trade(null, 100));

        assertEquals(1, calc.getCompletedBucketCount("TEST"));
        // Sell-only bucket
        assertEquals(1.0, calc.getVpin("TEST"), DELTA);
    }

    @Test
    void testEmptyDirectionTreatedAsSell() {
        VpinCalculator calc = new VpinCalculator(100);
        calc.onTrade("TEST", trade("", 100));

        assertEquals(1, calc.getCompletedBucketCount("TEST"));
        assertEquals(1.0, calc.getVpin("TEST"), DELTA);
    }

    @Test
    void testMixedImbalancePartialBucket() {
        VpinCalculator calc = new VpinCalculator(100);
        calc.onTrade("TEST", trade("BUY", 60));
        calc.onTrade("TEST", trade("SELL", 40));

        assertEquals(1, calc.getCompletedBucketCount("TEST"));
        // |60-40| / 100 = 0.2
        assertEquals(0.2, calc.getVpin("TEST"), DELTA);
    }

    @Test
    void testMultipleBucketsVPIN() {
        VpinCalculator calc = new VpinCalculator(100, 10);
        // Bucket 1: all buy
        calc.onTrade("TEST", trade("BUY", 100));
        // Bucket 2: all sell
        calc.onTrade("TEST", trade("SELL", 100));

        assertEquals(2, calc.getCompletedBucketCount("TEST"));
        // Average of [1.0, 1.0] = 1.0
        assertEquals(1.0, calc.getVpin("TEST"), DELTA);
    }

    @Test
    void testVpinZeroForNoTrades() {
        VpinCalculator calc = new VpinCalculator(100);
        assertEquals(0.0, calc.getVpin("TEST"), DELTA);
        assertEquals(0, calc.getCompletedBucketCount("TEST"));
    }

    @Test
    void testConstructorRejectsInvalidBucketSize() {
        assertThrows(IllegalArgumentException.class, () -> new VpinCalculator(0));
        assertThrows(IllegalArgumentException.class, () -> new VpinCalculator(-10));
    }

    @Test
    void testResetClearsState() {
        VpinCalculator calc = new VpinCalculator(100);
        calc.onTrade("TEST", trade("BUY", 100));

        assertEquals(1, calc.getCompletedBucketCount("TEST"));
        assertEquals(1.0, calc.getVpin("TEST"), DELTA);

        calc.reset("TEST");

        assertEquals(0, calc.getCompletedBucketCount("TEST"));
        assertEquals(0.0, calc.getVpin("TEST"), DELTA);
    }
}
