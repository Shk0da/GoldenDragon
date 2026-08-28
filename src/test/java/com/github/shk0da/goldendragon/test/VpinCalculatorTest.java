package com.github.shk0da.goldendragon.test;

import com.github.shk0da.goldendragon.model.MarketTradeTick;
import com.github.shk0da.goldendragon.strategy.orderbook.VpinCalculator;

import java.time.Instant;

/**
 * Unit tests for VpinCalculator. Tests VPIN calculation for balanced flow, one-sided flow,
 * cold start behavior, and bucket count tracking.
 *
 * <p>Use: java -cp build/classes/java/test VpinCalculatorTest
 */
public class VpinCalculatorTest {

    /** Helper assertion methods. */
    private static void assertEquals(double expected, double actual, double delta, String message) {
        double diff = Math.abs(expected - actual);
        if (diff > delta) {
            throw new AssertionError(
                    message
                            + ": expected "
                            + expected
                            + " but got "
                            + actual
                            + " (diff: "
                            + diff
                            + ")");
        }
    }

    private static void assertEquals(int expected, int actual, String message) {
        if (expected != actual) {
            throw new AssertionError(
                    message + ": expected " + expected + " but got " + actual);
        }
    }

    private static void assertTrue(boolean condition, String message) {
        if (!condition) {
            throw new AssertionError(message);
        }
    }

    private static void printSuccess(String testName) {
        System.out.println("[PASS] " + testName);
    }

    private static void printFail(String testName, Throwable e) {
        System.err.println("[FAIL] " + testName + ": " + e.getMessage());
        e.printStackTrace(System.err);
    }

    // =========================================================================
    // VPIN CALCULATION TESTS
    // =========================================================================

    public static void testVpinColdStart() {
        VpinCalculator calc = new VpinCalculator(100, 5);
        double vpin = calc.getVpin("TEST");
        assertEquals(0.0, vpin, 0.001, "Cold start should return 0.0");
        assertEquals(0, calc.getCompletedBucketCount("TEST"), "Cold start should have 0 buckets");
    }

    public static void testVpinBalancedFlow() {
        VpinCalculator calc = new VpinCalculator(100, 5);
        Instant now = Instant.now();

        // Add balanced buy/sell trades to fill 5 buckets
        for (int i = 0; i < 5; i++) {
            calc.onTrade("TEST", new MarketTradeTick("FIGI", now, 100.0, 50, "BUY"));
            calc.onTrade("TEST", new MarketTradeTick("FIGI", now, 100.0, 50, "SELL"));
        }

        double vpin = calc.getVpin("TEST");
        // Balanced flow: each bucket has 50 buy + 50 sell = imbalance 0
        assertEquals(0.0, vpin, 0.001, "Balanced flow should have VPIN near 0");
        assertEquals(5, calc.getCompletedBucketCount("TEST"), "Should have 5 completed buckets");
    }

    public static void testVpinOneSidedBuyFlow() {
        VpinCalculator calc = new VpinCalculator(100, 5);
        Instant now = Instant.now();

        // Add only buy trades to fill 5 buckets
        for (int i = 0; i < 5; i++) {
            calc.onTrade("TEST", new MarketTradeTick("FIGI", now, 100.0, 100, "BUY"));
        }

        double vpin = calc.getVpin("TEST");
        // One-sided buy flow: each bucket has 100 buy + 0 sell = imbalance 1.0
        assertEquals(1.0, vpin, 0.001, "One-sided buy flow should have VPIN near 1.0");
    }

    public static void testVpinOneSidedSellFlow() {
        VpinCalculator calc = new VpinCalculator(100, 5);
        Instant now = Instant.now();

        // Add only sell trades to fill 5 buckets
        for (int i = 0; i < 5; i++) {
            calc.onTrade("TEST", new MarketTradeTick("FIGI", now, 100.0, 100, "SELL"));
        }

        double vpin = calc.getVpin("TEST");
        // One-sided sell flow: each bucket has 0 buy + 100 sell = imbalance 1.0
        assertEquals(1.0, vpin, 0.001, "One-sided sell flow should have VPIN near 1.0");
    }

    public static void testVpinBucketHistoryCap() {
        VpinCalculator calc = new VpinCalculator(100, 3); // history size = 3
        Instant now = Instant.now();

        // Fill 10 buckets (more than history size)
        for (int i = 0; i < 10; i++) {
            calc.onTrade("TEST", new MarketTradeTick("FIGI", now, 100.0, 100, "BUY"));
        }

        assertEquals(3, calc.getCompletedBucketCount("TEST"), "Bucket count should cap at history size");
    }

    public static void testVpinPartialBucket() {
        VpinCalculator calc = new VpinCalculator(100, 5);
        Instant now = Instant.now();

        // Add partial volume (less than bucket size)
        calc.onTrade("TEST", new MarketTradeTick("FIGI", now, 100.0, 50, "BUY"));

        double vpin = calc.getVpin("TEST");
        assertEquals(0.0, vpin, 0.001, "Partial bucket should not complete, VPIN stays 0");
        assertEquals(0, calc.getCompletedBucketCount("TEST"), "No completed buckets yet");
    }

    public static void testVpinReset() {
        VpinCalculator calc = new VpinCalculator(100, 5);
        Instant now = Instant.now();

        // Fill some buckets
        for (int i = 0; i < 3; i++) {
            calc.onTrade("TEST", new MarketTradeTick("FIGI", now, 100.0, 100, "BUY"));
        }

        assertTrue(calc.getCompletedBucketCount("TEST") > 0, "Should have buckets before reset");

        calc.reset("TEST");

        assertEquals(0, calc.getCompletedBucketCount("TEST"), "Reset should clear buckets");
        assertEquals(0.0, calc.getVpin("TEST"), 0.001, "Reset should clear VPIN");
    }

    public static void testVpinUnknownTicker() {
        VpinCalculator calc = new VpinCalculator(100, 5);
        assertEquals(0.0, calc.getVpin("UNKNOWN"), 0.001, "Unknown ticker should return 0.0");
        assertEquals(0, calc.getCompletedBucketCount("UNKNOWN"), "Unknown ticker should have 0 buckets");
    }

    public static void testVpinMixedFlow() {
        VpinCalculator calc = new VpinCalculator(100, 5);
        Instant now = Instant.now();

        // Fill 5 buckets with mixed flow: 70 buy + 30 sell = imbalance 0.4
        for (int i = 0; i < 5; i++) {
            calc.onTrade("TEST", new MarketTradeTick("FIGI", now, 100.0, 70, "BUY"));
            calc.onTrade("TEST", new MarketTradeTick("FIGI", now, 100.0, 30, "SELL"));
        }

        double vpin = calc.getVpin("TEST");
        // Each bucket: |70-30|/100 = 0.4
        assertEquals(0.4, vpin, 0.001, "Mixed flow should have VPIN = 0.4");
    }

    // =========================================================================
    // MAIN
    // =========================================================================

    public static void main(String[] args) {
        int passed = 0;
        int failed = 0;

        try {
            testVpinColdStart();
            passed++;
            printSuccess("testVpinColdStart");
        } catch (Exception e) {
            printFail("testVpinColdStart", e);
            failed++;
        }

        try {
            testVpinBalancedFlow();
            passed++;
            printSuccess("testVpinBalancedFlow");
        } catch (Exception e) {
            printFail("testVpinBalancedFlow", e);
            failed++;
        }

        try {
            testVpinOneSidedBuyFlow();
            passed++;
            printSuccess("testVpinOneSidedBuyFlow");
        } catch (Exception e) {
            printFail("testVpinOneSidedBuyFlow", e);
            failed++;
        }

        try {
            testVpinOneSidedSellFlow();
            passed++;
            printSuccess("testVpinOneSidedSellFlow");
        } catch (Exception e) {
            printFail("testVpinOneSidedSellFlow", e);
            failed++;
        }

        try {
            testVpinBucketHistoryCap();
            passed++;
            printSuccess("testVpinBucketHistoryCap");
        } catch (Exception e) {
            printFail("testVpinBucketHistoryCap", e);
            failed++;
        }

        try {
            testVpinPartialBucket();
            passed++;
            printSuccess("testVpinPartialBucket");
        } catch (Exception e) {
            printFail("testVpinPartialBucket", e);
            failed++;
        }

        try {
            testVpinReset();
            passed++;
            printSuccess("testVpinReset");
        } catch (Exception e) {
            printFail("testVpinReset", e);
            failed++;
        }

        try {
            testVpinUnknownTicker();
            passed++;
            printSuccess("testVpinUnknownTicker");
        } catch (Exception e) {
            printFail("testVpinUnknownTicker", e);
            failed++;
        }

        try {
            testVpinMixedFlow();
            passed++;
            printSuccess("testVpinMixedFlow");
        } catch (Exception e) {
            printFail("testVpinMixedFlow", e);
            failed++;
        }

        // Summary
        System.out.println("\n=============================");
        System.out.println("  Total: " + (passed + failed) + " tests");
        System.out.println("  Passed: " + passed);
        System.out.println("  Failed: " + failed);
        System.out.println("=============================");

        System.exit(failed > 0 ? 1 : 0);
    }
}
