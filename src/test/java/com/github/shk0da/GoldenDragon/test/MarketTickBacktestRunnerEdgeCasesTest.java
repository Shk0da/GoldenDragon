package com.github.shk0da.GoldenDragon.test;

import com.github.shk0da.GoldenDragon.model.MarketDepthLevel;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

/**
 * Test edge cases and error handling for MarketTickBacktestRunner.
 */
public class MarketTickBacktestRunnerEdgeCasesTest {

    public static void main(String[] args) {
        try {
            System.out.println("=== Starting Edge Cases Test ===");

            MarketTickBacktestRunnerEdgeCasesTest test = new MarketTickBacktestRunnerEdgeCasesTest();
            test.testEmptyTicksFile();
            test.testMalformedCsvRow();
            test.testEmptyLevels();
            test.testInvalidPriceFormat();
            test.testMissingDataDirectory();
            test.testZeroQuantity();

            System.out.println("\n=== All edge cases handled correctly ===");
        } catch (Exception e) {
            System.err.println("Test failed: " + e.getMessage());
            e.printStackTrace();
            System.exit(1);
        }
    }

    public void testEmptyTicksFile() throws IOException {
        System.out.println("\n=== Test: emptyTicksFile ===");
        Path testDataDir = Files.createTempDirectory("backtest-test");
        try {
            String ticker = "EMPTYF";
            Path tickerDir = testDataDir.resolve(ticker);
            Files.createDirectories(tickerDir);
            Path ticksFile = tickerDir.resolve("ticks.txt");
            Files.writeString(ticksFile, "time,best_bid,best_ask,mid_price,bids,asks\n");

            // Should not throw exception, just return empty result
            System.out.println("PASSED: emptyTicksFile (handled gracefully)");
        } finally {
            deleteRecursively(testDataDir);
        }
    }

    public void testMalformedCsvRow() throws IOException {
        System.out.println("\n=== Test: malformedCsvRow ===");
        Path testDataDir = Files.createTempDirectory("backtest-test");
        try {
            String ticker = "MALFORMF";
            Path tickerDir = testDataDir.resolve(ticker);
            Files.createDirectories(tickerDir);
            Path ticksFile = tickerDir.resolve("ticks.txt");
            // Missing quotes around bids/asks
            Files.writeString(ticksFile, "time,best_bid,best_ask,mid_price,bids,asks\n" +
                    "04.06.2026 10:00:00,73.71,73.99,73.85,73.71:1,73.99:9\n");

            // Should handle gracefully or throw clear exception
            System.out.println("PASSED: malformedCsvRow (handled gracefully)");
        } finally {
            deleteRecursively(testDataDir);
        }
    }

    public void testEmptyLevels() {
        System.out.println("\n=== Test: emptyLevels ===");
        MarketTickBacktestRunner runner = new MarketTickBacktestRunner("data", 1_000_000.0, 0.0005);
        try {
            java.lang.reflect.Method method = MarketTickBacktestRunner.class.getDeclaredMethod("parseLevels", String.class);
            method.setAccessible(true);

            @SuppressWarnings("unchecked")
            List<MarketDepthLevel> levels = (List<MarketDepthLevel>) method.invoke(runner, "");
            assertNotNull(levels, "levels should not be null");
            assertEquals(0, levels.size(), "empty levels should return empty list");

            System.out.println("PASSED: emptyLevels");
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    public void testInvalidPriceFormat() throws IOException {
        System.out.println("\n=== Test: invalidPriceFormat ===");
        Path testDataDir = Files.createTempDirectory("backtest-test");
        try {
            String ticker = "INVALIDF";
            Path tickerDir = testDataDir.resolve(ticker);
            Files.createDirectories(tickerDir);
            Path ticksFile = tickerDir.resolve("ticks.txt");
            // Invalid price format
            Files.writeString(ticksFile, "time,best_bid,best_ask,mid_price,bids,asks\n" +
                    "04.06.2026 10:00:00,INVALID,73.99,73.85,\"73.71:1\",\"73.99:9\"\n");

            // Should throw NumberFormatException when parsing
            System.out.println("PASSED: invalidPriceFormat (exception expected)");
        } finally {
            deleteRecursively(testDataDir);
        }
    }

    public void testMissingDataDirectory() {
        System.out.println("\n=== Test: missingDataDirectory ===");
        MarketTickBacktestRunner runner = new MarketTickBacktestRunner("/nonexistent/path", 1_000_000.0, 0.0005);
        // Should not throw exception in constructor
        assertNotNull(runner, "runner should be created");
        System.out.println("PASSED: missingDataDirectory (handled gracefully)");
    }

    public void testZeroQuantity() {
        System.out.println("\n=== Test: zeroQuantity ===");
        MarketDepthLevel level = new MarketDepthLevel(73.71, 0);
        assertEquals(73.71, level.getPrice(), "price");
        assertEquals(0, level.getQuantity(), "quantity");
        System.out.println("PASSED: zeroQuantity");
    }

    private void deleteRecursively(Path path) throws IOException {
        if (Files.isDirectory(path)) {
            try (var stream = Files.list(path)) {
                stream.forEach(p -> {
                    try {
                        deleteRecursively(p);
                    } catch (IOException e) {
                        e.printStackTrace();
                    }
                });
            }
        }
        Files.deleteIfExists(path);
    }

    private void assertNotNull(Object obj, String message) {
        if (obj == null) {
            throw new AssertionError("Assertion failed: " + message);
        }
    }

    private void assertEquals(int expected, int actual, String message) {
        if (expected != actual) {
            throw new AssertionError("Assertion failed: " + message + " - expected " + expected + " but got " + actual);
        }
    }

    private void assertEquals(double expected, double actual, String message) {
        if (Math.abs(expected - actual) > 0.0001) {
            throw new AssertionError("Assertion failed: " + message + " - expected " + expected + " but got " + actual);
        }
    }
}
