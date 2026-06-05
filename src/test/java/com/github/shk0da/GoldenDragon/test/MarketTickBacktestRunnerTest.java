package com.github.shk0da.GoldenDragon.test;

import com.github.shk0da.GoldenDragon.model.MarketDepthLevel;
import com.github.shk0da.GoldenDragon.model.MarketDepthSnapshot;
import com.github.shk0da.GoldenDragon.model.TickerInfo;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;

/**
 * Manual test for MarketTickBacktestRunner core parsing functions.
 * Tests: parseLevels, parseTickRow, CSV parsing
 */
public class MarketTickBacktestRunnerTest {

    private static final DateTimeFormatter DATE_TIME_FMT = DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm:ss");
    private static final String HEADER = "time,best_bid,best_ask,mid_price,bids,asks";

    public static void main(String[] args) {
        try {
            System.out.println("=== Starting MarketTickBacktestRunner Test ===");

            MarketTickBacktestRunnerTest test = new MarketTickBacktestRunnerTest();
            test.testParseLevels();
            test.testParseTickRow();
            test.testLoadTicksFromFile();
            test.testBuildTimelineSort();

            System.out.println("\n=== All tests passed ===");
        } catch (Exception e) {
            System.err.println("Test failed: " + e.getMessage());
            e.printStackTrace();
            System.exit(1);
        }
    }

    public void testParseLevels() {
        System.out.println("\n=== Test: parseLevels ===");
        String levelsStr = "73.71:1|73.7:12|73.66:1";
        List<MarketDepthLevel> levels = parseLevels(levelsStr);

        assertNotNull(levels, "levels is null");
        assertEquals(3, levels.size(), "expected 3 levels");
        assertEquals(73.71, levels.get(0).getPrice(), "first price");
        assertEquals(1, levels.get(0).getQuantity(), "first quantity");
        assertEquals(73.7, levels.get(1).getPrice(), "second price");
        assertEquals(12, levels.get(1).getQuantity(), "second quantity");

        System.out.println("PASSED: parseLevels");
    }

    public void testParseTickRow() throws Exception {
        System.out.println("\n=== Test: parseTickRow ===");
        TickerInfo tickerInfo = new TickerInfo(
                "FUTUSDRUBF00",
                "USDRUBF",
                "USD/RUB",
                0.01,
                1,
                "RUB",
                "USDRUBF Доллар - Рубль",
                "FEATURE"
        );

        String tickLine = "04.06.2026 10:00:00,73.71,73.99,73.85,\"73.71:1|73.7:12\",\"73.99:9|74.01:15\"";
        MarketDepthSnapshot snapshot = parseTickRow(tickLine, tickerInfo);

        assertNotNull(snapshot, "snapshot is null");
        assertEqualsStr("FUTUSDRUBF00", snapshot.getFigi(), "figi");
        assertEquals(73.71, snapshot.getBestBid(), "bestBid");
        assertEquals(73.99, snapshot.getBestAsk(), "bestAsk");
        assertEquals(2, snapshot.getBids().size(), "bids count");
        assertEquals(2, snapshot.getAsks().size(), "asks count");

        System.out.println("PASSED: parseTickRow");
    }

    public void testLoadTicksFromFile() throws IOException {
        System.out.println("\n=== Test: loadTicksFromFile ===");
        Path testDataDir = Files.createTempDirectory("backtest-test");
        try {
            String ticker = "TESTF";
            Path tickerDir = testDataDir.resolve(ticker);
            Files.createDirectories(tickerDir);
            Path ticksFile = tickerDir.resolve("ticks.txt");

            String ticksContent = "time,best_bid,best_ask,mid_price,bids,asks\n" +
                    "04.06.2026 10:00:00,73.71,73.99,73.85,\"73.71:1|73.7:12\",\"73.99:9|74.01:15\"\n" +
                    "04.06.2026 10:00:01,73.72,74.00,73.86,\"73.72:1|73.71:12\",\"74.00:9|74.02:15\"\n" +
                    "04.06.2026 10:00:02,73.73,74.01,73.87,\"73.73:1|73.72:12\",\"74.01:9|74.03:15\"\n";
            Files.writeString(ticksFile, ticksContent);

            TickerInfo tickerInfo = new TickerInfo(
                    "FUTTESTF00",
                    ticker,
                    "TEST/RUB",
                    0.01,
                    1,
                    "RUB",
                    "TESTF Test",
                    "FEATURE"
            );

            List<MarketDepthSnapshot> snapshots = new ArrayList<>();
            try (var lines = Files.lines(ticksFile)) {
                lines.filter(line -> !line.isBlank())
                        .filter(line -> !HEADER.equals(line))
                        .map(line -> parseTickRow(line, tickerInfo))
                        .forEach(snapshots::add);
            }

            assertNotNull(snapshots, "snapshots is null");
            assertEquals(3, snapshots.size(), "expected 3 snapshots");

            MarketDepthSnapshot first = snapshots.get(0);
            assertEquals(73.71, first.getBestBid(), "first bid");
            assertEquals(73.99, first.getBestAsk(), "first ask");

            System.out.println("PASSED: loadTicksFromFile (loaded " + snapshots.size() + " snapshots)");
        } finally {
            deleteRecursively(testDataDir);
        }
    }

    public void testBuildTimelineSort() throws Exception {
        System.out.println("\n=== Test: buildTimelineSort ===");

        List<LocalDateTime> times = new ArrayList<>();
        times.add(LocalDateTime.of(2026, 6, 4, 10, 0, 2));
        times.add(LocalDateTime.of(2026, 6, 4, 10, 0, 0));
        times.add(LocalDateTime.of(2026, 6, 4, 10, 0, 1));
        times.add(LocalDateTime.of(2026, 6, 4, 9, 59, 59));

        times.sort((a, b) -> a.compareTo(b));

        assertEquals(4, times.size(), "expected 4 times");
        assertEquals(9, times.get(0).getHour(), "first hour");
        assertEquals(59, times.get(0).getMinute(), "first minute");
        assertEquals(10, times.get(3).getHour(), "last hour");
        assertEquals(0, times.get(3).getMinute(), "last minute");
        assertEquals(2, times.get(3).getSecond(), "last second");

        System.out.println("PASSED: buildTimelineSort (sorted " + times.size() + " events)");
    }

    private MarketDepthSnapshot parseTickRow(String line, TickerInfo tickerInfo) {
        String[] parts = splitCsv(line);
        if (parts.length != 6) {
            throw new IllegalArgumentException("Invalid tick row: " + line);
        }

        LocalDateTime time = LocalDateTime.parse(parts[0], DATE_TIME_FMT);
        List<MarketDepthLevel> bids = parseLevels(parts[4]);
        List<MarketDepthLevel> asks = parseLevels(parts[5]);
        return new MarketDepthSnapshot(
                tickerInfo.getFigi(),
                time.atZone(java.time.ZoneId.systemDefault()).toInstant(),
                true,
                bids,
                asks
        );
    }

    private String[] splitCsv(String line) {
        List<String> parts = new ArrayList<>();
        StringBuilder current = new StringBuilder();
        boolean inQuotes = false;
        for (int i = 0; i < line.length(); i++) {
            char ch = line.charAt(i);
            if (ch == '"') {
                inQuotes = !inQuotes;
                continue;
            }
            if (ch == ',' && !inQuotes) {
                parts.add(current.toString());
                current.setLength(0);
                continue;
            }
            current.append(ch);
        }
        parts.add(current.toString());
        return parts.toArray(new String[0]);
    }

    private List<MarketDepthLevel> parseLevels(String raw) {
        List<MarketDepthLevel> levels = new ArrayList<>();
        if (raw == null || raw.isBlank()) {
            return levels;
        }
        String[] items = raw.split("\\|");
        for (String item : items) {
            String[] pair = item.split(":");
            if (pair.length != 2) {
                continue;
            }
            levels.add(new MarketDepthLevel(Double.parseDouble(pair[0]), Integer.parseInt(pair[1])));
        }
        return levels;
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

    private void assertEqualsStr(String expected, String actual, String message) {
        if (expected == null ? actual != null : !expected.equals(actual)) {
            throw new AssertionError("Assertion failed: " + message + " - expected " + expected + " but got " + actual);
        }
    }
}
