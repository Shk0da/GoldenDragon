package com.github.shk0da.goldendragon.repository;

import com.github.shk0da.goldendragon.model.Candle;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Tests for CandleRepository - in-memory candle cache.
 *
 * <p>Each test uses a unique ticker symbol so tests remain isolated because CandleRepository
 * is a process-wide singleton and has no reset method.
 */
class CandleRepositoryTest {

    private static final AtomicInteger TICKER_SEQ = new AtomicInteger();

    private String uniqueTicker() {
        return "T" + TICKER_SEQ.incrementAndGet();
    }

    private CandleRepository repository() {
        return CandleRepository.getInstance();
    }

    @Test
    @DisplayName("Put and get candles for single ticker")
    void testPutAndGetCandles() {
        String ticker = uniqueTicker();
        CandleRepository repository = repository();
        List<Candle> candles = createCandles("10:00", "10:05", "10:10");

        repository.putCandles(ticker, "HOUR", candles);

        List<Candle> retrieved = repository.getCandles(ticker, "HOUR");
        assertEquals(3, retrieved.size());
        assertEquals("10:00", retrieved.get(0).time);
        assertEquals("10:10", retrieved.get(2).time);
    }

    @Test
    @DisplayName("Put null candles - no exception")
    void testPutNullCandles() {
        String ticker = uniqueTicker();
        CandleRepository repository = repository();
        assertDoesNotThrow(() -> repository.putCandles(ticker, "HOUR", null));
        assertTrue(repository.getCandles(ticker, "HOUR").isEmpty());
    }

    @Test
    @DisplayName("Put empty candles - no exception")
    void testPutEmptyCandles() {
        String ticker = uniqueTicker();
        CandleRepository repository = repository();
        assertDoesNotThrow(() -> repository.putCandles(ticker, "HOUR", Collections.emptyList()));
        assertTrue(repository.getCandles(ticker, "HOUR").isEmpty());
    }

    @Test
    @DisplayName("Get candles for non-existent ticker - empty list")
    void testGetNonExistentCandles() {
        CandleRepository repository = repository();
        List<Candle> retrieved = repository.getCandles("UNKNOWN", "HOUR");
        assertTrue(retrieved.isEmpty());
    }

    @Test
    @DisplayName("Merge candles - no duplicates")
    void testMergeCandlesNoDuplicates() {
        String ticker = uniqueTicker();
        CandleRepository repository = repository();
        // First batch
        repository.putCandles(ticker, "HOUR", createCandles("10:00", "10:05", "10:10"));

        // Second batch with overlap
        repository.putCandles(ticker, "HOUR", createCandles("10:10", "10:15", "10:20"));

        List<Candle> merged = repository.getCandles(ticker, "HOUR");
        assertEquals(5, merged.size(), "Should merge without duplicates");
        assertEquals("10:00", merged.get(0).time);
        assertEquals("10:20", merged.get(4).time);
    }

    @Test
    @DisplayName("Merge candles - new values overwrite duplicates")
    void testMergeCandlesOverwriteDuplicates() {
        String ticker = uniqueTicker();
        CandleRepository repository = repository();
        // First batch
        repository.putCandles(ticker, "HOUR", List.of(
            new Candle("10:00", 100.0, 105.0, 95.0, 102.0, 1000)
        ));

        // Second batch with same time but different values
        repository.putCandles(ticker, "HOUR", List.of(
            new Candle("10:00", 200.0, 210.0, 190.0, 205.0, 2000)
        ));

        List<Candle> merged = repository.getCandles(ticker, "HOUR");
        assertEquals(1, merged.size());
        assertEquals(200.0, merged.get(0).open, "New value should overwrite");
        assertEquals(2000, merged.get(0).volume, "New value should overwrite");
    }

    @Test
    @DisplayName("Get last N candles")
    void testGetLastCandles() {
        String ticker = uniqueTicker();
        CandleRepository repository = repository();
        repository.putCandles(ticker, "5_MIN", createCandles("10:00", "10:05", "10:10", "10:15", "10:20"));

        List<Candle> last2 = repository.getLastCandles(ticker, "5_MIN", 2);
        assertEquals(2, last2.size());
        assertEquals("10:15", last2.get(0).time);
        assertEquals("10:20", last2.get(1).time);

        List<Candle> last10 = repository.getLastCandles(ticker, "5_MIN", 10);
        assertEquals(5, last10.size(), "Should return all if count > size");
    }

    @Test
    @DisplayName("Get last N candles - non-existent ticker")
    void testGetLastNonExistentCandles() {
        CandleRepository repository = repository();
        List<Candle> last = repository.getLastCandles("UNKNOWN", "HOUR", 5);
        assertTrue(last.isEmpty());
    }

    @Test
    @DisplayName("Get stats")
    void testGetStats() {
        String ticker1 = uniqueTicker();
        String ticker2 = uniqueTicker();
        CandleRepository repository = repository();
        repository.putCandles(ticker1, "HOUR", createCandles("10:00", "10:05"));
        repository.putCandles(ticker1, "5_MIN", createCandles("10:00", "10:05", "10:10"));
        repository.putCandles(ticker2, "HOUR", createCandles("10:00"));

        Map<String, Integer> stats = repository.getStats();

        assertEquals(5, stats.get(ticker1)); // 2 + 3 candles
        assertEquals(1, stats.get(ticker2));
    }

    @Test
    @DisplayName("Multiple intervals for same ticker")
    void testMultipleIntervals() {
        String ticker = uniqueTicker();
        CandleRepository repository = repository();
        repository.putCandles(ticker, "HOUR", createCandles("10:00", "11:00"));
        repository.putCandles(ticker, "5_MIN", createCandles("10:00", "10:05"));

        List<Candle> hour = repository.getCandles(ticker, "HOUR");
        List<Candle> min = repository.getCandles(ticker, "5_MIN");

        assertEquals(2, hour.size());
        assertEquals(2, min.size());
        assertNotSame(hour, min);
    }

    @Test
    @DisplayName("Chronological order preserved after merge")
    void testChronologicalOrderPreserved() {
        String ticker = uniqueTicker();
        CandleRepository repository = repository();
        // Add candles out of order
        repository.putCandles(ticker, "HOUR", createCandles("10:00", "10:05"));
        repository.putCandles(ticker, "HOUR", createCandles("09:55", "10:10"));

        List<Candle> merged = repository.getCandles(ticker, "HOUR");
        assertEquals(4, merged.size());
        // Order should be preserved as inserted (LinkedHashMap)
        assertEquals("10:00", merged.get(0).time);
        assertEquals("10:10", merged.get(3).time);
    }

    // Helper methods

    private List<Candle> createCandles(String... times) {
        return java.util.Arrays.stream(times)
            .map(time -> new Candle(time, 100.0, 105.0, 95.0, 102.0, 1000))
            .collect(java.util.stream.Collectors.toList());
    }
}