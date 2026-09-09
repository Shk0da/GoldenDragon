package com.github.shk0da.goldendragon.repository;

import com.github.shk0da.goldendragon.model.Candle;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Tests for CandleRepository - in-memory candle cache.
 */
class CandleRepositoryTest {

    private CandleRepository repository;

    @BeforeEach
    void setUp() {
        repository = CandleRepository.getInstance();
        repository.clear(); // Clean state before each test
    }

    @AfterEach
    void tearDown() {
        repository.clear(); // Clean up after each test
    }

    @Test
    @DisplayName("Put and get candles for single ticker")
    void testPutAndGetCandles() {
        List<Candle> candles = createCandles("10:00", "10:05", "10:10");

        repository.putCandles("T", "HOUR", candles);

        List<Candle> retrieved = repository.getCandles("T", "HOUR");
        assertEquals(3, retrieved.size());
        assertEquals("10:00", retrieved.get(0).time);
        assertEquals("10:10", retrieved.get(2).time);
    }

    @Test
    @DisplayName("Put null candles - no exception")
    void testPutNullCandles() {
        assertDoesNotThrow(() -> repository.putCandles("T", "HOUR", null));
        assertTrue(repository.getCandles("T", "HOUR").isEmpty());
    }

    @Test
    @DisplayName("Put empty candles - no exception")
    void testPutEmptyCandles() {
        assertDoesNotThrow(() -> repository.putCandles("T", "HOUR", Collections.emptyList()));
        assertTrue(repository.getCandles("T", "HOUR").isEmpty());
    }

    @Test
    @DisplayName("Get candles for non-existent ticker - empty list")
    void testGetNonExistentCandles() {
        List<Candle> retrieved = repository.getCandles("UNKNOWN", "HOUR");
        assertTrue(retrieved.isEmpty());
    }

    @Test
    @DisplayName("Merge candles - no duplicates")
    void testMergeCandlesNoDuplicates() {
        // First batch
        repository.putCandles("T", "HOUR", createCandles("10:00", "10:05", "10:10"));

        // Second batch with overlap
        repository.putCandles("T", "HOUR", createCandles("10:10", "10:15", "10:20"));

        List<Candle> merged = repository.getCandles("T", "HOUR");
        assertEquals(5, merged.size(), "Should merge without duplicates");
        assertEquals("10:00", merged.get(0).time);
        assertEquals("10:20", merged.get(4).time);
    }

    @Test
    @DisplayName("Merge candles - new values overwrite duplicates")
    void testMergeCandlesOverwriteDuplicates() {
        // First batch
        repository.putCandles("T", "HOUR", List.of(
            new Candle("10:00", 100.0, 105.0, 95.0, 102.0, 1000)
        ));

        // Second batch with same time but different values
        repository.putCandles("T", "HOUR", List.of(
            new Candle("10:00", 200.0, 210.0, 190.0, 205.0, 2000)
        ));

        List<Candle> merged = repository.getCandles("T", "HOUR");
        assertEquals(1, merged.size());
        assertEquals(200.0, merged.get(0).open, "New value should overwrite");
        assertEquals(2000, merged.get(0).volume, "New value should overwrite");
    }

    @Test
    @DisplayName("Get last N candles")
    void testGetLastCandles() {
        repository.putCandles("T", "5_MIN", createCandles("10:00", "10:05", "10:10", "10:15", "10:20"));

        List<Candle> last2 = repository.getLastCandles("T", "5_MIN", 2);
        assertEquals(2, last2.size());
        assertEquals("10:15", last2.get(0).time);
        assertEquals("10:20", last2.get(1).time);

        List<Candle> last10 = repository.getLastCandles("T", "5_MIN", 10);
        assertEquals(5, last10.size(), "Should return all if count > size");
    }

    @Test
    @DisplayName("Get last N candles - non-existent ticker")
    void testGetLastNonExistentCandles() {
        List<Candle> last = repository.getLastCandles("UNKNOWN", "HOUR", 5);
        assertTrue(last.isEmpty());
    }

    @Test
    @DisplayName("Has candles - true")
    void testHasCandles() {
        repository.putCandles("T", "HOUR", createCandles("10:00"));
        assertTrue(repository.hasCandles("T", "HOUR"));
    }

    @Test
    @DisplayName("Has candles - false")
    void testHasCandlesFalse() {
        assertFalse(repository.hasCandles("T", "HOUR"));
        repository.putCandles("T", "5_MIN", createCandles("10:00"));
        assertFalse(repository.hasCandles("T", "HOUR"), "Different interval should return false");
    }

    @Test
    @DisplayName("Get latest candle")
    void testGetLatestCandle() {
        repository.putCandles("T", "HOUR", createCandles("10:00", "10:05", "10:10"));

        Candle latest = repository.getLatestCandle("T", "HOUR");
        assertNotNull(latest);
        assertEquals("10:10", latest.time);
    }

    @Test
    @DisplayName("Get latest candle - non-existent")
    void testGetLatestNonExistent() {
        Candle latest = repository.getLatestCandle("UNKNOWN", "HOUR");
        assertNull(latest);
    }

    @Test
    @DisplayName("Remove ticker")
    void testRemoveTicker() {
        repository.putCandles("T1", "HOUR", createCandles("10:00"));
        repository.putCandles("T2", "HOUR", createCandles("10:00"));

        repository.removeTicker("T1");

        assertTrue(repository.getCandles("T1", "HOUR").isEmpty());
        assertFalse(repository.getCandles("T2", "HOUR").isEmpty());
    }

    @Test
    @DisplayName("Clear all candles")
    void testClear() {
        repository.putCandles("T1", "HOUR", createCandles("10:00"));
        repository.putCandles("T2", "5_MIN", createCandles("10:00"));

        repository.clear();

        assertTrue(repository.getCandles("T1", "HOUR").isEmpty());
        assertTrue(repository.getCandles("T2", "5_MIN").isEmpty());
    }

    @Test
    @DisplayName("Get stats")
    void testGetStats() {
        repository.putCandles("T1", "HOUR", createCandles("10:00", "10:05"));
        repository.putCandles("T1", "5_MIN", createCandles("10:00", "10:05", "10:10"));
        repository.putCandles("T2", "HOUR", createCandles("10:00"));

        Map<String, Integer> stats = repository.getStats();

        assertEquals(2, stats.size());
        assertEquals(5, stats.get("T1")); // 2 + 3 candles
        assertEquals(1, stats.get("T2"));
    }

    @Test
    @DisplayName("Multiple intervals for same ticker")
    void testMultipleIntervals() {
        repository.putCandles("T", "HOUR", createCandles("10:00", "11:00"));
        repository.putCandles("T", "5_MIN", createCandles("10:00", "10:05"));

        List<Candle> hour = repository.getCandles("T", "HOUR");
        List<Candle> min = repository.getCandles("T", "5_MIN");

        assertEquals(2, hour.size());
        assertEquals(2, min.size());
        assertNotSame(hour, min);
    }

    @Test
    @DisplayName("Chronological order preserved after merge")
    void testChronologicalOrderPreserved() {
        // Add candles out of order
        repository.putCandles("T", "HOUR", createCandles("10:00", "10:05"));
        repository.putCandles("T", "HOUR", createCandles("09:55", "10:10"));

        List<Candle> merged = repository.getCandles("T", "HOUR");
        assertEquals(4, merged.size());
        // Order should be preserved as inserted (LinkedHashMap)
        assertEquals("10:00", merged.get(0).time);
        assertEquals("10:10", merged.get(3).time);
    }

    // Helper methods

    private List<Candle> createCandles(String... times) {
        return java.util.Arrays.stream(times)
            .map(time -> new Candle(time, 100.0, 105.0, 95.0, 102.0, 1000))
            .toList();
    }
}
