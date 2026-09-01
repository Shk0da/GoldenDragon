package com.github.shk0da.goldendragon.strategy.orderbook;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Tests for {@link DailyLossLimit} circuit breaker.
 */
class DailyLossLimitTest {

    @Test
    void testTradingAllowedWhenWithinLimit() {
        DailyLossLimit limit = new DailyLossLimit(-500.0);

        assertTrue(limit.canTrade());
        assertTrue(limit.addPnl(-200.0));
        assertTrue(limit.addPnl(-200.0));
        assertEquals(-400.0, limit.getCumulativePnl(), 0.01);
    }

    @Test
    void testTradingStopsAfterLimitHit() {
        DailyLossLimit limit = new DailyLossLimit(-500.0);

        limit.addPnl(-300.0);
        assertTrue(limit.canTrade());

        limit.addPnl(-250.0); // Cumulative: -550
        assertFalse(limit.canTrade());
        assertEquals(-550.0, limit.getCumulativePnl(), 0.01);
    }

    @Test
    void testWinsOffsetLosses() {
        DailyLossLimit limit = new DailyLossLimit(-500.0);

        limit.addPnl(-300.0);
        limit.addPnl(150.0); // Cumulative: -150
        assertTrue(limit.canTrade());
    }

    @Test
    void testConstructorRejectsPositiveLimit() {
        assertThrows(IllegalArgumentException.class, () -> new DailyLossLimit(500.0));
        assertThrows(IllegalArgumentException.class, () -> new DailyLossLimit(0.0));
    }

    @Test
    void testStopTradingStopsTrading() {
        DailyLossLimit limit = new DailyLossLimit(-500.0);
        assertTrue(limit.canTrade());

        limit.stopTrading();
        assertFalse(limit.canTrade());
    }

    @Test
    void testCumulativePnlStartsAtZero() {
        DailyLossLimit limit = new DailyLossLimit(-500.0);
        assertEquals(0.0, limit.getCumulativePnl(), 0.01);
    }

    @Test
    void testGetDailyLimit() {
        DailyLossLimit limit = new DailyLossLimit(-1000.0);
        assertEquals(-1000.0, limit.getDailyLimit());
    }
}