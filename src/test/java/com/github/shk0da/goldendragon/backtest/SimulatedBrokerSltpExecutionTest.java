package com.github.shk0da.goldendragon.backtest;

import com.github.shk0da.goldendragon.model.Candle;
import org.junit.jupiter.api.Test;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Tests SL/TP execution at price level (not bar.close).
 */
class SimulatedBrokerSltpExecutionTest {

    private static final DateTimeFormatter DATE_TIME_FMT =
            DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm:ss");

    @Test
    void testStopLossExecutedAtSlPrice_NotBarClose() {
        // Given: long position with SL at 99.0
        SimulatedBroker broker = new SimulatedBroker(100_000, 0.0005, 0.001);
        Candle candle = new Candle("18.09.2026 10:00:00", 100.0, 101.0, 98.0, 100.5, 1000);
        broker.loadCandles("TEST", "5_MIN", Collections.singletonList(candle));
        broker.setCurrentTime(LocalDateTime.parse("18.09.2026 10:00:00", DATE_TIME_FMT));

        // Open long position at 100.0 with SL=99.0, TP=102.0
        broker.buy("TEST", 100, 99.0, 102.0);

        // When: SL is hit (low=98.0 <= 99.0)
        var result = broker.checkStopLossTakeProfit("TEST", candle);

        // Then: position closed at SL price (99.0), NOT bar.close (100.5)
        assertNotNull(result);
        assertTrue(result.isSuccess());
        var trades = broker.getTradeHistory();
        assertEquals(2, trades.size());
        assertEquals(99.0, trades.get(trades.size() - 1).exitPrice, 0.001);
    }

    @Test
    void testTakeProfitExecutedAtTpPrice_NotBarClose() {
        // Given: long position with TP at 102.0
        SimulatedBroker broker = new SimulatedBroker(100_000, 0.0005, 0.001);
        Candle candle = new Candle("18.09.2026 10:00:00", 100.0, 102.5, 99.5, 101.0, 1000);
        broker.loadCandles("TEST", "5_MIN", Collections.singletonList(candle));
        broker.setCurrentTime(LocalDateTime.parse("18.09.2026 10:00:00", DATE_TIME_FMT));

        // Open long position at 100.0 with SL=98.0, TP=102.0
        broker.buy("TEST", 100, 98.0, 102.0);

        // When: TP is hit (high=102.5 >= 102.0)
        var result = broker.checkStopLossTakeProfit("TEST", candle);

        // Then: position closed at TP price (102.0), NOT bar.close (101.0)
        assertNotNull(result);
        assertTrue(result.isSuccess());
        var trades = broker.getTradeHistory();
        assertEquals(2, trades.size());
        assertEquals(102.0, trades.get(trades.size() - 1).exitPrice, 0.001);
    }

    @Test
    void testSlHitTakesPrecedenceOverTpOnSameBar() {
        // Given: long position where BOTH SL and TP are hit on the same bar
        SimulatedBroker broker = new SimulatedBroker(100_000, 0.0005, 0.001);
        // Bar that hits both: low=97.0 (hits SL=98.0), high=103.0 (hits TP=102.0)
        Candle candle = new Candle("18.09.2026 10:00:00", 100.0, 103.0, 97.0, 100.5, 1000);
        broker.loadCandles("TEST", "5_MIN", Collections.singletonList(candle));
        broker.setCurrentTime(LocalDateTime.parse("18.09.2026 10:00:00", DATE_TIME_FMT));

        broker.buy("TEST", 100, 98.0, 102.0);

        // When: both SL and TP are hit
        var result = broker.checkStopLossTakeProfit("TEST", candle);

        // Then: SL takes precedence, closed at SL price (98.0)
        assertNotNull(result);
        assertTrue(result.isSuccess());
        var trades = broker.getTradeHistory();
        assertEquals(2, trades.size());
        assertEquals(98.0, trades.get(trades.size() - 1).exitPrice, 0.001);
    }
}
