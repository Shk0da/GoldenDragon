package com.github.shk0da.goldendragon.strategy.orderbook;

import java.util.Arrays;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Tests for {@link SlippageModel} predictive order-book slippage estimation.
 */
class SlippageModelTest {

    private static final double DELTA = 0.01;

    @Test
    void testSmallOrderFilledAtBestLevel() {
        SlippageModel model = new SlippageModel();
        // 10 contracts, 50 available at best level
        double slippage = model.calculateSlippage(10, Arrays.asList(50, 100));
        assertEquals(0.0, slippage, DELTA);
    }

    @Test
    void testOrderConsumingTwoLevels() {
        SlippageModel model = new SlippageModel();
        // 100 contracts: 50 at best, 50 at next → fills across 2 levels → 1 tick slippage
        double slippage = model.calculateSlippage(100, Arrays.asList(50, 50));
        assertEquals(1.0, slippage, DELTA);
    }

    @Test
    void testOrderExceedingAllLevels() {
        SlippageModel model = new SlippageModel();
        // 500 contracts: only 3 levels of 50 → walks all 3 levels = 2 ticks slippage
        double slippage = model.calculateSlippage(500, Arrays.asList(50, 50, 50));
        assertEquals(2.0, slippage, DELTA);
    }

    @Test
    void testExactFillAtBoundary() {
        SlippageModel model = new SlippageModel();
        // Exactly fills best level → 0 slippage
        double slippage = model.calculateSlippage(50, Arrays.asList(50, 100));
        assertEquals(0.0, slippage, DELTA);
    }

    @Test
    void testZeroOrderSize() {
        SlippageModel model = new SlippageModel();
        assertEquals(0.0, model.calculateSlippage(0, Arrays.asList(50, 100)), DELTA);
    }

    @Test
    void testNegativeOrderSize() {
        SlippageModel model = new SlippageModel();
        assertEquals(0.0, model.calculateSlippage(-5, Arrays.asList(50)), DELTA);
    }

    @Test
    void testNullDepth() {
        SlippageModel model = new SlippageModel();
        assertEquals(0.0, model.calculateSlippage(10, null), DELTA);
    }

    @Test
    void testEmptyDepth() {
        SlippageModel model = new SlippageModel();
        assertEquals(0.0, model.calculateSlippage(10, Arrays.asList()), DELTA);
    }

    @Test
    void testMaxLevelsLimitsSlippageEstimate() {
        SlippageModel model = new SlippageModel(3);
        // 1000 contracts, but only 3 levels considered → cap at 2 ticks
        double slippage = model.calculateSlippage(1000, Arrays.asList(10, 10, 10, 10, 10));
        assertEquals(2.0, slippage, DELTA);
    }

    @Test
    void testIsFillableWithinLimit() {
        SlippageModel model = new SlippageModel();
        // 10 contracts fits at best level (0 ticks) → OK with 1 tick limit
        assertTrue(model.isFillable(10, Arrays.asList(50), 1.0));
        // 100 contracts needs 2 levels (1 tick) → OK with 1 tick limit
        assertTrue(model.isFillable(100, Arrays.asList(50, 50), 1.0));
        // 500 contracts needs all levels (2 ticks) → NOT OK with 1 tick limit
        assertFalse(model.isFillable(500, Arrays.asList(50, 50, 50), 1.0));
    }

    @Test
    void testConstructorRejectsInvalidMaxLevels() {
        assertThrows(IllegalArgumentException.class, () -> new SlippageModel(0));
        assertThrows(IllegalArgumentException.class, () -> new SlippageModel(-5));
    }
}