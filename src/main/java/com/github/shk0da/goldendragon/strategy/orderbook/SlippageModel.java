package com.github.shk0da.goldendragon.strategy.orderbook;

import java.util.List;

/**
 * Estimates expected slippage for a planned order based on current order book depth.
 *
 * <p>Uses the available volume at each price level to estimate how far an order would
 * walk the book before being fully filled. This is a <em>predictive</em> model used
 * at decision time (before placing the order), complementing {@link SlippageTracker}
 * which records <em>actual</em> slippage after execution.
 *
 * <p>Slippage is returned in <em>ticks</em> (price levels consumed beyond the best level).
 */
public final class SlippageModel {

    private static final int DEFAULT_MAX_LEVELS = 5;

    private final int maxLevels;

    public SlippageModel() {
        this(DEFAULT_MAX_LEVELS);
    }

    public SlippageModel(int maxLevels) {
        if (maxLevels <= 0) {
            throw new IllegalArgumentException("maxLevels must be positive, got " + maxLevels);
        }
        this.maxLevels = maxLevels;
    }

    /**
     * Estimates slippage in ticks for an order of the given size.
     *
     * @param orderSize order size in contracts
     * @param orderBookDepth volume available at successive price levels (best, next, ...),
     *                       ordered from best to worst for the given side
     * @return expected slippage in ticks (0 = filled at best level, 1 = one level worse, ...);
     *         if the order size exceeds available depth, returns number of levels consumed
     */
    public double calculateSlippage(int orderSize, List<Integer> orderBookDepth) {
        if (orderSize <= 0) {
            return 0.0;
        }
        if (orderBookDepth == null || orderBookDepth.isEmpty()) {
            return 0.0;
        }

        int remaining = orderSize;
        int levelsConsumed = 0;

        for (int i = 0; i < Math.min(orderBookDepth.size(), maxLevels); i++) {
            int depth = Math.max(0, orderBookDepth.get(i));
            remaining -= depth;
            levelsConsumed++;
            if (remaining <= 0) {
                break;
            }
        }

        // Slippage = levels consumed minus the first (best) level, which has no slippage
        return Math.max(0.0, levelsConsumed - 1);
    }

    /**
     * Whether an order of the given size can be filled within the available depth
     * without excessive slippage.
     *
     * @param orderSize order size in contracts
     * @param orderBookDepth volume available at successive price levels
     * @param maxAllowedSlippageTicks maximum acceptable slippage in ticks
     * @return true if expected slippage does not exceed the limit
     */
    public boolean isFillable(int orderSize, List<Integer> orderBookDepth, double maxAllowedSlippageTicks) {
        double slippage = calculateSlippage(orderSize, orderBookDepth);
        return slippage <= maxAllowedSlippageTicks;
    }
}