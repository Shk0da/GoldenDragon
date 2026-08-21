package com.github.shk0da.goldendragon.strategy.orderbook;

import com.github.shk0da.goldendragon.model.MarketDepthLevel;
import com.github.shk0da.goldendragon.model.MarketTradeTick;

import java.util.List;

/** Shared order-book metric helpers for signals and the trading engine. */
public final class OrderBookMath {

    private OrderBookMath() {}

    public static double calculateObi(
            List<MarketDepthLevel> bids, List<MarketDepthLevel> asks, int levels) {
        double bidVol = sumQuantity(bids, levels);
        double askVol = sumQuantity(asks, levels);
        double total = bidVol + askVol;
        if (total <= 0.0) {
            return 0.0;
        }
        return (bidVol - askVol) / total;
    }

    /**
     * Calculates microprice edge relative to mid-price using top-of-book imbalance.
     *
     * <p>The formula intentionally cross-weights each best price by liquidity on the opposite side
     * of the book:
     *
     * <pre>
     * micro = (bestBid * askQty0 + bestAsk * bidQty0) / (bidQty0 + askQty0)
     * edge = micro - mid
     * </pre>
     *
     * <p>This produces a microprice that shifts toward the side more likely to trade next. The
     * returned value is an edge relative to the simple midpoint and is consumed consistently by all
     * order-book signals in this strategy.
     */
    public static double calculateMicroEdge(
            double bestBid, double bestAsk, int bidQty0, int askQty0) {
        double mid = (bestBid + bestAsk) / 2.0;
        int totalTop = bidQty0 + askQty0;
        if (totalTop <= 0 || mid <= 0.0) {
            return 0.0;
        }
        double micro = (bestBid * askQty0 + bestAsk * bidQty0) / totalTop;
        return micro - mid;
    }

    public static double calculateTradeDelta(List<MarketTradeTick> trades) {
        long buyVolume = 0;
        long sellVolume = 0;
        for (MarketTradeTick trade : trades) {
            if (isBuyDirection(trade.getDirection())) {
                buyVolume += trade.getQuantity();
            } else {
                sellVolume += trade.getQuantity();
            }
        }
        return buyVolume - sellVolume;
    }

    public static int topQuantity(List<MarketDepthLevel> levels, int index) {
        if (levels == null || levels.size() <= index) {
            return 0;
        }
        return levels.get(index).getQuantity();
    }

    private static double sumQuantity(List<MarketDepthLevel> levels, int maxLevels) {
        if (levels == null || levels.isEmpty()) {
            return 0.0;
        }
        double sum = 0.0;
        int limit = Math.min(maxLevels, levels.size());
        for (int i = 0; i < limit; i++) {
            sum += levels.get(i).getQuantity();
        }
        return sum;
    }

    /**
     * Calculates weighted depth imbalance where levels closer to mid-price count more.
     *
     * <p>Best practice for scalping: weight by inverse distance from mid so that immediate
     * support/resistance matters more than distant levels.
     *
     * @param bids bid levels sorted best-to-worst
     * @param asks ask levels sorted best-to-worst
     * @param levels number of levels to consider
     * @return imbalance in range [-1.0, 1.0], positive = bid-heavy (support)
     */
    public static double calculateWeightedDepthImbalance(
            List<MarketDepthLevel> bids, List<MarketDepthLevel> asks, int levels) {
        if (bids == null || asks == null || bids.isEmpty() || asks.isEmpty()) {
            return 0.0;
        }
        double weightedBidVol = 0.0;
        double weightedAskVol = 0.0;
        int limit = Math.min(levels, Math.min(bids.size(), asks.size()));
        for (int i = 0; i < limit; i++) {
            double weight = 1.0 / (1.0 + i);
            weightedBidVol += bids.get(i).getQuantity() * weight;
            weightedAskVol += asks.get(i).getQuantity() * weight;
        }
        double total = weightedBidVol + weightedAskVol;
        if (total <= 0.0) {
            return 0.0;
        }
        return (weightedBidVol - weightedAskVol) / total;
    }

    /**
     * Calculates depth gradient: whether liquidity increases or decreases towards mid-price.
     *
     * <p>Positive means bids get thicker deeper in the book (strong support below).
     * Negative means asks get thicker deeper in the book (strong resistance above).
     *
     * @param bids bid levels sorted best-to-worst
     * @param asks ask levels sorted best-to-worst
     * @param levels number of levels to consider
     * @return gradient value, positive = support building, negative = resistance building
     */
    public static double calculateDepthGradient(
            List<MarketDepthLevel> bids, List<MarketDepthLevel> asks, int levels) {
        if (bids == null || asks == null || bids.size() < 2 || asks.size() < 2) {
            return 0.0;
        }
        int limit = Math.min(levels, Math.min(bids.size(), asks.size()));
        if (limit < 2) {
            return 0.0;
        }
        // Bid gradient: volume increasing away from mid (deeper levels have more)
        double bidNear = bids.get(0).getQuantity();
        double bidFar = bids.get(limit - 1).getQuantity();
        double bidGradient = bidNear > 0.0 ? (bidFar - bidNear) / bidNear : 0.0;

        // Ask gradient: volume increasing away from mid (deeper levels have more)
        double askNear = asks.get(0).getQuantity();
        double askFar = asks.get(limit - 1).getQuantity();
        double askGradient = askNear > 0.0 ? (askFar - askNear) / askNear : 0.0;

        return bidGradient - askGradient;
    }

    /**
     * Detects potential absorption: large resting order at a level being consumed.
     *
     * <p>Compares top-of-book quantity ratio. A high ratio (e.g., bid much thicker than ask) may
     * indicate absorption on the bid side (large buyer absorbing sells).
     *
     * @param bids bid levels sorted best-to-worst
     * @param asks ask levels sorted best-to-worst
     * @return absorption score: positive = bid absorption (buying pressure),
     *         negative = ask absorption (selling pressure), 0 = no absorption
     */
    public static double calculateAbsorptionScore(
            List<MarketDepthLevel> bids, List<MarketDepthLevel> asks) {
        if (bids == null || asks == null || bids.isEmpty() || asks.isEmpty()) {
            return 0.0;
        }
        int bidQty0 = bids.get(0).getQuantity();
        int askQty0 = asks.get(0).getQuantity();
        if (bidQty0 <= 0 && askQty0 <= 0) {
            return 0.0;
        }
        // Ratio of imbalances: larger imbalance = stronger absorption
        double total = bidQty0 + askQty0;
        if (total <= 0) {
            return 0.0;
        }
        double imbalance = (double) (bidQty0 - askQty0) / total;

        // Check if there's depth behind the top level (confirmation)
        double bidDepth = 0.0;
        int bidLimit = Math.min(3, bids.size());
        for (int i = 1; i < bidLimit; i++) {
            bidDepth += bids.get(i).getQuantity();
        }
        double askDepth = 0.0;
        int askLimit = Math.min(3, asks.size());
        for (int i = 1; i < askLimit; i++) {
            askDepth += asks.get(i).getQuantity();
        }

        // Absorption is stronger when top level is large AND there's supporting depth
        double depthRatio = (bidDepth + askDepth) > 0
                ? Math.abs(bidDepth - askDepth) / (bidDepth + askDepth)
                : 0.0;

        return imbalance * (0.7 + 0.3 * depthRatio);
    }

    private static boolean isBuyDirection(String direction) {
        return direction != null && direction.toUpperCase().contains("BUY");
    }
}
