package com.github.shk0da.goldendragon.strategy.orderbook;

import java.util.ArrayDeque;
import java.util.Deque;

/**
 * Real-time trend filter for order-book scalping using order book data.
 *
 * <p>Detects short-term directional bias by tracking:
 * <ul>
 *   <li>Price momentum: consecutive mid-price direction changes over recent ticks</li>
 *   <li>OBI momentum: whether order book imbalance is increasing or decreasing</li>
 *   <li>Trade flow accumulation: net aggressive buying/selling over a rolling window</li>
 * </ul>
 *
 * <p>Best practices for scalping applied:
 * <ul>
 *   <li>Only enter in the direction of the micro-trend (not against it)</li>
 *   <li>Require alignment of at least 2 out of 3 indicators</li>
 *   <li>Use rolling windows (not cumulative) to adapt quickly to regime changes</li>
 * </ul>
 */
public final class OrderBookTrendFilter {

    private final int momentumWindow;
    private final int flowAccumulationWindow;
    private final double minMomentumRatio;
    private final double minFlowAccumulation;

    private final Deque<Double> midPriceHistory = new ArrayDeque<>();
    private final Deque<Double> obiHistory = new ArrayDeque<>();
    private final Deque<Double> flowHistory = new ArrayDeque<>();

    public OrderBookTrendFilter(
            int momentumWindow,
            int flowAccumulationWindow,
            double minMomentumRatio,
            double minFlowAccumulation) {
        this.momentumWindow = Math.max(3, momentumWindow);
        this.flowAccumulationWindow = Math.max(1, flowAccumulationWindow);
        this.minMomentumRatio = minMomentumRatio;
        this.minFlowAccumulation = minFlowAccumulation;
    }

    /**
     * Evaluates whether trading in the given direction aligns with the micro-trend.
     *
     * @param mid current mid price
     * @param obi current order book imbalance
     * @param tradeDelta current trade flow delta
     * @param isLong {@code true} to check LONG direction, {@code false} for SHORT
     * @return trend signal strength: positive = aligned, negative = against trend, 0 = no trend
     */
    public double evaluate(double mid, double obi, double tradeDelta, boolean isLong) {
        recordTick(mid, obi, tradeDelta);

        double priceScore = evaluatePriceMomentum(isLong);
        double obiScore = evaluateObiMomentum(isLong);
        double flowScore = evaluateFlowAccumulation(isLong);

        // Weighted composite: price momentum is most important for scalping
        double composite = priceScore * 0.45 + obiScore * 0.30 + flowScore * 0.25;
        return composite;
    }

    /**
     * Returns {@code true} if the micro-trend supports trading in the given direction.
     *
     * <p>Requires at least 2 of 3 indicators to agree on direction.
     */
    public boolean allowsDirection(double mid, double obi, double tradeDelta, boolean isLong) {
        double score = evaluate(mid, obi, tradeDelta, isLong);

        int aligned = 0;
        if (evaluatePriceMomentum(isLong) > 0) aligned++;
        if (evaluateObiMomentum(isLong) > 0) aligned++;
        if (evaluateFlowAccumulation(isLong) > 0) aligned++;

        // Minimum 2 indicators must agree, and composite must be positive
        return score > 0.0 && aligned >= 2;
    }

    public void reset() {
        midPriceHistory.clear();
        obiHistory.clear();
        flowHistory.clear();
    }

    private void recordTick(double mid, double obi, double tradeDelta) {
        midPriceHistory.addLast(mid);
        obiHistory.addLast(obi);
        flowHistory.addLast(tradeDelta);
        while (midPriceHistory.size() > momentumWindow) {
            midPriceHistory.removeFirst();
        }
        while (obiHistory.size() > momentumWindow) {
            obiHistory.removeFirst();
        }
        while (flowHistory.size() > flowAccumulationWindow) {
            flowHistory.removeFirst();
        }
    }

    /**
     * Price momentum: compares recent price movement to older movement within the window.
     * Returns positive if price is trending in the isLong direction.
     */
    private double evaluatePriceMomentum(boolean isLong) {
        if (midPriceHistory.size() < 3) {
            return 0.0;
        }
        int half = Math.max(1, midPriceHistory.size() / 2);
        Double[] prices = midPriceHistory.toArray(new Double[0]);
        int len = prices.length;

        // First half average vs second half average
        double firstHalfAvg = 0.0;
        for (int i = 0; i < half; i++) {
            firstHalfAvg += prices[i];
        }
        firstHalfAvg /= half;

        double secondHalfAvg = 0.0;
        for (int i = half; i < len; i++) {
            secondHalfAvg += prices[i];
        }
        secondHalfAvg /= (len - half);

        double change = secondHalfAvg - firstHalfAvg;
        double midRef = firstHalfAvg > 0.0 ? firstHalfAvg : 1.0;
        double changeBps = Math.abs(change) / midRef * 10_000.0;

        if (changeBps < 0.5) {
            return 0.0; // No meaningful movement
        }

        boolean trendingUp = change > 0.0;
        return (isLong == trendingUp) ? Math.min(1.0, changeBps / 5.0) : -Math.min(1.0, changeBps / 5.0);
    }

    /**
     * OBI momentum: whether the order book imbalance is strengthening in the isLong direction.
     */
    private double evaluateObiMomentum(boolean isLong) {
        if (obiHistory.size() < 3) {
            return 0.0;
        }
        int half = Math.max(1, obiHistory.size() / 2);
        Double[] obis = obiHistory.toArray(new Double[0]);
        int len = obis.length;

        double firstHalfAvg = 0.0;
        for (int i = 0; i < half; i++) {
            firstHalfAvg += obis[i];
        }
        firstHalfAvg /= half;

        double secondHalfAvg = 0.0;
        for (int i = half; i < len; i++) {
            secondHalfAvg += obis[i];
        }
        secondHalfAvg /= (len - half);

        double obiDelta = secondHalfAvg - firstHalfAvg;
        if (Math.abs(obiDelta) < 0.05) {
            return 0.0;
        }

        boolean obiIncreasing = obiDelta > 0.0;
        return (isLong == obiIncreasing) ? Math.min(1.0, Math.abs(obiDelta) * 5.0)
                : -Math.min(1.0, Math.abs(obiDelta) * 5.0);
    }

    /**
     * Trade flow accumulation: net flow over the rolling window. Positive means net buying
     * pressure.
     */
    private double evaluateFlowAccumulation(boolean isLong) {
        if (flowHistory.isEmpty()) {
            return 0.0;
        }
        double totalFlow = 0.0;
        for (Double f : flowHistory) {
            totalFlow += f;
        }
        double avgFlow = totalFlow / flowHistory.size();

        if (Math.abs(avgFlow) < minFlowAccumulation) {
            return 0.0;
        }

        boolean netBuying = avgFlow > 0.0;
        return (isLong == netBuying) ? Math.min(1.0, Math.abs(avgFlow) / (minFlowAccumulation * 3.0))
                : -Math.min(1.0, Math.abs(avgFlow) / (minFlowAccumulation * 3.0));
    }
}
