package com.github.shk0da.goldendragon.strategy.orderbook;

import com.github.shk0da.goldendragon.config.OrderBookScalpConfig;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Entry signal based on order book density analysis.
 *
 * <p>Best practices for scalping density signals:
 * <ul>
 *   <li>Weighted depth imbalance: immediate support/resistance matters more than distant levels</li>
 *   <li>Depth gradient: liquidity building below = support (buy), building above = resistance (sell)</li>
 *   <li>Absorption: large resting orders being consumed indicate directional pressure</li>
 *   <li>Combine with OBI and trade flow for confirmation (not standalone)</li>
 * </ul>
 */
public final class DensityImbalanceSignal implements OrderBookSignal {

    public static final String SIGNAL_ID = "density";

    private static final double MIN_WEIGHTED_IMBALANCE = 0.25;
    private static final double MIN_ABSORPTION = 0.15;

    private final OrderBookScalpConfig config;
    private final Map<String, Integer> persistenceByTicker = new ConcurrentHashMap<>();

    public DensityImbalanceSignal(OrderBookScalpConfig config) {
        this.config = config;
    }

    @Override
    public String id() {
        return SIGNAL_ID;
    }

    @Override
    public OrderBookEntryDecision evaluateEntry(OrderBookMarketContext context, String ticker) {
        double weightedImbalance = context.getWeightedDepthImbalance();
        double gradient = context.getDepthGradient();
        double absorption = context.getAbsorptionScore();

        // Strong bid-side support: weighted imbalance positive, gradient positive (support building),
        // absorption positive (large buyer absorbing), and trade flow confirms
        boolean strongSupport =
                weightedImbalance > MIN_WEIGHTED_IMBALANCE
                        && gradient > 0.0
                        && (absorption > MIN_ABSORPTION || context.getObi() > 0.2);

        // Also require trade flow confirmation
        boolean flowConfirm = context.getTradeDelta() > config.getMinTradeFlow();

        if (strongSupport && flowConfirm) {
            int persistence = persistenceByTicker.merge(ticker, 1, Integer::sum);
            if (persistence >= Math.max(2, config.getPersistenceTicks() - 1)) {
                return OrderBookEntryDecision.enter(
                        String.format(
                                "depth_imb=%.2f grad=%.3f abs=%.2f flow=%.0f",
                                weightedImbalance,
                                gradient,
                                absorption,
                                context.getTradeDelta()));
            }
            return OrderBookEntryDecision.none();
        }
        persistenceByTicker.put(ticker, 0);
        return OrderBookEntryDecision.none();
    }

    @Override
    public OrderBookEntryDecision evaluateEntryShort(
            OrderBookMarketContext context, String ticker) {
        double weightedImbalance = context.getWeightedDepthImbalance();
        double gradient = context.getDepthGradient();
        double absorption = context.getAbsorptionScore();

        // Strong ask-side resistance: weighted imbalance negative, gradient negative (resistance building),
        // absorption negative (large seller absorbing), and trade flow confirms
        boolean strongResistance =
                weightedImbalance < -MIN_WEIGHTED_IMBALANCE
                        && gradient < 0.0
                        && (absorption < -MIN_ABSORPTION || context.getObi() < -0.2);

        boolean flowConfirm = context.getTradeDelta() < -config.getMinTradeFlow();

        if (strongResistance && flowConfirm) {
            int persistence = persistenceByTicker.merge(ticker + "_short", 1, Integer::sum);
            if (persistence >= Math.max(2, config.getPersistenceTicks() - 1)) {
                return OrderBookEntryDecision.enter(
                        String.format(
                                "SHORT depth_imb=%.2f grad=%.3f abs=%.2f flow=%.0f",
                                weightedImbalance,
                                gradient,
                                absorption,
                                context.getTradeDelta()));
            }
            return OrderBookEntryDecision.none();
        }
        persistenceByTicker.put(ticker + "_short", 0);
        return OrderBookEntryDecision.none();
    }

    @Override
    public String evaluateExit(
            OrderBookMarketContext context, OrderBookPositionView position, String ticker) {
        boolean isLong = "LONG".equals(position.getDirection());
        double absorption = context.getAbsorptionScore();

        // Exit when absorption reverses (support consumed or resistance consumed)
        if (isLong && absorption < -MIN_ABSORPTION) {
            return "absorption_reversal";
        }
        if (!isLong && absorption > MIN_ABSORPTION) {
            return "absorption_reversal";
        }

        // Exit when weighted depth imbalance flips
        if (isLong && context.getWeightedDepthImbalance() < -0.2) {
            return "depth_flip";
        }
        if (!isLong && context.getWeightedDepthImbalance() > 0.2) {
            return "depth_flip";
        }

        return null;
    }

    @Override
    public void reset(String ticker) {
        persistenceByTicker.put(ticker, 0);
        persistenceByTicker.put(ticker + "_short", 0);
    }
}
