package com.github.shk0da.goldendragon.strategy.orderbook;

import com.github.shk0da.goldendragon.config.OrderBookScalpConfig;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/** Aggressive buy-side trade flow with mild book support. */
public final class TradeFlowScalpSignal implements OrderBookSignal {

    public static final String SIGNAL_ID = "tradeFlow";

    private static final double FLOW_MULTIPLIER = 1.5;
    private static final double MIN_OBI = 0.15;

    private final OrderBookScalpConfig config;
    private final Map<String, Integer> persistenceByTicker = new ConcurrentHashMap<>();

    public TradeFlowScalpSignal(OrderBookScalpConfig config) {
        this.config = config;
    }

    @Override
    public String id() {
        return SIGNAL_ID;
    }

    @Override
    public OrderBookEntryDecision evaluateEntry(OrderBookMarketContext context, String ticker) {
        double flowThreshold = config.getMinTradeFlow() * FLOW_MULTIPLIER;
        if (context.getTradeDelta() >= flowThreshold && context.getObi() > MIN_OBI) {
            int persistence = persistenceByTicker.merge(ticker, 1, Integer::sum);
            if (persistence >= Math.max(2, config.getPersistenceTicks() - 2)) {
                return OrderBookEntryDecision.enter(
                        String.format(
                                "flow=%.0f obi=%.2f", context.getTradeDelta(), context.getObi()));
            }
            return OrderBookEntryDecision.none();
        }
        persistenceByTicker.put(ticker, 0);
        return OrderBookEntryDecision.none();
    }

    @Override
    public OrderBookEntryDecision evaluateEntryShort(OrderBookMarketContext context, String ticker) {
        double flowThreshold = config.getMinTradeFlow() * FLOW_MULTIPLIER;
        if (context.getTradeDelta() <= -flowThreshold && context.getObi() < -MIN_OBI) {
            int persistence = persistenceByTicker.merge(ticker + "_short", 1, Integer::sum);
            if (persistence >= Math.max(2, config.getPersistenceTicks() - 2)) {
                return OrderBookEntryDecision.enter(
                        String.format(
                                "SHORT flow=%.0f obi=%.2f", context.getTradeDelta(), context.getObi()));
            }
            return OrderBookEntryDecision.none();
        }
        persistenceByTicker.put(ticker + "_short", 0);
        return OrderBookEntryDecision.none();
    }

    @Override
    public String evaluateExit(
            OrderBookMarketContext context, OrderBookPositionView position, String ticker) {
        if (context.getTradeDelta() < -config.getMinTradeFlow()) {
            return "flow_reversal";
        }
        return null;
    }

    @Override
    public void reset(String ticker) {
        persistenceByTicker.put(ticker, 0);
        persistenceByTicker.put(ticker + "_short", 0);
    }
}
