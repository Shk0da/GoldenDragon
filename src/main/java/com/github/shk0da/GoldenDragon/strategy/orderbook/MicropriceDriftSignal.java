package com.github.shk0da.goldendragon.strategy.orderbook;

import com.github.shk0da.goldendragon.config.OrderBookScalpConfig;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/** Sustained microprice drift above mid with light OBI confirmation. */
public final class MicropriceDriftSignal implements OrderBookSignal {

    public static final String SIGNAL_ID = "microprice";

    private static final double EDGE_FRACTION = 0.8;
    private static final double MIN_OBI = 0.1;

    private final OrderBookScalpConfig config;
    private final Map<String, Integer> persistenceByTicker = new ConcurrentHashMap<>();

    public MicropriceDriftSignal(OrderBookScalpConfig config) {
        this.config = config;
    }

    @Override
    public String id() {
        return SIGNAL_ID;
    }

    @Override
    public OrderBookEntryDecision evaluateEntry(OrderBookMarketContext context, String ticker) {
        double edgeThreshold = config.getEdgeSpreadFraction() * EDGE_FRACTION * context.getSpread();
        if (context.getMicroEdge() > edgeThreshold && context.getObi() > MIN_OBI) {
            int persistence = persistenceByTicker.merge(ticker, 1, Integer::sum);
            if (persistence >= config.getPersistenceTicks()) {
                return OrderBookEntryDecision.enter(
                        String.format(
                                "edge=%.5f obi=%.2f", context.getMicroEdge(), context.getObi()));
            }
            return OrderBookEntryDecision.none();
        }
        persistenceByTicker.put(ticker, 0);
        return OrderBookEntryDecision.none();
    }

    @Override
    public OrderBookEntryDecision evaluateEntryShort(OrderBookMarketContext context, String ticker) {
        double edgeThreshold = config.getEdgeSpreadFraction() * EDGE_FRACTION * context.getSpread();
        if (context.getMicroEdge() < -edgeThreshold && context.getObi() < -MIN_OBI) {
            int persistence = persistenceByTicker.merge(ticker + "_short", 1, Integer::sum);
            if (persistence >= config.getPersistenceTicks()) {
                return OrderBookEntryDecision.enter(
                        String.format(
                                "SHORT edge=%.5f obi=%.2f", context.getMicroEdge(), context.getObi()));
            }
            return OrderBookEntryDecision.none();
        }
        persistenceByTicker.put(ticker + "_short", 0);
        return OrderBookEntryDecision.none();
    }

    @Override
    public String evaluateExit(
            OrderBookMarketContext context, OrderBookPositionView position, String ticker) {
        if (context.getMicroEdge() < 0.0) {
            return "microprice_reversal";
        }
        return null;
    }

    @Override
    public void reset(String ticker) {
        persistenceByTicker.put(ticker, 0);
        persistenceByTicker.put(ticker + "_short", 0);
    }
}
