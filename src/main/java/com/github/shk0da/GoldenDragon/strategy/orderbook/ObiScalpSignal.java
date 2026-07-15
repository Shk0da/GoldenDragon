package com.github.shk0da.goldendragon.strategy.orderbook;

import com.github.shk0da.goldendragon.config.OrderBookScalpConfig;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/** OBI + microprice edge + trade flow persistence entry; OBI flip exit. */
public final class ObiScalpSignal implements OrderBookSignal {

    public static final String SIGNAL_ID = "obi";

    private final OrderBookScalpConfig config;
    private final Map<String, Integer> persistenceByTicker = new ConcurrentHashMap<>();

    public ObiScalpSignal(OrderBookScalpConfig config) {
        this.config = config;
    }

    @Override
    public String id() {
        return SIGNAL_ID;
    }

    @Override
    public OrderBookEntryDecision evaluateEntry(OrderBookMarketContext context, String ticker) {
        if (context.getObi() > config.getObiThreshold()
                && context.getMicroEdge() > config.getEdgeSpreadFraction() * context.getSpread()
                && context.getTradeDelta() >= config.getMinTradeFlow()) {
            int persistence = persistenceByTicker.merge(ticker, 1, Integer::sum);
            if (persistence >= config.getPersistenceTicks()) {
                return OrderBookEntryDecision.enter(
                        String.format(
                                "OBI=%.2f edge=%.5f flow=%.0f",
                                context.getObi(), context.getMicroEdge(), context.getTradeDelta()));
            }
            return OrderBookEntryDecision.none();
        }
        persistenceByTicker.put(ticker, 0);
        return OrderBookEntryDecision.none();
    }

    @Override
    public String evaluateExit(
            OrderBookMarketContext context, OrderBookPositionView position, String ticker) {
        if (context.getObi() < config.getObiExitThreshold()) {
            return "obi_flip";
        }
        return null;
    }

    @Override
    public void reset(String ticker) {
        persistenceByTicker.put(ticker, 0);
    }
}
