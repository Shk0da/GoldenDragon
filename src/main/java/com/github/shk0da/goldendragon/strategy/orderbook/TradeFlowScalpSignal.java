package com.github.shk0da.goldendragon.strategy.orderbook;

import com.github.shk0da.goldendragon.config.OrderBookScalpConfig;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/** Aggressive buy-side trade flow with mild book support. */
public final class TradeFlowScalpSignal implements OrderBookSignal {

    public static final String SIGNAL_ID = "tradeFlow";

    private static final double FLOW_MULTIPLIER = 1.5;
    private static final double MIN_OBI = 0.15;
    // Reject entry if price extended more than 2 ticks above recent average (prevents buying tops)
    private static final int MAX_PRICE_EXTENSION_TICKS = 2;

    private final OrderBookScalpConfig config;
    private final Map<String, Integer> persistenceByTicker = new ConcurrentHashMap<>();
    // Track recent average price per ticker for pullback check
    private final Map<String, Double> avgPrice5Sec = new ConcurrentHashMap<>();

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
            // Pullback check: reject entry if price extended too far above average
            if (isPriceExtended(context, ticker)) {
                return OrderBookEntryDecision.none();
            }
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

    /**
     * Checks if the current price has extended too far above the recent average.
     * Rejects entries that chase the market (buying the top).
     */
    private boolean isPriceExtended(OrderBookMarketContext context, String ticker) {
        double currentPrice = context.getBestBid();
        if (currentPrice <= 0) {
            return false;
        }

        // Get or compute average price from last ~5 seconds
        Double avgPrice = avgPrice5Sec.get(ticker);
        if (avgPrice == null || avgPrice <= 0) {
            // No history yet — allow entry
            avgPrice5Sec.put(ticker, currentPrice);
            return false;
        }

        double tickSize = HftScalpDecision.calculateTickSize(currentPrice);
        double maxExtension = MAX_PRICE_EXTENSION_TICKS * tickSize;
        boolean extended = (currentPrice - avgPrice) > maxExtension;

        // Update running average with exponential decay
        double alpha = 0.1; // 10% weight for new price
        avgPrice5Sec.put(ticker, avgPrice * (1 - alpha) + currentPrice * alpha);

        return extended;
    }

    @Override
    public OrderBookEntryDecision evaluateEntryShort(
            OrderBookMarketContext context, String ticker) {
        double flowThreshold = config.getMinTradeFlow() * FLOW_MULTIPLIER;
        if (context.getTradeDelta() <= -flowThreshold && context.getObi() < -MIN_OBI) {
            // Pullback check for short: reject if price dropped too far below average
            double currentPrice = context.getBestAsk();
            if (currentPrice > 0) {
                Double avgPrice = avgPrice5Sec.get(ticker + "_short");
                if (avgPrice != null && avgPrice > 0) {
                    double tickSize = HftScalpDecision.calculateTickSize(currentPrice);
                    double maxExtension = MAX_PRICE_EXTENSION_TICKS * tickSize;
                    if ((avgPrice - currentPrice) > maxExtension) {
                        return OrderBookEntryDecision.none();
                    }
                }
                // Update average
                avgPrice5Sec.put(ticker + "_short",
                        avgPrice != null ? avgPrice * 0.9 + currentPrice * 0.1 : currentPrice);
            }

            int persistence = persistenceByTicker.merge(ticker + "_short", 1, Integer::sum);
            if (persistence >= Math.max(2, config.getPersistenceTicks() - 2)) {
                return OrderBookEntryDecision.enter(
                        String.format(
                                "SHORT flow=%.0f obi=%.2f",
                                context.getTradeDelta(), context.getObi()));
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
        boolean reversal =
                isLong
                        ? context.getTradeDelta() < -config.getMinTradeFlow()
                        : context.getTradeDelta() > config.getMinTradeFlow();
        if (reversal) {
            return "flow_reversal";
        }
        return null;
    }

    @Override
    public void reset(String ticker) {
        persistenceByTicker.put(ticker, 0);
        persistenceByTicker.put(ticker + "_short", 0);
        avgPrice5Sec.remove(ticker);
        avgPrice5Sec.remove(ticker + "_short");
    }
}
