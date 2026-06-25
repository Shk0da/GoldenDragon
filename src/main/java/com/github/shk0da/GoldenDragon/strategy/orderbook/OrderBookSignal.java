package com.github.shk0da.goldendragon.strategy.orderbook;

/**
 * Pluggable entry/exit logic for order-book trading.
 *
 * <p>Each implementation keeps per-ticker state internally. The engine calls signals in priority
 * order for entries; only the opening signal is consulted for signal-specific exits.
 */
public interface OrderBookSignal {

    String id();

    OrderBookEntryDecision evaluateEntry(OrderBookMarketContext context, String ticker);

    String evaluateExit(
            OrderBookMarketContext context, OrderBookPositionView position, String ticker);

    void reset(String ticker);
}
