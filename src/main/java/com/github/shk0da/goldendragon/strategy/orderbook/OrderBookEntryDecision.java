package com.github.shk0da.goldendragon.strategy.orderbook;

/** Entry decision produced by an {@link OrderBookSignal}. */
public final class OrderBookEntryDecision {

    private static final OrderBookEntryDecision NONE = new OrderBookEntryDecision(false, "");

    private final boolean enter;
    private final String description;

    private OrderBookEntryDecision(boolean enter, String description) {
        this.enter = enter;
        this.description = description;
    }

    public static OrderBookEntryDecision none() {
        return NONE;
    }

    public static OrderBookEntryDecision enter(String description) {
        return new OrderBookEntryDecision(true, description);
    }

    public boolean isEnter() {
        return enter;
    }

    public String getDescription() {
        return description;
    }
}
