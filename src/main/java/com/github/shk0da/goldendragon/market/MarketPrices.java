package com.github.shk0da.goldendragon.market;

/**
 * Container for bid/ask prices.
 */
public class MarketPrices {
    private final Double bid;
    private final Double ask;

    public MarketPrices(Double bid, Double ask) {
        this.bid = bid;
        this.ask = ask;
    }

    public Double getBid() {
        return bid;
    }

    public Double getAsk() {
        return ask;
    }

    @Override
    public String toString() {
        return "MarketPrices{bid=" + bid + ", ask=" + ask + "}";
    }
}
