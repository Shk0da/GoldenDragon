package com.github.shk0da.goldendragon.strategy.orderbook;

import com.github.shk0da.goldendragon.model.MarketDepthSnapshot;
import com.github.shk0da.goldendragon.model.TickerInfo;

/** Snapshot of order book metrics shared across signals on one tick. */
public final class OrderBookMarketContext {

    private final MarketDepthSnapshot snapshot;
    private final TickerInfo.Key key;
    private final String ticker;
    private final double bestBid;
    private final double bestAsk;
    private final double spread;
    private final double spreadBps;
    private final int bidQty0;
    private final int askQty0;
    private final double obi;
    private final double microEdge;
    private final double tradeDelta;

    public OrderBookMarketContext(
            MarketDepthSnapshot snapshot,
            TickerInfo.Key key,
            String ticker,
            double bestBid,
            double bestAsk,
            double spread,
            double spreadBps,
            int bidQty0,
            int askQty0,
            double obi,
            double microEdge,
            double tradeDelta) {
        this.snapshot = snapshot;
        this.key = key;
        this.ticker = ticker;
        this.bestBid = bestBid;
        this.bestAsk = bestAsk;
        this.spread = spread;
        this.spreadBps = spreadBps;
        this.bidQty0 = bidQty0;
        this.askQty0 = askQty0;
        this.obi = obi;
        this.microEdge = microEdge;
        this.tradeDelta = tradeDelta;
    }

    public MarketDepthSnapshot getSnapshot() {
        return snapshot;
    }

    public TickerInfo.Key getKey() {
        return key;
    }

    public String getTicker() {
        return ticker;
    }

    public double getBestBid() {
        return bestBid;
    }

    public double getBestAsk() {
        return bestAsk;
    }

    public double getSpread() {
        return spread;
    }

    public double getSpreadBps() {
        return spreadBps;
    }

    public int getBidQty0() {
        return bidQty0;
    }

    public int getAskQty0() {
        return askQty0;
    }

    public double getObi() {
        return obi;
    }

    public double getMicroEdge() {
        return microEdge;
    }

    public double getTradeDelta() {
        return tradeDelta;
    }
}
