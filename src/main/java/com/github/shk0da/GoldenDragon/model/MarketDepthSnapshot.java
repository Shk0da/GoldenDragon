package com.github.shk0da.GoldenDragon.model;

import java.time.Instant;
import java.util.Collections;
import java.util.List;

public class MarketDepthSnapshot {

  private final String figi;
  private final Instant time;
  private final boolean consistent;
  private final List<MarketDepthLevel> bids;
  private final List<MarketDepthLevel> asks;

  public MarketDepthSnapshot(
      String figi,
      Instant time,
      boolean consistent,
      List<MarketDepthLevel> bids,
      List<MarketDepthLevel> asks) {
    this.figi = figi;
    this.time = time;
    this.consistent = consistent;
    this.bids = bids == null ? Collections.emptyList() : Collections.unmodifiableList(bids);
    this.asks = asks == null ? Collections.emptyList() : Collections.unmodifiableList(asks);
  }

  public String getFigi() {
    return figi;
  }

  public Instant getTime() {
    return time;
  }

  public boolean isConsistent() {
    return consistent;
  }

  public List<MarketDepthLevel> getBids() {
    return bids;
  }

  public List<MarketDepthLevel> getAsks() {
    return asks;
  }

  public Double getBestBid() {
    return bids.isEmpty() ? null : bids.get(0).getPrice();
  }

  public Double getBestAsk() {
    return asks.isEmpty() ? null : asks.get(0).getPrice();
  }

  public Double getMidPrice() {
    Double bestBid = getBestBid();
    Double bestAsk = getBestAsk();
    if (bestBid == null || bestAsk == null) {
      return null;
    }
    return (bestBid + bestAsk) / 2.0;
  }
}
