package com.github.shk0da.goldendragon.model;

import java.time.Instant;

public class MarketTradeTick {

  private final String figi;
  private final Instant time;
  private final double price;
  private final long quantity;
  private final String direction;

  public MarketTradeTick(String figi, Instant time, double price, long quantity, String direction) {
    this.figi = figi;
    this.time = time;
    this.price = price;
    this.quantity = quantity;
    this.direction = direction;
  }

  public String getFigi() {
    return figi;
  }

  public Instant getTime() {
    return time;
  }

  public double getPrice() {
    return price;
  }

  public long getQuantity() {
    return quantity;
  }

  public String getDirection() {
    return direction;
  }
}
