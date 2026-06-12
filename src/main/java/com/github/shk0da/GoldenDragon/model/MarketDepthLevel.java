package com.github.shk0da.goldendragon.model;

public class MarketDepthLevel {

  private final double price;
  private final int quantity;

  public MarketDepthLevel(double price, int quantity) {
    this.price = price;
    this.quantity = quantity;
  }

  public double getPrice() {
    return price;
  }

  public int getQuantity() {
    return quantity;
  }
}
