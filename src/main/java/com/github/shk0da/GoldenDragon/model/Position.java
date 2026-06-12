package com.github.shk0da.GoldenDragon.model;

public class Position {

  public final String direction;
  public final Double entryPrice;
  public final Double stopLoss;
  public final Double takeProfit;
  public final int quantity;
  public final int candlesHeld;
  public final int cooldownRemaining;

  public Position() {
    this(null, null, null, null, 0, 0, 0);
  }

  public Position(int cooldownRemaining) {
    this(null, null, null, null, 0, 0, cooldownRemaining);
  }

  public Position(
      String direction,
      Double entryPrice,
      Double stopLoss,
      Double takeProfit,
      int quantity,
      int candlesHeld) {
    this(direction, entryPrice, stopLoss, takeProfit, quantity, candlesHeld, 0);
  }

  public Position(
      String direction,
      Double entryPrice,
      Double stopLoss,
      Double takeProfit,
      int quantity,
      int candlesHeld,
      int cooldownRemaining) {
    this.direction = direction;
    this.entryPrice = entryPrice;
    this.stopLoss = stopLoss;
    this.takeProfit = takeProfit;
    this.quantity = quantity;
    this.candlesHeld = candlesHeld;
    this.cooldownRemaining = cooldownRemaining;
  }
}
