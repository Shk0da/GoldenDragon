package com.github.shk0da.GoldenDragon.model;

public class TradingDecision {

  public final String action;
  public final String reason;
  public final double confidence;
  public final int quantity;
  public final Double stopLoss;
  public final Double takeProfit;
  public final Double entryPrice;
  public final Position updatedPosition;

  public TradingDecision(String action, String reason) {
    this(action, reason, 0.0, 0, null, null, null, null);
  }

  public TradingDecision(
      String action,
      String reason,
      double confidence,
      int quantity,
      Double stopLoss,
      Double takeProfit,
      Double entryPrice,
      Position updatedPosition) {
    this.action = action;
    this.reason = reason;
    this.confidence = confidence;
    this.quantity = quantity;
    this.stopLoss = stopLoss;
    this.takeProfit = takeProfit;
    this.entryPrice = entryPrice;
    this.updatedPosition = updatedPosition;
  }
}
