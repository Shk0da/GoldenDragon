package com.github.shk0da.goldendragon.model;

public class TradingDecision {

    public final String action;
    public final String reason;
    public final double confidence;
    public final int quantity;
    public final Double depositPercent;
    public final Double stopLoss;
    public final Double takeProfit;
    public final Double entryPrice;
    public final Position updatedPosition;
    public final int ttlMinutes;

    public TradingDecision(String action, String reason) {
        this(action, reason, 0.0, 0, null, null, null, null, null, 30);
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
        this(action, reason, confidence, quantity, null, stopLoss, takeProfit, entryPrice, updatedPosition, 30);
    }

    public TradingDecision(
            String action,
            String reason,
            double confidence,
            int quantity,
            Double depositPercent,
            Double stopLoss,
            Double takeProfit,
            Double entryPrice,
            Position updatedPosition,
            int ttlMinutes) {
        this.action = action;
        this.reason = reason;
        this.confidence = confidence;
        this.quantity = quantity;
        this.depositPercent = depositPercent;
        this.stopLoss = stopLoss;
        this.takeProfit = takeProfit;
        this.entryPrice = entryPrice;
        this.updatedPosition = updatedPosition;
        this.ttlMinutes = ttlMinutes;
    }
}
