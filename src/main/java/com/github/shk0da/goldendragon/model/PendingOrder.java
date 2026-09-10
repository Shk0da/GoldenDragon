package com.github.shk0da.goldendragon.model;

/**
 * Pending order with entry conditions from LLM debate.
 * Created by TradeCouncilStrategy when debate produces a trading decision.
 * Monitored until entry condition is met or order expires.
 */
public class PendingOrder {
    public final String ticker;
    public final String direction;
    public final Double entryPrice;
    public final Double stopLoss;
    public final Double takeProfit;
    public final Double depositPercent;
    public final String reasoning;
    public final long createdAt;
    public final long expiresAt;
    public final int ttlMinutes;

    public PendingOrder(String ticker, String direction, Double entryPrice, Double stopLoss,
                        Double takeProfit, Double depositPercent, String reasoning, int ttlMinutes) {
        this.ticker = ticker;
        this.direction = direction;
        this.entryPrice = entryPrice;
        this.stopLoss = stopLoss;
        this.takeProfit = takeProfit;
        this.depositPercent = depositPercent;
        this.reasoning = reasoning;
        this.ttlMinutes = ttlMinutes;
        this.createdAt = System.currentTimeMillis();
        this.expiresAt = this.createdAt + (ttlMinutes * 60L * 1000L);
    }

    /**
     * Check if order has expired based on TTL.
     * @return true if current time is past expiration time
     */
    public boolean isExpired() {
        return System.currentTimeMillis() > expiresAt;
    }

    /**
     * Check if entry condition is met based on current price.
     * @param currentPrice current market price
     * @param isLong true for LONG position, false for SHORT
     * @return true if entry condition is satisfied
     */
    public boolean shouldEnter(double currentPrice, boolean isLong) {
        if (entryPrice == null) return false;
        if (isLong) {
            // For LONG: enter when price is at or below entry (with 0.2% tolerance)
            return currentPrice <= entryPrice * 1.002;
        } else {
            // For SHORT: enter when price is at or above entry (with 0.2% tolerance)
            return currentPrice >= entryPrice * 0.998;
        }
    }
}
