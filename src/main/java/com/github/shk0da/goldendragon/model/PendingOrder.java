package com.github.shk0da.goldendragon.model;

/**
 * Pending order with entry conditions from LLM debate.
 * Created by TradeCouncilStrategy when debate produces a trading decision.
 * Monitored until entry condition is met or order expires.
 *
 * <p>The entry target is interpreted relative to {@code priceAtCreation}:
 * <ul>
 *   <li>target below creation price - wait for price to fall (pullback LONG / breakdown SHORT)</li>
 *   <li>target above creation price - wait for price to rise (breakout LONG / retest SHORT)</li>
 *   <li>target within touch tolerance - immediate market entry</li>
 * </ul>
 * The engine enters at market when price touches the target, and skips the entry
 * if price gapped beyond {@code MAX_ENTRY_DRIFT} to avoid chasing runaway prices.
 */
public class PendingOrder {

    // Relative distance at which price is considered to touch the entry target (0.1%)
    private static final double ENTRY_TOUCH_TOLERANCE = 0.001;

    // Max allowed price deviation beyond the entry target for a market entry (0.5%)
    private static final double MAX_ENTRY_DRIFT = 0.005;

    // Slippage protection: expire order if market price deviates >0.2% from entry target
    private static final double MAX_SLIPPAGE_PERCENT = 0.002;

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
    public final double priceAtCreation;

    public PendingOrder(String ticker, String direction, Double entryPrice, Double stopLoss,
                        Double takeProfit, Double depositPercent, String reasoning, int ttlMinutes,
                        double priceAtCreation) {
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
        this.priceAtCreation = priceAtCreation;
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
     * The waiting side is derived from the entry target position relative to
     * the price at order creation, not from the trade direction: a target below
     * the creation price waits for a fall, a target above waits for a rise,
     * a target within touch tolerance enters immediately.
     *
     * @param currentPrice current market price
     * @return true if entry condition is satisfied
     */
    public boolean shouldEnter(double currentPrice) {
        if (entryPrice == null || entryPrice <= 0 || currentPrice <= 0) {
            return false;
        }
        // Slippage protection: expire order if market price deviates >0.2% from entry target
        if (isSlippageExceeded(currentPrice)) {
            return false;
        }
        if (isImmediateEntry()) {
            return Math.abs(currentPrice - entryPrice) <= entryPrice * MAX_ENTRY_DRIFT;
        }
        if (entryPrice < priceAtCreation) {
            // wait for price to fall to target: pullback long or breakdown short
            return currentPrice <= entryPrice * (1 + ENTRY_TOUCH_TOLERANCE)
                && currentPrice >= entryPrice * (1 - MAX_ENTRY_DRIFT);
        }
        // wait for price to rise to target: breakout long or retest short
        return currentPrice >= entryPrice * (1 - ENTRY_TOUCH_TOLERANCE)
            && currentPrice <= entryPrice * (1 + MAX_ENTRY_DRIFT);
    }

    /**
     * Check if current price has deviated more than 0.2% from entry target.
     * If exceeded, the order should be expired (ORDER_EXPIRED) rather than executed.
     *
     * @param currentPrice current market price
     * @return true if slippage exceeds the threshold
     */
    public boolean isSlippageExceeded(double currentPrice) {
        if (entryPrice == null || entryPrice <= 0 || currentPrice <= 0) {
            return false;
        }
        double deviation = Math.abs(currentPrice - entryPrice) / entryPrice;
        return deviation > MAX_SLIPPAGE_PERCENT;
    }

    /**
     * Describe the entry trigger mode for logging.
     * @return human-readable entry mode description
     */
    public String describeEntryMode() {
        if (entryPrice == null) {
            return "no entry target";
        }
        if (isImmediateEntry()) {
            return "market entry";
        }
        boolean isLong = "LONG".equalsIgnoreCase(direction) || "BUY".equalsIgnoreCase(direction);
        if (entryPrice < priceAtCreation) {
            return isLong
                ? "wait for pullback down to " + entryPrice
                : "wait for breakdown down to " + entryPrice;
        }
        return isLong
            ? "wait for breakout up to " + entryPrice
            : "wait for retest up to " + entryPrice;
    }

    private boolean isImmediateEntry() {
        return Math.abs(entryPrice - priceAtCreation) <= priceAtCreation * ENTRY_TOUCH_TOLERANCE;
    }
}
