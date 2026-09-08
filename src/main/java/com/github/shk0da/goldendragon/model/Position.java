package com.github.shk0da.goldendragon.model;

public class Position {

    public final String direction;
    public final Double entryPrice;
    public final Double stopLoss;
    public final Double takeProfit;
    public final Double takeProfit2;
    public final int quantity2;
    public final boolean partialClosed;
    public final int quantity;
    public final int candlesHeld;
    public final int cooldownRemaining;

    /** Leverage applied at entry; used for margin and PnL accounting. */
    public final int appliedLeverage;

    public Position() {
        this(null, null, null, null, null, 0, 0, 0, 0, false);
    }

    public Position(int cooldownRemaining) {
        this(null, null, null, null, null, 0, 0, cooldownRemaining, 0, false);
    }

    public Position(
            String direction,
            Double entryPrice,
            Double stopLoss,
            Double takeProfit,
            int quantity,
            int candlesHeld) {
        this(direction, entryPrice, stopLoss, takeProfit, null, quantity, candlesHeld, 0, 0, false);
    }

    public Position(
            String direction,
            Double entryPrice,
            Double stopLoss,
            Double takeProfit,
            int quantity,
            int candlesHeld,
            int cooldownRemaining) {
        this(direction, entryPrice, stopLoss, takeProfit, null, quantity, candlesHeld, cooldownRemaining, 0, false);
    }

    public Position(
            String direction,
            Double entryPrice,
            Double stopLoss,
            Double takeProfit,
            Double takeProfit2,
            int quantity,
            int candlesHeld,
            int cooldownRemaining,
            int appliedLeverage,
            boolean partialClosed) {
        this.direction = direction;
        this.entryPrice = entryPrice;
        this.stopLoss = stopLoss;
        this.takeProfit = takeProfit;
        this.takeProfit2 = takeProfit2;
        this.quantity = quantity;
        this.quantity2 = 0;
        this.candlesHeld = candlesHeld;
        this.cooldownRemaining = cooldownRemaining;
        this.appliedLeverage = Math.max(1, appliedLeverage);
        this.partialClosed = partialClosed;
    }

    /**
     * Create position after partial close (TP1 hit).
     * @param original original position
     * @param remainingQty remaining quantity after TP1
     * @param newStopLoss new stop loss (breakeven)
     */
    public Position(Position original, int remainingQty, Double newStopLoss) {
        this.direction = original.direction;
        this.entryPrice = original.entryPrice;
        this.stopLoss = newStopLoss;
        this.takeProfit = original.takeProfit2;
        this.takeProfit2 = null;
        this.quantity = remainingQty;
        this.quantity2 = 0;
        this.candlesHeld = original.candlesHeld;
        this.cooldownRemaining = original.cooldownRemaining;
        this.appliedLeverage = original.appliedLeverage;
        this.partialClosed = true;
    }
}
