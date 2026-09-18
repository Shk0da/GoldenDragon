package com.github.shk0da.goldendragon.model;

public class Position {

    public final String direction;
    public final Double entryPrice;
    public final Double stopLoss;
    public final Double takeProfit;
    public final int quantity;
    public final int candlesHeld;
    public final int cooldownRemaining;

    /** Leverage applied at entry; used for margin and PnL accounting. */
    public final int appliedLeverage;

    /** Highest price reached since trailing TP activated. */
    public final Double highestPrice;

    /** Execution price for trailing TP close (highestPrice * (1 - callbackPercent)). */
    public final Double executionPrice;

    public Position() {
        this(null, null, null, null, 0, 0, 0, 0, null, null);
    }

    public Position(int cooldownRemaining) {
        this(null, null, null, null, 0, 0, cooldownRemaining, 0, null, null);
    }

    public Position(
            String direction,
            Double entryPrice,
            Double stopLoss,
            Double takeProfit,
            int quantity,
            int candlesHeld) {
        this(direction, entryPrice, stopLoss, takeProfit, quantity, candlesHeld, 0, 1, null, null);
    }

    public Position(
            String direction,
            Double entryPrice,
            Double stopLoss,
            Double takeProfit,
            int quantity,
            int candlesHeld,
            int cooldownRemaining) {
        this(direction, entryPrice, stopLoss, takeProfit, quantity, candlesHeld, cooldownRemaining, 1, null, null);
    }

    public Position(
            String direction,
            Double entryPrice,
            Double stopLoss,
            Double takeProfit,
            int quantity,
            int candlesHeld,
            int cooldownRemaining,
            int appliedLeverage) {
        this(direction, entryPrice, stopLoss, takeProfit, quantity, candlesHeld, cooldownRemaining, appliedLeverage, null, null);
    }

    public Position(
            String direction,
            Double entryPrice,
            Double stopLoss,
            Double takeProfit,
            int quantity,
            int candlesHeld,
            int cooldownRemaining,
            int appliedLeverage,
            Double highestPrice,
            Double executionPrice) {
        this.direction = direction;
        this.entryPrice = entryPrice;
        this.stopLoss = stopLoss;
        this.takeProfit = takeProfit;
        this.quantity = quantity;
        this.candlesHeld = candlesHeld;
        this.cooldownRemaining = cooldownRemaining;
        this.appliedLeverage = Math.max(1, appliedLeverage);
        this.highestPrice = highestPrice;
        this.executionPrice = executionPrice;
    }
}
