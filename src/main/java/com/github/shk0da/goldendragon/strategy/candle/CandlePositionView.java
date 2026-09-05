package com.github.shk0da.goldendragon.strategy.candle;

/**
 * View of a current position for exit evaluation.
 */
public class CandlePositionView {
    private final String direction;
    private final double entryPrice;
    private final int units;
    private final String signalId;

    public CandlePositionView(String direction, double entryPrice, int units, String signalId) {
        this.direction = direction;
        this.entryPrice = entryPrice;
        this.units = units;
        this.signalId = signalId;
    }

    public String getDirection() {
        return direction;
    }

    public double getEntryPrice() {
        return entryPrice;
    }

    public int getUnits() {
        return units;
    }

    public String getSignalId() {
        return signalId;
    }
}
