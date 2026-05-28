package com.github.shk0da.GoldenDragon.model;

public class Config {

    public final int emaTrend;
    public final int emaFast;
    public final int emaSlow;
    public final int rsiPeriod;
    public final int adxPeriod;
    public final int atrPeriod;
    public final double adxMin;
    public final double rsiOversold;
    public final double rsiOverbought;
    public final double commission;
    public final int maxCandlesHold;
    public final int maxCandlesHoldFx;
    public final double atrSpikeThreshold;
    public final int atrSpikeWindow;
    public final int cooldownCandles;

    public Config() {
        this.emaTrend = 24;
        this.emaFast = 3;
        this.emaSlow = 7;
        this.rsiPeriod = 14;
        this.adxPeriod = 14;
        this.atrPeriod = 14;
        this.adxMin = 20.0;
        this.rsiOversold = 25.0;
        this.rsiOverbought = 75.0;
        this.commission = 0.0005;
        this.maxCandlesHold = 24;
        this.maxCandlesHoldFx = 12;
        this.atrSpikeThreshold = 3.0;
        this.atrSpikeWindow = 10;
        this.cooldownCandles = 3;
    }
}
