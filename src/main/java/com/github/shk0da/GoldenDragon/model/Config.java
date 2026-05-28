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

    // Bad Weather Filter parameters
    public final boolean badWeatherFilterEnabled;
    public final double badWeatherLowVolumeThreshold;
    public final double badWeatherLowAtrThreshold;
    public final double badWeatherMinRangePercent;
    public final double badWeatherHighAtrThreshold;
    public final double badWeatherMaxSpreadPercent;
    public final double badWeatherMaxWickRatio;
    public final double badWeatherPanicVolumeThreshold;
    public final double badWeatherMinAvgDailyVolume;
    public final double badWeatherAtrSpikeThreshold;

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

        // Bad Weather Filter defaults
        this.badWeatherFilterEnabled = false; // Disabled by default
        this.badWeatherLowVolumeThreshold = 0.5;
        this.badWeatherLowAtrThreshold = 0.7;
        this.badWeatherMinRangePercent = 0.005;
        this.badWeatherHighAtrThreshold = 2.0;
        this.badWeatherMaxSpreadPercent = 0.01;
        this.badWeatherMaxWickRatio = 0.4;
        this.badWeatherPanicVolumeThreshold = 3.0;
        this.badWeatherMinAvgDailyVolume = 100000;
        this.badWeatherAtrSpikeThreshold = 2.5;
    }

    public Config(
            boolean badWeatherFilterEnabled,
            double badWeatherLowVolumeThreshold,
            double badWeatherLowAtrThreshold,
            double badWeatherMinRangePercent,
            double badWeatherHighAtrThreshold,
            double badWeatherMaxSpreadPercent,
            double badWeatherMaxWickRatio,
            double badWeatherPanicVolumeThreshold,
            double badWeatherMinAvgDailyVolume,
            double badWeatherAtrSpikeThreshold
    ) {
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

        this.badWeatherFilterEnabled = badWeatherFilterEnabled;
        this.badWeatherLowVolumeThreshold = badWeatherLowVolumeThreshold;
        this.badWeatherLowAtrThreshold = badWeatherLowAtrThreshold;
        this.badWeatherMinRangePercent = badWeatherMinRangePercent;
        this.badWeatherHighAtrThreshold = badWeatherHighAtrThreshold;
        this.badWeatherMaxSpreadPercent = badWeatherMaxSpreadPercent;
        this.badWeatherMaxWickRatio = badWeatherMaxWickRatio;
        this.badWeatherPanicVolumeThreshold = badWeatherPanicVolumeThreshold;
        this.badWeatherMinAvgDailyVolume = badWeatherMinAvgDailyVolume;
        this.badWeatherAtrSpikeThreshold = badWeatherAtrSpikeThreshold;
    }
}
