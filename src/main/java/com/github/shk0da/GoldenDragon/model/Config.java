package com.github.shk0da.goldendragon.model;

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

    // Market Regime Filter parameters
    public final boolean marketRegimeFilterEnabled;

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

    // Money Management parameters
    public final boolean mmEnabled;
    public final double mmRiskPercent;
    public final double mmMaxDailyLossPercent;
    public final int mmMaxConsecutiveLosses;
    public final String mmSizingStrategy;
    public final double mmVolatilityBaseAtr;
    public final double mmVolatilityMinAdjustment;
    public final double mmVolatilityMaxAdjustment;
    public final double mmAtrStopMultiplier;
    public final double mmTrailingActivationR;
    public final double mmTrailingMultiplier;
    public final double mmBreakevenActivationR;
    public final double mmBreakevenBuffer;
    public final boolean mmAdaptiveEnabled;
    public final int mmLossesToReduce;
    public final int mmWinsToRestore;
    public final double mmRiskReductionFactor;
    public final double mmCriticalDrawdownPercent;
    public final double mmMaxPositionSize;
    public final boolean shortsEnabled;

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
        this.marketRegimeFilterEnabled = true;

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

        // Money Management defaults
        this.mmEnabled = true;
        this.mmRiskPercent = 0.01;
        this.mmMaxDailyLossPercent = 0.03;
        this.mmMaxConsecutiveLosses = 3;
        this.mmSizingStrategy = "FIXED";
        this.mmVolatilityBaseAtr = 1.0;
        this.mmVolatilityMinAdjustment = 0.5;
        this.mmVolatilityMaxAdjustment = 1.5;
        this.mmAtrStopMultiplier = 2.0;
        this.mmTrailingActivationR = 1.0;
        this.mmTrailingMultiplier = 1.0;
        this.mmBreakevenActivationR = 0.5;
        this.mmBreakevenBuffer = 0.001;
        this.mmAdaptiveEnabled = true;
        this.mmLossesToReduce = 3;
        this.mmWinsToRestore = 5;
        this.mmRiskReductionFactor = 0.5;
        this.mmCriticalDrawdownPercent = 0.10;
        this.mmMaxPositionSize = 0.25;
        this.shortsEnabled = false;
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
            double badWeatherAtrSpikeThreshold) {
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

        this.marketRegimeFilterEnabled = true;

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

        // Money Management defaults
        this.mmEnabled = true;
        this.mmRiskPercent = 0.01;
        this.mmMaxDailyLossPercent = 0.03;
        this.mmMaxConsecutiveLosses = 3;
        this.mmSizingStrategy = "FIXED";
        this.mmVolatilityBaseAtr = 1.0;
        this.mmVolatilityMinAdjustment = 0.5;
        this.mmVolatilityMaxAdjustment = 1.5;
        this.mmAtrStopMultiplier = 2.0;
        this.mmTrailingActivationR = 1.0;
        this.mmTrailingMultiplier = 1.0;
        this.mmBreakevenActivationR = 0.5;
        this.mmBreakevenBuffer = 0.001;
        this.mmAdaptiveEnabled = true;
        this.mmLossesToReduce = 3;
        this.mmWinsToRestore = 5;
        this.mmRiskReductionFactor = 0.5;
        this.mmCriticalDrawdownPercent = 0.10;
        this.mmMaxPositionSize = 0.25;
        this.shortsEnabled = false;
    }

    /** Create Config with Money Management parameters. */
    public Config(
            boolean mmEnabled,
            double mmRiskPercent,
            double mmMaxDailyLossPercent,
            int mmMaxConsecutiveLosses,
            String mmSizingStrategy,
            double mmVolatilityBaseAtr,
            double mmVolatilityMinAdjustment,
            double mmVolatilityMaxAdjustment,
            double mmAtrStopMultiplier,
            double mmTrailingActivationR,
            double mmTrailingMultiplier,
            double mmBreakevenActivationR,
            double mmBreakevenBuffer,
            boolean mmAdaptiveEnabled,
            int mmLossesToReduce,
            int mmWinsToRestore,
            double mmRiskReductionFactor,
            double mmCriticalDrawdownPercent,
            double mmMaxPositionSize,
            boolean shortsEnabled) {
        // Default values for existing fields
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
        this.marketRegimeFilterEnabled = true;
        this.badWeatherFilterEnabled = false;
        this.badWeatherLowVolumeThreshold = 0.5;
        this.badWeatherLowAtrThreshold = 0.7;
        this.badWeatherMinRangePercent = 0.005;
        this.badWeatherHighAtrThreshold = 2.0;
        this.badWeatherMaxSpreadPercent = 0.01;
        this.badWeatherMaxWickRatio = 0.4;
        this.badWeatherPanicVolumeThreshold = 3.0;
        this.badWeatherMinAvgDailyVolume = 100000;
        this.badWeatherAtrSpikeThreshold = 2.5;

        // Money Management parameters
        this.mmEnabled = mmEnabled;
        this.mmRiskPercent = mmRiskPercent;
        this.mmMaxDailyLossPercent = mmMaxDailyLossPercent;
        this.mmMaxConsecutiveLosses = mmMaxConsecutiveLosses;
        this.mmSizingStrategy = mmSizingStrategy;
        this.mmVolatilityBaseAtr = mmVolatilityBaseAtr;
        this.mmVolatilityMinAdjustment = mmVolatilityMinAdjustment;
        this.mmVolatilityMaxAdjustment = mmVolatilityMaxAdjustment;
        this.mmAtrStopMultiplier = mmAtrStopMultiplier;
        this.mmTrailingActivationR = mmTrailingActivationR;
        this.mmTrailingMultiplier = mmTrailingMultiplier;
        this.mmBreakevenActivationR = mmBreakevenActivationR;
        this.mmBreakevenBuffer = mmBreakevenBuffer;
        this.mmAdaptiveEnabled = mmAdaptiveEnabled;
        this.mmLossesToReduce = mmLossesToReduce;
        this.mmWinsToRestore = mmWinsToRestore;
        this.mmRiskReductionFactor = mmRiskReductionFactor;
        this.mmCriticalDrawdownPercent = mmCriticalDrawdownPercent;
        this.mmMaxPositionSize = mmMaxPositionSize;
        this.shortsEnabled = shortsEnabled;
    }
}
