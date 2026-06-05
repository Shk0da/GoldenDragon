package com.github.shk0da.GoldenDragon.config;

import com.github.shk0da.GoldenDragon.model.TickerType;
import com.github.shk0da.GoldenDragon.utils.PropertiesUtils;
import com.github.shk0da.GoldenDragon.utils.TickerTypeResolver;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;


import static java.util.Arrays.stream;
import static java.util.stream.Collectors.toList;

public class OrderFlowScalpingConfig {

    public static class Instrument {

        private final String ticker;
        private final TickerType type;

        public Instrument(String ticker, TickerType type) {
            this.ticker = ticker;
            this.type = type;
        }

        public String getTicker() {
            return ticker;
        }

        public TickerType getType() {
            return type;
        }

        @Override
        public String toString() {
            return "Instrument{" +
                    "ticker='" + ticker + '\'' +
                    ", type=" + type +
                    '}';
        }
    }

    private final List<Instrument> instruments;
    private final int orderBookDepth;
    private final double dailyLossLimitPercent;
    private final double maxPositionBalancePercent;
    private final int maxTotalPositions;
    private final int maxPositionsPerTicker;
    private final int loopSleepMs;
    private final int imbalanceHoldMs;
    private final int icebergReplenishMs;
    private final int spoofFastCancelMs;
    private final double baseRiskPercent;
    private final double highQualityRiskPercent;
    private final double stackedRiskPercent;
    private final int spoofTakeTicks;
    private final int imbalanceTakeTicksMin;
    private final int imbalanceTakeTicksMax;
    private final int trailingTicks;
    private final int icebergTrailingTicks;
    private final int absorptionTrailingTicks;
    private final int imbalanceThresholdLevels;
    private final double imbalanceThreshold;
    private final int icebergMinReplenishments;
    private final double icebergTradeToVisibleRatio;
    private final double spoofLevelMultiplier;
    private final int spoofMinDepthTicks;
    private final int spoofMaxDepthTicks;
    private final int stackedLevels;
    private final double stackedVolumeMultiplier;
    private final double breakoutTradeMultiplier;
    private final int timeoutSeconds;
    private final int spoofTimeoutSeconds;
    private final int absorptionFlatTicks;
    private final int localWindowSeconds;
    private final int maxSpreadTicks;
    private final int bookHistorySeconds;
    private final int volumeHistorySeconds;
    private final int icebergOppositeTicks;
    private final int absorptionWindowSeconds;
    private final int stackedPersistenceSeconds;
    private final int breakoutConfirmationMs;
    private final double lowLiquidityBookVolume;
    private final double highVolatilitySpreadMultiplier;
    private final double minNetEdgeToCommissionRatio;
    private final int signalInvalidationMinHoldSeconds;
    private final int obiInvalidationMinHoldSeconds;
    private final int falseSignalReentryCooldownSeconds;
    private final int obiAnchorRemovalConfirmSeconds;
    private final int absorptionOppositeConfirmSeconds;
    private final boolean levelsEnabled;
    private final int levelsRefreshMinutes;
    private final int levelsLookbackCandles;
    private final double levelZonePercent;
    private final int levelMinTouches;
    private final double levelMinStrengthPercentile;
    private final int levelConsolidationWindow;
    private final double levelConsolidationThreshold;
    private final int levelProximityTicks;
    private final double levelConfidenceBoost;
    private final boolean levelStrictFilter;
    private final boolean levelBasedStops;
    private final boolean levelBasedTakes;

    public OrderFlowScalpingConfig() {
        final Properties properties;
        try {
            properties = PropertiesUtils.loadProperties();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }

        instruments = parseInstruments(properties);
        orderBookDepth = Integer.parseInt(properties.getProperty("orderFlowScalping.orderBookDepth", "10"));
        dailyLossLimitPercent = Double.parseDouble(properties.getProperty("orderFlowScalping.dailyLossLimitPercent", "0.03"));
        maxPositionBalancePercent = Double.parseDouble(properties.getProperty("orderFlowScalping.maxPositionBalancePercent", "0.30"));
        maxTotalPositions = Integer.parseInt(properties.getProperty("orderFlowScalping.maxTotalPositions", "5"));
        maxPositionsPerTicker = Integer.parseInt(properties.getProperty("orderFlowScalping.maxPositionsPerTicker", "2"));
        loopSleepMs = Integer.parseInt(properties.getProperty("orderFlowScalping.loopSleepMs", "100"));
        imbalanceHoldMs = Integer.parseInt(properties.getProperty("orderFlowScalping.imbalanceHoldMs", "300"));
        icebergReplenishMs = Integer.parseInt(properties.getProperty("orderFlowScalping.icebergReplenishMs", "500"));
        spoofFastCancelMs = Integer.parseInt(properties.getProperty("orderFlowScalping.spoofFastCancelMs", "200"));
        baseRiskPercent = Double.parseDouble(properties.getProperty("orderFlowScalping.baseRiskPercent", "0.001"));
        highQualityRiskPercent = Double.parseDouble(properties.getProperty("orderFlowScalping.highQualityRiskPercent", "0.0015"));
        stackedRiskPercent = Double.parseDouble(properties.getProperty("orderFlowScalping.stackedRiskPercent", "0.002"));
        spoofTakeTicks = Integer.parseInt(properties.getProperty("orderFlowScalping.spoofTakeTicks", "7"));
        imbalanceTakeTicksMin = Integer.parseInt(properties.getProperty("orderFlowScalping.imbalanceTakeTicksMin", "4"));
        imbalanceTakeTicksMax = Integer.parseInt(properties.getProperty("orderFlowScalping.imbalanceTakeTicksMax", "6"));
        trailingTicks = Integer.parseInt(properties.getProperty("orderFlowScalping.trailingTicks", "3"));
        icebergTrailingTicks = Integer.parseInt(properties.getProperty("orderFlowScalping.icebergTrailingTicks", "4"));
        absorptionTrailingTicks = Integer.parseInt(properties.getProperty("orderFlowScalping.absorptionTrailingTicks", "5"));
        imbalanceThresholdLevels = Integer.parseInt(properties.getProperty("orderFlowScalping.imbalanceThresholdLevels", "5"));
        imbalanceThreshold = Double.parseDouble(properties.getProperty("orderFlowScalping.imbalanceThreshold", "0.35"));
        icebergMinReplenishments = Integer.parseInt(properties.getProperty("orderFlowScalping.icebergMinReplenishments", "3"));
        icebergTradeToVisibleRatio = Double.parseDouble(properties.getProperty("orderFlowScalping.icebergTradeToVisibleRatio", "5.0"));
        spoofLevelMultiplier = Double.parseDouble(properties.getProperty("orderFlowScalping.spoofLevelMultiplier", "5.0"));
        spoofMinDepthTicks = Integer.parseInt(properties.getProperty("orderFlowScalping.spoofMinDepthTicks", "2"));
        spoofMaxDepthTicks = Integer.parseInt(properties.getProperty("orderFlowScalping.spoofMaxDepthTicks", "10"));
        stackedLevels = Integer.parseInt(properties.getProperty("orderFlowScalping.stackedLevels", "3"));
        stackedVolumeMultiplier = Double.parseDouble(properties.getProperty("orderFlowScalping.stackedVolumeMultiplier", "4.0"));
        breakoutTradeMultiplier = Double.parseDouble(properties.getProperty("orderFlowScalping.breakoutTradeMultiplier", "1.5"));
        timeoutSeconds = Integer.parseInt(properties.getProperty("orderFlowScalping.timeoutSeconds", "30"));
        spoofTimeoutSeconds = Integer.parseInt(properties.getProperty("orderFlowScalping.spoofTimeoutSeconds", "5"));
        absorptionFlatTicks = Integer.parseInt(properties.getProperty("orderFlowScalping.absorptionFlatTicks", "1"));
        localWindowSeconds = Integer.parseInt(properties.getProperty("orderFlowScalping.localWindowSeconds", "30"));
        maxSpreadTicks = Integer.parseInt(properties.getProperty("orderFlowScalping.maxSpreadTicks", "3"));
        bookHistorySeconds = Integer.parseInt(properties.getProperty("orderFlowScalping.bookHistorySeconds", "600"));
        volumeHistorySeconds = Integer.parseInt(properties.getProperty("orderFlowScalping.volumeHistorySeconds", "300"));
        icebergOppositeTicks = Integer.parseInt(properties.getProperty("orderFlowScalping.icebergOppositeTicks", "5"));
        absorptionWindowSeconds = Integer.parseInt(properties.getProperty("orderFlowScalping.absorptionWindowSeconds", "20"));
        stackedPersistenceSeconds = Integer.parseInt(properties.getProperty("orderFlowScalping.stackedPersistenceSeconds", "30"));
        breakoutConfirmationMs = Integer.parseInt(properties.getProperty("orderFlowScalping.breakoutConfirmationMs", "500"));
        lowLiquidityBookVolume = Double.parseDouble(properties.getProperty("orderFlowScalping.lowLiquidityBookVolume", "1000"));
        highVolatilitySpreadMultiplier = Double.parseDouble(properties.getProperty("orderFlowScalping.highVolatilitySpreadMultiplier", "6.0"));
        minNetEdgeToCommissionRatio = Double.parseDouble(properties.getProperty("orderFlowScalping.minNetEdgeToCommissionRatio", "1.10"));
        signalInvalidationMinHoldSeconds = Integer.parseInt(properties.getProperty("orderFlowScalping.signalInvalidationMinHoldSeconds", "8"));
        obiInvalidationMinHoldSeconds = Integer.parseInt(properties.getProperty("orderFlowScalping.obiInvalidationMinHoldSeconds", "15"));
        falseSignalReentryCooldownSeconds = Integer.parseInt(properties.getProperty("orderFlowScalping.falseSignalReentryCooldownSeconds", "20"));
        obiAnchorRemovalConfirmSeconds = Integer.parseInt(properties.getProperty("orderFlowScalping.obiAnchorRemovalConfirmSeconds", "3"));
        absorptionOppositeConfirmSeconds = Integer.parseInt(properties.getProperty("orderFlowScalping.absorptionOppositeConfirmSeconds", "3"));
        levelsEnabled = Boolean.parseBoolean(properties.getProperty("orderFlowScalping.levelsEnabled", "true"));
        levelsRefreshMinutes = Integer.parseInt(properties.getProperty("orderFlowScalping.levelsRefreshMinutes", "15"));
        levelsLookbackCandles = Integer.parseInt(properties.getProperty("orderFlowScalping.levelsLookbackCandles", "200"));
        levelZonePercent = Double.parseDouble(properties.getProperty("orderFlowScalping.levelZonePercent", "0.003"));
        levelMinTouches = Integer.parseInt(properties.getProperty("orderFlowScalping.levelMinTouches", "2"));
        levelMinStrengthPercentile = Double.parseDouble(properties.getProperty("orderFlowScalping.levelMinStrengthPercentile", "0.3"));
        levelConsolidationWindow = Integer.parseInt(properties.getProperty("orderFlowScalping.levelConsolidationWindow", "5"));
        levelConsolidationThreshold = Double.parseDouble(properties.getProperty("orderFlowScalping.levelConsolidationThreshold", "0.003"));
        levelProximityTicks = Integer.parseInt(properties.getProperty("orderFlowScalping.levelProximityTicks", "8"));
        levelConfidenceBoost = Double.parseDouble(properties.getProperty("orderFlowScalping.levelConfidenceBoost", "0.10"));
        levelStrictFilter = Boolean.parseBoolean(properties.getProperty("orderFlowScalping.levelStrictFilter", "false"));
        levelBasedStops = Boolean.parseBoolean(properties.getProperty("orderFlowScalping.levelBasedStops", "true"));
        levelBasedTakes = Boolean.parseBoolean(properties.getProperty("orderFlowScalping.levelBasedTakes", "true"));
    }

    private List<Instrument> parseInstruments(Properties properties) {
        String raw = properties.getProperty("orderFlowScalping.instruments", "").trim();
        if (!raw.isEmpty()) {
            return stream(raw.split(","))
                    .map(String::trim)
                    .filter(it -> !it.isEmpty())
                    .map(this::parseInstrument)
                    .collect(toList());
        }

        List<Instrument> result = new ArrayList<>();
        String stocks = properties.getProperty("orderFlowScalping.stocks", properties.getProperty("datacollector.instruments", ""));
        if (!stocks.trim().isEmpty()) {
            stream(stocks.split(","))
                    .map(String::trim)
                    .filter(it -> !it.isEmpty())
                    .forEach(it -> result.add(new Instrument(it, resolveTickerType(it))));
        }

        String futures = properties.getProperty("orderFlowScalping.features", "RTSI,SBERF,GAZPF,Si,NG,BR");
        if (!futures.trim().isEmpty()) {
            stream(futures.split(","))
                    .map(String::trim)
                    .filter(it -> !it.isEmpty())
                    .forEach(it -> result.add(new Instrument(it, resolveTickerType(it))));
        }
        return result;
    }

    private Instrument parseInstrument(String value) {
        String[] parts = value.split(":");
        if (parts.length == 2) {
            return new Instrument(parts[0].trim(), TickerType.byName(parts[1].trim()));
        }
        String ticker = value.trim();
        return new Instrument(ticker, resolveTickerType(ticker));
    }

    /**
     * Resolves TickerType by ticker name using TickerTypeResolver utility.
     * Searches through ticker repository to find the instrument type.
     */
    private TickerType resolveTickerType(String ticker) {
        return TickerTypeResolver.resolve(ticker, TickerType.FEATURE);
    }

    public List<Instrument> getInstruments() {
        return instruments;
    }

    public int getOrderBookDepth() {
        return orderBookDepth;
    }

    public double getDailyLossLimitPercent() {
        return dailyLossLimitPercent;
    }

    public int getMaxTotalPositions() {
        return maxTotalPositions;
    }

    public double getMaxPositionBalancePercent() {
        return maxPositionBalancePercent;
    }

    public int getMaxPositionsPerTicker() {
        return maxPositionsPerTicker;
    }

    public int getLoopSleepMs() {
        return loopSleepMs;
    }

    public int getImbalanceHoldMs() {
        return imbalanceHoldMs;
    }

    public int getIcebergReplenishMs() {
        return icebergReplenishMs;
    }

    public int getSpoofFastCancelMs() {
        return spoofFastCancelMs;
    }

    public double getBaseRiskPercent() {
        return baseRiskPercent;
    }

    public double getHighQualityRiskPercent() {
        return highQualityRiskPercent;
    }

    public double getStackedRiskPercent() {
        return stackedRiskPercent;
    }

    public int getSpoofTakeTicks() {
        return spoofTakeTicks;
    }

    public int getImbalanceTakeTicksMin() {
        return imbalanceTakeTicksMin;
    }

    public int getImbalanceTakeTicksMax() {
        return imbalanceTakeTicksMax;
    }

    public int getTrailingTicks() {
        return trailingTicks;
    }

    public int getIcebergTrailingTicks() {
        return icebergTrailingTicks;
    }

    public int getAbsorptionTrailingTicks() {
        return absorptionTrailingTicks;
    }

    public int getImbalanceThresholdLevels() {
        return imbalanceThresholdLevels;
    }

    public double getImbalanceThreshold() {
        return imbalanceThreshold;
    }

    public int getIcebergMinReplenishments() {
        return icebergMinReplenishments;
    }

    public double getIcebergTradeToVisibleRatio() {
        return icebergTradeToVisibleRatio;
    }

    public double getSpoofLevelMultiplier() {
        return spoofLevelMultiplier;
    }

    public int getSpoofMinDepthTicks() {
        return spoofMinDepthTicks;
    }

    public int getSpoofMaxDepthTicks() {
        return spoofMaxDepthTicks;
    }

    public int getStackedLevels() {
        return stackedLevels;
    }

    public double getStackedVolumeMultiplier() {
        return stackedVolumeMultiplier;
    }

    public double getBreakoutTradeMultiplier() {
        return breakoutTradeMultiplier;
    }

    public int getTimeoutSeconds() {
        return timeoutSeconds;
    }

    public int getSpoofTimeoutSeconds() {
        return spoofTimeoutSeconds;
    }

    public int getAbsorptionFlatTicks() {
        return absorptionFlatTicks;
    }

    public int getLocalWindowSeconds() {
        return localWindowSeconds;
    }

    public int getMaxSpreadTicks() {
        return maxSpreadTicks;
    }

    public int getBookHistorySeconds() {
        return bookHistorySeconds;
    }

    public int getVolumeHistorySeconds() {
        return volumeHistorySeconds;
    }

    public int getIcebergOppositeTicks() {
        return icebergOppositeTicks;
    }

    public int getAbsorptionWindowSeconds() {
        return absorptionWindowSeconds;
    }

    public int getStackedPersistenceSeconds() {
        return stackedPersistenceSeconds;
    }

    public int getBreakoutConfirmationMs() {
        return breakoutConfirmationMs;
    }

    public double getLowLiquidityBookVolume() {
        return lowLiquidityBookVolume;
    }

    public double getHighVolatilitySpreadMultiplier() {
        return highVolatilitySpreadMultiplier;
    }

    public double getMinNetEdgeToCommissionRatio() {
        return minNetEdgeToCommissionRatio;
    }

    public int getSignalInvalidationMinHoldSeconds() {
        return signalInvalidationMinHoldSeconds;
    }

    public int getObiInvalidationMinHoldSeconds() {
        return obiInvalidationMinHoldSeconds;
    }

    public int getFalseSignalReentryCooldownSeconds() {
        return falseSignalReentryCooldownSeconds;
    }

    public int getObiAnchorRemovalConfirmSeconds() {
        return obiAnchorRemovalConfirmSeconds;
    }

    public int getAbsorptionOppositeConfirmSeconds() {
        return absorptionOppositeConfirmSeconds;
    }

    public boolean isLevelsEnabled() { return levelsEnabled; }
    public int getLevelsRefreshMinutes() { return levelsRefreshMinutes; }
    public int getLevelsLookbackCandles() { return levelsLookbackCandles; }
    public double getLevelZonePercent() { return levelZonePercent; }
    public int getLevelMinTouches() { return levelMinTouches; }
    public double getLevelMinStrengthPercentile() { return levelMinStrengthPercentile; }
    public int getLevelConsolidationWindow() { return levelConsolidationWindow; }
    public double getLevelConsolidationThreshold() { return levelConsolidationThreshold; }
    public int getLevelProximityTicks() { return levelProximityTicks; }
    public double getLevelConfidenceBoost() { return levelConfidenceBoost; }
    public boolean isLevelStrictFilter() { return levelStrictFilter; }
    public boolean isLevelBasedStops() { return levelBasedStops; }
    public boolean isLevelBasedTakes() { return levelBasedTakes; }
}
