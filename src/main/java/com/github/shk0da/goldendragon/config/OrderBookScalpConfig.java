package com.github.shk0da.goldendragon.config;

import static java.util.Arrays.stream;
import static java.util.stream.Collectors.toList;

import com.github.shk0da.goldendragon.utils.PropertiesUtils;
import java.io.IOException;
import java.util.List;
import java.util.Properties;

/** Configuration for order-book scalping strategy (OFIS). */
public class OrderBookScalpConfig {

    private final List<String> instruments;
    private final int depth;
    private final boolean paperMode;
    private final double positionCash;
    private final double obiThreshold;
    private final double edgeSpreadFraction;
    private final double maxSpreadBps;
    private final int persistenceTicks;
    private final int obiLevels;
    private final int minBestLevelQty;
    private final double takeProfitSpreads;
    private final double stopLossSpreads;
    private final int maxHoldSeconds;
    private final int cooldownSeconds;
    private final int tradeFlowWindowSeconds;
    private final int screeningTopN;
    private final int rescreenMinutes;
    private final double commissionRate;
    private final double obiExitThreshold;
    private final int entryGraceSeconds;
    private final double minTradeFlow;
    private final int screeningMinTopDepth;
    private final int screeningMinBookDepth;
    private final int screeningBookLevels;
    private final int screeningNearestContracts;
    private final List<String> enabledSignals;
    private final boolean trailingEnabled;
    private final double trailingActivationSpreads;
    private final double trailingStepSpreads;
    private final boolean shortsEnabled;
    private final boolean riskManagementEnabled;
    private final double riskPerTradePercent;
    private final double maxDailyLossPercent;
    private final int maxConsecutiveLosses;
    private final double criticalDrawdownPercent;

    public OrderBookScalpConfig() {
        final Properties properties;
        try {
            properties = PropertiesUtils.loadProperties();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }

        this.instruments =
                stream(properties.getProperty("orderBookScalp.instruments", "ALL").split(","))
                        .map(String::trim)
                        .filter(s -> !s.isEmpty())
                        .collect(toList());
        this.depth = Integer.parseInt(properties.getProperty("orderBookScalp.depth", "10"));
        this.paperMode =
                Boolean.parseBoolean(properties.getProperty("orderBookScalp.paperMode", "true"));
        this.positionCash =
                Double.parseDouble(properties.getProperty("orderBookScalp.positionCash", "50000"));
        this.obiThreshold =
                Double.parseDouble(properties.getProperty("orderBookScalp.obiThreshold", "0.35"));
        this.edgeSpreadFraction =
                Double.parseDouble(
                        properties.getProperty("orderBookScalp.edgeSpreadFraction", "0.3"));
        this.maxSpreadBps =
                Double.parseDouble(properties.getProperty("orderBookScalp.maxSpreadBps", "15"));
        this.persistenceTicks =
                Integer.parseInt(properties.getProperty("orderBookScalp.persistenceTicks", "3"));
        this.obiLevels = Integer.parseInt(properties.getProperty("orderBookScalp.obiLevels", "5"));
        this.minBestLevelQty =
                Integer.parseInt(properties.getProperty("orderBookScalp.minBestLevelQty", "1"));
        this.takeProfitSpreads =
                Double.parseDouble(
                        properties.getProperty("orderBookScalp.takeProfitSpreads", "2.0"));
        this.stopLossSpreads =
                Double.parseDouble(properties.getProperty("orderBookScalp.stopLossSpreads", "1.0"));
        this.maxHoldSeconds =
                Integer.parseInt(properties.getProperty("orderBookScalp.maxHoldSeconds", "90"));
        this.cooldownSeconds =
                Integer.parseInt(properties.getProperty("orderBookScalp.cooldownSeconds", "30"));
        this.tradeFlowWindowSeconds =
                Integer.parseInt(
                        properties.getProperty("orderBookScalp.tradeFlowWindowSeconds", "5"));
        this.screeningTopN =
                Integer.parseInt(properties.getProperty("orderBookScalp.screeningTopN", "30"));
        this.rescreenMinutes =
                Integer.parseInt(properties.getProperty("orderBookScalp.rescreenMinutes", "60"));
        this.commissionRate =
                Double.parseDouble(
                        properties.getProperty("orderBookScalp.commissionRate", "0.0005"));
        this.obiExitThreshold =
                Double.parseDouble(
                        properties.getProperty("orderBookScalp.obiExitThreshold", "-0.25"));
        this.entryGraceSeconds =
                Integer.parseInt(properties.getProperty("orderBookScalp.entryGraceSeconds", "3"));
        this.minTradeFlow =
                Double.parseDouble(properties.getProperty("orderBookScalp.minTradeFlow", "10"));
        this.screeningMinTopDepth =
                Integer.parseInt(
                        properties.getProperty("orderBookScalp.screeningMinTopDepth", "40"));
        this.screeningMinBookDepth =
                Integer.parseInt(
                        properties.getProperty("orderBookScalp.screeningMinBookDepth", "150"));
        this.screeningBookLevels =
                Integer.parseInt(properties.getProperty("orderBookScalp.screeningBookLevels", "5"));
        this.screeningNearestContracts =
                Integer.parseInt(
                        properties.getProperty("orderBookScalp.screeningNearestContracts", "3"));
        this.enabledSignals =
                stream(
                                properties
                                        .getProperty(
                                                "orderBookScalp.enabledSignals",
                                                "obi,tradeFlow,microprice")
                                        .split(","))
                        .map(String::trim)
                        .filter(s -> !s.isEmpty())
                        .collect(toList());
        this.trailingEnabled =
                Boolean.parseBoolean(
                        properties.getProperty("orderBookScalp.trailingEnabled", "true"));
        this.trailingActivationSpreads =
                Double.parseDouble(
                        properties.getProperty("orderBookScalp.trailingActivationSpreads", "1.0"));
        this.trailingStepSpreads =
                Double.parseDouble(
                        properties.getProperty("orderBookScalp.trailingStepSpreads", "0.5"));
        this.shortsEnabled =
                Boolean.parseBoolean(
                        properties.getProperty("orderBookScalp.shortsEnabled", "false"));
        this.riskManagementEnabled =
                Boolean.parseBoolean(
                        properties.getProperty("orderBookScalp.riskManagementEnabled", "true"));
        this.riskPerTradePercent =
                Double.parseDouble(
                        properties.getProperty("orderBookScalp.riskPerTradePercent", "0.01"));
        this.maxDailyLossPercent =
                Double.parseDouble(
                        properties.getProperty("orderBookScalp.maxDailyLossPercent", "0.03"));
        this.maxConsecutiveLosses =
                Integer.parseInt(
                        properties.getProperty("orderBookScalp.maxConsecutiveLosses", "3"));
        this.criticalDrawdownPercent =
                Double.parseDouble(
                        properties.getProperty("orderBookScalp.criticalDrawdownPercent", "0.10"));
    }

    public List<String> getInstruments() {
        return instruments;
    }

    public int getDepth() {
        return depth;
    }

    public boolean isPaperMode() {
        return paperMode;
    }

    public double getPositionCash() {
        return positionCash;
    }

    public double getObiThreshold() {
        return obiThreshold;
    }

    public double getEdgeSpreadFraction() {
        return edgeSpreadFraction;
    }

    public double getMaxSpreadBps() {
        return maxSpreadBps;
    }

    public int getPersistenceTicks() {
        return persistenceTicks;
    }

    public int getObiLevels() {
        return obiLevels;
    }

    public int getMinBestLevelQty() {
        return minBestLevelQty;
    }

    public double getTakeProfitSpreads() {
        return takeProfitSpreads;
    }

    public double getStopLossSpreads() {
        return stopLossSpreads;
    }

    public int getMaxHoldSeconds() {
        return maxHoldSeconds;
    }

    public int getCooldownSeconds() {
        return cooldownSeconds;
    }

    public int getTradeFlowWindowSeconds() {
        return tradeFlowWindowSeconds;
    }

    public int getScreeningTopN() {
        return screeningTopN;
    }

    public int getRescreenMinutes() {
        return rescreenMinutes;
    }

    public double getCommissionRate() {
        return commissionRate;
    }

    public double getObiExitThreshold() {
        return obiExitThreshold;
    }

    public int getEntryGraceSeconds() {
        return entryGraceSeconds;
    }

    public double getMinTradeFlow() {
        return minTradeFlow;
    }

    public int getScreeningMinTopDepth() {
        return screeningMinTopDepth;
    }

    public int getScreeningMinBookDepth() {
        return screeningMinBookDepth;
    }

    public int getScreeningBookLevels() {
        return screeningBookLevels;
    }

    public int getScreeningNearestContracts() {
        return screeningNearestContracts;
    }

    public List<String> getEnabledSignals() {
        return enabledSignals;
    }

    public boolean isTrailingEnabled() {
        return trailingEnabled;
    }

    public double getTrailingActivationSpreads() {
        return trailingActivationSpreads;
    }

    public double getTrailingStepSpreads() {
        return trailingStepSpreads;
    }

    public boolean isShortsEnabled() {
        return shortsEnabled;
    }

    public boolean isRiskManagementEnabled() {
        return riskManagementEnabled;
    }

    public double getRiskPerTradePercent() {
        return riskPerTradePercent;
    }

    public double getMaxDailyLossPercent() {
        return maxDailyLossPercent;
    }

    public int getMaxConsecutiveLosses() {
        return maxConsecutiveLosses;
    }

    public double getCriticalDrawdownPercent() {
        return criticalDrawdownPercent;
    }
}
