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
  private final int idleRescreenSeconds;
  private final double commissionRate;
  private final double targetFeeMultiple;
  private final double stopFeeMultiple;
  private final double expectedWinRate;
  private final double evGateBuffer;
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
  private final boolean swingMode;
  private final double trailingActivationBps;
  private final double trailingStepBps;
  private final boolean shortsEnabled;
  private final boolean riskManagementEnabled;
  private final double riskPerTradePercent;
  private final double maxDailyLossPercent;
  private final int maxConsecutiveLosses;
  private final double criticalDrawdownPercent;
  private final boolean diagnosticsEnabled;
  private final boolean diagnosticsSummaryEnabled;
  private final boolean diagnosticsReplayEnabled;
  private final String diagnosticsReplayFile;
  private final String positionStateFile;

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
        Double.parseDouble(properties.getProperty("orderBookScalp.obiThreshold", "0.30"));
    this.edgeSpreadFraction =
        Double.parseDouble(properties.getProperty("orderBookScalp.edgeSpreadFraction", "0.3"));
    this.maxSpreadBps =
        Double.parseDouble(properties.getProperty("orderBookScalp.maxSpreadBps", "15"));
    this.persistenceTicks =
        Integer.parseInt(properties.getProperty("orderBookScalp.persistenceTicks", "3"));
    this.obiLevels = Integer.parseInt(properties.getProperty("orderBookScalp.obiLevels", "5"));
    this.minBestLevelQty =
        Integer.parseInt(properties.getProperty("orderBookScalp.minBestLevelQty", "1"));
    this.takeProfitSpreads =
        Double.parseDouble(properties.getProperty("orderBookScalp.takeProfitSpreads", "2.5"));
    this.stopLossSpreads =
        Double.parseDouble(properties.getProperty("orderBookScalp.stopLossSpreads", "1.25"));
    this.maxHoldSeconds =
        Integer.parseInt(properties.getProperty("orderBookScalp.maxHoldSeconds", "90"));
    this.cooldownSeconds =
        Integer.parseInt(properties.getProperty("orderBookScalp.cooldownSeconds", "30"));
    this.tradeFlowWindowSeconds =
        Integer.parseInt(properties.getProperty("orderBookScalp.tradeFlowWindowSeconds", "5"));
    this.screeningTopN =
        Integer.parseInt(properties.getProperty("orderBookScalp.screeningTopN", "30"));
    this.rescreenMinutes =
        Integer.parseInt(properties.getProperty("orderBookScalp.rescreenMinutes", "60"));
    this.idleRescreenSeconds =
        Integer.parseInt(properties.getProperty("orderBookScalp.idleRescreenSeconds", "60"));
    this.commissionRate =
        Double.parseDouble(properties.getProperty("orderBookScalp.commissionRate", "0.0005"));
    this.targetFeeMultiple =
        Double.parseDouble(properties.getProperty("orderBookScalp.targetFeeMultiple", "2.0"));
    this.stopFeeMultiple =
        Double.parseDouble(properties.getProperty("orderBookScalp.stopFeeMultiple", "0.75"));
    this.expectedWinRate =
        Double.parseDouble(properties.getProperty("orderBookScalp.expectedWinRate", "0.55"));
    this.evGateBuffer =
        Double.parseDouble(properties.getProperty("orderBookScalp.evGateBuffer", "1.0"));
    this.obiExitThreshold =
        Double.parseDouble(properties.getProperty("orderBookScalp.obiExitThreshold", "-0.25"));
    this.entryGraceSeconds =
        Integer.parseInt(properties.getProperty("orderBookScalp.entryGraceSeconds", "10"));
    this.minTradeFlow =
        Double.parseDouble(properties.getProperty("orderBookScalp.minTradeFlow", "5"));
    this.screeningMinTopDepth =
        Integer.parseInt(properties.getProperty("orderBookScalp.screeningMinTopDepth", "40"));
    this.screeningMinBookDepth =
        Integer.parseInt(properties.getProperty("orderBookScalp.screeningMinBookDepth", "150"));
    this.screeningBookLevels =
        Integer.parseInt(properties.getProperty("orderBookScalp.screeningBookLevels", "5"));
    this.screeningNearestContracts =
        Integer.parseInt(properties.getProperty("orderBookScalp.screeningNearestContracts", "3"));
    this.enabledSignals =
        stream(
                properties
                    .getProperty("orderBookScalp.enabledSignals", "obi,tradeFlow,microprice")
                    .split(","))
            .map(String::trim)
            .filter(s -> !s.isEmpty())
            .collect(toList());
    this.trailingEnabled =
        Boolean.parseBoolean(properties.getProperty("orderBookScalp.trailingEnabled", "true"));
    this.trailingActivationSpreads =
        Double.parseDouble(
            properties.getProperty("orderBookScalp.trailingActivationSpreads", "1.0"));
    this.trailingStepSpreads =
        Double.parseDouble(properties.getProperty("orderBookScalp.trailingStepSpreads", "0.5"));
    this.swingMode =
        Boolean.parseBoolean(properties.getProperty("orderBookScalp.swingMode", "true"));
    this.trailingActivationBps =
        Double.parseDouble(properties.getProperty("orderBookScalp.trailingActivationBps", "30"));
    this.trailingStepBps =
        Double.parseDouble(properties.getProperty("orderBookScalp.trailingStepBps", "15"));
    this.shortsEnabled =
        Boolean.parseBoolean(properties.getProperty("orderBookScalp.shortsEnabled", "false"));
    this.riskManagementEnabled =
        Boolean.parseBoolean(
            properties.getProperty("orderBookScalp.riskManagementEnabled", "true"));
    this.riskPerTradePercent =
        Double.parseDouble(properties.getProperty("orderBookScalp.riskPerTradePercent", "0.01"));
    this.maxDailyLossPercent =
        Double.parseDouble(properties.getProperty("orderBookScalp.maxDailyLossPercent", "0.03"));
    this.maxConsecutiveLosses =
        Integer.parseInt(properties.getProperty("orderBookScalp.maxConsecutiveLosses", "3"));
    this.criticalDrawdownPercent =
        Double.parseDouble(
            properties.getProperty("orderBookScalp.criticalDrawdownPercent", "0.10"));
    this.diagnosticsEnabled =
        Boolean.parseBoolean(properties.getProperty("orderBookScalp.diagnosticsEnabled", "false"));
    this.diagnosticsSummaryEnabled =
        Boolean.parseBoolean(
            properties.getProperty("orderBookScalp.diagnosticsSummaryEnabled", "false"));
    this.diagnosticsReplayEnabled =
        Boolean.parseBoolean(
            properties.getProperty("orderBookScalp.diagnosticsReplayEnabled", "false"));
    this.diagnosticsReplayFile =
        properties.getProperty(
            "orderBookScalp.diagnosticsReplayFile", "build/orderbook-diagnostics-replay.log");
    this.positionStateFile =
        properties.getProperty("orderBookScalp.positionStateFile", "data/orderbook-positions.json");
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

  public int getIdleRescreenSeconds() {
    return idleRescreenSeconds;
  }

  public double getCommissionRate() {
    return commissionRate;
  }

  public double getTargetFeeMultiple() {
    return targetFeeMultiple;
  }

  public double getStopFeeMultiple() {
    return stopFeeMultiple;
  }

  public double getExpectedWinRate() {
    return expectedWinRate;
  }

  public double getEvGateBuffer() {
    return evGateBuffer;
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

  public boolean isSwingMode() {
    return swingMode;
  }

  public double getTrailingActivationBps() {
    return trailingActivationBps;
  }

  public double getTrailingStepBps() {
    return trailingStepBps;
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

  public boolean isDiagnosticsEnabled() {
    return diagnosticsEnabled;
  }

  public boolean isDiagnosticsSummaryEnabled() {
    return diagnosticsSummaryEnabled;
  }

  public boolean isDiagnosticsReplayEnabled() {
    return diagnosticsReplayEnabled;
  }

  public String getDiagnosticsReplayFile() {
    return diagnosticsReplayFile;
  }

  public String getPositionStateFile() {
    return positionStateFile;
  }
}
