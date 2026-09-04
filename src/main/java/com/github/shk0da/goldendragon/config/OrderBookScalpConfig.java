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
  private final boolean closeUntrackedPositions;
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
  private final double futuresCommissionPerContract;
  private final double minScreeningTradeFlow;
  private final double minEconomicsRatio;
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
  private final boolean metricsCsvEnabled;
  private final String metricsCsvFile;
  private final String positionStateFile;
  private final boolean volatilitySizingEnabled;
  private final double volatilityTargetSpreadBps;
  private final double volatilityMinMultiplier;
  private final double volatilityMaxMultiplier;
  private final boolean timeOfDayFilterEnabled;
  private final double morningLiquidityMultiplier;
  private final double lunchLiquidityMultiplier;
  private final double eveningLiquidityMultiplier;
  private final boolean dynamicTpSlEnabled;
  private final double atrTpMultiplier;
  private final double atrSlMultiplier;
  private final int trendMomentumWindow;
  private final int trendFlowWindow;
  private final double trendMinMomentumRatio;
  private final double trendMinFlowAccumulation;
  
  // HFT scalping spec parameters (TODO.md Section 2)
  private final double tickSize;
  private final double lotStep;
  private final int volLookbackHours;
  private final int clusterTicks;
  private final double fadeRatio;
  private final double accelRatio;
  private final double densityPullExit;
  private final boolean serverStopEnabled;
  private final long orderTimeoutMs;
  
  // Additional HFT parameters (TODO.md compliance)
  private final int deltaBarsLookback;      // TODO.md 3.1: number of 10s bars for delta analysis
  private final int stickBars;              // TODO.md 3.3: bars for stickiness detection
  private final int minNetProfitTicks;      // TODO.md 2: minimum net profit in ticks for entry
  private final double eatenRatioEntry;     // TODO.md 2: density consumption ratio for breakout
  
  // DensityScalpStrategy parameters
  private final boolean stocksEnabled;
  private final int leaderLagSeconds;
  private final double basisAnomalySigma;
  private final boolean divergenceBlockEnabled;
  private final int trendTimeframeMinutes;
  private final int trendLookbackCandles;
  private final long trendCacheTtlMs;
  private final double minLevelVolumeRatio;
  private final int maxLevelAgeMinutes;
  private final double levelPriceToleranceBps;
  private final double compressionSpreadBps;
  private final double compressionVolumeMultiplier;
  private final double compressionProximityBps;
  private final int compressionHistorySize;
  private final int microImpulseMinTrades;
  private final int microImpulseWindowMs;
  private final double microImpulseVolumeMultiplier;

  // Enhanced engine components config (subtask 14)
  private final int regimeAtrPeriod;
  private final int regimeAdxPeriod;
  private final double regimeAdxTrendThreshold;
  private final double regimeAtrVolatilityMultiplier;
  private final int tapeWindowSize;
  private final double tapeBlockMultiplier;
  private final int vpinBucketSize;
  private final int vpinBucketHistorySize;
  private final int vpinTradesPerBucket;
  private final double maxVpinEntry;
  private final int minVpinBuckets;
  private final double entryQualityThreshold;
  private final boolean trendFilterEnabled;
  private final boolean regimeFilterEnabled;
  private final long volumeProfileWindowMillis;
  private final int queueHistoryWindow;
  private final double queuePriceToleranceBps;
  private final double queueFastFillThreshold;
  private final int signalPerfWindowSize;
  private final double dynamicTpMaxDistanceBps;
  private final double dynamicTpMinDistanceBps;
  private final boolean correlationFilterEnabled;
  private final double correlationThreshold;
  private final int correlationReturnWindow;
  private final boolean volSpikeFilterEnabled;
  private final double volSpikeSpreadMultiplier;
  private final double volSpikeVolumeMultiplier;
  private final int volSpikeCooldownMs;
  private final int volSpikeLookbackPeriod;
  private final boolean adaptiveParamsEnabled;
  private final int slippageWindowSize;
  private final double slippageWarningThresholdTicks;
  private final long partialFillTimeoutMs;
  private final int partialFillMaxResubmitAttempts;
  private final long blocklistDurationMs;

  public OrderBookScalpConfig() {
    final Properties properties;
    try {
      properties = PropertiesUtils.loadProperties();
    } catch (IOException e) {
      throw new RuntimeException(e);
    }

    this.instruments =
        stream(properties.getProperty("orderBookScalp.instruments", "IMOEXF,CNYRUBF").split(","))
            .map(String::trim)
            .filter(s -> !s.isEmpty())
            .collect(toList());
    this.depth = Integer.parseInt(properties.getProperty("orderBookScalp.depth", "10"));
    this.paperMode =
        Boolean.parseBoolean(properties.getProperty("orderBookScalp.paperMode", "false"));
    // off by default: closing foreign positions is destructive when several
    // strategies share one brokerage account
    this.closeUntrackedPositions =
        Boolean.parseBoolean(properties.getProperty("orderBookScalp.closeUntrackedPositions", "false"));
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
        Integer.parseInt(properties.getProperty("orderBookScalp.screeningTopN", "10"));
    this.rescreenMinutes =
        Integer.parseInt(properties.getProperty("orderBookScalp.rescreenMinutes", "60"));
    this.idleRescreenSeconds =
        Integer.parseInt(properties.getProperty("orderBookScalp.idleRescreenSeconds", "60"));
    this.commissionRate =
        Double.parseDouble(properties.getProperty("orderBookScalp.commissionRate", "0.0005"));
    this.futuresCommissionPerContract =
        Double.parseDouble(
            properties.getProperty("orderBookScalp.futuresCommissionPerContract", "4.0"));
    this.minScreeningTradeFlow =
        Double.parseDouble(
            properties.getProperty("orderBookScalp.minScreeningTradeFlow", "20.0"));
    this.minEconomicsRatio =
        Double.parseDouble(
            properties.getProperty("orderBookScalp.minEconomicsRatio", "1.0"));
        this.targetFeeMultiple =
            Double.parseDouble(properties.getProperty("orderBookScalp.targetFeeMultiple", "4.0"));
    this.stopFeeMultiple =
        Double.parseDouble(properties.getProperty("orderBookScalp.stopFeeMultiple", "1.0"));
    this.expectedWinRate =
        Double.parseDouble(properties.getProperty("orderBookScalp.expectedWinRate", "0.60"));
    this.evGateBuffer =
        Double.parseDouble(properties.getProperty("orderBookScalp.evGateBuffer", "0.8"));
    this.obiExitThreshold =
        Double.parseDouble(properties.getProperty("orderBookScalp.obiExitThreshold", "-0.25"));
    this.entryGraceSeconds =
        Integer.parseInt(properties.getProperty("orderBookScalp.entryGraceSeconds", "10"));
    this.minTradeFlow =
        Double.parseDouble(properties.getProperty("orderBookScalp.minTradeFlow", "2.5"));
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
                    .getProperty("orderBookScalp.enabledSignals", "obi,tradeFlow,microprice,density")
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
        Boolean.parseBoolean(properties.getProperty("orderBookScalp.diagnosticsEnabled", "true"));
    this.diagnosticsSummaryEnabled =
        Boolean.parseBoolean(
            properties.getProperty("orderBookScalp.diagnosticsSummaryEnabled", "true"));
    this.diagnosticsReplayEnabled =
        Boolean.parseBoolean(
            properties.getProperty("orderBookScalp.diagnosticsReplayEnabled", "true"));
    this.diagnosticsReplayFile =
        properties.getProperty(
            "orderBookScalp.diagnosticsReplayFile", "analytics/orderbook-diagnostics-replay.log");
    this.metricsCsvEnabled =
        Boolean.parseBoolean(
            properties.getProperty("orderBookScalp.metricsCsvEnabled", "true"));
    this.metricsCsvFile =
        properties.getProperty(
            "orderBookScalp.metricsCsvFile", "analytics/orderbook-metrics.csv");
    this.positionStateFile =
        properties.getProperty("orderBookScalp.positionStateFile", "data/orderbook-positions.json");

    // Volatility-adjusted position sizing
    this.volatilitySizingEnabled =
        Boolean.parseBoolean(
            properties.getProperty("orderBookScalp.volatilitySizingEnabled", "true"));
    this.volatilityTargetSpreadBps =
        Double.parseDouble(
            properties.getProperty("orderBookScalp.volatilityTargetSpreadBps", "5.0"));
    this.volatilityMinMultiplier =
        Double.parseDouble(
            properties.getProperty("orderBookScalp.volatilityMinMultiplier", "0.5"));
    this.volatilityMaxMultiplier =
        Double.parseDouble(
            properties.getProperty("orderBookScalp.volatilityMaxMultiplier", "2.0"));

    // Time-of-day liquidity filter
    this.timeOfDayFilterEnabled =
        Boolean.parseBoolean(
            properties.getProperty("orderBookScalp.timeOfDayFilterEnabled", "true"));
    this.morningLiquidityMultiplier =
        Double.parseDouble(
            properties.getProperty("orderBookScalp.morningLiquidityMultiplier", "1.5"));
    this.lunchLiquidityMultiplier =
        Double.parseDouble(
            properties.getProperty("orderBookScalp.lunchLiquidityMultiplier", "0.6"));
    this.eveningLiquidityMultiplier =
        Double.parseDouble(
            properties.getProperty("orderBookScalp.eveningLiquidityMultiplier", "0.8"));

    // Dynamic TP/SL based on spread volatility
    this.dynamicTpSlEnabled =
        Boolean.parseBoolean(
            properties.getProperty("orderBookScalp.dynamicTpSlEnabled", "true"));
    this.atrTpMultiplier =
        Double.parseDouble(
            properties.getProperty("orderBookScalp.atrTpMultiplier", "1.5"));
    this.atrSlMultiplier =
        Double.parseDouble(
            properties.getProperty("orderBookScalp.atrSlMultiplier", "1.0"));

    // Trend filter parameters
    this.trendMomentumWindow =
        Integer.parseInt(properties.getProperty("orderBookScalp.trendMomentumWindow", "5"));
    this.trendFlowWindow =
        Integer.parseInt(properties.getProperty("orderBookScalp.trendFlowWindow", "3"));
    this.trendMinMomentumRatio =
        Double.parseDouble(
            properties.getProperty("orderBookScalp.trendMinMomentumRatio", "0.1"));
    this.trendMinFlowAccumulation =
        Double.parseDouble(
            properties.getProperty("orderBookScalp.trendMinFlowAccumulation", "1.0"));
    
    // HFT scalping spec parameters (TODO.md Section 2)
    this.tickSize =
        Double.parseDouble(properties.getProperty("orderBookScalp.tickSize", "0.0"));
    this.lotStep =
        Double.parseDouble(properties.getProperty("orderBookScalp.lotStep", "1.0"));
    this.volLookbackHours =
        Integer.parseInt(properties.getProperty("orderBookScalp.volLookbackHours", "2"));
    this.clusterTicks =
        Integer.parseInt(properties.getProperty("orderBookScalp.clusterTicks", "3"));
    this.fadeRatio =
        Double.parseDouble(properties.getProperty("orderBookScalp.fadeRatio", "0.3"));
    this.accelRatio =
        Double.parseDouble(properties.getProperty("orderBookScalp.accelRatio", "1.5"));
    this.densityPullExit =
        Double.parseDouble(properties.getProperty("orderBookScalp.densityPullExit", "0.5"));
    this.serverStopEnabled =
        Boolean.parseBoolean(properties.getProperty("orderBookScalp.serverStopEnabled", "true"));
    this.orderTimeoutMs =
        Long.parseLong(properties.getProperty("orderBookScalp.orderTimeoutMs", "5000"));
    
    // Additional HFT parameters (TODO.md compliance)
    this.deltaBarsLookback =
        Integer.parseInt(properties.getProperty("orderBookScalp.deltaBarsLookback", "2"));
    this.stickBars =
        Integer.parseInt(properties.getProperty("orderBookScalp.stickBars", "2"));
    this.minNetProfitTicks =
        Integer.parseInt(properties.getProperty("orderBookScalp.minNetProfitTicks", "2"));
    this.eatenRatioEntry =
        Double.parseDouble(properties.getProperty("orderBookScalp.eatenRatioEntry", "0.75"));
    
    // DensityScalpStrategy parameters
    this.stocksEnabled =
        Boolean.parseBoolean(properties.getProperty("orderBookScalp.stocksEnabled", "true"));
    this.leaderLagSeconds =
        Integer.parseInt(properties.getProperty("orderBookScalp.leaderLagSeconds", "4"));
    this.basisAnomalySigma =
        Double.parseDouble(properties.getProperty("orderBookScalp.basisAnomalySigma", "2.0"));
    this.divergenceBlockEnabled =
        Boolean.parseBoolean(properties.getProperty("orderBookScalp.divergenceBlockEnabled", "true"));
    this.trendTimeframeMinutes =
        Integer.parseInt(properties.getProperty("orderBookScalp.trendTimeframeMinutes", "5"));
    this.trendLookbackCandles =
        Integer.parseInt(properties.getProperty("orderBookScalp.trendLookbackCandles", "20"));
    this.trendCacheTtlMs =
        Long.parseLong(properties.getProperty("orderBookScalp.trendCacheTtlMs", "60000"));
    this.minLevelVolumeRatio =
        Double.parseDouble(properties.getProperty("orderBookScalp.minLevelVolumeRatio", "3.0"));
    this.maxLevelAgeMinutes =
        Integer.parseInt(properties.getProperty("orderBookScalp.maxLevelAgeMinutes", "30"));
    this.levelPriceToleranceBps =
        Double.parseDouble(properties.getProperty("orderBookScalp.levelPriceToleranceBps", "5.0"));
    this.compressionSpreadBps =
        Double.parseDouble(properties.getProperty("orderBookScalp.compressionSpreadBps", "3.0"));
    this.compressionVolumeMultiplier =
        Double.parseDouble(properties.getProperty("orderBookScalp.compressionVolumeMultiplier", "2.0"));
    this.compressionProximityBps =
        Double.parseDouble(properties.getProperty("orderBookScalp.compressionProximityBps", "10.0"));
    this.compressionHistorySize =
        Integer.parseInt(properties.getProperty("orderBookScalp.compressionHistorySize", "50"));
    this.microImpulseMinTrades =
        Integer.parseInt(properties.getProperty("orderBookScalp.microImpulseMinTrades", "5"));
    this.microImpulseWindowMs =
        Integer.parseInt(properties.getProperty("orderBookScalp.microImpulseWindowMs", "2000"));
    this.microImpulseVolumeMultiplier =
        Double.parseDouble(properties.getProperty("orderBookScalp.microImpulseVolumeMultiplier", "2.0"));

    // Enhanced engine components config (subtask 14)
    this.regimeAtrPeriod =
        Integer.parseInt(properties.getProperty("orderBookScalp.regimeAtrPeriod", "14"));
    this.regimeAdxPeriod =
        Integer.parseInt(properties.getProperty("orderBookScalp.regimeAdxPeriod", "14"));
    this.regimeAdxTrendThreshold =
        Double.parseDouble(properties.getProperty("orderBookScalp.regimeAdxTrendThreshold", "25.0"));
    this.regimeAtrVolatilityMultiplier =
        Double.parseDouble(properties.getProperty("orderBookScalp.regimeAtrVolatilityMultiplier", "1.5"));
    this.tapeWindowSize =
        Integer.parseInt(properties.getProperty("orderBookScalp.tapeWindowSize", "200"));
    this.tapeBlockMultiplier =
        Double.parseDouble(properties.getProperty("orderBookScalp.tapeBlockMultiplier", "3.0"));
    this.vpinBucketSize =
        Integer.parseInt(properties.getProperty("orderBookScalp.vpinBucketSize", "50"));
    this.vpinBucketHistorySize =
        Integer.parseInt(properties.getProperty("orderBookScalp.vpinBucketHistorySize", "20"));
    this.vpinTradesPerBucket =
        Integer.parseInt(properties.getProperty("orderBookScalp.vpinTradesPerBucket", "25"));
    this.maxVpinEntry =
        Double.parseDouble(properties.getProperty("orderBookScalp.maxVpinEntry", "0.70"));
    this.minVpinBuckets =
        Integer.parseInt(properties.getProperty("orderBookScalp.minVpinBuckets", "5"));
    this.entryQualityThreshold =
        Double.parseDouble(properties.getProperty("orderBookScalp.entryQualityThreshold", "0.30"));
    // signals enforce their own trend alignment; engine-level gates are optional
    this.trendFilterEnabled =
        Boolean.parseBoolean(properties.getProperty("orderBookScalp.trendFilterEnabled", "false"));
    this.regimeFilterEnabled =
        Boolean.parseBoolean(properties.getProperty("orderBookScalp.regimeFilterEnabled", "false"));
    this.volumeProfileWindowMillis =
        Long.parseLong(properties.getProperty("orderBookScalp.volumeProfileWindowMillis", "300000"));
    this.queueHistoryWindow =
        Integer.parseInt(properties.getProperty("orderBookScalp.queueHistoryWindow", "50"));
    this.queuePriceToleranceBps =
        Double.parseDouble(properties.getProperty("orderBookScalp.queuePriceToleranceBps", "5.0"));
    this.queueFastFillThreshold =
        Double.parseDouble(properties.getProperty("orderBookScalp.queueFastFillThreshold", "500.0"));
    this.signalPerfWindowSize =
        Integer.parseInt(properties.getProperty("orderBookScalp.signalPerfWindowSize", "100"));
    this.dynamicTpMaxDistanceBps =
        Double.parseDouble(properties.getProperty("orderBookScalp.dynamicTpMaxDistanceBps", "50.0"));
    this.dynamicTpMinDistanceBps =
        Double.parseDouble(properties.getProperty("orderBookScalp.dynamicTpMinDistanceBps", "2.0"));
    this.correlationFilterEnabled =
        Boolean.parseBoolean(properties.getProperty("orderBookScalp.correlationFilterEnabled", "false"));
    this.correlationThreshold =
        Double.parseDouble(properties.getProperty("orderBookScalp.correlationThreshold", "0.8"));
    this.correlationReturnWindow =
        Integer.parseInt(properties.getProperty("orderBookScalp.correlationReturnWindow", "20"));
    this.volSpikeFilterEnabled =
        Boolean.parseBoolean(properties.getProperty("orderBookScalp.volSpikeFilterEnabled", "false"));
    this.volSpikeSpreadMultiplier =
        Double.parseDouble(properties.getProperty("orderBookScalp.volSpikeSpreadMultiplier", "3.0"));
    this.volSpikeVolumeMultiplier =
        Double.parseDouble(properties.getProperty("orderBookScalp.volSpikeVolumeMultiplier", "4.0"));
    this.volSpikeCooldownMs =
        Integer.parseInt(properties.getProperty("orderBookScalp.volSpikeCooldownMs", "60000"));
    this.volSpikeLookbackPeriod =
        Integer.parseInt(properties.getProperty("orderBookScalp.volSpikeLookbackPeriod", "20"));
    this.adaptiveParamsEnabled =
        Boolean.parseBoolean(properties.getProperty("orderBookScalp.adaptiveParamsEnabled", "false"));
    this.slippageWindowSize =
        Integer.parseInt(properties.getProperty("orderBookScalp.slippageWindowSize", "200"));
    this.slippageWarningThresholdTicks =
        Double.parseDouble(properties.getProperty("orderBookScalp.slippageWarningThresholdTicks", "5.0"));
    this.partialFillTimeoutMs =
        Long.parseLong(properties.getProperty("orderBookScalp.partialFillTimeoutMs", "5000"));
    this.partialFillMaxResubmitAttempts =
        Integer.parseInt(properties.getProperty("orderBookScalp.partialFillMaxResubmitAttempts", "2"));
    this.blocklistDurationMs =
        Long.parseLong(properties.getProperty("orderBookScalp.blocklistDurationMs", "86400000"));
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

  public boolean isCloseUntrackedPositionsEnabled() {
    return closeUntrackedPositions;
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

  public double getFuturesCommissionPerContract() {
    return futuresCommissionPerContract;
  }

  public double getMinScreeningTradeFlow() {
    return minScreeningTradeFlow;
  }

  public double getMinEconomicsRatio() {
    return minEconomicsRatio;
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

  public boolean isMetricsCsvEnabled() {
    return metricsCsvEnabled;
  }

  public String getMetricsCsvFile() {
    return metricsCsvFile;
  }

  public String getPositionStateFile() {
    return positionStateFile;
  }

  public boolean isVolatilitySizingEnabled() {
    return volatilitySizingEnabled;
  }

  public double getVolatilityTargetSpreadBps() {
    return volatilityTargetSpreadBps;
  }

  public double getVolatilityMinMultiplier() {
    return volatilityMinMultiplier;
  }

  public double getVolatilityMaxMultiplier() {
    return volatilityMaxMultiplier;
  }

  public boolean isTimeOfDayFilterEnabled() {
    return timeOfDayFilterEnabled;
  }

  public double getMorningLiquidityMultiplier() {
    return morningLiquidityMultiplier;
  }

  public double getLunchLiquidityMultiplier() {
    return lunchLiquidityMultiplier;
  }

  public double getEveningLiquidityMultiplier() {
    return eveningLiquidityMultiplier;
  }

  public boolean isDynamicTpSlEnabled() {
    return dynamicTpSlEnabled;
  }

  public double getAtrTpMultiplier() {
    return atrTpMultiplier;
  }

  public double getAtrSlMultiplier() {
    return atrSlMultiplier;
  }

  public int getTrendMomentumWindow() {
    return trendMomentumWindow;
  }

  public int getTrendFlowWindow() {
    return trendFlowWindow;
  }

  public double getTrendMinMomentumRatio() {
    return trendMinMomentumRatio;
  }

  public double getTrendMinFlowAccumulation() {
    return trendMinFlowAccumulation;
  }
  
  // HFT scalping spec parameters (TODO.md Section 2)
  public double getTickSize() {
    return tickSize;
  }
  
  public double getLotStep() {
    return lotStep;
  }
  
  public int getVolLookbackHours() {
    return volLookbackHours;
  }
  
  public int getClusterTicks() {
    return clusterTicks;
  }
  
  public double getFadeRatio() {
    return fadeRatio;
  }
  
  public double getAccelRatio() {
    return accelRatio;
  }
  
  public double getDensityPullExit() {
    return densityPullExit;
  }
  
  public boolean isServerStopEnabled() {
    return serverStopEnabled;
  }
  
  public long getOrderTimeoutMs() {
    return orderTimeoutMs;
  }
  
  // Additional HFT parameters (TODO.md compliance)
  public int getDeltaBarsLookback() {
    return deltaBarsLookback;
  }
  
  public int getStickBars() {
    return stickBars;
  }
  
  public int getMinNetProfitTicks() {
    return minNetProfitTicks;
  }
  
  public double getEatenRatioEntry() {
    return eatenRatioEntry;
  }
  
  // DensityScalpStrategy parameters
  public boolean isStocksEnabled() {
    return stocksEnabled;
  }
  
  public int getLeaderLagSeconds() {
    return leaderLagSeconds;
  }
  
  public double getBasisAnomalySigma() {
    return basisAnomalySigma;
  }
  
  public boolean isDivergenceBlockEnabled() {
    return divergenceBlockEnabled;
  }
  
  public int getTrendTimeframeMinutes() {
    return trendTimeframeMinutes;
  }
  
  public int getTrendLookbackCandles() {
    return trendLookbackCandles;
  }
  
  public long getTrendCacheTtlMs() {
    return trendCacheTtlMs;
  }
  
  public double getMinLevelVolumeRatio() {
    return minLevelVolumeRatio;
  }
  
  public int getMaxLevelAgeMinutes() {
    return maxLevelAgeMinutes;
  }
  
  public double getLevelPriceToleranceBps() {
    return levelPriceToleranceBps;
  }
  
  public double getCompressionSpreadBps() {
    return compressionSpreadBps;
  }
  
  public double getCompressionVolumeMultiplier() {
    return compressionVolumeMultiplier;
  }
  
  public double getCompressionProximityBps() {
    return compressionProximityBps;
  }
  
  public int getCompressionHistorySize() {
    return compressionHistorySize;
  }
  
  public int getMicroImpulseMinTrades() {
    return microImpulseMinTrades;
  }
  
  public int getMicroImpulseWindowMs() {
    return microImpulseWindowMs;
  }
  
  public double getMicroImpulseVolumeMultiplier() {
    return microImpulseVolumeMultiplier;
  }

  // Enhanced engine components config getters (subtask 14)

  public int getRegimeAtrPeriod() {
    return regimeAtrPeriod;
  }

  public int getRegimeAdxPeriod() {
    return regimeAdxPeriod;
  }

  public double getRegimeAdxTrendThreshold() {
    return regimeAdxTrendThreshold;
  }

  public double getRegimeAtrVolatilityMultiplier() {
    return regimeAtrVolatilityMultiplier;
  }

  public int getTapeWindowSize() {
    return tapeWindowSize;
  }

  public double getTapeBlockMultiplier() {
    return tapeBlockMultiplier;
  }

  public int getVpinBucketSize() {
    return vpinBucketSize;
  }

  public int getVpinBucketHistorySize() {
    return vpinBucketHistorySize;
  }

  public double getMaxVpinEntry() {
    return maxVpinEntry;
  }

  public int getMinVpinBuckets() {
    return minVpinBuckets;
  }

  public int getVpinTradesPerBucket() {
    return vpinTradesPerBucket;
  }

  public double getEntryQualityThreshold() {
    return entryQualityThreshold;
  }

  public boolean isTrendFilterEnabled() {
    return trendFilterEnabled;
  }

  public boolean isRegimeFilterEnabled() {
    return regimeFilterEnabled;
  }

  public long getVolumeProfileWindowMillis() {
    return volumeProfileWindowMillis;
  }

  public int getQueueHistoryWindow() {
    return queueHistoryWindow;
  }

  public double getQueuePriceToleranceBps() {
    return queuePriceToleranceBps;
  }

  public double getQueueFastFillThreshold() {
    return queueFastFillThreshold;
  }

  public int getSignalPerfWindowSize() {
    return signalPerfWindowSize;
  }

  public double getDynamicTpMaxDistanceBps() {
    return dynamicTpMaxDistanceBps;
  }

  public double getDynamicTpMinDistanceBps() {
    return dynamicTpMinDistanceBps;
  }

  public boolean isCorrelationFilterEnabled() {
    return correlationFilterEnabled;
  }

  public double getCorrelationThreshold() {
    return correlationThreshold;
  }

  public int getCorrelationReturnWindow() {
    return correlationReturnWindow;
  }

  public boolean isVolSpikeFilterEnabled() {
    return volSpikeFilterEnabled;
  }

  public double getVolSpikeSpreadMultiplier() {
    return volSpikeSpreadMultiplier;
  }

  public double getVolSpikeVolumeMultiplier() {
    return volSpikeVolumeMultiplier;
  }

  public int getVolSpikeCooldownMs() {
    return volSpikeCooldownMs;
  }

  public int getVolSpikeLookbackPeriod() {
    return volSpikeLookbackPeriod;
  }

  public boolean isAdaptiveParamsEnabled() {
    return adaptiveParamsEnabled;
  }

  public int getSlippageWindowSize() {
    return slippageWindowSize;
  }

  public double getSlippageWarningThresholdTicks() {
    return slippageWarningThresholdTicks;
  }

  public long getPartialFillTimeoutMs() {
    return partialFillTimeoutMs;
  }

  public int getPartialFillMaxResubmitAttempts() {
    return partialFillMaxResubmitAttempts;
  }

  public long getBlocklistDurationMs() {
    return blocklistDurationMs;
  }
}
