package com.github.shk0da.goldendragon.strategy.orderbook;

import static com.github.shk0da.goldendragon.utils.TimeUtils.sleep;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toSet;

import com.github.shk0da.goldendragon.config.MainConfig;
import com.github.shk0da.goldendragon.config.OrderBookScalpConfig;
import com.github.shk0da.goldendragon.filters.CorrelationFilter;
import com.github.shk0da.goldendragon.filters.VolatilitySpikeFilter;
import com.github.shk0da.goldendragon.model.Candle;
import com.github.shk0da.goldendragon.model.MarketDepthSnapshot;
import com.github.shk0da.goldendragon.model.MarketTickListener;
import com.github.shk0da.goldendragon.model.MarketTradeTick;
import com.github.shk0da.goldendragon.model.PositionInfo;
import com.github.shk0da.goldendragon.model.TickerInfo;
import com.github.shk0da.goldendragon.model.TickerType;
import com.github.shk0da.goldendragon.money.KillSwitch;
import com.github.shk0da.goldendragon.money.RiskManager;
import com.github.shk0da.goldendragon.repository.TickerRepository;
import com.github.shk0da.goldendragon.service.TCSService;
import com.github.shk0da.goldendragon.strategy.OrderBookScalpScreener;
import com.github.shk0da.goldendragon.strategy.orderbook.diagnostics.OrderBookDiagnosticEvent;
import com.github.shk0da.goldendragon.strategy.orderbook.diagnostics.OrderBookDiagnosticEventType;
import com.github.shk0da.goldendragon.strategy.orderbook.diagnostics.OrderBookDiagnosticsCollector;
import com.github.shk0da.goldendragon.strategy.orderbook.diagnostics.OrderBookDiagnosticsReplayWriter;
import com.github.shk0da.goldendragon.strategy.orderbook.diagnostics.OrderBookDiagnosticsSummary;
import com.github.shk0da.goldendragon.strategy.orderbook.diagnostics.OrderBookMetricsCsvWriter;
import com.github.shk0da.goldendragon.utils.IndicatorsUtil;
import com.github.shk0da.goldendragon.utils.LoggingUtils;
import com.github.shk0da.goldendragon.utils.TickerTypeResolver;
import java.time.Duration;
import java.time.Instant;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import ru.tinkoff.piapi.contract.v1.HistoricCandle;
import static ru.tinkoff.piapi.contract.v1.CandleInterval.CANDLE_INTERVAL_5_MIN;

/**
 * Shared order-book trading engine: subscriptions, screening, position management and execution.
 *
 * <p>Delegates entry/exit signals to pluggable {@link OrderBookSignal} implementations. One
 * position per ticker; first signal in priority order wins entry.
 */
public final class OrderBookTradingEngine implements MarketTickListener {

  private static final long COOLDOWN_DURATION_MS = 5 * 60 * 1000L;

  private static final long STREAM_STALE_THRESHOLD_MS = 60 * 1000L;

  private static final int MAX_STREAM_RECOVERY_ATTEMPTS = 3;

  private static final int EXIT_PERSISTENCE_TICKS = 5;

  private static final double FLOW_REVERSAL_EXIT_MULTIPLIER = 2.0;

  private static final double MIN_ENTRY_FLOW_MULTIPLIER = 2.0;

  private static final long RECOVERY_STABILIZATION_MS = 15_000L;

  private static final double ENTRY_QUALITY_THRESHOLD = 0.30;

  private static final double ENTRY_QUALITY_OBI_WEIGHT = 0.15;

  private static final double ENTRY_QUALITY_EDGE_WEIGHT = 0.40;

  private static final double ENTRY_QUALITY_FLOW_WEIGHT = 0.25;

  private static final double ENTRY_QUALITY_SPREAD_WEIGHT = 0.10;

  private static final double ENTRY_QUALITY_RECOVERY_WEIGHT = 0.10;

  private static final double ENTRY_QUALITY_OBI_MIN = 0.35;

  private static final int ORDER_PLACE_ATTEMPTS = 2;

  private static final long ORDER_RETRY_DELAY_MS = 3 * 1000L;

  private static final long DIAGNOSTICS_SUMMARY_INTERVAL_MS = 10 * 60 * 1000L;

  private static final long INITIAL_DIAGNOSTICS_SUMMARY_DELAY_MS = 30 * 1000L;

  private static final long SKIP_DIAGNOSTIC_THROTTLE_MS = 30 * 1000L;
  
  // Manual emergency stop state (TODO.md Section 5, item 135)
  private volatile boolean manualEmergencyStopRequested = false;

  private final TCSService tcsService;
  private final MainConfig mainConfig;
  private final OrderBookScalpConfig config;
  private final List<OrderBookSignal> signals;
  private final Map<String, OrderBookSignal> signalsById;
  private final String strategyName;
  private final Map<String, TickerRuntime> runtimesByTicker = new ConcurrentHashMap<>();
  private final Map<String, TickerRuntime> runtimesByFigi = new ConcurrentHashMap<>();
  private final TradeStats tradeStats = new TradeStats();
  private final KillSwitch killSwitch;
  private final RiskManager riskManager;
  private final OrderBookDiagnosticsCollector diagnosticsCollector;
  private final OrderBookDiagnosticsReplayWriter diagnosticsReplayWriter;
  private final OrderBookMetricsCsvWriter metricsCsvWriter;
  private final Map<String, Long> lastSkipDiagnosticMsByKey = new ConcurrentHashMap<>();
  private final Map<String, CommissionEstimator> commissionEstimators = new ConcurrentHashMap<>();
  private volatile long lastStreamErrorLogMs;
  private volatile long lastMarketDataAtMs;
  private volatile int streamRecoveryAttempts;
  private volatile long nextRescreenMs;
  private volatile long nextDiagnosticsSummaryMs;
  private volatile long nextHeartbeatDiagnosticMs;
  private volatile long marketDataRecoveryUntilMs;
  private volatile double initialEquity;
  private final OrderBookPositionStore positionStore;
  private final Map<String, Long> lastProcessingLatencyNs = new ConcurrentHashMap<>();
  private final Map<String, List<Long>> processingLatencySamples = new ConcurrentHashMap<>();
  private static final int MAX_LATENCY_SAMPLES = 100;
  private final Map<String, VolatilityTracker> volatilityTrackers = new ConcurrentHashMap<>();
  private final OrderBookTrendFilter trendFilter;
  private final LiquidityWindows liquidityWindows;
  private final Map<String, Long> lastThrottledLogMs = new ConcurrentHashMap<>();
  private static final ZoneId MSK_ZONE = ZoneId.of("Europe/Moscow");

  // Enhanced engine components (subtask 14)
  private final MarketRegimeDetector regimeDetector;
  private final TapeReader tapeReader;
  private final VpinCalculator vpinCalculator;
  private final VolumeProfileTracker volumeProfileTracker;
  private final QueueDynamicsTracker queueDynamicsTracker;
  private final SignalPerformanceTracker signalPerformanceTracker;
  private final DynamicTakeProfit dynamicTakeProfit;
  private final CorrelationFilter correlationFilter;
  private final VolatilitySpikeFilter volatilitySpikeFilter;
  private final AdaptiveParameters adaptiveParameters;
  private final SlippageTracker slippageTracker;
  private final PartialFillHandler partialFillHandler;
  private final TickerBlocklist tickerBlocklist;

  public OrderBookTradingEngine(
      TCSService tcsService,
      MainConfig mainConfig,
      OrderBookScalpConfig config,
      List<OrderBookSignal> signals,
      String strategyName) {
    this.tcsService = tcsService;
    this.mainConfig = mainConfig;
    this.config = config;
    this.signals = List.copyOf(signals);
    this.signalsById = new HashMap<>();
    for (OrderBookSignal signal : this.signals) {
      this.signalsById.put(signal.id(), signal);
    }
    this.strategyName = strategyName;
    this.positionStore = new OrderBookPositionStore(config.getPositionStateFile());
    this.liquidityWindows = new LiquidityWindows(config);
    this.trendFilter =
        new OrderBookTrendFilter(
            config.getTrendMomentumWindow(),
            config.getTrendFlowWindow(),
            config.getTrendMinMomentumRatio(),
            config.getTrendMinFlowAccumulation());
    if (config.isRiskManagementEnabled()) {
      this.killSwitch = new KillSwitch(config.getCriticalDrawdownPercent());
      this.riskManager =
          new RiskManager(
              config.getRiskPerTradePercent(),
              config.getMaxDailyLossPercent(),
              config.getMaxConsecutiveLosses());
    } else {
      this.killSwitch = null;
      this.riskManager = null;
    }
    if (config.isDiagnosticsEnabled()) {
      this.diagnosticsCollector = new OrderBookDiagnosticsCollector();
      log(strategyName + ": diagnostics collector initialized");
      this.diagnosticsReplayWriter =
          config.isDiagnosticsReplayEnabled()
              ? new OrderBookDiagnosticsReplayWriter(config.getDiagnosticsReplayFile())
              : null;
      this.metricsCsvWriter =
          config.isMetricsCsvEnabled()
              ? new OrderBookMetricsCsvWriter(config.getMetricsCsvFile())
              : null;
      if (metricsCsvWriter != null) {
        log(strategyName + ": metrics CSV writer initialized -> " + config.getMetricsCsvFile());
      }
      
      // Setup DensityScalpSignal skip metrics callback
      for (OrderBookSignal signal : this.signals) {
        if (signal instanceof DensityScalpSignal) {
          DensityScalpSignal densitySignal = (DensityScalpSignal) signal;
          densitySignal.setSkipMetricsCallback(this::emitDensityScalpSkip);
        }
      }
    } else {
      this.diagnosticsCollector = null;
      this.diagnosticsReplayWriter = null;
      this.metricsCsvWriter = null;
      log(strategyName + ": diagnostics DISABLED");
    }

    // Initialize enhanced engine components (subtask 14)
    this.regimeDetector = new MarketRegimeDetector(
        config.getRegimeAtrPeriod(),
        config.getRegimeAdxPeriod(),
        config.getRegimeAdxTrendThreshold(),
        config.getRegimeAtrVolatilityMultiplier());
    this.tapeReader = new TapeReader(
        config.getTapeWindowSize(),
        config.getTapeBlockMultiplier());
    this.vpinCalculator = new VpinCalculator(
        config.getVpinBucketSize(),
        config.getVpinBucketHistorySize());
    double effectiveTickSize = config.getTickSize() > 0.0 ? config.getTickSize() : 0.01;
    this.volumeProfileTracker = new VolumeProfileTracker(
        effectiveTickSize,
        config.getVolumeProfileWindowMillis());
    this.queueDynamicsTracker = new QueueDynamicsTracker(
        config.getQueueHistoryWindow(),
        config.getQueuePriceToleranceBps(),
        config.getQueueFastFillThreshold());
    this.signalPerformanceTracker = new SignalPerformanceTracker(
        config.getSignalPerfWindowSize());
    DensityAnalyzer densityAnalyzer = new DensityAnalyzer(tcsService, config);
    this.dynamicTakeProfit = new DynamicTakeProfit(
        densityAnalyzer,
        volumeProfileTracker,
        config.getClusterTicks(),
        config.getDynamicTpMaxDistanceBps(),
        config.getDynamicTpMinDistanceBps());
    this.correlationFilter = new CorrelationFilter(
        config.isCorrelationFilterEnabled(),
        config.getCorrelationThreshold(),
        config.getCorrelationReturnWindow());
    this.volatilitySpikeFilter = new VolatilitySpikeFilter(
        config.isVolSpikeFilterEnabled(),
        config.getVolSpikeSpreadMultiplier(),
        config.getVolSpikeVolumeMultiplier(),
        config.getVolSpikeCooldownMs(),
        config.getVolSpikeLookbackPeriod());
    AdaptiveParameters.Config adaptiveConfig = AdaptiveParameters.Config.builder()
        .baseMinDelta(config.getMinTradeFlow())
        .build();
    this.adaptiveParameters = new AdaptiveParameters(signalPerformanceTracker, adaptiveConfig);
    this.slippageTracker = new SlippageTracker(
        effectiveTickSize,
        config.getSlippageWindowSize(),
        config.getSlippageWarningThresholdTicks());
    this.partialFillHandler = new PartialFillHandler(
        config.getPartialFillTimeoutMs(),
        PartialFillHandler.Strategy.CANCEL_REMAINING,
        config.getPartialFillMaxResubmitAttempts());
    this.tickerBlocklist = new TickerBlocklist();

    log(strategyName + ": enhanced components initialized"
        + ", regimeDetector=on"
        + ", tapeReader=on"
        + ", vpinCalculator=on"
        + ", volumeProfileTracker=on"
        + ", queueDynamicsTracker=on"
        + ", signalPerformanceTracker=on"
        + ", dynamicTakeProfit=on"
        + ", correlationFilter=" + config.isCorrelationFilterEnabled()
        + ", volatilitySpikeFilter=" + config.isVolSpikeFilterEnabled()
        + ", adaptiveParameters=" + config.isAdaptiveParamsEnabled()
        + ", slippageTracker=on"
        + ", partialFillHandler=on");
  }

  public void run() {
    boolean paper = config.isPaperMode() || mainConfig.isTestMode();
    List<String> signalIds = signals.stream().map(OrderBookSignal::id).collect(toList());
    log(
        strategyName
            + " start: instruments="
            + config.getInstruments()
            + ", depth="
            + config.getDepth()
            + ", paper="
            + paper
            + ", positionCash="
            + config.getPositionCash()
            + ", screeningTopN="
            + config.getScreeningTopN()
            + ", signals="
            + signalIds);


    tcsService.logAccountTradingEligibility();
    tcsService.logAccountPositions();

    while (true) {
      // Check for manual emergency stop (TODO.md Section 5, item 135)
      if (manualEmergencyStopRequested) {
        log(strategyName + ": MANUAL EMERGENCY STOP TRIGGERED - closing all positions immediately");
        emergencyCloseAllPositions(paper);
        manualEmergencyStopRequested = false;
        log(strategyName + ": Emergency stop completed, waiting cooldown");
        sleep(COOLDOWN_DURATION_MS);
        continue;
      }
      
      try {
        runTradingSession(paper);
        return;
      } catch (Exception ex) {
        String message = strategyName + " error: " + ex.getMessage();
        log(message);
        log(strategyName + " entering cooldown for " + (COOLDOWN_DURATION_MS / 1000L) + "s");
        sleep(COOLDOWN_DURATION_MS);
      }
    }
  }
  
  /**
   * Manual emergency stop - immediately closes all positions and stops trading.
   * Can be called externally (e.g., via API, monitoring system, or manual intervention).
   * TODO.md Section 5, item 135: Ручной аварийный стоп.
   */
  public void requestManualEmergencyStop() {
    log(strategyName + ": MANUAL EMERGENCY STOP REQUESTED by external trigger");
    manualEmergencyStopRequested = true;
  }
  
  /**
   * Emergency close all positions immediately.
   */
  private void emergencyCloseAllPositions(boolean paper) {
    for (TickerRuntime runtime : runtimesByTicker.values()) {
      if (runtime.openPosition != null) {
        log(strategyName + ": Emergency closing position for " + runtime.ticker);
        closeOpenPosition(runtime, "manual_emergency_stop", paper);
      }
    }
  }

  private void runTradingSession(boolean paper) {
    if (!initInitialEquity(paper)) {
      log(strategyName + ": cannot start, no account equity available, stopping");
      return;
    }

    List<TickerRuntime> subscribed = subscribeInstruments(resolveInstruments());
    if (!subscribed.isEmpty()) {
      closeUntrackedPositions(subscribed, paper);
      restoreTrackedPositions(subscribed, paper);
      lastMarketDataAtMs = System.currentTimeMillis();
    }

    nextRescreenMs = System.currentTimeMillis() + config.getRescreenMinutes() * 60_000L;
    nextDiagnosticsSummaryMs = System.currentTimeMillis() + INITIAL_DIAGNOSTICS_SUMMARY_DELAY_MS;
    nextHeartbeatDiagnosticMs = System.currentTimeMillis();
    log(strategyName + ": diagnostics collector initialized, emitting startup diagnostic");
    emitDiagnostic(
        OrderBookDiagnosticEventType.SUMMARY,
        strategyName,
        "session_started",
        Map.of(
            "subscribed",
            subscribed.size(),
            "paper",
            paper,
            "diagnosticsEnabled",
            config.isDiagnosticsEnabled(),
            "diagnosticsSummaryEnabled",
            config.isDiagnosticsSummaryEnabled()));

    try {
      while (true) {
        if (subscribed.isEmpty()) {
          long now = System.currentTimeMillis();
          if (now >= nextRescreenMs) {
            log(strategyName + ": no instruments subscribed, entering idle rescreen");
            List<TickerRuntime> refreshed = subscribeInstruments(resolveInstruments());
            if (!refreshed.isEmpty()) {
              subscribed.addAll(refreshed);
              closeUntrackedPositions(subscribed, paper);
              lastMarketDataAtMs = System.currentTimeMillis();
              log(strategyName + ": resumed from idle with " + subscribed.size() + " instruments");
            }
            nextRescreenMs = now + config.getIdleRescreenSeconds() * 1_000L;
          }
          maybeLogPeriodicDiagnosticsSummary();
          sleep(1_000);
          continue;
        }

        checkStreamHealth(paper);
        maybeLogPeriodicDiagnosticsSummary();
        maybeLogHeartbeatDiagnostic();
        if (isAllFuturesMode(config.getInstruments())
            && System.currentTimeMillis() >= nextRescreenMs) {
          try {
            rescreenSubscriptions(subscribed, paper);
          } catch (Exception ex) {
            log(strategyName + " rescreen failed: " + ex.getMessage());
          }
          nextRescreenMs = System.currentTimeMillis() + config.getRescreenMinutes() * 60_000L;
        }
        sleep(1_000);
      }
    } finally {
      for (TickerRuntime runtime : subscribed) {
        tcsService.unsubscribeMarketData(runtime.key, this);
        closeOpenPosition(runtime, "session_end", paper);
      }
      logStats("session_end");
      if (config.isDiagnosticsSummaryEnabled() && diagnosticsCollector != null) {
        OrderBookDiagnosticsSummary summary = diagnosticsCollector.summarize();
        log(buildPeriodicDiagnosticsSummary(summary, summary));
        log(buildPeriodicDiagnosticsRecommendation(summary, summary));
        log(
            strategyName
                + " diagnostics: opened="
                + summary.getOpenedCount()
                + ", skipped="
                + summary.getSkippedCount()
                + ", closed="
                + summary.getClosedCount()
                + ", recoveries="
                + summary.getRecoveryCount()
                + ", avgQuality="
                + String.format("%.3f", summary.getAverageEntryQuality())
                + ", grossPnl="
                + String.format("%.2f", summary.getGrossPnl())
                + ", netPnl="
                + String.format("%.2f", summary.getNetPnl())
                + ", feeDrag="
                + String.format("%.2f", summary.getFeeDrag())
                + ", avgHoldSeconds="
                + String.format("%.1f", summary.getAverageHoldSeconds())
                + ", skipReasons="
                + summary.getSkipReasons()
                + ", closeReasons="
                + summary.getCloseReasons());

        Map<String, Object> metrics = new HashMap<>();
        metrics.put("session_opened", summary.getOpenedCount());
        metrics.put("session_skipped", summary.getSkippedCount());
        metrics.put("session_closed", summary.getClosedCount());
        metrics.put("session_recoveries", summary.getRecoveryCount());
        metrics.put("session_avgQuality", summary.getAverageEntryQuality());
        metrics.put("session_grossPnl", summary.getGrossPnl());
        metrics.put("session_netPnl", summary.getNetPnl());
        metrics.put("session_feeDrag", summary.getFeeDrag());
        metrics.put("session_avgHoldSeconds", summary.getAverageHoldSeconds());
        metrics.put("session_skipReasons", summary.getSkipReasons().toString());
        metrics.put("session_closeReasons", summary.getCloseReasons().toString());
        metrics.put("session_skippedTickers", summary.getSkippedTickers().toString());

        emitDiagnostic(OrderBookDiagnosticEventType.SUMMARY, strategyName, "session_end", metrics);
      }
      if (diagnosticsReplayWriter != null) {
        diagnosticsReplayWriter.close();
      }
      if (metricsCsvWriter != null) {
        metricsCsvWriter.close();
      }
      log(strategyName + " stopped");
    }
  }

  private void closeUntrackedPositions(List<TickerRuntime> subscribed, boolean paper) {
    if (paper) {
      return;
    }
    Map<TickerInfo.Key, PositionInfo> positions;
    try {
      positions = tcsService.getCurrentPositions(TickerType.ALL);
    } catch (Exception ex) {
      log(strategyName + " position sync failed: " + ex.getMessage());
      return;
    }
    Set<String> trackedTickers =
        subscribed.stream().map(runtime -> runtime.ticker).collect(toSet());
    double availableCash = 0.0;
    try {
      availableCash = tcsService.getAvailableCash();
    } catch (Exception ex) {
      logThrottled("_close_untracked_cash", strategyName + ": cannot fetch available cash: " + ex.getMessage(), 5);
    }
    for (PositionInfo position : positions.values()) {
      if (position.getBalance() == 0) {
        continue;
      }
      if (!isTradablePositionType(position.getInstrumentType())) {
        continue;
      }
      if (trackedTickers.contains(position.getTicker())) {
        continue;
      }
      // Skip blocked tickers — broker requires confirmation, retry later
      if (tickerBlocklist.isBlocked(position.getTicker())) {
        logThrottled(
            "_close_untracked_blocked_" + position.getTicker(),
            "Untracked position for " + position.getTicker()
                + " skipped: ticker is blocked (" + tickerBlocklist.getRemainingMs(position.getTicker()) / 3600000 + "h remaining)",
            15);
        continue;
      }
      boolean isShort = position.getBalance() < 0;
      if (isShort) {
        // Calculate required cash to close short position
        TickerInfo.Key posKey = new TickerInfo.Key(position.getTicker(), position.getInstrumentType());
        TickerInfo posTickerInfo = tcsService.searchTicker(posKey);
        if (posTickerInfo == null) {
          logThrottled(
              "_close_untracked_no_info_" + position.getTicker(),
              "Untracked short position for "
                  + position.getTicker()
                  + ", skipping close: ticker info not found",
              15);
          continue;
        }
        int lot = posTickerInfo.getLot() != null ? Math.max(1, posTickerInfo.getLot()) : 1;
        double askPrice = tcsService.getLiveAskPrice(posKey);
        if (askPrice <= 0.0) {
          logThrottled(
              "_close_untracked_no_price_" + position.getTicker(),
              "Untracked short position for "
                  + position.getTicker()
                  + ", skipping close: no ask price available",
              15);
          continue;
        }
        int absBalance = Math.abs(position.getBalance());
        double requiredCash = absBalance * lot * askPrice;
        if (availableCash < requiredCash) {
          logThrottled(
              "_close_untracked_" + position.getTicker(),
              "Untracked short position for "
                  + position.getTicker()
                  + " qty="
                  + position.getBalance()
                  + ", skipping close: insufficient cash (required="
                  + String.format("%.2f", requiredCash)
                  + ", available="
                  + String.format("%.2f", availableCash)
                  + ")",
              15);
          continue;
        }
      } else {
        // LONG position: verify that position actually exists on broker before attempting close
        TickerInfo.Key posKey = new TickerInfo.Key(position.getTicker(), position.getInstrumentType());
        double bidPrice = tcsService.getLiveBidPrice(posKey);
        if (bidPrice <= 0.0) {
          logThrottled(
              "_close_untracked_no_price_" + position.getTicker(),
              "Untracked long position for "
                  + position.getTicker()
                  + ", skipping close: no bid price available",
              15);
          continue;
        }
        int absBalance = Math.abs(position.getBalance());
        double positionValue = absBalance * bidPrice;
        log("Untracked long position for "
            + position.getTicker()
            + " qty=" + position.getBalance()
            + ", value=" + String.format("%.2f", positionValue)
            + ", available=" + String.format("%.2f", availableCash)
            + ", closing");
      }
      log(
          "Untracked position detected for "
              + position.getTicker()
              + " qty="
              + position.getBalance()
              + ", closing");
      try {
        TCSService.OrderExecutionResult result =
            isShort
                ? tcsService.closeShortByMarketWithDetails(
                    position.getTicker(), position.getInstrumentType())
                : tcsService.closeLongByMarketWithDetails(
                    position.getTicker(), position.getInstrumentType());
        if (!result.isSuccess()) {
          logThrottled(
              "_close_untracked_fail_" + position.getTicker(),
              "Failed to close untracked position for " + position.getTicker(),
              15);
        } else {
          log("Successfully closed untracked position for " + position.getTicker());
        }
      } catch (Exception ex) {
        logThrottled(
            "_close_untracked_exception_" + position.getTicker(),
            "Exception closing untracked position for " + position.getTicker()
                + ": " + ex.getMessage(),
            15);
      }
    }
  }

  private static boolean isTradablePositionType(TickerType type) {
    return TickerType.FEATURE == type || TickerType.STOCK == type || TickerType.ETF == type;
  }

  private void restoreTrackedPositions(List<TickerRuntime> subscribed, boolean paper) {
    if (paper) {
      return;
    }
    Map<String, PositionState> restored = positionStore.load();
    if (restored.isEmpty()) {
      log(strategyName + ": no tracked positions to restore");
      return;
    }
    Map<TickerInfo.Key, PositionInfo> brokerPositions;
    try {
      brokerPositions = tcsService.getCurrentPositions(TickerType.ALL);
    } catch (Exception ex) {
      log(strategyName + " position restore sync failed: " + ex.getMessage());
      return;
    }
    for (TickerRuntime runtime : subscribed) {
      PositionState state = restored.get(runtime.ticker);
      if (state == null) {
        adoptBrokerPositionIfAny(runtime, brokerPositions.get(runtime.key));
        continue;
      }
      PositionInfo brokerPosition = brokerPositions.get(runtime.key);
      if (brokerPosition == null || brokerPosition.getBalance() == 0) {
        positionStore.remove(runtime.ticker);
        log("Tracked position " + runtime.ticker + " gone at broker, dropping tracking");
        emitDiagnostic(
            OrderBookDiagnosticEventType.POSITION_CLOSED,
            runtime.ticker,
            "externally_closed",
            Map.of(
                "signalId",
                state.signalId,
                "direction",
                state.direction,
                "entryPrice",
                state.entryPrice,
                "units",
                state.units));
        continue;
      }
      runtime.openPosition = state.toOpenPosition();
      
      // Apply tighter stop loss for restored positions
      // Restored positions may be stale, so we tighten the stop loss to reduce risk
      applyTighterStopLossForRestoredPosition(runtime);
      
      log(
          "Restored tracked position "
              + runtime.ticker
              + " "
              + state.direction
              + " entry="
              + state.entryPrice
              + " units="
              + state.units
              + " sl="
              + runtime.openPosition.stopLossPrice);
      emitDiagnostic(
          OrderBookDiagnosticEventType.POSITION_RESTORED,
          runtime.ticker,
          "restored",
          Map.of(
              "signalId",
              state.signalId,
              "direction",
              state.direction,
              "entryPrice",
              state.entryPrice,
              "units",
              state.units));
    }
  }

  private void adoptBrokerPositionIfAny(TickerRuntime runtime, PositionInfo brokerPosition) {
    if (brokerPosition == null || brokerPosition.getBalance() == 0) {
      return;
    }
    Map<String, Map<Double, Integer>> book;
    try {
      book = tcsService.getCurrentPrices(runtime.key, false);
    } catch (Exception ex) {
      log("Cannot adopt broker position for " + runtime.ticker + ": " + ex.getMessage());
      return;
    }
    int balance = brokerPosition.getBalance();
    String direction = balance > 0 ? "LONG" : "SHORT";
    double bid = resolveBestBid(book, 0.0);
    double ask = resolveBestAsk(book, 0.0);
    if (bid <= 0.0 || ask <= 0.0) {
      log("Cannot adopt broker position for " + runtime.ticker + ": no market prices");
      return;
    }
    double entryPrice = "LONG".equals(direction) ? ask : bid;
    double spread = Math.max(0.0, ask - bid);
    int units = Math.abs(balance);
    double entryValue = units * entryPrice;
    BracketPrices bracket =
        "LONG".equals(direction)
            ? buildBracketPrices(runtime.ticker, bid, entryPrice)
            : buildBracketPricesShort(runtime.ticker, entryPrice, ask);
    runtime.openPosition =
        new OpenPosition(
            "adopted-" + runtime.ticker,
            direction,
            entryPrice,
            spread,
            Instant.now(),
            bracket.tpPrice,
            bracket.slPrice,
            units,
            entryValue,
            entryValue * config.getCommissionRate());
    persistPosition(runtime);
    log(
        "Adopted broker position "
            + runtime.ticker
            + " "
            + direction
            + " entry="
            + entryPrice
            + " units="
            + units);
  }

  private void persistPosition(TickerRuntime runtime) {
    if (runtime.openPosition == null) {
      return;
    }
    positionStore.save(runtime.ticker, new PositionState(runtime.openPosition));
  }

  private void checkStreamHealth(boolean paper) {
    long now = System.currentTimeMillis();
    if (now - lastMarketDataAtMs < STREAM_STALE_THRESHOLD_MS) {
      return;
    }
    log(
        strategyName
            + ": no market data for "
            + ((now - lastMarketDataAtMs) / 1000L)
            + "s, recovering stream");
    for (TickerRuntime runtime : runtimesByTicker.values()) {
      if (runtime.openPosition != null) {
        closeOpenPosition(runtime, "stream_outage", paper);
      }
    }
    streamRecoveryAttempts++;
    if (streamRecoveryAttempts >= MAX_STREAM_RECOVERY_ATTEMPTS) {
      throw new IllegalStateException(
          strategyName
              + ": market data stream dead after "
              + streamRecoveryAttempts
              + " recovery attempts, restarting session");
    }
    for (TickerRuntime runtime : runtimesByTicker.values()) {
      if (runtime.openPosition != null) {
        emitDiagnostic(
            OrderBookDiagnosticEventType.STREAM_OUTAGE_EXIT,
            runtime.ticker,
            "stream_outage",
            Map.of());
      }
    }
    emitDiagnostic(
        OrderBookDiagnosticEventType.STREAM_RECOVERY_STARTED,
        strategyName,
        String.valueOf(streamRecoveryAttempts),
        Map.of());
    recoverStreamSubscriptions();
    marketDataRecoveryUntilMs = System.currentTimeMillis() + RECOVERY_STABILIZATION_MS;
    lastMarketDataAtMs = System.currentTimeMillis();
    emitDiagnostic(
        OrderBookDiagnosticEventType.STREAM_RECOVERY_FINISHED,
        strategyName,
        String.valueOf(streamRecoveryAttempts),
        Map.of());
  }

  private void recoverStreamSubscriptions() {
    for (TickerRuntime runtime : runtimesByTicker.values()) {
      try {
        tcsService.unsubscribeMarketData(runtime.key, this);
        tcsService.subscribeMarketData(runtime.key, config.getDepth(), this);
        log("Resubscribed to order book: " + runtime.ticker);
      } catch (Exception ex) {
        log("Failed to resubscribe " + runtime.ticker + ": " + ex.getMessage());
      }
    }
  }
  
  /**
   * Reconciles tracked position with broker position after stream outage (TODO.md Section 5).
   * Ensures internal state matches exchange reality.
   */
  private void reconcilePositionAfterClose(TickerRuntime runtime, String signalId, String reason) {
    try {
      Map<TickerInfo.Key, PositionInfo> brokerPositions = tcsService.getCurrentPositions(TickerType.ALL);
      PositionInfo brokerPosition = brokerPositions.get(runtime.key);
      
      if (brokerPosition != null && brokerPosition.getBalance() != 0) {
        log("WARN: Position closed but broker still shows " + runtime.ticker + " qty=" + brokerPosition.getBalance());
        // This could indicate a failed close - trigger emergency investigation
        emitDiagnostic(
            OrderBookDiagnosticEventType.RECONCILIATION_MISMATCH,
            runtime.ticker,
            "position_mismatch",
            Map.of(
                "signalId", signalId,
                "reason", reason,
                "brokerQty", brokerPosition.getBalance()));
      }
    } catch (Exception e) {
      log("Reconciliation failed for " + runtime.ticker + ": " + e.getMessage());
    }
  }

  private boolean initInitialEquity(boolean paper) {
    if (paper) {
      initialEquity = Math.max(config.getPositionCash(), 1.0);
      log(
          strategyName
              + ": paper mode, using synthetic initialEquity="
              + String.format("%.2f", initialEquity));
      return true;
    }
    try {
      initialEquity = tcsService.getTotalPortfolioCost();
    } catch (Exception ex) {
      log(
          strategyName
              + ": portfolio cost unavailable, fallback to available cash: "
              + ex.getMessage());
      Double availableCash = tcsService.getAvailableCash();
      initialEquity = availableCash != null ? availableCash : 0.0;
    }
    log(strategyName + ": initialEquity=" + String.format("%.2f", initialEquity));
    return initialEquity > 0.0;
  }

  @Override
  public void onOrderBook(MarketDepthSnapshot snapshot) {
    lastMarketDataAtMs = System.currentTimeMillis();
    if (streamRecoveryAttempts > 0) {
      streamRecoveryAttempts = 0;
    }
    if (snapshot == null || !snapshot.isConsistent()) {
      return;
    }

    TickerRuntime runtime = runtimesByFigi.get(snapshot.getFigi());
    if (runtime == null) {
      return;
    }

    // Update enhanced components with order book data (subtask 14)
    queueDynamicsTracker.update(runtime.ticker, snapshot);

    synchronized (runtime) {
      handleOrderBook(runtime, snapshot);
    }
  }

  @Override
  public void onTrade(MarketTradeTick trade) {
    // trade flow is read from TCSService recent trades buffer on each book update

    // Feed enhanced components with trade data (subtask 14)
    if (trade == null || trade.getFigi() == null) {
      return;
    }
    TickerRuntime runtime = runtimesByFigi.get(trade.getFigi());
    if (runtime == null) {
      return;
    }
    tapeReader.onTrade(runtime.ticker, trade);
    vpinCalculator.onTrade(runtime.ticker, trade);
    volumeProfileTracker.addTrade(
        runtime.ticker, trade.getPrice(), trade.getQuantity(),
        trade.getTime() != null ? trade.getTime().toEpochMilli() : System.currentTimeMillis());
  }

  @Override
  public void onError(Throwable throwable) {
    long now = System.currentTimeMillis();
    if (now - lastStreamErrorLogMs < 5_000L) {
      return;
    }
    lastStreamErrorLogMs = now;
    log(strategyName + " stream error: " + throwable.getMessage());
  }

  private void handleOrderBook(TickerRuntime runtime, MarketDepthSnapshot snapshot) {
    long startTime = System.nanoTime();

    // Store latest snapshot for DynamicTakeProfit access (subtask 14)
    runtime.latestSnapshot = snapshot;

    Double bestBid = snapshot.getBestBid();
    Double bestAsk = snapshot.getBestAsk();
    if (bestBid == null || bestAsk == null || bestAsk <= bestBid) {
      return;
    }

    int bidQty0 = OrderBookMath.topQuantity(snapshot.getBids(), 0);
    int askQty0 = OrderBookMath.topQuantity(snapshot.getAsks(), 0);
    if (bidQty0 < config.getMinBestLevelQty() || askQty0 < config.getMinBestLevelQty()) {
      return;
    }

    double spread = bestAsk - bestBid;
    double mid = (bestBid + bestAsk) / 2.0;
    double spreadBps = mid > 0.0 ? spread / mid * 10_000.0 : Double.MAX_VALUE;
    if (spreadBps > config.getMaxSpreadBps()) {
      return;
    }

    boolean paper = config.isPaperMode() || mainConfig.isTestMode();
    double obi =
        OrderBookMath.calculateObi(snapshot.getBids(), snapshot.getAsks(), config.getObiLevels());
    double microEdge = OrderBookMath.calculateMicroEdge(bestBid, bestAsk, bidQty0, askQty0);
    double tradeDelta = calculateTradeDelta(runtime.key);
    double weightedDepthImbalance =
        OrderBookMath.calculateWeightedDepthImbalance(
            snapshot.getBids(), snapshot.getAsks(), config.getObiLevels());
    double depthGradient =
        OrderBookMath.calculateDepthGradient(
            snapshot.getBids(), snapshot.getAsks(), config.getObiLevels());
    double absorptionScore =
        OrderBookMath.calculateAbsorptionScore(snapshot.getBids(), snapshot.getAsks());

    OrderBookMarketContext context =
        new OrderBookMarketContext(
            snapshot,
            runtime.key,
            runtime.ticker,
            bestBid,
            bestAsk,
            spread,
            spreadBps,
            bidQty0,
            askQty0,
            obi,
            microEdge,
            tradeDelta,
            weightedDepthImbalance,
            depthGradient,
            absorptionScore);

    // Update volatility tracker for this ticker
    VolatilityTracker volatilityTracker =
        volatilityTrackers.computeIfAbsent(runtime.ticker, k -> new VolatilityTracker());
    volatilityTracker.update(spread, mid);

    // Measure and record processing latency
    long latencyNs = System.nanoTime() - startTime;
    lastProcessingLatencyNs.put(runtime.ticker, latencyNs);
    recordLatencySample(runtime.ticker, latencyNs);
    
    // Log slow processing events (> 50ms threshold)
    if (latencyNs > 50_000_000) { // 50ms in nanoseconds
      logThrottled(
          "_latency_warning_" + runtime.ticker,
          String.format(
              "High latency detected for %s: %.2fms (threshold: 50ms)",
              runtime.ticker, latencyNs / 1_000_000.0),
          5);
    }

    if (runtime.openPosition != null) {
      manageOpenPosition(runtime, context, bestBid, spread, paper);
      return;
    }

    if (System.currentTimeMillis() < runtime.cooldownUntilMs) {
      return;
    }

    if (System.currentTimeMillis() < marketDataRecoveryUntilMs) {
      emitSkipDiagnostic(runtime.ticker, "market_data_recovery", Map.of());
      return;
    }

    if (!hasPositiveExpectedValue(runtime.ticker)) {
      emitSkipDiagnostic(
          runtime.ticker,
          "expected_edge_below_fees",
          Map.of(
              "spread",
              spread,
              "spreadBps",
              context.getSpreadBps(),
              "entryAsk",
              bestAsk,
              "commissionRate",
              estimateCommissionRate(runtime.ticker),
              "roundTripFeeBps",
              toBps(roundTripFeeFraction(runtime.ticker)),
              "tpBps",
              toBps(feeDistanceFraction(runtime.ticker, config.getTargetFeeMultiple())),
              "slBps",
              toBps(feeDistanceFraction(runtime.ticker, config.getStopFeeMultiple())),
              "expectedValueBps",
              toBps(expectedValueFraction(runtime.ticker)),
              "expectedWinRate",
              config.getExpectedWinRate()));
      return;
    }

    double adjustedMinFlow = getTimeAdjustedMinTradeFlow();
    if (Math.abs(context.getTradeDelta()) < adjustedMinFlow * MIN_ENTRY_FLOW_MULTIPLIER) {
      emitSkipDiagnostic(
          runtime.ticker,
          "trade_flow_too_weak",
          Map.of(
              "flow",
              context.getTradeDelta(),
              "requiredFlow",
              adjustedMinFlow * MIN_ENTRY_FLOW_MULTIPLIER,
              "obi",
              context.getObi(),
              "microEdge",
              context.getMicroEdge(),
              "spreadBps",
              context.getSpreadBps()));
      return;
    }

    if (riskManager != null && !riskManager.canTrade(initialEquity + tradeStats.netPnl)) {
      return;
    }

    if (killSwitch != null && !killSwitch.isTradingAllowed()) {
      return;
    }

    // Capital-aware check: skip signal generation if insufficient capital
    if (!hasSufficientCapital(runtime.ticker, bestAsk)) {
      return;
    }

    // Blocklist check: skip if ticker is temporarily blocked due to broker errors
    if (tickerBlocklist.isBlocked(runtime.ticker)) {
      emitSkipDiagnostic(
          runtime.ticker,
          "ticker_blocked",
          Map.of(
              "remainingMs",
              tickerBlocklist.getRemainingMs(runtime.ticker)));
      return;
    }

    boolean inRecovery = System.currentTimeMillis() < marketDataRecoveryUntilMs;
    double longQuality = calculateEntryQuality(context, inRecovery, false);
    double dynamicThreshold = calculateDynamicQualityThreshold(runtime.ticker, context.getSpreadBps());
    if (longQuality < dynamicThreshold) {
      emitSkipDiagnostic(
          runtime.ticker,
          "entry_quality_below_threshold",
          Map.of(
              "quality",
              longQuality,
              "threshold",
              dynamicThreshold,
              "obi",
              context.getObi(),
              "microEdge",
              context.getMicroEdge(),
              "tradeDelta",
              context.getTradeDelta(),
              "spreadBps",
              context.getSpreadBps()));
    } else if (!trendFilter.allowsDirection(mid, obi, tradeDelta, true)) {
      emitSkipDiagnostic(
          runtime.ticker,
          "trend_filter_blocks_long",
          Map.of(
              "quality",
              longQuality,
              "obi",
              context.getObi(),
              "microEdge",
              context.getMicroEdge(),
              "tradeDelta",
              context.getTradeDelta(),
              "weightedDepthImbalance",
              context.getWeightedDepthImbalance()));
    } else {
      for (OrderBookSignal signal : signals) {
        OrderBookEntryDecision decision = signal.evaluateEntry(context, runtime.ticker);
        if (!decision.isEnter()) {
          continue;
        }
        openLong(
            runtime,
            bestBid,
            bestAsk,
            spread,
            signal.id(),
            decision.getDescription(),
            longQuality,
            paper);
        for (OrderBookSignal s : signals) {
          s.reset(runtime.ticker);
        }
        break;
      }
    }

    if (config.isShortsEnabled() && runtime.openPosition == null) {
      double shortQuality = calculateEntryQuality(context, inRecovery, true);
      if (shortQuality < dynamicThreshold) {
        emitSkipDiagnostic(
            runtime.ticker,
            "short_entry_quality_below_threshold",
            Map.of(
                "quality",
                shortQuality,
                "threshold",
                dynamicThreshold,
                "obi",
                context.getObi(),
                "microEdge",
                context.getMicroEdge(),
                "tradeDelta",
                context.getTradeDelta(),
                "spreadBps",
                context.getSpreadBps()));
      } else if (!trendFilter.allowsDirection(mid, obi, tradeDelta, false)) {
        emitSkipDiagnostic(
            runtime.ticker,
            "trend_filter_blocks_short",
            Map.of(
                "quality",
                shortQuality,
                "obi",
                context.getObi(),
                "microEdge",
                context.getMicroEdge(),
                "tradeDelta",
                context.getTradeDelta(),
                "weightedDepthImbalance",
                context.getWeightedDepthImbalance()));
      } else {
        for (OrderBookSignal signal : signals) {
          OrderBookEntryDecision decision = signal.evaluateEntryShort(context, runtime.ticker);
          if (!decision.isEnter()) {
            continue;
          }
          openShort(
              runtime,
              bestBid,
              bestAsk,
              spread,
              signal.id(),
              decision.getDescription(),
              shortQuality,
              paper);
          for (OrderBookSignal s : signals) {
            s.reset(runtime.ticker);
          }
          break;
        }
      }
    }
  }

  private void manageOpenPosition(
      TickerRuntime runtime,
      OrderBookMarketContext context,
      double bestBid,
      double spread,
      boolean paper) {
    OpenPosition position = runtime.openPosition;
    if (position == null) {
      return;
    }

    boolean isLong = "LONG".equals(position.direction);
    double currentPrice = isLong ? bestBid : context.getBestAsk();

    long heldSeconds = Duration.between(position.entryTime, Instant.now()).getSeconds();
    boolean inGracePeriod = heldSeconds < config.getEntryGraceSeconds();

    // Check TP first — profitable positions should exit at target, not by time
    if (!inGracePeriod) {
      boolean tpHit =
          isLong
              ? currentPrice >= position.takeProfitPrice
              : currentPrice <= position.takeProfitPrice;
      if (tpHit) {
        closeOpenPosition(runtime, "take_profit", paper);
        return;
      }
    }

    // time_stop after TP check — gives TP a chance to hit before forced exit
    long effectiveMaxHoldSeconds = position.isRestored
        ? config.getMaxHoldSeconds() / 2
        : config.getMaxHoldSeconds();
    if (heldSeconds >= effectiveMaxHoldSeconds) {
      closeOpenPosition(runtime, "time_stop", paper);
      return;
    }

    if (inGracePeriod) {
      return;
    }

    if (config.isTrailingEnabled() && position.spreadAtEntry > 0) {
      double profit =
          isLong ? currentPrice - position.entryPrice : position.entryPrice - currentPrice;
      double newSl;
      if (config.isSwingMode()) {
        double activation = config.getTrailingActivationBps() / 10_000.0 * position.entryPrice;
        double step = config.getTrailingStepBps() / 10_000.0 * position.entryPrice;
        if (profit >= activation) {
          newSl = isLong ? currentPrice - step : currentPrice + step;
          boolean slImproved =
              isLong ? newSl > position.stopLossPrice : newSl < position.stopLossPrice;
          if (slImproved) {
            position.stopLossPrice = newSl;
          }
        }
      } else {
        double activationThreshold = config.getTrailingActivationSpreads() * position.spreadAtEntry;
        if (profit >= activationThreshold) {
          newSl =
              isLong
                  ? currentPrice - config.getTrailingStepSpreads() * position.spreadAtEntry
                  : currentPrice + config.getTrailingStepSpreads() * position.spreadAtEntry;
          boolean slImproved =
              isLong ? newSl > position.stopLossPrice : newSl < position.stopLossPrice;
          if (slImproved) {
            position.stopLossPrice = newSl;
          }
        }
      }
    }

    boolean slHit =
        isLong ? currentPrice <= position.stopLossPrice : currentPrice >= position.stopLossPrice;
    if (slHit) {
      closeOpenPosition(runtime, "stop_loss", paper);
      return;
    }

    if (config.isSwingMode()) {
      return;
    }

    double microExitThreshold = config.getEdgeSpreadFraction() * spread;
    boolean weakObiForDirection =
        isLong
            ? context.getObi() < Math.max(0.0, config.getObiThreshold() * 0.5)
            : context.getObi() > -Math.max(0.0, config.getObiThreshold() * 0.5);
    boolean reversingMicro =
        (isLong && context.getMicroEdge() < 0) || (!isLong && context.getMicroEdge() > 0);
    if (reversingMicro && weakObiForDirection) {
      position.microReversalTicks++;
    } else {
      position.microReversalTicks = 0;
    }
    boolean strongMicroReversal =
        isLong
            ? context.getMicroEdge() < -microExitThreshold && weakObiForDirection
            : context.getMicroEdge() > microExitThreshold && weakObiForDirection;
    if (position.microReversalTicks >= EXIT_PERSISTENCE_TICKS || strongMicroReversal) {
      closeOpenPosition(runtime, "microprice_reversal", paper);
      return;
    }

    double flowExitThreshold = config.getMinTradeFlow() * FLOW_REVERSAL_EXIT_MULTIPLIER;
    boolean reversingFlow =
        (isLong && context.getTradeDelta() < -flowExitThreshold)
            || (!isLong && context.getTradeDelta() > flowExitThreshold);
    if (reversingFlow && weakObiForDirection) {
      position.flowReversalTicks++;
    } else {
      position.flowReversalTicks = 0;
    }
    boolean strongFlowReversal = Math.abs(context.getTradeDelta()) > config.getMinTradeFlow() * 4;
    if (position.flowReversalTicks >= EXIT_PERSISTENCE_TICKS
        || (strongFlowReversal && weakObiForDirection)) {
      closeOpenPosition(runtime, "flow_reversal", paper);
      return;
    }

    if (context.getSpreadBps() > config.getMaxSpreadBps() * 1.5) {
      closeOpenPosition(runtime, "spread_widen", paper);
      return;
    }

    OrderBookSignal activeSignal = signalsById.get(position.signalId);
    if (activeSignal == null) {
      return;
    }
    String signalExit = activeSignal.evaluateExit(context, position, runtime.ticker);
    if (signalExit != null) {
      closeOpenPosition(runtime, signalExit, paper);
    }
  }

  private double calculateEntryQuality(
      OrderBookMarketContext context, boolean inRecovery, boolean shortSide) {
    double obi = shortSide ? -context.getObi() : context.getObi();
    double microEdge = shortSide ? -context.getMicroEdge() : context.getMicroEdge();
    double obiScore =
        obi >= ENTRY_QUALITY_OBI_MIN
            ? Math.min(1.0, (obi - ENTRY_QUALITY_OBI_MIN) / 0.5 + 0.5)
            : 0.0;

    double edgeThreshold =
        config.getEdgeSpreadFraction() * ObiScalpSignal.EDGE_FRACTION * 1.5 * context.getSpread();
    double edgeScore = edgeThreshold > 0.0 ? microEdge / edgeThreshold : 0.0;
    edgeScore = Math.min(1.0, Math.max(0.0, edgeScore));

    double flowRequired = config.getMinTradeFlow() * MIN_ENTRY_FLOW_MULTIPLIER;
    double flowScore = flowRequired > 0.0 ? Math.abs(context.getTradeDelta()) / flowRequired : 0.0;
    flowScore = Math.min(1.0, Math.max(0.0, flowScore));

    double spreadQuality = Math.max(0.0, 1.0 - context.getSpreadBps() / config.getMaxSpreadBps());

    double recoveryScore = inRecovery ? 0.0 : 1.0;

    // Density score: weighted depth imbalance aligned with direction
    double depthImb = shortSide ? -context.getWeightedDepthImbalance() : context.getWeightedDepthImbalance();
    double depthScore = Math.min(1.0, Math.max(0.0, (depthImb + 1.0) / 2.0));
    depthScore = depthImb > 0.0 ? depthScore : 0.0;

    // Absorption score: absorption aligned with direction
    double absorption = shortSide ? -context.getAbsorptionScore() : context.getAbsorptionScore();
    double absorptionScoreVal = Math.min(1.0, Math.max(0.0, absorption * 2.0 + 0.5));

    double densityComponent = (depthScore * 0.6 + absorptionScoreVal * 0.4) * 0.15;

    return obiScore * ENTRY_QUALITY_OBI_WEIGHT
        + edgeScore * ENTRY_QUALITY_EDGE_WEIGHT
        + flowScore * ENTRY_QUALITY_FLOW_WEIGHT
        + spreadQuality * ENTRY_QUALITY_SPREAD_WEIGHT
        + recoveryScore * ENTRY_QUALITY_RECOVERY_WEIGHT
        + densityComponent;
  }

  private void emitSkipDiagnostic(String ticker, String reason, Map<String, Object> metrics) {
    if (!shouldEmitSkipDiagnostic(ticker, reason)) {
      return;
    }
    emitDiagnostic(OrderBookDiagnosticEventType.ENTRY_SKIPPED, ticker, reason, metrics);
  }

  private boolean shouldEmitSkipDiagnostic(String ticker, String reason) {
    long now = System.currentTimeMillis();
    String key = ticker + "|" + reason;
    Long last = lastSkipDiagnosticMsByKey.get(key);
    if (last != null && now - last < SKIP_DIAGNOSTIC_THROTTLE_MS) {
      return false;
    }
    lastSkipDiagnosticMsByKey.put(key, now);
    return true;
  }

  private void openLong(
      TickerRuntime runtime,
      double entryBid,
      double entryAsk,
      double spread,
      String signalId,
      String signalDescription,
      double quality,
      boolean paper) {
    if (!tcsService.isTradableForAccount(runtime.tickerInfo)) {
      log("Skip OPEN " + runtime.ticker + ": not tradable for current account");
      return;
    }

    // Volatility and time-of-day adjusted position sizing
    double midPrice = (entryBid + entryAsk) / 2.0;
    double adjustedCash = volatilityAdjustedPositionCash(runtime.ticker, midPrice);
    if (adjustedCash <= 0.0) {
      log("Skip OPEN " + runtime.ticker + ": outside trading hours or low liquidity window");
      return;
    }
    int units = tcsService.calculateTradeCount(runtime.key, adjustedCash, entryAsk);
    if (units <= 0) {
      logThrottled(runtime.ticker + "_insufficient_cash_open",
          "Skip OPEN " + runtime.ticker + ": insufficient cash for one lot " +
          "(adjusted=" + String.format("%.0f", adjustedCash) + ", session=" + currentSessionLabel() + ")", 5);
      return;
    }

    // Apply fee-aware cap to prevent excessive commission drag
    units = calculateFeeAwareUnits(units, entryAsk, runtime.ticker);

    // Verify sufficient cash before placing order
    int lot = runtime.tickerInfo.getLot() != null ? Math.max(1, runtime.tickerInfo.getLot()) : 1;
    double requiredCash = units * entryAsk * lot;
    double currentAvailableCash = 0.0;
    try {
      currentAvailableCash = tcsService.getAvailableCash();
    } catch (Exception ex) {
      log("Cannot fetch available cash for " + runtime.ticker + ": " + ex.getMessage());
    }
    if (currentAvailableCash < requiredCash) {
      log("Skip OPEN " + runtime.ticker + " LONG: insufficient cash"
          + " (required=" + String.format("%.2f", requiredCash)
          + ", available=" + String.format("%.2f", currentAvailableCash)
          + ", units=" + units + ")");
      return;
    }

    // Dynamic TP/SL based on volatility
    double[] tpSl = calculateDynamicTpSl(runtime.ticker, entryAsk, true);

    // Override TP with density-based DynamicTakeProfit when snapshot available (subtask 14)
    MarketDepthSnapshot currentSnapshot = runtime.latestSnapshot;
    if (currentSnapshot != null && currentSnapshot.isConsistent()) {
      double densityTp = dynamicTakeProfit.calculateTakeProfit(
          entryAsk, true, currentSnapshot, runtime.ticker);
      if (densityTp > entryAsk) {
        tpSl[0] = densityTp;
      }
    }

    BracketPrices bracket = new BracketPrices(tpSl[0], tpSl[1]);

    log(
        "OPEN signal ["
            + signalId
            + "] "
            + runtime.ticker
            + ": "
            + signalDescription
            + ", entryAsk="
            + entryAsk
            + ", session="
            + currentSessionLabel()
            + ", volAdj="
            + String.format("%.2f", adjustedCash / effectivePositionCash()));

    if (paper) {
      double entryValue = units * entryAsk;
      runtime.openPosition =
          new OpenPosition(
              signalId,
              "LONG",
              entryAsk,
              spread,
              Instant.now(),
              bracket.tpPrice,
              bracket.slPrice,
              units,
              entryValue,
              entryValue * config.getCommissionRate());
      runtime.cooldownUntilMs = System.currentTimeMillis() + config.getCooldownSeconds() * 1000L;
      emitDiagnostic(
          OrderBookDiagnosticEventType.ENTRY_OPENED,
          runtime.ticker,
          signalId,
          Map.of(
              "direction",
              "LONG",
              "entryPrice",
              entryAsk,
              "units",
              units,
              "spread",
              spread,
              "quality",
              quality,
              "roundTripFeeBps",
              toBps(roundTripFeeFraction(runtime.ticker)),
              "tpBps",
              toBps(feeDistanceFraction(runtime.ticker, config.getTargetFeeMultiple())),
              "slBps",
              toBps(feeDistanceFraction(runtime.ticker, config.getStopFeeMultiple())),
              "expectedValueBps",
              toBps(expectedValueFraction(runtime.ticker))));
      log(
          "PAPER OPEN ["
              + signalId
              + "] "
              + runtime.ticker
              + " entry="
              + entryAsk
              + " units="
              + units
              + " tp="
              + bracket.tpPrice
              + " sl="
              + bracket.slPrice);
      return;
    }

    // Place server-side stop-loss if enabled (TODO.md Section 5)
    String brokerOrderId = "ob_" + runtime.ticker + "_" + System.currentTimeMillis();
    String brokerStopLossOrderId = null;
    double brokerStopLossPrice = 0.0;
    
    TCSService.OrderExecutionResult result = placeBuyOrderWithRetry(runtime, brokerOrderId);

    if (!result.isSuccess()) {
      log("OPEN failed for " + runtime.ticker);
      runtime.cooldownUntilMs = System.currentTimeMillis() + config.getCooldownSeconds() * 1000L;
      return;
    }

    double executedEntry = result.getExecutedPrice() != null ? result.getExecutedPrice() : entryAsk;
    int executedUnits = result.getExecutedCount() > 0 ? result.getExecutedCount() : units;

    // Track slippage on order fill (subtask 14)
    slippageTracker.recordTrade("LONG", entryAsk, executedEntry);

    // Process partial fills if detected (subtask 14)
    if (executedUnits < units && executedUnits > 0) {
      PartialFillHandler.OrderReport report = new PartialFillHandler.OrderReport(
          runtime.ticker, "ob_" + runtime.ticker + "_" + System.currentTimeMillis(),
          "BUY", units, executedUnits, executedEntry);
      PartialFillHandler.FillResult fillResult = partialFillHandler.processExecution(report);
      log("Partial fill detected for " + runtime.ticker
          + " LONG: filled=" + fillResult.getFilledQuantity()
          + ", unfilled=" + fillResult.getUnfilledQuantity()
          + ", action=" + fillResult.getRecommendedAction());
    }

    BracketPrices executedBracket = buildBracketPrices(runtime.ticker, entryBid, executedEntry);
    double entryValue = executedUnits * executedEntry;

    // Place server-side stop-loss order if enabled (not in sandbox mode - server stops not supported there)
    if (config.isServerStopEnabled() && !mainConfig.isTestMode() && !mainConfig.isSandbox()) {
      var stopLossResult = placeServerStopLossOrder(runtime, executedUnits, executedBracket.slPrice, "BUY");
      if (stopLossResult != null && stopLossResult.orderId != null) {
        brokerStopLossOrderId = stopLossResult.orderId;
        brokerStopLossPrice = executedBracket.slPrice;
        log("Server SL placed for " + runtime.ticker + ": orderId=" + brokerStopLossOrderId + ", price=" + brokerStopLossPrice);
      } else {
        log("WARN: Server SL failed for " + runtime.ticker);
      }
    } else if (mainConfig.isSandbox()) {
      // In sandbox mode, track SL/TP client-side only
      brokerStopLossPrice = executedBracket.slPrice;
      log("Sandbox mode: SL tracked client-side for " + runtime.ticker + ", price=" + brokerStopLossPrice);
    }
    
    runtime.openPosition =
        new OpenPosition(
            signalId,
            "LONG",
            executedEntry,
            spread,
            Instant.now(),
            executedBracket.tpPrice,
            executedBracket.slPrice,
            executedUnits,
            entryValue,
            result.getCommission(),
            false,
            brokerOrderId,
            brokerStopLossOrderId,
            brokerStopLossPrice);
    recordRealizedCommission(runtime.ticker, entryValue, result.getCommission());
    persistPosition(runtime);
    runtime.cooldownUntilMs = System.currentTimeMillis() + config.getCooldownSeconds() * 1000L;
    emitDiagnostic(
        OrderBookDiagnosticEventType.ENTRY_OPENED,
        runtime.ticker,
        signalId,
        Map.of(
            "direction",
            "LONG",
            "entryPrice",
            executedEntry,
            "units",
            executedUnits,
            "spread",
            spread,
            "fees",
            result.getCommission(),
            "quality",
            quality,
            "roundTripFeeBps",
            toBps(roundTripFeeFraction(runtime.ticker)),
            "tpBps",
            toBps(feeDistanceFraction(runtime.ticker, config.getTargetFeeMultiple())),
            "slBps",
            toBps(feeDistanceFraction(runtime.ticker, config.getStopFeeMultiple())),
            "expectedValueBps",
            toBps(expectedValueFraction(runtime.ticker))));
  }

  private TCSService.OrderExecutionResult placeBuyOrderWithRetry(TickerRuntime runtime, String clientOrderId) {
    TCSService.OrderExecutionResult result = TCSService.OrderExecutionResult.failed();
    for (int attempt = 1; attempt <= ORDER_PLACE_ATTEMPTS; attempt++) {
      result =
          tcsService.buyByMarketWithDetails(
              runtime.ticker, runtime.key.getType(), config.getPositionCash(), 0.0, 0.0);
      if (result.isSuccess()) {
        return result;
      }
      // Block ticker on confirmation-required error (code 90001)
      if (result.getErrorCode() == 90001) {
        tickerBlocklist.block(runtime.ticker, config.getBlocklistDurationMs());
        log("Ticker " + runtime.ticker + " blocked for "
            + (config.getBlocklistDurationMs() / 3600000) + "h due to confirmation required error");
        return result;
      }
      if (attempt < ORDER_PLACE_ATTEMPTS) {
        log("OPEN attempt " + attempt + " failed for " + runtime.ticker + ", retrying");
        sleep(ORDER_RETRY_DELAY_MS);
      }
    }
    return result;
  }
  
  /**
   * Places server-side stop-loss order for position protection (TODO.md Section 5).
   * @return OrderId if successful, null otherwise
   */
  private TCSService.StopLossOrderResult placeServerStopLossOrder(
      TickerRuntime runtime, int units, double stopLossPrice, String direction) {
    try {
      // For BUY positions, SL is a SELL order at stopLossPrice
      // For SHORT positions, SL is a BUY order at stopLossPrice
      String operation = "BUY".equals(direction) ? "Sell" : "Buy";
      var result = tcsService.createStopLossOrder(
          runtime.key,
          units,
          stopLossPrice,
          operation);
      return result;
    } catch (Exception e) {
      log("Server SL placement failed for " + runtime.ticker + ": " + e.getMessage());
      return null;
    }
  }

  private TCSService.OrderExecutionResult sellByMarketWithRetry(
      TickerRuntime runtime, double cashToSell, double fallbackEntryBid) {
    TCSService.OrderExecutionResult result = TCSService.OrderExecutionResult.failed();
    for (int attempt = 1; attempt <= ORDER_PLACE_ATTEMPTS; attempt++) {
      result =
          tcsService.sellByMarketWithDetails(
              runtime.ticker, runtime.key.getType(), cashToSell, 0.0, 0.0);
      if (result.isSuccess()) {
        return result;
      }
      // Block ticker on confirmation-required error (code 90001)
      if (result.getErrorCode() == 90001) {
        tickerBlocklist.block(runtime.ticker, config.getBlocklistDurationMs());
        log("Ticker " + runtime.ticker + " blocked for "
            + (config.getBlocklistDurationMs() / 3600000) + "h due to confirmation required error");
        return result;
      }
      if (attempt < ORDER_PLACE_ATTEMPTS) {
        log(
            "SHORT OPEN attempt "
                + attempt
                + " failed for "
                + runtime.ticker
                + ", retrying from bid="
                + fallbackEntryBid);
        sleep(ORDER_RETRY_DELAY_MS);
      }
    }
    return result;
  }

  private void openShort(
      TickerRuntime runtime,
      double entryBid,
      double entryAsk,
      double spread,
      String signalId,
      String signalDescription,
      double quality,
      boolean paper) {
    if (!tcsService.isTradableForAccount(runtime.tickerInfo)) {
      log("Skip SHORT " + runtime.ticker + ": not tradable for current account");
      return;
    }

    // Volatility and time-of-day adjusted position sizing
    double midPrice = (entryBid + entryAsk) / 2.0;
    double adjustedCash = volatilityAdjustedPositionCash(runtime.ticker, midPrice);
    if (adjustedCash <= 0.0) {
      log("Skip SHORT " + runtime.ticker + ": outside trading hours or low liquidity window");
      return;
    }
    int units = tcsService.calculateTradeCount(runtime.key, adjustedCash, entryBid);
    if (units <= 0) {
      logThrottled(runtime.ticker + "_insufficient_cash_short",
          "Skip SHORT " + runtime.ticker + ": insufficient cash for one lot " +
          "(adjusted=" + String.format("%.0f", adjustedCash) + ", session=" + currentSessionLabel() + ")", 5);
      return;
    }

    // Apply fee-aware cap to prevent excessive commission drag
    units = calculateFeeAwareUnits(units, entryBid, runtime.ticker);

    // Verify sufficient cash/margin before placing short order
    int lot = runtime.tickerInfo.getLot() != null ? Math.max(1, runtime.tickerInfo.getLot()) : 1;
    double requiredCash = units * entryBid * lot;
    double currentAvailableCash = 0.0;
    try {
      currentAvailableCash = tcsService.getAvailableCash();
    } catch (Exception ex) {
      log("Cannot fetch available cash for " + runtime.ticker + ": " + ex.getMessage());
    }
    if (currentAvailableCash < requiredCash) {
      log("Skip SHORT " + runtime.ticker + ": insufficient cash/margin"
          + " (required=" + String.format("%.2f", requiredCash)
          + ", available=" + String.format("%.2f", currentAvailableCash)
          + ", units=" + units + ")");
      return;
    }

    TCSService.OrderExecutionResult result =
        sellByMarketWithRetry(runtime, adjustedCash, entryBid);

    // Dynamic TP/SL based on volatility
    double[] tpSl = calculateDynamicTpSl(runtime.ticker, entryBid, false);

    // Override TP with density-based DynamicTakeProfit when snapshot available (subtask 14)
    MarketDepthSnapshot currentSnapshot = runtime.latestSnapshot;
    if (currentSnapshot != null && currentSnapshot.isConsistent()) {
      double densityTp = dynamicTakeProfit.calculateTakeProfit(
          entryBid, false, currentSnapshot, runtime.ticker);
      if (densityTp > 0 && densityTp < entryBid) {
        tpSl[0] = densityTp;
      }
    }

    BracketPrices bracket = new BracketPrices(tpSl[0], tpSl[1]);

    log(
        "SHORT signal ["
            + signalId
            + "] "
            + runtime.ticker
            + ": "
            + signalDescription
            + ", entryBid="
            + entryBid
            + ", session="
            + currentSessionLabel()
            + ", volAdj="
            + String.format("%.2f", adjustedCash / effectivePositionCash()));

    if (paper) {
      double entryValue = units * entryBid;
      runtime.openPosition =
          new OpenPosition(
              signalId,
              "SHORT",
              entryBid,
              spread,
              Instant.now(),
              bracket.tpPrice,
              bracket.slPrice,
              units,
              entryValue,
              entryValue * config.getCommissionRate());
      runtime.cooldownUntilMs = System.currentTimeMillis() + config.getCooldownSeconds() * 1000L;
      emitDiagnostic(
          OrderBookDiagnosticEventType.ENTRY_OPENED,
          runtime.ticker,
          signalId,
          Map.of(
              "direction",
              "SHORT",
              "entryPrice",
              entryBid,
              "units",
              units,
              "spread",
              spread,
              "quality",
              quality,
              "roundTripFeeBps",
              toBps(roundTripFeeFraction(runtime.ticker)),
              "tpBps",
              toBps(feeDistanceFraction(runtime.ticker, config.getTargetFeeMultiple())),
              "slBps",
              toBps(feeDistanceFraction(runtime.ticker, config.getStopFeeMultiple())),
              "expectedValueBps",
              toBps(expectedValueFraction(runtime.ticker))));
      log(
          "PAPER SHORT ["
              + signalId
              + "] "
              + runtime.ticker
              + " entry="
              + entryBid
              + " units="
              + units
              + " tp="
              + bracket.tpPrice
              + " sl="
              + bracket.slPrice);
      return;
    }

    if (!result.isSuccess()) {
      log("SHORT failed for " + runtime.ticker);
      runtime.cooldownUntilMs = System.currentTimeMillis() + config.getCooldownSeconds() * 1000L;
      return;
    }

    double executedEntry = result.getExecutedPrice() != null ? result.getExecutedPrice() : entryBid;
    int executedUnits = result.getExecutedCount() > 0 ? result.getExecutedCount() : units;

    // Track slippage on order fill (subtask 14)
    slippageTracker.recordTrade("SHORT", entryBid, executedEntry);

    // Process partial fills if detected (subtask 14)
    if (executedUnits < units && executedUnits > 0) {
      PartialFillHandler.OrderReport report = new PartialFillHandler.OrderReport(
          runtime.ticker, "ob_" + runtime.ticker + "_" + System.currentTimeMillis(),
          "SELL", units, executedUnits, executedEntry);
      PartialFillHandler.FillResult fillResult = partialFillHandler.processExecution(report);
      log("Partial fill detected for " + runtime.ticker
          + " SHORT: filled=" + fillResult.getFilledQuantity()
          + ", unfilled=" + fillResult.getUnfilledQuantity()
          + ", action=" + fillResult.getRecommendedAction());
    }

    BracketPrices executedBracket =
        buildBracketPricesShort(runtime.ticker, entryBid, executedEntry);
    double entryValue = executedUnits * executedEntry;

    // Track SL/TP client-side in sandbox mode (server stops not supported)
    String brokerStopLossOrderId = null;
    double brokerStopLossPrice = 0.0;
    if (mainConfig.isSandbox()) {
      brokerStopLossPrice = executedBracket.slPrice;
      log("Sandbox mode: SHORT SL tracked client-side for " + runtime.ticker + ", price=" + brokerStopLossPrice);
    }
    
    runtime.openPosition =
        new OpenPosition(
            signalId,
            "SHORT",
            executedEntry,
            spread,
            Instant.now(),
            executedBracket.tpPrice,
            executedBracket.slPrice,
            executedUnits,
            entryValue,
            result.getCommission(),
            false,
            null,  // brokerOrderId
            brokerStopLossOrderId,
            brokerStopLossPrice);
    recordRealizedCommission(runtime.ticker, entryValue, result.getCommission());
    persistPosition(runtime);
    runtime.cooldownUntilMs = System.currentTimeMillis() + config.getCooldownSeconds() * 1000L;
    emitDiagnostic(
        OrderBookDiagnosticEventType.ENTRY_OPENED,
        runtime.ticker,
        signalId,
        Map.of(
            "direction",
            "SHORT",
            "entryPrice",
            executedEntry,
            "units",
            executedUnits,
            "spread",
            spread,
            "fees",
            result.getCommission(),
            "quality",
            quality,
            "roundTripFeeBps",
            toBps(roundTripFeeFraction(runtime.ticker)),
            "tpBps",
            toBps(feeDistanceFraction(runtime.ticker, config.getTargetFeeMultiple())),
            "slBps",
            toBps(feeDistanceFraction(runtime.ticker, config.getStopFeeMultiple())),
            "expectedValueBps",
            toBps(expectedValueFraction(runtime.ticker))));
  }

  private BracketPrices buildBracketPricesShort(String ticker, double entryBid, double entryAsk) {
    double tpDistance = feeDistanceFraction(ticker, config.getTargetFeeMultiple()) * entryBid;
    double slDistance = feeDistanceFraction(ticker, config.getStopFeeMultiple()) * entryAsk;
    double tpPrice = Math.max(0.0, entryBid - tpDistance);
    double slPrice = entryAsk + slDistance;
    return new BracketPrices(tpPrice, slPrice);
  }

  private void closeOpenPosition(TickerRuntime runtime, String reason, boolean paper) {
    OpenPosition position = runtime.openPosition;
    if (position == null) {
      return;
    }

    if (paper) {
      Map<String, Map<Double, Integer>> book = tcsService.getCurrentPrices(runtime.key, false);
      boolean isLong = "LONG".equals(position.direction);
      double grossPnl;
      double exitPrice;
      if (isLong) {
        // closing LONG: sell at BID
        double fallback = position.entryPrice - position.spreadAtEntry;
        exitPrice = resolveBestBid(book, fallback);
        grossPnl = (exitPrice - position.entryPrice) * position.units;
      } else {
        // closing SHORT: buy at ASK
        double fallback = position.entryPrice + position.spreadAtEntry;
        exitPrice = resolveBestAsk(book, fallback);
        grossPnl = (position.entryPrice - exitPrice) * position.units;
      }
      double exitValue = position.units * exitPrice;
      double commission = (position.entryValue + exitValue) * config.getCommissionRate();
      double netPnl = grossPnl - commission;
      tradeStats.record(netPnl);
      if (riskManager != null) {
        riskManager.registerTrade(netPnl);
      }

      // Update signal performance tracking (subtask 14)
      signalPerformanceTracker.recordTrade(position.signalId, netPnl);
      if (config.isAdaptiveParamsEnabled()) {
        adaptiveParameters.update(position.signalId);
      }

      emitDiagnostic(
          OrderBookDiagnosticEventType.POSITION_CLOSED,
          runtime.ticker,
          reason,
          Map.of(
              "signalId", position.signalId,
              "direction", position.direction,
              "entryPrice", position.entryPrice,
              "exitPrice", exitPrice,
              "grossPnl", grossPnl,
              "netPnl", netPnl,
              "fees", commission));
      log(
          "PAPER CLOSE ["
              + position.signalId
              + "] "
              + runtime.ticker
              + " reason="
              + reason
              + " entry="
              + position.entryPrice
              + " exit="
              + exitPrice
              + " gross="
              + String.format("%.2f", grossPnl)
              + " commission="
              + String.format("%.2f", commission)
              + " net="
              + String.format("%.2f", netPnl));
      runtime.openPosition = null;
      runtime.cooldownUntilMs = System.currentTimeMillis() + config.getCooldownSeconds() * 1000L;
      logStatsIfNeeded();
      return;
    }

    TCSService.OrderExecutionResult result = closePositionWithRetry(runtime);
    if (!result.isSuccess()) {
      if (isBrokerPositionGone(runtime)) {
        log(
            "CLOSE ["
                + position.signalId
                + "] "
                + runtime.ticker
                + " reason=externally_closed: no broker position, dropping tracking");
        emitDiagnostic(
            OrderBookDiagnosticEventType.POSITION_CLOSED,
            runtime.ticker,
            "externally_closed",
            Map.of(
                "signalId",
                position.signalId,
                "direction",
                position.direction,
                "entryPrice",
                position.entryPrice,
                "units",
                position.units));
        runtime.openPosition = null;
        runtime.cooldownUntilMs = System.currentTimeMillis() + config.getCooldownSeconds() * 1000L;
        positionStore.remove(runtime.ticker);
        logStatsIfNeeded();
        return;
      }
      log("CLOSE failed for " + runtime.ticker + " reason=" + reason);
      return;
    }

    double exitPrice =
        result.getExecutedPrice() != null ? result.getExecutedPrice() : position.entryPrice;
    double grossPnl =
        "SHORT".equals(position.direction)
            ? (position.entryPrice - exitPrice) * position.units
            : (exitPrice - position.entryPrice) * position.units;
    double exitCommission = result.getCommission();
    double exitNotional = position.units * exitPrice;
    recordRealizedCommission(runtime.ticker, exitNotional, exitCommission);
    double netPnl = grossPnl - position.entryCommission - exitCommission;
    tradeStats.record(netPnl);
    if (riskManager != null) {
      riskManager.registerTrade(netPnl);
    }

    // Update signal performance tracking (subtask 14)
    signalPerformanceTracker.recordTrade(position.signalId, netPnl);
    if (config.isAdaptiveParamsEnabled()) {
      adaptiveParameters.update(position.signalId);
    }

    long holdSeconds = Duration.between(position.entryTime, Instant.now()).getSeconds();

    // Cancel server-side stop-loss if it was set
    if (position.brokerStopLossOrderId != null && !position.brokerStopLossOrderId.isEmpty()) {
      try {
        tcsService.cancelStopOrder(runtime.key, position.brokerStopLossOrderId, "ServerSL");
        log("Cancelled server SL for " + runtime.ticker + ": orderId=" + position.brokerStopLossOrderId);
      } catch (Exception e) {
        log("Failed to cancel server SL for " + runtime.ticker + ": " + e.getMessage());
      }
    }
    
    // Record realized PnL for reporting (TODO.md Section 6)
    position.realizedPnl = netPnl;
    
    Map<String, Object> closeMetrics = new HashMap<>();
    closeMetrics.put("signalId", position.signalId);
    closeMetrics.put("direction", position.direction);
    closeMetrics.put("entryPrice", position.entryPrice);
    closeMetrics.put("exitPrice", exitPrice);
    closeMetrics.put("grossPnl", grossPnl);
    closeMetrics.put("netPnl", netPnl);
    closeMetrics.put("fees", position.entryCommission + exitCommission);
    closeMetrics.put("holdSeconds", holdSeconds);
    closeMetrics.put("units", position.units);
    closeMetrics.put("realizedPnl", netPnl);
    closeMetrics.put("serverSlCancelled", position.brokerStopLossOrderId != null);
    
    emitDiagnostic(
        OrderBookDiagnosticEventType.POSITION_CLOSED,
        runtime.ticker,
        reason,
        closeMetrics);
    runtime.openPosition = null;
    positionStore.remove(runtime.ticker);
    runtime.cooldownUntilMs = System.currentTimeMillis() + config.getCooldownSeconds() * 1000L;
    log(
        "CLOSE ["
            + position.signalId
            + "] "
            + runtime.ticker
            + " reason="
            + reason
            + " gross="
            + String.format("%.2f", grossPnl)
            + " entryCommission="
            + String.format("%.2f", position.entryCommission)
            + " exitCommission="
            + String.format("%.2f", exitCommission)
            + " holdSeconds="
            + Duration.between(position.entryTime, Instant.now()).getSeconds()
            + " net="
            + String.format("%.2f", netPnl));
    logStatsIfNeeded();
  }

  private boolean isBrokerPositionGone(TickerRuntime runtime) {
    try {
      int brokerCount =
          tcsService.getCountOfCurrentPositions(runtime.key.getType(), runtime.ticker);
      return brokerCount == 0;
    } catch (Exception ex) {
      log("Cannot verify broker position for " + runtime.ticker + ": " + ex.getMessage());
      return false;
    }
  }

  private TCSService.OrderExecutionResult closePositionWithRetry(TickerRuntime runtime) {
    TCSService.OrderExecutionResult result = TCSService.OrderExecutionResult.failed();
    boolean isShortShort = "SHORT".equals(runtime.openPosition.direction);
    String closeLabel = isShortShort ? "SHORT" : "LONG";
    for (int attempt = 1; attempt <= ORDER_PLACE_ATTEMPTS; attempt++) {
      result =
          isShortShort
              ? tcsService.closeShortByMarketWithDetails(runtime.ticker, runtime.key.getType())
              : tcsService.closeLongByMarketWithDetails(runtime.ticker, runtime.key.getType());
      if (result.isSuccess()) {
        return result;
      }
      if (attempt < ORDER_PLACE_ATTEMPTS) {
        log(
            "CLOSE attempt "
                + attempt
                + " failed for "
                + runtime.ticker
                + " ("
                + closeLabel
                + "), retrying");
        sleep(ORDER_RETRY_DELAY_MS);
      }
    }
    return result;
  }

  private BracketPrices buildBracketPrices(String ticker, double entryBid, double entryAsk) {
    double tpDistance = feeDistanceFraction(ticker, config.getTargetFeeMultiple()) * entryAsk;
    double slDistance = feeDistanceFraction(ticker, config.getStopFeeMultiple()) * entryBid;
    double tpPrice = entryAsk + tpDistance;
    double slPrice = Math.max(0.0, entryBid - slDistance);
    return new BracketPrices(tpPrice, slPrice);
  }

  private boolean hasPositiveExpectedValue(String ticker) {
    return expectedValueFraction(ticker) >= roundTripFeeFraction(ticker) * config.getEvGateBuffer();
  }

  private double roundTripFeeFraction(String ticker) {
    return estimateCommissionRate(ticker) * 2.0;
  }

  private double feeDistanceFraction(String ticker, double multiple) {
    return roundTripFeeFraction(ticker) * multiple;
  }

  private double expectedValueFraction(String ticker) {
    double winRate = config.getExpectedWinRate();
    double tpDistance = feeDistanceFraction(ticker, config.getTargetFeeMultiple());
    double slDistance = feeDistanceFraction(ticker, config.getStopFeeMultiple());
    return winRate * tpDistance - (1.0 - winRate) * slDistance;
  }

  private static double toBps(double fraction) {
    return fraction * 10_000.0;
  }

  /** Returns position cash minus commission reserve so the order never exceeds available funds. */
  private double effectivePositionCash() {
    return config.getPositionCash() / (1.0 + config.getCommissionRate());
  }

  /**
   * Calculates volatility-adjusted position cash for a specific ticker.
   *
   * <p>Applies two multipliers:
   * <ul>
   *   <li>Volatility multiplier: proportional to spread volatility (higher vol = larger position for scalping)</li>
   *   <li>Liquidity multiplier: based on time-of-day (MOEX session windows)</li>
   * </ul>
   *
   * @param ticker the instrument ticker
   * @param currentMidPrice current mid-price for volatility calculation
   * @return adjusted cash amount for position sizing
   */
  private double volatilityAdjustedPositionCash(String ticker, double currentMidPrice) {
    double baseCash = effectivePositionCash();

    if (!config.isVolatilitySizingEnabled() && !config.isTimeOfDayFilterEnabled()) {
      return baseCash;
    }

    double multiplier = 1.0;

    // Volatility adjustment
    if (config.isVolatilitySizingEnabled()) {
      VolatilityTracker tracker = volatilityTrackers.get(ticker);
      if (tracker != null && tracker.isReady()) {
        double volRatio = tracker.getVolatilityRatio(config.getVolatilityTargetSpreadBps(), currentMidPrice);
        // Clamp to configured bounds
        multiplier *= Math.max(config.getVolatilityMinMultiplier(),
                      Math.min(config.getVolatilityMaxMultiplier(), volRatio));
      }
    }

    // Time-of-day adjustment
    if (config.isTimeOfDayFilterEnabled()) {
      LocalTime currentTime = LocalTime.now(MSK_ZONE);
      double liquidityMultiplier = liquidityWindows.getLiquidityMultiplier(currentTime);
      if (liquidityMultiplier <= 0.0) {
        return 0.0; // Outside trading hours
      }
      multiplier *= liquidityMultiplier;
    }

    return baseCash * multiplier;
  }

  /**
   * Calculates dynamic TP/SL distances based on current volatility.
   *
   * <p>Uses ATR-proxy (spread volatility + price volatility) to set adaptive levels:
   * <ul>
   *   <li>TP = entryPrice ± (atrProxy * atrTpMultiplier)</li>
   *   <li>SL = entryPrice ∓ (atrProxy * atrSlMultiplier)</li>
   * </ul>
   *
   * @param ticker the instrument ticker
   * @param entryPrice entry price
   * @param isLong true for long position, false for short
   * @return array of [tpPrice, slPrice]
   */
  private double[] calculateDynamicTpSl(String ticker, double entryPrice, boolean isLong) {
    double defaultTpDistance = feeDistanceFraction(ticker, config.getTargetFeeMultiple()) * entryPrice;
    double defaultSlDistance = feeDistanceFraction(ticker, config.getStopFeeMultiple()) * entryPrice;

    if (!config.isDynamicTpSlEnabled()) {
      double tp = isLong ? entryPrice + defaultTpDistance : entryPrice - defaultTpDistance;
      double sl = isLong ? entryPrice - defaultSlDistance : entryPrice + defaultSlDistance;
      return new double[]{tp, sl};
    }

    VolatilityTracker tracker = volatilityTrackers.get(ticker);
    if (tracker == null || !tracker.isReady()) {
      // Not enough data, use defaults
      double tp = isLong ? entryPrice + defaultTpDistance : entryPrice - defaultTpDistance;
      double sl = isLong ? entryPrice - defaultSlDistance : entryPrice + defaultSlDistance;
      return new double[]{tp, sl};
    }

    // ATR proxy = spread volatility + mid-price volatility
    double atrProxy = tracker.getCombinedVolatility();
    if (atrProxy <= 0.0) {
      double tp = isLong ? entryPrice + defaultTpDistance : entryPrice - defaultTpDistance;
      double sl = isLong ? entryPrice - defaultSlDistance : entryPrice + defaultSlDistance;
      return new double[]{tp, sl};
    }

    double tpDistance = atrProxy * config.getAtrTpMultiplier();
    double slDistance = atrProxy * config.getAtrSlMultiplier();

    // Ensure minimum distances (at least 1 spread to cover fees)
    double minDistance = tracker.getAverageSpread() * 2.0;
    tpDistance = Math.max(tpDistance, minDistance * config.getTargetFeeMultiple());
    slDistance = Math.max(slDistance, minDistance * config.getStopFeeMultiple());

    double tp = isLong ? entryPrice + tpDistance : entryPrice - tpDistance;
    double sl = isLong ? entryPrice - slDistance : entryPrice + slDistance;

    return new double[]{tp, sl};
  }

  /** Returns current session label for logging. */
  private String currentSessionLabel() {
    if (!config.isTimeOfDayFilterEnabled()) {
      return "FILTER_DISABLED";
    }
    return liquidityWindows.getSessionLabel(LocalTime.now(MSK_ZONE));
  }

  /**
   * Logs message with throttling to prevent spam of repeated warnings.
   *
   * @param key unique identifier for the log category
   * @param message message to log
   * @param throttleMinutes minutes to wait between logs for the same key
   */
  private void logThrottled(String key, String message, int throttleMinutes) {
    long now = System.currentTimeMillis();
    long throttleMs = throttleMinutes * 60 * 1000L;
    Long lastTime = lastThrottledLogMs.get(key);
    if (lastTime == null || (now - lastTime) >= throttleMs) {
      lastThrottledLogMs.put(key, now);
      log(message);
    }
  }

  /**
   * Applies tighter stop loss for restored positions.
   *
   * <p>Restored positions from previous sessions may be stale or in unfavorable conditions.
   * This method tightens the stop loss by 50% to reduce risk and exit faster if the position
   * moves against us.
   *
   * @param runtime ticker runtime with restored position
   */
  private void applyTighterStopLossForRestoredPosition(TickerRuntime runtime) {
    if (runtime.openPosition == null) {
      return;
    }
    
    OpenPosition pos = runtime.openPosition;
    double entryPrice = pos.entryPrice;
    double currentSl = pos.stopLossPrice;
    
    // Calculate original SL distance
    double slDistance = Math.abs(entryPrice - currentSl);
    
    // Apply 50% tighter stop loss
    double tighterSlDistance = slDistance * 0.5;
    
    // Calculate new SL price based on direction
    double newSlPrice;
    if ("LONG".equals(pos.direction)) {
      newSlPrice = entryPrice - tighterSlDistance;
    } else { // SHORT
      newSlPrice = entryPrice + tighterSlDistance;
    }
    
    // Update stop loss
    pos.stopLossPrice = newSlPrice;
    
    log("Tightened SL for restored " + runtime.ticker + ": " + 
        String.format("%.2f", currentSl) + " → " + String.format("%.2f", newSlPrice));
  }

  /**
   * Returns time-adjusted minimum trade flow threshold.
   *
   * <p>During low liquidity periods (lunch break, evening session), increases the threshold
   * to reduce false entries and improve signal quality.
   *
   * <ul>
   *   <li>Lunch break (14:00-15:00 MSK): +50% threshold</li>
   *   <li>Evening session (18:45-23:50 MSK): +25% threshold</li>
   *   <li>Normal hours: base threshold</li>
   * </ul>
   *
   * @return adjusted minimum trade flow
   */
  private double getTimeAdjustedMinTradeFlow() {
    double baseFlow = config.getMinTradeFlow();
    LocalTime now = LocalTime.now(MSK_ZONE);
    
    // Lunch break: 14:00-15:00 MSK (low liquidity)
    if (now.isAfter(LocalTime.of(14, 0)) && now.isBefore(LocalTime.of(15, 0))) {
      return baseFlow * 1.5;
    }
    
    // Evening session: 18:45-23:50 MSK (reduced liquidity)
    if (now.isAfter(LocalTime.of(18, 45)) && now.isBefore(LocalTime.of(23, 50))) {
      return baseFlow * 1.25;
    }
    
    return baseFlow;
  }

  /**
   * Calculates dynamic quality threshold based on volatility.
   *
   * <p>Higher volatility → lower threshold (more entries, more profit potential)
   * Lower volatility → higher threshold (fewer entries, less profit potential)
   *
   * @param ticker instrument ticker
   * @param currentSpreadBps current spread in basis points
   * @return dynamic quality threshold
   */
  private double calculateDynamicQualityThreshold(String ticker, double currentSpreadBps) {
    double baseThreshold = ENTRY_QUALITY_THRESHOLD;
    
    // Use current spread as volatility proxy
    // Higher spread → higher volatility → lower threshold
    double targetSpreadBps = config.getVolatilityTargetSpreadBps();
    double volRatio = currentSpreadBps / targetSpreadBps;
    
    // Adjust threshold based on volatility
    // High vol (volRatio > 1.0) → lower threshold (down to 0.25)
    // Low vol (volRatio < 1.0) → higher threshold (up to 0.40)
    double minThreshold = 0.25;
    double maxThreshold = 0.40;
    
    // Inverse relationship: higher vol → lower threshold
    double adjustedThreshold = baseThreshold / Math.max(0.7, Math.min(1.3, volRatio));
    
    // Clamp to bounds
    return Math.max(minThreshold, Math.min(maxThreshold, adjustedThreshold));
  }

  /**
   * Checks if there is sufficient capital to open a new position.
   *
   * <p>For futures, uses margin requirement (25% of notional) instead of full notional.
   * This prevents futile signal generation when capital is insufficient.
   *
   * @param ticker instrument ticker
   * @param price current market price
   * @return true if sufficient capital is available, false otherwise
   */
  private boolean hasSufficientCapital(String ticker, double price) {
    if (price <= 0.0) {
      return false;
    }

    try {
      double availableCash = tcsService.getAvailableCash();
      
      // getAvailableCash() from Tinkoff API already returns free cash after margin blocking,
      // so we don't need to subtract reservedCapital again (was causing double deduction).
      // Just protect against negative values from API.
      double effectiveAvailableCash = Math.max(0.0, availableCash);
      
      // Calculate minimum capital required for one lot
      TickerInfo.Key key = runtimesByTicker.get(ticker) != null 
          ? runtimesByTicker.get(ticker).key 
          : new TickerInfo.Key(ticker, TickerType.FEATURE);
      
      TickerInfo tickerInfo = tcsService.searchTicker(key);
      int lot = tickerInfo.getLot() != null ? Math.max(1, tickerInfo.getLot()) : 1;
      // price is per unit, lot is units per lot — price * lot = notional per lot
      double minLotCost = price * lot;
      
      // For futures, use margin requirement
      if (tickerInfo.getType() == TickerType.FEATURE) {
        minLotCost *= 0.25;
      }

      // Need at least enough for one lot plus some buffer
      double requiredCapital = minLotCost * 1.1; // 10% buffer

      if (effectiveAvailableCash < requiredCapital) {
        logThrottled(ticker + "_insufficient_capital",
            "Skip " + ticker + ": insufficient capital (available=" + 
            String.format("%.0f", effectiveAvailableCash) + 
            ", required=" + String.format("%.0f", requiredCapital) + ")", 5);
        return false;
      }

      return true;
    } catch (Exception e) {
      // If we can't get cash info, allow trading to proceed
      return true;
    }
  }

  /**
   * Calculates fee-aware position size cap.
   *
   * <p>Ensures that total commission does not exceed maxFeePercent of expected profit.
   * This prevents opening positions where fees would consume too much of the potential profit.
   *
   * @param units initial calculated units
   * @param entryPrice expected entry price
   * @param ticker instrument ticker
   * @return capped units (may be same or lower than input)
   */
  private int calculateFeeAwareUnits(int units, double entryPrice, String ticker) {
    if (units <= 0 || entryPrice <= 0.0) {
      return units;
    }

    // Calculate expected profit per unit (TP distance)
    double tpDistanceFraction = feeDistanceFraction(ticker, config.getTargetFeeMultiple());
    double expectedProfitPerUnit = entryPrice * tpDistanceFraction;

    // Calculate commission per unit (round trip)
    double commissionRate = estimateCommissionRate(ticker);
    double commissionPerUnit = entryPrice * commissionRate * 2.0; // entry + exit

    if (expectedProfitPerUnit <= 0.0 || commissionPerUnit <= 0.0) {
      return units;
    }

    // Max fee percent of expected profit (default 15%)
    double maxFeePercent = 0.15;
    double maxCommissionPerUnit = expectedProfitPerUnit * maxFeePercent;

    // If commission already acceptable, return original units
    if (commissionPerUnit <= maxCommissionPerUnit) {
      return units;
    }

    // Cap units to a reasonable maximum based on position cash
    // For futures: positionCash covers margin (25% of notional), not full notional
    TickerInfo.Key key = runtimesByTicker.get(ticker) != null
        ? runtimesByTicker.get(ticker).key
        : new TickerInfo.Key(ticker, TickerType.FEATURE);
    TickerInfo tickerInfo = tcsService.searchTicker(key);
    double marginPerUnit;
    if (tickerInfo != null && tickerInfo.getType() == TickerType.FEATURE) {
      double marginRate = 0.25; // MOEX futures margin ~25%
      marginPerUnit = entryPrice * marginRate;
    } else {
      marginPerUnit = entryPrice; // equities: full price per unit
    }
    double maxUnitsByCash = config.getPositionCash() / marginPerUnit;
    int cappedUnits = (int) Math.min(units, maxUnitsByCash);

    if (cappedUnits < units) {
      logThrottled(ticker + "_fee_cap",
          "Fee-aware cap for " + ticker + ": " + units + " → " + cappedUnits +
          " (commission=" + String.format("%.2f", commissionPerUnit) +
          " > " + (maxFeePercent * 100) + "% of profit=" + String.format("%.2f", expectedProfitPerUnit) + ")", 5);
    }

    return cappedUnits;
  }

  private double estimateCommissionRate(String ticker) {
    CommissionEstimator estimator = commissionEstimators.get(ticker);
    double realizedRate = estimator != null ? estimator.rate() : Double.NaN;
    if (!Double.isNaN(realizedRate)) {
      return Math.max(config.getCommissionRate(), realizedRate);
    }
    return config.getCommissionRate();
  }

  private void recordRealizedCommission(String ticker, double notional, double commission) {
    if (notional <= 0.0) {
      return;
    }
    commissionEstimators
        .computeIfAbsent(ticker, key -> new CommissionEstimator())
        .add(notional, Math.max(0.0, commission));
  }

  private List<TickerRuntime> subscribeInstruments(List<TickerInfo> instruments) {
    List<TickerRuntime> subscribed = new ArrayList<>();
    for (TickerInfo info : instruments) {
      TickerRuntime runtime = subscribeSingle(info);
      if (runtime != null) {
        subscribed.add(runtime);
      }
    }
    log(strategyName + " subscribed to " + subscribed.size() + " instruments");
    return subscribed;
  }

  private TickerRuntime subscribeSingle(TickerInfo info) {
    String ticker = info.getTicker();
    if (!tcsService.isTradableForAccount(info)) {
      log(
          "Skip "
              + ticker
              + ": not tradable (qualOnly="
              + info.isForQualInvestorFlag()
              + ", apiTrade="
              + info.isApiTradeAvailableFlag()
              + ", normalTrading="
              + info.isNormalTradingStatus()
              + ")");
      return null;
    }
    TickerInfo.Key key = info.getKey();
    try {
      TickerRuntime runtime = new TickerRuntime(ticker, key, info.getFigi(), info);
      runtimesByTicker.put(ticker, runtime);
      runtimesByFigi.put(info.getFigi(), runtime);
      tcsService.subscribeMarketData(key, config.getDepth(), this);
      log("Subscribed to order book: " + ticker + " (" + key.getType() + ")");
      return runtime;
    } catch (Exception ex) {
      log("Failed to subscribe " + ticker + ": " + ex.getMessage());
      return null;
    }
  }

  private void rescreenSubscriptions(List<TickerRuntime> subscribed, boolean paper) {
    log("Rescreening order-book universe...");
    List<TickerInfo> refreshed = resolveInstruments();
    Set<String> targetTickers = refreshed.stream().map(TickerInfo::getTicker).collect(toSet());
    Set<String> currentTickers =
        subscribed.stream().map(runtime -> runtime.ticker).collect(toSet());

    for (TickerRuntime runtime : new ArrayList<>(subscribed)) {
      if (targetTickers.contains(runtime.ticker)) {
        continue;
      }
      closeOpenPosition(runtime, "rescreen_exit", paper);
      tcsService.unsubscribeMarketData(runtime.key, this);
      runtimesByTicker.remove(runtime.ticker);
      runtimesByFigi.remove(runtime.figi);
      subscribed.remove(runtime);
      log("Rescreen unsubscribe: " + runtime.ticker);
    }

    for (TickerInfo info : refreshed) {
      if (currentTickers.contains(info.getTicker())) {
        continue;
      }
      TickerRuntime runtime = subscribeSingle(info);
      if (runtime != null) {
        subscribed.add(runtime);
        log("Rescreen subscribe: " + info.getTicker());
      }
    }
    log("Rescreen complete: watching " + subscribed.size() + " instruments");
  }

  private double resolveBestBid(Map<String, Map<Double, Integer>> book, double fallback) {
    if (book == null || !book.containsKey("bids") || book.get("bids").isEmpty()) {
      return fallback;
    }
    return book.get("bids").keySet().stream()
        .mapToDouble(Double::doubleValue)
        .max()
        .orElse(fallback);
  }

  private double resolveBestAsk(Map<String, Map<Double, Integer>> book, double fallback) {
    if (book == null || !book.containsKey("asks") || book.get("asks").isEmpty()) {
      return fallback;
    }
    return book.get("asks").keySet().stream()
        .mapToDouble(Double::doubleValue)
        .min()
        .orElse(fallback);
  }

  private double calculateTradeDelta(TickerInfo.Key key) {
    List<MarketTradeTick> trades =
        tcsService.getRecentTrades(key, Duration.ofSeconds(config.getTradeFlowWindowSeconds()));
    return OrderBookMath.calculateTradeDelta(trades);
  }

  private List<TickerInfo> resolveInstruments() {
    List<String> configured = config.getInstruments();
    if (isAllFuturesMode(configured)) {
      // Load futures
      Map<TickerInfo.Key, TickerInfo> allFutures = tcsService.getFuturesList();
      List<TickerInfo> futuresCandidates =
          allFutures.values().stream()
              .filter(info -> "rub".equalsIgnoreCase(info.getCurrency()))
              .filter(tcsService::isTradableForAccount)
              .sorted(Comparator.comparing(TickerInfo::getTicker))
              .collect(toList());
      log(
          "Loaded "
              + allFutures.size()
              + " RUB futures, "
              + futuresCandidates.size()
              + " tradable for current account");
      
      // Load stocks if enabled
      List<TickerInfo> allCandidates = new ArrayList<>(futuresCandidates);
      if (config.isStocksEnabled()) {
        Map<TickerInfo.Key, TickerInfo> allStocks = tcsService.getStockList();
        List<TickerInfo> stockCandidates =
            allStocks.values().stream()
                .filter(info -> "rub".equalsIgnoreCase(info.getCurrency()))
                .filter(tcsService::isTradableForAccount)
                .sorted(Comparator.comparing(TickerInfo::getTicker))
                .collect(toList());
        log(
            "Loaded "
                + allStocks.size()
                + " RUB stocks, "
                + stockCandidates.size()
                + " tradable for current account");
        allCandidates.addAll(stockCandidates);
      }
      
      return OrderBookScalpScreener.selectTop(tcsService, allCandidates, config);
    }

    List<TickerInfo> resolved = new ArrayList<>();
    for (String ticker : configured) {
      TickerType type = TickerTypeResolver.resolve(ticker);
      if (TickerType.UNKNOWN == type) {
        log("Skip " + ticker + ": unknown instrument type");
        continue;
      }
      try {
        TickerInfo info = tcsService.searchTicker(new TickerInfo.Key(ticker, type));
        if (!tcsService.isTradableForAccount(info)) {
          log("Skip " + ticker + ": not tradable for current account");
          continue;
        }
        resolved.add(info);
      } catch (Exception ex) {
        log("Failed to resolve " + ticker + ": " + ex.getMessage());
      }
    }
    return resolved;
  }

  private static boolean isAllFuturesMode(List<String> configured) {
    return configured.size() == 1 && "ALL".equalsIgnoreCase(configured.get(0).trim());
  }

  private void logStatsIfNeeded() {
    if (tradeStats.trades % 5 == 0) {
      logStats("progress");
    }
  }

  private void logStats(String label) {
    log(
        strategyName
            + " stats ["
            + label
            + "]: trades="
            + tradeStats.trades
            + ", wins="
            + tradeStats.wins
            + ", winRate="
            + String.format("%.1f%%", tradeStats.winRatePercent())
            + ", netPnl="
            + String.format("%.2f", tradeStats.netPnl));
  }

  private static void log(String message) {
    LoggingUtils.log(message);
  }
  
  /**
   * Record latency sample for processing order book events.
   * Maintains rolling window of samples for statistical analysis.
   */
  private void recordLatencySample(String ticker, long latencyNs) {
    List<Long> samples = processingLatencySamples.computeIfAbsent(
        ticker, k -> new ArrayList<>());
    synchronized (samples) {
      samples.add(latencyNs);
      if (samples.size() > MAX_LATENCY_SAMPLES) {
        samples.remove(0);
      }
    }
  }
  
  /**
   * Get average latency for ticker in milliseconds.
   */
  private double getAverageLatencyMs(String ticker) {
    List<Long> samples = processingLatencySamples.get(ticker);
    if (samples == null || samples.isEmpty()) {
      return 0.0;
    }
    synchronized (samples) {
      return samples.stream()
          .mapToLong(Long::longValue)
          .average()
          .orElse(0.0) / 1_000_000.0;
    }
  }

  private void maybeLogPeriodicDiagnosticsSummary() {
    if (!config.isDiagnosticsSummaryEnabled() || diagnosticsCollector == null) {
      return;
    }

    long now = System.currentTimeMillis();
    if (now < nextDiagnosticsSummaryMs) {
      return;
    }
    nextDiagnosticsSummaryMs = now + DIAGNOSTICS_SUMMARY_INTERVAL_MS;

    Instant since = Instant.ofEpochMilli(now - DIAGNOSTICS_SUMMARY_INTERVAL_MS);
    OrderBookDiagnosticsSummary windowSummary = diagnosticsCollector.summarizeSince(since);
    OrderBookDiagnosticsSummary totalSummary = diagnosticsCollector.summarize();

    Map<String, Object> metrics = new HashMap<>();
    metrics.put("window_opened", windowSummary.getOpenedCount());
    metrics.put("window_skipped", windowSummary.getSkippedCount());
    metrics.put("window_closed", windowSummary.getClosedCount());
    metrics.put("window_recoveries", windowSummary.getRecoveryCount());
    metrics.put("window_avgQuality", windowSummary.getAverageEntryQuality());
    metrics.put("window_grossPnl", windowSummary.getGrossPnl());
    metrics.put("window_netPnl", windowSummary.getNetPnl());
    metrics.put("window_feeDrag", windowSummary.getFeeDrag());
    metrics.put("window_avgHoldSeconds", windowSummary.getAverageHoldSeconds());
    metrics.put("total_opened", totalSummary.getOpenedCount());
    metrics.put("total_skipped", totalSummary.getSkippedCount());
    metrics.put("total_closed", totalSummary.getClosedCount());
    metrics.put("total_netPnl", totalSummary.getNetPnl());
    metrics.put("total_feeDrag", totalSummary.getFeeDrag());
    metrics.put("hints", buildDiagnosticsHints(windowSummary, totalSummary));
    metrics.put(
        "recommendation", buildPeriodicDiagnosticsRecommendation(windowSummary, totalSummary));

    emitDiagnostic(OrderBookDiagnosticEventType.SUMMARY, strategyName, "periodic_10m", metrics);

    log(buildPeriodicDiagnosticsSummary(windowSummary, totalSummary));
    log(buildPeriodicDiagnosticsRecommendation(windowSummary, totalSummary));
  }

  private void maybeLogHeartbeatDiagnostic() {
    long now = System.currentTimeMillis();
    if (now < nextHeartbeatDiagnosticMs) {
      return;
    }
    nextHeartbeatDiagnosticMs = now + 30_000L;

    long timeSinceData = now - lastMarketDataAtMs;
    Map<String, Object> metrics = new HashMap<>();
    metrics.put("tickers", runtimesByTicker.size());
    metrics.put("timeSinceMarketData", timeSinceData);
    metrics.put(
        "positions",
        runtimesByTicker.values().stream().filter(r -> r.openPosition != null).count());
    metrics.put("streamRecoveryAttempts", streamRecoveryAttempts);
    emitDiagnostic(OrderBookDiagnosticEventType.SUMMARY, strategyName, "heartbeat_30s", metrics);
  }

  private String buildPeriodicDiagnosticsSummary(
      OrderBookDiagnosticsSummary windowSummary, OrderBookDiagnosticsSummary totalSummary) {
    return strategyName
        + " diagnostics [last_10m]: opened="
        + windowSummary.getOpenedCount()
        + ", skipped="
        + windowSummary.getSkippedCount()
        + ", closed="
        + windowSummary.getClosedCount()
        + ", recoveries="
        + windowSummary.getRecoveryCount()
        + ", avgQuality="
        + String.format("%.3f", windowSummary.getAverageEntryQuality())
        + ", grossPnl="
        + String.format("%.2f", windowSummary.getGrossPnl())
        + ", netPnl="
        + String.format("%.2f", windowSummary.getNetPnl())
        + ", feeDrag="
        + String.format("%.2f", windowSummary.getFeeDrag())
        + ", avgHoldSeconds="
        + String.format("%.1f", windowSummary.getAverageHoldSeconds())
        + ", skipReasons="
        + windowSummary.getSkipReasons()
        + ", closeReasons="
        + windowSummary.getCloseReasons()
        + ", topSkippedTickers="
        + windowSummary.getSkippedTickers()
        + " | cumulative: opened="
        + totalSummary.getOpenedCount()
        + ", skipped="
        + totalSummary.getSkippedCount()
        + ", closed="
        + totalSummary.getClosedCount()
        + ", netPnl="
        + String.format("%.2f", totalSummary.getNetPnl())
        + ", feeDrag="
        + String.format("%.2f", totalSummary.getFeeDrag())
        + ", topSkippedTickers="
        + totalSummary.getSkippedTickers()
        + " | hints="
        + buildDiagnosticsHints(windowSummary, totalSummary);
  }

  private String buildDiagnosticsHints(
      OrderBookDiagnosticsSummary windowSummary, OrderBookDiagnosticsSummary totalSummary) {
    List<String> hints = new ArrayList<>();
    if (windowSummary.getOpenedCount() == 0 && windowSummary.getSkippedCount() > 20) {
      hints.add("entry_starvation");
    }
    Integer feeSkips = windowSummary.getSkipReasons().get("expected_edge_below_fees");
    if (feeSkips != null && feeSkips > 0) {
      hints.add("fees_dominate_edge");
    }
    if (windowSummary.getRecoveryCount() > 0) {
      hints.add("stream_recovery_active");
    }
    if (windowSummary.getOpenedCount() > 0 && windowSummary.getClosedCount() == 0) {
      hints.add("positions_still_open");
    }
    if (totalSummary.getOpenedCount() == 0 && totalSummary.getSkippedCount() > 100) {
      hints.add("session_not_trading");
    }
    if (hints.isEmpty()) {
      hints.add("no_clear_pathology_detected");
    }
    return hints.toString();
  }

  private String buildPeriodicDiagnosticsRecommendation(
      OrderBookDiagnosticsSummary windowSummary, OrderBookDiagnosticsSummary totalSummary) {
    List<String> recommendations = new ArrayList<>();
    Integer feeSkips = windowSummary.getSkipReasons().get("expected_edge_below_fees");
    if (feeSkips != null && feeSkips > 0) {
      recommendations.add("consider_higher_tp_spreads");
      recommendations.add("consider_stricter_universe_filter");
    }
    if (windowSummary.getOpenedCount() == 0 && windowSummary.getSkippedCount() > 20) {
      recommendations.add("consider_excluding_untradable_instruments");
    }
    if (windowSummary.getRecoveryCount() > 0) {
      recommendations.add("investigate_stream_stability");
    }
    if (totalSummary.getOpenedCount() == 0 && totalSummary.getSkippedCount() > 100) {
      recommendations.add("review_instrument_selection_and_execution_costs");
    }
    windowSummary.getSkippedTickers().entrySet().stream()
        .sorted((left, right) -> Integer.compare(right.getValue(), left.getValue()))
        .limit(2)
        .map(Map.Entry::getKey)
        .map(String::toLowerCase)
        .forEach(ticker -> recommendations.add("consider_excluding_" + ticker));
    if (recommendations.isEmpty()) {
      recommendations.add("no_immediate_strategy_change_recommended");
    }
    return strategyName + " recommendation [last_10m]: " + recommendations;
  }

  private void emitDiagnostic(
      OrderBookDiagnosticEventType type,
      String ticker,
      String reason,
      Map<String, Object> metrics) {
    if (diagnosticsCollector == null) {
      return;
    }
    // Enrich metrics with advanced component data
    Map<String, Object> enrichedMetrics = new HashMap<>(metrics);
    collectAdvancedMetrics(ticker, enrichedMetrics);

    OrderBookDiagnosticEvent event =
        new OrderBookDiagnosticEvent(Instant.now(), type, ticker, reason, enrichedMetrics);
    diagnosticsCollector.record(event);
    String logLine = buildDiagnosticLogLine(event);
    log(logLine);
    System.out.flush();
    if (diagnosticsReplayWriter != null) {
      diagnosticsReplayWriter.write(event);
    }
    if (metricsCsvWriter != null) {
      metricsCsvWriter.write(event);
    }
  }

  /**
   * Collects metrics from advanced components for diagnostic events.
   * Loads candles for regime detection and uses cached snapshot for dynamic TP.
   */
  private void collectAdvancedMetrics(String ticker, Map<String, Object> metrics) {
    TickerRuntime runtime = runtimesByTicker.get(ticker);

    // Market regime (requires candles)
    if (regimeDetector != null) {
      List<Candle> candles = loadCandles(ticker);
      if (candles != null && !candles.isEmpty()) {
        MarketRegimeDetector.RegimeResult regime = regimeDetector.detect(candles);
        if (regime != null) {
          metrics.put("regime", regime.regime.name());
          metrics.put("atr", regime.atr);
          metrics.put("adx", regime.adx);
        }
      }
    }

    // Tape reader
    if (tapeReader != null) {
      List<TapeReader.BlockTrade> recentBlocks = tapeReader.getRecentBlockTrades(ticker, 60_000);
      if (recentBlocks != null && !recentBlocks.isEmpty()) {
        metrics.put("blockTradeCount", recentBlocks.size());
        double totalBlockVolume = recentBlocks.stream()
            .mapToDouble(b -> b.getVolume())
            .sum();
        metrics.put("blockTradeVolume", totalBlockVolume);
      }
    }
    // VPIN
    if (vpinCalculator != null) {
      double vpin = vpinCalculator.getVpin(ticker);
      if (vpin >= 0.0) {
        metrics.put("vpin", vpin);
      }
    }
    // Volume profile
    if (volumeProfileTracker != null) {
      double vwap = volumeProfileTracker.getVwap(ticker);
      if (vwap > 0.0) {
        metrics.put("vwap", vwap);
      }
      double poc = volumeProfileTracker.getPoc(ticker);
      if (poc > 0.0) {
        metrics.put("poc", poc);
      }
      double vaHigh = volumeProfileTracker.getValueAreaHigh(ticker);
      if (vaHigh > 0.0) {
        metrics.put("valueAreaHigh", vaHigh);
      }
      double vaLow = volumeProfileTracker.getValueAreaLow(ticker);
      if (vaLow > 0.0) {
        metrics.put("valueAreaLow", vaLow);
      }
    }
    // Queue dynamics
    if (queueDynamicsTracker != null) {
      double avgFillRateBid = queueDynamicsTracker.getAverageFillRate(ticker, true);
      double avgFillRateAsk = queueDynamicsTracker.getAverageFillRate(ticker, false);
      double avgFillRate = Math.max(avgFillRateBid, avgFillRateAsk);
      if (avgFillRate >= 0.0) {
        metrics.put("avgFillRate", avgFillRate);
      }
      double eatenRatioBid = queueDynamicsTracker.getEatenRatio(ticker, 0.0, true);
      double eatenRatioAsk = queueDynamicsTracker.getEatenRatio(ticker, 0.0, false);
      double eatenRatio = Math.max(eatenRatioBid, eatenRatioAsk);
      if (eatenRatio >= 0.0) {
        metrics.put("eatenRatio", eatenRatio);
      }
    }
    // Signal performance
    if (signalPerformanceTracker != null) {
      if (runtime != null && runtime.openPosition != null) {
        String signalId = runtime.openPosition.signalId;
        double winRate = signalPerformanceTracker.getWinRate(signalId);
        if (winRate >= 0.0) {
          metrics.put("signalWinRate", winRate);
        }
        double avgPnl = signalPerformanceTracker.getAveragePnl(signalId);
        metrics.put("signalAvgPnl", avgPnl);
        int tradeCount = signalPerformanceTracker.getTradeCount(signalId);
        metrics.put("signalTradeCount", tradeCount);
      }
    }
    // Dynamic TP (uses cached snapshot from runtime)
    if (dynamicTakeProfit != null && runtime != null && runtime.latestSnapshot != null) {
      double entryPrice = metrics.containsKey("entryPrice")
          ? ((Number) metrics.get("entryPrice")).doubleValue()
          : 0.0;
      if (entryPrice > 0.0) {
        String direction = metrics.containsKey("direction")
            ? metrics.get("direction").toString()
            : "LONG";
        double dynamicTp = dynamicTakeProfit.calculateTakeProfit(
            entryPrice, "LONG".equals(direction), runtime.latestSnapshot, ticker);
        if (dynamicTp > 0.0) {
          metrics.put("dynamicTpPrice", dynamicTp);
        }
      }
    }
    // Volatility spike filter
    if (volatilitySpikeFilter != null) {
      metrics.put("inCooldown", volatilitySpikeFilter.isInCooldown());
    }
    // Adaptive parameters
    if (adaptiveParameters != null && config.isAdaptiveParamsEnabled()) {
      String signalId = (runtime != null && runtime.openPosition != null)
          ? runtime.openPosition.signalId
          : "default";
      AdaptiveParameters.AdjustedThresholds thresholds = adaptiveParameters.getAdjustedThresholds(signalId);
      if (thresholds != null) {
        metrics.put("adjustedMinDelta", thresholds.getMinDeltaThreshold());
        metrics.put("adjustedMinDensity", thresholds.getMinDensityThreshold());
        metrics.put("adjustedConfidence", thresholds.getConfidenceFloor());
      }
    }
    // Slippage tracker
    if (slippageTracker != null) {
      double avgSlippage = slippageTracker.getAverageSlippage();
      metrics.put("avgSlippage", avgSlippage);
      double maxSlippage = slippageTracker.getMaxSlippage();
      metrics.put("maxSlippage", maxSlippage);
    }
    // Partial fill handler
    if (partialFillHandler != null) {
      PartialFillHandler.PendingFill pending = partialFillHandler.getPendingFill(ticker);
      if (pending != null) {
        double fillRatio = pending.getFillRatio();
        metrics.put("fillRatio", fillRatio);
        metrics.put("unfilledQty", pending.getUnfilledQuantity());
      }
    }
  }

  /**
   * Loads recent 5-minute candles for a ticker.
   * Used for regime detection and volatility spike analysis.
   * Loads 6 hours of data to ensure sufficient candles for ATR/ADX calculation.
   */
  private List<Candle> loadCandles(String ticker) {
    try {
      TickerInfo info = TickerRepository.INSTANCE.getByName(ticker);
      if (info == null) {
        return Collections.emptyList();
      }
      String figi = info.getFigi();
      Instant now = Instant.now();
      Instant from = now.minus(6, ChronoUnit.HOURS);
      List<HistoricCandle> historicCandles = tcsService.getCandles(figi, from, now, CANDLE_INTERVAL_5_MIN);
      return historicCandles.stream()
          .map(hc -> new Candle(
              hc.getTime().toString(),
              IndicatorsUtil.toDouble(hc.getOpen()),
              IndicatorsUtil.toDouble(hc.getHigh()),
              IndicatorsUtil.toDouble(hc.getLow()),
              IndicatorsUtil.toDouble(hc.getClose()),
              hc.getVolume()))
          .toList();
    } catch (Exception e) {
      return Collections.emptyList();
    }
  }

  /**
   * Emit densityScalp skip metrics for CSV diagnostics.
   * Called by DensityScalpSignal when entry is skipped.
   */
  private void emitDensityScalpSkip(String ticker, Map<String, Object> metrics) {
    String skipReason = metrics.getOrDefault("skipReason", "unknown").toString();
    emitDiagnostic(
        OrderBookDiagnosticEventType.ENTRY_SKIPPED,
        ticker,
        "densityScalp_" + skipReason,
        metrics);
  }

  private String buildDiagnosticLogLine(OrderBookDiagnosticEvent event) {
    StringBuilder builder = new StringBuilder();
    builder.append("OBD|").append(event.getTimestamp());
    builder.append('|').append(event.getType());
    builder.append('|').append(sanitizeDiagnosticValue(event.getTicker()));
    builder.append('|').append(sanitizeDiagnosticValue(event.getReason()));
    if (event.getMetrics().isEmpty()) {
      return builder.toString();
    }
    builder.append('|');
    boolean first = true;
    for (Map.Entry<String, Object> entry : new java.util.TreeMap<>(event.getMetrics()).entrySet()) {
      if (!first) {
        builder.append(';');
      }
      builder.append(sanitizeDiagnosticValue(entry.getKey()));
      builder.append('=');
      builder.append(sanitizeDiagnosticValue(String.valueOf(entry.getValue())));
      first = false;
    }
    return builder.toString();
  }

  private String sanitizeDiagnosticValue(String value) {
    if (value == null) {
      return "";
    }
    return value.replace("|", "/").replace(";", ",").replace('\n', ' ').replace('\r', ' ');
  }

  private static final class BracketPrices {

    final double tpPrice;
    final double slPrice;

    BracketPrices(double tpPrice, double slPrice) {
      this.tpPrice = tpPrice;
      this.slPrice = slPrice;
    }
  }

  private static final class TradeStats {

    private int trades;
    private int wins;
    private double netPnl;

    private void record(double pnl) {
      trades++;
      if (pnl > 0.0) {
        wins++;
      }
      netPnl += pnl;
    }

    private double winRatePercent() {
      if (trades == 0) {
        return 0.0;
      }
      return wins * 100.0 / trades;
    }
  }

  private static final class OpenPosition implements OrderBookPositionView {

    final String signalId;
    final String direction;
    final double entryPrice;
    final double spreadAtEntry;
    final Instant entryTime;
    final double takeProfitPrice;
    volatile double stopLossPrice;
    final int units;
    final double entryValue;
    final double entryCommission;
    final boolean isRestored;
    volatile int microReversalTicks;
    volatile int flowReversalTicks;
    
    // TODO.md Section 5: Server-side stop-loss and order idempotency
    final String brokerOrderId;          // Client-provided unique order ID for idempotency
    final String brokerStopLossOrderId;  // Broker-side stop-loss order ID
    final double brokerStopLossPrice;    // Server-side SL price
    volatile double realizedPnl;         // Net PnL after commissions

    OpenPosition(
        String signalId,
        String direction,
        double entryPrice,
        double spreadAtEntry,
        Instant entryTime,
        double takeProfitPrice,
        double stopLossPrice,
        int units,
        double entryValue,
        double entryCommission) {
      this(signalId, direction, entryPrice, spreadAtEntry, entryTime, takeProfitPrice,
           stopLossPrice, units, entryValue, entryCommission, false, null, null, 0.0);
    }

    OpenPosition(
        String signalId,
        String direction,
        double entryPrice,
        double spreadAtEntry,
        Instant entryTime,
        double takeProfitPrice,
        double stopLossPrice,
        int units,
        double entryValue,
        double entryCommission,
        boolean isRestored) {
      this(signalId, direction, entryPrice, spreadAtEntry, entryTime, takeProfitPrice,
           stopLossPrice, units, entryValue, entryCommission, isRestored, null, null, 0.0);
    }
    
    // Full constructor with server-side stop and idempotency
    OpenPosition(
        String signalId,
        String direction,
        double entryPrice,
        double spreadAtEntry,
        Instant entryTime,
        double takeProfitPrice,
        double stopLossPrice,
        int units,
        double entryValue,
        double entryCommission,
        boolean isRestored,
        String brokerOrderId,
        String brokerStopLossOrderId,
        double brokerStopLossPrice) {
      this.signalId = signalId;
      this.direction = direction;
      this.entryPrice = entryPrice;
      this.spreadAtEntry = spreadAtEntry;
      this.entryTime = entryTime;
      this.takeProfitPrice = takeProfitPrice;
      this.stopLossPrice = stopLossPrice;
      this.units = units;
      this.entryValue = entryValue;
      this.entryCommission = entryCommission;
      this.isRestored = isRestored;
      this.microReversalTicks = 0;
      this.flowReversalTicks = 0;
      this.brokerOrderId = brokerOrderId;
      this.brokerStopLossOrderId = brokerStopLossOrderId;
      this.brokerStopLossPrice = brokerStopLossPrice;
      this.realizedPnl = 0.0;
    }

    @Override
    public String getSignalId() {
      return signalId;
    }

    @Override
    public String getDirection() {
      return direction;
    }

    @Override
    public double getEntryPrice() {
      return entryPrice;
    }

    @Override
    public double getSpreadAtEntry() {
      return spreadAtEntry;
    }

    @Override
    public Instant getEntryTime() {
      return entryTime;
    }

    @Override
    public double getTakeProfitPrice() {
      return takeProfitPrice;
    }

    @Override
    public double getStopLossPrice() {
      return stopLossPrice;
    }

    @Override
    public long getHeldSeconds() {
      return Duration.between(entryTime, Instant.now()).getSeconds();
    }
  }

  static final class PositionState {

    String signalId;
    String direction;
    double entryPrice;
    double spreadAtEntry;
    long entryTimeEpochMillis;
    double takeProfitPrice;
    double stopLossPrice;
    int units;
    double entryValue;
    double entryCommission;
    String brokerOrderId;
    String brokerStopLossOrderId;
    double brokerStopLossPrice;
    double realizedPnl;

    PositionState() {}

    PositionState(OpenPosition position) {
      this.signalId = position.signalId;
      this.direction = position.direction;
      this.entryPrice = position.entryPrice;
      this.spreadAtEntry = position.spreadAtEntry;
      this.entryTimeEpochMillis = position.entryTime.toEpochMilli();
      this.takeProfitPrice = position.takeProfitPrice;
      this.stopLossPrice = position.stopLossPrice;
      this.units = position.units;
      this.entryValue = position.entryValue;
      this.entryCommission = position.entryCommission;
      this.brokerOrderId = position.brokerOrderId;
      this.brokerStopLossOrderId = position.brokerStopLossOrderId;
      this.brokerStopLossPrice = position.brokerStopLossPrice;
      this.realizedPnl = position.realizedPnl;
    }

    OpenPosition toOpenPosition() {
      return new OpenPosition(
          signalId,
          direction,
          entryPrice,
          spreadAtEntry,
          Instant.ofEpochMilli(entryTimeEpochMillis),
          takeProfitPrice,
          stopLossPrice,
          units,
          entryValue,
          entryCommission,
          true,
          brokerOrderId,
          brokerStopLossOrderId,
          brokerStopLossPrice);
    }
  }

  private static class TickerRuntime {

    final String ticker;
    final TickerInfo.Key key;
    final String figi;
    final TickerInfo tickerInfo;
    volatile long cooldownUntilMs;
    volatile OpenPosition openPosition;
    volatile MarketDepthSnapshot latestSnapshot;

    TickerRuntime(String ticker, TickerInfo.Key key, String figi, TickerInfo tickerInfo) {
      this.ticker = ticker;
      this.key = key;
      this.figi = figi;
      this.tickerInfo = tickerInfo;
    }
  }

  private static class CommissionEstimator {

    private double notional;
    private double commission;
    private int samples;

    synchronized void add(double tradeNotional, double tradeCommission) {
      notional += tradeNotional;
      commission += tradeCommission;
      samples++;
    }

    synchronized double rate() {
      return samples > 0 && notional > 0.0 ? commission / notional : Double.NaN;
    }
  }
}
