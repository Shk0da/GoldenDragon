package com.github.shk0da.goldendragon.strategy;

import com.github.shk0da.goldendragon.config.MainConfig;
import com.github.shk0da.goldendragon.config.UnifiedTraderConfig;
import com.github.shk0da.goldendragon.filters.BadWeatherFilter;
import com.github.shk0da.goldendragon.filters.MarketRegimeFilter;
import com.github.shk0da.goldendragon.market.LiveMarketDataProvider;
import com.github.shk0da.goldendragon.market.LiveOrderExecutor;
import com.github.shk0da.goldendragon.market.MarketDataProvider;
import com.github.shk0da.goldendragon.market.MarketPrices;
import com.github.shk0da.goldendragon.market.OrderExecutor;
import com.github.shk0da.goldendragon.model.Candle;
import com.github.shk0da.goldendragon.model.Config;
import com.github.shk0da.goldendragon.model.Position;
import com.github.shk0da.goldendragon.model.PositionInfo;
import com.github.shk0da.goldendragon.model.TickerInfo;
import com.github.shk0da.goldendragon.model.TickerType;
import com.github.shk0da.goldendragon.model.TradingDecision;
import com.github.shk0da.goldendragon.money.CashParkingManager;
import com.github.shk0da.goldendragon.money.LossStreakMonitor;
import com.github.shk0da.goldendragon.money.TmonCashParkingMonitor;
import com.github.shk0da.goldendragon.repository.CandleRepository;
import com.github.shk0da.goldendragon.repository.TickerRepository;
import com.github.shk0da.goldendragon.service.TradingService;
import com.github.shk0da.goldendragon.service.TradingServiceCache;
import com.github.shk0da.goldendragon.time.LiveTimeProvider;
import com.github.shk0da.goldendragon.time.TimeProvider;
import com.github.shk0da.goldendragon.ui.DashboardServer;
import com.github.shk0da.goldendragon.utils.LoggingUtils;

import java.io.IOException;
import java.time.DayOfWeek;
import java.time.LocalTime;
import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReentrantLock;

import static com.github.shk0da.goldendragon.model.TickerType.CRYPTO;
import static com.github.shk0da.goldendragon.model.TickerType.FEATURE;
import static com.github.shk0da.goldendragon.model.TickerType.STOCK;
import static com.github.shk0da.goldendragon.utils.TimeUtils.sleep;
import static java.lang.Math.abs;
import static java.lang.Math.max;
import static java.util.concurrent.CompletableFuture.allOf;
import static java.util.concurrent.CompletableFuture.runAsync;

/**
 * Base abstract trading strategy class implementing common execution lifecycle, position
 * management, market data loading, entry filtering, and technical indicator calculation. Concrete
 * strategies (e.g., {@code UnifiedStrategy}) extend it and implement signal logic in {@link
 * #decide} method.
 *
 * <h2>Architecture</h2>
 *
 * <p>Class acts as strategy "engine":
 *
 * <ul>
 *   <li>Manages execution flow (trading hours, trading days, EOD).
 *   <li>Loads and caches historical candles (hourly and 5-minute).
 *   <li>Coordinates parallel ticker processing.
 *   <li>Delegates trading decision to subclass via {@link #decide}.
 *   <li>Executes orders via {@link TradingService} (broker API).
 *   <li>Tracks positions, cooldowns, integrates with Money Management.
 * </ul>
 *
 * <h2>Lifecycle {@link #run()}</h2>
 *
 * <ol>
 *   <li>Get initial portfolio value and send Telegram notification.
 *   <li>Collect active ticker list from {@link UnifiedTraderConfig}.
 *   <li>Call {@link #onDailyReset()} — reset MM daily limits.
 *   <li>If trading day ended or weekend — close all positions and exit.
 *   <li>Calculate capital allocation per ticker ({@link #computeCapitalAllocation}).
 *   <li>Start thread pool ({@code activeTickers.size() + 1}):
 *       <ul>
 *         <li>One thread — background peer candle update every 60 seconds ({@link
 *             #refreshPeerCandles}) for group confirmations.
 *         <li>One thread per ticker — {@link #processTicker} call loop every 30 seconds while
 *             trading hours active.
 *       </ul>
 *   <li>After trading hours: close all positions ({@link #closeAllPositions}), stop executor, final
 *       report.
 * </ol>
 *
 * <h2>Ticker Processing ({@link #processTicker})</h2>
 *
 * <p>For each ticker on each cycle:
 *
 * <ol>
 *   <li>Check personal cooldown — skip if still active ({@link #COOLDOWN_DURATION_MS} = 5 minutes
 *       after error).
 *   <li>Check trading hours and {@code tickerParams.enabled} flag.
 *   <li>Find {@link TickerInfo} in {@link TickerRepository}.
 *   <li>Load candles via {@link #loadCandlesFromApi} from the trading service API.
 *   <li>Hourly candles required; 5-minute only if {@code tickerParams.useMinuteCandles}.
 *   <li>Detect hourly bar change ({@code hourChanged}) for correct {@code candlesHeld} increment in
 *       open position.
 *   <li>Calculate balance: allocated capital or current liquidity.
 *   <li>Call abstract {@link #decide} — get {@link TradingDecision}.
 *   <li>Route by action:
 *       <ul>
 *         <li>{@code HOLD} — update position in {@link #positionStore} (reasons logged only if
 *             {@code unifiedTrader.logHoldReasons=true}).
 *         <li>{@code OPEN} — open position via {@link #openPosition}.
 *         <li>{@code CLOSE} — close via {@link #closePosition}.
 *       </ul>
 *   <li>On any error, ticker put on 5-minute cooldown, send Telegram notification.
 * </ol>
 *
 * <h2>Position Open ({@link #openPosition})</h2>
 *
 * <ul>
 *   <li>Validate direction ({@code BUY}/{@code SELL}) and quantity.
 *   <li>Check concurrent position limit ({@link #MAX_CONCURRENT_POSITIONS} = 8).
 *   <li>Calculate SL/TP as percentage of entry price (defaults 2%/4% if not set).
 *   <li>Call {@code tradingService.buyByMarket} / {@code sellByMarket} with market order and automatic
 *       SL/TP setup.
  *   <li>Save position, record entry bar for backtest metrics.
  * </ul>
 *
 * <h2>Position Close ({@link #closePosition})</h2>
 *
 * <ul>
 *   <li>Close long/short position via {@code closeLongByMarket} / {@code closeShortByMarket}.
 *   <li>Set position cooldown ({@code config.cooldownCandles}).
 *   <li>Calculate PnL ({@link #calculatePnl}) and write result to ML-pipeline.
 *   <li>Call {@link #onTradeClosed} — hook for MM integration in subclass.
 *   <li>Telegram notification with close reason and PnL.
 * </ul>
 *
 * <h2>Data Management (Candles)</h2>
 *
 * <ul>
 *   <li>{@link #loadCandlesFromApi} — loads candles from the trading service API.
 *   <li>{@link #throttleApiCall} — global rate-limiter (100ms between calls, synchronization via
 *       {@link #API_LOCK}).
 *   <li>{@link #refreshPeerCandles} — parallel hourly candle update for all tickers for group
 *       confirmation filters.
 * </ul>
 *
 * <h2>Capital Allocation ({@link #computeCapitalAllocation})</h2>
 *
 * <p>Proportional free cash distribution across active tickers based on {@code allocationWeight}
 * (default 1.0): {@code allocation[i] = totalCash × (weight[i] / Σweights)}.
 *
 * <h2>Временные ограничения</h2>
 *
 * <ul>
 *   <li>Торговая сессия: {@link #WORK_START_TIME} (10:00) — {@link #EOD_CLOSE_TIME} (21:00).
 *   <li>Торговые дни: пн–пт ({@link #isTradingDay}).
 *   <li>{@link #isEndOfDayReached} — триггер для принудительного закрытия позиций.
 * </ul>
 *
 * <h2>Технические индикаторы</h2>
 *
 * <p>Базовая реализация (используется наследниками):
 *
 * <ul>
 *   <li>{@link #ema} — экспоненциальное скользящее среднее с SMA-инициализацией.
 *   <li>{@link #atrVal} — Average True Range (простое среднее).
 *   <li>{@link #emaAtr} — сглаженный ATR через скользящее окно.
 *   <li>{@link #rsiVal} — Relative Strength Index (период по умолчанию).
 *   <li>{@link #adxVal} — Average Directional Index с Wilder-сглаживанием (+DI, -DI, DX → ADX).
 * </ul>
 *
 * <h2>Хуки для наследников</h2>
 *
 * <ul>
 *   <li>{@link #getStrategyName()} — имя стратегии для логов и уведомлений.
 *   <li>{@link #decide} — основная сигнальная логика (обязательна).
 *   <li>{@link #onTradeClosed} — callback после закрытия сделки (для MM).
 *   <li>{@link #onDailyReset} — callback в начале торгового дня (для сброса дневных лимитов MM).
 * </ul>
 *
 * <h2>Режим бэктеста</h2>
 *
 * <p>Флаг {@code isBacktest} переключает поведение:
 *
 * <ul>
 *   <li>{@link #log} — silent-логирование в backtest-режиме.
 * </ul>
 *
 * <h2>Параллелизм и потокобезопасность</h2>
 *
 * <ul>
 *   <li>{@link #positionStore}, {@link #tickerCooldown}, {@link #lastSeenHourBarByTicker}, {@link
 *       #peerCandles} — {@link ConcurrentHashMap} для безопасного доступа из потоков тикеров.
 *   <li>API-вызовы сериализуются через {@link #API_LOCK}.
 * </ul>
 *
 * <h2>Интеграции</h2>
 *
 * <ul>
 *   <li>{@link TradingService} — брокерский API (Tinkoff Invest).
 *   <li>{@code TelegramNotifyService} — уведомления о запуске, сделках, ошибках.
  *   <li>{@link TickerRepository} — справочник инструментов.
  * </ul>
  */
  public abstract class BaseStrategy {

    protected final Config config;
    protected final MainConfig mainConfig;
    protected final TradingService tradingService;
    protected final UnifiedTraderConfig unifiedTraderConfig;
    protected MarketDataProvider marketDataProvider;
    protected final CashParkingManager cashParkingManager;
    protected OrderExecutor orderExecutor;
    protected TimeProvider timeProvider;

    /** Backtest broker for parity with live trading (injected via setBacktestBroker). */
    protected static OrderExecutor backtestBroker;
    /** Backtest TradingService wrapper (injected via setBacktestTradingService). */
    protected static TradingService backtestTradingService;
    protected final BadWeatherFilter badWeatherFilter;
    protected final MarketRegimeFilter marketRegimeFilter;

    protected static final long COOLDOWN_DURATION_MS = 5 * 60 * 1000L;
    protected static final long API_CALL_DELAY_MS = 100;
    protected static final Object API_LOCK = new Object();
    protected static final int MIN_CANDLES_THRESHOLD = 5;

    protected static final LocalTime WORK_START_TIME = LocalTime.of(10, 0);
    protected static final LocalTime EOD_CLOSE_TIME = LocalTime.of(19, 0);

    protected static long lastApiCallTime = 0;

    protected final Map<String, Long> tickerCooldown = new ConcurrentHashMap<>();
    protected final Map<String, Position> positionStore = new ConcurrentHashMap<>();
    private final Map<String, ReentrantLock> tickerLocks = new ConcurrentHashMap<>();
    private LossStreakMonitor lossStreakMonitor;
    protected volatile boolean tradingHalted = false;
    protected DashboardServer dashboard;
    protected TmonCashParkingMonitor tmonCashParkingMonitor;
    protected final Map<String, String> lastSeenHourBarByTicker = new ConcurrentHashMap<>();
    protected volatile Map<String, List<Candle>> peerCandles = new ConcurrentHashMap<>();
    protected final Map<String, Long> throttledLogLastTime = new ConcurrentHashMap<>();

    /**
     * Get position store for backtest access.
     */
    public Map<String, Position> getPositionStore() {
        return positionStore;
    }

    protected static final int MAX_CONCURRENT_POSITIONS = 8; // Максимум 8 одновременных позиций

    /**
     * Set backtest broker for all strategies.
     * Called by BacktestRunner before starting simulation.
     * @deprecated Use setBacktestTradingService instead for full TradingService parity
     */
    @Deprecated
    public static void setBacktestBroker(OrderExecutor broker) {
        BaseStrategy.backtestBroker = broker;
    }

    /**
     * Set backtest TradingService for all strategies.
     * Called by BacktestRunner before starting simulation.
     */
    public static void setBacktestTradingService(TradingService service) {
        BaseStrategy.backtestTradingService = service;
    }

    /**
     * Check if currently running in backtest mode.
     * @return true if backtest broker or trading service is set
     */
    protected static boolean isBacktestMode() {
        return backtestBroker != null || backtestTradingService != null;
    }

    protected BaseStrategy(
            UnifiedTraderConfig unifiedTraderConfig,
            TradingService tradingService,
            Config config) {
        this(unifiedTraderConfig, tradingService, config, null, null);
    }

    protected BaseStrategy(
            UnifiedTraderConfig unifiedTraderConfig,
            TradingService tradingService,
            Config config,
            TimeProvider timeProvider) {
        this(unifiedTraderConfig, tradingService, config, timeProvider, null);
    }

    protected BaseStrategy(
            UnifiedTraderConfig unifiedTraderConfig,
            TradingService tradingService,
            Config config,
            TimeProvider timeProvider,
            MainConfig mainConfig) {
        this.config = config;
        this.mainConfig = mainConfig;
        // Use backtestTradingService if set (backtest mode), otherwise wrap live service with cache
        this.tradingService = backtestTradingService != null
                ? backtestTradingService
                : new TradingServiceCache(tradingService);
        this.unifiedTraderConfig = unifiedTraderConfig;
        this.timeProvider = timeProvider != null ? timeProvider : new LiveTimeProvider();

        this.marketDataProvider = new LiveMarketDataProvider(this.tradingService);
        this.orderExecutor = new LiveOrderExecutor(this.tradingService);
        this.cashParkingManager = new CashParkingManager(this.tradingService, marketDataProvider, positionStore);

        boolean bwFilterEnabled =
                unifiedTraderConfig != null
                        ? unifiedTraderConfig.isBadWeatherFilterEnabled()
                        : config.badWeatherFilterEnabled;
        this.badWeatherFilter = new BadWeatherFilter(bwFilterEnabled);
        this.marketRegimeFilter = new MarketRegimeFilter(config.marketRegimeFilterEnabled);
    }

    public void setPeerCandles(Map<String, List<Candle>> peerCandles) {
        this.peerCandles =
                peerCandles != null
                        ? new ConcurrentHashMap<>(peerCandles)
                        : new ConcurrentHashMap<>();
    }

    /**
     * Set position for a ticker (used in backtest for TMON@ cash parking sync).
     */
    public void setPosition(String ticker, Position position) {
        positionStore.put(ticker, position);
    }

    /**
     * Resolve instruments to trade.
     */
    protected List<String> resolveInstruments() {
        // Use configured instruments from unifiedTraderConfig
        return unifiedTraderConfig.getStocks();
    }

    public void run() {
        if (tradingService == null) {
            log(getStrategyName() + " stopped: tradingService is null.");
            return;
        }

        throttleApiCall();
        var initPortfolioCost = safeGetTotalPortfolioCost();
        var infoMessage =
                getStrategyName() + " started. Total Portfolio Cost: " + initPortfolioCost;
        log(infoMessage);

        // Start dashboard server
        try {
            dashboard = new DashboardServer(tradingService, EOD_CLOSE_TIME);
            dashboard.start();
            log("Dashboard started at http://localhost:" + dashboard.getPort());
            dashboard.updateBalance(initPortfolioCost);
        } catch (IOException ex) {
            log("Failed to start dashboard: " + ex.getMessage());
        }

        // Start TMON cash parking monitor (separate thread, runs every 5 minutes)
        if (unifiedTraderConfig.isTmonCashParkingEnabled()) {
            tmonCashParkingMonitor = new TmonCashParkingMonitor(
                    tradingService,
                    marketDataProvider,
                    cashParkingManager,
                    positionStore);
            Thread parkingThread = new Thread(tmonCashParkingMonitor, "TmonCashParkingMonitor");
            parkingThread.setDaemon(true);
            parkingThread.start();
            log("TMON cash parking monitor started (interval: 5 min)");
        }

        // Start loss streak monitor (separate thread, checks broker history periodically)
        if (mainConfig != null && mainConfig.isLossStreakEnabled()) {
            lossStreakMonitor =
                    new LossStreakMonitor(
                            tradingService,
                            mainConfig.getLossStreakThreshold(),
                            mainConfig.getLossStreakCheckIntervalMinutes() * 60_000L,
                            cashParkingManager.getParkingTicker(),
                            this::haltTrading);
            Thread lossStreakThread = new Thread(lossStreakMonitor, "LossStreakMonitor");
            lossStreakThread.setDaemon(true);
            lossStreakThread.start();
            log(
                    "Loss streak monitor started (interval: "
                            + mainConfig.getLossStreakCheckIntervalMinutes()
                            + " min, threshold: "
                            + mainConfig.getLossStreakThreshold()
                            + ")");
        }

        List<String> allTickers = resolveInstruments();
        List<String> activeTickers = new ArrayList<>();
        for (String ticker : allTickers) {
            try {
                if (unifiedTraderConfig.getTickerParams(ticker).enabled) {
                    activeTickers.add(ticker);
                }
            } catch (Exception ex) {
                log("Failed to load config for ticker " + ticker + ": " + ex.getMessage());
            }
        }

        log(
                getStrategyName()
                        + ": active instruments ("
                        + activeTickers.size()
                        + "): "
                        + activeTickers);

        if (activeTickers.isEmpty()) {
            log("No active tickers found. " + getStrategyName() + " stopped.");
            return;
        }

        restoreTrackedPositions(activeTickers);

        onDailyReset();

        log("Loading historical candles for " + activeTickers.size() + " tickers...");
        loadCandlesForAllTickers(activeTickers);
        log("Candle cache initialized. Stats: " + CandleRepository.getInstance().getStats());

        if (!isWorkingHours() && isTradingDay() && timeProvider.now().toLocalTime().isBefore(WORK_START_TIME)) {
            log("Trading session not started yet (current: " + timeProvider.now().toLocalTime() + ", start: " + WORK_START_TIME + "). Waiting...");
        }

        while (!isWorkingHours() && isTradingDay() && !tradingHalted) {
            LocalTime now = timeProvider.now().toLocalTime();
            if (now.isBefore(WORK_START_TIME)) {
                sleep(60_000);
            } else {
                break;
            }
        }

        if (!isWorkingHours()) {
            var message =
                    getStrategyName() + ": outside working hours, closing positions if needed.";
            log(message);
            closeAllPositions(tradingService, unifiedTraderConfig);
            return;
        }

        ExecutorService executor = Executors.newFixedThreadPool(activeTickers.size() + 1);
        Map<String, Double> capitalAllocation = computeCapitalAllocation(activeTickers);

        try {
            List<CompletableFuture<Void>> tasks = new ArrayList<>();

            tasks.add(
                    runAsync(
                            () -> {
                                while (isWorkingHours() && !tradingHalted) {
                                    try {
                                        refreshPeerCandles(activeTickers);
                                    } catch (Exception ex) {
                                        log("Failed to refresh peer candles: " + ex.getMessage());
                                    }
                                    sleep(60_000);
                                }
                            },
                            executor));

            for (String name : activeTickers) {
                double allocatedBalance = capitalAllocation.getOrDefault(name, 0.0);
                tasks.add(
                        runAsync(
                                () -> {
                                    while (isWorkingHours() && !tradingHalted) {
                                        processTicker(
                                                name,
                                                tradingService,
                                                unifiedTraderConfig,
                                                allocatedBalance);
                                        sleep(30_000);
                                    }
                                },
                                executor));
            }

            allOf(tasks.toArray(new CompletableFuture[0])).join();
        } finally {
            if (tmonCashParkingMonitor != null) {
                tmonCashParkingMonitor.stop();
                log("TMON cash parking monitor stopped");
            }
            if (lossStreakMonitor != null) {
                lossStreakMonitor.stop();
                log("Loss streak monitor stopped");
            }
            if (dashboard != null) {
                dashboard.stop();
            }
            closeAllPositions(tradingService, unifiedTraderConfig);
            shutdownExecutor(executor);

            var endPortfolioCost = safeGetTotalPortfolioCost();
            var message = getStrategyName() + " stopped. Total Portfolio Cost: " + endPortfolioCost;
            log(message);
        }
    }

    protected abstract String getStrategyName();

    /** Halt trading: block new entries and close all positions. Called by LossStreakMonitor. */
    private void haltTrading() {
        tradingHalted = true;
        if (tmonCashParkingMonitor != null) {
            tmonCashParkingMonitor.stop();
            log("TMON cash parking monitor stopped (loss streak halt)");
        }
        log("LOSS_STREAK: Halting trading, closing all positions...");
        closeAllPositions(tradingService, unifiedTraderConfig);
        log("LOSS_STREAK: Strategy will stop");
    }

    public abstract TradingDecision decide(
            String ticker,
            List<Candle> hourCandles,
            List<Candle> minuteCandles,
            Position position,
            double balance,
            boolean incrementCandlesHeld);

    protected void refreshPeerCandles(List<String> tickers) {
        if (tradingService == null) {
            return;
        }

        CandleRepository repository = CandleRepository.getInstance();
        OffsetDateTime now = timeProvider != null ? timeProvider.nowOffset() : OffsetDateTime.now();

        for (String ticker : tickers) {
            try {
                TickerInfo info = findTickerInfo(ticker);
                if (info == null || info.getFigi() == null) {
                    continue;
                }

                List<Candle> hourCandles = loadCandlesFromApi(ticker, info.getFigi(), now, "HOUR");
                if (hourCandles != null && !hourCandles.isEmpty()) {
                    repository.putCandles(ticker, "HOUR", hourCandles);
                }

                List<Candle> minCandles = loadCandlesFromApi(ticker, info.getFigi(), now, "5_MIN");
                if (minCandles != null && !minCandles.isEmpty()) {
                    repository.putCandles(ticker, "5_MIN", minCandles);
                }
            } catch (Exception ex) {
                logThrottled(
                        "refreshPeer_" + ticker,
                        "refreshPeerCandles failed for " + ticker + ": " + resolveRootMessage(ex),
                        5);
            }
        }
    }

    public void processTicker(
            String name,
            TradingService tradingService,
            UnifiedTraderConfig unifiedTraderConfig,
            double allocatedBalance) {
        if (tradingHalted) {
            return;
        }

        // Set trading in progress flag to prevent TMON monitor from interfering
        if (tmonCashParkingMonitor != null) {
            tmonCashParkingMonitor.setTradingInProgress(true);
        }

        Long cooldownUntil = tickerCooldown.get(name);
        if (cooldownUntil != null) {
            long remaining = cooldownUntil - timeProvider.currentTimeMillis();
            if (remaining > 0) {
                String cooldownMessage =
                        "Ticker "
                                + name
                                + " is on cooldown for "
                                + (remaining / 1000)
                                + "s, skipping.";
                if (cashParkingManager.isParkingTicker(name)) {
                    logThrottled(name + "_cooldown", cooldownMessage, 30);
                } else {
                    log(cooldownMessage);
                }
                return;
            } else {
                tickerCooldown.remove(name);
                log("Ticker " + name + " cooldown expired, resuming.");
            }
        }

        ReentrantLock lock = tickerLocks.computeIfAbsent(name, k -> new ReentrantLock());
        lock.lock();
        try {
            if (!isWorkingHours()) {
                return;
            }

            UnifiedTraderConfig.TickerParams tickerParams =
                    unifiedTraderConfig.getTickerParams(name);
            if (!tickerParams.enabled) {
                log("Ticker " + name + " disabled, skipping.");
                return;
            }

            TickerInfo ticker = findTickerInfo(name);
            if (ticker == null) {
                log("Ticker " + name + " not found, skipping.");
                return;
            }

            CandleRepository repository = CandleRepository.getInstance();
            List<Candle> hourCandles = repository.getLastCandles(name, "HOUR", 100);
            if (hourCandles == null || hourCandles.isEmpty()) {
                logThrottled(
                        name + "_no_hour",
                        "No hourly candles for " + name + " (cache empty), skipping.",
                        5);
                return;
            }

            boolean useMinCandles = tickerParams.useMinuteCandles;
            List<Candle> minuteCandles;
            if (useMinCandles) {
                minuteCandles = repository.getLastCandles(name, "5_MIN", 100);
                if (minuteCandles == null || minuteCandles.isEmpty()) {
                    minuteCandles = hourCandles;
                }
            } else {
                minuteCandles = hourCandles;
            }

            Position storedPosition = positionStore.getOrDefault(name, new Position());

            if (storedPosition.quantity > 0 && mainConfig != null && mainConfig.isSandbox()) {
                checkAndClosePositionBySLTP(name, ticker, storedPosition);
                storedPosition = positionStore.getOrDefault(name, new Position());
            }

            boolean hourChanged = false;
            if (storedPosition.quantity > 0) {
                String lastHourBar = hourCandles.get(hourCandles.size() - 1).time;
                String prevSeen = lastSeenHourBarByTicker.get(name);

                if (prevSeen == null || !prevSeen.equals(lastHourBar)) {
                    hourChanged = true;
                    lastSeenHourBarByTicker.put(name, lastHourBar);
                }
            } else {
                lastSeenHourBarByTicker.remove(name);
            }

            double balance = allocatedBalance > 0.0 ? allocatedBalance : orderExecutor.getAvailableCash();

            // Include cash parking value into effective cash for position sizing,
            // so decide() can size a position using cash parked in the parking ticker
            // Read parking position from broker (not local store) to handle parallel execution
            double effectiveBalance = balance;
            if (cashParkingManager.isParkingEnabled()
                && !cashParkingManager.isParkingTicker(name)
                && (tradingService != null || marketDataProvider != null)) {
                try {
                    String parkingTicker = cashParkingManager.getParkingTicker();
                    TickerType parkingType = cashParkingManager.getParkingTickerType();
                    PositionInfo parkingInfo =
                            tradingService != null
                                    ? tradingService.getCurrentPositions(parkingType, parkingTicker)
                                    : marketDataProvider.getCurrentPositions(parkingType, parkingTicker);
                    if (parkingInfo != null && parkingInfo.getBalance() > 0) {
                        double parkingQty = Math.abs(parkingInfo.getBalance());
                        Double parkingPrice = parkingInfo.getAveragePositionPrice();
                        if (parkingPrice != null && parkingPrice > 0) {
                            double parkingValue = parkingQty * parkingPrice;
                            effectiveBalance = balance + parkingValue;
                            if (unifiedTraderConfig != null && unifiedTraderConfig.isLogEffectiveBalance()) {
                                log(
                                        "EFFECTIVE-BALANCE "
                                                + name
                                                + ": cash="
                                                + String.format("%.2f", balance)
                                                + " + parking="
                                                + String.format("%.2f", parkingValue)
                                                + " = "
                                                + String.format("%.2f", effectiveBalance));
                            }
                        }
                    }
                } catch (Exception ignored) {
                    // no parking position in broker
                }
            }

            TradingDecision decision =
                    decide(name, hourCandles, minuteCandles, storedPosition, effectiveBalance, hourChanged);

            if ("HOLD".equals(decision.action)) {
                if (unifiedTraderConfig.isLogHoldReasons()) {
                    logThrottled(
                        "hold_" + name,
                        "HOLD "
                            + name
                            + ": reason="
                            + decision.reason
                            + " balance="
                            + String.format("%.2f", balance),
                        30);
                }
            } else {
                logThrottled(
                    "decision_" + name,
                    "DECISION "
                        + name
                        + ": hourCandles="
                        + hourCandles.size()
                        + " minuteCandles="
                        + minuteCandles.size()
                        + " action="
                        + decision.action
                        + " reason="
                        + decision.reason
                        + " quantity="
                        + decision.quantity
                        + " balance="
                        + String.format("%.2f", balance),
                    5);
            }

            if (decision.updatedPosition != null && "HOLD".equals(decision.action)) {
                positionStore.put(name, decision.updatedPosition);
                syncProtectiveOrdersIfNeeded(
                        name, ticker, storedPosition, decision.updatedPosition);
            }

            if ("OPEN".equals(decision.action)) {
                // Free cash from parking only for the amount missing for the trade
                if (cashParkingManager.isParkingEnabled()
                        && !cashParkingManager.isParkingTicker(name)
                        && decision.quantity > 0
                        && decision.entryPrice != null) {
                    try {
                        String parkingTicker = cashParkingManager.getParkingTicker();
                        TickerType parkingType = cashParkingManager.getParkingTickerType();
                        // read parking position from broker (not local store) to get actual qty
                        PositionInfo parkingInfo =
                                tradingService != null
                                        ? tradingService.getCurrentPositions(parkingType, parkingTicker)
                                        : marketDataProvider.getCurrentPositions(parkingType, parkingTicker);
                        if (parkingInfo != null && parkingInfo.getBalance() > 0) {
                            int parkingQty = parkingInfo.getBalance();
                            Double parkingPriceDouble = parkingInfo.getAveragePositionPrice();
                            double parkingPrice =
                                    parkingPriceDouble != null && parkingPriceDouble > 0
                                            ? parkingPriceDouble
                                            : decision.entryPrice;
                            int lotSize = ticker.getLot() != null ? Math.max(1, ticker.getLot()) : 1;
                            double positionValue = decision.quantity * decision.entryPrice * lotSize;
                            double availableCash0 = orderExecutor.getAvailableCash();
                            double missing = positionValue - availableCash0;
                            if (missing > 0 && parkingQty > 0 && parkingPrice > 0) {
                                // sell only the required parking value to free cash for the trade
                                TickerInfo.Key parkingKey = new TickerInfo.Key(parkingTicker, parkingType);
                                TickerInfo parkingTickerInfo = tradingService != null
                                        ? tradingService.searchTicker(parkingKey)
                                        : null;
                                int parkingLotSize =
                                        parkingTickerInfo != null && parkingTickerInfo.getLot() != null
                                                ? Math.max(1, parkingTickerInfo.getLot())
                                                : 1;
                                double parkingLotCost = parkingPrice * parkingLotSize;
                                // lots needed to cover the missing amount
                                int neededLots = (int) Math.ceil(missing / parkingLotCost);
                                int parkingLotsToSell = Math.min(neededLots, parkingQty);
                                if (parkingLotsToSell > 0) {
                                    double cashToFree = parkingLotsToSell * parkingLotCost;
                                    if (tradingService != null) {
                                        tradingService.sellByMarketWithDetails(
                                                parkingTicker, parkingType, cashToFree, 0.0, 0.0);
                                    } else {
                                        marketDataProvider.sellByMarket(
                                                parkingTicker, parkingType, cashToFree);
                                    }
                                    log(
                                            "PARTIALFREE "
                                                    + name
                                                    + ": sold "
                                                    + parkingTicker
                                                    + " value="
                                                    + String.format("%.2f", cashToFree)
                                                    + " ("
                                                    + parkingLotsToSell
                                                    + " lots) to cover missing "
                                                    + String.format("%.2f", missing)
                                                    + ", positionValue="
                                                    + String.format("%.2f", positionValue)
                                                    + ", availableCash="
                                                    + String.format("%.2f", availableCash0)
                                                    + ", "
                                                    + parkingTicker
                                                    + " remaining (est.)="
                                                    + (parkingQty - parkingLotsToSell));
                                }
                            }
                        }
                    } catch (Exception ex) {
                        log(
                                "PARTIALFREE_FAIL: "
                                        + name
                                        + ": "
                                        + ex.getMessage());
                    }
                }
                openPosition(name, ticker, hourCandles, decision);
            }

            if ("CLOSE".equals(decision.action)) {
                closePosition(name, ticker, storedPosition, decision);
            }

            if ("PARTIAL_CLOSE".equals(decision.action)) {
                partialClosePosition(name, ticker, storedPosition, decision);
            }
        } catch (Exception ex) {
            long cooldownExpiry = timeProvider.currentTimeMillis() + COOLDOWN_DURATION_MS;
            tickerCooldown.put(name, cooldownExpiry);
            String message = getStrategyName() + " error for " + name + ": " + resolveRootMessage(ex);
            logThrottled(name + "_error", message, 5);
        } finally {
            // Reset trading in progress flag after processing is complete
            if (tmonCashParkingMonitor != null) {
                tmonCashParkingMonitor.setTradingInProgress(false);
            }
            lock.unlock();
        }
    }

    protected void openPosition(
            String name, TickerInfo ticker, List<Candle> candles, TradingDecision decision) {
        log("Decision for " + name + ": " + decision.action + " (" + decision.reason + ")");
        if (decision.updatedPosition == null || decision.quantity <= 0) {
            logOpenCandidateSkipped(name, "invalid_open_decision", decision);
            log("Invalid OPEN decision for " + name + ", skipping.");
            return;
        }

        if (!"BUY".equals(decision.updatedPosition.direction)
                && !"SELL".equals(decision.updatedPosition.direction)) {
            logOpenCandidateSkipped(name, "invalid_direction", decision);
            log("Invalid direction for " + name + ", skipping.");
            return;
        }

        // Pre-trade: sell parking to free cash for new positions
        if (positionStore != null) {
            String parkingTicker = cashParkingManager.getParkingTicker();
            Position parkingPos = parkingTicker != null ? positionStore.get(parkingTicker) : null;
            if (parkingPos != null
                    && parkingPos.quantity > 0
                    && !cashParkingManager.isParkingTicker(name)
                    && "BUY".equals(decision.updatedPosition.direction)) {
                try {
                    TickerInfo parkingInfo = findTickerInfo(parkingTicker);
                    if (parkingInfo != null) {
                        log(
                                "CASH_FREETRIGGER "
                                        + name
                                        + ": "
                                        + parkingTicker
                                        + " parked="
                                        + parkingPos.quantity
                                        + " selling to free cash for "
                                        + name);
                        TickerType parkingType = cashParkingManager.getParkingTickerType();
                        if (tradingService != null) {
                            tradingService.closeLongByMarket(parkingTicker, parkingType);
                        } else {
                            marketDataProvider.closeLongByMarket(parkingTicker, parkingType);
                        }
                        positionStore.remove(parkingTicker);
                        log(parkingTicker + " sold, positionStore cleared for new position");
                    }
                } catch (Exception ex) {
                    log(
                            "CASH_FREEFAIL: Failed to sell "
                                    + parkingTicker
                                    + " for "
                                    + name
                                    + ": "
                                    + ex.getMessage());
                }
            }
        }

        // Проверяем максимальное количество одновременных позиций
        long currentPositionCount =
                positionStore.values().stream().filter(pos -> pos.quantity > 0).count();

        if (currentPositionCount >= MAX_CONCURRENT_POSITIONS) {
            logOpenCandidateSkipped(name, "max_concurrent_positions_reached", decision);
            log(
                    "Maximum concurrent positions reached ("
                            + MAX_CONCURRENT_POSITIONS
                            + "), skipping "
                            + name);
            return;
        }

        double entryPrice =
                decision.entryPrice != null
                        ? decision.entryPrice
                        : candles.get(candles.size() - 1).close;

        int lotSize = ticker.getLot() != null ? Math.max(1, ticker.getLot()) : 1;

        // Get live price from market data provider (works in both backtest and live)
        MarketPrices prices = marketDataProvider.getLivePrices(name);
        double liveAskPrice = prices.getAsk() != null ? prices.getAsk() : entryPrice;
        if (liveAskPrice <= 0.0) {
            logOpenCandidateSkipped(name, "no_live_price", decision);
            return;
        }

        // Use strategy-computed quantity
        int qty = decision.quantity;
        boolean isTmonCashParking = cashParkingManager.isParkingTicker(name);
        if (isTmonCashParking) {
            double availableCash = orderExecutor.getAvailableCash();
            qty = (int) Math.floor(availableCash / (liveAskPrice * lotSize));
            if (qty <= 0) {
                logOpenCandidateSkipped(name, "insufficient_cash", decision);
                return;
            }
        }
        double positionValue = qty * liveAskPrice * lotSize;
        double slPercent;
        double tpPercent;
        if (isTmonCashParking) {
            slPercent = 0.0;
            tpPercent = 0.0;
        } else {
            double slPrice =
                    decision.stopLoss != null
                            ? decision.stopLoss
                            : "BUY".equals(decision.updatedPosition.direction)
                                    ? entryPrice * 0.98
                                    : entryPrice * 1.02;
            double tpPrice =
                    decision.takeProfit != null
                            ? decision.takeProfit
                            : "BUY".equals(decision.updatedPosition.direction)
                                    ? entryPrice * 1.04
                                    : entryPrice * 0.96;
            slPercent = abs(entryPrice - slPrice) / entryPrice * 100;
            tpPercent = abs(tpPrice - entryPrice) / entryPrice * 100;
        }

        String openingLogMessage =
                "Opening "
                        + decision.updatedPosition.direction
                        + " for "
                        + name
                        + ": qty="
                        + qty
                        + ", entry="
                        + liveAskPrice
                        + ", value="
                        + positionValue
                        + ", SL="
                        + String.format("%.2f", slPercent)
                        + "%"
                        + ", TP="
                        + String.format("%.2f", tpPercent)
                        + "%";

        if (cashParkingManager.isParkingTicker(name)) {
            logThrottled(name + "_opening", openingLogMessage, 5);
        } else {
            log(openingLogMessage);
        }

        try {
            // Parking ticker bypasses orderExecutor to avoid the 1% safety margin
            // (it buys with the entire available cash, so the margin would always fail)
            OrderExecutor.ExecutionResult orderResult;
            if (isTmonCashParking && tradingService != null) {
                double cash = orderExecutor.getAvailableCash();
                // Re-check capital to guard against race with TMON monitor path
                double minCost = liveAskPrice * lotSize;
                if (cash < minCost) {
                    logOpenCandidateSkipped(name, "insufficient_cash_at_execution", decision);
                    log("Insufficient capital to buy " + name
                            + " (have=" + String.format("%.2f", cash)
                            + ", need=" + String.format("%.2f", minCost) + "), skipping");
                    return;
                }
                TradingService.OrderExecutionResult r =
                        tradingService.buyByMarketWithDetails(
                                name, ticker.getType(), cash, tpPercent, slPercent);
                if (r.isSuccess()) {
                    orderResult = OrderExecutor.ExecutionResult.success(
                            r.getExecutedCount(), r.getExecutedPrice());
                } else {
                    orderResult = OrderExecutor.ExecutionResult.failed(
                            r.getErrorMessage());
                }
            } else if ("BUY".equals(decision.updatedPosition.direction)) {
                orderResult = orderExecutor.buy(name, qty, slPercent, tpPercent);
            } else { // SELL
                orderResult = orderExecutor.sell(name, qty, slPercent, tpPercent);
            }

            if (!orderResult.isSuccess()) {
                logOpenCandidateSkipped(name, "order_execution_failed", decision);
                String failedLogMessage =
                        "Failed to open " + decision.updatedPosition.direction + " for " + name + ".";
                log(failedLogMessage);
                return;
            }

            // Create position from execution result
            Position executedPosition = new Position(
                    decision.updatedPosition.direction,
                    orderResult.getExecutedPrice(),
                    decision.stopLoss,
                    decision.takeProfit,
                    null,
                    orderResult.getExecutedQuantity(),
                    0,
                    0,
                    decision.updatedPosition.appliedLeverage,
                    false);

            positionStore.put(name, executedPosition);
            lastSeenHourBarByTicker.put(name, candles.get(candles.size() - 1).time);
        } catch (Exception ex) {
            log(
                    "Failed to open "
                            + decision.updatedPosition.direction
                            + " for "
                            + name
                            + ": "
                            + ex.getMessage());
        }
    }

    private void logOpenCandidateSkipped(String name, String reason, TradingDecision decision) {
        String direction =
                decision != null && decision.updatedPosition != null
                        ? decision.updatedPosition.direction
                        : "null";
        int quantity = decision != null ? decision.quantity : 0;
        Double entryPrice = decision != null ? decision.entryPrice : null;
        String signal = decision != null ? decision.reason : null;
        String message =
                "OPEN candidate skipped for "
                        + name
                        + ": reason="
                        + reason
                        + ", signal="
                        + signal
                        + ", direction="
                        + direction
                        + ", qty="
                        + quantity
                        + ", entry="
                        + (entryPrice != null ? entryPrice : 0.0);
        if (cashParkingManager.isParkingTicker(name)) {
            logThrottled(name + "_skipped_" + reason, message, 5);
        } else {
            log(message);
        }
    }

    protected void closePosition(
            String name, TickerInfo ticker, Position storedPosition, TradingDecision decision) {
        log("Decision for " + name + ": " + decision.action + " (" + decision.reason + ")");
        if (storedPosition.quantity <= 0) {
            log("CLOSE decision but no position for " + name + ", skipping.");
            return;
        }

        log(
                "Closing position for "
                        + name
                        + ": "
                        + storedPosition.quantity
                        + " shares, direction="
                        + storedPosition.direction
                        + ", reason="
                        + decision.reason);

        OrderExecutor.ExecutionResult closeResult;
        if ("BUY".equals(storedPosition.direction)) {
            closeResult = orderExecutor.closeLong(name);
        } else if ("SELL".equals(storedPosition.direction)) {
            closeResult = orderExecutor.closeShort(name);
        } else {
            closeResult = OrderExecutor.ExecutionResult.failed("Invalid direction");
        }

        if (closeResult.isSuccess()) {
            int closedQuantity =
                    closeResult.getExecutedQuantity() > 0
                            ? closeResult.getExecutedQuantity()
                            : storedPosition.quantity;
            if (closedQuantity <= 0) {
                log("Failed to close position for " + name + " (executed quantity is zero)");
                return;
            }

            double entryPrice = storedPosition.entryPrice != null ? storedPosition.entryPrice : 0.0;
            double exitPrice =
                    closeResult.getExecutedPrice() != null && closeResult.getExecutedPrice() > 0.0
                            ? closeResult.getExecutedPrice()
                            : decision.entryPrice != null ? decision.entryPrice : 0.0;
            double pnl = calculatePnlForQuantity(storedPosition, exitPrice, closedQuantity);
            double stopLoss =
                    storedPosition.stopLoss != null ? storedPosition.stopLoss : entryPrice;

            if (closedQuantity >= storedPosition.quantity) {
                positionStore.put(name, getCooldownPosition());
                lastSeenHourBarByTicker.remove(name);
            } else {
                int remainingQuantity = storedPosition.quantity - closedQuantity;
                positionStore.put(
                        name,
                        new Position(
                                storedPosition.direction,
                                storedPosition.entryPrice,
                                storedPosition.stopLoss,
                                storedPosition.takeProfit,
                                remainingQuantity,
                                storedPosition.candlesHeld,
                                storedPosition.cooldownRemaining));
                log(
                        "Position for "
                                + name
                                + " partially closed: closed="
                                + closedQuantity
                                + ", remaining="
                                + remainingQuantity);
            }
            onTradeClosed(
                    name, pnl, entryPrice, exitPrice, closedQuantity, storedPosition.direction);
        } else {
            log("Failed to close position for " + name + " (may not exist in broker account)");
        }
    }

    protected void partialClosePosition(
            String name, TickerInfo ticker, Position storedPosition, TradingDecision decision) {
        log("PARTIAL_CLOSE for " + name + ": " + decision.quantity + " shares (TP1 hit)");

        if (storedPosition.quantity <= 0) {
            log("PARTIAL_CLOSE but no position for " + name + ", skipping.");
            return;
        }

        int closeQty = decision.quantity;
        if (closeQty <= 0 || closeQty >= storedPosition.quantity) {
            log("Invalid partial close quantity: " + closeQty + ", position: " + storedPosition.quantity);
            return;
        }

        OrderExecutor.ExecutionResult closeResult;
        if ("BUY".equals(storedPosition.direction)) {
            closeResult = orderExecutor.partialCloseLong(name, closeQty);
        } else {
            closeResult = orderExecutor.partialCloseShort(name, closeQty);
        }

        if (closeResult.isSuccess()) {
            double entryPrice = storedPosition.entryPrice != null ? storedPosition.entryPrice : 0.0;
            double exitPrice = closeResult.getExecutedPrice() != null ? closeResult.getExecutedPrice() : decision.entryPrice != null ? decision.entryPrice : 0.0;
            double pnl = calculatePnlForQuantity(storedPosition, exitPrice, closeQty);

            positionStore.put(name, decision.updatedPosition);

            log(
                    "PARTIAL_CLOSE " + name + ": closed=" + closeQty + ", remaining=" + decision.updatedPosition.quantity +
                    ", pnl=" + String.format("%.2f", pnl) + ", newStop=" + decision.updatedPosition.stopLoss);

            onTradeClosed(name, pnl, entryPrice, exitPrice, closeQty, storedPosition.direction);
        } else {
            log("Failed to partial close " + name + ": " + closeResult.getErrorMessage());
        }
    }

    /** Calculate PnL for a closed position. */
    private double calculatePnl(Position position, double exitPrice) {
        if (position.entryPrice == null || exitPrice <= 0) {
            return 0.0;
        }
        if ("BUY".equals(position.direction)) {
            return (exitPrice - position.entryPrice) * position.quantity;
        } else {
            return (position.entryPrice - exitPrice) * position.quantity;
        }
    }

    private double calculatePnlForQuantity(Position position, double exitPrice, int quantity) {
        if (position.entryPrice == null || exitPrice <= 0 || quantity <= 0) {
            return 0.0;
        }
        if ("BUY".equals(position.direction)) {
            return (exitPrice - position.entryPrice) * quantity;
        }
        return (position.entryPrice - exitPrice) * quantity;
    }

    private void syncProtectiveOrdersIfNeeded(
            String name, TickerInfo ticker, Position previousPosition, Position updatedPosition) {
        if (tradingService == null || ticker == null || updatedPosition == null || updatedPosition.quantity <= 0) {
            return;
        }

        boolean stopChanged =
                !java.util.Objects.equals(previousPosition.stopLoss, updatedPosition.stopLoss);
        boolean takeChanged =
                !java.util.Objects.equals(previousPosition.takeProfit, updatedPosition.takeProfit);
        if (!stopChanged && !takeChanged) {
            return;
        }

        try {
            throttleApiCall();
            tradingService.syncProtectiveOrders(name, ticker.getType(), updatedPosition);
        } catch (Exception ex) {
            log("Failed to sync protective orders for " + name + ": " + ex.getMessage());
        }
    }

    /**
     * Virtual SL/TP check for sandbox mode.
     * Server-side stop orders are disabled in sandbox, so we check and close positions locally.
     */
    private void checkAndClosePositionBySLTP(String name, TickerInfo ticker, Position position) {
        if (position.stopLoss == null && position.takeProfit == null) {
            return;
        }

        try {
            MarketPrices prices = marketDataProvider.getLivePrices(name);
            double currentPrice = prices.getBid() != null && prices.getBid() > 0
                    ? prices.getBid()
                    : (prices.getAsk() != null && prices.getAsk() > 0 ? prices.getAsk() : 0.0);

            if (currentPrice <= 0.0) {
                return;
            }

            boolean shouldClose = false;
            String reason = null;

            if ("BUY".equals(position.direction)) {
                if (position.stopLoss != null && currentPrice <= position.stopLoss) {
                    shouldClose = true;
                    reason = "STOP_LOSS";
                } else if (position.takeProfit != null && currentPrice >= position.takeProfit) {
                    shouldClose = true;
                    reason = "TAKE_PROFIT";
                }
            } else if ("SELL".equals(position.direction)) {
                if (position.stopLoss != null && currentPrice >= position.stopLoss) {
                    shouldClose = true;
                    reason = "STOP_LOSS";
                } else if (position.takeProfit != null && currentPrice <= position.takeProfit) {
                    shouldClose = true;
                    reason = "TAKE_PROFIT";
                }
            }

            if (shouldClose) {
                log("SANDBOX VIRTUAL SL/TP HIT: " + name + " " + reason +
                    " (dir=" + position.direction + ", entry=" + position.entryPrice +
                    ", SL=" + position.stopLoss + ", TP=" + position.takeProfit +
                    ", current=" + currentPrice + ")");

                TradingService.OrderExecutionResult closeResult;
                if ("BUY".equals(position.direction)) {
                    closeResult = tradingService.closeLongByMarketWithDetails(name, ticker.getType());
                } else {
                    closeResult = tradingService.closeShortByMarketWithDetails(name, ticker.getType());
                }

                if (closeResult != null && closeResult.isSuccess()) {
                    int closedQuantity = closeResult.getExecutedCount() > 0
                            ? closeResult.getExecutedCount() : position.quantity;
                    double exitPrice = closeResult.getExecutedPrice() != null
                            ? closeResult.getExecutedPrice() : currentPrice;
                    double entryPrice = position.entryPrice != null ? position.entryPrice : 0.0;
                    double pnl = calculatePnlForQuantity(position, exitPrice, closedQuantity);

                    positionStore.put(name, getCooldownPosition());
                    lastSeenHourBarByTicker.remove(name);
                    onTradeClosed(name, pnl, entryPrice, exitPrice, closedQuantity, position.direction);
                } else {
                    log("Failed to close position for " + name + " via TradingService");
                }
            }
        } catch (Exception ex) {
            log("Failed to check SL/TP for " + name + ": " + ex.getMessage());
        }
    }

    /**
     * Callback for trade closure (for Money Management integration). Override in subclasses to
     * register trade results.
     */
    public void onTradeClosed(
            String ticker,
            double pnl,
            double entryPrice,
            double exitPrice,
            int quantity,
            String direction) {
        log("onTradeClosed: " + ticker + " pnl=" + String.format("%.2f", pnl) + " qty=" + quantity);
        // Update dashboard statistics
        if (dashboard != null) {
            boolean isWin = pnl > 0;
            dashboard.updateStats(pnl, isWin);
            dashboard.addTrade(ticker, direction, quantity, exitPrice, pnl);
            dashboard.updateBalance(safeGetTotalPortfolioCost());
            log("Dashboard updated: " + ticker);
        } else {
            log("Dashboard is null, skipping update");
        }
    }

    /**
     * Callback for daily reset (for Money Management integration). Override in subclasses to reset
     * daily limits.
     */
    public void onDailyReset() {
        // Default: no-op. Override in UnifiedStrategy for MM integration.
    }

    protected Position getCooldownPosition() {
        return new Position(config.cooldownCandles);
    }

    /**
     * Checks if there are any active positions on tickers other than the cash parking ticker.
     * Used by cash parking logic to determine whether parking should be sold (to free cash
     * for other positions) or bought (when idle).
     */
    protected boolean hasActiveNonTmonPositions() {
        for (Map.Entry<String, Position> entry : positionStore.entrySet()) {
            if (cashParkingManager.isParkingTicker(entry.getKey())) {
                continue;
            }
            if (entry.getValue().quantity > 0) {
                return true;
            }
        }
        return false;
    }

    protected Double safeGetTotalPortfolioCost() {
        if (tradingService == null) {
            return 0.0;
        }
        try {
            return tradingService.getTotalPortfolioCost();
        } catch (Exception ex) {
            log("Failed to read portfolio cost: " + ex.getMessage());
            return 0.0;
        }
    }

    protected TickerInfo findTickerInfo(String name) {
        // First try TickerRepository (disk cache for Tinkoff instruments)
        TickerInfo found =
                TickerRepository.INSTANCE.getAll().values().stream()
                        .filter(
                                t ->
                                        t.getType() == TickerType.STOCK
                                                || t.getType() == TickerType.FEATURE
                                                || t.getType() == TickerType.ETF
                                                || t.getType() == TickerType.CRYPTO)
                        .filter(
                                t ->
                                        t.getName().equalsIgnoreCase(name)
                                                || t.getTicker().equalsIgnoreCase(name))
                        .findFirst()
                        .orElse(null);
        if (found != null) {
            return found;
        }

        return null;
    }

    private void restoreTrackedPositions(List<String> activeTickers) {
        if (tradingService == null) {
            return;
        }

        Set<String> activeTickerSet = new HashSet<>(activeTickers);
        restoreTrackedPositions(activeTickerSet, STOCK);
        restoreTrackedPositions(activeTickerSet, FEATURE);
        restoreTrackedPositions(activeTickerSet, CRYPTO);
        logRestoredPositionsReport();
    }

    private void restoreTrackedPositions(Set<String> activeTickers, TickerType tickerType) {
        Map<TickerInfo.Key, PositionInfo> currentPositions =
                tradingService.getCurrentPositions(tickerType);
        currentPositions.values().stream()
                .filter(positionInfo -> activeTickers.contains(positionInfo.getTicker()))
                .filter(positionInfo -> positionInfo.getBalance() != 0)
                .forEach(
                        positionInfo -> {
                            TickerInfo tickerInfo = findTickerInfo(positionInfo.getTicker());
                            if (tickerInfo == null) {
                                return;
                            }

                            String direction = positionInfo.getBalance() > 0 ? "BUY" : "SELL";
                            int quantity = Math.abs(positionInfo.getBalance());
                            Double entryPrice = positionInfo.getAveragePositionPrice();
                            Position restoredPosition =
                                    new Position(direction, entryPrice, null, null, quantity, 0);
                            restoredPosition =
                                    tradingService.restoreProtectivePosition(
                                            positionInfo.getTicker(), tickerType, restoredPosition);
                            positionStore.put(positionInfo.getTicker(), restoredPosition);
                            initializeLastSeenHourBar(positionInfo.getTicker(), tickerInfo);

                            log(
                                    "Restored tracked position for "
                                            + positionInfo.getTicker()
                                            + ": direction="
                                            + direction
                                            + ", quantity="
                                            + quantity
                                            + ", entry="
                                            + (entryPrice != null ? entryPrice : 0.0)
                                            + ", stopLoss="
                                            + (restoredPosition.stopLoss != null
                                                    ? restoredPosition.stopLoss
                                                    : 0.0)
                                            + ", takeProfit="
                                            + (restoredPosition.takeProfit != null
                                                    ? restoredPosition.takeProfit
                                                    : 0.0));

                            if (entryPrice != null) {
                                tradingService.syncProtectiveOrders(
                                        positionInfo.getTicker(), tickerType, restoredPosition);
                            }
                        });
    }

    private void initializeLastSeenHourBar(String ticker, TickerInfo tickerInfo) {
        try {
            List<Candle> lastCandles = tradingService.getLastCandles(ticker, tickerInfo.getType(), 1);
            if (lastCandles != null && !lastCandles.isEmpty()) {
                lastSeenHourBarByTicker.put(ticker, lastCandles.get(0).time);
            }
        } catch (Exception ex) {
            log("Failed to initialize last seen hour bar for " + ticker + ": " + ex.getMessage());
        }
    }

    private void logRestoredPositionsReport() {
        if (positionStore.isEmpty()) {
            log("Restored positions report: no tracked positions were recovered from portfolio.");
            return;
        }

        StringBuilder report = new StringBuilder("Restored positions report:");
        positionStore.forEach(
                (ticker, position) ->
                        report.append("\n - ")
                                .append(ticker)
                                .append(": direction=")
                                .append(position.direction)
                                .append(", quantity=")
                                .append(position.quantity)
                                .append(", entry=")
                                .append(position.entryPrice != null ? position.entryPrice : 0.0)
                                .append(", stopLoss=")
                                .append(position.stopLoss != null ? position.stopLoss : 0.0)
                                .append(", takeProfit=")
                                .append(position.takeProfit != null ? position.takeProfit : 0.0));
        log(report.toString());
    }

    protected List<Candle> loadCandlesFromApi(
            String name, String figi, OffsetDateTime now, String interval) {
        if (now == null && timeProvider != null) {
            now = timeProvider.nowOffset();
        }
        if (tradingService == null) {
            return null;
        }

        throttleApiCall();

        return tradingService.getCandles(
                figi,
                "HOUR".equals(interval)
                        ? now.minusMinutes(7 * 24 * 60)
                        : now.minusMinutes(6 * 60),
                now,
                interval);
    }

    protected void loadCandlesForAllTickers(List<String> tickers) {
        CandleRepository repository = CandleRepository.getInstance();
        OffsetDateTime now = timeProvider != null ? timeProvider.nowOffset() : OffsetDateTime.now();

        for (String ticker : tickers) {
            try {
                TickerInfo info = findTickerInfo(ticker);
                if (info == null || info.getFigi() == null) {
                    continue;
                }

                loadCandlesForTicker(ticker, info.getFigi(), now, "HOUR", 30, 7);
                loadCandlesForTicker(ticker, info.getFigi(), now, "5_MIN", 3 * 24, 3 * 24 * 7);
            } catch (Exception ex) {
                log("Failed to load candles for " + ticker + ": " + resolveRootMessage(ex));
            }
        }
    }

    private void loadCandlesForTicker(
            String ticker, String figi, OffsetDateTime now, String interval,
            int initialHours, int expandedHours) {
        CandleRepository repository = CandleRepository.getInstance();
        OffsetDateTime start = now.minusHours(initialHours);
        List<Candle> candles = loadCandlesFromApi(ticker, figi, start, interval);
        if (candles != null && !candles.isEmpty() && candles.size() >= MIN_CANDLES_THRESHOLD) {
            repository.putCandles(ticker, interval, candles);
            return;
        }
        start = now.minusHours(expandedHours);
        candles = loadCandlesFromApi(ticker, figi, start, interval);
        if (candles != null && !candles.isEmpty()) {
            repository.putCandles(ticker, interval, candles);
        }
    }

    protected boolean isTradingDay() {
        DayOfWeek day = timeProvider.now().getDayOfWeek();
        return day != DayOfWeek.SATURDAY && day != DayOfWeek.SUNDAY;
    }

    protected boolean isWorkingHours() {
        if (!isTradingDay()) {
            return false;
        }
        LocalTime now = timeProvider.now().toLocalTime();
        return !now.isBefore(WORK_START_TIME) && now.isBefore(EOD_CLOSE_TIME);
    }

    protected boolean isEndOfDayReached() {
        if (!isTradingDay()) {
            return true;
        }
        LocalTime now = timeProvider.now().toLocalTime();
        return !now.isBefore(EOD_CLOSE_TIME);
    }

    protected void throttleApiCall() {
        synchronized (API_LOCK) {
            long waitTime = API_CALL_DELAY_MS - (timeProvider.currentTimeMillis() - lastApiCallTime);
            if (waitTime > 0) {
                sleep(waitTime);
            }
            lastApiCallTime = timeProvider.currentTimeMillis();
        }
    }

    protected void closeAllPositions(
            TradingService tradingService, UnifiedTraderConfig unifiedTraderConfig) {
        if (tradingService == null) {
            return;
        }

        log("End-of-day reached. Closing all positions...");
        boolean anyClosed = false;

        for (Map.Entry<String, Position> entry : positionStore.entrySet()) {
            String tickerName = entry.getKey();
            Position position = entry.getValue();

            if (position.quantity <= 0) {
                continue;
            }

            try {
                UnifiedTraderConfig.TickerParams tickerParams =
                        unifiedTraderConfig.getTickerParams(tickerName);
                if (!tickerParams.enabled) {
                    continue;
                }

                TickerInfo ticker = findTickerInfo(tickerName);
                if (ticker == null) {
                    log("Ticker " + tickerName + " not found, skipping position close.");
                    continue;
                }

                log("Closing position for " + tickerName + ": " + position.quantity + " shares");
                throttleApiCall();

                boolean closed = false;
                if ("BUY".equals(position.direction)) {
                    closed = tradingService.closeLongByMarket(tickerName, ticker.getType());
                } else if ("SELL".equals(position.direction)) {
                    closed = tradingService.closeShortByMarket(tickerName, ticker.getType());
                }

                if (closed) {
                    positionStore.put(tickerName, getCooldownPosition());
                    lastSeenHourBarByTicker.remove(tickerName);
                    double exitPrice =
                            tradingService.getAvailablePrice(
                                    new TickerInfo.Key(tickerName, ticker.getType()));
                    double entryPrice = position.entryPrice != null ? position.entryPrice : 0.0;
                    double pnl = calculatePnl(position, exitPrice);
                    onTradeClosed(
                            tickerName,
                            pnl,
                            entryPrice,
                            exitPrice,
                            position.quantity,
                            position.direction);
                    anyClosed = true;
                } else {
                    log("Failed to close position for " + tickerName);
                }
            } catch (Exception ex) {
                log("Error closing position for " + tickerName + ": " + ex.getMessage());
            }
        }

        try {
            tradingService.closeAllByMarket(STOCK);
        } catch (Exception ex) {
            log("Failed to close all STOCK positions: " + ex.getMessage());
        }

        try {
            tradingService.closeAllByMarket(FEATURE);
        } catch (Exception ex) {
            log("Failed to close all FEATURE positions: " + ex.getMessage());
        }

        if (anyClosed) {
            log("End-of-day position closing completed.");
        }
    }

    protected static void log(String message) {
        log(message, isBacktestMode());
    }

    protected static void log(String message, boolean silent) {
        if (silent) {
            return;
        }
        LoggingUtils.log(message);
    }

    /**
     * Logs message with throttling to prevent spam of repeated warnings.
     * Only logs if more than {@code throttleMinutes} have passed since the last log for this key.
     *
     * @param key unique identifier for the log category (e.g., "SPYUSDT_heartbeat")
     * @param message message to log
     * @param throttleMinutes minutes to wait between logs for the same key
     */
    protected void logThrottled(String key, String message, long throttleMinutes) {
        if (!isVerboseLogging()) {
            return;
        }
        long now = timeProvider.currentTimeMillis();
        long throttleMs = throttleMinutes * 60 * 1000L;
        Long lastTime = throttledLogLastTime.get(key);
        if (lastTime == null || (now - lastTime) >= throttleMs) {
            throttledLogLastTime.put(key, now);
            log(message);
        }
    }

    /** Verbose diagnostic logging flag; trades and errors are always logged. */
    protected boolean isVerboseLogging() {
        return unifiedTraderConfig == null || unifiedTraderConfig.isVerboseLoggingEnabled();
    }

    private static String resolveRootMessage(Throwable ex) {
        Throwable root = ex;
        while (root.getCause() != null && root.getCause() != root) {
            root = root.getCause();
        }
        String msg = root.getMessage();
        if (msg != null && !msg.isEmpty() && !"unknown error".equalsIgnoreCase(msg) && !msg.contains("Failed to get candles")) {
            return msg;
        }
        msg = ex.getMessage();
        if (msg != null && !msg.isEmpty() && !"unknown error".equalsIgnoreCase(msg) && !msg.contains("Failed to get candles")) {
            return msg;
        }
        return root.getClass().getSimpleName() + ": " + (msg != null ? msg : "null");
    }

    protected static void shutdownExecutor(ExecutorService executor) {
        executor.shutdown();
        try {
            if (!executor.awaitTermination(5, TimeUnit.SECONDS)) {
                executor.shutdownNow();
            }
        } catch (InterruptedException skip) {
            executor.shutdownNow();
            Thread.currentThread().interrupt();
        }
    }

    protected double ema(List<Candle> candles, int period) {
        if (candles == null || candles.isEmpty()) return 0.0;
        if (candles.size() < period) return candles.get(candles.size() - 1).close;

        double[] c = candles.stream().mapToDouble(cdl -> cdl.close).toArray();
        double k = 2.0 / (period + 1);
        double e = 0.0;

        for (int i = 0; i < period; i++) e += c[i];
        e /= period;

        for (int i = period; i < c.length; i++) {
            e = c[i] * k + e * (1 - k);
        }

        return e;
    }

    protected double atrVal(List<Candle> candles, int period) {
        if (candles == null || candles.size() < period + 1) return 0.0;

        double sum = 0.0;
        for (int i = candles.size() - period; i < candles.size(); i++) {
            Candle c = candles.get(i);
            Candle p = candles.get(i - 1);
            sum += max(max(c.high - c.low, abs(c.high - p.close)), abs(c.low - p.close));
        }
        return sum / period;
    }

    protected double rsiVal(List<Candle> candles, int period) {
        if (candles == null || candles.size() < period + 1) return 50.0;

        double g = 0.0;
        double l = 0.0;
        double[] c = candles.stream().mapToDouble(cdl -> cdl.close).toArray();

        for (int i = c.length - period; i < c.length; i++) {
            double ch = c[i] - c[i - 1];
            if (ch >= 0.0) g += ch;
            else l += abs(ch);
        }

        double ag = g / period;
        double al = l / period;
        if (al == 0.0) return 100.0;

        return 100.0 - (100.0 / (1.0 + ag / al));
    }

    protected double adxVal(List<Candle> candles, int period) {
        if (candles == null || candles.size() < period * 2 + 1) return 0.0;

        int n = candles.size();
        double[] tr = new double[n];
        double[] plusDM = new double[n];
        double[] minusDM = new double[n];

        for (int i = 1; i < n; i++) {
            Candle c = candles.get(i);
            Candle prev = candles.get(i - 1);

            tr[i] = max(max(c.high - c.low, abs(c.high - prev.close)), abs(c.low - prev.close));

            double up = c.high - prev.high;
            double dn = prev.low - c.low;

            plusDM[i] = (up > dn && up > 0) ? up : 0.0;
            minusDM[i] = (dn > up && dn > 0) ? dn : 0.0;
        }

        double trS = 0.0, pdmS = 0.0, mdmS = 0.0;
        for (int i = 1; i <= period; i++) {
            trS += tr[i];
            pdmS += plusDM[i];
            mdmS += minusDM[i];
        }

        double[] dx = new double[n];
        int dxStart = period;

        if (trS > 0) {
            double pDI = pdmS / trS * 100.0;
            double mDI = mdmS / trS * 100.0;
            double sum = pDI + mDI;
            dx[dxStart] = sum > 0 ? abs(pDI - mDI) / sum * 100.0 : 0.0;
        }

        for (int i = period + 1; i < n; i++) {
            trS = trS - (trS / period) + tr[i];
            pdmS = pdmS - (pdmS / period) + plusDM[i];
            mdmS = mdmS - (mdmS / period) + minusDM[i];

            if (trS > 0) {
                double pDI = pdmS / trS * 100.0;
                double mDI = mdmS / trS * 100.0;
                double sum = pDI + mDI;
                dx[i] = sum > 0 ? abs(pDI - mDI) / sum * 100.0 : 0.0;
            } else {
                dx[i] = 0.0;
            }
        }

        if (n < 2 * period) return dx[n - 1];

        double adxSum = 0.0;
        for (int i = period; i < 2 * period; i++) {
            adxSum += dx[i];
        }
        double adx = adxSum / period;

        for (int i = 2 * period; i < n; i++) {
            adx = (adx * (period - 1) + dx[i]) / period;
        }

        return adx;
    }

    protected double emaAtr(List<Candle> candles, int period) {
        if (candles == null || candles.isEmpty()) {
            return 0.0;
        }

        if (candles.size() < period + 5) {
            return atrVal(candles, period);
        }

        List<Double> vals = new ArrayList<>();
        for (int i = Math.max(0, candles.size() - 20); i < candles.size(); i++) {
            if (i < period + 1) {
                continue;
            }

            double s = 0.0;
            for (int j = i - period; j < i; j++) {
                Candle c = candles.get(j);
                Candle p = candles.get(j - 1);
                s += max(max(c.high - c.low, abs(c.high - p.close)), abs(c.low - p.close));
            }
            vals.add(s / period);
        }

        return vals.isEmpty()
                ? atrVal(candles, period)
                : vals.stream().mapToDouble(v -> v).average().orElse(0.0);
    }

    protected Map<String, Double> computeCapitalAllocation(List<String> tickers) {
        Map<String, Double> weights = new HashMap<>();
        double totalWeight = 0.0;

        for (String ticker : tickers) {
            try {
                UnifiedTraderConfig.TickerParams params =
                        unifiedTraderConfig.getTickerParams(ticker);
                if (!params.enabled) {
                    continue;
                }

                double weight = params.allocationWeight > 0.0 ? params.allocationWeight : 1.0;
                weights.put(ticker, weight);
                totalWeight += weight;
            } catch (Exception ex) {
                log(
                        "Failed to read allocation weight for ticker "
                                + ticker
                                + ": "
                                + ex.getMessage());
            }
        }

        if (weights.isEmpty() || totalWeight <= 0.0) {
            return new HashMap<>();
        }

        double totalCash;
        try {
            totalCash = tradingService.getAvailableCash();
        } catch (Exception ex) {
            log("Failed to read available cash for allocation: " + ex.getMessage());
            return new HashMap<>();
        }

        String parkingTicker = cashParkingManager.getParkingTicker();
        boolean tmonCashParking =
                parkingTicker != null
                        && weights.containsKey(parkingTicker)
                        && unifiedTraderConfig.isTmonCashParkingEnabled();

        // Each position gets totalCash / MAX_CONCURRENT_POSITIONS, not totalCash / tickers.size()
        double capitalPerPosition = totalCash / MAX_CONCURRENT_POSITIONS;

        Map<String, Double> allocation = new HashMap<>();
        for (Map.Entry<String, Double> e : weights.entrySet()) {
            if (tmonCashParking && cashParkingManager.isParkingTicker(e.getKey())) {
                // Cash parking always uses real-time available cash via
                // getAvailableCash() in processTicker(), not a stale startup snapshot.
                // Skipping allocation so allocatedBalance = 0.0 and the fallback kicks in.
            } else {
                allocation.put(e.getKey(), capitalPerPosition);
            }
        }

        return allocation;
    }
}
