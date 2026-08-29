package com.github.shk0da.goldendragon.strategy;

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
import com.github.shk0da.goldendragon.model.TickerCandle;
import com.github.shk0da.goldendragon.model.TickerInfo;
import com.github.shk0da.goldendragon.model.TickerType;
import com.github.shk0da.goldendragon.model.TradingDecision;
import com.github.shk0da.goldendragon.repository.TickerRepository;
import com.github.shk0da.goldendragon.service.TCSService;
import com.github.shk0da.goldendragon.time.LiveTimeProvider;
import com.github.shk0da.goldendragon.time.TimeProvider;
import com.github.shk0da.goldendragon.utils.IndicatorsUtil;
import com.github.shk0da.goldendragon.utils.LoggingUtils;
import ru.tinkoff.piapi.contract.v1.CandleInterval;
import ru.tinkoff.piapi.contract.v1.HistoricCandle;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.Timestamp;
import java.text.SimpleDateFormat;
import java.time.DayOfWeek;
import java.time.LocalTime;
import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
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
 *   <li>Executes orders via {@link TCSService} (broker API).
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
 *   <li>Load candles via {@link #loadOrRefreshCandles}: first check cache freshness ({@link
 *       #isCandleDataFresh}), then API if needed.
 *   <li>Hourly candles required; 5-minute only if {@code tickerParams.useMinuteCandles}.
 *   <li>Detect hourly bar change ({@code hourChanged}) for correct {@code candlesHeld} increment in
 *       open position.
 *   <li>Calculate balance: allocated capital or current liquidity.
 *   <li>Call abstract {@link #decide} — get {@link TradingDecision}.
 *   <li>Route by action:
 *       <ul>
 *         <li>{@code HOLD} — update position in {@link #positionStore}.
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
 *   <li>Call {@code tcsService.buyByMarket} / {@code sellByMarket} with market order and automatic
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
 *   <li>{@link #loadOrRefreshCandles} — two-level loading: first disk cache, then API on stale.
 *   <li>{@link #isCandleDataFresh} — check by day and hour/5-minute slot.
 *   <li>{@link #writeCandlesToFile} — incremental CSV write with deduplication by timestamp.
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
 *   <li>Торговая сессия: {@link #WORK_START_TIME} (08:30) — {@link #EOD_CLOSE_TIME} (21:00).
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
 *   <li>{@link SimpleDateFormat} обёрнут в {@link ThreadLocal}.
 * </ul>
 *
 * <h2>Интеграции</h2>
 *
 * <ul>
 *   <li>{@link TCSService} — брокерский API (Tinkoff Invest).
 *   <li>{@code TelegramNotifyService} — уведомления о запуске, сделках, ошибках.
  *   <li>{@link TickerRepository} — справочник инструментов.
  * </ul>
  */
 public abstract class BaseStrategy {

    protected final Config config;
    protected final TCSService tcsService;
    protected final UnifiedTraderConfig unifiedTraderConfig;
    protected MarketDataProvider marketDataProvider;
    protected OrderExecutor orderExecutor;
    protected TimeProvider timeProvider;

    /** Backtest broker for parity with live trading (injected via setBacktestBroker). */
    protected static com.github.shk0da.goldendragon.backtest.SimulatedBroker backtestBroker;
    protected final BadWeatherFilter badWeatherFilter;
    protected final MarketRegimeFilter marketRegimeFilter;

    protected static final ThreadLocal<SimpleDateFormat> CANDLE_TIME_FORMAT =
            ThreadLocal.withInitial(() -> new SimpleDateFormat("dd.MM.yyyy HH:mm:ss"));

    protected static final long COOLDOWN_DURATION_MS = 5 * 60 * 1000L;
    protected static final long API_CALL_DELAY_MS = 100;
    protected static final Object API_LOCK = new Object();

    protected static final LocalTime WORK_START_TIME = LocalTime.of(8, 30);
    protected static final LocalTime EOD_CLOSE_TIME = LocalTime.of(21, 0);

    protected static long lastApiCallTime = 0;

    protected final Map<String, Long> tickerCooldown = new ConcurrentHashMap<>();
    protected final Map<String, Position> positionStore = new ConcurrentHashMap<>();
    private final Map<String, ReentrantLock> tickerLocks = new ConcurrentHashMap<>();
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
     */
    public static void setBacktestBroker(com.github.shk0da.goldendragon.backtest.SimulatedBroker broker) {
        BaseStrategy.backtestBroker = broker;
    }

    /**
     * Check if currently running in backtest mode.
     * @return true if backtest broker is set (backtest is running)
     */
    protected static boolean isBacktestMode() {
        return backtestBroker != null;
    }

    protected BaseStrategy(
            UnifiedTraderConfig unifiedTraderConfig,
            TCSService tcsService,
            Config config) {
        this(unifiedTraderConfig, tcsService, config, null);
    }

    protected BaseStrategy(
            UnifiedTraderConfig unifiedTraderConfig,
            TCSService tcsService,
            Config config,
            TimeProvider timeProvider) {
        this.config = config;
        this.tcsService = tcsService;
        this.unifiedTraderConfig = unifiedTraderConfig;
        this.timeProvider = timeProvider != null ? timeProvider : new LiveTimeProvider();

        this.marketDataProvider = new LiveMarketDataProvider(tcsService);
        this.orderExecutor = new LiveOrderExecutor(tcsService);

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

    public void run() {
        if (tcsService == null) {
            log(getStrategyName() + " stopped: tcsService is null.");
            return;
        }

        throttleApiCall();
        var initPortfolioCost = safeGetTotalPortfolioCost();
        var infoMessage =
                getStrategyName() + " started. Total Portfolio Cost: " + initPortfolioCost;
        log(infoMessage);

        List<String> activeTickers = new ArrayList<>();
        for (String ticker : unifiedTraderConfig.getStocks()) {
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

        // Daily reset for Money Management
        onDailyReset();

        if (isEndOfDayReached() || !isTradingDay()) {
            var message =
                    getStrategyName() + ": outside working hours, closing positions if needed.";
            log(message);
            closeAllPositions(tcsService, unifiedTraderConfig);
            return;
        }

        ExecutorService executor = Executors.newFixedThreadPool(activeTickers.size() + 1);
        Map<String, Double> capitalAllocation = computeCapitalAllocation(activeTickers);

        try {
            List<CompletableFuture<Void>> tasks = new ArrayList<>();

            tasks.add(
                    runAsync(
                            () -> {
                                while (isWorkingHours()) {
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
                                    while (isWorkingHours()) {
                                        processTicker(
                                                name,
                                                tcsService,
                                                unifiedTraderConfig,
                                                allocatedBalance);
                                        sleep(30_000);
                                    }
                                },
                                executor));
            }

            allOf(tasks.toArray(new CompletableFuture[0])).join();
        } finally {
            closeAllPositions(tcsService, unifiedTraderConfig);
            shutdownExecutor(executor);

            var endPortfolioCost = safeGetTotalPortfolioCost();
            var message = getStrategyName() + " stopped. Total Portfolio Cost: " + endPortfolioCost;
            log(message);
        }
    }

    protected abstract String getStrategyName();

    public abstract TradingDecision decide(
            String ticker,
            List<Candle> hourCandles,
            List<Candle> minuteCandles,
            Position position,
            double balance,
            boolean incrementCandlesHeld);

    protected void refreshPeerCandles(List<String> tickers) {
        if (tcsService == null) {
            return;
        }

        Map<String, List<Candle>> snapshot = new HashMap<>();
        String dataDir = unifiedTraderConfig.getDataDir();

        for (String ticker : tickers) {
            try {
                TickerInfo info = findTickerInfo(ticker);
                if (info == null) continue;

                List<Candle> hourCandles =
                        loadOrRefreshCandles(
                                ticker,
                                info.getFigi(),
                                dataDir,
                                timeProvider != null ? timeProvider.nowOffset() : OffsetDateTime.now(),
                                CandleInterval.CANDLE_INTERVAL_HOUR);
                if (hourCandles != null && !hourCandles.isEmpty()) {
                    snapshot.put(ticker, hourCandles);
                }
            } catch (Exception ex) {
                log("refreshPeerCandles failed for " + ticker + ": " + ex.getMessage());
            }
        }

        if (!snapshot.isEmpty()) {
            setPeerCandles(snapshot);
        }
    }

    public void processTicker(
            String name,
            TCSService tcsService,
            UnifiedTraderConfig unifiedTraderConfig,
            double allocatedBalance) {
        Long cooldownUntil = tickerCooldown.get(name);
        if (cooldownUntil != null) {
            long remaining = cooldownUntil - timeProvider.currentTimeMillis();
            if (remaining > 0) {
                log(
                        "Ticker "
                                + name
                                + " is on cooldown for "
                                + (remaining / 1000)
                                + "s, skipping.");
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

            // Get candles from market data provider (works in both backtest and live)
            List<Candle> hourCandles = marketDataProvider.getCandles(name, "HOUR");
            if (hourCandles == null || hourCandles.isEmpty()) {
                log("No hourly candles for " + name + ", skipping.");
                return;
            }

            boolean useMinCandles = tickerParams.useMinuteCandles;
            List<Candle> minuteCandles;
            if (useMinCandles) {
                minuteCandles = marketDataProvider.getCandles(name, "5_MIN");
                if (minuteCandles == null || minuteCandles.isEmpty()) {
                    log("No minute candles for " + name + ", skipping.");
                    return;
                }
            } else {
                minuteCandles = hourCandles;
            }

                        Position storedPosition = positionStore.getOrDefault(name, new Position());

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

            // Include TMON@ parking value into effective cash for position sizing,
            // so decide() can size a position using cash parked in TMON@
            // Read TMON@ position from broker (not local store) to handle parallel execution
            double effectiveBalance = balance;
            if (!"TMON@".equals(name) && (tcsService != null || marketDataProvider != null)) {
                try {
                    com.github.shk0da.goldendragon.model.PositionInfo tmonInfo =
                            tcsService != null
                                    ? tcsService.getCurrentPositions(TickerType.ETF, "TMON@")
                                    : marketDataProvider.getCurrentPositions(TickerType.ETF, "TMON@");
                    if (tmonInfo != null && tmonInfo.getBalance() > 0) {
                        double tmonQty = Math.abs(tmonInfo.getBalance());
                        Double tmonPrice = tmonInfo.getAveragePositionPrice();
                        if (tmonPrice != null && tmonPrice > 0) {
                            double tmonValue = tmonQty * tmonPrice;
                            effectiveBalance = balance + tmonValue;
                            log(
                                    "EFFECTIVE-BALANCE "
                                            + name
                                            + ": cash="
                                            + String.format("%.2f", balance)
                                            + " + tmon="
                                            + String.format("%.2f", tmonValue)
                                            + " = "
                                            + String.format("%.2f", effectiveBalance));
                        }
                    }
                } catch (Exception ignored) {
                    // no TMON@ position in broker
                }
            }

            TradingDecision decision =
                    decide(name, hourCandles, minuteCandles, storedPosition, effectiveBalance, hourChanged);

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

            if (decision.updatedPosition != null && "HOLD".equals(decision.action)) {
                positionStore.put(name, decision.updatedPosition);
                syncProtectiveOrdersIfNeeded(
                        name, ticker, storedPosition, decision.updatedPosition);
            }

            if ("OPEN".equals(decision.action)) {
                // Free cash from TMON@ parking only for the amount missing for the trade
                if (!"TMON@".equals(name)
                        && decision.quantity > 0
                        && decision.entryPrice != null) {
                    try {
                        // read TMON@ position from broker (not local store) to get actual qty
                        com.github.shk0da.goldendragon.model.PositionInfo tmonInfo =
                                tcsService != null
                                        ? tcsService.getCurrentPositions(TickerType.ETF, "TMON@")
                                        : marketDataProvider.getCurrentPositions(TickerType.ETF, "TMON@");
                        if (tmonInfo != null && tmonInfo.getBalance() > 0) {
                            int tmonQty = tmonInfo.getBalance();
                            Double tmonPriceDouble = tmonInfo.getAveragePositionPrice();
                            double tmonPrice =
                                    tmonPriceDouble != null && tmonPriceDouble > 0
                                            ? tmonPriceDouble
                                            : decision.entryPrice;
                            int lotSize = ticker.getLot() != null ? Math.max(1, ticker.getLot()) : 1;
                            double positionValue = decision.quantity * decision.entryPrice * lotSize;
                            double availableCash0 = orderExecutor.getAvailableCash();
                            double missing = positionValue - availableCash0;
                            if (missing > 0 && tmonQty > 0 && tmonPrice > 0) {
                                // sell only the required TMON@ value to free cash for the trade
                                int tmonLots = tmonInfo.getLots() > 0
                                        ? tmonInfo.getLots()
                                        : 1;
                                double tmonLotCost = tmonPrice * tmonLots;
                                // whole lots needed to cover the missing amount
                                int neededLots = (int) Math.ceil(missing / tmonLotCost);
                                int tmonLotsToSell = Math.min(neededLots, tmonQty);
                                if (tmonLotsToSell > 0) {
                                    double cashToFree = tmonLotsToSell * tmonLotCost;
                                    if (tcsService != null) {
                                        tcsService.sellByMarket(
                                                "TMON@", TickerType.ETF, cashToFree, 0.0, 0.0);
                                    } else {
                                        marketDataProvider.sellByMarket(
                                                "TMON@", TickerType.ETF, cashToFree);
                                    }
                                    log(
                                            "PARTIALFREE "
                                                    + name
                                                    + ": sold TMON@ value="
                                                    + String.format("%.2f", cashToFree)
                                                    + " ("
                                                    + tmonLotsToSell
                                                    + " lots) to cover missing "
                                                    + String.format("%.2f", missing)
                                                    + ", positionValue="
                                                    + String.format("%.2f", positionValue)
                                                    + ", availableCash="
                                                    + String.format("%.2f", availableCash0)
                                                    + ", TMON@ remaining (est.)="
                                                    + (tmonQty - tmonLotsToSell));
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
        } catch (Exception ex) {
            long cooldownExpiry = timeProvider.currentTimeMillis() + COOLDOWN_DURATION_MS;
            tickerCooldown.put(name, cooldownExpiry);
            String message = getStrategyName() + " error for " + name + ": " + ex.getMessage();
            log(message);
        } finally {
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

        // Pre-trade: sell TMON@ parking to free cash for new positions
        if (positionStore != null) {
            Position tmonPos = positionStore.get("TMON@");
            if (tmonPos != null
                    && tmonPos.quantity > 0
                    && !"TMON@".equals(name)
                    && "BUY".equals(decision.updatedPosition.direction)) {
                try {
                    TickerInfo tmonInfo = findTickerInfo("TMON@");
                    if (tmonInfo != null) {
                        log(
                                "CASH_FREETRIGGER "
                                        + name
                                        + ": TMON@ parked="
                                        + tmonPos.quantity
                                        + " selling to free cash for "
                                        + name);
                        if (tcsService != null) {
                            tcsService.closeLongByMarket("TMON@", TickerType.ETF);
                        } else {
                            marketDataProvider.closeLongByMarket("TMON@", TickerType.ETF);
                        }
                        positionStore.remove("TMON@");
                        log("TMON@ sold, positionStore cleared for new position");
                    }
                } catch (Exception ex) {
                    log(
                            "CASH_FREEFAIL: Failed to sell TMON@ for "
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
        if (qty <= 0) {
            double availableCash = orderExecutor.getAvailableCash();
            qty = (int) Math.floor(availableCash / (liveAskPrice * lotSize));
            if (qty <= 0) {
                logOpenCandidateSkipped(name, "insufficient_cash", decision);
                return;
            }
        }
        double positionValue = qty * liveAskPrice * lotSize;

        boolean isTmonCashParking = "TMON@".equals(name);
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

        if ("TMON@".equals(name)) {
            logThrottled(name + "_opening", openingLogMessage, 5);
        } else {
            log(openingLogMessage);
        }

        try {
            // Execute order through order executor (works in both backtest and live)
            OrderExecutor.ExecutionResult orderResult;
            if ("BUY".equals(decision.updatedPosition.direction)) {
                orderResult = orderExecutor.buy(name, qty, slPercent, tpPercent);
            } else { // SELL
                orderResult = orderExecutor.sell(name, qty, slPercent, tpPercent);
            }

            if (!orderResult.isSuccess()) {
                logOpenCandidateSkipped(name, "order_execution_failed", decision);
                String failedLogMessage =
                        "Failed to open " + decision.updatedPosition.direction + " for " + name + ".";
                if ("TMON@".equals(name)) {
                    logThrottled(name + "_failed_open", failedLogMessage, 5);
                } else {
                    log(failedLogMessage);
                }
                return;
            }

            // Create position from execution result
            Position executedPosition = new Position(
                    decision.updatedPosition.direction,
                    orderResult.getExecutedPrice(),
                    decision.stopLoss,
                    decision.takeProfit,
                    orderResult.getExecutedQuantity(),
                    0,
                    0,
                    decision.updatedPosition.appliedLeverage);

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
        if ("TMON@".equals(name)) {
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
        if (tcsService == null || ticker == null || updatedPosition == null || updatedPosition.quantity <= 0) {
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
            tcsService.syncProtectiveOrders(name, ticker.getType(), updatedPosition);
        } catch (Exception ex) {
            log("Failed to sync protective orders for " + name + ": " + ex.getMessage());
        }
    }

    /**
     * Callback for trade closure (for Money Management integration). Override in subclasses to
     * register trade results.
     */
    protected void onTradeClosed(
            String ticker,
            double pnl,
            double entryPrice,
            double exitPrice,
            int quantity,
            String direction) {
        // Default: no-op. Override in UnifiedStrategy for MM integration.
    }

    /**
     * Callback for daily reset (for Money Management integration). Override in subclasses to reset
     * daily limits.
     */
    protected void onDailyReset() {
        // Default: no-op. Override in UnifiedStrategy for MM integration.
    }

    protected Position getCooldownPosition() {
        return new Position(config.cooldownCandles);
    }

    /**
     * Checks if there are any active positions on tickers other than TMON@. Used by TMON@ cash
     * parking logic to determine whether TMON@ should be sold (to free cash for other positions) or
     * bought (when idle).
     */
    protected boolean hasActiveNonTmonPositions() {
        for (Map.Entry<String, Position> entry : positionStore.entrySet()) {
            if ("TMON@".equals(entry.getKey())) {
                continue;
            }
            if (entry.getValue().quantity > 0) {
                return true;
            }
        }
        return false;
    }

    protected Double safeGetTotalPortfolioCost() {
        if (tcsService == null) {
            return 0.0;
        }
        try {
            return tcsService.getTotalPortfolioCost();
        } catch (Exception ex) {
            log("Failed to read portfolio cost: " + ex.getMessage());
            return 0.0;
        }
    }

    protected TickerInfo findTickerInfo(String name) {
        return TickerRepository.INSTANCE.getAll().values().stream()
                .filter(
                        t ->
                                t.getType() == TickerType.STOCK
                                        || t.getType() == TickerType.FEATURE
                                        || t.getType() == TickerType.ETF)
                .filter(
                        t ->
                                t.getName().equalsIgnoreCase(name)
                                        || t.getTicker().equalsIgnoreCase(name))
                .findFirst()
                .orElse(null);
    }

    private void restoreTrackedPositions(List<String> activeTickers) {
        if (tcsService == null) {
            return;
        }

        Set<String> activeTickerSet = new HashSet<>(activeTickers);
        restoreTrackedPositions(activeTickerSet, STOCK);
        restoreTrackedPositions(activeTickerSet, FEATURE);
        logRestoredPositionsReport();
    }

    private void restoreTrackedPositions(Set<String> activeTickers, TickerType tickerType) {
        Map<TickerInfo.Key, com.github.shk0da.goldendragon.model.PositionInfo> currentPositions =
                tcsService.getCurrentPositions(tickerType);
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
                                    tcsService.restoreProtectivePosition(
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
                                tcsService.syncProtectiveOrders(
                                        positionInfo.getTicker(), tickerType, restoredPosition);
                            }
                        });
    }

    private void initializeLastSeenHourBar(String ticker, TickerInfo tickerInfo) {
        try {
            List<Candle> hourCandles =
                    loadOrRefreshCandles(
                            ticker,
                            tickerInfo.getFigi(),
                            unifiedTraderConfig.getDataDir(),
                            timeProvider != null ? timeProvider.nowOffset() : OffsetDateTime.now(),
                            CandleInterval.CANDLE_INTERVAL_HOUR);
            if (hourCandles != null && !hourCandles.isEmpty()) {
                lastSeenHourBarByTicker.put(ticker, hourCandles.get(hourCandles.size() - 1).time);
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

    protected List<Candle> loadOrRefreshCandles(
            String name, String figi, String dataDir, OffsetDateTime now, CandleInterval interval) {
        // Use timeProvider if 'now' is null (for backtest compatibility)
        if (now == null && timeProvider != null) {
            now = timeProvider.nowOffset();
        }
        if (tcsService == null) {
            return readCachedCandles(name, dataDir, interval);
        }

        String fileName =
                interval == CandleInterval.CANDLE_INTERVAL_HOUR
                        ? "candlesHOUR.txt"
                        : "candles5_MIN.txt";
        File candleFile = new File(dataDir + "/" + name + "/" + fileName);

        List<Candle> candles = readCachedCandles(name, dataDir, interval);
        if (candles != null && !candles.isEmpty() && isCandleDataFresh(candles, interval)) {
            return candles;
        }

        throttleApiCall();

        List<HistoricCandle> historicCandles =
                tcsService.getCandles(
                        figi,
                        interval == CandleInterval.CANDLE_INTERVAL_HOUR
                                ? now.minusMinutes(7 * 24 * 60)
                                : now.minusMinutes(6 * 60),
                        now,
                        interval);

        List<Candle> refreshed = new ArrayList<>();
        for (HistoricCandle hc : historicCandles) {
            Timestamp ts = new Timestamp(hc.getTime().getSeconds() * 1000);
            refreshed.add(
                    new Candle(
                            CANDLE_TIME_FORMAT.get().format(ts),
                            IndicatorsUtil.toDouble(hc.getOpen()),
                            IndicatorsUtil.toDouble(hc.getHigh()),
                            IndicatorsUtil.toDouble(hc.getLow()),
                            IndicatorsUtil.toDouble(hc.getClose()),
                            hc.getVolume()));
        }

        if (!refreshed.isEmpty()) {
            writeCandlesToFile(name, dataDir, fileName, refreshed);
            return refreshed;
        }

        if (candleFile.exists() && candles != null && !candles.isEmpty()) {
            return candles;
        }

        return refreshed;
    }

    protected List<Candle> readCachedCandles(String name, String dataDir, CandleInterval interval) {
        try {
            List<TickerCandle> cached = DataCollector.readCandlesFile(name, dataDir, interval);
            if (cached == null || cached.isEmpty()) {
                return null;
            }

            List<Candle> candles = new ArrayList<>(cached.size());
            for (TickerCandle tc : cached) {
                candles.add(
                        new Candle(
                                tc.getDate(),
                                tc.getOpen(),
                                tc.getHigh(),
                                tc.getLow(),
                                tc.getClose(),
                                tc.getVolume()));
            }
            return candles;
        } catch (Exception ex) {
            log("Failed to read cached candles for " + name + ": " + ex.getMessage());
            return null;
        }
    }

    protected boolean isCandleDataFresh(List<Candle> candles, CandleInterval interval) {
        if (candles == null || candles.isEmpty()) {
            return false;
        }

        try {
            String lastTimeStr = candles.get(candles.size() - 1).time;
            Date lastCandleDate = CANDLE_TIME_FORMAT.get().parse(lastTimeStr);

            Calendar lastCal = Calendar.getInstance();
            lastCal.setTime(lastCandleDate);

            Calendar nowCal = Calendar.getInstance();

            boolean sameDay =
                    lastCal.get(Calendar.YEAR) == nowCal.get(Calendar.YEAR)
                            && lastCal.get(Calendar.DAY_OF_YEAR)
                                    == nowCal.get(Calendar.DAY_OF_YEAR);

            if (!sameDay) {
                return false;
            }

            if (interval == CandleInterval.CANDLE_INTERVAL_HOUR) {
                return lastCal.get(Calendar.HOUR_OF_DAY) == nowCal.get(Calendar.HOUR_OF_DAY);
            }

            return lastCal.get(Calendar.HOUR_OF_DAY) == nowCal.get(Calendar.HOUR_OF_DAY)
                    && (lastCal.get(Calendar.MINUTE) / 5) == (nowCal.get(Calendar.MINUTE) / 5);
        } catch (Exception ex) {
            return false;
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
            TCSService tcsService, UnifiedTraderConfig unifiedTraderConfig) {
        if (tcsService == null) {
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
                    closed = tcsService.closeLongByMarket(tickerName, ticker.getType());
                } else if ("SELL".equals(position.direction)) {
                    closed = tcsService.closeShortByMarket(tickerName, ticker.getType());
                }

                if (closed) {
                    positionStore.put(tickerName, getCooldownPosition());
                    lastSeenHourBarByTicker.remove(tickerName);
                    double exitPrice =
                            tcsService.getAvailablePrice(
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
            tcsService.closeAllByMarket(STOCK);
        } catch (Exception ex) {
            log("Failed to close all STOCK positions: " + ex.getMessage());
        }

        try {
            tcsService.closeAllByMarket(FEATURE);
        } catch (Exception ex) {
            log("Failed to close all FEATURE positions: " + ex.getMessage());
        }

        if (anyClosed) {
            log("End-of-day position closing completed.");
        }
    }

    protected void writeCandlesToFile(
            String name, String dataDir, String fileName, List<Candle> candles) {
        try {
            Path dir = Paths.get(dataDir, name);
            Files.createDirectories(dir);
            Path filePath = dir.resolve(fileName);

            boolean fileExists = Files.exists(filePath);
            Set<String> existingTimestamps = new HashSet<>();

            if (fileExists) {
                try (BufferedReader reader = Files.newBufferedReader(filePath)) {
                    String line = reader.readLine();
                    while ((line = reader.readLine()) != null) {
                        String[] parts = line.split(",");
                        if (parts.length > 0) {
                            existingTimestamps.add(parts[0]);
                        }
                    }
                }
            }

            boolean writeHeader = !fileExists || Files.size(filePath) == 0;

            try (FileWriter writer = new FileWriter(filePath.toFile(), true)) {
                if (writeHeader) {
                    writer.write("Datetime,Open,High,Low,Close,Volume" + System.lineSeparator());
                }

                for (Candle c : candles) {
                    if (existingTimestamps.add(c.time)) {
                        writer.write(
                                String.format(
                                                "%s,%s,%s,%s,%s,%s",
                                                c.time, c.open, c.high, c.low, c.close, c.volume)
                                        + System.lineSeparator());
                    }
                }
            }
        } catch (IOException ex) {
            log("Failed to write candles file for " + name + ": " + ex.getMessage());
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
     * @param key unique identifier for the log category (e.g., "TMON@_empty_orderbook")
     * @param message message to log
     * @param throttleMinutes minutes to wait between logs for the same key
     */
    protected void logThrottled(String key, String message, long throttleMinutes) {
        long now = timeProvider.currentTimeMillis();
        long throttleMs = throttleMinutes * 60 * 1000L;
        Long lastTime = throttledLogLastTime.get(key);
        if (lastTime == null || (now - lastTime) >= throttleMs) {
            throttledLogLastTime.put(key, now);
            log(message);
        }
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
            totalCash = tcsService.getAvailableCash();
        } catch (Exception ex) {
            log("Failed to read available cash for allocation: " + ex.getMessage());
            return new HashMap<>();
        }

        boolean tmonCashParking =
                weights.containsKey("TMON@") && unifiedTraderConfig.isTmonCashParkingEnabled();

        Map<String, Double> allocation = new HashMap<>();
        for (Map.Entry<String, Double> e : weights.entrySet()) {
            if (tmonCashParking && "TMON@".equals(e.getKey())) {
                // TMON@ cash parking always uses real-time available cash via
                // getAvailableCash() in processTicker(), not a stale startup snapshot.
                // Skipping allocation so allocatedBalance = 0.0 and the fallback kicks in.
            } else {
                // Allocate capital to non-TMON@ tickers (or all tickers if tmonCashParking=false)
                allocation.put(e.getKey(), totalCash * (e.getValue() / totalWeight));
            }
        }

        return allocation;
    }
}
