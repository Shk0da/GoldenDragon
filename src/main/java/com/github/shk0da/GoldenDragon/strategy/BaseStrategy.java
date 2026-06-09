package com.github.shk0da.GoldenDragon.strategy;

import com.github.shk0da.GoldenDragon.config.UnifiedTraderConfig;
import com.github.shk0da.GoldenDragon.filters.BadWeatherFilter;
import com.github.shk0da.GoldenDragon.filters.MarketRegimeFilter;
import com.github.shk0da.GoldenDragon.ml.TradeDataCollector;
import com.github.shk0da.GoldenDragon.model.Candle;
import com.github.shk0da.GoldenDragon.model.Config;
import com.github.shk0da.GoldenDragon.model.Position;
import com.github.shk0da.GoldenDragon.model.TickerCandle;
import com.github.shk0da.GoldenDragon.model.TickerInfo;
import com.github.shk0da.GoldenDragon.model.TickerType;
import com.github.shk0da.GoldenDragon.model.TradingDecision;
import com.github.shk0da.GoldenDragon.repository.TickerRepository;
import com.github.shk0da.GoldenDragon.service.TCSService;
import com.github.shk0da.GoldenDragon.utils.IndicatorsUtil;
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
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.OffsetDateTime;
import java.time.ZoneId;
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

import static com.github.shk0da.GoldenDragon.model.TickerType.FEATURE;
import static com.github.shk0da.GoldenDragon.model.TickerType.STOCK;
import static com.github.shk0da.GoldenDragon.service.TelegramNotifyService.telegramNotifyService;
import static com.github.shk0da.GoldenDragon.utils.TimeUtils.sleep;
import static java.lang.Math.abs;
import static java.lang.Math.max;
import static java.lang.System.out;
import static java.util.concurrent.CompletableFuture.allOf;
import static java.util.concurrent.CompletableFuture.runAsync;

/**
 * Базовый абстрактный класс торговой стратегии, реализующий общий жизненный цикл
 * исполнения, управление позициями, загрузку рыночных данных, фильтрацию входов
 * и расчёт технических индикаторов. Конкретные стратегии (например,
 * {@code UnifiedStrategy}) наследуются от него и реализуют сигнальную логику в
 * методе {@link #decide}.
 *
 * <h2>Архитектура</h2>
 * Класс выступает «движком» (engine) стратегии:
 * <ul>
 *   <li>Управляет потоком исполнения (рабочие часы, торговые дни, EOD).</li>
 *   <li>Загружает и кеширует исторические свечи (часовые и 5-минутные).</li>
 *   <li>Координирует параллельную обработку нескольких тикеров.</li>
 *   <li>Делегирует принятие торгового решения наследнику через {@link #decide}.</li>
 *   <li>Исполняет ордера через {@link TCSService} (брокерский API).</li>
 *   <li>Ведёт учёт позиций, cooldown'ов и интегрируется с Money Management.</li>
 * </ul>
 *
 * <h2>Жизненный цикл {@link #run()}</h2>
 * <ol>
 *   <li>Получение начальной стоимости портфеля и отправка уведомления в Telegram.</li>
 *   <li>Сбор списка активных тикеров из {@link UnifiedTraderConfig}.</li>
 *   <li>Вызов {@link #onDailyReset()} — сброс дневных лимитов MM.</li>
 *   <li>Если торговый день закончен или сегодня выходной — закрытие всех позиций
 *       и выход.</li>
 *   <li>Расчёт распределения капитала по тикерам ({@link #computeCapitalAllocation}).</li>
 *   <li>Запуск пула потоков ({@code activeTickers.size() + 1}):
 *     <ul>
 *       <li>Один поток — фоновое обновление peer-свечей раз в 60 секунд
 *           ({@link #refreshPeerCandles}) для групповых подтверждений.</li>
 *       <li>По одному потоку на тикер — цикл вызова {@link #processTicker}
 *           каждые 30 секунд, пока активны рабочие часы.</li>
 *     </ul>
 *   </li>
 *   <li>По завершении рабочих часов: закрытие всех позиций
 *       ({@link #closeAllPositions}), остановка executor'а, финальный отчёт.</li>
 * </ol>
 *
 * <h2>Обработка тикера ({@link #processTicker})</h2>
 * Для каждого тикера на каждом цикле:
 * <ol>
 *   <li>Проверка персонального cooldown'а — пропуск, если ещё действует
 *       ({@link #COOLDOWN_DURATION_MS} = 5 минут после ошибки).</li>
 *   <li>Проверка рабочих часов и флага {@code tickerParams.enabled}.</li>
 *   <li>Поиск {@link TickerInfo} в {@link TickerRepository}.</li>
 *   <li>Загрузка свечей через {@link #loadOrRefreshCandles}: сначала проверяется
 *       свежесть кэша ({@link #isCandleDataFresh}), при необходимости — запрос к API.</li>
 *   <li>Часовые свечи обязательны; 5-минутные — только если
 *       {@code tickerParams.useMinuteCandles}.</li>
 *   <li>Определение смены часового бара ({@code hourChanged}) для корректного
 *       инкремента {@code candlesHeld} в открытой позиции.</li>
 *   <li>Расчёт баланса: выделенный капитал или текущая ликвидность.</li>
 *   <li>Вызов абстрактного {@link #decide} — получение {@link TradingDecision}.</li>
 *   <li>Маршрутизация по действию:
 *     <ul>
 *       <li>{@code HOLD} — обновление позиции в {@link #positionStore}.</li>
 *       <li>{@code OPEN} — открытие позиции через {@link #openPosition}.</li>
 *       <li>{@code CLOSE} — закрытие через {@link #closePosition}.</li>
 *     </ul>
 *   </li>
 *   <li>При любой ошибке тикер ставится на cooldown 5 минут, отправляется
 *       уведомление в Telegram.</li>
 * </ol>
 *
 * <h2>Открытие позиции ({@link #openPosition})</h2>
 * <ul>
 *   <li>Валидация направления ({@code BUY}/{@code SELL}) и количества.</li>
 *   <li>Проверка лимита одновременных позиций
 *       ({@link #MAX_CONCURRENT_POSITIONS} = 8).</li>
 *   <li>Расчёт SL/TP в процентах от цены входа (с дефолтами 2%/4% если не заданы).</li>
 *   <li>Вызов {@code tcsService.buyByMarket} / {@code sellByMarket} рыночным
 *       ордером с автоматической установкой SL/TP.</li>
 *   <li>Сохранение позиции, фиксация бара входа, запись в
 *       {@link TradeDataCollector} (для ML-pipeline), Telegram-уведомление.</li>
 * </ul>
 *
 * <h2>Закрытие позиции ({@link #closePosition})</h2>
 * <ul>
 *   <li>Закрытие длинной/короткой позиции через
 *       {@code closeLongByMarket} / {@code closeShortByMarket}.</li>
 *   <li>Установка cooldown-позиции ({@code config.cooldownCandles}).</li>
 *   <li>Расчёт PnL ({@link #calculatePnl}) и запись результата в ML-pipeline.</li>
 *   <li>Вызов {@link #onTradeClosed} — хук для MM-интеграции в наследнике.</li>
 *   <li>Telegram-уведомление с причиной закрытия и PnL.</li>
 * </ul>
 *
 * <h2>Управление данными (свечи)</h2>
 * <ul>
 *   <li>{@link #loadOrRefreshCandles} — двухуровневая загрузка:
 *       сначала кэш на диске, затем API при устаревании.</li>
 *   <li>{@link #isCandleDataFresh} — проверка по дню и часу/5-минутному слоту.</li>
 *   <li>{@link #writeCandlesToFile} — инкрементальная запись CSV с дедупликацией
 *       по timestamp.</li>
 *   <li>{@link #throttleApiCall} — глобальный rate-limiter (100мс между вызовами,
 *       синхронизация через {@link #API_LOCK}).</li>
 *   <li>{@link #refreshPeerCandles} — параллельное обновление часовых свечей
 *       всех тикеров для использования в групповых фильтрах подтверждения.</li>
 * </ul>
 *
 * <h2>Расчёт распределения капитала ({@link #computeCapitalAllocation})</h2>
 * Пропорциональное распределение свободного кэша между активными тикерами
 * на основе весов {@code allocationWeight} (по умолчанию 1.0):
 * {@code allocation[i] = totalCash × (weight[i] / Σweights)}.
 *
 * <h2>Фильтры входа ({@link #applyFilters})</h2>
 * Утилитарный метод, последовательно применяющий:
 * <ol>
 *   <li>{@link BadWeatherFilter} — фильтрация неблагоприятных рыночных условий
 *       (низкий объём, ATR-спайки, широкие спреды, длинные тени, паника).</li>
 *   <li>{@link MarketRegimeFilter} — оценка пригодности режима рынка по ADX,
 *       объёму и confidence.</li>
 * </ol>
 * Возвращает {@code HOLD} с причиной блокировки или {@code null} при прохождении.
 *
 * <h2>Временные ограничения</h2>
 * <ul>
 *   <li>Торговая сессия: {@link #WORK_START_TIME} (08:30) —
 *       {@link #EOD_CLOSE_TIME} (21:00).</li>
 *   <li>Торговые дни: пн–пт ({@link #isTradingDay}).</li>
 *   <li>{@link #isEndOfDayReached} — триггер для принудительного закрытия позиций.</li>
 * </ul>
 *
 * <h2>Технические индикаторы</h2>
 * Базовая реализация (используется наследниками):
 * <ul>
 *   <li>{@link #ema} — экспоненциальное скользящее среднее с SMA-инициализацией.</li>
 *   <li>{@link #atrVal} — Average True Range (простое среднее).</li>
 *   <li>{@link #emaAtr} — сглаженный ATR через скользящее окно.</li>
 *   <li>{@link #rsiVal} — Relative Strength Index (период по умолчанию).</li>
 *   <li>{@link #adxVal} — Average Directional Index с Wilder-сглаживанием
 *       (+DI, -DI, DX → ADX).</li>
 * </ul>
 *
 * <h2>Хуки для наследников</h2>
 * <ul>
 *   <li>{@link #getStrategyName()} — имя стратегии для логов и уведомлений.</li>
 *   <li>{@link #decide} — основная сигнальная логика (обязательна).</li>
 *   <li>{@link #onTradeClosed} — callback после закрытия сделки (для MM).</li>
 *   <li>{@link #onDailyReset} — callback в начале торгового дня (для сброса
 *       дневных лимитов MM).</li>
 * </ul>
 *
 * <h2>Режим бэктеста</h2>
 * Флаг {@code isBacktest} переключает поведение:
 * <ul>
 *   <li>{@link #logWithBacktest} — silent-логирование в backtest-режиме.</li>
 *   <li>{@link #recordBacktestTradeEntry} / {@link #recordBacktestTradeOutcome} —
 *       специальные методы записи сделок с парсингом времени из свечей.</li>
 * </ul>
 *
 * <h2>Параллелизм и потокобезопасность</h2>
 * <ul>
 *   <li>{@link #positionStore}, {@link #tickerCooldown},
 *       {@link #lastSeenHourBarByTicker}, {@link #peerCandles} —
 *       {@link ConcurrentHashMap} для безопасного доступа из потоков тикеров.</li>
 *   <li>API-вызовы сериализуются через {@link #API_LOCK}.</li>
 *   <li>{@link SimpleDateFormat} обёрнут в {@link ThreadLocal}.</li>
 * </ul>
 *
 * <h2>Интеграции</h2>
 * <ul>
 *   <li>{@link TCSService} — брокерский API (Tinkoff Invest).</li>
 *   <li>{@code TelegramNotifyService} — уведомления о запуске, сделках, ошибках.</li>
 *   <li>{@link TradeDataCollector} — запись сделок в CSV
 *       ({@code ml_strategy/data_pipeline/trades.csv}) для обучения ML-моделей.</li>
 *   <li>{@link TickerRepository} — справочник инструментов.</li>
 * </ul>
 */
public abstract class BaseStrategy {

    protected static final TradeDataCollector TRADE_DATA_COLLECTOR = new TradeDataCollector("ml_strategy/data_pipeline/trades.csv");

    protected final Config config;
    protected final TCSService tcsService;
    protected final UnifiedTraderConfig unifiedTraderConfig;
    protected final BadWeatherFilter badWeatherFilter;
    protected final MarketRegimeFilter marketRegimeFilter;
    protected final boolean isBacktest;

    protected static final ThreadLocal<SimpleDateFormat> LOG_TIME_FORMAT = ThreadLocal.withInitial(
            () -> new SimpleDateFormat("dd.MM.yyyy HH:mm:ss.SSS")
    );

    protected static final ThreadLocal<SimpleDateFormat> CANDLE_TIME_FORMAT = ThreadLocal.withInitial(
            () -> new SimpleDateFormat("dd.MM.yyyy HH:mm:ss")
    );

    protected static final long COOLDOWN_DURATION_MS = 5 * 60 * 1000L;
    protected static final long API_CALL_DELAY_MS = 100;
    protected static final Object API_LOCK = new Object();

    protected static final LocalTime WORK_START_TIME = LocalTime.of(8, 30);
    protected static final LocalTime EOD_CLOSE_TIME = LocalTime.of(21, 0);

    protected static long lastApiCallTime = 0;

    protected final Map<String, Long> tickerCooldown = new ConcurrentHashMap<>();
    protected final Map<String, Position> positionStore = new ConcurrentHashMap<>();
    protected final Map<String, String> lastSeenHourBarByTicker = new ConcurrentHashMap<>();
    protected volatile Map<String, List<Candle>> peerCandles = new ConcurrentHashMap<>();

    protected static final int MAX_CONCURRENT_POSITIONS = 8; // Максимум 8 одновременных позиций

    protected BaseStrategy(UnifiedTraderConfig unifiedTraderConfig, TCSService tcsService, Config config, boolean isBacktest) {
        this.config = config;
        this.tcsService = tcsService;
        this.unifiedTraderConfig = unifiedTraderConfig;
        this.isBacktest = isBacktest;
        this.badWeatherFilter = new BadWeatherFilter(
                config.badWeatherFilterEnabled,
                config.badWeatherLowVolumeThreshold,
                config.badWeatherLowAtrThreshold,
                config.badWeatherMinRangePercent,
                config.badWeatherHighAtrThreshold,
                config.badWeatherMaxSpreadPercent,
                config.badWeatherMaxWickRatio,
                config.badWeatherPanicVolumeThreshold,
                config.badWeatherMinAvgDailyVolume,
                config.badWeatherAtrSpikeThreshold
        );
        this.marketRegimeFilter = new MarketRegimeFilter(config.marketRegimeFilterEnabled);
    }

    public void setPeerCandles(Map<String, List<Candle>> peerCandles) {
        this.peerCandles = peerCandles != null ? new ConcurrentHashMap<>(peerCandles) : new ConcurrentHashMap<>();
    }

    public void run() {
        if (tcsService == null) {
            log(getStrategyName() + " stopped: tcsService is null.");
            return;
        }

        throttleApiCall();
        var initPortfolioCost = safeGetTotalPortfolioCost();
        var infoMessage = getStrategyName() + " started. Total Portfolio Cost: " + initPortfolioCost;
        telegramNotifyService.sendMessage(infoMessage);
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

        if (activeTickers.isEmpty()) {
            log("No active tickers found. " + getStrategyName() + " stopped.");
            return;
        }

        restoreTrackedPositions(activeTickers);

        // Daily reset for Money Management
        onDailyReset();

        if (isEndOfDayReached() || !isTradingDay()) {
            var message = getStrategyName() + ": outside working hours, closing positions if needed.";
            log(message);
            telegramNotifyService.sendMessage(message);
            closeAllPositions(tcsService, unifiedTraderConfig);
            return;
        }

        ExecutorService executor = Executors.newFixedThreadPool(activeTickers.size() + 1);
        Map<String, Double> capitalAllocation = computeCapitalAllocation(activeTickers);

        try {
            List<CompletableFuture<Void>> tasks = new ArrayList<>();

            tasks.add(runAsync(() -> {
                while (isWorkingHours()) {
                    try {
                        refreshPeerCandles(activeTickers);
                    } catch (Exception ex) {
                        log("Failed to refresh peer candles: " + ex.getMessage());
                    }
                    sleep(60_000);
                }
            }, executor));

            for (String name : activeTickers) {
                double allocatedBalance = capitalAllocation.getOrDefault(name, 0.0);
                tasks.add(runAsync(() -> {
                    while (isWorkingHours()) {
                        processTicker(name, tcsService, unifiedTraderConfig, allocatedBalance);
                        sleep(30_000);
                    }
                }, executor));
            }

            allOf(tasks.toArray(new CompletableFuture[0])).join();
        } finally {
            closeAllPositions(tcsService, unifiedTraderConfig);
            shutdownExecutor(executor);

            var endPortfolioCost = safeGetTotalPortfolioCost();
            var message = getStrategyName() + " stopped. Total Portfolio Cost: " + endPortfolioCost;
            telegramNotifyService.sendMessage(message);
            log(message);
        }
    }

    protected abstract String getStrategyName();

    public abstract TradingDecision decide(String ticker,
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

                List<Candle> hourCandles = loadOrRefreshCandles(
                        ticker, info.getFigi(), dataDir, OffsetDateTime.now(), CandleInterval.CANDLE_INTERVAL_HOUR
                );
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

    public void processTicker(String name,
                              TCSService tcsService,
                              UnifiedTraderConfig unifiedTraderConfig,
                              double allocatedBalance) {
        if (tcsService == null) {
            return;
        }

        Long cooldownUntil = tickerCooldown.get(name);
        if (cooldownUntil != null) {
            long remaining = cooldownUntil - System.currentTimeMillis();
            if (remaining > 0) {
                log("Ticker " + name + " is on cooldown for " + (remaining / 1000) + "s, skipping.");
                return;
            } else {
                tickerCooldown.remove(name);
                log("Ticker " + name + " cooldown expired, resuming.");
            }
        }

        try {
            if (!isWorkingHours()) {
                return;
            }

            UnifiedTraderConfig.TickerParams tickerParams = unifiedTraderConfig.getTickerParams(name);
            if (!tickerParams.enabled) {
                log("Ticker " + name + " disabled, skipping.");
                return;
            }

            TickerInfo ticker = findTickerInfo(name);
            if (ticker == null) {
                log("Ticker " + name + " not found, skipping.");
                return;
            }

            String figi = ticker.getFigi();
            OffsetDateTime now = OffsetDateTime.now();
            String dataDir = unifiedTraderConfig.getDataDir();

            List<Candle> hourCandles = loadOrRefreshCandles(name, figi, dataDir, now, CandleInterval.CANDLE_INTERVAL_HOUR);
            if (hourCandles == null || hourCandles.isEmpty()) {
                log("No hourly candles for " + name + ", skipping.");
                return;
            }

            boolean useMinCandles = tickerParams.useMinuteCandles;
            List<Candle> minuteCandles = hourCandles;

            if (useMinCandles) {
                minuteCandles = loadOrRefreshCandles(name, figi, dataDir, now, CandleInterval.CANDLE_INTERVAL_5_MIN);
                if (minuteCandles == null || minuteCandles.isEmpty()) {
                    log("No minute candles for " + name + ", skipping.");
                    return;
                }
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

            double balance = allocatedBalance > 0.0 ? allocatedBalance : tcsService.getAvailableCash();

            TradingDecision decision = decide(name, hourCandles, minuteCandles, storedPosition, balance, hourChanged);

            if (decision.updatedPosition != null && "HOLD".equals(decision.action)) {
                positionStore.put(name, decision.updatedPosition);
                syncProtectiveOrdersIfNeeded(name, ticker, storedPosition, decision.updatedPosition);
            }

            if ("OPEN".equals(decision.action)) {
                openPosition(name, ticker, hourCandles, decision);
            }

            if ("CLOSE".equals(decision.action)) {
                closePosition(name, ticker, storedPosition, decision);
            }
        } catch (Exception ex) {
            long cooldownExpiry = System.currentTimeMillis() + COOLDOWN_DURATION_MS;
            tickerCooldown.put(name, cooldownExpiry);
            String message = getStrategyName() + " error for " + name + ": " + ex.getMessage();
            log(message);
            telegramNotifyService.sendMessage(message);
        }
    }

    protected void openPosition(String name, TickerInfo ticker, List<Candle> candles, TradingDecision decision) {
        log("Decision for " + name + ": " + decision.action + " (" + decision.reason + ")");
        if (decision.updatedPosition == null || decision.quantity <= 0) {
            logOpenCandidateSkipped(name, "invalid_open_decision", decision);
            log("Invalid OPEN decision for " + name + ", skipping.");
            return;
        }

        if (!"BUY".equals(decision.updatedPosition.direction) && !"SELL".equals(decision.updatedPosition.direction)) {
            logOpenCandidateSkipped(name, "invalid_direction", decision);
            log("Invalid direction for " + name + ", skipping.");
            return;
        }

        // Проверяем максимальное количество одновременных позиций
        long currentPositionCount = positionStore.values().stream()
                .filter(pos -> pos.quantity > 0)
                .count();
                
        if (currentPositionCount >= MAX_CONCURRENT_POSITIONS) {
            logOpenCandidateSkipped(name, "max_concurrent_positions_reached", decision);
            log("Maximum concurrent positions reached (" + MAX_CONCURRENT_POSITIONS + "), skipping " + name);
            return;
        }

        double entryPrice = decision.entryPrice != null
                ? decision.entryPrice
                : candles.get(candles.size() - 1).close;

        int qty = decision.quantity;
        int lotSize = ticker.getLot() != null ? Math.max(1, ticker.getLot()) : 1;
        double positionValue = qty * entryPrice * lotSize;

        double slPrice = decision.stopLoss != null ? decision.stopLoss : 
            ("BUY".equals(decision.updatedPosition.direction) ? entryPrice * 0.98 : entryPrice * 1.02);
        double tpPrice = decision.takeProfit != null ? decision.takeProfit : 
            ("BUY".equals(decision.updatedPosition.direction) ? entryPrice * 1.04 : entryPrice * 0.96);

        double slPercent = abs(entryPrice - slPrice) / entryPrice * 100;
        double tpPercent = abs(tpPrice - entryPrice) / entryPrice * 100;

        log("Opening " + decision.updatedPosition.direction + " for " + name
                + ": qty=" + qty
                + ", entry=" + entryPrice
                + ", value=" + positionValue
                + ", SL=" + String.format("%.2f", slPercent) + "%"
                + ", TP=" + String.format("%.2f", tpPercent) + "%");

        try {
            throttleApiCall();
            TCSService.OrderExecutionResult orderResult;
            if ("BUY".equals(decision.updatedPosition.direction)) {
                orderResult = tcsService.buyByMarketWithDetails(name, ticker.getType(), positionValue, tpPercent, slPercent);
            } else { // SELL
                orderResult = tcsService.sellByMarketWithDetails(name, ticker.getType(), positionValue, tpPercent, slPercent);
            }

            if (!orderResult.isSuccess()) {
                logOpenCandidateSkipped(name, "order_execution_failed", decision);
                log("Failed to open " + decision.updatedPosition.direction + " for " + name + ".");
                return;
            }

            Position executedPosition = mergeExecutedPosition(decision.updatedPosition, orderResult);

            positionStore.put(name, executedPosition);
            lastSeenHourBarByTicker.put(name, candles.get(candles.size() - 1).time);
            if (executedPosition.entryPrice != null && executedPosition.stopLoss != null && executedPosition.takeProfit != null) {
                TRADE_DATA_COLLECTOR.recordTradeEntry(
                        name,
                        getStrategyName(),
                        candles,
                        executedPosition.entryPrice,
                        executedPosition.stopLoss,
                        executedPosition.takeProfit,
                        decision.confidence,
                        decision.reason,
                        LocalDateTime.now()
                );
            }

            double executedEntryPrice = executedPosition.entryPrice != null ? executedPosition.entryPrice : entryPrice;
            telegramNotifyService.sendMessage(getStrategyName() + " " + executedPosition.direction + " " + name
                    + ": qty=" + executedPosition.quantity
                    + ", entry=" + executedEntryPrice
                    + ", SL=" + String.format("%.2f", abs(executedEntryPrice - executedPosition.stopLoss) / executedEntryPrice * 100) + "%"
                    + ", TP=" + String.format("%.2f", abs(executedPosition.takeProfit - executedEntryPrice) / executedEntryPrice * 100) + "%");
        } catch (Exception ex) {
            log("Failed to open " + decision.updatedPosition.direction + " for " + name + ": " + ex.getMessage());
            telegramNotifyService.sendMessage(getStrategyName() + " FAILED " + decision.updatedPosition.direction + " " + name + ": " + ex.getMessage());
        }
    }

    private void logOpenCandidateSkipped(String name, String reason, TradingDecision decision) {
        String direction = decision != null && decision.updatedPosition != null ? decision.updatedPosition.direction : "null";
        int quantity = decision != null ? decision.quantity : 0;
        Double entryPrice = decision != null ? decision.entryPrice : null;
        String signal = decision != null ? decision.reason : null;
        log("OPEN candidate skipped for " + name
                + ": reason=" + reason
                + ", signal=" + signal
                + ", direction=" + direction
                + ", qty=" + quantity
                + ", entry=" + (entryPrice != null ? entryPrice : 0.0));
    }

    protected void closePosition(String name, TickerInfo ticker, Position storedPosition, TradingDecision decision) {
        log("Decision for " + name + ": " + decision.action + " (" + decision.reason + ")");
        if (storedPosition.quantity <= 0) {
            log("CLOSE decision but no position for " + name + ", skipping.");
            return;
        }

        log("Closing position for " + name + ": " + storedPosition.quantity +
                " shares, direction=" + storedPosition.direction +
                ", reason=" + decision.reason);

        TCSService.OrderExecutionResult closeResult = TCSService.OrderExecutionResult.failed();
        if ("BUY".equals(storedPosition.direction)) {
            throttleApiCall();
            closeResult = tcsService.closeLongByMarketWithDetails(name, ticker.getType());
        } else if ("SELL".equals(storedPosition.direction)) {
            throttleApiCall();
            closeResult = tcsService.closeShortByMarketWithDetails(name, ticker.getType());
        }

        if (closeResult.isSuccess()) {
            int closedQuantity = closeResult.getExecutedCount() > 0 ? closeResult.getExecutedCount() : storedPosition.quantity;
            if (closedQuantity <= 0) {
                log("Failed to close position for " + name + " (executed quantity is zero)");
                return;
            }

            double entryPrice = storedPosition.entryPrice != null ? storedPosition.entryPrice : 0.0;
            Double executedExitPrice = closeResult.getExecutedPrice() != null
                    ? closeResult.getExecutedPrice()
                    : tcsService.getLastExecutedPrice(name, ticker.getType());
            double exitPrice = executedExitPrice != null && executedExitPrice > 0.0
                    ? executedExitPrice
                    : (decision.entryPrice != null ? decision.entryPrice : 0.0);
            double pnl = calculatePnlForQuantity(storedPosition, exitPrice, closedQuantity);
            double stopLoss = storedPosition.stopLoss != null ? storedPosition.stopLoss : entryPrice;

            if (closedQuantity >= storedPosition.quantity) {
                tcsService.clearProtectiveOrders(name, ticker.getType());
                positionStore.put(name, getCooldownPosition());
                lastSeenHourBarByTicker.remove(name);
                telegramNotifyService.sendMessage(getStrategyName() + " CLOSED " + name +
                        " (reason: " + decision.reason + ", PnL: " + String.format("%.2f", pnl) + ")");
            } else {
                int remainingQuantity = storedPosition.quantity - closedQuantity;
                positionStore.put(name, new Position(
                        storedPosition.direction,
                        storedPosition.entryPrice,
                        storedPosition.stopLoss,
                        storedPosition.takeProfit,
                        remainingQuantity,
                        storedPosition.candlesHeld,
                        storedPosition.cooldownRemaining
                ));
                log("Position for " + name + " partially closed: closed=" + closedQuantity + ", remaining=" + remainingQuantity);
                telegramNotifyService.sendMessage(getStrategyName() + " PARTIAL CLOSE " + name
                        + ": closed=" + closedQuantity
                        + ", remaining=" + remainingQuantity
                        + ", reason=" + decision.reason
                        + ", PnL=" + String.format("%.2f", pnl));
            }

            TRADE_DATA_COLLECTOR.recordTradeOutcome(name, getStrategyName(), pnl, entryPrice, stopLoss, closedQuantity);
            onTradeClosed(name, pnl, entryPrice, exitPrice, closedQuantity, storedPosition.direction);
        } else {
            log("Failed to close position for " + name + " (may not exist in broker account)");
        }
    }

    /**
     * Calculate PnL for a closed position.
     */
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

    private Position mergeExecutedPosition(Position requestedPosition, TCSService.OrderExecutionResult orderResult) {
        Position protectivePosition = orderResult.getProtectivePosition();
        Double executedEntryPrice = orderResult.getExecutedPrice() != null
                ? orderResult.getExecutedPrice()
                : requestedPosition.entryPrice;
        Double stopLoss = protectivePosition != null && protectivePosition.stopLoss != null
                ? protectivePosition.stopLoss
                : requestedPosition.stopLoss;
        Double takeProfit = protectivePosition != null && protectivePosition.takeProfit != null
                ? protectivePosition.takeProfit
                : requestedPosition.takeProfit;
        int quantity = orderResult.getExecutedCount() > 0 ? orderResult.getExecutedCount() : requestedPosition.quantity;

        return new Position(
                requestedPosition.direction,
                executedEntryPrice,
                stopLoss,
                takeProfit,
                quantity,
                requestedPosition.candlesHeld,
                requestedPosition.cooldownRemaining
        );
    }

    private void syncProtectiveOrdersIfNeeded(String name,
                                              TickerInfo ticker,
                                              Position previousPosition,
                                              Position updatedPosition) {
        if (ticker == null || updatedPosition == null || updatedPosition.quantity <= 0) {
            return;
        }

        boolean stopChanged = !java.util.Objects.equals(previousPosition.stopLoss, updatedPosition.stopLoss);
        boolean takeChanged = !java.util.Objects.equals(previousPosition.takeProfit, updatedPosition.takeProfit);
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
     * Callback for trade closure (for Money Management integration).
     * Override in subclasses to register trade results.
     */
    protected void onTradeClosed(String ticker, double pnl, double entryPrice,
                                  double exitPrice, int quantity, String direction) {
        // Default: no-op. Override in UnifiedStrategy for MM integration.
    }

    public void recordBacktestTradeEntry(String ticker,
                                         List<Candle> hourCandles,
                                         TradingDecision decision) {
        LocalDateTime entryTime = null;
        if (hourCandles != null && !hourCandles.isEmpty()) {
            try {
                Date date = CANDLE_TIME_FORMAT.get().parse(hourCandles.get(hourCandles.size() - 1).time);
                entryTime = LocalDateTime.ofInstant(date.toInstant(), ZoneId.systemDefault());
            } catch (Exception ignored) {
            }
        }
        TRADE_DATA_COLLECTOR.recordTradeEntry(ticker, getStrategyName(), hourCandles, decision, entryTime);
    }

    public void recordBacktestTradeOutcome(String ticker,
                                           double pnl,
                                           double entryPrice,
                                           double stopLoss,
                                           int quantity) {
        TRADE_DATA_COLLECTOR.recordTradeOutcome(ticker, getStrategyName(), pnl, entryPrice, stopLoss, quantity);
    }

    /**
     * Callback for daily reset (for Money Management integration).
     * Override in subclasses to reset daily limits.
     */
    protected void onDailyReset() {
        // Default: no-op. Override in UnifiedStrategy for MM integration.
    }

    protected Position getCooldownPosition() {
        return new Position(config.cooldownCandles);
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
                .filter(t -> t.getType() == TickerType.STOCK || t.getType() == TickerType.FEATURE)
                .filter(t -> t.getName().equalsIgnoreCase(name) || t.getTicker().equalsIgnoreCase(name))
                .findFirst()
                .orElse(null);
    }

    private void restoreTrackedPositions(List<String> activeTickers) {
        if (tcsService == null || isBacktest) {
            return;
        }

        Set<String> activeTickerSet = new HashSet<>(activeTickers);
        restoreTrackedPositions(activeTickerSet, STOCK);
        restoreTrackedPositions(activeTickerSet, FEATURE);
        logRestoredPositionsReport();
    }

    private void restoreTrackedPositions(Set<String> activeTickers, TickerType tickerType) {
        Map<TickerInfo.Key, com.github.shk0da.GoldenDragon.model.PositionInfo> currentPositions = tcsService.getCurrentPositions(tickerType);
        currentPositions.values().stream()
                .filter(positionInfo -> activeTickers.contains(positionInfo.getTicker()))
                .filter(positionInfo -> positionInfo.getBalance() != 0)
                .forEach(positionInfo -> {
                    TickerInfo tickerInfo = findTickerInfo(positionInfo.getTicker());
                    if (tickerInfo == null) {
                        return;
                    }

                    String direction = positionInfo.getBalance() > 0 ? "BUY" : "SELL";
                    int quantity = Math.abs(positionInfo.getBalance());
                    Double entryPrice = positionInfo.getAveragePositionPrice();
                    Position restoredPosition = new Position(direction, entryPrice, null, null, quantity, 0);
                    restoredPosition = tcsService.restoreProtectivePosition(positionInfo.getTicker(), tickerType, restoredPosition);
                    positionStore.put(positionInfo.getTicker(), restoredPosition);
                    initializeLastSeenHourBar(positionInfo.getTicker(), tickerInfo);

                    log("Restored tracked position for " + positionInfo.getTicker()
                            + ": direction=" + direction
                            + ", quantity=" + quantity
                            + ", entry=" + (entryPrice != null ? entryPrice : 0.0)
                            + ", stopLoss=" + (restoredPosition.stopLoss != null ? restoredPosition.stopLoss : 0.0)
                            + ", takeProfit=" + (restoredPosition.takeProfit != null ? restoredPosition.takeProfit : 0.0));

                    if (entryPrice != null) {
                        tcsService.syncProtectiveOrders(positionInfo.getTicker(), tickerType, restoredPosition);
                    }
                });
    }

    private void initializeLastSeenHourBar(String ticker, TickerInfo tickerInfo) {
        try {
            List<Candle> hourCandles = loadOrRefreshCandles(
                    ticker,
                    tickerInfo.getFigi(),
                    unifiedTraderConfig.getDataDir(),
                    OffsetDateTime.now(),
                    CandleInterval.CANDLE_INTERVAL_HOUR
            );
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
        positionStore.forEach((ticker, position) -> report.append("\n - ")
                .append(ticker)
                .append(": direction=").append(position.direction)
                .append(", quantity=").append(position.quantity)
                .append(", entry=").append(position.entryPrice != null ? position.entryPrice : 0.0)
                .append(", stopLoss=").append(position.stopLoss != null ? position.stopLoss : 0.0)
                .append(", takeProfit=").append(position.takeProfit != null ? position.takeProfit : 0.0));
        log(report.toString());
    }

    protected List<Candle> loadOrRefreshCandles(String name,
                                                String figi,
                                                String dataDir,
                                                OffsetDateTime now,
                                                CandleInterval interval) {
        if (tcsService == null) {
            return readCachedCandles(name, dataDir, interval);
        }

        String fileName = interval == CandleInterval.CANDLE_INTERVAL_HOUR ? "candlesHOUR.txt" : "candles5_MIN.txt";
        File candleFile = new File(dataDir + "/" + name + "/" + fileName);

        List<Candle> candles = readCachedCandles(name, dataDir, interval);
        if (candles != null && !candles.isEmpty() && isCandleDataFresh(candles, interval)) {
            return candles;
        }

        throttleApiCall();

        List<HistoricCandle> historicCandles = tcsService.getCandles(
                figi,
                interval == CandleInterval.CANDLE_INTERVAL_HOUR ? now.minusMinutes(24 * 60) : now.minusMinutes(6 * 60),
                now,
                interval
        );

        List<Candle> refreshed = new ArrayList<>();
        for (HistoricCandle hc : historicCandles) {
            Timestamp ts = new Timestamp(hc.getTime().getSeconds() * 1000);
            refreshed.add(new Candle(
                    CANDLE_TIME_FORMAT.get().format(ts),
                    IndicatorsUtil.toDouble(hc.getOpen()),
                    IndicatorsUtil.toDouble(hc.getHigh()),
                    IndicatorsUtil.toDouble(hc.getLow()),
                    IndicatorsUtil.toDouble(hc.getClose()),
                    hc.getVolume()
            ));
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
                candles.add(new Candle(tc.getDate(), tc.getOpen(), tc.getHigh(), tc.getLow(), tc.getClose(), tc.getVolume()));
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

            boolean sameDay = lastCal.get(Calendar.YEAR) == nowCal.get(Calendar.YEAR)
                    && lastCal.get(Calendar.DAY_OF_YEAR) == nowCal.get(Calendar.DAY_OF_YEAR);

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
        DayOfWeek day = LocalDateTime.now().getDayOfWeek();
        return day != DayOfWeek.SATURDAY && day != DayOfWeek.SUNDAY;
    }

    protected boolean isWorkingHours() {
        if (!isTradingDay()) {
            return false;
        }
        LocalTime now = LocalTime.now();
        return !now.isBefore(WORK_START_TIME) && now.isBefore(EOD_CLOSE_TIME);
    }

    protected boolean isEndOfDayReached() {
        if (!isTradingDay()) {
            return true;
        }
        LocalTime now = LocalTime.now();
        return !now.isBefore(EOD_CLOSE_TIME);
    }

    protected void throttleApiCall() {
        synchronized (API_LOCK) {
            long waitTime = API_CALL_DELAY_MS - (System.currentTimeMillis() - lastApiCallTime);
            if (waitTime > 0) {
                sleep(waitTime);
            }
            lastApiCallTime = System.currentTimeMillis();
        }
    }

    protected void closeAllPositions(TCSService tcsService, UnifiedTraderConfig unifiedTraderConfig) {
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
                UnifiedTraderConfig.TickerParams tickerParams = unifiedTraderConfig.getTickerParams(tickerName);
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
                    double exitPrice = tcsService.getAvailablePrice(new TickerInfo.Key(tickerName, ticker.getType()));
                    double entryPrice = position.entryPrice != null ? position.entryPrice : 0.0;
                    double pnl = calculatePnl(position, exitPrice);
                    double stopLoss = position.stopLoss != null ? position.stopLoss : entryPrice;
                    TRADE_DATA_COLLECTOR.recordTradeOutcome(
                            tickerName,
                            getStrategyName(),
                            pnl,
                            entryPrice,
                            stopLoss,
                            position.quantity
                    );
                    onTradeClosed(tickerName, pnl, entryPrice, exitPrice, position.quantity, position.direction);
                    anyClosed = true;
                    telegramNotifyService.sendMessage(getStrategyName() + " EOD CLOSED " + tickerName +
                            " (" + position.quantity + " shares)");
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

    protected void writeCandlesToFile(String name, String dataDir, String fileName, List<Candle> candles) {
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
                        writer.write(String.format(
                                "%s,%s,%s,%s,%s,%s",
                                c.time, c.open, c.high, c.low, c.close, c.volume
                        ) + System.lineSeparator());
                    }
                }
            }
        } catch (IOException ex) {
            log("Failed to write candles file for " + name + ": " + ex.getMessage());
        }
    }

    protected static void log(String message) {
        log(message, false);
    }

    protected void logWithBacktest(String message) {
        log(message, isBacktest);
    }

    protected static void log(String message, boolean silent) {
        if (silent) {
            return;
        }
        out.println("[" + LOG_TIME_FORMAT.get().format(new Date()) + "] " + message);
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
            sum += max(
                    max(c.high - c.low, abs(c.high - p.close)),
                    abs(c.low - p.close)
            );
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

            tr[i] = max(
                    max(c.high - c.low, abs(c.high - prev.close)),
                    abs(c.low - prev.close)
            );

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
            double pDI = (pdmS / trS) * 100.0;
            double mDI = (mdmS / trS) * 100.0;
            double sum = pDI + mDI;
            dx[dxStart] = sum > 0 ? abs(pDI - mDI) / sum * 100.0 : 0.0;
        }

        for (int i = period + 1; i < n; i++) {
            trS = trS - (trS / period) + tr[i];
            pdmS = pdmS - (pdmS / period) + plusDM[i];
            mdmS = mdmS - (mdmS / period) + minusDM[i];

            if (trS > 0) {
                double pDI = (pdmS / trS) * 100.0;
                double mDI = (mdmS / trS) * 100.0;
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
                s += max(
                        max(c.high - c.low, abs(c.high - p.close)),
                        abs(c.low - p.close)
                );
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
                UnifiedTraderConfig.TickerParams params = unifiedTraderConfig.getTickerParams(ticker);
                if (!params.enabled) {
                    continue;
                }

                double weight = params.allocationWeight > 0.0 ? params.allocationWeight : 1.0;
                weights.put(ticker, weight);
                totalWeight += weight;
            } catch (Exception ex) {
                log("Failed to read allocation weight for ticker " + ticker + ": " + ex.getMessage());
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

        Map<String, Double> allocation = new HashMap<>();
        for (Map.Entry<String, Double> e : weights.entrySet()) {
            allocation.put(e.getKey(), totalCash * (e.getValue() / totalWeight));
        }

        return allocation;
    }

    /**
     * Применяет фильтры BadWeather и MarketRegime перед открытием позиции.
     * Возвращает TradingDecision с HOLD если фильтр не прошёл, или null если всё ок.
     */
    protected TradingDecision applyFilters(String ticker,
                                           List<Candle> hourCandles,
                                           Candle currentCandle,
                                           Position position,
                                           UnifiedTraderConfig.TickerParams tickerParams) {
        if (!badWeatherFilter.canTrade(hourCandles, currentCandle.close, tickerParams.badWeatherParams)) {
            String reason = badWeatherFilter.getBlockReason(hourCandles, currentCandle.close, tickerParams.badWeatherParams);
            return new TradingDecision("HOLD", reason != null ? "BAD_WEATHER_" + reason : "BAD_WEATHER",
                    0.0, 0, null, null, null, position);
        }

        MarketRegimeFilter.FilterResult regimeResult = marketRegimeFilter.evaluate(
                hourCandles,
                tickerParams.marketRegimeAdxRangeThreshold,
                tickerParams.marketRegimeAdxUnclearThreshold,
                tickerParams.marketRegimeVolumeRatioMin,
                tickerParams.marketRegimeConfidenceMin,
                tickerParams.marketRegimeAtrBars
        );

        if (!regimeResult.canTrade) {
            return new TradingDecision("HOLD", "REGIME_" + regimeResult.reason,
                    0.0, 0, null, null, null, position);
        }

        return null;
    }
}
