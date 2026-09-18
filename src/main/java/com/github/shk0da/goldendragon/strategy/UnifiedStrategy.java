package com.github.shk0da.goldendragon.strategy;

import com.github.shk0da.goldendragon.config.UnifiedTraderConfig;
import com.github.shk0da.goldendragon.config.UnifiedTraderConfig.RegimeFilterParams;
import com.github.shk0da.goldendragon.filters.GroupConfirmationFilter;
import com.github.shk0da.goldendragon.filters.MarketRegimeFilter;
import com.github.shk0da.goldendragon.model.Candle;
import com.github.shk0da.goldendragon.model.Config;
import com.github.shk0da.goldendragon.model.Group;
import com.github.shk0da.goldendragon.model.Position;
import com.github.shk0da.goldendragon.model.TickerInfo;
import com.github.shk0da.goldendragon.model.TickerType;
import com.github.shk0da.goldendragon.model.TradingDecision;
import com.github.shk0da.goldendragon.money.AdaptiveCapital;
import com.github.shk0da.goldendragon.money.AdaptiveLeverage;
import com.github.shk0da.goldendragon.money.FixedRiskSizing;
import com.github.shk0da.goldendragon.money.KillSwitch;
import com.github.shk0da.goldendragon.money.PerformanceTracker;
import com.github.shk0da.goldendragon.money.PositionSizer;
import com.github.shk0da.goldendragon.money.RiskManager;
import com.github.shk0da.goldendragon.money.SizingStrategy;
import com.github.shk0da.goldendragon.money.StopLossManager;
import com.github.shk0da.goldendragon.money.StopLossTakeProfitStrategy;
import com.github.shk0da.goldendragon.money.StopLossTakeProfitStrategyFactory;
import com.github.shk0da.goldendragon.money.VolatilityAdjustedSizing;
import com.github.shk0da.goldendragon.repository.TickerRepository;
import com.github.shk0da.goldendragon.service.TradingService;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

/**
 * Унифицированная торговая стратегия, объединяющая трендовые, контртрендовые (FX) и смешанные
 * подходы с интегрированной системой управления капиталом (Money Management).
 *
 * <h2>Общее описание</h2>
 *
 * Стратегия принимает торговые решения на основе часовых и минутных свечей, адаптируя логику
 * входа/выхода под группу инструмента ({@link Group#TREND}, {@link Group#FX}, {@link Group#MIXED})
 * и текущий рыночный режим. Поддерживает работу как в режиме реальной торговли, так и в режиме
 * бэктеста.
 *
 * <h2>Сигнальная логика</h2>
 *
 * <ul>
 *   <li><b>{@link #trendSignal(List)}</b> — трендовый сигнал по схеме голосования (тренд по EMA,
 *       ADX, RSI, паттерн свечи). Возвращает {@code TB_*} (buy) или {@code TS_*} (sell) при наборе
 *       4+ голосов.
 *   <li><b>{@link #fxSignal(List, List)}</b> — контртрендовый сигнал на экстремумах RSI
 *       (перепроданность/перекупленность) в сочетании с разворотными свечными паттернами.
 *       Возвращает {@code FXB_*} / {@code FXS_*}.
 *   <li><b>{@link #mixedSignal(List, List)}</b> — гибридный сигнал, комбинирующий трендовые
 *       признаки и свечные паттерны с весом. Возвращает {@code MXB_*} / {@code MXS_*}.
 *   <li><b>{@link #candlePattern(List)}</b> — распознавание свечных паттернов: DOJI, PIN_BAR,
 *       ENGULFING, MORNING/EVENING_STAR, THREE_WHITE/BLACK.
 * </ul>
 *
 * Шорт-сигналы открываются только при {@code config.shortsEnabled = true}; иначе обрабатываются
 * лишь BUY-позиции (reason {@code short_disabled}).
 *
 * <h2>Классификация рыночного режима</h2>
 *
 * Используются пороги ADX:
 *
 * <ul>
 *   <li>{@link #RANGE_ADX} (15.0) — флэт/диапазон → риск снижается на 35%.
 *   <li>{@link #STRONG_TREND_ADX} (30.0) — сильный тренд → риск увеличивается на 20%, SL
 *       расширяется на 10%, TP на 20%.
 *   <li>{@link #HOT_TREND_ADX} (38.0) — экстремально сильный тренд → риск +45%, TP расширяется на
 *       35%.
 * </ul>
 *
 * <h2>Regime Filter</h2>
 *
 * <p>Настраиваемый фильтр рыночного режима на основе ADX (мигрирован из RegimeAwareStrategy):
 *
 * <ul>
 *   <li>RANGE (ADX &lt; rangeAdxMax): пропуск всех входов
 *   <li>NORMAL (ADX rangeAdxMax..trendAdxMin): фильтр слабых сигналов (FX, TB_4)
 *   <li>TREND (ADX &gt; trendAdxMin): все сигналы разрешены
 * </ul>
 *
 * <p>Конфигурация через {@code unifiedTrader.regimeFilter.*} свойства.
 *
 * <h2>Фильтры входа</h2>
 *
 * Перед открытием позиции проверяются последовательно:
 *
 * <ol>
 *   <li>{@code KillSwitch} — глобальная остановка торговли при критической просадке.
 *   <li>Активность тикера и MM-флаг ({@code tpCfg.enabled}, {@code tpCfg.mmEnabled}).
 *   <li>{@code RiskManager} — дневной лимит убытков и серия проигрышей.
 *   <li>Cooldown после закрытия позиции ({@code cooldownRemaining}).
 *   <li>{@code badWeatherFilter} — фильтр неблагоприятных рыночных условий.
 *   <li>{@link MarketRegimeFilter} — оценка режима по ADX, объёму и confidence.
 *   <li>ATR-фильтры: нулевой ATR и спайки ({@code atrSpikeThreshold}).
 *   <li>{@link GroupConfirmationFilter} — подтверждение по peer-инструментам группы.
 *   <li>RSI overheating ({@code rsi > 72.0}).
 * </ol>
 *
 * <h2>Управление открытой позицией</h2>
 *
 * <ul>
 *   <li>Закрытие по SL / TP при касании ценой high/low текущей свечи.
 *   <li>Закрытие по таймауту ({@code maxCandlesHold} / {@code maxCandlesHoldFx} для FX).
 *   <li>Если MM включён — стоп-лосс обновляется через {@link StopLossManager} (breakeven + трейлинг
 *       по ATR с учётом R-кратности от начального риска).
 *   <li>Иначе используется legacy-трейлинг с тремя ступенями (0.5R / 1.0R / 1.8R) и групповым
 *       множителем (FX=0.7, MIXED=0.9, TREND=1.0).
 * </ul>
 *
 * <h2>Money Management</h2>
 *
 * Активируется флагом {@code config.mmEnabled}. Подсистемы:
 *
 * <ul>
 *   <li>{@link PositionSizer} с {@link SizingStrategy}: {@link FixedRiskSizing} или {@link
 *       VolatilityAdjustedSizing} (макс. 25% капитала).
 *   <li>{@link RiskManager} — лимиты дневного убытка и серии лоссов.
 *   <li>{@link AdaptiveCapital} — динамическая корректировка риска по результатам (снижение после N
 *       проигрышей, восстановление после N выигрышей).
 *   <li>{@link KillSwitch} — аварийное отключение при критической просадке.
 *   <li>{@link PerformanceTracker} — учёт PnL, статистика, drawdown.
 * </ul>
 *
 * Начальный риск каждой позиции кэшируется в {@link #initialRiskPerTicker} для корректного расчёта
 * R-кратностей в трейлинге.
 *
 * <h2>Расчёт размера позиции</h2>
 *
 * При включённом MM — через {@link PositionSizer} с поправкой на {@code
 * adaptiveCapital.getRiskMultiplier()}. При выключенном MM — legacy: размер риска = {@code balance
 * × riskP × regimeMultiplier × confidenceK × signalStrengthK}, далее qty = risk / slDist с
 * ограничением {@code balance / entry}. Итоговый риск ограничен диапазоном [0.5%; 3%].
 *
 * <h2>Жизненный цикл</h2>
 *
 * <ul>
 *   <li>{@link #decide} — основной метод принятия решения на каждой свече.
 *   <li>{@link #onTradeClosed} / {@link #registerTradeResult} — учёт результата сделки в
 *       MM-компонентах.
 *   <li>{@link #onDailyReset} / {@link #dailyReset} — сброс дневных лимитов (RiskManager,
 *       PerformanceTracker session, KillSwitch, AdaptiveCapital).
 * </ul>
 *
 * <h2>Возвращаемые причины решений (reason codes)</h2>
 *
 * {@code init}, {@code ticker_disabled}, {@code MM_DISABLED_TICKER}, {@code KILL_SWITCH_*}, {@code
 * RISK_LIMIT_*}, {@code stop_loss}, {@code take_profit}, {@code expired}, {@code CD<n>}, {@code
 * in_pos}, {@code BAD_WEATHER_*}, {@code REGIME_*}, {@code ATR0}, {@code ATRspike}, {@code noSig},
 * {@code short_disabled}, {@code noGroupConf_*}, {@code rsi_hot}, {@code dist0}, {@code qty0},
 * {@code MM_QTY_ZERO}.
 */
public class UnifiedStrategy extends BaseStrategy {

    private static final double RANGE_ADX = 15.0;
    private static final double STRONG_TREND_ADX = 30.0;
    private static final double HOT_TREND_ADX = 38.0;
    private static final int DEFAULT_FUTURES_LOT = 1000;
    private static final double MARKET_ORDER_CASH_BUFFER_PERCENT = 0.001;
    private static final double MARKET_ORDER_CASH_BUFFER_MIN = 10.0;

    // Minimum votes required for a trend signal (out of 6 indicators)
    private static final int TREND_SIGNAL_MIN_VOTES = 6;
    // Skip entries after this many consecutive losses
    private static final int LOSS_STREAK_SKIP_THRESHOLD = 2;

    // Money Management components
    private final RiskManager riskManager;
    private final PositionSizer positionSizer;
    private final AdaptiveCapital adaptiveCapital;
    private final KillSwitch killSwitch;
    private final PerformanceTracker performanceTracker;
    private final StopLossManager stopLossManager;
    private final boolean mmEnabled;

    // Track initial risk per position for R-based calculations
    private final ConcurrentMap<String, Double> initialRiskPerTicker = new ConcurrentHashMap<>();

    // Track consecutive losses per ticker for entry cooldown
    private final ConcurrentMap<String, Integer> consecutiveLossTracker = new ConcurrentHashMap<>();

    // Liquidity cache: ticker -> {liquidity, timestamp} to avoid repeated order book reads
    private static final long LIQUIDITY_CACHE_TTL_MS = 5000; // 5 seconds
    private final ConcurrentMap<String, LiquidityCacheEntry> liquidityCache = new ConcurrentHashMap<>();

    private static class LiquidityCacheEntry {
        final int liquidity;
        final long timestamp;
        LiquidityCacheEntry(int liquidity, long timestamp) {
            this.liquidity = liquidity;
            this.timestamp = timestamp;
        }
        boolean isExpired() {
            return System.currentTimeMillis() - timestamp > LIQUIDITY_CACHE_TTL_MS;
        }
    }

    // Regime Filter (migrated from RegimeAwareStrategy)
    private final boolean regimeFilterEnabled;
    private final String regimeFilterMode;
    private final double regimeRangeAdxMax;
    private final double regimeTrendAdxMin;
    private final double regimeNormalMinAdx;
    private int trendBars = 0;
    private int rangeBars = 0;
    private int normalBars = 0;

    public UnifiedStrategy(UnifiedTraderConfig unifiedTraderConfig, TradingService tradingService) {
        this(unifiedTraderConfig, tradingService, new Config(unifiedTraderConfig));
    }

    public UnifiedStrategy(
            UnifiedTraderConfig unifiedTraderConfig,
            TradingService tradingService,
            Config config) {
        this(unifiedTraderConfig, tradingService, config, null);
    }

    public UnifiedStrategy(
            UnifiedTraderConfig unifiedTraderConfig,
            TradingService tradingService,
            Config config,
            com.github.shk0da.goldendragon.config.MainConfig mainConfig) {
        this(unifiedTraderConfig, tradingService, config, mainConfig, null, null);
    }

    public UnifiedStrategy(
            UnifiedTraderConfig unifiedTraderConfig,
            TradingService tradingService,
            Config config,
            com.github.shk0da.goldendragon.config.MainConfig mainConfig,
            TradingService backtestTradingService,
            Map<String, List<Candle>> peerCandlesForBacktest) {
        super(unifiedTraderConfig, tradingService,
                config != null ? config : new Config(unifiedTraderConfig), null,
                mainConfig, backtestTradingService, peerCandlesForBacktest);

        Config effectiveConfig = config != null ? config : new Config(unifiedTraderConfig);
        effectiveConfig.shortsEnabled = unifiedTraderConfig.getShortsEnabled();
        this.mmEnabled = effectiveConfig.mmEnabled;

        if (mmEnabled) {
            // Initialize SizingStrategy
            SizingStrategy sizingStrategy;
            if ("VOLATILITY".equalsIgnoreCase(effectiveConfig.mmSizingStrategy)) {
                sizingStrategy =
                        new VolatilityAdjustedSizing(
                                effectiveConfig.mmRiskPercent,
                                effectiveConfig.mmVolatilityBaseAtr,
                                effectiveConfig.mmVolatilityMinAdjustment,
                                effectiveConfig.mmVolatilityMaxAdjustment,
                                effectiveConfig.mmMaxPositionSize);
            } else {
                sizingStrategy =
                        new FixedRiskSizing(effectiveConfig.mmRiskPercent, effectiveConfig.mmMaxPositionSize);
            }

            // Initialize MM components
            this.positionSizer = new PositionSizer(sizingStrategy);
            this.riskManager =
                    new RiskManager(
                            effectiveConfig.mmMaxDailyLossPercent,
                            effectiveConfig.mmMaxConsecutiveLosses);
            this.adaptiveCapital =
                    new AdaptiveCapital(
                            effectiveConfig.mmRiskPercent,
                            effectiveConfig.mmLossesToReduce,
                            effectiveConfig.mmWinsToRestore,
                            effectiveConfig.mmRiskReductionFactor);
            this.killSwitch = new KillSwitch(effectiveConfig.mmCriticalDrawdownPercent);
            this.performanceTracker = new PerformanceTracker();
            this.stopLossManager =
                    new StopLossManager(
                            effectiveConfig.mmTrailingActivationR,
                            effectiveConfig.mmTrailingMultiplier,
                            effectiveConfig.mmBreakevenActivationR,
                            effectiveConfig.mmBreakevenBuffer,
                            effectiveConfig.mmTrailingTpEnabled,
                            effectiveConfig.mmTrailingTpCallbackPercent,
                            effectiveConfig.mmTrailingEnabled,
                            effectiveConfig.mmTrailingCheckInterval,
                            effectiveConfig.commission);

            log(
                    "Money Management initialized: risk="
                            + (effectiveConfig.mmRiskPercent * 100)
                            + "%, dailyLoss="
                            + (effectiveConfig.mmMaxDailyLossPercent * 100)
                            + "%, criticalDD="
                            + (effectiveConfig.mmCriticalDrawdownPercent * 100)
                            + "%");
        } else {
            this.positionSizer = null;
            this.riskManager = null;
            this.adaptiveCapital = null;
            this.killSwitch = null;
            this.performanceTracker = null;
            this.stopLossManager = null;
            log("Money Management disabled");
        }

        // Initialize regime filter from config
        RegimeFilterParams rfCfg = unifiedTraderConfig.getRegimeFilterConfig();
        this.regimeFilterEnabled = rfCfg.enabled;
        this.regimeFilterMode = rfCfg.mode;
        this.regimeRangeAdxMax = rfCfg.rangeAdxMax;
        this.regimeTrendAdxMin = rfCfg.trendAdxMin;
        this.regimeNormalMinAdx = rfCfg.normalMinAdx;

        log("RegimeFilter: enabled=" + regimeFilterEnabled + ", mode=" + regimeFilterMode
                + ", RANGE<=" + String.format("%.1f", regimeRangeAdxMax)
                + ", TREND>=" + String.format("%.1f", regimeTrendAdxMin));
    }

    @Override
    protected String getStrategyName() {
        return "UnifiedStrategy";
    }

    @Override
    public TradingDecision decide(
            String ticker,
            List<Candle> hourCandles,
            List<Candle> minuteCandles,
            Position position,
            double balance,
            boolean incrementCandlesHeld) {
        if (cashParkingManager.isParkingTicker(ticker) && unifiedTraderConfig.isTmonCashParkingEnabled()) {
            return new TradingDecision("HOLD", "PARKING_TICKER");
        }

        if (hourCandles == null
                || hourCandles.size() < 60
                || minuteCandles == null
                || minuteCandles.isEmpty()) {
            return new TradingDecision("HOLD", "init");
        }

        // Update equity and check KillSwitch on every decision (not just on trade close)
        if (mmEnabled && performanceTracker != null && tradingService != null) {
            Double equity = tradingService.getTotalPortfolioValue();
            if (equity > 0) {
                performanceTracker.updateEquity(equity);
                // Check KillSwitch immediately after equity update
                if (killSwitch != null) {
                    killSwitch.checkDrawdown(performanceTracker.getCurrentDrawdown());
                }
            }
        }

        // Money Management: Check KillSwitch
        if (mmEnabled && killSwitch != null && !killSwitch.isTradingAllowed()) {
            // Emergency close all positions on critical drawdown
            if (tradingService != null) {
                try {
                    tradingService.closeLong(ticker);
                } catch (Exception ignore) {
                    try {
                        tradingService.closeShort(ticker);
                    } catch (Exception ignored2) {
                        // No position to close
                    }
                }
            }
            return new TradingDecision("HOLD", "KILL_SWITCH_" + killSwitch.getTriggerReason());
        }

        UnifiedTraderConfig.TickerParams tpCfg = unifiedTraderConfig.getTickerParams(ticker);
        if (!tpCfg.enabled) {
            return new TradingDecision("HOLD", "ticker_disabled");
        }

        // Money Management: Check per-ticker MM enabled
        if (mmEnabled && !tpCfg.mmEnabled) {
            return new TradingDecision("HOLD", "MM_DISABLED_TICKER");
        }

        // Money Management: Check RiskManager limits
        if (mmEnabled && riskManager != null) {
            // Use initial balance for daily loss calculation (prevents equity drift)
            double riskBalance = balance;
            if (isBacktestMode()) {
                double initialBalance = tradingService.getInitialBalance();
                if (initialBalance > 0) {
                    riskBalance = initialBalance;
                }
            }
            if (!riskManager.canTrade(riskBalance)) {
                return new TradingDecision(
                        "HOLD",
                        "RISK_LIMIT_"
                                + riskManager.getConsecutiveLosses()
                                + "_LOSS_"
                                + (int) (riskManager.getDailyPnL() * 100)
                                + "%");
            }
        }

        Candle cur = minuteCandles.get(minuteCandles.size() - 1);
        Position p = position;
        Group grp = Group.valueOf(unifiedTraderConfig.getTickerGroup(ticker));

        if (position.quantity > 0 && incrementCandlesHeld) {
            p =
                    copyPosition(
                            position,
                            position.direction,
                            position.entryPrice,
                            position.stopLoss,
                            position.takeProfit,
                            position.quantity,
                            position.candlesHeld + 1,
                            position.cooldownRemaining);
        } else if (position.quantity > 0) {
            p =
                    copyPosition(
                            position,
                            position.direction,
                            position.entryPrice,
                            position.stopLoss,
                            position.takeProfit,
                            position.quantity,
                            position.candlesHeld,
                            position.cooldownRemaining);
        }

        if (p.cooldownRemaining > 0) {
            return new TradingDecision(
                    "HOLD",
                    "CD" + p.cooldownRemaining,
                    0.0,
                    0,
                    null,
                    null,
                    null,
                    new Position(
                            p.direction,
                            p.entryPrice,
                            p.stopLoss,
                            p.takeProfit,
                            p.quantity,
                            p.candlesHeld,
                            p.cooldownRemaining - 1,
                            p.appliedLeverage));
        }

        if (p.quantity > 0) {
            return manageOpenPosition(ticker, p, hourCandles, minuteCandles, cur, grp);
        }

        if (!badWeatherFilter.canTrade(hourCandles, cur.close, tpCfg.badWeatherParams)) {
            String reason =
                    badWeatherFilter.getBlockReason(hourCandles, cur.close, tpCfg.badWeatherParams);
            return new TradingDecision(
                    "HOLD",
                    reason != null ? "BAD_WEATHER_" + reason : "BAD_WEATHER",
                    0.0,
                    0,
                    null,
                    null,
                    null,
                    p);
        }

        MarketRegimeFilter.FilterResult regimeResult =
                marketRegimeFilter.evaluate(
                        hourCandles,
                        tpCfg.marketRegimeAdxRangeThreshold,
                        tpCfg.marketRegimeAdxUnclearThreshold,
                        tpCfg.marketRegimeVolumeRatioMin,
                        tpCfg.marketRegimeConfidenceMin,
                        tpCfg.marketRegimeAtrBars);

        if (!regimeResult.canTrade) {
            return new TradingDecision(
                    "HOLD", "REGIME_" + regimeResult.reason, 0.0, 0, null, null, null, p);
        }

        double dAtr = atrVal(hourCandles, config.atrPeriod);
        double avgAtr = emaAtr(hourCandles, config.atrPeriod);

        if (dAtr <= 0.0 || avgAtr <= 0.0) {
            return new TradingDecision("HOLD", "ATR0", 0.0, 0, null, null, null, p);
        }

        if (dAtr > avgAtr * config.atrSpikeThreshold) {
            return new TradingDecision("HOLD", "ATRspike", 0.0, 0, null, null, null, p);
        }

        double adx = adxVal(hourCandles, config.adxPeriod);
        double rsi = rsiVal(hourCandles, config.rsiPeriod);

        String signal;
        switch (grp) {
            case FX:
                signal = fxSignal(hourCandles, minuteCandles);
                break;
            case MIXED:
                signal = mixedSignal(hourCandles, minuteCandles);
                break;
            default:
                signal = trendSignal(hourCandles);
        }

        if (signal == null) {
            return new TradingDecision("HOLD", "noSig", 0.0, 0, null, null, null, p);
        }

        // Regime filter (migrated from RegimeAwareStrategy)
        MarketRegime detectedRegime = MarketRegime.UNKNOWN;
        double regimeAdx = 0.0;

        if (regimeFilterEnabled && hourCandles != null && hourCandles.size() >= 60) {
            regimeAdx = calculateAdxForRegime(hourCandles, 14);

            if (regimeAdx >= regimeTrendAdxMin) {
                detectedRegime = MarketRegime.TREND;
                trendBars++;
            } else if (regimeAdx <= regimeRangeAdxMax) {
                detectedRegime = MarketRegime.RANGE;
                rangeBars++;
            } else {
                detectedRegime = MarketRegime.NORMAL;
                normalBars++;
            }

            if (MarketRegime.RANGE == detectedRegime) {
                return new TradingDecision("HOLD", "RANGE_SKIP_ADX" + (int) regimeAdx);
            }

            if (MarketRegime.NORMAL == detectedRegime && regimeAdx < regimeNormalMinAdx) {
                return new TradingDecision("HOLD", "NORMAL_WEAK_ADX" + (int) regimeAdx);
            }
        }

        boolean strongTrend = adx >= STRONG_TREND_ADX;
        boolean rangeRegime = adx > 0.0 && adx <= RANGE_ADX;

        boolean isBuy =
                signal.startsWith("TB") || signal.startsWith("FXB") || signal.startsWith("MXB");
        if (!isBuy && !config.shortsEnabled) {
            return new TradingDecision("HOLD", "short_disabled", 0.0, 0, null, null, null, p);
        }

        String direction = isBuy ? "BUY" : "SELL";

        String allocationGroup = tpCfg.allocationGroup;
        if (allocationGroup != null
                && !allocationGroup.isEmpty()
                && peerCandles != null
                && !peerCandles.isEmpty()) {
            if (!GroupConfirmationFilter.isConfirmed(ticker, isBuy, peerCandles)) {
                return new TradingDecision(
                        "HOLD", "noGroupConf_" + signal, 0.0, 0, null, null, null, p);
            }
        }

        // Skip entry after consecutive losses
        int lossStreak = consecutiveLossTracker.getOrDefault(ticker, 0);
        if (lossStreak >= LOSS_STREAK_SKIP_THRESHOLD) {
            return new TradingDecision(
                    "HOLD", "loss_streak_" + lossStreak, 0.0, 0, null, null, null, p);
        }

        // Regime-based signal filtering
        if (regimeFilterEnabled && MarketRegime.NORMAL == detectedRegime) {
            if (signal.startsWith("FX")) {
                return new TradingDecision("HOLD", "NORMAL_SKIP_FX_" + signal);
            }
            if (regimeAdx < regimeTrendAdxMin && signal.startsWith("TB_4")) {
                return new TradingDecision("HOLD", "WEAK_TREND_SKIP_" + signal);
            }
        }

        // RSI overheating filter: only block in weak trends (ADX < 25)
        // In strong trends, RSI can remain overbought/oversold for extended periods
        if (adx < 25.0) {
            if (isBuy && rsi > 70.0) {
                return new TradingDecision("HOLD", "rsi_hot", 0.0, 0, null, null, null, p);
            }
            if (!isBuy && rsi < 30.0) {
                return new TradingDecision("HOLD", "rsi_cold", 0.0, 0, null, null, null, p);
            }
        }

        double entry = cur.close;
        if (minuteCandles.size() >= 3) {
            Candle prev1 = minuteCandles.get(minuteCandles.size() - 2);
            Candle prev2 = minuteCandles.get(minuteCandles.size() - 3);

            if (isBuy) {
                boolean pullbackBuy = cur.close < prev1.close && cur.close > prev2.low;
                if (pullbackBuy) {
                    entry = Math.min(cur.open, cur.close);
                }
            } else {
                boolean pullbackSell = cur.close > prev1.close && cur.close < prev2.high;
                if (pullbackSell) {
                    entry = Math.max(cur.open, cur.close);
                }
            }
        }

        // Calculate SL/TP using configured algorithm
        double slMult = tpCfg.mmEnabled ? tpCfg.mmAtrStopMultiplier : tpCfg.slMult;
        double tpMult = tpCfg.tpMult;
        StopLossTakeProfitStrategy strategy = StopLossTakeProfitStrategyFactory.create(tpCfg.slTpAlgorithm);
        StopLossTakeProfitStrategy.SLTPResult sltpResult = strategy.calculate(
                entry, isBuy, hourCandles, dAtr, avgAtr, adx, slMult, tpMult, config.atrPeriod, config.commission);

        if (sltpResult == null || sltpResult.slDistance <= 0.0 || sltpResult.tpDistance <= 0.0) {
            return new TradingDecision("HOLD", "dist0", 0.0, 0, null, null, null, p);
        }

        double slDist = sltpResult.slDistance;
        double tpDist = sltpResult.tpDistance;
        double sl = isBuy ? entry - slDist : entry + slDist;
        double tp = isBuy ? entry + tpDist : entry - tpDist;

        int maxLeverage = Math.max(1, tpCfg.leverage);
        int effectiveLeverage = resolveEntryLeverage(
                maxLeverage,
                adx,
                dAtr,
                avgAtr,
                regimeResult.confidence,
                strongTrend,
                rangeRegime,
                                signal);

        int maxAffordableQty =
                calculateMaxAffordableQuantity(ticker, balance, entry, effectiveLeverage);
        int maxAskQty = calculateAvailableLiquidity(ticker, isBuy, hourCandles);

        if (maxAskQty <= 0) {
            return new TradingDecision("HOLD", "ASK_QTY0", 0.0, 0, null, null, null, p);
        }

        // Money Management: Use PositionSizer if enabled
        int qty;
        if (mmEnabled && positionSizer != null) {
            // Size positions on initial balance to prevent compounding oversizing in backtest
            // In backtest mode, use SimulatedBroker's initialBalance; in live mode, use current balance
            double sizingBalance = balance;
            if (isBacktestMode()) {
                double initialBalance = tradingService.getInitialBalance();
                if (initialBalance > 0) {
                    sizingBalance = initialBalance;
                }
            }
            double riskMultiplier = adaptiveCapital.getRiskMultiplier();
            double adjustedBalance = sizingBalance * riskMultiplier;

            qty = positionSizer.calculateSize(ticker, entry, sl, adjustedBalance, dAtr);
            if (effectiveLeverage > 1) {
                qty = (int) Math.min((long) qty * effectiveLeverage, maxAffordableQty);
            }
            qty = Math.min(qty, Math.min(maxAffordableQty, maxAskQty));

            // Defensive cap: position value cannot exceed mmMaxPositionSize fraction of sizingBalance
            // (not current balance) to prevent unrealistic position growth from compounding in backtest.
            double positionSizeCap = config.mmMaxPositionSize;
            // Reduce position size on drawdown for capital preservation
            if (performanceTracker != null && tradingService != null) {
                Double equity = tradingService.getTotalPortfolioValue();
                if (equity != null && equity > 0) {
                    // For backtest, get peak from broker directly (not from reset daily PerformanceTracker)
                    if (isBacktestMode()) {
                        Double peak = tradingService.getGlobalPeakEquity();
                        if (peak != null && peak > 0) {
                            positionSizeCap *= performanceTracker.getPositionSizeMultiplier(peak, equity);
                        }
                    } else {
                        positionSizeCap *= performanceTracker.getPositionSizeMultiplier(
                                performanceTracker.getGlobalPeakEquity(), equity);
                    }
                }
            }
            // Get lot size to cap position value correctly
            TickerInfo tickerInfo = resolveTickerInfo(ticker);
            int lotSize = tickerInfo != null && tickerInfo.getLot() != null
                ? Math.max(1, tickerInfo.getLot())
                : 1;
            long maxPositionValue = (long) (sizingBalance * positionSizeCap);
            long maxQtyByValue = maxPositionValue / (long) Math.max(1, (int) (entry * lotSize));
            qty = Math.min(qty, (int) Math.min(maxQtyByValue, (long) Integer.MAX_VALUE));

            if (qty > 0 && isVerboseLogging()) {
                System.out.println("SIZE " + ticker + ": qty=" + qty + " sizingBalance="
                        + String.format("%.0f", sizingBalance) + " bal=" + String.format("%.0f", balance)
                        + " maxAfford=" + maxAffordableQty + " maxAsk=" + maxAskQty);
            }

            if (qty <= 0) {
                return new TradingDecision("HOLD", "MM_QTY_ZERO", 0.0, 0, null, null, null, p);
            }

        } else {
            // Use balance as direct position cost (full balance invested)
            // Calculate quantity to invest the full balance amount
            // qty is in LOTS (consistent with Position.quantity and SimulatedBroker.buyByQuantity)
            TickerInfo tickerInfo = resolveTickerInfo(ticker);
            int lotSize = tickerInfo != null && tickerInfo.getLot() != null
                ? Math.max(1, tickerInfo.getLot())
                : 1;

            double positionValue = balance;
            double maxLotsByCapital = positionValue / (entry * lotSize);

            // maxAffordableQty is in UNITS (shares), convert to lots
            double maxLotsAffordable = maxAffordableQty / (double) lotSize;
            // maxAskQty is in LOTS
            double maxLots = Math.min(maxLotsAffordable, maxAskQty);

            qty = (int) Math.floor(Math.min(maxLotsByCapital, maxLots));

            if (qty <= 0) {
                return new TradingDecision("HOLD", "qty0", 0.0, 0, null, null, null, p);
            }
        }

        if (effectiveLeverage > 1
                && unifiedTraderConfig.isAdaptiveLeverageEnabled()
                && isVerboseLogging()) {
            log(
                    "Adaptive leverage for "
                            + ticker
                            + ": "
                            + effectiveLeverage
                            + "x (max "
                            + maxLeverage
                            + "x, ADX="
                            + String.format("%.1f", adx)
                            + ")");
        }

        double tradeConfidence =
                mmEnabled ? adaptiveCapital.getCurrentRiskPercent() / config.mmRiskPercent : 1.0;

        // Track initial risk per position for R-based trailing calculations
        double initialRisk = Math.abs(entry - sl);
        if (mmEnabled && initialRisk > 0) {
            initialRiskPerTicker.put(ticker, initialRisk);
        }

        return new TradingDecision(
                "OPEN",
                signal,
                tradeConfidence,
                qty,
                sl,
                tp,
                entry,
                new Position(direction, entry, sl, tp, qty, 0, 0, effectiveLeverage));
    }

    public String trendSignal(List<Candle> candles) {
        if (candles.size() < 60) return null;

        Candle cur = candles.get(candles.size() - 1);
        double p = cur.close;
        double emaT = ema(candles, config.emaTrend);
        double emaF = ema(candles, config.emaFast);
        double emaS = ema(candles, config.emaSlow);
        double adx = adxVal(candles, config.adxPeriod);
        double rsi = rsiVal(candles, config.rsiPeriod);

        boolean uptrend = p > emaT;
        boolean dnTrend = p < emaT;
        boolean emaUp = emaF > emaS;
        boolean emaDn = emaF < emaS;
        boolean trendOk = adx >= Math.max(config.adxMin, 18.0);
        boolean candleUp = cur.close > cur.open && cur.close > emaF;
        boolean candleDn = cur.close < cur.open && cur.close < emaF;
        boolean momentumUp = rsi >= 40.0 && rsi <= 70.0;
        boolean momentumDn = rsi >= 30.0 && rsi <= 60.0;

        int bs = 0, ss = 0;

        if (uptrend) bs++;
        if (dnTrend) ss++;

        if (trendOk) {
            bs++;
            ss++;
        }

        if (emaUp) bs++;
        if (emaDn) ss++;

        if (momentumUp) bs++;
        if (momentumDn) ss++;

        if (candleUp) bs++;
        if (candleDn) ss++;

        if (p > emaF && emaF > emaS) bs++;
        if (p < emaF && emaF < emaS) ss++;

        if (bs >= TREND_SIGNAL_MIN_VOTES && uptrend)
            return "TB_" + bs + "_" + (int) adx + "_" + (int) rsi;
        if (ss >= TREND_SIGNAL_MIN_VOTES && dnTrend)
            return "TS_" + ss + "_" + (int) adx + "_" + (int) rsi;

        return null;
    }

    private int resolveEntryLeverage(
             int maxLeverage,
             double adx,
             double atr,
             double avgAtr,
             double regimeConfidence,
             boolean strongTrend,
             boolean rangeRegime,
             String signal) {
        if (maxLeverage <= 1 || !unifiedTraderConfig.isAdaptiveLeverageEnabled()) {
            return maxLeverage;
        }
        double riskMultiplier = mmEnabled ? adaptiveCapital.getRiskMultiplier() : 1.0;
        return AdaptiveLeverage.resolve(
                new AdaptiveLeverage.Context(
                        maxLeverage,
                        unifiedTraderConfig.getLeverageMin(),
                        adx,
                        atr,
                        avgAtr,
                        regimeConfidence,
                        riskMultiplier,
                        strongTrend,
                        rangeRegime,
                        signal));
    }

    private Position copyPosition(
            Position src,
            String direction,
            Double entryPrice,
            Double stopLoss,
            Double takeProfit,
            int quantity,
            int candlesHeld,
            int cooldownRemaining) {
        int leverage = src != null && src.appliedLeverage > 0 ? src.appliedLeverage : 1;
        return new Position(
                direction,
                entryPrice,
                stopLoss,
                takeProfit,
                quantity,
                candlesHeld,
                cooldownRemaining,
                leverage);
    }

    /**
     * Manage an already-open position on the current bar: apply the max-hold timeout exit and the
     * adaptive stop-loss management (breakeven + trailing via {@link StopLossManager}). Take-profit
     * levels (TP1 partial, TP2) are handled by the broker-side protective orders, not here.
     *
     * @return {@code CLOSE} when the position reached its max hold time, or {@code HOLD} with an
     *     updated (trailed) stop-loss, otherwise {@code HOLD} unchanged.
     */
    private TradingDecision manageOpenPosition(
            String ticker,
            Position p,
            List<Candle> hourCandles,
            List<Candle> minuteCandles,
            Candle cur,
            Group grp) {
        int maxHold = Group.FX == grp ? config.maxCandlesHoldFx : config.maxCandlesHold;
        if (maxHold > 0 && p.candlesHeld >= maxHold) {
            return new TradingDecision(
                    "CLOSE",
                    "expired_" + p.candlesHeld,
                    0.0,
                    p.quantity,
                    null,
                    null,
                    cur.close,
                    null);
        }

        if (mmEnabled && stopLossManager != null && p.entryPrice != null) {
            double dAtr = atrVal(hourCandles, config.atrPeriod);
            Double initialRisk = initialRiskPerTicker.get(ticker);
            if (dAtr > 0 && initialRisk != null && initialRisk > 0) {
                StopLossManager.TrailingResult result = stopLossManager.updateStopLoss(
                        p, cur, dAtr, initialRisk, p.candlesHeld);
                if (result != null && result.newStopLoss != null) {
                    Double newStop = result.newStopLoss;
                    Double newTP = result.newTakeProfit != null ? result.newTakeProfit : p.takeProfit;
                    if (!Double.valueOf(newStop).equals(p.stopLoss) || !java.util.Objects.equals(newTP, p.takeProfit)) {
                        p =
                                copyPosition(
                                        p,
                                        p.direction,
                                        p.entryPrice,
                                        newStop,
                                        newTP,
                                        p.quantity,
                                        p.candlesHeld,
                                        p.cooldownRemaining);
                        String trailReason = result.trailingActivated ? "trail" : "breakeven";
                        return new TradingDecision("HOLD", trailReason, 0.0, 0, null, null, null, p);
                    }
                }
            }
        }

        return new TradingDecision("HOLD", "in_pos", 0.0, 0, null, null, null, p);
    }

    private int calculateMaxAffordableQuantity(
            String ticker, double balance, double entryPrice, int leverage) {
        if (entryPrice <= 0.0 || balance <= 0.0) {
            return 0;
        }

        TickerInfo tickerInfo = resolveTickerInfo(ticker);
        if (tickerInfo == null) {
            return (int) Math.floor(balance / entryPrice);
        }

        if (tradingService != null) {
            double cashBuffer =
                    Math.max(
                            MARKET_ORDER_CASH_BUFFER_MIN,
                            balance * MARKET_ORDER_CASH_BUFFER_PERCENT);
            double availableCash = Math.max(0.0, balance - cashBuffer);
            return tradingService.calculateTradeCount(
                    new TickerInfo.Key(ticker, tickerInfo.getType()), availableCash, entryPrice);
        }

        int lot = tickerInfo.getLot() != null ? tickerInfo.getLot() : 1;
        double orderCost = lot * entryPrice;
        double marginMultiplier = 1.0;
        if (TickerType.FEATURE == tickerInfo.getType()) {
            marginMultiplier = TradingService.FUTURES_MARGIN_RATE;
        }
        int effectiveLeverage = Math.max(1, leverage);
        if (effectiveLeverage > 1) {
            marginMultiplier /= effectiveLeverage;
        }
        orderCost *= marginMultiplier;
        if (orderCost <= 0.0) {
            return 0;
        }

        return (int) (Math.floor(balance / orderCost) * lot);
    }

    private int calculateAvailableLiquidity(String ticker, boolean isBuy, List<Candle> hourCandles) {
        if (isBacktestMode()) {
            if (hourCandles == null || hourCandles.isEmpty()) {
                return 0;
            }
            long sumVolume = 0;
            int n = Math.min(hourCandles.size(), 60);
            for (int i = 0; i < n; i++) {
                Candle c = hourCandles.get(hourCandles.size() - 1 - i);
                sumVolume += c.volume;
            }
            double avgHourlyVolume = sumVolume / (double) n;
            double fillable = avgHourlyVolume * 0.05;
            return fillable > Integer.MAX_VALUE ? Integer.MAX_VALUE : (int) fillable;
        }

        if (tradingService == null) {
            return Integer.MAX_VALUE;
        }

        String cacheKey = ticker + ":" + (isBuy ? "asks" : "bids");
        LiquidityCacheEntry cached = liquidityCache.get(cacheKey);
        if (cached != null && !cached.isExpired()) {
            return cached.liquidity;
        }

        TickerInfo tickerInfo = resolveTickerInfo(ticker);
        if (tickerInfo == null) {
            return 0;
        }

        String side = isBuy ? "asks" : "bids";
        try {
            Map<Double, Long> levels =
                    tradingService
                            .getCurrentPrices(
                                    new TickerInfo.Key(ticker, tickerInfo.getType()), false)
                            .get(side);
            if (levels == null || levels.isEmpty()) {
                return 0;
            }

            long sum = levels.values().stream().mapToLong(Long::longValue).sum();
            int result = sum > Integer.MAX_VALUE ? Integer.MAX_VALUE : (int) sum;
            liquidityCache.put(cacheKey, new LiquidityCacheEntry(result, System.currentTimeMillis()));
            return result;
        } catch (Exception ex) {
            log("Failed to read " + side + " for " + ticker + ": " + ex.getMessage());
            return 0;
        }
    }

    private TickerInfo resolveTickerInfo(String ticker) {
        TickerInfo.Key stockKey = new TickerInfo.Key(ticker, TickerType.STOCK);
        if (TickerRepository.INSTANCE.containsKey(stockKey)) {
            return TickerRepository.INSTANCE.getById(stockKey);
        }

        TickerInfo.Key etfKey = new TickerInfo.Key(ticker, TickerType.ETF);
        if (TickerRepository.INSTANCE.containsKey(etfKey)) {
            return TickerRepository.INSTANCE.getById(etfKey);
        }

        TickerInfo.Key futureKey = new TickerInfo.Key(ticker, TickerType.FEATURE);
        if (TickerRepository.INSTANCE.containsKey(futureKey)) {
            return TickerRepository.INSTANCE.getById(futureKey);
        }

        if (tradingService != null) {
            try {
                return tradingService.searchTicker(futureKey);
            } catch (Exception ignored) {
                try {
                    return tradingService.searchTicker(stockKey);
                } catch (Exception ignoredToo) {
                    try {
                        return tradingService.searchTicker(etfKey);
                    } catch (Exception ignoredThree) {
                        return null;
                    }
                }
            }
        }

        return new TickerInfo(
                null,
                ticker,
                null,
                null,
                DEFAULT_FUTURES_LOT,
                null,
                null,
                TickerType.FEATURE.name());
    }

    public String fxSignal(List<Candle> candles, List<Candle> minuteCandles) {
        if (candles.size() < 30) return null;

        double rsi = rsiVal(candles, config.rsiPeriod);
        String pat = candlePattern(minuteCandles);

        boolean extremeBuy = rsi <= config.rsiOversold;
        boolean extremeSell = rsi >= config.rsiOverbought;

        List<String> bullishPats =
                Arrays.asList("DOJI", "PIN_BAR_BUY", "ENGULFING_BUY", "MORNING_STAR");
        List<String> bearishPats =
                Arrays.asList("DOJI", "PIN_BAR_SELL", "ENGULFING_SELL", "EVENING_STAR");

        if (extremeBuy && bullishPats.contains(pat)) return "FXB_" + (int) rsi + "_" + pat;
        if (extremeSell && bearishPats.contains(pat)) return "FXS_" + (int) rsi + "_" + pat;

        return null;
    }

    public String mixedSignal(List<Candle> candles, List<Candle> minuteCandles) {
        if (candles.size() < 60) return null;

        Candle cur = candles.get(candles.size() - 1);
        double p = cur.close;
        double emaT = ema(candles, config.emaTrend);
        double emaF = ema(candles, config.emaFast);
        double emaS = ema(candles, config.emaSlow);
        double adx = adxVal(candles, config.adxPeriod);
        double rsi = rsiVal(candles, config.rsiPeriod);
        String pat = candlePattern(minuteCandles);

        boolean uptrend = p > emaT;
        boolean dnTrend = p < emaT;
        boolean emaUp = emaF > emaS;
        boolean emaDn = emaF < emaS;
        boolean trendOk = adx >= config.adxMin;

        List<String> patternUp =
                Arrays.asList("PIN_BAR_BUY", "ENGULFING_BUY", "MORNING_STAR", "THREE_WHITE");
        List<String> patternDn =
                Arrays.asList("PIN_BAR_SELL", "ENGULFING_SELL", "EVENING_STAR", "THREE_BLACK");
        boolean patUp = patternUp.contains(pat);
        boolean patDn = patternDn.contains(pat);

        int bs = 0, ss = 0;
        List<String> br = new ArrayList<>();
        List<String> sr = new ArrayList<>();

        if (uptrend) {
            bs++;
            br.add("TR");
        }
        if (dnTrend) {
            ss++;
            sr.add("TR");
        }

        if (trendOk) {
            bs++;
            ss++;
            br.add("AD" + (int) adx);
            sr.add("AD" + (int) adx);
        }

        if (emaUp) {
            bs++;
            br.add("EM");
        }
        if (emaDn) {
            ss++;
            sr.add("EM");
        }

        if (rsi >= 40.0 && rsi <= 68.0) {
            bs++;
            br.add("RS" + (int) rsi);
        }
        if (rsi >= 32.0 && rsi <= 58.0) {
            ss++;
            sr.add("RS" + (int) rsi);
        }

        if (patUp) {
            bs += 2;
            br.add(pat);
        }
        if (patDn) {
            ss += 2;
            sr.add(pat);
        }

        if (bs >= 4 && !patDn) return "MXB_" + String.join("_", br);
        if (ss >= 4 && !patUp) return "MXS_" + String.join("_", sr);

        return null;
    }

    public String candlePattern(List<Candle> candles) {
        if (candles == null || candles.size() < 3) return "NONE";

        Candle c = candles.get(candles.size() - 1);
        Candle p1 = candles.get(candles.size() - 2);
        Candle p2 = candles.get(candles.size() - 3);

        double body = Math.abs(c.close - c.open);
        double range = c.high - c.low;
        if (range <= 0.0) return "NONE";

        double upperShadow = c.high - Math.max(c.open, c.close);
        double lowerShadow = Math.min(c.open, c.close) - c.low;

        if (body < range * 0.15) return "DOJI";
        if (lowerShadow > body * 2 && upperShadow < body * 0.3 && c.close > c.open)
            return "PIN_BAR_BUY";
        if (upperShadow > body * 2 && lowerShadow < body * 0.3 && c.close < c.open)
            return "PIN_BAR_SELL";
        if (c.close > c.open && p1.close < p1.open && c.open < p1.close && c.close > p1.open)
            return "ENGULFING_BUY";
        if (c.close < c.open && p1.close > p1.open && c.open > p1.close && c.close < p1.open)
            return "ENGULFING_SELL";
        if (c.close > c.open && p1.close > p1.open && p2.close > p2.open) return "THREE_WHITE";
        if (c.close < c.open && p1.close < p1.open && p2.close < p2.open) return "THREE_BLACK";

        // MORNING STAR: 3-candle reversal pattern
        // p2: bearish candle, p1: small body (gap down preferred), c: bullish candle closing above
        // p1 midpoint
        double p1Body = Math.abs(p1.close - p1.open);
        double p2Body = Math.abs(p2.close - p2.open);
        if (c.close > c.open // 3rd candle: bullish
                && p2.close < p2.open // 1st candle: bearish
                && p1Body < p2Body * 0.5 // 2nd candle: small body (< 50% of 1st)
                && c.close > (p2.open + p2.close) / 2 // 3rd candle closes above midpoint of 1st
                && p1.close < p2.close) { // 2nd candle gaps down from 1st
            return "MORNING_STAR";
        }

        // EVENING STAR: 3-candle reversal pattern
        // p2: bullish candle, p1: small body (gap up preferred), c: bearish candle closing below p1
        // midpoint
        if (c.close < c.open // 3rd candle: bearish
                && p2.close > p2.open // 1st candle: bullish
                && p1Body < p2Body * 0.5 // 2nd candle: small body (< 50% of 1st)
                && c.close < (p2.open + p2.close) / 2 // 3rd candle closes below midpoint of 1st
                && p1.close > p2.close) { // 2nd candle gaps up from 1st
            return "EVENING_STAR";
        }

        return "NONE";
    }

    /** Reset daily MM limits (called at start of new trading day). */
    public void dailyReset() {
        if (mmEnabled) {
            if (riskManager != null) {
                riskManager.resetDailyLimits();
            }
            if (performanceTracker != null) {
                performanceTracker.resetSession();
            }
            if (killSwitch != null) {
                killSwitch.reset();
            }
            adaptiveCapital.reset();
            initialRiskPerTicker.clear();

            // Decay consecutive loss tracker: reduce by 1 instead of clearing
            // This prevents re-entry too quickly after a losing streak while
            // allowing gradual recovery
            consecutiveLossTracker.replaceAll((ticker, count) -> Math.max(0, count - 1));
            consecutiveLossTracker.entrySet().removeIf(e -> e.getValue() <= 0);

            log("MM: Daily reset completed");
        }
    }

    /** Register trade result with MM components (called on position close). */
    public void registerTradeResult(String ticker, double pnl) {
        if (mmEnabled) {
            if (riskManager != null) {
                riskManager.registerTrade(pnl);
            }
            if (performanceTracker != null) {
                performanceTracker.registerTrade(pnl);
                Double equity = tradingService != null ? tradingService.getTotalPortfolioValue() : null;
                if (equity > 0) {
                    performanceTracker.updateEquity(equity);
                }
            }
            if (adaptiveCapital != null) {
                if (pnl >= 0) {
                    adaptiveCapital.registerWin();
                } else {
                    adaptiveCapital.registerLoss();
                }
            }
            if (killSwitch != null && performanceTracker != null) {
                killSwitch.checkDrawdown(performanceTracker.getCurrentDrawdown());
            }
            initialRiskPerTicker.remove(ticker);

            // Track consecutive losses per ticker
            if (pnl < 0) {
                consecutiveLossTracker.merge(ticker, 1, Integer::sum);
            } else {
                consecutiveLossTracker.remove(ticker);
            }

            log(
                    "MM: Registered trade for "
                            + ticker
                            + ": PnL="
                            + String.format("%.2f", pnl)
                            + ", consecutiveLosses="
                            + (riskManager != null ? riskManager.getConsecutiveLosses() : 0));
        }
    }

    @Override
    public void onTradeClosed(
            String ticker,
            double pnl,
            double entryPrice,
            double exitPrice,
            int quantity,
            String direction) {
        registerTradeResult(ticker, pnl);
    }

    @Override
    public void onDailyReset() {
        dailyReset();

        int total = trendBars + rangeBars + normalBars;
        if (total > 0) {
            log("Regime stats: TREND=" + trendBars + "("
                    + (trendBars * 100 / total) + "%), RANGE=" + rangeBars + "("
                    + (rangeBars * 100 / total) + "%), NORMAL=" + normalBars + "("
                    + (normalBars * 100 / total) + "%)");
        }
        trendBars = 0;
        rangeBars = 0;
        normalBars = 0;
    }

    /**
     * Calculate ADX(14) for regime detection.
     * Migrated from RegimeAwareStrategy for unified regime-based filtering.
     */
    private double calculateAdxForRegime(List<Candle> candles, int period) {
        if (candles.size() < period * 2 + 10) {
            return 0.0;
        }

        int start = candles.size() - period;
        double trSum = 0.0, pdSum = 0.0, mdSum = 0.0;

        for (int i = start; i < candles.size(); i++) {
            Candle c = candles.get(i);
            Candle p = candles.get(i - 1);

            double tr = Math.max(
                    Math.max(c.high - c.low, Math.abs(c.high - p.close)),
                    Math.abs(c.low - p.close));
            trSum += tr;

            double up = c.high - p.high;
            double dn = p.low - c.low;

            pdSum += (up > dn && up > 0) ? up : 0.0;
            mdSum += (dn > up && dn > 0) ? dn : 0.0;
        }

        double atr = trSum / period;
        double diPlus = atr > 0 ? (pdSum / period) / atr * 100 : 0.0;
        double diMinus = atr > 0 ? (mdSum / period) / atr * 100 : 0.0;
        double adx = (diPlus + diMinus) > 0
                ? Math.abs(diPlus - diMinus) / (diPlus + diMinus) * 100
                : 0.0;

        return adx;
    }

    /** Market regime as detected by ADX thresholds. */
    private enum MarketRegime {
        TREND,
        RANGE,
        NORMAL,
        UNKNOWN
    }
}
