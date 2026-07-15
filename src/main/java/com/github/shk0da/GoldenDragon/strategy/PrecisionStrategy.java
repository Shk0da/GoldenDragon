package com.github.shk0da.goldendragon.strategy;

import com.github.shk0da.goldendragon.config.UnifiedTraderConfig;
import com.github.shk0da.goldendragon.model.Candle;
import com.github.shk0da.goldendragon.model.Config;
import com.github.shk0da.goldendragon.model.Position;
import com.github.shk0da.goldendragon.model.TradingDecision;
import com.github.shk0da.goldendragon.service.TCSService;
import java.util.List;

/**
 * Высокоточная (high-precision) торговая стратегия, нацеленная на низкую просадку, высокий winrate
 * и умеренную доходность.
 *
 * <p>Стратегия не вводит новую сигнальную логику с нуля, а комбинирует уже существующие проверенные
 * компоненты проекта, добавляя поверх них строгий селективный фильтр входа и консервативный
 * риск-профиль:
 *
 * <ol>
 *   <li><b>Базовый движок сигналов + ML.</b> Делегирует принятие решений {@link
 *       RegimeAwareStrategyMl}, который уже объединяет сигналы {@link UnifiedStrategy} (тренд / FX
 *       / mixed), определение рыночного режима по ADX, ML-фильтрацию вероятности выигрыша сделки и
 *       ML-сайзинг позиции, а также полный Money Management.
 *   <li><b>Confluence-гейт входа.</b> Новая позиция открывается только при подтверждённой связке
 *       «восходящий тренд + завершённый откат + сброс RSI + сильный ADX» (см. {@link
 *       #isTrendPullbackLong}). Вход на откате по тренду статистически даёт более высокий winrate,
 *       чем вход по импульсу/пробою, а жёсткое требование совпадения условий резко снижает частоту
 *       сделок в пользу их качества.
 *   <li><b>Консервативный риск-оверлей.</b> Внутреннему движку передаётся специально настроенный
 *       {@link Config} ({@link #buildConservativeConfig}) с пониженным риском на сделку,
 *       волатильностным сайзингом, ранним переходом в безубыток и жёстким лимитом критической
 *       просадки — это удерживает max drawdown в целевом коридоре.
 * </ol>
 *
 * <p>Управление уже открытой позицией (стоп-лосс, тейк-профит, трейлинг, выход по таймауту)
 * полностью делегируется внутреннему движку — confluence-гейт применяется только к новым входам.
 *
 * <p>ML-порог вероятности настраивается системным свойством {@code ml.probability.threshold}
 * (обрабатывается внутри {@link RegimeAwareStrategyMl}); per-ticker модели подхватываются из {@code
 * ml_strategy/models/trade_classifier_<TICKER>.txt}, при их отсутствии используется дефолтная
 * модель.
 *
 * @see RegimeAwareStrategyMl
 * @see UnifiedStrategy
 */
public class PrecisionStrategy extends BaseStrategy {

    private static final int MIN_HOUR_CANDLES = 60;
    private static final double MIN_ENTRY_ADX = 18.0;
    private static final double RSI_PULLBACK_MIN = 35.0;
    private static final double RSI_PULLBACK_MAX = 72.0;

    // Conservative risk overlay (targets low drawdown)
    private static final double RISK_PER_TRADE_PERCENT = 0.03;
    private static final double MAX_DAILY_LOSS_PERCENT = 0.04;
    private static final int MAX_CONSECUTIVE_LOSSES = 3;
    private static final double MAX_POSITION_SIZE = 0.9;
    private static final double ATR_STOP_MULTIPLIER = 2.0;
    private static final double VOLATILITY_BASE_ATR = 1.0;
    private static final double VOLATILITY_MIN_ADJUSTMENT = 0.5;
    private static final double VOLATILITY_MAX_ADJUSTMENT = 1.5;
    private static final double TRAILING_ACTIVATION_R = 1.0;
    private static final double TRAILING_MULTIPLIER = 1.0;
    private static final double BREAKEVEN_ACTIVATION_R = 0.5;
    private static final double BREAKEVEN_BUFFER = 0.001;
    private static final int LOSSES_TO_REDUCE = 2;
    private static final int WINS_TO_RESTORE = 4;
    private static final double RISK_REDUCTION_FACTOR = 0.5;
    private static final double CRITICAL_DRAWDOWN_PERCENT = 0.05;

    private final RegimeAwareStrategyMl delegate;

    public PrecisionStrategy(UnifiedTraderConfig unifiedTraderConfig, TCSService tcsService) {
        this(unifiedTraderConfig, tcsService, new Config(), false);
    }

    public PrecisionStrategy(
            UnifiedTraderConfig unifiedTraderConfig,
            TCSService tcsService,
            Config config,
            boolean isBacktest) {
        super(unifiedTraderConfig, tcsService, config, isBacktest);

        Config conservativeConfig = buildConservativeConfig();
        this.delegate =
                new RegimeAwareStrategyMl(
                        unifiedTraderConfig,
                        tcsService,
                        conservativeConfig,
                        isBacktest,
                        true,
                        true);
        this.delegate.setFixedEntryLeverage(1);

        logWithBacktest(
                "PrecisionStrategy: confluence trend-pullback gate (ADX>="
                        + MIN_ENTRY_ADX
                        + ", RSI["
                        + RSI_PULLBACK_MIN
                        + ".."
                        + RSI_PULLBACK_MAX
                        + "]) + ML filter + conservative risk (risk="
                        + (RISK_PER_TRADE_PERCENT * 100)
                        + "%, criticalDD="
                        + (CRITICAL_DRAWDOWN_PERCENT * 100)
                        + "%, maxPos="
                        + (MAX_POSITION_SIZE * 100)
                        + "%, leverage=1x fixed)");
    }

    @Override
    protected String getStrategyName() {
        return "PrecisionStrategy";
    }

    @Override
    public TradingDecision decide(
            String ticker,
            List<Candle> hourCandles,
            List<Candle> minuteCandles,
            Position position,
            double balance,
            boolean incrementCandlesHeld) {

        // Always delegate management of an open position (SL/TP/trailing/exit).
        if (position != null && position.quantity > 0) {
            return delegate.decide(
                    ticker, hourCandles, minuteCandles, position, balance, incrementCandlesHeld);
        }

        // New entries require a confirmed high-probability trend-pullback setup.
        if (!isTrendPullbackLong(hourCandles)) {
            return new TradingDecision("HOLD", "PRX_NO_SETUP");
        }

        TradingDecision decision =
                delegate.decide(
                        ticker,
                        hourCandles,
                        minuteCandles,
                        position,
                        balance,
                        incrementCandlesHeld);

        if ("OPEN".equals(decision.action)) {
            return new TradingDecision(
                    decision.action,
                    "PRX_" + decision.reason,
                    decision.confidence,
                    decision.quantity,
                    decision.stopLoss,
                    decision.takeProfit,
                    decision.entryPrice,
                    decision.updatedPosition);
        }

        return decision;
    }

    /**
     * Confluence-фильтр входа в лонг: восходящий тренд по EMA, завершённый откат к быстрой EMA,
     * сброс RSI в нейтральную зону и достаточно сильный ADX. Все условия должны выполняться
     * одновременно.
     */
    private boolean isTrendPullbackLong(List<Candle> hourCandles) {
        if (hourCandles == null || hourCandles.size() < MIN_HOUR_CANDLES) {
            return false;
        }

        Candle cur = hourCandles.get(hourCandles.size() - 1);
        Candle prev = hourCandles.get(hourCandles.size() - 2);
        double emaTrend = ema(hourCandles, config.emaTrend);
        double emaFast = ema(hourCandles, config.emaFast);
        double emaSlow = ema(hourCandles, config.emaSlow);
        double adx = adxVal(hourCandles, config.adxPeriod);
        double rsi = rsiVal(hourCandles, config.rsiPeriod);

        boolean uptrend = cur.close > emaTrend && emaFast > emaSlow;
        boolean strongTrend = adx >= MIN_ENTRY_ADX;
        boolean pullback = prev.close < emaFast || cur.low <= emaFast;
        boolean recovering = cur.close > cur.open && cur.close >= emaSlow;
        boolean rsiReset = rsi >= RSI_PULLBACK_MIN && rsi <= RSI_PULLBACK_MAX;

        return uptrend && strongTrend && pullback && recovering && rsiReset;
    }

    /**
     * Build a conservative Money Management configuration aimed at keeping drawdown low: small risk
     * per trade, volatility-based sizing, early breakeven and a tight critical drawdown
     * kill-switch.
     */
    private Config buildConservativeConfig() {
        return new Config(
                true,
                RISK_PER_TRADE_PERCENT,
                MAX_DAILY_LOSS_PERCENT,
                MAX_CONSECUTIVE_LOSSES,
                "VOLATILITY",
                VOLATILITY_BASE_ATR,
                VOLATILITY_MIN_ADJUSTMENT,
                VOLATILITY_MAX_ADJUSTMENT,
                ATR_STOP_MULTIPLIER,
                TRAILING_ACTIVATION_R,
                TRAILING_MULTIPLIER,
                BREAKEVEN_ACTIVATION_R,
                BREAKEVEN_BUFFER,
                true,
                LOSSES_TO_REDUCE,
                WINS_TO_RESTORE,
                RISK_REDUCTION_FACTOR,
                CRITICAL_DRAWDOWN_PERCENT,
                MAX_POSITION_SIZE,
                false);
    }

    @Override
    protected void onTradeClosed(
            String ticker,
            double pnl,
            double entryPrice,
            double exitPrice,
            int quantity,
            String direction) {
        delegate.onTradeClosed(ticker, pnl, entryPrice, exitPrice, quantity, direction);
    }

    @Override
    protected void onDailyReset() {
        delegate.onDailyReset();
    }
}
