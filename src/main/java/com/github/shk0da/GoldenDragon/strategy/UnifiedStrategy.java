package com.github.shk0da.GoldenDragon.strategy;

import com.github.shk0da.GoldenDragon.config.UnifiedTraderConfig;
import com.github.shk0da.GoldenDragon.filters.GroupConfirmationFilter;
import com.github.shk0da.GoldenDragon.filters.MarketRegimeFilter;
import com.github.shk0da.GoldenDragon.model.Candle;
import com.github.shk0da.GoldenDragon.model.Config;
import com.github.shk0da.GoldenDragon.model.Group;
import com.github.shk0da.GoldenDragon.model.Position;
import com.github.shk0da.GoldenDragon.model.TickerInfo;
import com.github.shk0da.GoldenDragon.model.TickerType;
import com.github.shk0da.GoldenDragon.model.TradingDecision;
import com.github.shk0da.GoldenDragon.money.AdaptiveCapital;
import com.github.shk0da.GoldenDragon.money.FixedRiskSizing;
import com.github.shk0da.GoldenDragon.money.KillSwitch;
import com.github.shk0da.GoldenDragon.money.PerformanceTracker;
import com.github.shk0da.GoldenDragon.money.PositionSizer;
import com.github.shk0da.GoldenDragon.money.RiskManager;
import com.github.shk0da.GoldenDragon.money.SizingStrategy;
import com.github.shk0da.GoldenDragon.money.StopLossManager;
import com.github.shk0da.GoldenDragon.money.VolatilityAdjustedSizing;
import com.github.shk0da.GoldenDragon.repository.TickerRepository;
import com.github.shk0da.GoldenDragon.service.TCSService;
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
 * Шорт-сигналы в текущей реализации отключены — открываются только BUY-позиции.
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

  // Money Management components
  private final RiskManager riskManager;
  private final PositionSizer positionSizer;
  private final StopLossManager stopLossManager;
  private final AdaptiveCapital adaptiveCapital;
  private final KillSwitch killSwitch;
  private final PerformanceTracker performanceTracker;
  private final boolean mmEnabled;

  // Track initial risk per position for R-based calculations
  private final ConcurrentMap<String, Double> initialRiskPerTicker = new ConcurrentHashMap<>();

  public UnifiedStrategy(UnifiedTraderConfig unifiedTraderConfig, TCSService tcsService) {
    this(unifiedTraderConfig, tcsService, new Config(), false);
  }

  public UnifiedStrategy(
      UnifiedTraderConfig unifiedTraderConfig,
      TCSService tcsService,
      Config config,
      boolean isBacktest) {
    super(unifiedTraderConfig, tcsService, config, isBacktest);

    this.mmEnabled = config.mmEnabled;

    if (mmEnabled) {
      // Initialize SizingStrategy
      SizingStrategy sizingStrategy;
      if ("VOLATILITY".equalsIgnoreCase(config.mmSizingStrategy)) {
        sizingStrategy =
            new VolatilityAdjustedSizing(
                config.mmRiskPercent,
                config.mmVolatilityBaseAtr,
                config.mmVolatilityMinAdjustment,
                config.mmVolatilityMaxAdjustment,
                0.25 // maxPositionSize: 25% of capital
                );
      } else {
        sizingStrategy =
            new FixedRiskSizing(
                config.mmRiskPercent, 0.25 // maxPositionSize: 25% of capital
                );
      }

      // Initialize MM components
      this.positionSizer = new PositionSizer(sizingStrategy);
      this.riskManager =
          new RiskManager(
              config.mmRiskPercent, config.mmMaxDailyLossPercent, config.mmMaxConsecutiveLosses);
      this.stopLossManager =
          new StopLossManager(
              config.mmAtrStopMultiplier,
              config.mmTrailingActivationR,
              config.mmTrailingMultiplier,
              config.mmBreakevenActivationR,
              config.mmBreakevenBuffer);
      this.adaptiveCapital =
          new AdaptiveCapital(
              config.mmRiskPercent,
              config.mmLossesToReduce,
              config.mmWinsToRestore,
              config.mmRiskReductionFactor);
      this.killSwitch = new KillSwitch(config.mmCriticalDrawdownPercent);
      this.performanceTracker = new PerformanceTracker();

      logWithBacktest(
          "Money Management initialized: risk="
              + (config.mmRiskPercent * 100)
              + "%, dailyLoss="
              + (config.mmMaxDailyLossPercent * 100)
              + "%, criticalDD="
              + (config.mmCriticalDrawdownPercent * 100)
              + "%");
    } else {
      this.positionSizer = null;
      this.riskManager = null;
      this.stopLossManager = null;
      this.adaptiveCapital = null;
      this.killSwitch = null;
      this.performanceTracker = null;
      logWithBacktest("Money Management disabled");
    }
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
    if (hourCandles == null
        || hourCandles.size() < 60
        || minuteCandles == null
        || minuteCandles.isEmpty()) {
      return new TradingDecision("HOLD", "init");
    }

    // Money Management: Check KillSwitch
    if (mmEnabled && killSwitch != null && !killSwitch.isTradingAllowed()) {
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
    if (mmEnabled && riskManager != null && !riskManager.canTrade(balance)) {
      return new TradingDecision(
          "HOLD",
          "RISK_LIMIT_"
              + riskManager.getConsecutiveLosses()
              + "_LOSS_"
              + (int) (riskManager.getDailyPnL() * 100)
              + "%");
    }

    Candle cur = minuteCandles.get(minuteCandles.size() - 1);
    Position p = position;
    Group grp = Group.valueOf(unifiedTraderConfig.getTickerGroup(ticker));

    if (position.quantity > 0 && incrementCandlesHeld) {
      p =
          new Position(
              position.direction,
              position.entryPrice,
              position.stopLoss,
              position.takeProfit,
              position.quantity,
              position.candlesHeld + 1,
              position.cooldownRemaining);
    } else if (position.quantity > 0) {
      p =
          new Position(
              position.direction,
              position.entryPrice,
              position.stopLoss,
              position.takeProfit,
              position.quantity,
              position.candlesHeld,
              position.cooldownRemaining);
    }

    if (p.quantity > 0) {
      Double sl = p.stopLoss;
      Double tp = p.takeProfit;
      String dir = p.direction;
      int maxH = grp == Group.FX ? config.maxCandlesHoldFx : config.maxCandlesHold;

      if (sl != null
          && (("BUY".equals(dir) && cur.low <= sl) || ("SELL".equals(dir) && cur.high >= sl))) {
        return new TradingDecision(
            "CLOSE",
            "stop_loss",
            0.0,
            p.quantity,
            null,
            null,
            sl,
            new Position(config.cooldownCandles));
      }

      if (tp != null
          && (("BUY".equals(dir) && cur.high >= tp) || ("SELL".equals(dir) && cur.low <= tp))) {
        return new TradingDecision(
            "CLOSE",
            "take_profit",
            0.0,
            p.quantity,
            null,
            null,
            tp,
            new Position(config.cooldownCandles));
      }

      if (p.candlesHeld >= maxH) {
        return new TradingDecision(
            "CLOSE",
            "expired",
            0.0,
            p.quantity,
            null,
            null,
            cur.close,
            new Position(config.cooldownCandles));
      }

      double ep = p.entryPrice != null ? p.entryPrice : cur.close;
      double pnlAbs = "BUY".equals(dir) ? cur.close - ep : ep - cur.close;
      double atr = atrVal(hourCandles, config.atrPeriod);
      // Money Management: Use StopLossManager if enabled
      if (mmEnabled && stopLossManager != null && atr > 0.0) {
        Double initialRisk = initialRiskPerTicker.get(ticker);
        if (initialRisk == null) {
          initialRisk = ep - (p.stopLoss != null ? p.stopLoss : ep);
          initialRiskPerTicker.put(ticker, initialRisk);
        }

        Double newStop = stopLossManager.updateStopLoss(p, cur, atr, initialRisk);
        if (newStop != null && newStop > (p.stopLoss != null ? p.stopLoss : 0.0)) {
          p =
              new Position(
                  p.direction,
                  p.entryPrice,
                  newStop,
                  p.takeProfit,
                  p.quantity,
                  p.candlesHeld,
                  p.cooldownRemaining);
          logWithBacktest(
              "MM: Updated stop loss for " + ticker + " to " + String.format("%.4f", newStop));
        }
      } else if (atr > 0.0 && pnlAbs > 0) {
        // Legacy trailing logic (fallback if MM disabled)
        double pnlAtr = pnlAbs / atr;
        double trMult = grp == Group.FX ? 0.7 : grp == Group.MIXED ? 0.9 : 1.0;

        if (pnlAtr >= 0.5 * trMult) {
          double beSl = "BUY".equals(dir) ? ep + atr * 0.08 : ep - atr * 0.08;
          if ("BUY".equals(dir) && (p.stopLoss != null ? p.stopLoss : 0.0) < beSl) {
            p =
                new Position(
                    p.direction,
                    p.entryPrice,
                    beSl,
                    p.takeProfit,
                    p.quantity,
                    p.candlesHeld,
                    p.cooldownRemaining);
          }
        }

        if (pnlAtr >= 1.0 * trMult) {
          double trailSl = cur.close - atr * 0.35;
          if ("BUY".equals(dir) && trailSl > (p.stopLoss != null ? p.stopLoss : 0.0)) {
            p =
                new Position(
                    p.direction,
                    p.entryPrice,
                    trailSl,
                    p.takeProfit,
                    p.quantity,
                    p.candlesHeld,
                    p.cooldownRemaining);
          }
        }

        if (pnlAtr >= 1.8 * trMult) {
          double tightTrail = cur.close - atr * 0.20;
          if ("BUY".equals(dir) && tightTrail > (p.stopLoss != null ? p.stopLoss : 0.0)) {
            p =
                new Position(
                    p.direction,
                    p.entryPrice,
                    tightTrail,
                    p.takeProfit,
                    p.quantity,
                    p.candlesHeld,
                    p.cooldownRemaining);
          }
        }
      }
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
              p.cooldownRemaining - 1));
    }

    if (p.quantity > 0) {
      return new TradingDecision("HOLD", "in_pos", 0.0, 0, null, null, null, p);
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

    boolean strongTrend = adx >= STRONG_TREND_ADX;
    boolean rangeRegime = adx > 0.0 && adx <= RANGE_ADX;

    boolean isBuy = signal.startsWith("TB") || signal.startsWith("FXB") || signal.startsWith("MXB");
    if (!isBuy) {
      return new TradingDecision("HOLD", "short_disabled", 0.0, 0, null, null, null, p);
    }

    String allocationGroup = tpCfg.allocationGroup;
    if (allocationGroup != null
        && !allocationGroup.isEmpty()
        && peerCandles != null
        && !peerCandles.isEmpty()) {
      if (!GroupConfirmationFilter.isConfirmed(ticker, true, peerCandles)) {
        return new TradingDecision("HOLD", "noGroupConf_" + signal, 0.0, 0, null, null, null, p);
      }
    }

    if (rsi > 72.0) {
      return new TradingDecision("HOLD", "rsi_hot", 0.0, 0, null, null, null, p);
    }

    double entry = cur.close;
    if (minuteCandles.size() >= 3) {
      Candle prev1 = minuteCandles.get(minuteCandles.size() - 2);
      Candle prev2 = minuteCandles.get(minuteCandles.size() - 3);

      boolean pullbackBuy = cur.close < prev1.close && cur.close > prev2.low;
      if (pullbackBuy) {
        entry = Math.min(cur.open, cur.close);
      }
    }

    double slMult = tpCfg.mmEnabled ? tpCfg.mmAtrStopMultiplier : tpCfg.slMult;
    double tpMult = tpCfg.tpMult;
    double riskP =
        tpCfg.mmEnabled && mmEnabled ? adaptiveCapital.getCurrentRiskPercent() : tpCfg.riskP;
    if (strongTrend) {
      riskP *= adx >= HOT_TREND_ADX ? 1.45 : 1.20;
    }
    if (rangeRegime) {
      riskP *= 0.65;
    }
    if (Group.FX == grp) {
      riskP *= 0.75;
    }
    riskP = Math.max(0.005, Math.min(riskP, 0.03));

    double slDist = dAtr * slMult;
    double tpDist = dAtr * tpMult;

    if (strongTrend) {
      slDist *= 1.10;
      tpDist *= adx >= HOT_TREND_ADX ? 1.35 : 1.20;
    }
    if (rangeRegime) {
      slDist *= 0.90;
      tpDist *= 0.85;
    }

    if (slDist <= 0.0 || tpDist <= 0.0) {
      return new TradingDecision("HOLD", "dist0", 0.0, 0, null, null, null, p);
    }

    double sl = entry - slDist;
    double tp = entry + tpDist;
    int maxAffordableQty = calculateMaxAffordableQuantity(ticker, balance, entry);
    int maxAskQty = calculateAvailableAskQuantity(ticker);

    if (maxAskQty <= 0) {
      return new TradingDecision("HOLD", "ASK_QTY0", 0.0, 0, null, null, null, p);
    }

    // Money Management: Use PositionSizer if enabled
    int qty;
    if (mmEnabled && positionSizer != null) {
      double riskMultiplier = adaptiveCapital.getRiskMultiplier();
      double adjustedBalance = balance * riskMultiplier;

      qty = positionSizer.calculateSize(ticker, entry, sl, adjustedBalance, dAtr);
      qty = Math.min(qty, Math.min(maxAffordableQty, maxAskQty));

      if (qty <= 0) {
        return new TradingDecision("HOLD", "MM_QTY_ZERO", 0.0, 0, null, null, null, p);
      }

    } else {
      // Legacy sizing (fallback if MM disabled)
      double confidenceK = Math.max(0.35, regimeResult.confidence / 100.0);

      double signalStrengthK = 1.0;
      if (signal.startsWith("TB_4")) signalStrengthK = 0.75;
      if (signal.startsWith("TB_5")) signalStrengthK = 0.90;
      if (signal.startsWith("TB_6")) signalStrengthK = 1.00;
      if (signal.startsWith("MX")) signalStrengthK = Math.max(signalStrengthK, 0.85);
      if (signal.startsWith("FX")) signalStrengthK = Math.max(signalStrengthK, 0.80);

      double finalRiskMultiplier = regimeResult.positionMultiplier * confidenceK * signalStrengthK;
      double maxRisk = balance * riskP * finalRiskMultiplier;

      double maxQty = Math.min(maxAffordableQty, maxAskQty);
      qty = (int) Math.min(Math.max(1, Math.floor(maxRisk / slDist)), maxQty);

      if (qty <= 0) {
        return new TradingDecision("HOLD", "qty0", 0.0, 0, null, null, null, p);
      }
    }

    double tradeConfidence =
        mmEnabled ? adaptiveCapital.getCurrentRiskPercent() / config.mmRiskPercent : 1.0;

    return new TradingDecision(
        "OPEN",
        signal,
        tradeConfidence,
        qty,
        sl,
        tp,
        entry,
        new Position("BUY", entry, sl, tp, qty, 0));
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
    boolean momentumUp = rsi >= 45.0 && rsi <= 68.0;
    boolean momentumDn = rsi >= 32.0 && rsi <= 55.0;

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

    if (bs >= 4 && uptrend) return "TB_" + bs + "_" + (int) adx + "_" + (int) rsi;
    if (ss >= 4 && dnTrend) return "TS_" + ss + "_" + (int) adx + "_" + (int) rsi;

    return null;
  }

  private int calculateMaxAffordableQuantity(String ticker, double balance, double entryPrice) {
    if (entryPrice <= 0.0 || balance <= 0.0) {
      return 0;
    }

    TickerInfo tickerInfo = resolveTickerInfo(ticker);
    if (tickerInfo == null) {
      return (int) Math.floor(balance / entryPrice);
    }

    if (tcsService != null) {
      double cashBuffer =
          Math.max(MARKET_ORDER_CASH_BUFFER_MIN, balance * MARKET_ORDER_CASH_BUFFER_PERCENT);
      double availableCash = Math.max(0.0, balance - cashBuffer);
      return tcsService.calculateTradeCount(
          new TickerInfo.Key(ticker, tickerInfo.getType()), availableCash, entryPrice);
    }

    int lot = tickerInfo.getLot() != null ? tickerInfo.getLot() : 1;
    double orderCost = lot * entryPrice;
    if (TickerType.FEATURE == tickerInfo.getType()) {
      orderCost *= TCSService.FUTURES_MARGIN_RATE;
    }
    if (orderCost <= 0.0) {
      return 0;
    }

    return (int) (Math.floor(balance / orderCost) * lot);
  }

  private int calculateAvailableAskQuantity(String ticker) {
    if (tcsService == null) {
      return Integer.MAX_VALUE;
    }

    TickerInfo tickerInfo = resolveTickerInfo(ticker);
    if (tickerInfo == null) {
      return 0;
    }

    try {
      Map<Double, Integer> asks =
          tcsService
              .getCurrentPrices(new TickerInfo.Key(ticker, tickerInfo.getType()), false)
              .get("asks");
      if (asks == null || asks.isEmpty()) {
        return 0;
      }

      return asks.values().stream().mapToInt(Integer::intValue).sum();
    } catch (Exception ex) {
      logWithBacktest("Failed to read asks for " + ticker + ": " + ex.getMessage());
      return 0;
    }
  }

  private TickerInfo resolveTickerInfo(String ticker) {
    TickerInfo.Key stockKey = new TickerInfo.Key(ticker, TickerType.STOCK);
    if (TickerRepository.INSTANCE.containsKey(stockKey)) {
      return TickerRepository.INSTANCE.getById(stockKey);
    }

    TickerInfo.Key futureKey = new TickerInfo.Key(ticker, TickerType.FEATURE);
    if (TickerRepository.INSTANCE.containsKey(futureKey)) {
      return TickerRepository.INSTANCE.getById(futureKey);
    }

    if (tcsService != null) {
      try {
        return tcsService.searchTicker(futureKey);
      } catch (Exception ignored) {
        try {
          return tcsService.searchTicker(stockKey);
        } catch (Exception ignoredToo) {
          return null;
        }
      }
    }

    return new TickerInfo(
        null, ticker, null, null, DEFAULT_FUTURES_LOT, null, null, TickerType.FEATURE.name());
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
    if (c.close > c.open && p1.close < p1.open && lowerShadow > body * 1.5) return "MORNING_STAR";
    if (c.close < c.open && p1.close > p1.open && upperShadow > body * 1.5) return "EVENING_STAR";

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
      logWithBacktest("MM: Daily reset completed");
    }
  }

  /** Register trade result with MM components (called on position close). */
  public void registerTradeResult(
      String ticker,
      double pnl,
      double entryPrice,
      double exitPrice,
      int quantity,
      String direction) {
    if (mmEnabled) {
      if (riskManager != null) {
        riskManager.registerTrade(pnl);
      }
      if (performanceTracker != null) {
        performanceTracker.registerTrade(pnl, ticker, direction, entryPrice, exitPrice);
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
      logWithBacktest(
          "MM: Registered trade for "
              + ticker
              + ": PnL="
              + String.format("%.2f", pnl)
              + ", consecutiveLosses="
              + (riskManager != null ? riskManager.getConsecutiveLosses() : 0));
    }
  }

  @Override
  protected void onTradeClosed(
      String ticker,
      double pnl,
      double entryPrice,
      double exitPrice,
      int quantity,
      String direction) {
    registerTradeResult(ticker, pnl, entryPrice, exitPrice, quantity, direction);
  }

  @Override
  protected void onDailyReset() {
    dailyReset();
  }
}
