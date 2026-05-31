package com.github.shk0da.GoldenDragon.strategy;

import com.github.shk0da.GoldenDragon.config.UnifiedTraderConfig;
import com.github.shk0da.GoldenDragon.ml.MlAutoTrainingService;
import com.github.shk0da.GoldenDragon.ml.MlPredictionService;
import com.github.shk0da.GoldenDragon.ml.TradeFeatures;
import com.github.shk0da.GoldenDragon.model.Candle;
import com.github.shk0da.GoldenDragon.model.Config;
import com.github.shk0da.GoldenDragon.model.Position;
import com.github.shk0da.GoldenDragon.model.TradingDecision;
import com.github.shk0da.GoldenDragon.service.TCSService;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * Regime-aware торговая стратегия (v5) с ML-фильтрацией и адаптивным сайзингом позиций.
 *
 * <p>Является обёрткой над {@link UnifiedStrategy}, добавляя два дополнительных слоя
 * принятия решений: определение рыночного режима на основе ADX и фильтрацию/масштабирование
 * сделок с помощью ML-модели классификации.</p>
 *
 * <h2>1. Определение рыночного режима</h2>
 * <p>На часовых свечах рассчитывается индикатор ADX(14), по которому рынок относится
 * к одному из режимов:</p>
 * <ul>
 *   <li>{@code TREND}  — ADX ≥ {@value #ADX_TREND_THRESHOLD} (сильный направленный тренд)</li>
 *   <li>{@code RANGE}  — ADX ≤ {@value #ADX_RANGE_THRESHOLD} (боковик / флэт)</li>
 *   <li>{@code NORMAL} — промежуточное состояние</li>
 * </ul>
 * <p>В зависимости от режима корректируется баланс, передаваемый в {@link UnifiedStrategy}
 * для расчёта размера позиции:</p>
 * <ul>
 *   <li>{@code TREND}  → balance × 1.5 (более агрессивный сайзинг)</li>
 *   <li>{@code RANGE}  → balance × 0.5 (более консервативный сайзинг)</li>
 *   <li>{@code NORMAL} → без изменений</li>
 * </ul>
 *
 * <h2>2. Базовое решение</h2>
 * <p>Основная логика входа/выхода делегируется {@link UnifiedStrategy#decide}.
 * ML-слой применяется <b>только к решениям {@code OPEN}</b>; сигналы {@code HOLD}/{@code CLOSE}
 * пропускаются без изменений.</p>
 *
 * <h2>3. ML-фильтрация сделок</h2>
 * <p>Для каждого потенциального входа формируется {@link TradeFeatures} с большим набором
 * признаков: рыночные индикаторы (ADX, DI+/DI−, ATR/ATR-ratio, RSI, EMA9/21 и их отношение),
 * контекст (позиция цены в диапазоне, volume ratio/trend), параметры сделки (confidence,
 * risk/reward, дистанция стопа), режим, тип исходного сигнала (TB/FX/MX), наличие group
 * confirmation, а также временные признаки (час, день недели).</p>
 *
 * <p>Модель {@code trade_classifier} возвращает вероятность выигрыша сделки.
 * Порог фильтрации адаптируется под режим:</p>
 * <ul>
 *   <li>{@code TREND}  → базовый порог (по умолчанию {@value #DEFAULT_ML_MIN_PROBABILITY},
 *       переопределяется через системное свойство {@code ml.probability.threshold})</li>
 *   <li>{@code NORMAL} → max(базовый, 0.48)</li>
 *   <li>{@code RANGE}  → max(базовый, 0.52)</li>
 * </ul>
 * <p>Сделки с вероятностью ниже порога отклоняются с действием {@code HOLD} и причиной
 * {@code ML_FILTERED_PROB_<значение>}.</p>
 *
 * <h2>4. ML-сайзинг позиции</h2>
 * <p>Если сделка прошла фильтр, итоговый объём корректируется коэффициентом от модели
 * с дополнительными поправками по режиму и уровню уверенности:</p>
 * <ul>
 *   <li>{@code RANGE} → ×0.75</li>
 *   <li>{@code TREND} и prob ≥ 0.54 → ×1.15</li>
 *   <li>prob ≥ 0.56 → дополнительно ×1.15</li>
 *   <li>prob ≥ 0.60 → дополнительно ×1.10</li>
 * </ul>
 * <p>На основе нового {@code quantity} пересобирается {@link TradingDecision} и {@link Position}.</p>
 *
 * <h2>5. Автодообучение модели</h2>
 * <p>Вне режима бэктеста при каждом вызове {@link #decide} выполняется попытка переобучения
 * модели через {@link MlAutoTrainingService#tryRetrain}, использующий накопленные сделки
 * из {@code ml_strategy/data_pipeline/trades.csv}.</p>
 *
 * <h2>6. Логирование и статистика</h2>
 * <p>В {@link #onDailyReset()} выводится распределение режимов (Trend/Range/Normal),
 * количество отфильтрованных ML сделок и сводная статистика вероятностей: min/avg/max
 * и распределение по уровням (≥0.10, 0.20, 0.30, 0.40, 0.50).</p>
 *
 * <h2>7. Обогащение причины сделки</h2>
 * <p>К полю {@code reason} итогового решения добавляется контекст вида
 * {@code <REGIME>_ADX<значение>[_ML<вероятность>]_<исходная_причина>}, что упрощает
 * последующий разбор логов и пост-анализ результатов.</p>
 *
 * @see UnifiedStrategy
 * @see MlPredictionService
 * @see MlAutoTrainingService
 * @see TradeFeatures
 */
public class RegimeAwareStrategyMl extends BaseStrategy {

    private static final double ADX_TREND_THRESHOLD = 30.0;
    private static final double ADX_RANGE_THRESHOLD = 15.0;
    private static final double DEFAULT_ML_MIN_PROBABILITY = 0.54;
    
    private final UnifiedStrategy unifiedStrategy;
    private final MlPredictionService mlService;
    private final MlAutoTrainingService mlAutoTrainingService;
    private final boolean useMlFiltering;
    private final boolean useMlSizing;
    
    private int trendBars = 0;
    private int rangeBars = 0;
    private int normalBars = 0;
    
    // ML statistics
    private int mlFilteredTrades = 0;
    private int mlApprovedTrades = 0;
    private int mlScoredTrades = 0;
    private double minProbability = Double.POSITIVE_INFINITY;
    private double maxProbability = Double.NEGATIVE_INFINITY;
    private double totalProbability = 0.0;
    private int above10 = 0;
    private int above20 = 0;
    private int above30 = 0;
    private int above40 = 0;
    private int above50 = 0;
    private enum MarketRegime {
        TREND, RANGE, NORMAL, UNKNOWN
    }

    public RegimeAwareStrategyMl(UnifiedTraderConfig unifiedTraderConfig, TCSService tcsService) {
        this(unifiedTraderConfig, tcsService, new Config(), false, true, true);
    }

    public RegimeAwareStrategyMl(UnifiedTraderConfig unifiedTraderConfig, TCSService tcsService, Config config, 
                                  boolean isBacktest, boolean useMlFiltering, boolean useMlSizing) {
        super(unifiedTraderConfig, tcsService, config, isBacktest);
        
        this.unifiedStrategy = new UnifiedStrategy(unifiedTraderConfig, tcsService, config, isBacktest);
        this.mlService = new MlPredictionService("ml_strategy/models/trade_classifier_v2.txt");
        this.mlService.setProbabilityThreshold(resolveMlProbabilityThreshold());
        this.mlAutoTrainingService = new MlAutoTrainingService(
                "ml_strategy/data_pipeline/trades.csv",
                "ml_strategy/models/trade_classifier_v2.txt",
                "ml_strategy"
        );
        this.useMlFiltering = useMlFiltering;
        this.useMlSizing = useMlSizing;
        
        logWithBacktest("RegimeAwareStrategy ML v5: ML_Filter=" + useMlFiltering + 
                        ", ML_Sizing=" + useMlSizing);
    }

    private double resolveMlProbabilityThreshold() {
        String configured = System.getProperty("ml.probability.threshold");
        if (configured == null || configured.isBlank()) {
            return DEFAULT_ML_MIN_PROBABILITY;
        }

        try {
            return Double.parseDouble(configured);
        } catch (NumberFormatException ignored) {
            return DEFAULT_ML_MIN_PROBABILITY;
        }
    }

    private double resolveRegimeProbabilityThreshold(MarketRegime regime) {
        double baseThreshold = mlService.getProbabilityThreshold();
        if (MarketRegime.RANGE == regime) {
            return Math.max(baseThreshold, 0.52);
        }
        if (MarketRegime.NORMAL == regime) {
            return Math.max(baseThreshold, 0.48);
        }
        return baseThreshold;
    }

    @Override
    protected String getStrategyName() {
        return "RegimeAwareStrategyMl";
    }

    @Override
    public TradingDecision decide(String ticker,
                                   List<Candle> hourCandles,
                                   List<Candle> minuteCandles,
                                   Position position,
                                   double balance,
                                   boolean incrementCandlesHeld) {
        
        // Detect market regime
        if (!isBacktest) {
            mlAutoTrainingService.tryRetrain(mlService);
        }

        MarketRegime regime = MarketRegime.UNKNOWN;
        double adx = 0.0;
        
        if (hourCandles != null && hourCandles.size() >= 60) {
            adx = calculateAdx(hourCandles, 14);
            
            if (adx >= ADX_TREND_THRESHOLD) {
                regime = MarketRegime.TREND;
                trendBars++;
            } else if (adx <= ADX_RANGE_THRESHOLD) {
                regime = MarketRegime.RANGE;
                rangeBars++;
            } else {
                regime = MarketRegime.NORMAL;
                normalBars++;
            }
        }
        
        // Get base decision from UnifiedStrategy
        double adjustedBalance = balance;
        if (regime == MarketRegime.TREND) {
            adjustedBalance = balance * 1.5;
        } else if (regime == MarketRegime.RANGE) {
            adjustedBalance = balance * 0.5;
        }
        
        TradingDecision decision = unifiedStrategy.decide(
                ticker, hourCandles, minuteCandles, position, adjustedBalance, incrementCandlesHeld
        );
        
        // Only apply ML to OPEN decisions
        if (!"OPEN".equals(decision.action) || decision.updatedPosition == null) {
            return wrapDecision(decision, regime, adx, 0.0);
        }
        
        // Create ML features
        TradeFeatures features = createTradeFeatures(
                ticker, hourCandles, decision, regime, adx
        );
        
        if (features == null) {
            return wrapDecision(decision, regime, adx, 0.0);
        }
        
        // Get ML prediction
        double winProbability = mlService.predictProbability(features);
        trackProbability(winProbability);
        // Apply ML filtering
        double regimeThreshold = resolveRegimeProbabilityThreshold(regime);
        if (useMlFiltering && winProbability < regimeThreshold) {
            mlFilteredTrades++;
            return new TradingDecision("HOLD", "ML_FILTERED_PROB_" + String.format("%.2f", winProbability));
        }
        
        mlApprovedTrades++;
        
        // Apply ML position sizing
        if (useMlSizing) {
            double sizeMultiplier = mlService.getPositionSizeMultiplier(features);
            if (regime == MarketRegime.RANGE) {
                sizeMultiplier *= 0.75;
            } else if (regime == MarketRegime.TREND && winProbability >= 0.54) {
                sizeMultiplier *= 1.15;
            }
            if (winProbability >= 0.56) {
                sizeMultiplier *= 1.15;
            }
            if (winProbability >= 0.60) {
                sizeMultiplier *= 1.10;
            }
            int adjustedQty = (int)(decision.quantity * sizeMultiplier);
            
            if (adjustedQty < decision.quantity) {
                decision = new TradingDecision(
                        decision.action,
                        decision.reason,
                        decision.confidence,
                        adjustedQty,
                        decision.stopLoss,
                        decision.takeProfit,
                        decision.entryPrice,
                        new Position(
                                decision.updatedPosition.direction,
                                decision.updatedPosition.entryPrice,
                                decision.updatedPosition.stopLoss,
                                decision.updatedPosition.takeProfit,
                                adjustedQty,
                                decision.updatedPosition.candlesHeld,
                                decision.updatedPosition.cooldownRemaining
                        )
                );
            } else if (adjustedQty > decision.quantity) {
                decision = new TradingDecision(
                        decision.action,
                        decision.reason,
                        decision.confidence,
                        adjustedQty,
                        decision.stopLoss,
                        decision.takeProfit,
                        decision.entryPrice,
                        new Position(
                                decision.updatedPosition.direction,
                                decision.updatedPosition.entryPrice,
                                decision.updatedPosition.stopLoss,
                                decision.updatedPosition.takeProfit,
                                adjustedQty,
                                decision.updatedPosition.candlesHeld,
                                decision.updatedPosition.cooldownRemaining
                        )
                );
            }
        }
        
        return wrapDecision(decision, regime, adx, winProbability);
    }

    private void trackProbability(double winProbability) {
        mlScoredTrades++;
        totalProbability += winProbability;
        minProbability = Math.min(minProbability, winProbability);
        maxProbability = Math.max(maxProbability, winProbability);

        if (winProbability >= 0.10) above10++;
        if (winProbability >= 0.20) above20++;
        if (winProbability >= 0.30) above30++;
        if (winProbability >= 0.40) above40++;
        if (winProbability >= 0.50) above50++;
    }

    private TradeFeatures createTradeFeatures(String ticker, List<Candle> candles, 
                                               TradingDecision decision, MarketRegime regime, double adx) {
        if (candles == null || candles.size() < 30) return null;
        if (decision.updatedPosition == null) return null;
        
        double rsi = calculateRsi(candles, 14);
        double emaFast = calculateEma(candles, 9);
        double emaSlow = calculateEma(candles, 21);
        double atr = calculateAtr(candles, 14);
        double atrAvg = calculateAverageAtr(candles, 14, 20);
        double[] diValues = calculateDirectionalIndicators(candles, 14);
        double pricePosition = calculatePricePosition(candles, entryPrice(decision));
        double volumeRatio = calculateVolumeRatio(candles, 20);
        double volumeTrend = calculateVolumeTrend(candles, 5);
        LocalDateTime entryTime = parseCandleTime(candles.get(candles.size() - 1).time);

        double entryPrice = entryPrice(decision);
        double stopLoss = decision.updatedPosition.stopLoss != null ? 
                          decision.updatedPosition.stopLoss : entryPrice * 0.98;
        double takeProfit = decision.updatedPosition.takeProfit != null ? 
                           decision.updatedPosition.takeProfit : entryPrice * 1.06;
        
        double riskReward = Math.abs(takeProfit - entryPrice) / Math.max(Math.abs(entryPrice - stopLoss), 0.0001);
        double stopDistance = Math.abs(entryPrice - stopLoss) / entryPrice * 100.0;
        
        return new TradeFeatures(
                adx, diValues[0], diValues[1], atr, atr / Math.max(atrAvg, 0.0001),
                rsi, emaFast, emaSlow, emaFast / Math.max(emaSlow, 0.01), pricePosition,
                volumeRatio, volumeTrend,
                decision.confidence, riskReward, stopDistance, regime.toString(),
                extractSignalStrength(decision.reason),
                decision.reason != null && decision.reason.startsWith("TB") ? 1.0 : 0.0,
                decision.reason != null && decision.reason.startsWith("FX") ? 1.0 : 0.0,
                decision.reason != null && decision.reason.startsWith("MX") ? 1.0 : 0.0,
                decision.reason != null && decision.reason.contains("noGroupConf") ? 0.0 : 1.0,
                adx >= ADX_TREND_THRESHOLD ? 1.0 : 0.0,
                adx <= ADX_RANGE_THRESHOLD ? 1.0 : 0.0,
                entryTime.getHour(), entryTime.getDayOfWeek().getValue(),
                ticker, getStrategyName(), entryTime, entryPrice
        );
    }

    private double extractSignalStrength(String reason) {
        if (reason == null || reason.isEmpty()) {
            return 0.0;
        }

        String[] parts = reason.split("_");
        if (parts.length < 2) {
            return 0.0;
        }

        try {
            return Double.parseDouble(parts[1]);
        } catch (NumberFormatException ex) {
            return 0.0;
        }
    }

    private double entryPrice(TradingDecision decision) {
        return decision.updatedPosition.entryPrice;
    }

    private LocalDateTime parseCandleTime(String time) {
        try {
            Date date = CANDLE_TIME_FORMAT.get().parse(time);
            return LocalDateTime.ofInstant(date.toInstant(), ZoneId.systemDefault());
        } catch (Exception ex) {
            return LocalDateTime.now();
        }
    }

    private double[] calculateDirectionalIndicators(List<Candle> candles, int period) {
        if (candles.size() < period + 1) {
            return new double[]{50.0, 50.0};
        }

        int start = candles.size() - period;
        double trSum = 0.0;
        double plusDmSum = 0.0;
        double minusDmSum = 0.0;

        for (int i = start; i < candles.size(); i++) {
            Candle current = candles.get(i);
            Candle previous = candles.get(i - 1);

            double trueRange = Math.max(
                    Math.max(current.high - current.low, Math.abs(current.high - previous.close)),
                    Math.abs(current.low - previous.close)
            );
            trSum += trueRange;

            double upMove = current.high - previous.high;
            double downMove = previous.low - current.low;
            plusDmSum += upMove > downMove && upMove > 0.0 ? upMove : 0.0;
            minusDmSum += downMove > upMove && downMove > 0.0 ? downMove : 0.0;
        }

        if (0.0 == trSum) {
            return new double[]{50.0, 50.0};
        }

        double diPlus = plusDmSum / trSum * 100.0;
        double diMinus = minusDmSum / trSum * 100.0;
        return new double[]{diPlus, diMinus};
    }

    private double calculateAverageAtr(List<Candle> candles, int atrPeriod, int lookback) {
        if (candles.size() < atrPeriod + 1) {
            return 1.0;
        }

        List<Double> atrValues = new ArrayList<>();
        int start = Math.max(atrPeriod + 1, candles.size() - lookback);
        for (int i = start; i <= candles.size(); i++) {
            atrValues.add(calculateAtr(candles.subList(0, i), atrPeriod));
        }

        double sum = 0.0;
        for (Double value : atrValues) {
            sum += value;
        }
        return atrValues.isEmpty() ? 1.0 : sum / atrValues.size();
    }

    private double calculatePricePosition(List<Candle> candles, double entryPrice) {
        int lookback = Math.min(20, candles.size());
        double highest = Double.NEGATIVE_INFINITY;
        double lowest = Double.POSITIVE_INFINITY;

        for (int i = candles.size() - lookback; i < candles.size(); i++) {
            Candle candle = candles.get(i);
            highest = Math.max(highest, candle.high);
            lowest = Math.min(lowest, candle.low);
        }

        double range = highest - lowest;
        if (range <= 0.0) {
            return 0.5;
        }
        return Math.max(0.0, Math.min(1.0, (entryPrice - lowest) / range));
    }

    private double calculateVolumeRatio(List<Candle> candles, int lookback) {
        int size = candles.size();
        if (size < 2) {
            return 1.0;
        }

        int start = Math.max(0, size - lookback);
        double avgVolume = 0.0;
        for (int i = start; i < size; i++) {
            avgVolume += candles.get(i).volume;
        }
        avgVolume /= (size - start);
        return avgVolume > 0.0 ? candles.get(size - 1).volume / avgVolume : 1.0;
    }

    private double calculateVolumeTrend(List<Candle> candles, int lookback) {
        int size = candles.size();
        if (size <= lookback) {
            return 0.0;
        }

        double recent = 0.0;
        double previous = 0.0;
        for (int i = size - lookback; i < size; i++) {
            recent += candles.get(i).volume;
            previous += candles.get(i - lookback).volume;
        }

        if (0.0 == previous) {
            return 0.0;
        }
        return (recent - previous) / previous;
    }
    
    private TradingDecision wrapDecision(TradingDecision decision, MarketRegime regime, 
                                          double adx, double mlProb) {
        if (decision == null) return decision;
        
        String newReason = regime + "_ADX" + (int)adx;
        if (mlProb > 0) {
            newReason += "_ML" + String.format("%.2f", mlProb);
        }
        newReason += "_" + decision.reason;
        
        return new TradingDecision(
                decision.action,
                newReason,
                decision.confidence,
                decision.quantity,
                decision.stopLoss,
                decision.takeProfit,
                decision.entryPrice,
                decision.updatedPosition
        );
    }

    private double calculateAdx(List<Candle> candles, int period) {
        if (candles.size() < period * 2 + 10) return 0.0;
        
        int start = candles.size() - period;
        double trSum = 0.0, pdSum = 0.0, mdSum = 0.0;
        
        for (int i = start; i < candles.size(); i++) {
            Candle c = candles.get(i);
            Candle p = candles.get(i - 1);
            
            double tr = Math.max(Math.max(c.high - c.low, Math.abs(c.high - p.close)), Math.abs(c.low - p.close));
            trSum += tr;
            
            double up = c.high - p.high;
            double dn = p.low - c.low;
            
            pdSum += (up > dn && up > 0) ? up : 0.0;
            mdSum += (dn > up && dn > 0) ? dn : 0.0;
        }
        
        double atr = trSum / period;
        double diPlus = atr > 0 ? (pdSum / period) / atr * 100 : 0.0;
        double diMinus = atr > 0 ? (mdSum / period) / atr * 100 : 0.0;
        double adx = (diPlus + diMinus) > 0 ? Math.abs(diPlus - diMinus) / (diPlus + diMinus) * 100 : 0.0;
        
        return adx;
    }
    
    private double calculateRsi(List<Candle> candles, int period) {
        if (candles.size() < period + 1) return 50.0;
        
        double gains = 0.0, losses = 0.0;
        for (int i = candles.size() - period; i < candles.size(); i++) {
            double change = candles.get(i).close - candles.get(i - 1).close;
            if (change > 0) gains += change;
            else losses -= change;
        }
        
        double rs = losses == 0 ? 100 : gains / losses;
        return 100.0 - (100.0 / (1.0 + rs));
    }
    
    private double calculateEma(List<Candle> candles, int period) {
        if (candles.size() < period) return candles.get(candles.size() - 1).close;
        
        double multiplier = 2.0 / (period + 1);
        double ema = candles.get(0).close;
        
        for (int i = 1; i < candles.size(); i++) {
            ema = (candles.get(i).close - ema) * multiplier + ema;
        }
        
        return ema;
    }
    
    private double calculateAtr(List<Candle> candles, int period) {
        if (candles.size() < period + 1) return 0.0;
        double sum = 0.0;
        for (int i = candles.size() - period; i < candles.size(); i++) {
            Candle c = candles.get(i);
            Candle p = candles.get(i - 1);
            sum += Math.max(Math.max(c.high - c.low, Math.abs(c.high - p.close)), Math.abs(c.low - p.close));
        }
        return sum / period;
    }

    @Override
    protected void onDailyReset() {
        unifiedStrategy.onDailyReset();
        
        int total = trendBars + rangeBars + normalBars;
        if (total > 0 && mlApprovedTrades + mlFilteredTrades > 0) {
            double filterRate = mlFilteredTrades * 100.0 / (mlApprovedTrades + mlFilteredTrades);
            logWithBacktest("RegimeAwareStrategy ML: Trend:" + trendBars + "(" + (trendBars*100/total) + 
                            "%), Range:" + rangeBars + "(" + (rangeBars*100/total) + 
                            "%), Normal:" + normalBars + "(" + (normalBars*100/total) + 
                            "%), ML_Filtered:" + mlFilteredTrades + "(" + String.format("%.1f", filterRate) + "%)");
        }

        if (mlScoredTrades > 0) {
            double avgProbability = totalProbability / mlScoredTrades;
            String probabilitySummary = "RegimeAwareStrategy ML Probabilities: threshold=" + String.format("%.2f", mlService.getProbabilityThreshold()) +
                    ", scored=" + mlScoredTrades +
                    ", min=" + String.format("%.4f", minProbability) +
                    ", avg=" + String.format("%.4f", avgProbability) +
                    ", max=" + String.format("%.4f", maxProbability) +
                    ", >=0.10=" + above10 +
                    ", >=0.20=" + above20 +
                    ", >=0.30=" + above30 +
                    ", >=0.40=" + above40 +
                    ", >=0.50=" + above50;
            if (!isBacktest) {
                log(probabilitySummary);
            }
        }
    }

    @Override
    protected void onTradeClosed(String ticker, double pnl, double entryPrice,
                                  double exitPrice, int quantity, String direction) {
        unifiedStrategy.onTradeClosed(ticker, pnl, entryPrice, exitPrice, quantity, direction);
    }

    @Override
    protected void closeAllPositions(TCSService tcsService, UnifiedTraderConfig config) {
        unifiedStrategy.closeAllPositions(tcsService, config);
    }
}
