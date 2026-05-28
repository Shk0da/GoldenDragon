package com.github.shk0da.GoldenDragon.strategy;

import com.github.shk0da.GoldenDragon.config.UnifiedTraderConfig;
import com.github.shk0da.GoldenDragon.filters.BadWeatherFilter;
import com.github.shk0da.GoldenDragon.filters.GroupConfirmationFilter;
import com.github.shk0da.GoldenDragon.filters.MarketRegimeFilter;
import com.github.shk0da.GoldenDragon.model.Candle;
import com.github.shk0da.GoldenDragon.model.Config;
import com.github.shk0da.GoldenDragon.model.Position;
import com.github.shk0da.GoldenDragon.model.TradingDecision;
import com.github.shk0da.GoldenDragon.service.TCSService;
import java.util.ArrayList;
import java.util.List;


import static java.lang.Math.floor;
import static java.lang.Math.max;
import static java.lang.Math.min;

public class HighWinRateUnifiedStrategy extends BaseStrategy {

    private final BadWeatherFilter badWeatherFilter;
    private final MarketRegimeFilter marketRegimeFilter;

    private static final int ATR_PERIOD = 7;
    private static final int EMA_FAST = 6;
    private static final int EMA_SLOW = 14;
    private static final int RSI_PERIOD = 7;
    private static final int VOLUME_PERIOD = 20;
    private static final double VOLUME_MULTIPLIER = 1.2;
    private static final double SL_MULTIPLIER = 2.5;
    private static final double TP_MULTIPLIER = 7.0;
    private static final int MAX_CANDLES_HOLD = 12;
    private static final double RISK_PER_TRADE = 0.02;
    private static final double MAX_POSITION_SIZE = 0.30;
    private static final double CONFIDENCE_THRESHOLD = 0.65;
    private static final int COOLDOWN_CANDLES = 2;
    private static final double ADX_MIN = 18.0;

    public HighWinRateUnifiedStrategy(UnifiedTraderConfig unifiedTraderConfig, TCSService tcsService) {
        this(unifiedTraderConfig, tcsService, new Config());
    }

    public HighWinRateUnifiedStrategy(UnifiedTraderConfig unifiedTraderConfig, TCSService tcsService, Config config) {
        super(unifiedTraderConfig, tcsService, config);
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

    @Override
    protected String getStrategyName() {
        return "HighWinRateUnifiedStrategy";
    }

    @Override
    protected Position getCooldownPosition() {
        return new Position(COOLDOWN_CANDLES);
    }

    @Override
    public TradingDecision decide(String ticker,
                                  List<Candle> hourCandles,
                                  List<Candle> minuteCandles,
                                  Position position,
                                  double balance,
                                  boolean incrementCandlesHeld) {
        if (hourCandles == null || hourCandles.size() < EMA_SLOW + 5) {
            return new TradingDecision("HOLD", "init");
        }

        UnifiedTraderConfig.TickerParams tpCfg = unifiedTraderConfig.getTickerParams(ticker);
        if (!tpCfg.enabled) {
            return new TradingDecision("HOLD", "ticker_disabled");
        }

        Candle cur = hourCandles.get(hourCandles.size() - 1);
        Position p = position;

        if (position.quantity > 0) {
            Double sl = position.stopLoss;
            Double tp = position.takeProfit;
            String dir = position.direction;

            if (sl != null && "BUY".equals(dir) && cur.close <= sl) {
                return new TradingDecision("CLOSE", "stop_loss", 0.0, position.quantity,
                        null, null, sl, new Position(COOLDOWN_CANDLES));
            }

            if (tp != null && "BUY".equals(dir) && cur.high >= tp) {
                return new TradingDecision("CLOSE", "take_profit", 0.0, position.quantity,
                        null, null, tp, new Position(COOLDOWN_CANDLES));
            }

            if (position.candlesHeld >= MAX_CANDLES_HOLD) {
                return new TradingDecision("CLOSE", "expired", 0.0, position.quantity,
                        null, null, cur.close, new Position(COOLDOWN_CANDLES));
            }

            p = new Position(
                    position.direction,
                    position.entryPrice,
                    position.stopLoss,
                    position.takeProfit,
                    position.quantity,
                    incrementCandlesHeld ? position.candlesHeld + 1 : position.candlesHeld,
                    position.cooldownRemaining
            );

            double ep = position.entryPrice != null ? position.entryPrice : cur.close;
            double pnlPct = (cur.close - ep) / ep;

            if (pnlPct > 0.002) {
                double atr = atrVal(hourCandles, ATR_PERIOD);
                if (atr > 0.0) {
                    double newSl = cur.close - atr * 0.4;
                    if (newSl > (sl != null ? sl : 0.0)) {
                        p = new Position(p.direction, p.entryPrice, newSl, p.takeProfit,
                                p.quantity, p.candlesHeld, p.cooldownRemaining);
                    }
                }
            }
        }

        if (p.cooldownRemaining > 0) {
            return new TradingDecision("HOLD", "CD" + p.cooldownRemaining,
                    0.0, 0, null, null, null,
                    new Position(p.direction, p.entryPrice, p.stopLoss, p.takeProfit,
                            p.quantity, p.candlesHeld, p.cooldownRemaining - 1));
        }

        if (p.quantity > 0) {
            return new TradingDecision("HOLD", "in_pos", 0.0, 0, null, null, null, p);
        }

        if (!badWeatherFilter.canTrade(hourCandles, cur.close, tpCfg.badWeatherParams)) {
            String reason = badWeatherFilter.getBlockReason(hourCandles, cur.close, tpCfg.badWeatherParams);
            return new TradingDecision("HOLD", reason != null ? "BAD_WEATHER_" + reason : "BAD_WEATHER");
        }

        MarketRegimeFilter.FilterResult regimeResult = marketRegimeFilter.evaluate(
                hourCandles,
                tpCfg.marketRegimeAdxRangeThreshold,
                tpCfg.marketRegimeAdxUnclearThreshold,
                tpCfg.marketRegimeVolumeRatioMin,
                tpCfg.marketRegimeConfidenceMin,
                tpCfg.marketRegimeAtrBars
        );

        if (!regimeResult.canTrade) {
            return new TradingDecision("HOLD", "REGIME_" + regimeResult.reason);
        }

        SignalResult signal = generateSignal(hourCandles);
        if ("HOLD".equals(signal.action) || signal.confidence < CONFIDENCE_THRESHOLD) {
            return new TradingDecision("HOLD", signal.reason, 0.0, 0, null, null, null, p);
        }

        String allocationGroup = tpCfg.allocationGroup;
        if (allocationGroup != null && !allocationGroup.isEmpty() && peerCandles != null && !peerCandles.isEmpty()) {
            if (!GroupConfirmationFilter.isConfirmed(ticker, true, peerCandles)) {
                return new TradingDecision("HOLD", "noGroupConf_" + signal.reason,
                        0.0, 0, null, null, null, p);
            }
        }

        double entry = signal.entryPrice != null ? signal.entryPrice : cur.close;
        double atr = atrVal(hourCandles, ATR_PERIOD);
        double riskPerShare = atr * SL_MULTIPLIER;
        if (riskPerShare <= 0.0) {
            return new TradingDecision("HOLD", "risk0", 0.0, 0, null, null, null, p);
        }

        double maxPos = balance * MAX_POSITION_SIZE;
        double targetPosValue = min(balance * RISK_PER_TRADE / (riskPerShare / entry), maxPos);
        int qty = (int) max(1.0, floor(targetPosValue / entry));

        if (qty <= 0) {
            return new TradingDecision("HOLD", "qty0", 0.0, 0, null, null, null, p);
        }

        return new TradingDecision(
                "OPEN",
                signal.reason,
                signal.confidence,
                qty,
                signal.stopLoss,
                signal.takeProfit,
                entry,
                new Position("BUY", entry, signal.stopLoss, signal.takeProfit, qty, 0)
        );
    }

    private SignalResult generateSignal(List<Candle> candles) {
        int n = candles.size();
        if (n < 30) return new SignalResult("HOLD", "init", 0.0, null, null, null);

        Candle cur = candles.get(n - 1);
        double p = cur.close;

        double emaF = ema(candles, EMA_FAST);
        double emaS = ema(candles, EMA_SLOW);
        double rsi = rsiVal(candles, RSI_PERIOD);
        double adx = adxVal(candles, ATR_PERIOD);
        double atr = atrVal(candles, ATR_PERIOD);
        double volRatio = analyzeVolume(candles);

        if (atr <= 0.0) {
            return new SignalResult("HOLD", "ATR=0", 0.0, null, null, null);
        }

        boolean trendUp = emaF > emaS && p > emaS;
        boolean trending = adx >= ADX_MIN;

        Candle prev = n >= 2 ? candles.get(n - 2) : null;
        double prevEmaF = n > EMA_FAST ? ema(candles.subList(0, n - 1), EMA_FAST) : emaF;
        double prevEmaS = n > EMA_SLOW ? ema(candles.subList(0, n - 1), EMA_SLOW) : emaS;
        boolean crossUp = prevEmaF <= prevEmaS && emaF > emaS;

        if (trendUp && trending) {
            double conf = 0.55;
            List<String> reasons = new ArrayList<>();
            reasons.add("LONG");

            if (crossUp) {
                conf += 0.15;
                reasons.add("X");
            }
            if (prev != null && prev.close < emaF && cur.close > emaF && cur.close > cur.open) {
                conf += 0.10;
                reasons.add("PB");
            }
            if (rsi > 50) {
                conf += 0.05;
                reasons.add("RSI" + (int) rsi);
            }
            if (volRatio >= VOLUME_MULTIPLIER) {
                conf += 0.05;
                reasons.add("VOL");
            }

            if (conf >= CONFIDENCE_THRESHOLD) {
                conf = min(0.90, conf);
                double sl = p - atr * SL_MULTIPLIER;
                double tp = p + atr * TP_MULTIPLIER;
                return new SignalResult("BUY", String.join("_", reasons), conf, p, sl, tp);
            }
        }

        return new SignalResult("HOLD", "adx" + (int) adx + "_rsi" + (int) rsi, 0.0, null, null, null);
    }

    private double analyzeVolume(List<Candle> candles) {
        if (candles.size() < VOLUME_PERIOD) {
            return 1.0;
        }

        double avg = 0.0;
        for (int i = candles.size() - VOLUME_PERIOD; i < candles.size(); i++) {
            avg += candles.get(i).volume;
        }
        avg /= VOLUME_PERIOD;

        return avg > 0.0 ? candles.get(candles.size() - 1).volume / avg : 1.0;
    }

    private static class SignalResult {
        final String action;
        final String reason;
        final double confidence;
        final Double entryPrice;
        final Double stopLoss;
        final Double takeProfit;

        SignalResult(String action, String reason, double confidence, Double entryPrice, Double stopLoss, Double takeProfit) {
            this.action = action;
            this.reason = reason;
            this.confidence = confidence;
            this.entryPrice = entryPrice;
            this.stopLoss = stopLoss;
            this.takeProfit = takeProfit;
        }
    }
}
