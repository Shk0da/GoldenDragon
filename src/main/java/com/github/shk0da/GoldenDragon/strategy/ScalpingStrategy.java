package com.github.shk0da.GoldenDragon.strategy;

import com.github.shk0da.GoldenDragon.config.UnifiedTraderConfig;
import com.github.shk0da.GoldenDragon.model.Candle;
import com.github.shk0da.GoldenDragon.model.Config;
import com.github.shk0da.GoldenDragon.model.Position;
import com.github.shk0da.GoldenDragon.model.TradingDecision;
import com.github.shk0da.GoldenDragon.service.TCSService;
import java.util.ArrayList;
import java.util.List;


import static java.lang.Math.abs;
import static java.lang.Math.floor;
import static java.lang.Math.max;
import static java.lang.Math.min;

public class ScalpingStrategy extends BaseStrategy {

    private static final int EMA_TREND = 24;
    private static final int EMA_FAST = 3;
    private static final int EMA_SLOW = 7;
    private static final int ADX_PERIOD = 14;
    private static final int RSI_PERIOD = 14;
    private static final int ATR_PERIOD = 14;

    private static final double ADX_MIN = 20.0;
    private static final double RSI_BUY_MIN = 40.0;
    private static final double RSI_BUY_MAX = 65.0;
    private static final double RSI_SELL_MIN = 35.0;
    private static final double RSI_SELL_MAX = 60.0;

    private static final double SL_MULTIPLIER = 1.2;
    private static final double TP_MULTIPLIER = 2.4;
    private static final double BREAK_EVEN_THRESHOLD = 1.2;
    private static final double TRAIL_ACTIVATION = 1.5;
    private static final double TRAIL_STEP = 0.4;

    private static final double ATR_SPIKE_THRESHOLD = 2.5;
    private static final int ATR_SPIKE_WINDOW = 10;
    private static final double RESIDUAL_ATR_MAX = 0.80;

    private static final double RISK_PER_TRADE = 0.01;
    private static final int COOLDOWN_CANDLES = 4;
    private static final int MAX_CANDLES_HOLD = 24;
    private static final int MIN_CONFLUENCE = 4;

    private int consecutiveLosses = 0;

    public ScalpingStrategy(UnifiedTraderConfig unifiedTraderConfig, TCSService tcsService) {
        this(unifiedTraderConfig, tcsService, new Config());
    }

    public ScalpingStrategy(UnifiedTraderConfig unifiedTraderConfig, TCSService tcsService, Config config) {
        super(unifiedTraderConfig, tcsService, config);
    }

    @Override
    protected String getStrategyName() {
        return "ScalpingUnifiedStrategy";
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
        List<Candle> candles = minuteCandles != null && !minuteCandles.isEmpty() ? minuteCandles : hourCandles;
        if (candles == null || candles.size() < 60) {
            return new TradingDecision("HOLD", "init");
        }

        Candle cur = candles.get(candles.size() - 1);
        Position p = position;

        if (position.quantity > 0) {
            Double sl = position.stopLoss;
            Double tp = position.takeProfit;
            String dir = position.direction;

            if (sl != null && (("BUY".equals(dir) && cur.close <= sl) || ("SELL".equals(dir) && cur.close >= sl))) {
                return new TradingDecision(
                        "CLOSE",
                        "stop_loss",
                        0.0,
                        position.quantity,
                        null,
                        null,
                        sl,
                        new Position(COOLDOWN_CANDLES)
                );
            }

            if (tp != null && (("BUY".equals(dir) && cur.high >= tp) || ("SELL".equals(dir) && cur.low <= tp))) {
                return new TradingDecision(
                        "CLOSE",
                        "take_profit",
                        0.0,
                        position.quantity,
                        null,
                        null,
                        tp,
                        new Position(COOLDOWN_CANDLES)
                );
            }

            if (position.candlesHeld >= MAX_CANDLES_HOLD) {
                return new TradingDecision(
                        "CLOSE",
                        "expired",
                        0.0,
                        position.quantity,
                        null,
                        null,
                        cur.close,
                        new Position(COOLDOWN_CANDLES)
                );
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
            double pnlAbs = "BUY".equals(dir) ? cur.close - ep : ep - cur.close;
            double atr = atrVal(candles, ATR_PERIOD);

            if (atr > 0.0) {
                double pnlAtr = pnlAbs / atr;

                if (pnlAtr >= TRAIL_ACTIVATION) {
                    Double trailRef = getTrailRefPrice(position);
                    if (trailRef == null) {
                        trailRef = cur.close;
                    }

                    double step = atr * TRAIL_STEP;

                    if ("BUY".equals(dir) && cur.close > trailRef + step) {
                        double baseSl = position.stopLoss != null ? position.stopLoss : ep;
                        double newSl = baseSl + step;
                        p = withTrailState(p, true, cur.close, newSl);
                    } else if ("SELL".equals(dir) && cur.close < trailRef - step) {
                        double baseSl = position.stopLoss != null ? position.stopLoss : ep;
                        double newSl = baseSl - step;
                        p = withTrailState(p, true, cur.close, newSl);
                    } else {
                        p = withTrailState(p, true, trailRef, p.stopLoss);
                    }
                } else if (pnlAtr >= BREAK_EVEN_THRESHOLD) {
                    double be = "BUY".equals(dir) ? ep + atr * 0.01 : ep - atr * 0.01;

                    if ("BUY".equals(dir) && (position.stopLoss != null ? position.stopLoss : 0.0) < be) {
                        p = withTrailState(p, false, getTrailRefPrice(p), be);
                    }
                    if ("SELL".equals(dir) && (position.stopLoss != null ? position.stopLoss : Double.MAX_VALUE) > be) {
                        p = withTrailState(p, false, getTrailRefPrice(p), be);
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
                            p.cooldownRemaining - 1
                    )
            );
        }

        if (position.quantity > 0) {
            return new TradingDecision("HOLD", "in_pos", 0.0, 0, null, null, null, p);
        }

        if (consecutiveLosses >= 3) {
            return new TradingDecision("HOLD", "maxLoss" + consecutiveLosses, 0.0, 0, null, null, null, p);
        }

        if (atrSpikeFilter(candles)) {
            return new TradingDecision("HOLD", "ATRspike", 0.0, 0, null, null, null, p);
        }

        TradingDecision signal = checkConfluence(candles);
        if ("HOLD".equals(signal.action)) {
            return new TradingDecision("HOLD", signal.reason, 0.0, 0, null, null, null, p);
        }

        if (!"BUY".equals(signal.action)) {
            return new TradingDecision("HOLD", "short_disabled", 0.0, 0, null, null, null, p);
        }

        double dAtr = atrVal(candles, ATR_PERIOD);
        if (dAtr <= 0.0) {
            return new TradingDecision("HOLD", "ATR0", 0.0, 0, null, null, null, p);
        }

        double entry = cur.close;
        double slDist = dAtr * SL_MULTIPLIER;
        double maxRisk = balance * RISK_PER_TRADE;
        int maxQty = (int) floor(balance / entry);
        int qty = min(max(1, (int) floor(maxRisk / slDist)), maxQty);

        if (qty <= 0) {
            return new TradingDecision("HOLD", "qty0", 0.0, 0, null, null, null, p);
        }

        double sl = entry - slDist;
        double tp = entry + slDist * TP_MULTIPLIER / SL_MULTIPLIER;

        return new TradingDecision(
                "OPEN",
                signal.reason,
                signal.confidence,
                qty,
                sl,
                tp,
                entry,
                new Position("BUY", entry, sl, tp, qty, 0)
        );
    }

    private TradingDecision checkConfluence(List<Candle> candles) {
        if (candles == null || candles.size() < 60) {
            return new TradingDecision("HOLD", "init");
        }

        Candle cur = candles.get(candles.size() - 1);
        double p = cur.close;

        double emaT = ema(candles, EMA_TREND);
        double emaF = ema(candles, EMA_FAST);
        double emaS = ema(candles, EMA_SLOW);
        double adx = adxVal(candles, ADX_PERIOD);
        double rsi = rsiVal(candles, RSI_PERIOD);
        double dAtr = atrVal(candles, ATR_PERIOD);

        if (dAtr <= 0.0) {
            return new TradingDecision("HOLD", "dAtr0");
        }

        int buyScore = 0;
        int sellScore = 0;
        List<String> buyReasons = new ArrayList<>();
        List<String> sellReasons = new ArrayList<>();

        if (p > emaT) {
            buyScore++;
            buyReasons.add("TREND");
        }
        if (p < emaT) {
            sellScore++;
            sellReasons.add("TREND");
        }

        if (adx >= ADX_MIN) {
            buyScore++;
            sellScore++;
            buyReasons.add("ADX" + (int) adx);
            sellReasons.add("ADX" + (int) adx);
        }

        if (emaF > emaS) {
            buyScore++;
            buyReasons.add("EMA" + EMA_FAST + "X" + EMA_SLOW);
        }
        if (emaF < emaS) {
            sellScore++;
            sellReasons.add("EMA" + EMA_FAST + "X" + EMA_SLOW);
        }

        if (rsi >= RSI_BUY_MIN && rsi <= RSI_BUY_MAX) {
            buyScore++;
            buyReasons.add("RSI" + (int) rsi);
        }
        if (rsi >= RSI_SELL_MIN && rsi <= RSI_SELL_MAX) {
            sellScore++;
            sellReasons.add("RSI" + (int) rsi);
        }

        if (cur.close > cur.open && cur.close > emaF) {
            buyScore++;
            buyReasons.add("CANDLE");
        }
        if (cur.close < cur.open && cur.close < emaF) {
            sellScore++;
            sellReasons.add("CANDLE");
        }

        double residual = atrUsedPct(candles);
        boolean canTrendBuy = residual <= RESIDUAL_ATR_MAX;
        boolean canTrendSell = residual <= RESIDUAL_ATR_MAX;

        if (buyScore >= MIN_CONFLUENCE && canTrendBuy) {
            double conf = min(0.90, 0.50 + buyScore * 0.08);
            return new TradingDecision("BUY", String.join("_", buyReasons), conf, 0, null, null, null, null);
        }

        if (sellScore >= MIN_CONFLUENCE && canTrendSell) {
            double conf = min(0.90, 0.50 + sellScore * 0.08);
            return new TradingDecision("SELL", String.join("_", sellReasons), conf, 0, null, null, null, null);
        }

        if (buyScore >= 3 && !canTrendBuy) {
            double conf = min(0.90, 0.50 + buyScore * 0.08) * 0.85;
            return new TradingDecision("BUY", "BOUNCE_" + String.join("_", buyReasons), conf, 0, null, null, null, null);
        }

        if (sellScore >= 3 && !canTrendSell) {
            double conf = min(0.90, 0.50 + sellScore * 0.08) * 0.85;
            return new TradingDecision("SELL", "BOUNCE_" + String.join("_", sellReasons), conf, 0, null, null, null, null);
        }

        return new TradingDecision("HOLD", "conv" + buyScore + "v" + sellScore);
    }

    private boolean atrSpikeFilter(List<Candle> candles) {
        if (candles == null || candles.size() < ATR_SPIKE_WINDOW + 5) {
            return false;
        }

        List<Double> vals = new ArrayList<>();
        for (int i = candles.size() - ATR_SPIKE_WINDOW; i < candles.size(); i++) {
            Candle c = candles.get(i);
            Candle p = candles.get(i - 1);
            vals.add(max(max(c.high - c.low, abs(c.high - p.close)), abs(c.low - p.close)));
        }

        double avg = vals.stream().mapToDouble(v -> v).average().orElse(0.0);
        if (avg <= 0.0) {
            return false;
        }

        return vals.get(vals.size() - 1) / avg >= ATR_SPIKE_THRESHOLD;
    }

    private double atrUsedPct(List<Candle> candles) {
        double dAtr = atrVal(candles, ATR_PERIOD);
        if (dAtr <= 0.0) {
            return 0.0;
        }

        int n = min(candles.size(), 24);
        List<Candle> last = candles.subList(candles.size() - n, candles.size());

        double high = last.stream().mapToDouble(c -> c.high).max().orElse(0.0);
        double low = last.stream().mapToDouble(c -> c.low).min().orElse(0.0);
        double rng = high - low;

        return Math.max(0.0, Math.min(1.0, rng / (dAtr * 0.7)));
    }

    private Position withTrailState(Position p, boolean trailActive, Double trailRefPrice, Double stopLoss) {
        return new Position(
                p.direction,
                p.entryPrice,
                stopLoss,
                p.takeProfit,
                p.quantity,
                p.candlesHeld,
                p.cooldownRemaining
        );
    }

    private Double getTrailRefPrice(Position p) {
        return null;
    }
}