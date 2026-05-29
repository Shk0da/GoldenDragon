package com.github.shk0da.GoldenDragon.strategy;

import com.github.shk0da.GoldenDragon.config.UnifiedTraderConfig;
import com.github.shk0da.GoldenDragon.model.Candle;
import com.github.shk0da.GoldenDragon.model.Config;
import com.github.shk0da.GoldenDragon.model.Position;
import com.github.shk0da.GoldenDragon.model.TradingDecision;
import com.github.shk0da.GoldenDragon.service.TCSService;
import java.util.List;

public class IchimokuStrategy extends BaseStrategy {

    public IchimokuStrategy(UnifiedTraderConfig unifiedTraderConfig, TCSService tcsService) {
        this(unifiedTraderConfig, tcsService, new Config());
    }

    public IchimokuStrategy(UnifiedTraderConfig unifiedTraderConfig, TCSService tcsService, Config config) {
        super(unifiedTraderConfig, tcsService, config);
    }

    @Override
    protected String getStrategyName() {
        return "IchimokuStrategy";
    }

    @Override
    public TradingDecision decide(String ticker,
                                  List<Candle> hourCandles,
                                  List<Candle> minuteCandles,
                                  Position position,
                                  double balance,
                                  boolean incrementCandlesHeld) {

        if (hourCandles == null || hourCandles.size() < 80) {
            return new TradingDecision("HOLD", "init");
        }

        UnifiedTraderConfig.TickerParams tpCfg = unifiedTraderConfig.getTickerParams(ticker);
        if (tpCfg == null || !tpCfg.enabled) {
            return new TradingDecision("HOLD", "ticker_disabled");
        }

        Candle cur = hourCandles.get(hourCandles.size() - 1);

        Position p = position;
        if (position.quantity > 0 && incrementCandlesHeld) {
            p = new Position(
                    position.direction,
                    position.entryPrice,
                    position.stopLoss,
                    position.takeProfit,
                    position.quantity,
                    position.candlesHeld + 1,
                    position.cooldownRemaining
            );
        } else if (position.quantity > 0) {
            p = new Position(
                    position.direction,
                    position.entryPrice,
                    position.stopLoss,
                    position.takeProfit,
                    position.quantity,
                    position.candlesHeld,
                    position.cooldownRemaining
            );
        }

        if (p.quantity > 0) {
            Double sl = p.stopLoss;
            Double tp = p.takeProfit;
            String dir = p.direction;

            if (sl != null && "BUY".equals(dir) && cur.low <= sl) {
                return new TradingDecision(
                        "CLOSE", "stop_loss", 0.0, p.quantity,
                        null, null, sl, new Position(config.cooldownCandles)
                );
            }

            if (sl != null && "SELL".equals(dir) && cur.high >= sl) {
                return new TradingDecision(
                        "CLOSE", "stop_loss", 0.0, p.quantity,
                        null, null, sl, new Position(config.cooldownCandles)
                );
            }

            if (tp != null && "BUY".equals(dir) && cur.high >= tp) {
                return new TradingDecision(
                        "CLOSE", "take_profit", 0.0, p.quantity,
                        null, null, tp, new Position(config.cooldownCandles)
                );
            }

            if (tp != null && "SELL".equals(dir) && cur.low <= tp) {
                return new TradingDecision(
                        "CLOSE", "take_profit", 0.0, p.quantity,
                        null, null, tp, new Position(config.cooldownCandles)
                );
            }

            int maxHold = config.maxCandlesHold;
            if (p.candlesHeld >= maxHold) {
                return new TradingDecision(
                        "CLOSE", "expired", 0.0, p.quantity,
                        null, null, cur.close, new Position(config.cooldownCandles)
                );
            }

            double atr = atrVal(hourCandles, config.atrPeriod > 0 ? config.atrPeriod : 14);
            if (atr > 0.0) {
                Position trailed = applyTrailing(p, cur, atr);
                if (trailed != p) {
                    return new TradingDecision("HOLD", "trail", 0.0, 0, null, null, null, trailed);
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

        int signal = calculateSignal(hourCandles);

        if (p.quantity > 0) {
            if ("BUY".equals(p.direction) && signal < 0) {
                return new TradingDecision(
                        "CLOSE", "trend_changed_sell", 0.0, p.quantity,
                        null, null, cur.close, new Position(config.cooldownCandles)
                );
            }
            if ("SELL".equals(p.direction) && signal > 0) {
                return new TradingDecision(
                        "CLOSE", "trend_changed_buy", 0.0, p.quantity,
                        null, null, cur.close, new Position(config.cooldownCandles)
                );
            }
            return new TradingDecision("HOLD", "in_pos", 0.0, 0, null, null, null, p);
        }

        TradingDecision filterResult = applyFilters(ticker, hourCandles, cur, p, tpCfg);
        if (filterResult != null) {
            return filterResult;
        }

        if (signal == 0) {
            return new TradingDecision("HOLD", "noSig", 0.0, 0, null, null, null, p);
        }

        double entry = cur.close;
        double atr = atrVal(hourCandles, config.atrPeriod > 0 ? config.atrPeriod : 14);
        if (atr <= 0.0) {
            return new TradingDecision("HOLD", "atr0", 0.0, 0, null, null, null, p);
        }

        double slMult = tpCfg.slMult > 0.0 ? tpCfg.slMult : 1.0;
        double tpMult = tpCfg.tpMult > 0.0 ? tpCfg.tpMult : 2.0;
        double riskP = tpCfg.riskP > 0.0 ? tpCfg.riskP : 0.01;

        double slDist = atr * slMult;
        double tpDist = atr * tpMult;

        if (slDist <= 0.0 || tpDist <= 0.0) {
            return new TradingDecision("HOLD", "dist0", 0.0, 0, null, null, null, p);
        }

        double maxRisk = balance * riskP;
        double maxQty = Math.floor(balance / entry);
        int qty = (int) Math.min(Math.max(1, Math.floor(maxRisk / slDist)), maxQty);

        if (qty <= 0) {
            return new TradingDecision("HOLD", "qty0", 0.0, 0, null, null, null, p);
        }

        boolean isBuy = signal > 0;
        double sl = isBuy ? entry - slDist : entry + slDist;
        double tp = isBuy ? entry + tpDist : entry - tpDist;
        String direction = isBuy ? "BUY" : "SELL";

        double confidence = Math.min(1.0, Math.abs(signal) / 2.0);

        return new TradingDecision(
                "OPEN",
                isBuy ? "ICHIMOKU_BUY" : "ICHIMOKU_SELL",
                confidence,
                qty,
                sl,
                tp,
                entry,
                new Position(direction, entry, sl, tp, qty, 0)
        );
    }

    private Position applyTrailing(Position p, Candle cur, double atr) {
        if (p.quantity <= 0 || p.entryPrice == null || p.direction == null) {
            return p;
        }

        double ep = p.entryPrice;
        Double currentSl = p.stopLoss;

        if ("BUY".equals(p.direction)) {
            double pnl = cur.close - ep;

            if (pnl >= atr * 0.5) {
                double be = ep + atr * 0.08;
                if (currentSl == null || be > currentSl) {
                    currentSl = be;
                }
            }

            if (pnl >= atr) {
                double trail = cur.close - atr * 0.35;
                if (currentSl == null || trail > currentSl) {
                    currentSl = trail;
                }
            }

            if (pnl >= atr * 1.8) {
                double tight = cur.close - atr * 0.20;
                if (currentSl == null || tight > currentSl) {
                    currentSl = tight;
                }
            }

            if (currentSl != null && (p.stopLoss == null || currentSl > p.stopLoss)) {
                return new Position(
                        p.direction,
                        p.entryPrice,
                        currentSl,
                        p.takeProfit,
                        p.quantity,
                        p.candlesHeld,
                        p.cooldownRemaining
                );
            }
        }

        if ("SELL".equals(p.direction)) {
            double pnl = ep - cur.close;

            if (pnl >= atr * 0.5) {
                double be = ep - atr * 0.08;
                if (currentSl == null || be < currentSl) {
                    currentSl = be;
                }
            }

            if (pnl >= atr) {
                double trail = cur.close + atr * 0.35;
                if (currentSl == null || trail < currentSl) {
                    currentSl = trail;
                }
            }

            if (pnl >= atr * 1.8) {
                double tight = cur.close + atr * 0.20;
                if (currentSl == null || tight < currentSl) {
                    currentSl = tight;
                }
            }

            if (currentSl != null && (p.stopLoss == null || currentSl < p.stopLoss)) {
                return new Position(
                        p.direction,
                        p.entryPrice,
                        currentSl,
                        p.takeProfit,
                        p.quantity,
                        p.candlesHeld,
                        p.cooldownRemaining
                );
            }
        }

        return p;
    }

    private int calculateSignal(List<Candle> candles) {
        if (candles == null || candles.size() < 60) {
            return 0;
        }

        int shift = 1;

        double tenkan1 = tenkanSen(candles, 9, shift);
        double kijun1 = kijunSen(candles, 26, shift);
        double spanA1 = senkouSpanA(candles, 9, 26, shift);
        double spanB1 = senkouSpanB(candles, 52, shift);
        double close1 = closeAt(candles, shift);

        double tenkan2 = tenkanSen(candles, 9, shift + 1);
        double kijun2 = kijunSen(candles, 26, shift + 1);
        double spanA2 = senkouSpanA(candles, 9, 26, shift + 1);
        double spanB2 = senkouSpanB(candles, 52, shift + 1);
        double close2 = closeAt(candles, shift + 1);

        double topCloud1 = Math.max(spanA1, spanB1);
        double bottomCloud1 = Math.min(spanA1, spanB1);
        double topCloud2 = Math.max(spanA2, spanB2);
        double bottomCloud2 = Math.min(spanA2, spanB2);

        boolean buy =
                (tenkan1 > kijun1 && tenkan2 < kijun2 && kijun1 > topCloud1)
                        || (close1 > kijun1 && close2 < kijun2 && kijun1 > topCloud1)
                        || (close1 > topCloud1 && close2 < topCloud2);

        boolean sell =
                (tenkan1 < kijun1 && tenkan2 > kijun2 && tenkan1 < bottomCloud1)
                        || (close1 < kijun1 && close2 > kijun2 && kijun1 < bottomCloud1)
                        || (close1 < bottomCloud1 && close2 > bottomCloud2);

        double adx = adxVal(candles, 14);
        double rsi = rsiVal(candles, 14);
        double emaFast = ema(candles, 12);
        double emaSlow = ema(candles, 26);
        double atr = atrVal(candles, 14);

        int strength = 0;
        if (adx > 25) {
            if (emaFast > emaSlow && rsi >= 40 && rsi <= 70) strength = 1;
            if (emaFast < emaSlow && rsi <= 60 && rsi >= 30) strength = -1;
        }
        if (adx > 35) {
            if (emaFast > emaSlow && rsi >= 45 && rsi <= 68) strength = 2;
            if (emaFast < emaSlow && rsi <= 55 && rsi >= 32) strength = -2;
        }

        int signal = 0;

        if (buy) signal += 1;
        if (sell) signal -= 1;

        if (close1 > topCloud1 && emaFast > emaSlow) signal += 1;
        if (close1 < bottomCloud1 && emaFast < emaSlow) signal -= 1;

        if (rsi < 35) signal += 1;
        if (rsi > 65) signal -= 1;

        signal += strength;

        if (atr <= 0.0) {
            return 0;
        }

        if (signal >= 2) return 2;
        if (signal <= -2) return -2;
        if (signal > 0) return 1;
        if (signal < 0) return -1;
        return 0;
    }

    private double closeAt(List<Candle> candles, int shift) {
        int idx = candles.size() - 1 - shift;
        if (idx < 0) return 0.0;
        return candles.get(idx).close;
    }

    private double high(List<Candle> candles, int fromInclusive, int toInclusive) {
        double max = Double.NEGATIVE_INFINITY;
        for (int i = fromInclusive; i <= toInclusive; i++) {
            max = Math.max(max, candles.get(i).high);
        }
        return max;
    }

    private double low(List<Candle> candles, int fromInclusive, int toInclusive) {
        double min = Double.POSITIVE_INFINITY;
        for (int i = fromInclusive; i <= toInclusive; i++) {
            min = Math.min(min, candles.get(i).low);
        }
        return min;
    }

    private double tenkanSen(List<Candle> candles, int period, int shift) {
        int end = candles.size() - 1 - shift;
        int start = end - period + 1;
        if (start < 0) return 0.0;
        return (high(candles, start, end) + low(candles, start, end)) / 2.0;
    }

    private double kijunSen(List<Candle> candles, int period, int shift) {
        int end = candles.size() - 1 - shift;
        int start = end - period + 1;
        if (start < 0) return 0.0;
        return (high(candles, start, end) + low(candles, start, end)) / 2.0;
    }

    private double senkouSpanA(List<Candle> candles, int tenkanPeriod, int kijunPeriod, int shift) {
        double tenkan = tenkanSen(candles, tenkanPeriod, shift);
        double kijun = kijunSen(candles, kijunPeriod, shift);
        return (tenkan + kijun) / 2.0;
    }

    private double senkouSpanB(List<Candle> candles, int period, int shift) {
        int end = candles.size() - 1 - shift;
        int start = end - period + 1;
        if (start < 0) return 0.0;
        return (high(candles, start, end) + low(candles, start, end)) / 2.0;
    }
}
