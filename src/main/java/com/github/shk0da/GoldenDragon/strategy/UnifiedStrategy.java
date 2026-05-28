package com.github.shk0da.GoldenDragon.strategy;

import com.github.shk0da.GoldenDragon.config.UnifiedTraderConfig;
import com.github.shk0da.GoldenDragon.model.TickerInfo;
import com.github.shk0da.GoldenDragon.model.TickerType;
import com.github.shk0da.GoldenDragon.repository.TickerRepository;
import com.github.shk0da.GoldenDragon.service.TCSService;
import com.github.shk0da.GoldenDragon.utils.IndicatorsUtil;
import java.sql.Timestamp;
import java.text.SimpleDateFormat;
import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import ru.tinkoff.piapi.contract.v1.CandleInterval;
import ru.tinkoff.piapi.contract.v1.HistoricCandle;


import static com.github.shk0da.GoldenDragon.utils.TimeUtils.sleep;
import static java.util.concurrent.CompletableFuture.allOf;
import static java.util.concurrent.CompletableFuture.runAsync;

public class UnifiedStrategy {

    public static class Config {
        public final int emaTrend;
        public final int emaFast;
        public final int emaSlow;
        public final int rsiPeriod;
        public final int adxPeriod;
        public final int atrPeriod;
        public final double adxMin;
        public final double rsiOversold;
        public final double rsiOverbought;
        public final double commission;
        public final int maxCandlesHold;
        public final int maxCandlesHoldFx;
        public final double atrSpikeThreshold;
        public final int atrSpikeWindow;
        public final int cooldownCandles;

        public Config() {
            this.emaTrend = 24;
            this.emaFast = 3;
            this.emaSlow = 7;
            this.rsiPeriod = 14;
            this.adxPeriod = 14;
            this.atrPeriod = 14;
            this.adxMin = 20.0;
            this.rsiOversold = 25.0;
            this.rsiOverbought = 75.0;
            this.commission = 0.0005;
            this.maxCandlesHold = 24;
            this.maxCandlesHoldFx = 12;
            this.atrSpikeThreshold = 3.0;
            this.atrSpikeWindow = 10;
            this.cooldownCandles = 3;
        }

        public Config(int emaTrend, int emaFast, int emaSlow, int rsiPeriod, int adxPeriod, int atrPeriod,
                      double adxMin, double rsiOversold, double rsiOverbought, double commission,
                      int maxCandlesHold, int maxCandlesHoldFx, double atrSpikeThreshold,
                      int atrSpikeWindow, int cooldownCandles) {
            this.emaTrend = emaTrend;
            this.emaFast = emaFast;
            this.emaSlow = emaSlow;
            this.rsiPeriod = rsiPeriod;
            this.adxPeriod = adxPeriod;
            this.atrPeriod = atrPeriod;
            this.adxMin = adxMin;
            this.rsiOversold = rsiOversold;
            this.rsiOverbought = rsiOverbought;
            this.commission = commission;
            this.maxCandlesHold = maxCandlesHold;
            this.maxCandlesHoldFx = maxCandlesHoldFx;
            this.atrSpikeThreshold = atrSpikeThreshold;
            this.atrSpikeWindow = atrSpikeWindow;
            this.cooldownCandles = cooldownCandles;
        }
    }

    public static class Candle {
        public final String time;
        public final double open;
        public final double high;
        public final double low;
        public final double close;
        public final long volume;

        public Candle(String time, double open, double high, double low, double close, long volume) {
            this.time = time;
            this.open = open;
            this.high = high;
            this.low = low;
            this.close = close;
            this.volume = volume;
        }
    }

    public static class Position {
        public final String direction;
        public final Double entryPrice;
        public final Double stopLoss;
        public final Double takeProfit;
        public final int quantity;
        public final int candlesHeld;
        public final int cooldownRemaining;

        public Position() {
            this(null, null, null, null, 0, 0, 0);
        }

        public Position(int cooldownRemaining) {
            this(null, null, null, null, 0, 0, cooldownRemaining);
        }

        public Position(String direction, Double entryPrice, Double stopLoss, Double takeProfit,
                        int quantity, int candlesHeld) {
            this(direction, entryPrice, stopLoss, takeProfit, quantity, candlesHeld, 0);
        }

        public Position(String direction, Double entryPrice, Double stopLoss, Double takeProfit,
                        int quantity, int candlesHeld, int cooldownRemaining) {
            this.direction = direction;
            this.entryPrice = entryPrice;
            this.stopLoss = stopLoss;
            this.takeProfit = takeProfit;
            this.quantity = quantity;
            this.candlesHeld = candlesHeld;
            this.cooldownRemaining = cooldownRemaining;
        }
    }

    public static class TradingDecision {
        public final String action;
        public final String reason;
        public final double confidence;
        public final int quantity;
        public final Double stopLoss;
        public final Double takeProfit;
        public final Double entryPrice;
        public final Position updatedPosition;

        public TradingDecision(String action, String reason) {
            this(action, reason, 0.0, 0, null, null, null, null);
        }

        public TradingDecision(String action, String reason, double confidence, int quantity,
                               Double stopLoss, Double takeProfit, Double entryPrice, Position updatedPosition) {
            this.action = action;
            this.reason = reason;
            this.confidence = confidence;
            this.quantity = quantity;
            this.stopLoss = stopLoss;
            this.takeProfit = takeProfit;
            this.entryPrice = entryPrice;
            this.updatedPosition = updatedPosition;
        }
    }

    public enum Group { TREND, FX, MIXED }

    private final Config config;

    private final TCSService tcsService;
    private final UnifiedTraderConfig unifiedTraderConfig;

    public UnifiedStrategy(UnifiedTraderConfig unifiedTraderConfig, TCSService tcsService) {
        this.config = new Config();
        this.tcsService = tcsService;
        this.unifiedTraderConfig = unifiedTraderConfig;
    }

    public Group getGroup(String ticker) {
        switch (ticker) {
            case "CNYRUBF": return Group.FX;
            case "USDRUBF": return Group.TREND;
            case "GLDRUBF": return Group.MIXED;
            default: return Group.TREND;
        }
    }

    public void run() {
        List<CompletableFuture<Void>> tasks = new ArrayList<>();
        ExecutorService executor = Executors.newFixedThreadPool(unifiedTraderConfig.getStocks().size());
        for (String name : unifiedTraderConfig.getStocks()) {
            tasks.add(runAsync(() -> {
                while (true) {
                    this.processTicker(name, tcsService, unifiedTraderConfig);
                    sleep(30_000);
                }
            }, executor));
            sleep(1_000);
        }
        allOf(tasks.toArray(new CompletableFuture[0])).join();
    }

    public TradingDecision decide(String ticker, List<Candle> candles, Position position, double balance) {
        if (candles.size() < 60) return new TradingDecision("HOLD", "init");
        Candle cur = candles.get(candles.size() - 1);
        Position p = position;
        Group grp = getGroup(ticker);

        if (position.quantity > 0) {
            Double sl = position.stopLoss;
            Double tp = position.takeProfit;
            String dir = position.direction;
            int maxH = grp == Group.FX ? config.maxCandlesHoldFx : config.maxCandlesHold;

            if (sl != null && (("BUY".equals(dir) && cur.close <= sl) || ("SELL".equals(dir) && cur.close >= sl)))
                return new TradingDecision("CLOSE", "stop_loss", 0.0, position.quantity,
                        null, null, null, new Position(config.cooldownCandles));
            if (tp != null && (("BUY".equals(dir) && cur.high >= tp) || ("SELL".equals(dir) && cur.low <= tp)))
                return new TradingDecision("CLOSE", "take_profit", 0.0, position.quantity,
                        null, null, null, new Position(config.cooldownCandles));
            if (position.candlesHeld >= maxH)
                return new TradingDecision("CLOSE", "expired", 0.0, position.quantity,
                        null, null, cur.close, new Position(config.cooldownCandles));

            p = new Position(position.direction, position.entryPrice, position.stopLoss, position.takeProfit,
                    position.quantity, position.candlesHeld + 1, position.cooldownRemaining);
            double ep = position.entryPrice != null ? position.entryPrice : cur.close;
            double pnlAbs = "BUY".equals(dir) ? cur.close - ep : ep - cur.close;
            double atr = atrVal(candles, config.atrPeriod);

            if (atr > 0.0 && pnlAbs > 0) {
                double pnlAtr = pnlAbs / atr;
                double trMult = grp == Group.FX ? 0.5 : grp == Group.MIXED ? 0.8 : 1.0;
                if (pnlAtr >= 0.4 * trMult) {
                    double beSl = "BUY".equals(dir) ? ep + atr * 0.01 : ep - atr * 0.01;
                    if ("BUY".equals(dir) && (position.stopLoss != null ? position.stopLoss : 0.0) < beSl)
                        p = new Position(p.direction, p.entryPrice, beSl, p.takeProfit, p.quantity, p.candlesHeld, p.cooldownRemaining);
                    if ("SELL".equals(dir) && (position.stopLoss != null ? position.stopLoss : Double.MAX_VALUE) > beSl)
                        p = new Position(p.direction, p.entryPrice, beSl, p.takeProfit, p.quantity, p.candlesHeld, p.cooldownRemaining);
                }
                if (pnlAtr >= 0.8 * trMult) {
                    double trailSl = "BUY".equals(dir) ? cur.close - atr * 0.3 : cur.close + atr * 0.3;
                    if ("BUY".equals(dir) && trailSl > (position.stopLoss != null ? position.stopLoss : 0.0))
                        p = new Position(p.direction, p.entryPrice, trailSl, p.takeProfit, p.quantity, p.candlesHeld, p.cooldownRemaining);
                    if ("SELL".equals(dir) && trailSl < (position.stopLoss != null ? position.stopLoss : Double.MAX_VALUE))
                        p = new Position(p.direction, p.entryPrice, trailSl, p.takeProfit, p.quantity, p.candlesHeld, p.cooldownRemaining);
                }
            }
        }

        if (p.cooldownRemaining > 0)
            return new TradingDecision("HOLD", "CD" + p.cooldownRemaining,
                    0.0, 0, null, null, null,
                    new Position(p.direction, p.entryPrice, p.stopLoss, p.takeProfit,
                            p.quantity, p.candlesHeld, p.cooldownRemaining - 1));
        if (position.quantity > 0)
            return new TradingDecision("HOLD", "in_pos", 0.0, 0, null, null, null, p);

        double dAtr = atrVal(candles, config.atrPeriod);
        double avgAtr = emaAtr(candles, config.atrPeriod);
        if (dAtr <= 0.0 || avgAtr <= 0.0)
            return new TradingDecision("HOLD", "ATR0", 0.0, 0, null, null, null, p);
        if (dAtr > avgAtr * config.atrSpikeThreshold)
            return new TradingDecision("HOLD", "ATRspike", 0.0, 0, null, null, null, p);

        String signal;
        switch (grp) {
            case FX: signal = fxSignal(candles); break;
            case MIXED: signal = mixedSignal(candles); break;
            default: signal = trendSignal(candles);
        }
        if (signal == null)
            return new TradingDecision("HOLD", "noSig", 0.0, 0, null, null, null, p);

        double entry = cur.close;

        double slMult, tpMult, riskP;
        if ("USDRUBF".equals(ticker)) { slMult = 1.5; tpMult = 3.5; riskP = 0.003; }
        else if ("LKOH".equals(ticker)) { slMult = 1.2; tpMult = 3.0; riskP = 0.005; }
        else if (grp == Group.FX) { slMult = 0.8; tpMult = 1.2; riskP = 0.005; }
        else if (grp == Group.TREND) { slMult = 1.2; tpMult = 2.5; riskP = 0.01; }
        else { slMult = 1.0; tpMult = 2.0; riskP = 0.0075; }

        double slDist = dAtr * slMult;
        double tpDist = dAtr * tpMult;

        double maxRisk = balance * riskP;
        double maxQty = Math.floor(balance / entry);
        int qty = (int) Math.min(Math.max(1, Math.floor(maxRisk / slDist)), maxQty);
        if (qty <= 0) return new TradingDecision("HOLD", "qty0", 0.0, 0, null, null, null, p);

        boolean isBuy = signal.startsWith("TB") || signal.startsWith("FXB") || signal.startsWith("MXB");
        double sl = isBuy ? entry - slDist : entry + slDist;
        double tp = isBuy ? entry + tpDist : entry - tpDist;
        String dirName = isBuy ? "BUY" : "SELL";

        return new TradingDecision("OPEN", signal, 0.7, qty, sl, tp, entry,
                new Position(dirName, entry, sl, tp, qty, 0));
    }

    public void processTicker(String name, TCSService tcsService, UnifiedTraderConfig unifiedTraderConfig) {
        try {
            TickerInfo ticker = TickerRepository.INSTANCE.getAll().values().stream()
                    .filter(t -> t.getType() == TickerType.STOCK || t.getType() == TickerType.FEATURE)
                    .filter(t -> t.getName().equalsIgnoreCase(name) || t.getTicker().equalsIgnoreCase(name))
                    .findFirst().orElse(null);
            if (ticker == null) return;

            String figi = ticker.getFigi();
            OffsetDateTime now = OffsetDateTime.now();
            List<HistoricCandle> historicCandles = tcsService.getCandles(figi,
                    now.minusMinutes(24 * 60), now, CandleInterval.CANDLE_INTERVAL_HOUR);

            SimpleDateFormat df = new SimpleDateFormat("dd.MM.yyyy HH:mm:ss");
            List<Candle> candles = new ArrayList<>();
            for (HistoricCandle hc : historicCandles) {
                Timestamp ts = new Timestamp(hc.getTime().getSeconds() * 1000);
                candles.add(new Candle(
                        df.format(ts),
                        IndicatorsUtil.toDouble(hc.getOpen()),
                        IndicatorsUtil.toDouble(hc.getHigh()),
                        IndicatorsUtil.toDouble(hc.getLow()),
                        IndicatorsUtil.toDouble(hc.getClose()),
                        hc.getVolume()
                ));
            }

            TradingDecision decision = decide(name, candles, new Position(), 1_000_000.0);
            if ("OPEN".equals(decision.action)) {
                double entryPrice = decision.entryPrice != null ? decision.entryPrice : candles.get(candles.size() - 1).close;
                double slPrice, tpPrice;
                boolean isBuy = decision.updatedPosition != null && "BUY".equals(decision.updatedPosition.direction);
                if (isBuy) {
                    slPrice = decision.stopLoss != null ? decision.stopLoss : entryPrice * 0.98;
                    tpPrice = decision.takeProfit != null ? decision.takeProfit : entryPrice * 1.04;
                } else {
                    slPrice = decision.stopLoss != null ? decision.stopLoss : entryPrice * 1.02;
                    tpPrice = decision.takeProfit != null ? decision.takeProfit : entryPrice * 0.96;
                }
                double slPercent = Math.abs(entryPrice - slPrice) / entryPrice * 100;
                double tpPercent = Math.abs(tpPrice - entryPrice) / entryPrice * 100;
                if (isBuy) {
                    tcsService.buyByMarket(name, ticker.getType(), unifiedTraderConfig.getAveragePositionCost(), tpPercent, slPercent);
                } else {
                    tcsService.sellByMarket(name, ticker.getType(), unifiedTraderConfig.getAveragePositionCost(), tpPercent, slPercent);
                }
            }
        } catch (Exception ex) {
            System.err.println("UnifiedStrategy error for " + name + ": " + ex.getMessage());
        }
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

        boolean uptrend = p > emaT, dnTrend = p < emaT;
        boolean emaUp = emaF > emaS, emaDn = emaF < emaS;
        boolean trendOk = adx >= config.adxMin;
        boolean candleUp = cur.close > cur.open && cur.close > emaF;
        boolean candleDn = cur.close < cur.open && cur.close < emaF;

        int bs = 0, ss = 0;
        if (uptrend) bs++;
        if (dnTrend) ss++;
        if (trendOk) { bs++; ss++; }
        if (emaUp) bs++;
        if (emaDn) ss++;
        if (rsi >= 40.0 && rsi <= 65.0) bs++;
        if (rsi >= 35.0 && rsi <= 60.0) ss++;
        if (candleUp) bs++;
        if (candleDn) ss++;

        if (bs >= 3 && uptrend) return "TB_" + bs + "_" + (int)adx + "_" + (int)rsi;
        if (ss >= 3 && dnTrend) return "TS_" + ss + "_" + (int)adx + "_" + (int)rsi;
        return null;
    }

    public String fxSignal(List<Candle> candles) {
        if (candles.size() < 30) return null;
        double rsi = rsiVal(candles, config.rsiPeriod);
        String pat = candlePattern(candles);

        boolean extremeBuy = rsi <= config.rsiOversold;
        boolean extremeSell = rsi >= config.rsiOverbought;

        List<String> bullishPats = Arrays.asList("DOJI", "PIN_BAR_BUY", "ENGULFING_BUY", "MORNING_STAR");
        List<String> bearishPats = Arrays.asList("DOJI", "PIN_BAR_SELL", "ENGULFING_SELL", "EVENING_STAR");

        if (extremeBuy && bullishPats.contains(pat)) return "FXB_" + (int)rsi + "_" + pat;
        if (extremeSell && bearishPats.contains(pat)) return "FXS_" + (int)rsi + "_" + pat;
        return null;
    }

    public String mixedSignal(List<Candle> candles) {
        if (candles.size() < 60) return null;
        Candle cur = candles.get(candles.size() - 1);
        double p = cur.close;
        double emaT = ema(candles, config.emaTrend);
        double emaF = ema(candles, config.emaFast);
        double emaS = ema(candles, config.emaSlow);
        double adx = adxVal(candles, config.adxPeriod);
        double rsi = rsiVal(candles, config.rsiPeriod);
        String pat = candlePattern(candles);

        boolean uptrend = p > emaT, dnTrend = p < emaT;
        boolean emaUp = emaF > emaS, emaDn = emaF < emaS;
        boolean trendOk = adx >= config.adxMin;

        List<String> patternUp = Arrays.asList("PIN_BAR_BUY", "ENGULFING_BUY", "MORNING_STAR", "THREE_WHITE");
        List<String> patternDn = Arrays.asList("PIN_BAR_SELL", "ENGULFING_SELL", "EVENING_STAR", "THREE_BLACK");
        boolean patUp = patternUp.contains(pat);
        boolean patDn = patternDn.contains(pat);

        int bs = 0, ss = 0;
        List<String> br = new ArrayList<>(), sr = new ArrayList<>();
        if (uptrend) { bs++; br.add("TR"); }
        if (dnTrend) { ss++; sr.add("TR"); }
        if (trendOk) { bs++; ss++; br.add("AD" + (int)adx); sr.add("AD" + (int)adx); }
        if (emaUp) { bs++; br.add("EM"); }
        if (emaDn) { ss++; sr.add("EM"); }
        if (rsi >= 35.0 && rsi <= 70.0) { bs++; br.add("RS" + (int)rsi); }
        if (rsi >= 30.0 && rsi <= 65.0) { ss++; sr.add("RS" + (int)rsi); }
        if (patUp) { bs += 2; br.add(pat); }
        if (patDn) { ss += 2; sr.add(pat); }

        if (bs >= 3 && !patDn) return "MXB_" + String.join("_", br);
        if (ss >= 3 && !patUp) return "MXS_" + String.join("_", sr);
        return null;
    }

    public String candlePattern(List<Candle> candles) {
        if (candles.size() < 3) return "NONE";
        Candle c = candles.get(candles.size() - 1);
        Candle p1 = candles.get(candles.size() - 2);
        Candle p2 = candles.get(candles.size() - 3);
        double body = Math.abs(c.close - c.open);
        double range = c.high - c.low;
        if (range <= 0.0) return "NONE";
        double upperShadow = c.high - Math.max(c.open, c.close);
        double lowerShadow = Math.min(c.open, c.close) - c.low;
        if (body < range * 0.15) return "DOJI";
        if (lowerShadow > body * 2 && upperShadow < body * 0.3 && c.close > c.open) return "PIN_BAR_BUY";
        if (upperShadow > body * 2 && lowerShadow < body * 0.3 && c.close < c.open) return "PIN_BAR_SELL";
        if (c.close > c.open && p1.close < p1.open && c.open < p1.close && c.close > p1.open) return "ENGULFING_BUY";
        if (c.close < c.open && p1.close > p1.open && c.open > p1.close && c.close < p1.open) return "ENGULFING_SELL";
        if (c.close > c.open && p1.close > p1.open && p2.close > p2.open) return "THREE_WHITE";
        if (c.close < c.open && p1.close < p1.open && p2.close < p2.open) return "THREE_BLACK";
        if (c.close > c.open && p1.close < p1.open && lowerShadow > body * 1.5) return "MORNING_STAR";
        if (c.close < c.open && p1.close > p1.open && upperShadow > body * 1.5) return "EVENING_STAR";
        return "NONE";
    }

    public double ema(List<Candle> candles, int period) {
        if (candles.size() < period) return candles.get(candles.size() - 1).close;
        double[] c = candles.stream().mapToDouble(cdl -> cdl.close).toArray();
        double k = 2.0 / (period + 1);
        double e = 0;
        for (int i = 0; i < period; i++) e += c[i];
        e /= period;
        for (int i = period; i < c.length; i++) e = c[i] * k + e * (1 - k);
        return e;
    }

    public double atrVal(List<Candle> candles, int period) {
        if (candles.size() < period + 1) return 0.0;
        double sum = 0.0;
        for (int i = candles.size() - period; i < candles.size(); i++) {
            Candle c = candles.get(i);
            Candle p = candles.get(i - 1);
            sum += Math.max(Math.max(c.high - c.low, Math.abs(c.high - p.close)), Math.abs(c.low - p.close));
        }
        return sum / period;
    }

    public double emaAtr(List<Candle> candles, int period) {
        if (candles.size() < period + 5) return atrVal(candles, period);
        List<Double> vals = new ArrayList<>();
        for (int i = Math.max(0, candles.size() - 20); i < candles.size(); i++) {
            if (i < period + 1) continue;
            double s = 0.0;
            for (int j = i - period; j < i; j++) {
                Candle c = candles.get(j);
                Candle p = candles.get(j - 1);
                s += Math.max(Math.max(c.high - c.low, Math.abs(c.high - p.close)), Math.abs(c.low - p.close));
            }
            vals.add(s / period);
        }
        return vals.isEmpty() ? atrVal(candles, period) : vals.stream().mapToDouble(v -> v).average().orElse(0);
    }

    public double rsiVal(List<Candle> candles, int period) {
        if (candles.size() < period + 1) return 50.0;
        double g = 0.0, l = 0.0;
        double[] c = candles.stream().mapToDouble(cdl -> cdl.close).toArray();
        for (int i = c.length - period; i < c.length; i++) {
            double ch = c[i] - c[i - 1];
            if (ch >= 0.0) g += ch; else l += Math.abs(ch);
        }
        double ag = g / period, al = l / period;
        if (al == 0.0) return 100.0;
        return 100.0 - (100.0 / (1.0 + ag / al));
    }

    public double adxVal(List<Candle> candles, int period) {
        if (candles.size() < period * 2) return 0.0;
        double tr = 0.0, pd = 0.0, md = 0.0;
        for (int i = candles.size() - period; i < candles.size(); i++) {
            Candle c = candles.get(i);
            Candle p = candles.get(i - 1);
            tr += Math.max(Math.max(c.high - c.low, Math.abs(c.high - p.close)), Math.abs(c.low - p.close));
            double up = c.high - p.high, dn = p.low - c.low;
            pd += up > dn && up > 0 ? up : 0.0;
            md += dn > up && dn > 0 ? dn : 0.0;
        }
        double atr = tr / period;
        double pDI = atr > 0 ? (pd / period) / atr * 100 : 0.0;
        double mDI = atr > 0 ? (md / period) / atr * 100 : 0.0;
        return (pDI + mDI) > 0 ? Math.abs(pDI - mDI) / (pDI + mDI) * 100 : 0.0;
    }

    public static double calculatePnl(String dir, double entry, double exit, int qty, double com) {
        double ev = entry * qty, xv = exit * qty;
        return "BUY".equals(dir) ? xv - ev - (ev + xv) * com : ev - xv - (ev + xv) * com;
    }

    public static double calculatePnl(String dir, double entry, double exit, int qty) {
        return calculatePnl(dir, entry, exit, qty, 0.0005);
    }
}
