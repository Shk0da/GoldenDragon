package com.github.shk0da.GoldenDragon.strategy;

import com.github.shk0da.GoldenDragon.config.UnifiedTraderConfig;
import com.github.shk0da.GoldenDragon.filters.BadWeatherFilter;
import com.github.shk0da.GoldenDragon.filters.GroupConfirmationFilter;
import com.github.shk0da.GoldenDragon.filters.MarketRegimeFilter;
import com.github.shk0da.GoldenDragon.model.Candle;
import com.github.shk0da.GoldenDragon.model.Config;
import com.github.shk0da.GoldenDragon.model.Group;
import com.github.shk0da.GoldenDragon.model.Position;
import com.github.shk0da.GoldenDragon.model.TickerCandle;
import com.github.shk0da.GoldenDragon.model.TickerInfo;
import com.github.shk0da.GoldenDragon.model.TickerType;
import com.github.shk0da.GoldenDragon.model.TradingDecision;
import com.github.shk0da.GoldenDragon.repository.TickerRepository;
import com.github.shk0da.GoldenDragon.service.TCSService;
import com.github.shk0da.GoldenDragon.utils.IndicatorsUtil;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.Timestamp;
import java.text.SimpleDateFormat;
import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import ru.tinkoff.piapi.contract.v1.CandleInterval;
import ru.tinkoff.piapi.contract.v1.HistoricCandle;


import static com.github.shk0da.GoldenDragon.service.TelegramNotifyService.telegramNotifyService;
import static com.github.shk0da.GoldenDragon.utils.TimeUtils.sleep;
import static java.lang.System.out;
import static java.util.concurrent.CompletableFuture.allOf;
import static java.util.concurrent.CompletableFuture.runAsync;

public class UnifiedStrategy {

    private final Config config;
    private final TCSService tcsService;
    private final UnifiedTraderConfig unifiedTraderConfig;

    private static final ThreadLocal<SimpleDateFormat> LOG_TIME_FORMAT = ThreadLocal.withInitial(
            () -> new SimpleDateFormat("dd.MM.yyyy HH:mm:ss.SSS")
    );

    private static final ThreadLocal<SimpleDateFormat> CANDLE_TIME_FORMAT = ThreadLocal.withInitial(
            () -> new SimpleDateFormat("dd.MM.yyyy HH:mm:ss")
    );

    private static final long COOLDOWN_DURATION_MS = 5 * 60 * 1000L;
    private static final long API_CALL_DELAY_MS = 100;
    private static final Object API_LOCK = new Object();

    private static long lastApiCallTime = 0;

    private final Map<String, Long> tickerCooldown = new ConcurrentHashMap<>();
    private final Map<String, Position> positionStore = new ConcurrentHashMap<>();

    private final Map<String, String> lastSeenHourBarByTicker = new ConcurrentHashMap<>();

    private final BadWeatherFilter badWeatherFilter;
    private final MarketRegimeFilter marketRegimeFilter;
    private volatile Map<String, List<Candle>> peerCandles;

    public UnifiedStrategy(UnifiedTraderConfig unifiedTraderConfig, TCSService tcsService) {
        this(unifiedTraderConfig, tcsService, new Config());
    }

    public UnifiedStrategy(UnifiedTraderConfig unifiedTraderConfig, TCSService tcsService, Config config) {
        this.config = config;
        this.tcsService = tcsService;
        this.unifiedTraderConfig = unifiedTraderConfig;
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

    public UnifiedTraderConfig getUnifiedTraderConfig() {
        return unifiedTraderConfig;
    }

    public void setPeerCandles(Map<String, List<Candle>> peerCandles) {
        this.peerCandles = peerCandles;
    }

    public void run() {
        throttleApiCall();
        var initPortfolioCost = tcsService.getTotalPortfolioCost();
        var infoMessage = "UnifiedStrategy started. Total Portfolio Cost: " + initPortfolioCost;
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
            log("No active tickers found. UnifiedStrategy stopped.");
            return;
        }

        Map<String, Double> capitalAllocation = computeCapitalAllocation(activeTickers);

        List<CompletableFuture<Void>> tasks = new ArrayList<>();
        ExecutorService executor = Executors.newFixedThreadPool(activeTickers.size() + 1);

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
                    this.processTicker(name, tcsService, unifiedTraderConfig, allocatedBalance);
                    sleep(30_000);
                }
            }, executor));
        }

        allOf(tasks.toArray(new CompletableFuture[0])).join();

        if (!isWorkingHours()) {
            var message = "UnifiedStrategy: Not working hours! Current Time: " + new Date() + ".";
            telegramNotifyService.sendMessage(message);
            log(message);
        }
    }

    private Map<String, Double> computeCapitalAllocation(List<String> tickers) {
        Map<String, Double> weights = new HashMap<>();
        double totalWeight = 0.0;

        for (String ticker : tickers) {
            try {
                UnifiedTraderConfig.TickerParams params = unifiedTraderConfig.getTickerParams(ticker);
                double w = params.allocationWeight > 0.0 ? params.allocationWeight : 1.0;
                weights.put(ticker, w);
                totalWeight += w;
            } catch (Exception ignored) {
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

    private void refreshPeerCandles(List<String> tickers) {
        Map<String, List<Candle>> snapshot = new HashMap<>();
        String dataDir = unifiedTraderConfig.getDataDir();

        for (String ticker : tickers) {
            try {
                TickerInfo info = TickerRepository.INSTANCE.getAll().values().stream()
                        .filter(t -> t.getType() == TickerType.STOCK || t.getType() == TickerType.FEATURE)
                        .filter(t -> t.getName().equalsIgnoreCase(ticker) || t.getTicker().equalsIgnoreCase(ticker))
                        .findFirst()
                        .orElse(null);
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

    public TradingDecision decide(String ticker, List<Candle> candles, Position position, double balance) {
        return decide(ticker, candles, candles, position, balance);
    }

    public TradingDecision decide(String ticker,
                                  List<Candle> hourCandles,
                                  List<Candle> minuteCandles,
                                  Position position,
                                  double balance) {
        if (hourCandles == null || hourCandles.size() < 60 || minuteCandles == null || minuteCandles.isEmpty()) {
            return new TradingDecision("HOLD", "init");
        }

        UnifiedTraderConfig.TickerParams tpCfg = unifiedTraderConfig.getTickerParams(ticker);

        if (!tpCfg.enabled) {
            return new TradingDecision("HOLD", "ticker_disabled");
        }

        Candle cur = minuteCandles.get(minuteCandles.size() - 1);
        Position p = position;
        Group grp = Group.valueOf(unifiedTraderConfig.getTickerGroup(ticker));
        boolean useMinuteCandles = tpCfg.useMinuteCandles;

        // BUG FIX #2: candlesHeld измеряется в часовых барах (унифицировано между live и бэктестом).
        // Множитель *12 убран как нерелевантный.
        if (position.quantity > 0) {
            Double sl = position.stopLoss;
            Double tp = position.takeProfit;
            String dir = position.direction;
            int maxH = grp == Group.FX ? config.maxCandlesHoldFx : config.maxCandlesHold;

            if (sl != null && (("BUY".equals(dir) && cur.low <= sl) || ("SELL".equals(dir) && cur.high >= sl))) {
                return new TradingDecision("CLOSE", "stop_loss", 0.0, position.quantity,
                        null, null, sl, new Position(config.cooldownCandles));
            }

            if (tp != null && (("BUY".equals(dir) && cur.high >= tp) || ("SELL".equals(dir) && cur.low <= tp))) {
                return new TradingDecision("CLOSE", "take_profit", 0.0, position.quantity,
                        null, null, tp, new Position(config.cooldownCandles));
            }

            if (position.candlesHeld >= maxH) {
                return new TradingDecision("CLOSE", "expired", 0.0, position.quantity,
                        null, null, cur.close, new Position(config.cooldownCandles));
            }

            p = new Position(
                    position.direction,
                    position.entryPrice,
                    position.stopLoss,
                    position.takeProfit,
                    position.quantity,
                    position.candlesHeld,
                    position.cooldownRemaining
            );

            double ep = position.entryPrice != null ? position.entryPrice : cur.close;
            double pnlAbs = "BUY".equals(dir) ? cur.close - ep : ep - cur.close;
            double atr = atrVal(hourCandles, config.atrPeriod);

            if (atr > 0.0 && pnlAbs > 0) {
                double pnlAtr = pnlAbs / atr;
                double trMult = grp == Group.FX ? 0.7 : grp == Group.MIXED ? 0.9 : 1.0;

                if (pnlAtr >= 0.5 * trMult) {
                    double beSl = "BUY".equals(dir) ? ep + atr * 0.08 : ep - atr * 0.08;

                    if ("BUY".equals(dir) && (position.stopLoss != null ? position.stopLoss : 0.0) < beSl) {
                        p = new Position(p.direction, p.entryPrice, beSl, p.takeProfit,
                                p.quantity, p.candlesHeld, p.cooldownRemaining);
                    }
                }

                if (pnlAtr >= 1.0 * trMult) {
                    double trailSl = cur.close - atr * 0.35;

                    if ("BUY".equals(dir) && trailSl > (position.stopLoss != null ? position.stopLoss : 0.0)) {
                        p = new Position(p.direction, p.entryPrice, trailSl, p.takeProfit,
                                p.quantity, p.candlesHeld, p.cooldownRemaining);
                    }
                }

                if (pnlAtr >= 1.8 * trMult) {
                    double tightTrail = cur.close - atr * 0.20;

                    if ("BUY".equals(dir) && tightTrail > (position.stopLoss != null ? position.stopLoss : 0.0)) {
                        p = new Position(p.direction, p.entryPrice, tightTrail, p.takeProfit,
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

        if (position.quantity > 0) {
            return new TradingDecision("HOLD", "in_pos", 0.0, 0, null, null, null, p);
        }

        if (!badWeatherFilter.canTrade(hourCandles, cur.close, tpCfg.badWeatherParams)) {
            String reason = badWeatherFilter.getBlockReason(hourCandles, cur.close, tpCfg.badWeatherParams);
            return new TradingDecision("HOLD", reason != null ? "BAD_WEATHER_" + reason : "BAD_WEATHER",
                    0.0, 0, null, null, null, p);
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
            return new TradingDecision("HOLD", "REGIME_" + regimeResult.reason,
                    0.0, 0, null, null, null, p);
        }

        double dAtr = atrVal(hourCandles, config.atrPeriod);
        double avgAtr = emaAtr(hourCandles, config.atrPeriod);

        if (dAtr <= 0.0 || avgAtr <= 0.0) {
            return new TradingDecision("HOLD", "ATR0", 0.0, 0, null, null, null, p);
        }

        if (dAtr > avgAtr * config.atrSpikeThreshold) {
            return new TradingDecision("HOLD", "ATRspike", 0.0, 0, null, null, null, p);
        }

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

        boolean isBuy = signal.startsWith("TB") || signal.startsWith("FXB") || signal.startsWith("MXB");
        if (!isBuy) {
            return new TradingDecision("HOLD", "short_disabled", 0.0, 0, null, null, null, p);
        }

        String allocationGroup = tpCfg.allocationGroup;
        if (allocationGroup != null && !allocationGroup.isEmpty() && peerCandles != null) {
            if (!GroupConfirmationFilter.isConfirmed(ticker, true, peerCandles)) {
                return new TradingDecision("HOLD", "noGroupConf_" + signal,
                        0.0, 0, null, null, null, p);
            }
        }

        double rsi = rsiVal(hourCandles, config.rsiPeriod);
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

        double slMult = tpCfg.slMult;
        double tpMult = tpCfg.tpMult;
        double riskP = tpCfg.riskP;

        double slDist = dAtr * slMult;
        double tpDist = dAtr * tpMult;

        if (slDist <= 0.0 || tpDist <= 0.0) {
            return new TradingDecision("HOLD", "dist0", 0.0, 0, null, null, null, p);
        }

        double confidenceK = Math.max(0.35, regimeResult.confidence / 100.0);

        double signalStrengthK = 1.0;
        if (signal.startsWith("TB_4")) signalStrengthK = 0.75;
        if (signal.startsWith("TB_5")) signalStrengthK = 0.90;
        if (signal.startsWith("TB_6")) signalStrengthK = 1.00;
        if (signal.startsWith("MX")) signalStrengthK = Math.max(signalStrengthK, 0.85);
        if (signal.startsWith("FX")) signalStrengthK = Math.max(signalStrengthK, 0.80);

        double finalRiskMultiplier = regimeResult.positionMultiplier * confidenceK * signalStrengthK;
        double maxRisk = balance * riskP * finalRiskMultiplier;

        double maxQty = Math.floor(balance / entry);
        int qty = (int) Math.min(Math.max(1, Math.floor(maxRisk / slDist)), maxQty);

        if (qty <= 0) {
            return new TradingDecision("HOLD", "qty0", 0.0, 0, null, null, null, p);
        }

        double sl = entry - slDist;
        double tp = entry + tpDist;
        double tradeConfidence = Math.min(1.0, confidenceK * signalStrengthK);

        return new TradingDecision(
                "OPEN",
                signal,
                tradeConfidence,
                qty,
                sl,
                tp,
                entry,
                new Position("BUY", entry, sl, tp, qty, 0)
        );
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

    public String fxSignal(List<Candle> candles, List<Candle> minuteCandles) {
        if (candles.size() < 30) return null;

        double rsi = rsiVal(candles, config.rsiPeriod);
        String pat = candlePattern(minuteCandles);

        boolean extremeBuy = rsi <= config.rsiOversold;
        boolean extremeSell = rsi >= config.rsiOverbought;

        List<String> bullishPats = Arrays.asList("DOJI", "PIN_BAR_BUY", "ENGULFING_BUY", "MORNING_STAR");
        List<String> bearishPats = Arrays.asList("DOJI", "PIN_BAR_SELL", "ENGULFING_SELL", "EVENING_STAR");

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

        List<String> patternUp = Arrays.asList("PIN_BAR_BUY", "ENGULFING_BUY", "MORNING_STAR", "THREE_WHITE");
        List<String> patternDn = Arrays.asList("PIN_BAR_SELL", "ENGULFING_SELL", "EVENING_STAR", "THREE_BLACK");
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

    public double atrVal(List<Candle> candles, int period) {
        if (candles == null || candles.size() < period + 1) return 0.0;

        double sum = 0.0;
        for (int i = candles.size() - period; i < candles.size(); i++) {
            Candle c = candles.get(i);
            Candle p = candles.get(i - 1);
            sum += Math.max(
                    Math.max(c.high - c.low, Math.abs(c.high - p.close)),
                    Math.abs(c.low - p.close)
            );
        }
        return sum / period;
    }

    public double emaAtr(List<Candle> candles, int period) {
        if (candles == null || candles.isEmpty()) return 0.0;
        if (candles.size() < period + 5) return atrVal(candles, period);

        List<Double> vals = new ArrayList<>();
        for (int i = Math.max(0, candles.size() - 20); i < candles.size(); i++) {
            if (i < period + 1) continue;

            double s = 0.0;
            for (int j = i - period; j < i; j++) {
                Candle c = candles.get(j);
                Candle p = candles.get(j - 1);
                s += Math.max(
                        Math.max(c.high - c.low, Math.abs(c.high - p.close)),
                        Math.abs(c.low - p.close)
                );
            }
            vals.add(s / period);
        }

        return vals.isEmpty()
                ? atrVal(candles, period)
                : vals.stream().mapToDouble(v -> v).average().orElse(0.0);
    }

    public double rsiVal(List<Candle> candles, int period) {
        if (candles == null || candles.size() < period + 1) return 50.0;

        double g = 0.0;
        double l = 0.0;
        double[] c = candles.stream().mapToDouble(cdl -> cdl.close).toArray();

        for (int i = c.length - period; i < c.length; i++) {
            double ch = c[i] - c[i - 1];
            if (ch >= 0.0) g += ch;
            else l += Math.abs(ch);
        }

        double ag = g / period;
        double al = l / period;
        if (al == 0.0) return 100.0;

        return 100.0 - (100.0 / (1.0 + ag / al));
    }

    public double adxVal(List<Candle> candles, int period) {
        if (candles == null || candles.size() < period * 2 + 1) return 0.0;

        int n = candles.size();
        double[] tr = new double[n];
        double[] plusDM = new double[n];
        double[] minusDM = new double[n];

        for (int i = 1; i < n; i++) {
            Candle c = candles.get(i);
            Candle prev = candles.get(i - 1);

            tr[i] = Math.max(
                    Math.max(c.high - c.low, Math.abs(c.high - prev.close)),
                    Math.abs(c.low - prev.close)
            );

            double up = c.high - prev.high;
            double dn = prev.low - c.low;

            plusDM[i] = (up > dn && up > 0) ? up : 0.0;
            minusDM[i] = (dn > up && dn > 0) ? dn : 0.0;
        }

        // Wilder smoothing для TR / +DM / -DM
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
            dx[dxStart] = sum > 0 ? Math.abs(pDI - mDI) / sum * 100.0 : 0.0;
        }

        for (int i = period + 1; i < n; i++) {
            trS = trS - (trS / period) + tr[i];
            pdmS = pdmS - (pdmS / period) + plusDM[i];
            mdmS = mdmS - (mdmS / period) + minusDM[i];

            if (trS > 0) {
                double pDI = (pdmS / trS) * 100.0;
                double mDI = (mdmS / trS) * 100.0;
                double sum = pDI + mDI;
                dx[i] = sum > 0 ? Math.abs(pDI - mDI) / sum * 100.0 : 0.0;
            } else {
                dx[i] = 0.0;
            }
        }

        // ADX = Wilder smoothing of DX
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

    public void processTicker(String name,
                              TCSService tcsService,
                              UnifiedTraderConfig unifiedTraderConfig,
                              double allocatedBalance) {
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
            UnifiedTraderConfig.TickerParams tickerParams = unifiedTraderConfig.getTickerParams(name);
            if (!tickerParams.enabled) {
                log("Ticker " + name + " disabled, skipping.");
                return;
            }

            TickerInfo ticker = TickerRepository.INSTANCE.getAll().values().stream()
                    .filter(t -> t.getType() == TickerType.STOCK || t.getType() == TickerType.FEATURE)
                    .filter(t -> t.getName().equalsIgnoreCase(name) || t.getTicker().equalsIgnoreCase(name))
                    .findFirst()
                    .orElse(null);

            if (ticker == null) {
                log("Ticker " + name + " not found, skipping.");
                return;
            }

            String figi = ticker.getFigi();
            OffsetDateTime now = OffsetDateTime.now();
            log("Processing " + name + " (" + figi + ")");

            String dataDir = unifiedTraderConfig.getDataDir();
            List<Candle> candles = loadOrRefreshCandles(name, figi, dataDir, now, CandleInterval.CANDLE_INTERVAL_HOUR);

            if (candles == null || candles.isEmpty()) {
                log("No hourly candles for " + name + ", skipping.");
                return;
            }

            boolean useMinCandles = tickerParams.useMinuteCandles;
            List<Candle> minuteCandles = candles;

            if (useMinCandles) {
                minuteCandles = loadOrRefreshCandles(name, figi, dataDir, now, CandleInterval.CANDLE_INTERVAL_5_MIN);
                if (minuteCandles == null || minuteCandles.isEmpty()) {
                    log("No minute candles for " + name + ", skipping.");
                    return;
                }
            }

            Position storedPosition = positionStore.getOrDefault(name, new Position());
            Position currentPosition = storedPosition;

            if (storedPosition.quantity > 0) {
                String lastHourBar = candles.get(candles.size() - 1).time;
                String prevSeen = lastSeenHourBarByTicker.get(name);

                if (prevSeen == null || !prevSeen.equals(lastHourBar)) {
                    currentPosition = new Position(
                            storedPosition.direction,
                            storedPosition.entryPrice,
                            storedPosition.stopLoss,
                            storedPosition.takeProfit,
                            storedPosition.quantity,
                            storedPosition.candlesHeld + 1,
                            storedPosition.cooldownRemaining
                    );
                    lastSeenHourBarByTicker.put(name, lastHourBar);
                }
            } else {
                lastSeenHourBarByTicker.remove(name);
            }

            double balance;
            if (allocatedBalance > 0.0) {
                balance = allocatedBalance;
            } else {
                balance = tcsService.getAvailableCash();
            }

            TradingDecision decision = useMinCandles
                    ? decide(name, candles, minuteCandles, currentPosition, balance)
                    : decide(name, candles, currentPosition, balance);

            log("Decision for " + name + ": " + decision.action + " (" + decision.reason + ")");

            if (decision.updatedPosition != null && "HOLD".equals(decision.action)) {
                positionStore.put(name, decision.updatedPosition);
            }

            if ("OPEN".equals(decision.action)) {
                if (decision.updatedPosition == null || decision.quantity <= 0) {
                    log("Invalid OPEN decision for " + name + ", skipping.");
                    return;
                }

                boolean isBuy = "BUY".equals(decision.updatedPosition.direction);
                if (!isBuy) {
                    log("Short is disabled for " + name + ", skipping SELL.");
                    return;
                }

                double entryPrice = decision.entryPrice != null
                        ? decision.entryPrice
                        : candles.get(candles.size() - 1).close;

                int qty = decision.quantity;
                double positionValue = qty * entryPrice;

                double slPrice = decision.stopLoss != null ? decision.stopLoss : entryPrice * 0.98;
                double tpPrice = decision.takeProfit != null ? decision.takeProfit : entryPrice * 1.04;

                double slPercent = Math.abs(entryPrice - slPrice) / entryPrice * 100;
                double tpPercent = Math.abs(tpPrice - entryPrice) / entryPrice * 100;

                log("Opening BUY for " + name
                        + ": qty=" + qty
                        + ", entry=" + entryPrice
                        + ", value=" + positionValue
                        + ", SL=" + String.format("%.2f", slPercent) + "%"
                        + ", TP=" + String.format("%.2f", tpPercent) + "%");

                throttleApiCall();
                tcsService.buyByMarket(name, ticker.getType(), positionValue, tpPercent, slPercent);

                positionStore.put(name, decision.updatedPosition);
                lastSeenHourBarByTicker.put(name, candles.get(candles.size() - 1).time);

                telegramNotifyService.sendMessage("UnifiedStrategy BUY " + name
                        + ": qty=" + qty
                        + ", entry=" + entryPrice
                        + ", SL=" + String.format("%.2f", slPercent) + "%"
                        + ", TP=" + String.format("%.2f", tpPercent) + "%");
            }

            if ("CLOSE".equals(decision.action)) {
                if (storedPosition.quantity <= 0) {
                    log("CLOSE decision but no position for " + name + ", skipping.");
                    return;
                }

                log("Closing position for " + name + ": " + storedPosition.quantity +
                        " shares, direction=" + storedPosition.direction +
                        ", reason=" + decision.reason);

                boolean closed = false;
                if ("BUY".equals(storedPosition.direction)) {
                    throttleApiCall();
                    closed = tcsService.closeLongByMarket(name, ticker.getType());
                }

                if (closed) {
                    positionStore.put(name, new Position(config.cooldownCandles));
                    lastSeenHourBarByTicker.remove(name);
                    telegramNotifyService.sendMessage("UnifiedStrategy CLOSED " + name +
                            " (reason: " + decision.reason + ")");
                } else {
                    log("Failed to close position for " + name +
                            " (may not exist in broker account)");
                }
            }
        } catch (Exception ex) {
            long cooldownExpiry = System.currentTimeMillis() + COOLDOWN_DURATION_MS;
            tickerCooldown.put(name, cooldownExpiry);
            String message = "UnifiedStrategy error for " + name + ": " + ex.getMessage();
            log(message);
            telegramNotifyService.sendMessage(message);
        }
    }

    private List<Candle> loadOrRefreshCandles(String name,
                                              String figi,
                                              String dataDir,
                                              OffsetDateTime now,
                                              CandleInterval interval) {
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

    private List<Candle> readCachedCandles(String name, String dataDir, CandleInterval interval) {
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

    private boolean isCandleDataFresh(List<Candle> candles, CandleInterval interval) {
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

    private boolean isWorkingHours() {
        var calendar = new GregorianCalendar();
        var hour = calendar.get(Calendar.HOUR_OF_DAY);
        var minute = calendar.get(Calendar.MINUTE);
        return !(hour == 18 && minute >= 30 || hour >= 19);
    }

    private void throttleApiCall() {
        synchronized (API_LOCK) {
            long waitTime = API_CALL_DELAY_MS - (System.currentTimeMillis() - lastApiCallTime);
            if (waitTime > 0) {
                sleep(waitTime);
            }
            lastApiCallTime = System.currentTimeMillis();
        }
    }

    private void writeCandlesToFile(String name, String dataDir, String fileName, List<Candle> candles) {
        try {
            Path dir = Paths.get(dataDir, name);
            Files.createDirectories(dir);
            try (FileWriter writer = new FileWriter(dir.resolve(fileName).toFile())) {
                writer.write("Datetime,Open,High,Low,Close,Volume" + System.lineSeparator());
                for (Candle c : candles) {
                    writer.write(String.format(
                            "%s,%s,%s,%s,%s,%s",
                            c.time, c.open, c.high, c.low, c.close, c.volume
                    ) + System.lineSeparator());
                }
            }
        } catch (IOException ex) {
            log("Failed to write candles file for " + name + ": " + ex.getMessage());
        }
    }

    private static void log(String message) {
        out.println("[" + LOG_TIME_FORMAT.get().format(new Date()) + "] " + message);
    }
}
