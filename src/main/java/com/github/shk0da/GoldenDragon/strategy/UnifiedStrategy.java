package com.github.shk0da.GoldenDragon.strategy;

import com.github.shk0da.GoldenDragon.config.UnifiedTraderConfig;
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

    public UnifiedTraderConfig getUnifiedTraderConfig() {
        return unifiedTraderConfig;
    }

    private static final ThreadLocal<SimpleDateFormat> LOG_TIME_FORMAT = ThreadLocal.withInitial(
            () -> new SimpleDateFormat("dd.MM.yyyy HH:mm:ss.SSS")
    );
    private static final long COOLDOWN_DURATION_MS = 5 * 60 * 1000L;
    private final Map<String, Long> tickerCooldown = new ConcurrentHashMap<>();

    private static final long API_CALL_DELAY_MS = 100;
    private static final Object API_LOCK = new Object();
    private static long lastApiCallTime = 0;

    public UnifiedStrategy(UnifiedTraderConfig unifiedTraderConfig, TCSService tcsService) {
        this.config = new Config();
        this.tcsService = tcsService;
        this.unifiedTraderConfig = unifiedTraderConfig;
    }

    public void run() {
        throttleApiCall();
        var initPortfolioCost = tcsService.getTotalPortfolioCost();
        var infoMessage = "UnifiedStrategy started. Total Portfolio Cost: " + initPortfolioCost;
        telegramNotifyService.sendMessage(infoMessage);
        log(infoMessage);

        List<CompletableFuture<Void>> tasks = new ArrayList<>();
        ExecutorService executor = Executors.newFixedThreadPool(unifiedTraderConfig.getStocks().size());
        for (String name : unifiedTraderConfig.getStocks()) {
            tasks.add(runAsync(() -> {
                while (isWorkingHours()) {
                    this.processTicker(name, tcsService, unifiedTraderConfig);
                    sleep(30_000);
                }
            }, executor));
        }
        allOf(tasks.toArray(new CompletableFuture[0])).join();

        if (!isWorkingHours()) {
            var buyMessage = "UnifiedStrategy: Not working hours! Current Time: " + new Date() + ".";
            telegramNotifyService.sendMessage(buyMessage);
            log(buyMessage);
        }
    }

    public TradingDecision decide(String ticker, List<Candle> candles, Position position, double balance) {
        return decide(ticker, candles, candles, position, balance);
    }

    public TradingDecision decide(String ticker, List<Candle> hourCandles, List<Candle> minuteCandles, Position position, double balance) {
        if (hourCandles.size() < 60) return new TradingDecision("HOLD", "init");
        Candle cur = minuteCandles.get(minuteCandles.size() - 1);
        Position p = position;
        Group grp = Group.valueOf(unifiedTraderConfig.getTickerGroup(ticker));

        if (position.quantity > 0) {
            Double sl = position.stopLoss;
            Double tp = position.takeProfit;
            String dir = position.direction;
            int maxH = grp == Group.FX ? config.maxCandlesHoldFx : config.maxCandlesHold;
            if (minuteCandles != hourCandles) {
                maxH = maxH * 12;
            }

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
            double atr = atrVal(hourCandles, config.atrPeriod);

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

        double dAtr = atrVal(hourCandles, config.atrPeriod);
        double avgAtr = emaAtr(hourCandles, config.atrPeriod);
        if (dAtr <= 0.0 || avgAtr <= 0.0)
            return new TradingDecision("HOLD", "ATR0", 0.0, 0, null, null, null, p);
        if (dAtr > avgAtr * config.atrSpikeThreshold)
            return new TradingDecision("HOLD", "ATRspike", 0.0, 0, null, null, null, p);

        String signal;
        switch (grp) {
            case FX: signal = fxSignal(hourCandles, minuteCandles); break;
            case MIXED: signal = mixedSignal(hourCandles, minuteCandles); break;
            default: signal = trendSignal(hourCandles, minuteCandles);
        }
        if (signal == null)
            return new TradingDecision("HOLD", "noSig", 0.0, 0, null, null, null, p);

        double entry = cur.close;

        UnifiedTraderConfig.TickerParams tpCfg = unifiedTraderConfig.getTickerParams(ticker);
        double slMult = tpCfg.slMult;
        double tpMult = tpCfg.tpMult;
        double riskP = tpCfg.riskP;

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

        return new TradingDecision("OPEN", signal, 0.7, qty, sl, tp, entry, new Position(dirName, entry, sl, tp, qty, 0));
    }

    public void processTicker(String name, TCSService tcsService, UnifiedTraderConfig unifiedTraderConfig) {
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
            TickerInfo ticker = TickerRepository.INSTANCE.getAll().values().stream()
                    .filter(t -> t.getType() == TickerType.STOCK || t.getType() == TickerType.FEATURE)
                    .filter(t -> t.getName().equalsIgnoreCase(name) || t.getTicker().equalsIgnoreCase(name))
                    .findFirst().orElse(null);
            if (ticker == null) {
                log("Ticker " + name + " not found, skipping.");
                return;
            }

            String figi = ticker.getFigi();
            OffsetDateTime now = OffsetDateTime.now();
            log("Processing " + name + " (" + figi + ")");

            List<Candle> candles = null;
            String dataDir = unifiedTraderConfig.getDataDir();
            SimpleDateFormat df = new SimpleDateFormat("dd.MM.yyyy HH:mm:ss");

            File candleFile = new File(dataDir + "/" + name + "/candlesHOUR.txt");
            if (candleFile.exists()) {
                List<TickerCandle> cached = DataCollector.readCandlesFile(name, dataDir, CandleInterval.CANDLE_INTERVAL_HOUR);
                if (!cached.isEmpty()) {
                    candles = new ArrayList<>();
                    for (TickerCandle tc : cached) {
                        candles.add(new Candle(tc.getDate(), tc.getOpen(), tc.getHigh(), tc.getLow(), tc.getClose(), tc.getVolume()));
                    }
                    try {
                        String lastTimeStr = candles.get(candles.size() - 1).time;
                        Date lastCandleDate = df.parse(lastTimeStr);
                        Calendar lastCal = Calendar.getInstance();
                        lastCal.setTime(lastCandleDate);
                        Calendar nowCal = Calendar.getInstance();

                        boolean isCurrentHour = lastCal.get(Calendar.YEAR) == nowCal.get(Calendar.YEAR)
                                && lastCal.get(Calendar.DAY_OF_YEAR) == nowCal.get(Calendar.DAY_OF_YEAR)
                                && lastCal.get(Calendar.HOUR_OF_DAY) == nowCal.get(Calendar.HOUR_OF_DAY);

                        if (isCurrentHour) {
                            log("Cached candles are current for " + name + " (" + candles.size() + " candles)");
                        } else {
                            log("Updating candles for " + name + ", last: " + lastTimeStr);
                            throttleApiCall();
                            List<HistoricCandle> newCandles = tcsService.getCandles(
                                    figi,
                                    lastCandleDate.toInstant().plusSeconds(3600),
                                    now.toInstant(),
                                    CandleInterval.CANDLE_INTERVAL_HOUR
                            );
                            if (!newCandles.isEmpty()) {
                                log("Appending " + newCandles.size() + " new candles for " + name);
                                try (FileWriter writer = new FileWriter(candleFile, true)) {
                                    for (HistoricCandle hc : newCandles) {
                                        Timestamp ts = new Timestamp(hc.getTime().getSeconds() * 1000);
                                        Candle c = new Candle(
                                                df.format(ts),
                                                IndicatorsUtil.toDouble(hc.getOpen()),
                                                IndicatorsUtil.toDouble(hc.getHigh()),
                                                IndicatorsUtil.toDouble(hc.getLow()),
                                                IndicatorsUtil.toDouble(hc.getClose()),
                                                hc.getVolume()
                                        );
                                        candles.add(c);
                                        writer.write(String.format(
                                                "%s,%s,%s,%s,%s,%s",
                                                c.time, c.open, c.high, c.low, c.close, c.volume
                                        ) + System.lineSeparator());
                                    }
                                }
                            }
                        }
                    } catch (Exception e) {
                        log("Failed to check candle freshness for " + name + ": " + e.getMessage() + ", refetching all");
                        candles = null;
                    }
                }
            }

            if (candles == null) {
                throttleApiCall();
                List<HistoricCandle> historicCandles = tcsService.getCandles(
                        figi,
                        now.minusMinutes(24 * 60),
                        now,
                        CandleInterval.CANDLE_INTERVAL_HOUR
                );
                log("Fetched " + historicCandles.size() + " candles for " + name);

                candles = new ArrayList<>();
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
                writeCandlesToFile(name, dataDir, "candlesHOUR.txt", candles);
            }

            boolean useMinCandles = unifiedTraderConfig.getTickerParams(name).useMinuteCandles;
            List<Candle> minuteCandles = null;
            if (useMinCandles) {
                File minCandleFile = new File(dataDir + "/" + name + "/candles5_MIN.txt");
                if (minCandleFile.exists()) {
                    List<TickerCandle> cached = DataCollector.readCandlesFile(name, dataDir, CandleInterval.CANDLE_INTERVAL_5_MIN);
                    if (!cached.isEmpty()) {
                        minuteCandles = new ArrayList<>();
                        for (TickerCandle tc : cached) {
                            minuteCandles.add(new Candle(tc.getDate(), tc.getOpen(), tc.getHigh(), tc.getLow(), tc.getClose(), tc.getVolume()));
                        }
                        try {
                            String lastTimeStr = minuteCandles.get(minuteCandles.size() - 1).time;
                            Date lastCandleDate = df.parse(lastTimeStr);
                            Calendar lastCal = Calendar.getInstance();
                            lastCal.setTime(lastCandleDate);
                            Calendar nowCal = Calendar.getInstance();

                            boolean isCurrent5Min = lastCal.get(Calendar.YEAR) == nowCal.get(Calendar.YEAR)
                                    && lastCal.get(Calendar.DAY_OF_YEAR) == nowCal.get(Calendar.DAY_OF_YEAR)
                                    && lastCal.get(Calendar.HOUR_OF_DAY) == nowCal.get(Calendar.HOUR_OF_DAY)
                                    && (lastCal.get(Calendar.MINUTE) / 5) == (nowCal.get(Calendar.MINUTE) / 5);

                            if (isCurrent5Min) {
                                log("Cached 5-min candles are current for " + name + " (" + minuteCandles.size() + " candles)");
                            } else {
                                log("Updating 5-min candles for " + name + ", last: " + lastTimeStr);
                                throttleApiCall();
                                List<HistoricCandle> newCandles = tcsService.getCandles(
                                        figi,
                                        lastCandleDate.toInstant().plusSeconds(300),
                                        now.toInstant(),
                                        CandleInterval.CANDLE_INTERVAL_5_MIN
                                );
                                if (!newCandles.isEmpty()) {
                                    log("Appending " + newCandles.size() + " new 5-min candles for " + name);
                                    try (FileWriter writer = new FileWriter(minCandleFile, true)) {
                                        for (HistoricCandle hc : newCandles) {
                                            Timestamp ts = new Timestamp(hc.getTime().getSeconds() * 1000);
                                            Candle c = new Candle(
                                                    df.format(ts),
                                                    IndicatorsUtil.toDouble(hc.getOpen()),
                                                    IndicatorsUtil.toDouble(hc.getHigh()),
                                                    IndicatorsUtil.toDouble(hc.getLow()),
                                                    IndicatorsUtil.toDouble(hc.getClose()),
                                                    hc.getVolume()
                                            );
                                            minuteCandles.add(c);
                                            writer.write(String.format(
                                                    "%s,%s,%s,%s,%s,%s",
                                                    c.time, c.open, c.high, c.low, c.close, c.volume
                                            ) + System.lineSeparator());
                                        }
                                    }
                                }
                            }
                        } catch (Exception e) {
                            log("Failed to check 5-min candle freshness for " + name + ": " + e.getMessage() + ", refetching all");
                            minuteCandles = null;
                        }
                    }
                }

                if (minuteCandles == null) {
                    throttleApiCall();
                    List<HistoricCandle> historicCandles = tcsService.getCandles(
                            figi,
                            now.minusMinutes(6 * 60),
                            now,
                            CandleInterval.CANDLE_INTERVAL_5_MIN
                    );
                    log("Fetched " + historicCandles.size() + " 5-min candles for " + name);

                    minuteCandles = new ArrayList<>();
                    for (HistoricCandle hc : historicCandles) {
                        Timestamp ts = new Timestamp(hc.getTime().getSeconds() * 1000);
                        minuteCandles.add(new Candle(
                                df.format(ts),
                                IndicatorsUtil.toDouble(hc.getOpen()),
                                IndicatorsUtil.toDouble(hc.getHigh()),
                                IndicatorsUtil.toDouble(hc.getLow()),
                                IndicatorsUtil.toDouble(hc.getClose()),
                                hc.getVolume()
                        ));
                    }
                    writeCandlesToFile(name, dataDir, "candles5_MIN.txt", minuteCandles);
                }
            }

            TradingDecision decision = useMinCandles
                    ? decide(name, candles, minuteCandles, new Position(), 1_000_000.0)
                    : decide(name, candles, new Position(), 1_000_000.0);
            log("Decision for " + name + ": " + decision.action + " (" + decision.reason + ")");

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

                log("Opening " + (isBuy ? "BUY" : "SELL") + " for " + name
                        + " at " + entryPrice + ", SL: " + String.format("%.2f", slPercent)
                        + "%, TP: " + String.format("%.2f", tpPercent) + "%");

                if (isBuy) {
                    throttleApiCall();
                    tcsService.buyByMarket(name, ticker.getType(), unifiedTraderConfig.getAveragePositionCost(), tpPercent, slPercent);
                } else {
                    throttleApiCall();
                    tcsService.sellByMarket(name, ticker.getType(), unifiedTraderConfig.getAveragePositionCost(), tpPercent, slPercent);
                }
                telegramNotifyService.sendMessage("UnifiedStrategy " + (isBuy ? "BUY" : "SELL") + " " + name
                        + " at " + entryPrice + ", SL: " + String.format("%.2f", slPercent)
                        + "%, TP: " + String.format("%.2f", tpPercent) + "%");
            }
        } catch (Exception ex) {
            long cooldownExpiry = System.currentTimeMillis() + COOLDOWN_DURATION_MS;
            tickerCooldown.put(name, cooldownExpiry);
            var message = "UnifiedStrategy error for " + name + ": " + ex.getMessage();
            log(message);
            telegramNotifyService.sendMessage(message);
        }
    }

    public String trendSignal(List<Candle> candles, List<Candle> minuteCandles) {
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

    public String fxSignal(List<Candle> candles, List<Candle> minuteCandles) {
        if (candles.size() < 30) return null;
        double rsi = rsiVal(candles, config.rsiPeriod);
        String pat = candlePattern(minuteCandles);

        boolean extremeBuy = rsi <= config.rsiOversold;
        boolean extremeSell = rsi >= config.rsiOverbought;

        List<String> bullishPats = Arrays.asList("DOJI", "PIN_BAR_BUY", "ENGULFING_BUY", "MORNING_STAR");
        List<String> bearishPats = Arrays.asList("DOJI", "PIN_BAR_SELL", "ENGULFING_SELL", "EVENING_STAR");

        if (extremeBuy && bullishPats.contains(pat)) return "FXB_" + (int)rsi + "_" + pat;
        if (extremeSell && bearishPats.contains(pat)) return "FXS_" + (int)rsi + "_" + pat;
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

    private static void log(String message) {
        out.println("[" + LOG_TIME_FORMAT.get().format(new Date()) + "] " + message);
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
}
