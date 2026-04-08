package com.github.shk0da.GoldenDragon.utils;

import com.github.shk0da.GoldenDragon.model.TickerCandle;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.Properties;
import java.util.stream.Collectors;


import static com.github.shk0da.GoldenDragon.utils.PropertiesUtils.loadProperties;

/**
 * Динамически вычисляемые параметры:
 * - maxSignalAge = на основе ATR и волатильности
 * - volumeConfirmationThreshold = на основе перцентиля объёмов
 * - breakout/falseBreakout множители = на основе volatility ratio
 * - pattern strength = на основе тренда
 */
public class GerchikUtils {

    public enum SignalType {BOUNCE, BREAKOUT}

    // ----------------------------- Model -----------------------------
    public static class Level {
        private final double price;
        private int touches;
        private final boolean support;
        private double volumeAtLevel;
        private double strengthScore;
        private final List<Integer> touchTimestamps = new ArrayList<>();

        public Level(double price, int touches, boolean isSupport, double volumeAtLevel) {
            this.price = price;
            this.touches = touches;
            this.support = isSupport;
            this.volumeAtLevel = volumeAtLevel;
            this.strengthScore = Math.max(1, touches) * Math.max(1e-9, volumeAtLevel);
        }

        public double getPrice() { return price; }
        public int getTouches() { return touches; }
        public boolean isSupport() { return support; }
        public double getStrengthScore() { return strengthScore; }

        public void addTouch(int timestamp, double volume) {
            touches++;
            volumeAtLevel += volume;
            touchTimestamps.add(timestamp);
            updateStrength();
        }

        private void updateStrength() {
            int n = touchTimestamps.size();
            int from = Math.max(0, n - 10);
            int recentTouches = n - from;
            double recencyFactor = recentTouches / (double) Math.max(1, touches);
            strengthScore = (0.6 * touches + 0.4 * recentTouches)
                    * (volumeAtLevel / Math.max(1, touches))
                    * (0.5 + 0.5 * recencyFactor);
        }
    }

    public static class TradingSignal {
        private final SignalType type;
        private final double price;
        private final Level level;
        private final double confirmationStrength;
        private boolean falseBreakout;
        private final int timestamp;
        private final double volume;

        public TradingSignal(SignalType type, double price, Level level,
                             double confirmationStrength, boolean isFalseBreakout,
                             int timestamp, double volume) {
            this.type = type;
            this.price = price;
            this.level = level;
            this.confirmationStrength = confirmationStrength;
            this.falseBreakout = isFalseBreakout;
            this.timestamp = timestamp;
            this.volume = volume;
        }

        public int getTimestamp() { return timestamp; }
        public SignalType getType() { return type; }
        public Level getLevel() { return level; }
        public double getVolume() { return volume; }
        public double getConfirmationStrength() { return confirmationStrength; }
        public boolean isFalseBreakout() { return falseBreakout; }
    }

    public static class LevelAction {
        private final boolean isShort;
        private final boolean isLong;

        public LevelAction(boolean isShort, boolean isLong) {
            this.isShort = isShort;
            this.isLong = isLong;
        }

        public boolean isShort() { return isShort; }
        public boolean isLong() { return isLong; }
    }

    // ----------------------------- ТОЛЬКО 4 ПАРАМЕТРА (вместо 22) -----------------------------
    
    /**
     * Минимальное количество касаний уровня для подтверждения.
     * Оптимально: 3-4
     */
    public final int levelConfirmationTouches;
    
    /**
     * Процентная зона вокруг уровня (в % от цены).
     * Оптимально: 0.008-0.012 (0.8%-1.2%)
     */
    public final double levelZonePercent;
    
    /**
     * Количество свечей для подтверждения сигнала.
     * Оптимально: 3-4
     */
    public final int confirmationCandles;
    
    /**
     * Минимальная сила паттерна для входа.
     * Оптимально: 0.8-1.2
     */
    public final double minPatternStrength;

    // Динамические параметры (вычисляются автоматически)
    private final int atrPeriod = 14;
    private static final int MEDIAN_ATR_LOOKBACK = 50;

    // Конструктор из properties
    public GerchikUtils(String name) {
        final Properties defaultProperties;
        final Properties presetProperties;
        try {
            defaultProperties = loadProperties();
            presetProperties = loadProperties(name + ".properties");
        } catch (IOException e) {
            throw new RuntimeException(e);
        }

        this.levelConfirmationTouches = Integer.parseInt(
                (null != presetProperties.getProperty("levelTrader.levelConfirmationTouches"))
                        ? presetProperties.getProperty("levelTrader.levelConfirmationTouches")
                        : defaultProperties.getProperty("levelTrader.levelConfirmationTouches", "3")
        );
        this.levelZonePercent = Double.parseDouble(
                (null != presetProperties.getProperty("levelTrader.levelZonePercent"))
                        ? presetProperties.getProperty("levelTrader.levelZonePercent")
                        : defaultProperties.getProperty("levelTrader.levelZonePercent", "0.01")
        );
        this.confirmationCandles = Integer.parseInt(
                (null != presetProperties.getProperty("levelTrader.confirmationCandles"))
                        ? presetProperties.getProperty("levelTrader.confirmationCandles")
                        : defaultProperties.getProperty("levelTrader.confirmationCandles", "3")
        );
        this.minPatternStrength = Double.parseDouble(
                (null != presetProperties.getProperty("levelTrader.minPatternStrength"))
                        ? presetProperties.getProperty("levelTrader.minPatternStrength")
                        : defaultProperties.getProperty("levelTrader.minPatternStrength", "1.0")
        );
    }

    // Полный конструктор
    public GerchikUtils(int levelConfirmationTouches, double levelZonePercent,
                        int confirmationCandles, double minPatternStrength) {
        this.levelConfirmationTouches = levelConfirmationTouches;
        this.levelZonePercent = levelZonePercent;
        this.confirmationCandles = confirmationCandles;
        this.minPatternStrength = minPatternStrength;
    }

    // ----------------------------- Dynamic Parameters -----------------------------

    /**
     * Динамический расчёт maxSignalAge на основе волатильности.
     * Чем выше волатильность, тем меньше возраст сигнала.
     */
    private int calculateMaxSignalAge(List<TickerCandle> candles, double currentAtr) {
        double avgPrice = candles.stream().mapToDouble(TickerCandle::getClose).average().orElse(1);
        double atrPercent = (currentAtr / avgPrice) * 100; // ATR в процентах
        
        // Высокая волатильность (>2%) = короткий возраст (5-8)
        // Средняя волатильность (1-2%) = средний возраст (8-12)
        // Низкая волатильность (<1%) = длинный возраст (12-15)
        if (atrPercent > 2.0) return 6;
        if (atrPercent > 1.0) return 10;
        return 14;
    }

    /**
     * Динамический расчёт порога объёма на основе перцентиля.
     */
    private double calculateVolumeThreshold(List<TickerCandle> candles) {
        int lookback = Math.min(50, candles.size());
        if (lookback < 5) return 2.0;
        
        List<Double> volumes = new ArrayList<>();
        for (int i = candles.size() - lookback; i < candles.size(); i++) {
            volumes.add((double) candles.get(i).getVolume());
        }
        Collections.sort(volumes);
        
        // 75-й перцентиль
        int idx = (int) (volumes.size() * 0.75);
        double percentile75 = volumes.get(idx);
        double avgVol = volumes.stream().mapToDouble(v -> v).average().orElse(1);
        
        // Порог = макс(2.0, 75-й перцентиль / средний объём)
        return Math.max(2.0, percentile75 / avgVol);
    }

    /**
     * Динамические множители ATR на основе volatility ratio.
     */
    private class AtrContext {
        final double currentAtr;
        final double medianAtr;
        final double volatilityRatio;
        final double breakoutMultiplier;
        final double falseBreakoutMultiplier;

        AtrContext(double currentAtr, double medianAtr) {
            this.currentAtr = Math.max(0.01, currentAtr);
            this.medianAtr = Math.max(0.01, medianAtr);
            this.volatilityRatio = this.currentAtr / this.medianAtr;
            
            // Автоматическая настройка множителей
            // Высокая волатильность = меньшие множители (быстрые сигналы)
            // Низкая волатильность = большие множители (медленные сигналы)
            this.breakoutMultiplier = clamp(0.5 / Math.max(0.3, volatilityRatio), 0.3, 0.8);
            this.falseBreakoutMultiplier = clamp(0.3 / Math.max(0.3, volatilityRatio), 0.2, 0.5);
        }

        private double clamp(double value, double min, double max) {
            return Math.max(min, Math.min(max, value));
        }
    }

    // ----------------------------- Public API -----------------------------

    public LevelAction getLevelAction(List<TickerCandle> candles, List<Double> levels) {
        if (candles == null || candles.isEmpty() || levels == null || levels.isEmpty()) {
            return new LevelAction(false, false);
        }

        // Динамические параметры
        double currentAtr = calculateATR(candles, Math.min(atrPeriod, candles.size() - 1));
        int dynamicMaxSignalAge = calculateMaxSignalAge(candles, currentAtr);
        double dynamicVolumeThreshold = calculateVolumeThreshold(candles);

        List<TradingSignal> signals = filterByTime(
            processMarketData(candles, levels, currentAtr, dynamicVolumeThreshold),
            dynamicMaxSignalAge
        );

        boolean longSignal = signals.stream()
                .filter(s -> !s.isFalseBreakout() && s.getConfirmationStrength() > minPatternStrength)
                .anyMatch(s -> (s.getLevel().isSupport() && s.getType() == SignalType.BOUNCE) ||
                        (!s.getLevel().isSupport() && s.getType() == SignalType.BREAKOUT));

        boolean shortSignal = signals.stream()
                .filter(s -> !s.isFalseBreakout() && s.getConfirmationStrength() > minPatternStrength)
                .anyMatch(s -> (!s.getLevel().isSupport() && s.getType() == SignalType.BOUNCE) ||
                        (s.getLevel().isSupport() && s.getType() == SignalType.BREAKOUT));

        return new LevelAction(shortSignal, longSignal);
    }

    // ----------------------------- Core -----------------------------

    private List<TradingSignal> processMarketData(List<TickerCandle> candles, List<Double> priceLevels,
                                                   double currentAtr, double volumeThreshold) {
        List<TradingSignal> signals = new ArrayList<>();
        List<Level> levels = identifyKeyLevels(candles, priceLevels);
        if (candles.size() < confirmationCandles + 2 || levels.isEmpty()) return signals;

        // Медианный ATR для контекста
        double medianAtr = calculateMedianAtr(candles);
        AtrContext atrCtx = new AtrContext(currentAtr, medianAtr);

        for (int i = confirmationCandles; i < candles.size(); i++) {
            TickerCandle curr = candles.get(i);
            TickerCandle prev = candles.get(i - 1);

            List<TickerCandle> upToCurr = candles.subList(0, i + 1);
            double avgVol20 = calculateAvgVolume(upToCurr, 20);
            double patternBias = calculatePatternBias(curr, upToCurr);

            for (Level level : levels) {
                // Breakout - строгая проверка (И объём И паттерн)
                if (isBreakout(level, prev, curr, atrCtx)) {
                    boolean volOk = curr.getVolume() > avgVol20 * volumeThreshold;
                    boolean patOk = level.isSupport()
                            ? (patternBias < -minPatternStrength)
                            : (patternBias > minPatternStrength);

                    if (volOk && patOk) {
                        double conf = calculateConfirmationStrength(level, upToCurr, i, currentAtr);
                        signals.add(new TradingSignal(SignalType.BREAKOUT, curr.getClose(),
                                level, conf, false, i, curr.getVolume()));
                    }
                }
                // Bounce - более мягкая проверка (ИЛИ объём ИЛИ паттерн)
                if (isBounce(level, prev, curr)) {
                    boolean volOk = curr.getVolume() > avgVol20 * 1.2;
                    boolean patOk = level.isSupport()
                            ? (patternBias > minPatternStrength * 0.8)
                            : (patternBias < -minPatternStrength * 0.8);

                    if (patOk || volOk) {
                        double conf = calculateConfirmationStrength(level, upToCurr, i, currentAtr);
                        signals.add(new TradingSignal(SignalType.BOUNCE, curr.getClose(),
                                level, conf, false, i, curr.getVolume()));
                    }
                }
            }
        }

        markFalseBreakouts(candles, signals, atrCtx);
        return signals;
    }

    /**
     * Детекция ложных пробоев с расширенным окном проверки.
     */
    private void markFalseBreakouts(List<TickerCandle> candles, List<TradingSignal> signals, AtrContext atrCtx) {
        for (TradingSignal s : signals) {
            if (s.getType() != SignalType.BREAKOUT) continue;

            int start = Math.min(candles.size() - 1, s.getTimestamp() + 1);
            int checkWindow = Math.max(5, confirmationCandles + 3); // Увеличенное окно
            int end = Math.min(candles.size(), s.getTimestamp() + checkWindow + 1);
            double baseLevel = s.getLevel().getPrice();
            double returnThreshold = atrCtx.currentAtr * atrCtx.falseBreakoutMultiplier;

            int rejections = 0;
            for (int i = start; i < end; i++) {
                TickerCandle c = candles.get(i);

                boolean returnedIntoZone;
                if (s.getLevel().isSupport()) {
                    returnedIntoZone = c.getClose() > baseLevel
                            && (c.getClose() - baseLevel) <= returnThreshold;
                } else {
                    returnedIntoZone = c.getClose() < baseLevel
                            && (baseLevel - c.getClose()) <= returnThreshold;
                }

                if (returnedIntoZone) rejections++;
            }

            if (rejections >= 2) s.falseBreakout = true; // Увеличенный порог
        }
    }

    // ----------------------------- Levels -----------------------------

    private List<Level> identifyKeyLevels(List<TickerCandle> candles, List<Double> priceLevels) {
        List<Level> levels = new ArrayList<>();
        for (int i = 0; i < candles.size(); i++) {
            TickerCandle c = candles.get(i);
            double close = c.getClose(), vol = c.getVolume();

            Optional<Double> support = priceLevels.stream()
                    .filter(p -> p <= close)
                    .max(Double::compareTo);
            Optional<Double> resistance = priceLevels.stream()
                    .filter(p -> p > close)
                    .min(Double::compareTo);

            final int t = i;
            support.ifPresent(p -> checkAndUpdateLevel(levels, p, true, vol, t));
            resistance.ifPresent(p -> checkAndUpdateLevel(levels, p, false, vol, t));
        }
        return filterSignificantLevels(levels);
    }

    private void checkAndUpdateLevel(List<Level> levels, double price, boolean isSupport,
                                     double volume, int timestamp) {
        for (Level lvl : levels) {
            if (lvl.isSupport() == isSupport
                    && withinPct(lvl.getPrice(), price, levelZonePercent * 2)) {
                lvl.addTouch(timestamp, volume);
                return;
            }
        }
        Level lvl = new Level(price, 0, isSupport, 0);
        lvl.addTouch(timestamp, volume);
        levels.add(lvl);
    }

    private List<Level> filterSignificantLevels(List<Level> levels) {
        if (levels.isEmpty()) return Collections.emptyList();

        // Приоритет уровням с большим количеством касаний
        return levels.stream()
                .filter(l -> l.getTouches() >= levelConfirmationTouches)
                .sorted(Comparator.comparingInt(Level::getTouches).reversed())
                .limit(5)
                .collect(Collectors.toList());
    }

    // ----------------------------- Signals logic -----------------------------

    private boolean isBreakout(Level level, TickerCandle prev, TickerCandle curr, AtrContext atrCtx) {
        double p = level.getPrice();
        double breakoutDistance = atrCtx.currentAtr * atrCtx.breakoutMultiplier;

        if (level.isSupport()) {
            return curr.getClose() < (p - breakoutDistance)
                    && prev.getClose() > p * (1 - levelZonePercent)
                    && bearish(curr);
        } else {
            return curr.getClose() > (p + breakoutDistance)
                    && prev.getClose() < p * (1 + levelZonePercent)
                    && bullish(curr);
        }
    }

    private boolean isBounce(Level level, TickerCandle prev, TickerCandle curr) {
        double p = level.getPrice();
        if (level.isSupport()) {
            boolean touched = withinPct(prev.getLow(), p, levelZonePercent);
            boolean reversal = bullish(curr)
                    && curr.getClose() > prev.getClose()
                    && curr.getLow() >= prev.getLow();
            boolean closeNear = curr.getClose() <= p * (1 + levelZonePercent * 4);
            return touched && reversal && closeNear;
        } else {
            boolean touched = withinPct(prev.getHigh(), p, levelZonePercent);
            boolean reversal = bearish(curr)
                    && curr.getClose() < prev.getClose()
                    && curr.getHigh() <= prev.getHigh();
            boolean closeNear = curr.getClose() >= p * (1 - levelZonePercent * 4);
            return touched && reversal && closeNear;
        }
    }

    private double calculateConfirmationStrength(Level level, List<TickerCandle> candles, int idx, double currentAtr) {
        int start = Math.max(0, idx - confirmationCandles + 1);
        double volSum = 0;
        int hits = 0;

        double p = level.getPrice();
        for (int i = start; i <= idx; i++) {
            TickerCandle c = candles.get(i);
            boolean near = level.isSupport()
                    ? (withinPct(c.getLow(), p, levelZonePercent)
                    || withinPct(c.getClose(), p, levelZonePercent))
                    : (withinPct(c.getHigh(), p, levelZonePercent)
                    || withinPct(c.getClose(), p, levelZonePercent));
            if (near) {
                volSum += c.getVolume();
                hits++;
            }
        }

        double confVol = volSum / Math.max(1, hits);
        double baseVol = Math.max(1e-9, calculateAvgVolume(candles, 20));
        double volRatio = Math.min(3.0, confVol / baseVol);

        // Защита от деления на ноль и искажения
        currentAtr = Math.max(0.01, currentAtr);
        double atrFactor = Math.min(5.0, 50.0 / currentAtr);

        double hitFactor = Math.min(1.0, hits / (double) Math.max(1, confirmationCandles));
        return level.getStrengthScore() * volRatio * atrFactor * (0.5 + 0.5 * hitFactor);
    }

    // ----------------------------- Patterns -----------------------------

    private double calculatePatternBias(TickerCandle c, List<TickerCandle> candles) {
        double s = 0.0;
        s += engulfingBias(candles);

        boolean isHammer = isHammerCore(c) && isDowntrend(candles);
        boolean isHanging = isHangingMan(candles, c);
        boolean isShooting = isShootingStar(candles, c);

        if (isHammer) s += 0.7;
        else if (isHanging) s -= 0.7;
        else if (isShooting) s -= 0.8;
        else s += pinBarBias(c);

        return s;
    }

    private double engulfingBias(List<TickerCandle> candles) {
        if (candles.size() < 2) return 0.0;
        TickerCandle curr = candles.get(candles.size() - 1);
        TickerCandle prev = candles.get(candles.size() - 2);

        boolean bull = bullish(curr) && bearish(prev)
                && curr.getClose() > prev.getOpen()
                && curr.getOpen() < prev.getClose();

        boolean bear = bearish(curr) && bullish(prev)
                && curr.getClose() < prev.getOpen()
                && curr.getOpen() > prev.getClose();

        if (bull) return +1.0;
        if (bear) return -1.0;
        return 0.0;
    }

    private double pinBarBias(TickerCandle c) {
        double rng = range(c);
        if (rng <= 0) return 0.0;
        double b = body(c);
        double up = upperWick(c);
        double dn = lowerWick(c);

        boolean smallBody = b < rng * 0.3;
        boolean longUp = up > rng * 0.6;
        boolean longDn = dn > rng * 0.6;

        if (!smallBody) return 0.0;
        if (longDn && !longUp) return +0.9;
        if (longUp && !longDn) return -0.9;
        return 0.0;
    }

    private boolean isHammerCore(TickerCandle c) {
        double b = body(c), up = upperWick(c), dn = lowerWick(c), rng = range(c);
        return rng > 0 && dn >= 2 * b && up <= rng * 0.15 && b <= rng * 0.35;
    }

    private boolean isHangingMan(List<TickerCandle> candles, TickerCandle c) {
        double b = body(c), up = upperWick(c), dn = lowerWick(c), rng = range(c);
        boolean hammerLike = rng > 0 && dn >= 2 * b && up <= rng * 0.15 && b <= rng * 0.35;
        return hammerLike && isUptrend(candles);
    }

    private boolean isShootingStar(List<TickerCandle> candles, TickerCandle c) {
        double b = body(c), up = upperWick(c), dn = lowerWick(c), rng = range(c);
        return rng > 0 && up >= 2 * b && dn <= rng * 0.15 && b <= rng * 0.35 && isUptrend(candles);
    }

    // ----------------------------- Indicators -----------------------------

    private double calculateATR(List<TickerCandle> candles, int period) {
        int n = candles.size();
        period = Math.min(period, n - 1);
        if (period <= 0 || n < 2) return 0.01;

        double sumTR = 0;
        for (int i = n - period; i < n; i++) {
            TickerCandle curr = candles.get(i);
            double prevClose = candles.get(i - 1).getClose();
            double tr = Math.max(curr.getHigh() - curr.getLow(),
                    Math.max(Math.abs(curr.getHigh() - prevClose),
                            Math.abs(curr.getLow() - prevClose)));
            sumTR += tr;
        }
        return Math.max(0.01, sumTR / period);
    }

    private double calculateMedianAtr(List<TickerCandle> candles) {
        int n = candles.size();
        int lookback = Math.min(MEDIAN_ATR_LOOKBACK, n - atrPeriod - 1);
        if (lookback <= 0) return calculateATR(candles, atrPeriod);

        double[] atrValues = new double[lookback];
        for (int i = 0; i < lookback; i++) {
            int endIdx = n - lookback + i;
            if (endIdx < atrPeriod + 1) {
                atrValues[i] = calculateATR(candles, atrPeriod);
                continue;
            }
            List<TickerCandle> slice = candles.subList(0, endIdx);
            atrValues[i] = calculateATR(slice, Math.min(atrPeriod, slice.size() - 1));
        }
        Arrays.sort(atrValues);
        return atrValues[lookback / 2];
    }

    private double calculateAvgVolume(List<TickerCandle> candles, int period) {
        int n = candles.size();
        if (n == 0) return 0;
        int start = Math.max(0, n - period);
        double vol = 0;
        for (int i = start; i < n; i++) vol += candles.get(i).getVolume();
        return vol / Math.max(1, n - start);
    }

    private boolean isUptrend(List<TickerCandle> candles) {
        int n = candles.size();
        if (n < 5) return false;
        int lookback = Math.min(5, n - 1);
        int up = 0;
        for (int i = n - lookback; i < n; i++) {
            if (candles.get(i).getClose() > candles.get(i - 1).getClose()) up++;
        }
        double sma20 = calculateSMA(candles, 20);
        double price = candles.get(n - 1).getClose();
        return up >= lookback / 2 && price > sma20;
    }

    private boolean isDowntrend(List<TickerCandle> candles) {
        int n = candles.size();
        if (n < 5) return false;
        int lookback = Math.min(5, n - 1);
        int down = 0;
        for (int i = n - lookback; i < n; i++) {
            if (candles.get(i).getClose() < candles.get(i - 1).getClose()) down++;
        }
        double sma20 = calculateSMA(candles, 20);
        double price = candles.get(n - 1).getClose();
        return down >= lookback / 2 && price < sma20;
    }

    private double calculateSMA(List<TickerCandle> candles, int period) {
        int n = candles.size();
        if (n == 0) return 0;
        int start = Math.max(0, n - period);
        double sum = 0;
        for (int i = start; i < n; i++) sum += candles.get(i).getClose();
        return sum / Math.max(1, n - start);
    }

    // ----------------------------- Utils -----------------------------

    private static boolean withinPct(double price, double level, double pct) {
        return Math.abs(price - level) <= Math.abs(level) * pct;
    }

    private static boolean bullish(TickerCandle c) { return c.getClose() > c.getOpen(); }
    private static boolean bearish(TickerCandle c) { return c.getClose() < c.getOpen(); }
    private static double range(TickerCandle c) { return c.getHigh() - c.getLow(); }
    private static double body(TickerCandle c) { return Math.abs(c.getClose() - c.getOpen()); }
    private static double upperWick(TickerCandle c) { return c.getHigh() - Math.max(c.getOpen(), c.getClose()); }
    private static double lowerWick(TickerCandle c) { return Math.min(c.getOpen(), c.getClose()) - c.getLow(); }

    private List<TradingSignal> filterByTime(List<TradingSignal> signals, int maxSignalAge) {
        if (signals.isEmpty()) return signals;
        int now = signals.stream().mapToInt(TradingSignal::getTimestamp).max().orElse(0);
        return signals.stream()
                .filter(s -> (now - s.getTimestamp()) <= maxSignalAge)
                .sorted(Comparator.comparingDouble(TradingSignal::getConfirmationStrength).reversed())
                .collect(Collectors.toList());
    }

    @Override
    public String toString() {
        return "GerchikUtils{" +
                "touches=" + levelConfirmationTouches +
                ", zone=" + levelZonePercent +
                ", candles=" + confirmationCandles +
                ", pattern=" + minPatternStrength;
    }
}
