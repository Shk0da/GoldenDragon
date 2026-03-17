package com.github.shk0da.GoldenDragon.utils;

import com.github.shk0da.GoldenDragon.model.TickerCandle;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

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

        public double getPrice() {
            return price;
        }

        public int getTouches() {
            return touches;
        }

        public boolean isSupport() {
            return support;
        }

        public double getVolumeAtLevel() {
            return volumeAtLevel;
        }

        public double getStrengthScore() {
            return strengthScore;
        }

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

        public int getTimestamp() {
            return timestamp;
        }

        public SignalType getType() {
            return type;
        }

        public Level getLevel() {
            return level;
        }

        public double getVolume() {
            return volume;
        }

        public double getConfirmationStrength() {
            return confirmationStrength;
        }

        public boolean isFalseBreakout() {
            return falseBreakout;
        }

        @Override
        public String toString() {
            return String.format(
                    "Signal: %s %s at %.2f (Level: %s @ %.2f, Strength: %.2f, Volume: %.0f)%s",
                    type,
                    falseBreakout ? "FALSE BREAKOUT" : "",
                    price,
                    level.isSupport() ? "Support" : "Resistance",
                    level.getPrice(),
                    confirmationStrength,
                    volume,
                    falseBreakout ? " - REJECTED" : ""
            );
        }
    }

    public static class LevelAction {
        private final boolean isShort;
        private final boolean isLong;

        public LevelAction(boolean isShort, boolean isLong) {
            this.isShort = isShort;
            this.isLong = isLong;
        }

        public boolean isShort() {
            return isShort;
        }

        public boolean isLong() {
            return isLong;
        }
    }

    // ----------------------------- Params -----------------------------
    public int levelConfirmationTouches;
    public double levelZonePercent;
    public double breakoutConfirmationPercent;
    public double falseBreakoutThreshold;
    public int confirmationCandles;
    public int maxSignalAge;
    public double volumeConfirmationThreshold;
    public double minPatternStrength;

    public GerchikUtils() {
        this(2, 0.005, 0.008, 0.002, 2, 3, 1.3, 1.0);
    }

    public GerchikUtils(int levelConfirmationTouches, double levelZonePercent,
                        double breakoutConfirmationPercent, double falseBreakoutThreshold,
                        int confirmationCandles, int maxSignalAge,
                        double volumeConfirmationThreshold, double minPatternStrength) {
        this.levelConfirmationTouches = levelConfirmationTouches;
        this.levelZonePercent = levelZonePercent;
        this.breakoutConfirmationPercent = breakoutConfirmationPercent;
        this.falseBreakoutThreshold = falseBreakoutThreshold;
        this.confirmationCandles = confirmationCandles;
        this.maxSignalAge = maxSignalAge;
        this.volumeConfirmationThreshold = volumeConfirmationThreshold;
        this.minPatternStrength = minPatternStrength;
    }

    // ----------------------------- Public API -----------------------------
    public LevelAction getLevelAction(List<TickerCandle> candles, List<Double> levels) {
        if (candles == null || candles.isEmpty() || levels == null || levels.isEmpty()) {
            return new LevelAction(false, false);
        }

        List<TradingSignal> signals = filterByTime(processMarketData(candles, levels), maxSignalAge);

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
    private List<TradingSignal> processMarketData(List<TickerCandle> candles, List<Double> priceLevels) {
        List<TradingSignal> signals = new ArrayList<>();
        List<Level> levels = identifyKeyLevels(candles, priceLevels);
        if (candles.size() < confirmationCandles + 2 || levels.isEmpty()) return signals;

        for (int i = confirmationCandles; i < candles.size(); i++) {
            TickerCandle curr = candles.get(i);
            TickerCandle prev = candles.get(i - 1);

            List<TickerCandle> upToCurr = candles.subList(0, i + 1);
            double avgVol20 = calculateAvgVolume(upToCurr, 20);
            double patternBias = calculatePatternBias(curr, upToCurr);

            for (Level level : levels) {
                // Breakout
                if (isBreakout(level, prev, curr)) {
                    boolean volOk = curr.getVolume() > avgVol20 * volumeConfirmationThreshold;

                    boolean patOk = level.isSupport()
                            ? (patternBias < -minPatternStrength)
                            : (patternBias > minPatternStrength);

                    if (volOk || patOk) {
                        double conf = calculateConfirmationStrength(level, upToCurr, i);
                        signals.add(new TradingSignal(SignalType.BREAKOUT, curr.getClose(),
                                level, conf, false, i, curr.getVolume()));
                    }
                }
                // Bounce
                if (isBounce(level, prev, curr)) {
                    boolean volOk = curr.getVolume() > avgVol20 * 1.1;

                    boolean patOk = level.isSupport()
                            ? (patternBias > minPatternStrength * 0.8)
                            : (patternBias < -minPatternStrength * 0.8);

                    if (patOk || volOk) {
                        double conf = calculateConfirmationStrength(level, upToCurr, i);
                        signals.add(new TradingSignal(SignalType.BOUNCE, curr.getClose(),
                                level, conf, false, i, curr.getVolume()));
                    }
                }
            }
        }

        markFalseBreakouts(candles, signals);
        return signals;
    }

    private void markFalseBreakouts(List<TickerCandle> candles, List<TradingSignal> signals) {
        for (TradingSignal s : signals) {
            if (s.getType() != SignalType.BREAKOUT) continue;

            int start = Math.min(candles.size() - 1, s.getTimestamp() + 1);
            int end = Math.min(candles.size(), s.getTimestamp() + Math.max(3, confirmationCandles + 2) + 1);
            double baseLevel = s.getLevel().getPrice();

            int rejections = 0;
            for (int i = start; i < end; i++) {
                TickerCandle c = candles.get(i);
                boolean returnedIntoZone = s.getLevel().isSupport()
                        ? c.getClose() > baseLevel
                        && withinPct(c.getClose(), baseLevel, falseBreakoutThreshold * 2)
                        : c.getClose() < baseLevel
                        && withinPct(c.getClose(), baseLevel, falseBreakoutThreshold * 2);

                // Смягчаем условие: достаточно одного из двух — всплеск объёма ИЛИ возврат в зону
                boolean volSpike = c.getVolume() > Math.max(
                        s.getVolume() * 1.15,
                        calculateAvgVolume(candles.subList(0, i + 1), 20) * 1.2);

                if (returnedIntoZone && volSpike) {
                    rejections++;
                } else if (returnedIntoZone) {
                    // Возврат без объёма тоже засчитываем, но с меньшим весом
                    rejections++;
                }
            }
            // Достаточно 1 подтверждения при коротком окне
            if (rejections >= 1) s.falseBreakout = true;
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

        double avg = levels.stream().mapToDouble(Level::getStrengthScore).average().orElse(0);
        return levels.stream()
                .filter(l -> l.getTouches() >= levelConfirmationTouches)
                .filter(l -> l.getStrengthScore() >= avg * 0.7)
                .sorted(Comparator.comparingDouble(Level::getStrengthScore).reversed())
                .limit(5)
                .collect(Collectors.toList());
    }

    // ----------------------------- Signals logic -----------------------------
    private boolean isBreakout(Level level, TickerCandle prev, TickerCandle curr) {
        double p = level.getPrice();
        if (level.isSupport()) {
            // Пробой поддержки вниз: закрытие ниже уровня с подтверждением
            return curr.getClose() < p * (1 - breakoutConfirmationPercent)
                    && prev.getClose() > p * (1 - levelZonePercent)
                    && bearish(curr);
        } else {
            // Пробой сопротивления вверх: закрытие выше уровня с подтверждением
            return curr.getClose() > p * (1 + breakoutConfirmationPercent)
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

    private double calculateConfirmationStrength(Level level, List<TickerCandle> candles, int idx) {
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

        double atr = calculateATR(candles, Math.min(14, candles.size()));
        atr = Math.max(1e-6, atr);
        double atrFactor = Math.min(5.0, 100.0 / atr);

        double hitFactor = Math.min(1.0, hits / (double) Math.max(1, confirmationCandles));
        return level.getStrengthScore() * volRatio * atrFactor * (0.5 + 0.5 * hitFactor);
    }

    // ----------------------------- Patterns (directional) -----------------------------
    private double calculatePatternBias(TickerCandle c, List<TickerCandle> candles) {
        double s = 0.0;

        // 1) Engulfing
        s += engulfingBias(candles);

        // 2) PinBar (только если НЕ является молотом/повешенным/падающей звездой в тренде)
        boolean isHammer = isHammerCore(c) && isDowntrend(candles);
        boolean isHanging = isHangingMan(candles, c);
        boolean isShooting = isShootingStar(candles, c);

        if (isHammer) {
            // 3) «Молот» после даунтренда — бычий сигнал (приоритет над пин-баром)
            s += 0.7;
        } else if (isHanging) {
            // 4) «Повешенный» после аптренда — медвежий сигнал
            s -= 0.7;
        } else if (isShooting) {
            // 5) «Падающая звезда» после аптренда — медвежий сигнал
            s -= 0.8;
        } else {
            // Пин-бар вне контекста тренда — используем как есть
            s += pinBarBias(c);
        }

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
        if (longDn && !longUp) return +0.9;  // бычий пин-бар
        if (longUp && !longDn) return -0.9;  // медвежий пин-бар
        return 0.0;
    }

    private boolean isHammerCore(TickerCandle c) {
        double b = body(c), up = upperWick(c), dn = lowerWick(c), rng = range(c);
        return rng > 0
                && dn >= 2 * b
                && up <= rng * 0.15   // верхняя тень не более 15% от всего диапазона
                && b <= rng * 0.35;
    }

    private boolean isHangingMan(List<TickerCandle> candles, TickerCandle c) {
        double b = body(c), up = upperWick(c), dn = lowerWick(c), rng = range(c);
        boolean hammerLike = rng > 0
                && dn >= 2 * b
                && up <= rng * 0.15
                && b <= rng * 0.35;
        return hammerLike && isUptrend(candles);
    }

    private boolean isShootingStar(List<TickerCandle> candles, TickerCandle c) {
        double b = body(c), up = upperWick(c), dn = lowerWick(c), rng = range(c);
        return rng > 0
                && up >= 2 * b
                && dn <= rng * 0.15   // нижняя тень не более 15% от диапазона
                && b <= rng * 0.35
                && isUptrend(candles);
    }

    // ----------------------------- Indicators -----------------------------
    private double calculateATR(List<TickerCandle> candles, int period) {
        int n = candles.size();
        period = Math.min(period, n - 1);
        if (period <= 0) return 0;

        double sumTR = 0;
        for (int i = n - period; i < n; i++) {
            TickerCandle curr = candles.get(i);
            double prevClose = candles.get(i - 1).getClose();
            double tr = Math.max(curr.getHigh() - curr.getLow(),
                    Math.max(Math.abs(curr.getHigh() - prevClose),
                            Math.abs(curr.getLow() - prevClose)));
            sumTR += tr;
        }
        return sumTR / period;
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

    private static boolean bullish(TickerCandle c) {
        return c.getClose() > c.getOpen();
    }

    private static boolean bearish(TickerCandle c) {
        return c.getClose() < c.getOpen();
    }

    private static double range(TickerCandle c) {
        return c.getHigh() - c.getLow();
    }

    private static double body(TickerCandle c) {
        return Math.abs(c.getClose() - c.getOpen());
    }

    private static double upperWick(TickerCandle c) {
        return c.getHigh() - Math.max(c.getOpen(), c.getClose());
    }

    private static double lowerWick(TickerCandle c) {
        return Math.min(c.getOpen(), c.getClose()) - c.getLow();
    }

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
                "levelConfirmationTouches=" + levelConfirmationTouches +
                ", levelZonePercent=" + levelZonePercent +
                ", breakoutConfirmationPercent=" + breakoutConfirmationPercent +
                ", falseBreakoutThreshold=" + falseBreakoutThreshold +
                ", confirmationCandles=" + confirmationCandles +
                ", maxSignalAge=" + maxSignalAge +
                ", volumeConfirmationThreshold=" + volumeConfirmationThreshold +
                ", minPatternStrength=" + minPatternStrength +
                '}';
    }
}
