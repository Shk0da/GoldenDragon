package com.github.shk0da.GoldenDragon.utils;

import com.github.shk0da.GoldenDragon.model.TickerCandle;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Properties;
import java.util.stream.Collectors;


import static com.github.shk0da.GoldenDragon.utils.PropertiesUtils.loadProperties;

public class GerchikUtils {

    public enum SignalType {
        BOUNCE,
        BREAKOUT
    }

    private enum MarketRegime {
        TRENDING_UP,
        TRENDING_DOWN,
        RANGING
    }

    public static class Level {
        private final double price;
        private final boolean support;
        private int touches;
        private double volumeAtLevel;
        private double strengthScore;
        private final List<Integer> touchTimestamps = new ArrayList<>();

        public Level(double price, boolean isSupport) {
            this.price = price;
            this.support = isSupport;
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

        public double getStrengthScore() {
            return strengthScore;
        }

        public void addTouch(int timestamp, double volume) {
            touches++;
            volumeAtLevel += volume;
            touchTimestamps.add(timestamp);

            int n = touchTimestamps.size();
            int recent = n - Math.max(0, n - 10);
            double recency = recent / (double) Math.max(1, touches);
            strengthScore = (0.6 * touches + 0.4 * recent)
                    * (volumeAtLevel / Math.max(1, touches))
                    * (0.5 + 0.5 * recency);
        }
    }

    public static class TradingSignal {
        private final SignalType type;
        private final Level level;
        private final int timestamp;
        private final double compositeScore;
        private boolean falseBreakout;

        public TradingSignal(SignalType type, Level level, int timestamp, double compositeScore) {
            this.type = type;
            this.level = level;
            this.timestamp = timestamp;
            this.compositeScore = compositeScore;
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

        public boolean isFalseBreakout() {
            return falseBreakout;
        }

        public double getCompositeScore() {
            return compositeScore;
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

    private static class MarketContext {
        final double currentAtr;
        final double levelZone;
        final double patternThreshold;
        final double volumeThreshold;
        final double breakoutMul;
        final double falseBreakoutMul;
        final int maxSignalAge;

        MarketContext(double currentAtr, double medianAtr,
                      double patternThreshold, double volumeThreshold) {
            this.currentAtr = currentAtr;
            this.volumeThreshold = volumeThreshold;
            this.patternThreshold = patternThreshold;

            double vr = currentAtr / Math.max(1e-9, medianAtr);
            this.breakoutMul = clamp(0.5 / Math.max(0.3, vr), 0.25, 0.9);
            this.falseBreakoutMul = clamp(0.3 / Math.max(0.3, vr), 0.15, 0.6);
            this.levelZone = currentAtr * 0.3;
            this.maxSignalAge = (int) clamp(8.0 / Math.max(0.3, vr), 3, 15);
        }

        static double clamp(double v, double lo, double hi) {
            return Math.max(lo, Math.min(hi, v));
        }
    }

    public int confirmationCandles;

    private static final int ATR_PERIOD = 14;
    private static final int RSI_PERIOD = 14;
    private static final int MEDIAN_LOOKBACK = 50;
    private static final int PATTERN_LOOKBACK = 30;
    private static final int VOL_PERIOD = 20;

    private static final double MIN_SCORE = 3.0;
    private static final double MAX_DIST_ATR = 1.5;
    private static final double RSI_OB = 75;
    private static final double RSI_OS = 25;
    private static final double TREND_THR = 0.6;
    private static final double PAT_FLOOR = 0.3;
    private static final double PAT_CEIL = 1.5;

    // Минимальный множитель объёма для подтверждения сигнала
    private static final double MIN_VOL_RATIO = 2.4;

    public GerchikUtils() {
        this(3);
    }

    public GerchikUtils(String name) {
        try {
            Properties def = loadProperties();
            Properties pre = loadProperties(name + ".properties");
            this.confirmationCandles = Integer.parseInt(res(pre, def, "levelTrader.confirmationCandles", "3"));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public GerchikUtils(int confirmationCandles) {
        this.confirmationCandles = confirmationCandles;
    }

    private static String res(Properties p, Properties d, String k, String fb) {
        String v = p.getProperty(k);
        return v != null ? v : d.getProperty(k, fb);
    }

    private MarketContext ctx(List<TickerCandle> c) {
        int n = c.size();
        double atr = atr(c, Math.min(ATR_PERIOD, n - 1));
        int lb = Math.min(MEDIAN_LOOKBACK, n - ATR_PERIOD - 1);
        double med = atr;
        if (lb > 0) {
            double[] vals = new double[lb];
            for (int i = 0; i < lb; i++) {
                int e = n - i;
                if (e < ATR_PERIOD + 1) break;
                vals[i] = atr(c.subList(0, e), Math.min(ATR_PERIOD, e - 1));
            }
            Arrays.sort(vals);
            med = vals[lb / 2];
        }
        return new MarketContext(atr, med, patternThreshold(c), volumeThreshold(c));
    }

    private double volumeThreshold(List<TickerCandle> candles) {
        int n = candles.size();
        int start = Math.max(0, n - VOL_PERIOD);
        int count = n - start;
        if (count < 2) return 0;

        double sum = 0, sumSq = 0;
        for (int i = start; i < n; i++) {
            double v = candles.get(i).getVolume();
            sum += v;
            sumSq += v * v;
        }
        double avg = sum / count;
        double variance = sumSq / count - avg * avg;
        return avg + Math.sqrt(Math.max(0, variance));
    }

    private double patternThreshold(List<TickerCandle> c) {
        int n = c.size();
        int h = Math.min(PATTERN_LOOKBACK, n - 1);
        if (h < 3) return PAT_FLOOR;

        double[] b = new double[h];
        for (int i = 0; i < h; i++) {
            int idx = n - 1 - i;
            b[i] = Math.abs(patternBias(c.get(idx), c.subList(0, idx + 1)));
        }
        Arrays.sort(b);
        return MarketContext.clamp(
                (b[h / 2] + b[Math.min(h - 1, (int) (h * 0.75))]) / 2.0,
                PAT_FLOOR, PAT_CEIL);
    }

    /**
     * Проверка: объём текущей свечи >= MIN_VOL_RATIO * volumeThreshold.
     * Без этого сигнал не генерируется.
     */
    private boolean volumeConfirmed(TickerCandle curr, MarketContext c) {
        return c.volumeThreshold <= 0 || curr.getVolume() >= c.volumeThreshold * MIN_VOL_RATIO;
    }

    public LevelAction getLevelAction(List<TickerCandle> candles, List<Double> levels) {
        if (candles == null || candles.isEmpty() || levels == null || levels.isEmpty()) {
            return new LevelAction(false, false);
        }

        MarketContext gc = ctx(candles);

        List<TradingSignal> sig = filterByTime(process(candles, levels), gc.maxSignalAge).stream()
                .filter(s -> !s.isFalseBreakout() && s.getCompositeScore() >= MIN_SCORE)
                .collect(Collectors.toList());

        boolean lo = sig.stream().anyMatch(s ->
                (s.getLevel().isSupport() && s.getType() == SignalType.BOUNCE)
                        || (!s.getLevel().isSupport() && s.getType() == SignalType.BREAKOUT));

        boolean sh = sig.stream().anyMatch(s ->
                (!s.getLevel().isSupport() && s.getType() == SignalType.BOUNCE)
                        || (s.getLevel().isSupport() && s.getType() == SignalType.BREAKOUT));

        return new LevelAction(sh, lo);
    }

    private List<TradingSignal> process(List<TickerCandle> candles, List<Double> priceLevels) {
        List<TradingSignal> signals = new ArrayList<>();
        if (candles.size() < confirmationCandles + 2) return signals;

        MarketContext c0 = ctx(candles);
        List<Level> levels = keyLevels(candles, priceLevels, c0.levelZone);
        if (levels.isEmpty()) return signals;

        for (int i = confirmationCandles; i < candles.size(); i++) {
            TickerCandle curr = candles.get(i);
            TickerCandle prev = candles.get(i - 1);
            List<TickerCandle> sub = candles.subList(0, i + 1);
            MarketContext lc = ctx(sub);

            // Hard-filter: объём должен быть подтверждён
            if (!volumeConfirmed(curr, lc)) continue;

            double bias = patternBias(curr, sub);
            MarketRegime reg = regime(sub);
            double rsi = rsi(sub, RSI_PERIOD);
            int cluster = cluster(sub, confirmationCandles);

            for (Level lv : levels) {
                for (SignalType st : SignalType.values()) {
                    boolean match = st == SignalType.BREAKOUT
                            ? isBreakout(lv, prev, curr, lc)
                            : isBounce(lv, prev, curr, lc);

                    if (!match) continue;

                    boolean isLong = st == SignalType.BREAKOUT ? !lv.isSupport() : lv.isSupport();
                    double score = quality(st, isLong, lv, curr, lc, bias, reg, rsi, cluster);

                    if (score >= MIN_SCORE) {
                        signals.add(new TradingSignal(st, lv, i, score));
                    }
                }
            }
        }

        markFalse(candles, signals);
        return signals;
    }

    private double quality(SignalType st, boolean isLong, Level lv,
                           TickerCandle curr, MarketContext c,
                           double bias, MarketRegime reg,
                           double rsi, int cluster) {
        return volScore(curr, c)
                + patScore(bias, isLong, c.patternThreshold)
                + trendScore(st, isLong, reg)
                + distScore(curr, lv, c)
                + momScore(isLong, rsi, cluster);
    }

    private double volScore(TickerCandle curr, MarketContext c) {
        double v = curr.getVolume();
        if (v >= c.volumeThreshold * 1.5) return 1.0;
        if (v >= c.volumeThreshold) return 0.7;
        if (v >= c.volumeThreshold * MIN_VOL_RATIO) return 0.3;
        return 0.0;
    }

    private double patScore(double bias, boolean isLong, double thr) {
        double d = isLong ? bias : -bias;
        if (d >= thr) return 1.0;
        if (d >= thr * 0.5) return 0.5;
        if (d < -thr * 0.3) return 0.0;
        return 0.2;
    }

    private double trendScore(SignalType st, boolean isLong, MarketRegime r) {
        boolean aligned = (isLong && r == MarketRegime.TRENDING_UP)
                || (!isLong && r == MarketRegime.TRENDING_DOWN);
        if (aligned) return 1.0;
        if (r == MarketRegime.RANGING) {
            return st == SignalType.BOUNCE ? 0.7 : 0.3;
        }
        return 0.0;
    }

    private double distScore(TickerCandle curr, Level lv, MarketContext c) {
        double d = Math.abs(curr.getClose() - lv.getPrice());
        double a = c.currentAtr;

        if (a * MAX_DIST_ATR <= 0) return 0.5;
        if (d <= a * 0.3) return 1.0;
        if (d <= a * 0.7) return 0.7;
        if (d <= a * MAX_DIST_ATR) return 0.4;
        return 0.0;
    }

    private double momScore(boolean isLong, double rsi, int cluster) {
        if (isLong && rsi > RSI_OB) return 0.0;
        if (!isLong && rsi < RSI_OS) return 0.0;

        double s = isLong ? (rsi < 40 ? 0.5 : 0.3) : (rsi > 60 ? 0.5 : 0.3);

        if ((isLong && cluster > 0) || (!isLong && cluster < 0)) {
            s += 0.5;
        } else if ((isLong && cluster < 0) || (!isLong && cluster > 0)) {
            s -= 0.2;
        }

        return MarketContext.clamp(s, 0, 1);
    }

    private MarketRegime regime(List<TickerCandle> c) {
        int n = c.size();
        if (n < 20) return MarketRegime.RANGING;

        double sma = sma(c, 20);
        double smaP = sma(c.subList(0, n - 5), 20);
        double slope = (sma - smaP) / Math.max(1e-9, smaP);

        int lb = Math.min(20, n - 1);
        int up = 0, dn = 0;

        for (int i = n - lb; i < n; i++) {
            if (c.get(i).getClose() > c.get(i - 1).getClose()) up++;
            else if (c.get(i).getClose() < c.get(i - 1).getClose()) dn++;
        }

        double str = Math.abs(up - dn) / (double) lb;
        boolean above = c.get(n - 1).getClose() > sma;

        if (str >= TREND_THR) {
            if (slope > 0 && above && up > dn) return MarketRegime.TRENDING_UP;
            if (slope < 0 && !above && dn > up) return MarketRegime.TRENDING_DOWN;
        }

        return MarketRegime.RANGING;
    }

    private double rsi(List<TickerCandle> c, int p) {
        int n = c.size();
        if (n < p + 1) return 50;

        double g = 0, l = 0;
        for (int i = n - p; i < n; i++) {
            double ch = c.get(i).getClose() - c.get(i - 1).getClose();
            if (ch > 0) g += ch;
            else l -= ch;
        }

        double ag = g / p;
        double al = l / p;
        return al == 0 ? 100 : 100 - 100 / (1 + ag / al);
    }

    private int cluster(List<TickerCandle> c, int lb) {
        int n = c.size();
        lb = Math.min(lb, n);
        if (lb < 2) return 0;

        int bull = 0, hl = 0, lh = 0;

        for (int i = n - lb; i < n; i++) {
            if (bullish(c.get(i))) bull++;
            if (i > n - lb) {
                if (c.get(i).getLow() > c.get(i - 1).getLow()) hl++;
                if (c.get(i).getHigh() < c.get(i - 1).getHigh()) lh++;
            }
        }

        int h = lb / 2;
        int bear = lb - bull;

        if (bull > h && hl >= h - 1) return 1;
        if (bear > h && lh >= h - 1) return -1;
        return 0;
    }

    private void markFalse(List<TickerCandle> candles, List<TradingSignal> signals) {
        for (TradingSignal s : signals) {
            if (s.getType() != SignalType.BREAKOUT) continue;

            int st = Math.min(candles.size() - 1, s.getTimestamp() + 1);
            int en = Math.min(candles.size(), s.getTimestamp() + Math.max(3, confirmationCandles + 2) + 1);
            MarketContext mc = ctx(candles.subList(0, Math.min(candles.size(), s.getTimestamp() + 1)));
            double thr = mc.currentAtr * mc.falseBreakoutMul;
            double base = s.getLevel().getPrice();

            for (int i = st; i < en; i++) {
                double cl = candles.get(i).getClose();
                boolean ret = s.getLevel().isSupport()
                        ? (cl > base && cl - base <= thr)
                        : (cl < base && base - cl <= thr);

                if (ret) {
                    s.falseBreakout = true;
                    break;
                }
            }
        }
    }

    private List<Level> keyLevels(List<TickerCandle> candles, List<Double> prices, double zone) {
        List<Level> levels = new ArrayList<>();

        for (int i = 0; i < candles.size(); i++) {
            double cl = candles.get(i).getClose();
            double vol = candles.get(i).getVolume();
            int t = i;

            prices.stream().filter(p -> p <= cl).max(Double::compareTo)
                    .ifPresent(p -> touch(levels, p, true, vol, t, zone));

            prices.stream().filter(p -> p > cl).min(Double::compareTo)
                    .ifPresent(p -> touch(levels, p, false, vol, t, zone));
        }

        if (levels.isEmpty()) return Collections.emptyList();

        List<Integer> touchCounts = levels.stream()
                .map(Level::getTouches)
                .collect(Collectors.toList());

        Collections.sort(touchCounts);
        int medianTouch = touchCounts.get(touchCounts.size() / 2);

        double avgTouch = touchCounts.stream()
                .mapToInt(Integer::intValue)
                .average()
                .orElse(1.0);

        return levels.stream()
                .filter(l -> l.getTouches() >= Math.max(medianTouch, Math.floor(avgTouch * 1.2)))
                .sorted(Comparator.comparingDouble(Level::getStrengthScore).reversed())
                .limit(5)
                .collect(Collectors.toList());
    }

    private void touch(List<Level> levels, double price, boolean sup, double vol, int ts, double zone) {
        for (Level l : levels) {
            if (l.isSupport() == sup && Math.abs(l.getPrice() - price) <= zone * 2) {
                l.addTouch(ts, vol);
                return;
            }
        }
        Level l = new Level(price, sup);
        l.addTouch(ts, vol);
        levels.add(l);
    }

    private boolean isBreakout(Level lv, TickerCandle prev, TickerCandle curr, MarketContext c) {
        double p = lv.getPrice();
        double bd = c.currentAtr * c.breakoutMul;
        double z = c.levelZone;
        return lv.isSupport()
                ? curr.getClose() < p - bd && prev.getClose() > p - z && bearish(curr)
                : curr.getClose() > p + bd && prev.getClose() < p + z && bullish(curr);
    }

    private boolean isBounce(Level lv, TickerCandle prev, TickerCandle curr, MarketContext c) {
        double p = lv.getPrice();
        double z = c.levelZone;
        double nz = z * 4;

        if (lv.isSupport()) {
            return Math.abs(prev.getLow() - p) <= z
                    && bullish(curr)
                    && curr.getClose() > prev.getClose()
                    && curr.getLow() >= prev.getLow()
                    && curr.getClose() <= p + nz;
        }

        return Math.abs(prev.getHigh() - p) <= z
                && bearish(curr)
                && curr.getClose() < prev.getClose()
                && curr.getHigh() <= prev.getHigh()
                && curr.getClose() >= p - nz;
    }

    private double patternBias(TickerCandle c, List<TickerCandle> candles) {
        double s = engulfing(candles);
        if (isHammer(c) && downtrend(candles)) return s + 0.7;
        if (isHammer(c) && uptrend(candles)) return s - 0.7;
        if (isStar(c) && uptrend(candles)) return s - 0.8;
        return s + pinBar(c);
    }

    private double engulfing(List<TickerCandle> c) {
        if (c.size() < 2) return 0;
        TickerCandle cu = c.get(c.size() - 1);
        TickerCandle pr = c.get(c.size() - 2);

        if (bullish(cu) && bearish(pr) && cu.getClose() > pr.getOpen() && cu.getOpen() < pr.getClose()) {
            return 1;
        }
        if (bearish(cu) && bullish(pr) && cu.getClose() < pr.getOpen() && cu.getOpen() > pr.getClose()) {
            return -1;
        }
        return 0;
    }

    private double pinBar(TickerCandle c) {
        double r = range(c);
        if (r <= 0 || body(c) >= r * 0.3) return 0;

        if (lowerWick(c) > r * 0.6 && upperWick(c) <= r * 0.6) return 0.9;
        if (upperWick(c) > r * 0.6 && lowerWick(c) <= r * 0.6) return -0.9;
        return 0;
    }

    private boolean isHammer(TickerCandle c) {
        double r = range(c);
        double b = body(c);
        return r > 0 && lowerWick(c) >= 2 * b && upperWick(c) <= r * 0.15 && b <= r * 0.35;
    }

    private boolean isStar(TickerCandle c) {
        double r = range(c);
        double b = body(c);
        return r > 0 && upperWick(c) >= 2 * b && lowerWick(c) <= r * 0.15 && b <= r * 0.35;
    }

    private double atr(List<TickerCandle> c, int p) {
        int n = c.size();
        p = Math.min(p, n - 1);
        if (p <= 0) return 0;

        double s = 0;
        for (int i = n - p; i < n; i++) {
            TickerCandle cu = c.get(i);
            double pc = c.get(i - 1).getClose();
            s += Math.max(cu.getHigh() - cu.getLow(),
                    Math.max(Math.abs(cu.getHigh() - pc), Math.abs(cu.getLow() - pc)));
        }
        return s / p;
    }

    private double avgVol(List<TickerCandle> c, int p) {
        int n = c.size();
        if (n == 0) return 0;
        int st = Math.max(0, n - p);
        double v = 0;
        for (int i = st; i < n; i++) {
            v += c.get(i).getVolume();
        }
        return v / Math.max(1, n - st);
    }

    private boolean uptrend(List<TickerCandle> c) {
        int n = c.size();
        if (n < 5) return false;
        int lb = Math.min(5, n - 1), up = 0;
        for (int i = n - lb; i < n; i++) {
            if (c.get(i).getClose() > c.get(i - 1).getClose()) up++;
        }
        return up >= lb / 2 && c.get(n - 1).getClose() > sma(c, 20);
    }

    private boolean downtrend(List<TickerCandle> c) {
        int n = c.size();
        if (n < 5) return false;
        int lb = Math.min(5, n - 1), dn = 0;
        for (int i = n - lb; i < n; i++) {
            if (c.get(i).getClose() < c.get(i - 1).getClose()) dn++;
        }
        return dn >= lb / 2 && c.get(n - 1).getClose() < sma(c, 20);
    }

    private double sma(List<TickerCandle> c, int p) {
        int n = c.size();
        if (n == 0) return 0;
        int st = Math.max(0, n - p);
        double s = 0;
        for (int i = st; i < n; i++) {
            s += c.get(i).getClose();
        }
        return s / Math.max(1, n - st);
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

    private List<TradingSignal> filterByTime(List<TradingSignal> signals, int maxAge) {
        if (signals.isEmpty()) return signals;
        int now = signals.stream()
                .mapToInt(TradingSignal::getTimestamp)
                .max()
                .orElse(0);
        return signals.stream()
                .filter(s -> (now - s.getTimestamp()) <= maxAge)
                .sorted(Comparator.comparingDouble(TradingSignal::getCompositeScore).reversed())
                .collect(Collectors.toList());
    }

    @Override
    public String toString() {
        return "GerchikUtils{confirmationCandles=" + confirmationCandles + '}';
    }
}
