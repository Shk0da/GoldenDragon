package com.github.shk0da.GoldenDragon.utils;

import com.github.shk0da.GoldenDragon.model.TickerCandle;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import org.apache.commons.lang3.tuple.Pair;


import static com.github.shk0da.GoldenDragon.utils.GerchikUtils.SignalType.BOUNCE;
import static com.github.shk0da.GoldenDragon.utils.GerchikUtils.SignalType.BREAKOUT;

public class GerchikUtils {

    public enum SignalType {
        BOUNCE, BREAKOUT
    }

    public static class Level {

        private final double price;
        private int touches;
        private final boolean isSupport;
        private double volumeAtLevel;
        private double strengthScore;

        public Level(double price, int touches, boolean isSupport, double volumeAtLevel) {
            this.price = price;
            this.touches = touches;
            this.isSupport = isSupport;
            this.volumeAtLevel = volumeAtLevel;
            this.strengthScore = touches * volumeAtLevel;
        }

        public double getPrice() {
            return price;
        }

        public int getTouches() {
            return touches;
        }

        public boolean isSupport() {
            return isSupport;
        }

        public double getVolumeAtLevel() {
            return volumeAtLevel;
        }

        public double getStrengthScore() {
            return strengthScore;
        }

        public void updateStrength() {
            this.strengthScore = touches * volumeAtLevel;
        }
    }

    public static class TradingSignal {

        private final SignalType type;
        private final double price;
        private final Level level;
        private final double confirmationStrength;
        private boolean isFalseBreakout;
        private final int timestamp;

        public TradingSignal(SignalType type, double price, Level level,
                             double confirmationStrength, boolean isFalseBreakout, int timestamp) {
            this.type = type;
            this.price = price;
            this.level = level;
            this.confirmationStrength = confirmationStrength;
            this.isFalseBreakout = isFalseBreakout;
            this.timestamp = timestamp;
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
            return isFalseBreakout;
        }

        @Override
        public String toString() {
            return String.format("Signal: %s %s at %.2f (Level: %s @ %.2f, Strength: %.2f)%s",
                    type,
                    isFalseBreakout ? "FALSE BREAKOUT" : "",
                    price,
                    level.isSupport ? "Support" : "Resistance",
                    level.price,
                    confirmationStrength,
                    isFalseBreakout ? " - REJECTED" : "");
        }
    }

    private int levelConfirmationTouches = 2;
    private double levelZonePercent = 0.0075;
    private double breakoutConfirmationPercent = 0.01;
    private double falseBreakoutThreshold = 0.00025;
    private double volumeMultiplier = 0.65;
    private int confirmationCandles = 3;
    private int maxSignalAge = 5;
    private double volumeConfirmationThreshold = 1.2;

    public GerchikUtils() {
    }

    public GerchikUtils(int levelConfirmationTouches, double levelZonePercent, double breakoutConfirmationPercent,
                        double falseBreakoutThreshold, double volumeMultiplier, int confirmationCandles, int maxSignalAge,
                        double volumeConfirmationThreshold) {
        this.levelConfirmationTouches = levelConfirmationTouches;
        this.levelZonePercent = levelZonePercent;
        this.breakoutConfirmationPercent = breakoutConfirmationPercent;
        this.falseBreakoutThreshold = falseBreakoutThreshold;
        this.volumeMultiplier = volumeMultiplier;
        this.confirmationCandles = confirmationCandles;
        this.maxSignalAge = maxSignalAge;
        this.volumeConfirmationThreshold = volumeConfirmationThreshold;
    }

    public Pair<Boolean, Boolean> getLevelAction(List<TickerCandle> candles, List<Double> levels) {
        List<TradingSignal> signals = processMarketData(candles, levels);

        signals = filterByTime(signals, maxSignalAge);

        boolean longSignal = signals.stream()
                .filter(s -> !s.isFalseBreakout())
                .anyMatch(s -> (s.getLevel().isSupport() && s.getType() == SignalType.BOUNCE) ||
                        (!s.getLevel().isSupport() && s.getType() == SignalType.BREAKOUT));

        boolean shortSignal = signals.stream()
                .filter(s -> !s.isFalseBreakout())
                .anyMatch(s -> (!s.getLevel().isSupport() && s.getType() == SignalType.BOUNCE) ||
                        (s.getLevel().isSupport() && s.getType() == SignalType.BREAKOUT));

        return Pair.of(longSignal, shortSignal);
    }

    private List<TradingSignal> processMarketData(List<TickerCandle> candles, List<Double> priceLevels) {
        List<TradingSignal> signals = new ArrayList<>();
        List<Level> levels = identifyKeyLevels(candles, priceLevels);

        if (candles.size() < confirmationCandles + 1 || levels.isEmpty()) {
            return signals;
        }

        for (Level level : levels) {
            checkBreakoutsAndBounces(candles, level, signals);
        }

        filterFalseBreakouts(candles, signals);

        return signals;
    }

    private void checkBreakoutsAndBounces(List<TickerCandle> candles, Level level, List<TradingSignal> signals) {
        int now = candles.size() - 1;
        TickerCandle currentCandle = candles.get(now);
        TickerCandle prevCandle = candles.get(now - 1);

        double avgVolume = calculateAvgVolume(candles, confirmationCandles);

        boolean volumeOk = currentCandle.getVolume() > avgVolume * volumeConfirmationThreshold;

        // Проверка паттернов
        boolean isHammer = isHammer(currentCandle);
        boolean isHangingMan = isHangingMan(currentCandle, candles);
        boolean isShootingStar = isShootingStar(currentCandle, candles);
        boolean isDoji = isDoji(currentCandle);

        boolean patternOk = isHammer || isHangingMan || isShootingStar || isDoji;

        if (isBreakout(level, prevCandle, currentCandle) && volumeOk && patternOk) {
            double strength = calculateConfirmationStrength(level, candles);
            signals.add(new TradingSignal(BREAKOUT, currentCandle.getClose(), level, strength, false, now));
        }

        if (isBounce(level, prevCandle, currentCandle) && volumeOk && patternOk) {
            double strength = calculateConfirmationStrength(level, candles);
            signals.add(new TradingSignal(BOUNCE, currentCandle.getClose(), level, strength, false, now));
        }
    }

    private void filterFalseBreakouts(List<TickerCandle> candles, List<TradingSignal> signals) {
        for (TradingSignal signal : signals) {
            if (signal.getType() == SignalType.BREAKOUT) {
                int startIndex = Math.max(0, signal.getTimestamp() + 1);
                int endIndex = Math.min(candles.size(), signal.getTimestamp() + confirmationCandles + 1);

                for (int i = startIndex; i < endIndex; i++) {
                    TickerCandle candle = candles.get(i);
                    if (isFalseBreakout(signal, candle)) {
                        signal.isFalseBreakout = true;
                        break;
                    }
                }
            }
        }
    }

    private boolean isFalseBreakout(TradingSignal signal, TickerCandle candle) {
        double price = candle.getClose();
        double levelPrice = signal.level.getPrice();

        if (signal.level.isSupport()) {
            boolean priceRejection = price > levelPrice * (1 + falseBreakoutThreshold);
            boolean volumeRejection = candle.getVolume() > signal.level.getVolumeAtLevel() * 1.5;
            return priceRejection && volumeRejection;
        } else {
            boolean priceRejection = price < levelPrice * (1 - falseBreakoutThreshold);
            boolean volumeRejection = candle.getVolume() > signal.level.getVolumeAtLevel() * 1.5;
            return priceRejection && volumeRejection;
        }
    }

    private double calculateConfirmationStrength(Level level, List<TickerCandle> candles) {
        double volumeConfirmation = 0;
        int confirmations = 0;

        for (int i = Math.max(0, candles.size() - confirmationCandles); i < candles.size(); i++) {
            TickerCandle candle = candles.get(i);
            if ((level.isSupport() && candle.getLow() <= level.getPrice() * (1 + levelZonePercent)) ||
                    (!level.isSupport() && candle.getHigh() >= level.getPrice() * (1 - levelZonePercent))) {
                volumeConfirmation += candle.getVolume();
                confirmations++;
            }
        }

        double avgVolume = volumeConfirmation / Math.max(1, confirmations);
        double atr = calculateATR(candles, 14);
        return level.getStrengthScore() * avgVolume * (1 / atr);
    }

    private List<Level> identifyKeyLevels(List<TickerCandle> candles, List<Double> priceLevels) {
        List<Level> levels = new ArrayList<>();

        for (TickerCandle candle : candles) {
            double close = candle.getClose();
            double volume = candle.getVolume();

            Optional<Double> support = priceLevels.stream()
                    .filter(p -> p < close)
                    .max(Double::compareTo);

            Optional<Double> resistance = priceLevels.stream()
                    .filter(p -> p > close)
                    .min(Double::compareTo);

            support.ifPresent(p -> checkAndUpdateLevel(levels, p, true, volume));
            resistance.ifPresent(p -> checkAndUpdateLevel(levels, p, false, volume));
        }

        return filterSignificantLevels(levels);
    }

    private List<Level> filterSignificantLevels(List<Level> levels) {
        if (levels.isEmpty()) return Collections.emptyList();

        double avgStrength = levels.stream().mapToDouble(Level::getStrengthScore).average().orElse(0);
        return levels.stream()
                .filter(level -> level.getTouches() >= levelConfirmationTouches &&
                        level.getStrengthScore() >= avgStrength)
                .collect(Collectors.toList());
    }

    private void checkAndUpdateLevel(List<Level> levels, double price, boolean isSupport, double volume) {
        for (Level level : levels) {
            if (Math.abs(level.getPrice() - price) / price <= levelZonePercent && level.isSupport() == isSupport) {
                level.touches++;
                level.volumeAtLevel += volume;
                level.updateStrength();
                return;
            }
        }
        levels.add(new Level(price, 1, isSupport, volume));
    }

    private boolean isBreakout(Level level, TickerCandle prevCandle, TickerCandle currentCandle) {
        if (level.isSupport()) {
            return currentCandle.getLow() < level.getPrice() * (1 - breakoutConfirmationPercent) &&
                    prevCandle.getClose() >= level.getPrice() &&
                    currentCandle.getVolume() >= prevCandle.getVolume() * volumeMultiplier;
        } else {
            return currentCandle.getHigh() > level.getPrice() * (1 + breakoutConfirmationPercent) &&
                    prevCandle.getClose() <= level.getPrice() &&
                    currentCandle.getVolume() >= prevCandle.getVolume() * volumeMultiplier;
        }
    }

    private boolean isBounce(Level level, TickerCandle prevCandle, TickerCandle currentCandle) {
        if (level.isSupport()) {
            return prevCandle.getLow() <= level.getPrice() * (1 + levelZonePercent) &&
                    prevCandle.getLow() >= level.getPrice() * (1 - levelZonePercent) &&
                    currentCandle.getClose() > prevCandle.getClose() &&
                    currentCandle.getVolume() >= prevCandle.getVolume();
        } else {
            return prevCandle.getHigh() <= level.getPrice() * (1 + levelZonePercent) &&
                    prevCandle.getHigh() >= level.getPrice() * (1 - levelZonePercent) &&
                    currentCandle.getClose() < prevCandle.getClose() &&
                    currentCandle.getVolume() >= prevCandle.getVolume();
        }
    }

    private double calculateATR(List<TickerCandle> candles, int period) {
        if (candles.size() < period) return 0;

        double sumTR = 0;
        for (int i = candles.size() - period; i < candles.size(); i++) {
            TickerCandle c = candles.get(i);
            double prevClose = i > 0 ? candles.get(i - 1).getClose() : c.getOpen();
            double tr = Math.max(
                    Math.abs(c.getHigh() - c.getLow()),
                    Math.max(
                            Math.abs(c.getHigh() - prevClose),
                            Math.abs(c.getLow() - prevClose)
                    )
            );
            sumTR += tr;
        }

        return sumTR / period;
    }

    private double calculateAvgVolume(List<TickerCandle> candles, int period) {
        int start = Math.max(0, candles.size() - period);
        double totalVolume = 0;
        for (int i = start; i < candles.size(); i++) {
            totalVolume += candles.get(i).getVolume();
        }
        return totalVolume / Math.max(1, candles.size() - start);
    }

    private boolean isUptrend(List<TickerCandle> candles) {
        int lookback = Math.min(5, candles.size());
        double first = candles.get(candles.size() - lookback).getClose();
        double last = candles.get(candles.size() - 1).getClose();
        return last > first;
    }

    private boolean isHammer(TickerCandle candle) {
        double body = Math.abs(candle.getClose() - candle.getOpen());
        double upperWick = candle.getHigh() - Math.max(candle.getOpen(), candle.getClose());
        double lowerWick = Math.min(candle.getOpen(), candle.getClose()) - candle.getLow();

        return lowerWick >= 2 * body && upperWick <= 0.5 * body;
    }

    private boolean isHangingMan(TickerCandle candle, List<TickerCandle> candles) {
        return isHammer(candle) && isUptrend(candles);
    }

    private boolean isShootingStar(TickerCandle candle, List<TickerCandle> candles) {
        double body = Math.abs(candle.getClose() - candle.getOpen());
        double upperWick = candle.getHigh() - Math.max(candle.getOpen(), candle.getClose());
        double lowerWick = Math.min(candle.getOpen(), candle.getClose()) - candle.getLow();

        return upperWick >= 2 * body && lowerWick <= 0.5 * body && isUptrend(candles);
    }

    private boolean isDoji(TickerCandle candle) {
        double body = Math.abs(candle.getClose() - candle.getOpen());
        double range = candle.getHigh() - candle.getLow();
        return body <= range * 0.1;
    }

    private List<TradingSignal> filterByTime(List<TradingSignal> signals, int maxSignalAge) {
        int now = signals.stream()
                .mapToInt(TradingSignal::getTimestamp)
                .max()
                .orElse(0);

        return signals.stream()
                .filter(signal -> (now - signal.getTimestamp()) <= maxSignalAge)
                .collect(Collectors.toList());
    }

    @Override
    public String toString() {
        return "GerchikUtils{" +
                "levelConfirmationTouches=" + levelConfirmationTouches +
                ", levelZonePercent=" + levelZonePercent +
                ", breakoutConfirmationPercent=" + breakoutConfirmationPercent +
                ", falseBreakoutThreshold=" + falseBreakoutThreshold +
                ", volumeMultiplier=" + volumeMultiplier +
                ", confirmationCandles=" + confirmationCandles +
                ", maxSignalAge=" + maxSignalAge +
                ", volumeConfirmationThreshold=" + volumeConfirmationThreshold +
                '}';
    }
}