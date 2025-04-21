package com.github.shk0da.GoldenDragon.utils;

import com.github.shk0da.GoldenDragon.model.TickerCandle;
import org.apache.commons.lang3.tuple.Pair;

import java.util.ArrayList;
import java.util.List;

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

        public boolean isSupport() {
            return isSupport;
        }
    }

    public static class TradingSignal {

        private final SignalType type;
        private final double price;
        private final Level level;
        private final double confirmationStrength;
        private boolean isFalseBreakout;

        public TradingSignal(SignalType type, double price, Level level,
                             double confirmationStrength, boolean isFalseBreakout) {
            this.type = type;
            this.price = price;
            this.level = level;
            this.confirmationStrength = confirmationStrength;
            this.isFalseBreakout = isFalseBreakout;
        }

        public SignalType getType() {
            return type;
        }

        public Level getLevel() {
            return level;
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

    private int levelConfirmationTouches = 0;
    private double levelZonePercent = 0.0075;
    private double breakoutConfirmationPercent = 0.01;
    private double falseBreakoutThreshold = 0.00025;
    private double volumeMultiplier = 0.65;
    private int confirmationCandles = 3;

    public GerchikUtils() {
    }

    public GerchikUtils(int levelConfirmationTouches, double levelZonePercent, double breakoutConfirmationPercent,
                        double falseBreakoutThreshold, double volumeMultiplier, int confirmationCandles) {
        this.levelConfirmationTouches = levelConfirmationTouches;
        this.levelZonePercent = levelZonePercent;
        this.breakoutConfirmationPercent = breakoutConfirmationPercent;
        this.falseBreakoutThreshold = falseBreakoutThreshold;
        this.volumeMultiplier = volumeMultiplier;
        this.confirmationCandles = confirmationCandles;
    }

    public Pair<Boolean, Boolean> getLevelAction(List<TickerCandle> candles, List<Double> levels) {
        var signals = processMarketData(candles, levels);
        var bounceLong = signals.stream()
                .filter(it -> it.getLevel().isSupport())
                .anyMatch(it -> BOUNCE.equals(it.getType()));
        var breakoutLong = signals.stream()
                .filter(it -> !it.getLevel().isSupport())
                .filter(it -> !it.isFalseBreakout)
                .anyMatch(it -> BREAKOUT.equals(it.getType()));
        var falseBreakoutShort = signals.stream()
                .filter(it -> it.getLevel().isSupport())
                .filter(it -> it.isFalseBreakout)
                .anyMatch(it -> BREAKOUT.equals(it.getType()));
        var bounceShort = signals.stream()
                .filter(it -> !it.getLevel().isSupport())
                .anyMatch(it -> BOUNCE.equals(it.getType()));
        var breakoutShort = signals.stream()
                .filter(it -> it.getLevel().isSupport())
                .filter(it -> !it.isFalseBreakout)
                .anyMatch(it -> BREAKOUT.equals(it.getType()));
        var falseBreakoutLong = signals.stream()
                .filter(it -> !it.getLevel().isSupport())
                .filter(it -> it.isFalseBreakout)
                .anyMatch(it -> BREAKOUT.equals(it.getType()));
        return Pair.of(
                (bounceLong || breakoutLong || falseBreakoutShort),
                (bounceShort || breakoutShort || falseBreakoutLong)
        );
    }

    private List<TradingSignal> processMarketData(List<TickerCandle> candles, List<Double> priceLevels) {
        List<TradingSignal> signals = new ArrayList<>();
        List<Level> levels = identifyKeyLevels(candles, priceLevels);

        if (candles.size() < confirmationCandles + 1 || levels.isEmpty()) {
            return signals;
        }

        // Проверяем последние свечи на пробои и отскоки
        for (Level level : levels) {
            checkBreakoutsAndBounces(candles, level, signals);
        }

        // Фильтрация ложных пробоев
        filterFalseBreakouts(candles, signals);

        return signals;
    }

    private void checkBreakoutsAndBounces(List<TickerCandle> candles, Level level, List<TradingSignal> signals) {
        TickerCandle currentCandle = candles.get(candles.size() - 1);
        TickerCandle prevCandle = candles.get(candles.size() - 2);

        // Проверка на пробой уровня
        if (isBreakout(level, prevCandle, currentCandle)) {
            double confirmationStrength = calculateConfirmationStrength(level, candles);
            signals.add(new TradingSignal(SignalType.BREAKOUT, currentCandle.getClose(), level, confirmationStrength, false));
        }

        // Проверка на отскок от уровня
        if (isBounce(level, prevCandle, currentCandle)) {
            double confirmationStrength = calculateConfirmationStrength(level, candles);
            signals.add(new TradingSignal(BOUNCE, currentCandle.getClose(), level, confirmationStrength, false));
        }
    }

    private void filterFalseBreakouts(List<TickerCandle> candles, List<TradingSignal> signals) {
        for (TradingSignal signal : signals) {
            if (BREAKOUT.equals(signal.type)) {
                // Проверяем последующие свечи после пробоя
                int startIndex = candles.size() - confirmationCandles - 1;
                if (startIndex < 0) startIndex = 0;

                for (int j = startIndex; j < candles.size(); j++) {
                    TickerCandle candle = candles.get(j);
                    if (isFalseBreakout(signal, candle)) {
                        signal.isFalseBreakout = true;
                        break;
                    }
                }
            }
        }
    }

    private boolean isFalseBreakout(TradingSignal signal, TickerCandle candle) {
        if (signal.level.isSupport) {
            // Для поддержки: ложный пробой если цена вернулась выше уровня
            return candle.getClose() > signal.level.price * (1 + falseBreakoutThreshold);
        } else {
            // Для сопротивления: ложный пробой если цена вернулась ниже уровня
            return candle.getClose() < signal.level.price * (1 - falseBreakoutThreshold);
        }
    }

    private double calculateConfirmationStrength(Level level, List<TickerCandle> candles) {
        // Рассчитываем силу подтверждения на основе объема и количества касаний
        double volumeConfirmation = 0;
        int confirmations = 0;

        for (int i = Math.max(0, candles.size() - confirmationCandles); i < candles.size(); i++) {
            TickerCandle candle = candles.get(i);
            if ((level.isSupport && candle.getLow() <= level.price * (1 + levelZonePercent)) ||
                    (!level.isSupport && candle.getHigh() >= level.price * (1 - levelZonePercent))) {
                volumeConfirmation += candle.getVolume();
                confirmations++;
            }
        }

        double avgVolume = volumeConfirmation / Math.max(1, confirmations);
        return level.strengthScore * avgVolume;
    }

    private List<Level> identifyKeyLevels(List<TickerCandle> candles, List<Double> priceLevels) {
        List<Level> levels = new ArrayList<>();
        for (TickerCandle candle : candles) {
            var supportLevel = 0.0; // уровень снизу
            var resistanceLevel = 0.0; // уровень сверху
            for (Double level : priceLevels) {
                if (level < candle.getClose()) {
                    supportLevel = level;
                }
                if (level > candle.getClose()) {
                    resistanceLevel = level;
                    break;
                }
            }
            checkAndUpdateLevel(levels, supportLevel, true, candle.getVolume());
            checkAndUpdateLevel(levels, resistanceLevel, false, candle.getVolume());
        }
        return filterSignificantLevels(levels);
    }

    private List<Level> filterSignificantLevels(List<Level> levels) {
        List<Level> significantLevels = new ArrayList<>();
        double avgStrength = levels.stream().mapToDouble(l -> l.strengthScore).average().orElse(0);

        for (Level level : levels) {
            if (level.touches >= levelConfirmationTouches && level.strengthScore >= avgStrength) {
                significantLevels.add(level);
            }
        }

        return significantLevels;
    }

    private void checkAndUpdateLevel(List<Level> levels, double price, boolean isSupport, double volume) {
        for (Level level : levels) {
            if (Math.abs(level.price - price)/price <= levelZonePercent && level.isSupport == isSupport) {
                level.touches++;
                level.volumeAtLevel += volume;
                level.strengthScore = level.touches * level.volumeAtLevel;
                return;
            }
        }
        levels.add(new Level(price, 1, isSupport, volume));
    }

    private boolean isBreakout(Level level, TickerCandle prevCandle, TickerCandle currentCandle) {
        if (level.isSupport) {
            return currentCandle.getClose() < level.price * (1 - breakoutConfirmationPercent) &&
                    prevCandle.getClose() >= level.price &&
                    currentCandle.getVolume() >= prevCandle.getVolume() * volumeMultiplier;
        } else {
            return currentCandle.getClose() > level.price * (1 + breakoutConfirmationPercent) &&
                    prevCandle.getClose() <= level.price &&
                    currentCandle.getVolume() >= prevCandle.getVolume() * volumeMultiplier;
        }
    }

    private boolean isBounce(Level level, TickerCandle prevCandle, TickerCandle currentCandle) {
        if (level.isSupport) {
            return prevCandle.getLow() <= level.price * (1 + levelZonePercent) &&
                    prevCandle.getLow() >= level.price * (1 - levelZonePercent) &&
                    currentCandle.getClose() > prevCandle.getClose() &&
                    currentCandle.getVolume() >= prevCandle.getVolume();
        } else {
            return prevCandle.getHigh() <= level.price * (1 + levelZonePercent) &&
                    prevCandle.getHigh() >= level.price * (1 - levelZonePercent) &&
                    currentCandle.getClose() < prevCandle.getClose() &&
                    currentCandle.getVolume() >= prevCandle.getVolume();
        }
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
                '}';
    }
}
