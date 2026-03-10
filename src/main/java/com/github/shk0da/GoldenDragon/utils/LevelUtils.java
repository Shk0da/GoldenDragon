package com.github.shk0da.GoldenDragon.utils;

import com.github.shk0da.GoldenDragon.model.TickerCandle;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

public class LevelUtils {

    public static class Level {
        private final double price;
        private int touches;
        private final boolean isSupport;
        private double totalVolume;
        private double strength;

        public Level(double price, boolean isSupport) {
            this.price = price;
            this.isSupport = isSupport;
            this.touches = 1;
            this.totalVolume = 0;
        }

        public void addTouch(double volume) {
            this.touches++;
            this.totalVolume += volume;
            this.strength = touches * totalVolume;
        }

        public double getPrice() {
            return price;
        }

        public int getTouches() {
            return touches;
        }

        public double getTotalVolume() {
            return totalVolume;
        }

        public double getStrength() {
            return strength;
        }

        public boolean isSupport() {
            return isSupport;
        }

        @Override
        public String toString() {
            return String.format("Level{%.5f} | Type: %s | Touches: %d | Volume: %.0f | Strength: %.0f",
                    price, isSupport ? "Support" : "Resistance", touches, totalVolume, strength);
        }
    }

    // Параметры
    private double levelZonePercent = 0.005; // 0.5% для группировки уровней
    private int minTouchesForLevel = 2;
    private double minStrength = 1_000_000;

    public LevelUtils() {
    }

    public LevelUtils(double levelZonePercent, int minTouchesForLevel, double minStrength) {
        this.levelZonePercent = levelZonePercent;
        this.minTouchesForLevel = minTouchesForLevel;
        this.minStrength = minStrength;
    }

    /**
     * Основной метод для поиска ключевых уровней
     */
    public List<Level> identifyKeyLevels(List<TickerCandle> candles) {
        List<Level> levels = new ArrayList<>();

        for (int i = 1; i < candles.size() - 1; i++) {
            TickerCandle prev = candles.get(i - 1);
            TickerCandle curr = candles.get(i);
            TickerCandle next = candles.get(i + 1);

            // Поддержка: локальный минимум
            if (curr.getLow() < prev.getLow() && curr.getLow() < next.getLow()) {
                addLevel(levels, curr.getLow(), true, curr.getVolume());
            }

            // Сопротивление: локальный максимум
            if (curr.getHigh() > prev.getHigh() && curr.getHigh() > next.getHigh()) {
                addLevel(levels, curr.getHigh(), false, curr.getVolume());
            }
        }

        // Группируем близкие уровни
        levels = mergeNearbyLevels(levels);

        // Фильтруем по силе и количеству касаний
        return filterSignificantLevels(levels);
    }

    /**
     * Добавляет уровень или обновляет существующий
     */
    private void addLevel(List<Level> levels, double price, boolean isSupport, double volume) {
        for (Level level : levels) {
            if (Math.abs(level.getPrice() - price) / price <= levelZonePercent && level.isSupport() == isSupport) {
                level.addTouch(volume);
                return;
            }
        }
        Level newLevel = new Level(price, isSupport);
        newLevel.addTouch(volume);
        levels.add(newLevel);
    }

    /**
     * Объединяет близкие уровни
     */
    private List<Level> mergeNearbyLevels(List<Level> levels) {
        List<Level> merged = new ArrayList<>();

        for (Level level : levels) {
            boolean mergedWithExisting = false;

            for (Level mergedLevel : merged) {
                if (Math.abs(mergedLevel.getPrice() - level.getPrice()) / level.getPrice() <= levelZonePercent &&
                        mergedLevel.isSupport() == level.isSupport()) {

                    // Средневзвешенное значение цены по объему
                    double mergedPrice = (mergedLevel.getPrice() * mergedLevel.getTotalVolume() +
                            level.getPrice() * level.getTotalVolume()) /
                            (mergedLevel.getTotalVolume() + level.getTotalVolume());

                    int touches = mergedLevel.getTouches() + level.getTouches();
                    double totalVolume = mergedLevel.getTotalVolume() + level.getTotalVolume();

                    mergedLevel = new Level(mergedPrice, mergedLevel.isSupport());
                    mergedLevel.touches = touches;
                    mergedLevel.totalVolume = totalVolume;
                    mergedLevel.strength = touches * totalVolume;

                    merged.remove(mergedLevel);
                    merged.add(mergedLevel);
                    mergedWithExisting = true;
                    break;
                }
            }

            if (!mergedWithExisting) {
                merged.add(level);
            }
        }

        return merged;
    }

    /**
     * Фильтрует уровни по силе и количеству касаний
     */
    private List<Level> filterSignificantLevels(List<Level> levels) {
        return levels.stream()
                .filter(l -> l.getTouches() >= minTouchesForLevel && l.getStrength() >= minStrength)
                .sorted(Comparator.comparingDouble(Level::getStrength).reversed())
                .collect(Collectors.toList());
    }
}