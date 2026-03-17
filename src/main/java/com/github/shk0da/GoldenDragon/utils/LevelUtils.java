package com.github.shk0da.GoldenDragon.utils;

import com.github.shk0da.GoldenDragon.model.TickerCandle;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

public class LevelUtils {

    public static class Level {
        private double price;
        private int touches;
        private final boolean isSupport;
        private double totalVolume;
        private double strength;

        public Level(double price, boolean isSupport) {
            this.price = price;
            this.isSupport = isSupport;
            this.touches = 0;
            this.totalVolume = 0;
            this.strength = 0;
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

        void mergeFrom(Level other) {
            // Средневзвешенная цена по объёму
            double combinedVolume = this.totalVolume + other.totalVolume;
            if (combinedVolume > 0) {
                this.price = (this.price * this.totalVolume + other.price * other.totalVolume) / combinedVolume;
            }
            this.touches += other.touches;
            this.totalVolume = combinedVolume;
            this.strength = this.touches * this.totalVolume;
        }

        @Override
        public String toString() {
            return String.format("Level{%.5f} | Type: %s | Touches: %d | Volume: %.0f | Strength: %.0f",
                    price, isSupport ? "Support" : "Resistance", touches, totalVolume, strength);
        }
    }

    // Параметры
    private final double levelZonePercent;
    private final int minTouchesForLevel;
    private final double minStrengthPercentile;
    // Для расширенного поиска уровней
    private final int consolidationWindow;
    private final double consolidationThreshold;

    public LevelUtils() {
        this(0.005, 2, 0.3, 5, 0.003);
    }

    /**
     * @param levelZonePercent       процент для группировки уровней (0.005 = 0.5%)
     * @param minTouchesForLevel     минимальное количество касаний
     * @param minStrengthPercentile  минимальный процентиль силы (0.3 = отсекаем нижние 30%)
     * @param consolidationWindow    окно для поиска зон консолидации
     * @param consolidationThreshold порог разброса цен для консолидации (0.3%)
     */
    public LevelUtils(double levelZonePercent, int minTouchesForLevel, double minStrengthPercentile,
                      int consolidationWindow, double consolidationThreshold) {
        this.levelZonePercent = levelZonePercent;
        this.minTouchesForLevel = minTouchesForLevel;
        this.minStrengthPercentile = minStrengthPercentile;
        this.consolidationWindow = consolidationWindow;
        this.consolidationThreshold = consolidationThreshold;
    }

    /**
     * Основной метод для поиска ключевых уровней.
     * Комбинирует несколько методов определения уровней по стратегии Герчика:
     * 1. Локальные экстремумы (с расширенным окном)
     * 2. Зоны консолидации
     * 3. Уровни, от которых были сильные отбои (длинные тени)
     */
    public List<Level> identifyKeyLevels(List<TickerCandle> candles) {
        if (candles == null || candles.size() < 3) {
            return new ArrayList<>();
        }

        List<Level> levels = new ArrayList<>();

        // 1. Локальные экстремумы (окно 2 свечи в каждую сторону для надёжности)
        findLocalExtremes(candles, levels);

        // 2. Зоны консолидации — горизонтальные уровни, где цена «топталась»
        findConsolidationZones(candles, levels);

        // 3. Уровни по длинным теням (сильные отбои)
        findWickRejections(candles, levels);

        // Группируем близкие уровни
        levels = mergeNearbyLevels(levels);

        // Фильтруем по силе и количеству касаний
        return filterSignificantLevels(levels);
    }

    /**
     * Поиск локальных экстремумов.
     * Используем окно ±2 свечи для более надёжного определения.
     */
    private void findLocalExtremes(List<TickerCandle> candles, List<Level> levels) {
        int window = 2; // смотрим 2 свечи в каждую сторону

        for (int i = window; i < candles.size() - window; i++) {
            TickerCandle curr = candles.get(i);
            boolean isLocalMin = true;
            boolean isLocalMax = true;

            for (int j = i - window; j <= i + window; j++) {
                if (j == i) continue;
                if (candles.get(j).getLow() <= curr.getLow()) isLocalMin = false;
                if (candles.get(j).getHigh() >= curr.getHigh()) isLocalMax = false;
            }

            if (isLocalMin) {
                addLevel(levels, curr.getLow(), true, curr.getVolume());
            }
            if (isLocalMax) {
                addLevel(levels, curr.getHigh(), false, curr.getVolume());
            }
        }
    }

    /**
     * Если в окне из N свечей разброс цен закрытия мал — это горизонтальный уровень.
     */
    private void findConsolidationZones(List<TickerCandle> candles, List<Level> levels) {
        for (int i = consolidationWindow; i < candles.size(); i++) {
            double sumClose = 0;
            double minClose = Double.MAX_VALUE;
            double maxClose = Double.MIN_VALUE;
            double sumVolume = 0;

            for (int j = i - consolidationWindow; j < i; j++) {
                double close = candles.get(j).getClose();
                sumClose += close;
                sumVolume += candles.get(j).getVolume();
                minClose = Math.min(minClose, close);
                maxClose = Math.max(maxClose, close);
            }

            double avgClose = sumClose / consolidationWindow;
            double spread = (maxClose - minClose) / avgClose;

            // Если разброс цен в окне меньше порога — зона консолидации
            if (spread <= consolidationThreshold && avgClose > 0) {
                // Определяем тип: если цена пришла сверху — поддержка, снизу — сопротивление
                double priceBeforeZone = (i - consolidationWindow > 0)
                        ? candles.get(i - consolidationWindow - 1).getClose()
                        : avgClose;

                boolean isSupport = priceBeforeZone > avgClose;
                addLevel(levels, avgClose, isSupport, sumVolume / consolidationWindow);
            }
        }
    }

    /**
     * Длинная нижняя тень = отбой от поддержки, длинная верхняя = отбой от сопротивления.
     */
    private void findWickRejections(List<TickerCandle> candles, List<Level> levels) {
        for (TickerCandle candle : candles) {
            double range = candle.getHigh() - candle.getLow();
            if (range <= 0) continue;

            double body = Math.abs(candle.getClose() - candle.getOpen());
            double lowerWick = Math.min(candle.getOpen(), candle.getClose()) - candle.getLow();
            double upperWick = candle.getHigh() - Math.max(candle.getOpen(), candle.getClose());

            // Длинная нижняя тень (>= 60% диапазона, тело <= 30%) — отбой от поддержки
            if (lowerWick >= range * 0.6 && body <= range * 0.3) {
                addLevel(levels, candle.getLow(), true, candle.getVolume());
            }

            // Длинная верхняя тень (>= 60% диапазона, тело <= 30%) — отбой от сопротивления
            if (upperWick >= range * 0.6 && body <= range * 0.3) {
                addLevel(levels, candle.getHigh(), false, candle.getVolume());
            }
        }
    }

    /**
     * Добавляет уровень или обновляет существующий, если цена в пределах зоны
     */
    private void addLevel(List<Level> levels, double price, boolean isSupport, double volume) {
        for (Level level : levels) {
            if (level.isSupport() == isSupport && withinZone(level.getPrice(), price)) {
                level.addTouch(volume);
                return;
            }
        }
        Level newLevel = new Level(price, isSupport);
        newLevel.addTouch(volume);
        levels.add(newLevel);
    }

    /**
     * Используем индексный проход и mergeFrom() для обновления на месте.
     */
    private List<Level> mergeNearbyLevels(List<Level> levels) {
        if (levels.size() <= 1) return levels;

        // Сортируем по цене для эффективного слияния соседних
        levels.sort(Comparator.comparingDouble(Level::getPrice));

        List<Level> merged = new ArrayList<>();
        merged.add(levels.get(0));

        for (int i = 1; i < levels.size(); i++) {
            Level current = levels.get(i);
            Level last = merged.get(merged.size() - 1);

            // Сливаем, если тот же тип и цена в пределах зоны
            if (last.isSupport() == current.isSupport() && withinZone(last.getPrice(), current.getPrice())) {
                last.mergeFrom(current);
            } else {
                merged.add(current);
            }
        }

        return merged;
    }

    /**
     * ✅ Исправление #5: фильтрация по относительному порогу (процентиль),
     * а не по абсолютному значению minStrength.
     */
    private List<Level> filterSignificantLevels(List<Level> levels) {
        if (levels.isEmpty()) return levels;

        // Вычисляем порог силы как процентиль
        List<Double> strengths = levels.stream()
                .mapToDouble(Level::getStrength)
                .sorted()
                .boxed()
                .collect(Collectors.toList());

        int percentileIdx = (int) (strengths.size() * minStrengthPercentile);
        percentileIdx = Math.min(percentileIdx, strengths.size() - 1);
        double strengthThreshold = strengths.get(percentileIdx);

        return levels.stream()
                .filter(l -> l.getTouches() >= minTouchesForLevel)
                .filter(l -> l.getStrength() >= strengthThreshold)
                .sorted(Comparator.comparingDouble(Level::getStrength).reversed())
                .collect(Collectors.toList());
    }

    private boolean withinZone(double price1, double price2) {
        double ref = Math.max(Math.abs(price1), Math.abs(price2));
        if (ref == 0) return true;
        return Math.abs(price1 - price2) / ref <= levelZonePercent;
    }
}
