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
    // Динамические параметры
    private final int extremeWindowPercent; // процент свечей для окна экстремумов
    private final double wickRatioThreshold; // порог соотношения тени к диапазону
    private final double bodyRatioThreshold; // порог соотношения тела к диапазону

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
        this(levelZonePercent, minTouchesForLevel, minStrengthPercentile, consolidationWindow,
                consolidationThreshold, 2, 0.5, 0.35);
    }

    /**
     * Полный конструктор с настройкой всех параметров.
     * @param levelZonePercent       процент для группировки уровней (0.005 = 0.5%)
     * @param minTouchesForLevel     минимальное количество касаний
     * @param minStrengthPercentile  минимальный процентиль силы (0.3 = отсекаем нижние 30%)
     * @param consolidationWindow    окно для поиска зон консолидации
     * @param consolidationThreshold порог разброса цен для консолидации (0.3%)
     * @param extremeWindowPercent   процент свечей для окна экстремумов (2 = 2% от истории)
     * @param wickRatioThreshold     порог соотношения тени к диапазону (0.5 = 50%)
     * @param bodyRatioThreshold     порог соотношения тела к диапазону (0.35 = 35%)
     */
    public LevelUtils(double levelZonePercent, int minTouchesForLevel, double minStrengthPercentile,
                      int consolidationWindow, double consolidationThreshold,
                      int extremeWindowPercent, double wickRatioThreshold, double bodyRatioThreshold) {
        this.levelZonePercent = levelZonePercent;
        this.minTouchesForLevel = minTouchesForLevel;
        this.minStrengthPercentile = minStrengthPercentile;
        this.consolidationWindow = consolidationWindow;
        this.consolidationThreshold = consolidationThreshold;
        this.extremeWindowPercent = extremeWindowPercent;
        this.wickRatioThreshold = wickRatioThreshold;
        this.bodyRatioThreshold = bodyRatioThreshold;
    }

    /**
     * Основной метод для поиска ключевых уровней.
     * Комбинирует несколько методов определения уровней по стратегии Герчика:
     * 1. Локальные экстремумы (с адаптивным окном)
     * 2. Зоны консолидации
     * 3. Уровни, от которых были сильные отбои (длинные тени)
     */
    public List<Level> identifyKeyLevels(List<TickerCandle> candles) {
        if (candles == null || candles.size() < 3) {
            return new ArrayList<>();
        }

        List<Level> levels = new ArrayList<>();

        // Предварительный расчёт среднего объёма для фильтрации шумовых экстремумов
        double avgVolume = calculateAvgVolume(candles);

        // 1. Локальные экстремумы (адаптивное окно на основе размера истории)
        findLocalExtremes(candles, levels, avgVolume);

        // 2. Зоны консолидации — горизонтальные уровни, где цена «топталась»
        findConsolidationZones(candles, levels);

        // 3. Уровни по длинным теням (сильные отбои)
        findWickRejections(candles, levels, avgVolume);

        // Группируем близкие уровни
        levels = mergeNearbyLevels(levels);

        // Фильтруем по силе и количеству касаний
        return filterSignificantLevels(levels);
    }

    /**
     * Расчёт среднего объёма за весь период.
     */
    private double calculateAvgVolume(List<TickerCandle> candles) {
        if (candles.isEmpty()) return 0;
        double sum = candles.stream().mapToDouble(TickerCandle::getVolume).sum();
        return sum / candles.size();
    }

    /**
     * Поиск локальных экстремумов.
     * Используем адаптивное окно на основе размера истории.
     * Учитываем объём — приоритет экстремумам с высоким объёмом.
     */
    private void findLocalExtremes(List<TickerCandle> candles, List<Level> levels, double avgVolume) {
        // Адаптивное окно: 2% от истории, но не менее 2 свечей
        int window = Math.max(2, candles.size() * extremeWindowPercent / 100);

        for (int i = window; i < candles.size() - window; i++) {
            TickerCandle curr = candles.get(i);
            boolean isLocalMin = true;
            boolean isLocalMax = true;

            for (int j = i - window; j <= i + window; j++) {
                if (j == i) continue;
                if (candles.get(j).getLow() <= curr.getLow()) isLocalMin = false;
                if (candles.get(j).getHigh() >= curr.getHigh()) isLocalMax = false;
            }

            // Фильтр по объёму: игнорируем экстремумы с объёмом ниже среднего
            boolean highVolume = curr.getVolume() >= avgVolume * 0.8;

            if (isLocalMin && highVolume) {
                addLevel(levels, curr.getLow(), true, curr.getVolume());
            }
            if (isLocalMax && highVolume) {
                addLevel(levels, curr.getHigh(), false, curr.getVolume());
            }
        }
    }

    /**
     * Поиск зон консолидации.
     * Если в окне из N свечей разброс цен закрытия мал — это горизонтальный уровень.
     * Улучшенная логика определения типа уровня (поддержка/сопротивление).
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
                // Определяем тип уровня по направлению подхода к зоне
                Boolean isSupport = null;
                if (i - consolidationWindow > 0) {
                    double priceBeforeZone = candles.get(i - consolidationWindow - 1).getClose();
                    // Цена пришла сверху → поддержка
                    if (priceBeforeZone > maxClose) {
                        isSupport = true;
                    }
                    // Цена пришла снизу → сопротивление
                    else if (priceBeforeZone < minClose) {
                        isSupport = false;
                    }
                }

                // Если тип не определён, используем середину диапазона
                if (isSupport == null) {
                    // Проверяем контекст: если цена выше зоны — поддержка, ниже — сопротивление
                    if (i < candles.size() - 1) {
                        double currentPrice = candles.get(i).getClose();
                        isSupport = currentPrice >= avgClose;
                    } else {
                        isSupport = true; // по умолчанию поддержка
                    }
                }

                addLevel(levels, avgClose, isSupport, sumVolume / consolidationWindow);
            }
        }
    }

    /**
     * Поиск отбоев по длинным теням.
     * Длинная нижняя тень = отбой от поддержки, длинная верхняя = отбой от сопротивления.
     * Использует динамические пороги на основе среднего диапазона свечей.
     */
    private void findWickRejections(List<TickerCandle> candles, List<Level> levels, double avgVolume) {
        // Предварительный расчёт среднего диапазона для динамических порогов
        double avgRange = candles.stream()
                .mapToDouble(c -> c.getHigh() - c.getLow())
                .average()
                .orElse(1);

        for (TickerCandle candle : candles) {
            double range = candle.getHigh() - candle.getLow();
            if (range <= 0) continue;

            double body = Math.abs(candle.getClose() - candle.getOpen());
            double lowerWick = Math.min(candle.getOpen(), candle.getClose()) - candle.getLow();
            double upperWick = candle.getHigh() - Math.max(candle.getOpen(), candle.getClose());

            // Динамические пороги на основе среднего диапазона
            double relativeLowerWick = lowerWick / avgRange;
            double relativeUpperWick = upperWick / avgRange;
            double relativeBody = body / avgRange;

            // Фильтр по объёму: отбои с высоким объёмом более значимы
            boolean highVolume = candle.getVolume() >= avgVolume * 1.2;

            // Длинная нижняя тень — отбой от поддержки
            // Тень >= 50% среднего диапазона, тело <= 35% среднего диапазона
            if (relativeLowerWick >= wickRatioThreshold && relativeBody <= bodyRatioThreshold) {
                // Усиленный уровень для высоких объёмов
                double volumeMultiplier = highVolume ? 1.5 : 1.0;
                addLevelWithMultiplier(levels, candle.getLow(), true, candle.getVolume(), volumeMultiplier);
            }

            // Длинная верхняя тень — отбой от сопротивления
            if (relativeUpperWick >= wickRatioThreshold && relativeBody <= bodyRatioThreshold) {
                double volumeMultiplier = highVolume ? 1.5 : 1.0;
                addLevelWithMultiplier(levels, candle.getHigh(), false, candle.getVolume(), volumeMultiplier);
            }
        }
    }

    /**
     * Добавляет уровень с множителем объёма (для усиления значимости).
     */
    private void addLevelWithMultiplier(List<Level> levels, double price, boolean isSupport,
                                        double volume, double multiplier) {
        for (Level level : levels) {
            if (level.isSupport() == isSupport && withinZone(level.getPrice(), price)) {
                level.addTouch(volume * multiplier);
                return;
            }
        }
        Level newLevel = new Level(price, isSupport);
        newLevel.addTouch(volume * multiplier);
        levels.add(newLevel);
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
     * Фильтрация уровней с комбинированным скорингом.
     * Учитывает: количество касаний (40%), силу уровня (40%), объём (20%).
     */
    private List<Level> filterSignificantLevels(List<Level> levels) {
        if (levels.isEmpty()) return levels;

        // Находим максимумы для нормализации
        double maxStrength = levels.stream()
                .mapToDouble(Level::getStrength)
                .max()
                .orElse(1);
        int maxTouches = levels.stream()
                .mapToInt(Level::getTouches)
                .max()
                .orElse(1);
        double maxVolume = levels.stream()
                .mapToDouble(Level::getTotalVolume)
                .max()
                .orElse(1);

        // Вычисляем скор для каждого уровня
        List<LevelScore> scored = new ArrayList<>();
        for (Level level : levels) {
            double normTouches = (double) level.getTouches() / maxTouches;
            double normStrength = level.getStrength() / maxStrength;
            double normVolume = level.getTotalVolume() / maxVolume;

            // Комбинированный скор: 40% touches + 40% strength + 20% volume
            double score = normTouches * 0.4 + normStrength * 0.4 + normVolume * 0.2;
            scored.add(new LevelScore(level, score));
        }

        // Сортируем по скору
        scored.sort(Comparator.comparingDouble(LevelScore::getScore).reversed());

        // Вычисляем порог отсечения по процентилю
        int cutoffIdx = (int) (scored.size() * minStrengthPercentile);
        cutoffIdx = Math.max(0, Math.min(cutoffIdx, scored.size() - 1));
        double scoreThreshold = scored.get(cutoffIdx).getScore();

        // Фильтруем и возвращаем уровни
        return scored.stream()
                .filter(ls -> ls.getLevel().getTouches() >= minTouchesForLevel)
                .filter(ls -> ls.getScore() >= scoreThreshold)
                .map(LevelScore::getLevel)
                .collect(Collectors.toList());
    }

    /**
     * Вспомогательный класс для хранения уровня и его скоринга.
     */
    private static class LevelScore {
        private final Level level;
        private final double score;

        LevelScore(Level level, double score) {
            this.level = level;
            this.score = score;
        }

        Level getLevel() { return level; }
        double getScore() { return score; }
    }

    private boolean withinZone(double price1, double price2) {
        double ref = Math.max(Math.abs(price1), Math.abs(price2));
        if (ref == 0) return true;
        return Math.abs(price1 - price2) / ref <= levelZonePercent;
    }
}
