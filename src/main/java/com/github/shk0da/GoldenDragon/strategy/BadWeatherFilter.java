package com.github.shk0da.GoldenDragon.strategy;

import com.github.shk0da.GoldenDragon.model.Candle;
import java.util.List;

/**
 * Фильтр рыночных условий "плохая погода".
 * Запрещает открывать новые сделки при неблагоприятных условиях рынка.
 */
public class BadWeatherFilter {

    // Параметры фильтра (настраиваемые)
    private final double lowVolumeThreshold;        // Порог низкого объёма (множитель от среднего)
    private final double lowAtrThreshold;           // Порог низкого ATR (множитель от среднего)
    private final double minRangePercent;           // Минимальный диапазон свечи в %
    private final double highAtrThreshold;          // Порог высокого ATR (множитель от среднего)
    private final double maxSpreadPercent;          // Максимальный спред в %
    private final double maxWickRatio;              // Максимальное соотношение теней
    private final double panicVolumeThreshold;      // Порог панического объёма
    private final double minAvgDailyVolume;         // Минимальный средний дневной объём
    private final double atrSpikeThreshold;         // Порог скачка ATR

    // Включён ли фильтр
    private final boolean enabled;

    public BadWeatherFilter(
            boolean enabled,
            double lowVolumeThreshold,
            double lowAtrThreshold,
            double minRangePercent,
            double highAtrThreshold,
            double maxSpreadPercent,
            double maxWickRatio,
            double panicVolumeThreshold,
            double minAvgDailyVolume,
            double atrSpikeThreshold
    ) {
        this.enabled = enabled;
        this.lowVolumeThreshold = lowVolumeThreshold;
        this.lowAtrThreshold = lowAtrThreshold;
        this.minRangePercent = minRangePercent;
        this.highAtrThreshold = highAtrThreshold;
        this.maxSpreadPercent = maxSpreadPercent;
        this.maxWickRatio = maxWickRatio;
        this.panicVolumeThreshold = panicVolumeThreshold;
        this.minAvgDailyVolume = minAvgDailyVolume;
        this.atrSpikeThreshold = atrSpikeThreshold;
    }

    /**
     * Создаёт фильтр с параметрами по умолчанию.
     */
    public BadWeatherFilter(boolean enabled) {
        this(
                enabled,
                0.5,    // lowVolumeThreshold
                0.7,    // lowAtrThreshold
                0.005,  // minRangePercent (0.5%)
                2.0,    // highAtrThreshold
                0.01,   // maxSpreadPercent (1%)
                0.4,    // maxWickRatio
                3.0,    // panicVolumeThreshold
                100000, // minAvgDailyVolume
                2.5     // atrSpikeThreshold
        );
    }

    /**
     * Проверяет, разрешено ли открывать новые сделки.
     * @return true если торговля разрешена, false если "плохая погода"
     */
    public boolean canTrade(List<Candle> candles, double currentPrice) {
        if (!enabled) {
            return true;
        }

        if (candles == null || candles.size() < 30) {
            return false; // Недостаточно данных
        }

        // Проверяем все условия "плохой погоды"
        if (isLowActivity(candles)) {
            return false;
        }

        if (isChaoticActivity(candles, currentPrice)) {
            return false;
        }

        if (isPoorLiquidity(candles, currentPrice)) {
            return false;
        }

        if (isTurbulentRegime(candles)) {
            return false;
        }

        return true;
    }

    /**
     * 1. Слишком низкая активность
     */
    private boolean isLowActivity(List<Candle> candles) {
        int lookback = Math.min(20, candles.size() - 1);
        if (lookback < 10) return true;

        Candle current = candles.get(candles.size() - 1);

        // Средний объём
        long avgVolume = 0;
        for (int i = candles.size() - lookback; i < candles.size(); i++) {
            avgVolume += candles.get(i).volume;
        }
        avgVolume /= lookback;

        // Текущий объём слишком низкий
        if (current.volume < avgVolume * lowVolumeThreshold) {
            return true;
        }

        // ATR слишком мал
        double atr = calculateAtr(candles, lookback);
        double avgAtr = calculateAvgAtr(candles, lookback * 2);
        if (avgAtr > 0 && atr < avgAtr * lowAtrThreshold) {
            return true;
        }

        // Инструмент в слишком узком диапазоне
        double rangePercent = (current.high - current.low) / current.close;
        if (rangePercent < minRangePercent) {
            return true;
        }

        return false;
    }

    /**
     * 2. Слишком хаотичная / опасная активность
     */
    private boolean isChaoticActivity(List<Candle> candles, double currentPrice) {
        int lookback = Math.min(20, candles.size() - 1);
        if (lookback < 10) return false;

        Candle current = candles.get(candles.size() - 1);

        // ATR аномально высокий
        double atr = calculateAtr(candles, lookback);
        double avgAtr = calculateAvgAtr(candles, lookback * 2);
        if (avgAtr > 0 && atr > avgAtr * highAtrThreshold) {
            return true;
        }

        // Длинные тени
        double body = Math.abs(current.close - current.open);
        double range = current.high - current.low;
        if (range > 0) {
            double upperWick = current.high - Math.max(current.open, current.close);
            double lowerWick = Math.min(current.open, current.close) - current.low;
            double wickRatio = Math.max(upperWick, lowerWick) / range;
            if (wickRatio > maxWickRatio) {
                return true;
            }
        }

        // Панический объём без подтверждения
        long avgVolume = 0;
        for (int i = candles.size() - lookback; i < candles.size(); i++) {
            avgVolume += candles.get(i).volume;
        }
        avgVolume /= lookback;

        if (current.volume > avgVolume * panicVolumeThreshold) {
            // Проверяем, есть ли подтверждение движения
            double bodyPercent = body / current.close;
            if (bodyPercent < 0.005) { // Тело свечи слишком маленькое
                return true;
            }
        }

        return false;
    }

    /**
     * 3. Плохая ликвидность
     */
    private boolean isPoorLiquidity(List<Candle> candles, double currentPrice) {
        int lookback = Math.min(20, candles.size() - 1);
        if (lookback < 10) return true;

        // Средний объём ниже минимального
        long avgVolume = 0;
        for (int i = candles.size() - lookback; i < candles.size(); i++) {
            avgVolume += candles.get(i).volume;
        }
        avgVolume /= lookback;

        if (avgVolume < minAvgDailyVolume) {
            return true;
        }

        // Спред слишком широкий (оцениваем через диапазон свечи)
        Candle current = candles.get(candles.size() - 1);
        double spreadPercent = (current.high - current.low) / current.close;
        if (spreadPercent > maxSpreadPercent) {
            return true;
        }

        return false;
    }

    /**
     * 4. Новостной / турбулентный режим
     */
    private boolean isTurbulentRegime(List<Candle> candles) {
        int lookback = Math.min(10, candles.size() - 1);
        if (lookback < 5) return false;

        // Резкий скачок ATR
        double currentAtr = calculateAtr(candles, lookback);
        double prevAvgAtr = calculateAvgAtr(candles.subList(0, candles.size() - lookback), lookback);

        if (prevAvgAtr > 0 && currentAtr > prevAvgAtr * atrSpikeThreshold) {
            return true;
        }

        // Экстремальный объём
        long currentVolume = candles.get(candles.size() - 1).volume;
        long avgVolume = 0;
        int volLookback = Math.min(20, candles.size() - 1);
        for (int i = candles.size() - volLookback; i < candles.size() - 1; i++) {
            avgVolume += candles.get(i).volume;
        }
        avgVolume /= volLookback;

        if (avgVolume > 0 && currentVolume > avgVolume * panicVolumeThreshold) {
            return true;
        }

        return false;
    }

    /**
     * Расчёт ATR за период
     */
    private double calculateAtr(List<Candle> candles, int period) {
        if (candles.size() < period + 1) return 0.0;

        double sum = 0.0;
        for (int i = candles.size() - period; i < candles.size(); i++) {
            Candle c = candles.get(i);
            Candle p = candles.get(i - 1);
            double tr = Math.max(
                    Math.max(c.high - c.low, Math.abs(c.high - p.close)),
                    Math.abs(c.low - p.close)
            );
            sum += tr;
        }
        return sum / period;
    }

    /**
     * Расчёт среднего ATR за период
     */
    private double calculateAvgAtr(List<Candle> candles, int period) {
        if (candles.size() < period + 1) return 0.0;

        int lookback = Math.min(period, candles.size() - 1);
        double sum = 0.0;
        int count = 0;

        for (int i = candles.size() - lookback; i < candles.size(); i++) {
            Candle c = candles.get(i);
            Candle p = candles.get(i - 1);
            double tr = Math.max(
                    Math.max(c.high - c.low, Math.abs(c.high - p.close)),
                    Math.abs(c.low - p.close)
            );
            sum += tr;
            count++;
        }

        return count > 0 ? sum / count : 0.0;
    }

    /**
     * Возвращает описание причины запрета торговли (для отладки)
     */
    public String getBlockReason(List<Candle> candles, double currentPrice) {
        if (!enabled) {
            return null;
        }

        if (candles == null || candles.size() < 30) {
            return "INSUFFICIENT_DATA";
        }

        if (isLowActivity(candles)) {
            return "LOW_ACTIVITY";
        }

        if (isChaoticActivity(candles, currentPrice)) {
            return "CHAOTIC_ACTIVITY";
        }

        if (isPoorLiquidity(candles, currentPrice)) {
            return "POOR_LIQUIDITY";
        }

        if (isTurbulentRegime(candles)) {
            return "TURBULENT_REGIME";
        }

        return null;
    }

    public boolean isEnabled() {
        return enabled;
    }
}
