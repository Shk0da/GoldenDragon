package com.github.shk0da.goldendragon.filters;

import com.github.shk0da.goldendragon.model.Candle;
import java.util.List;

/**
 * Market regime filter for detecting trending vs ranging markets. Uses ADX (trend strength), ATR
 * (volatility), and volume analysis. Returns trading permission with confidence score and position
 * multiplier.
 */
public class MarketRegimeFilter {

    private final boolean enabled;
    private final int adxPeriod;
    private final int atrPeriod;
    private final int volumePeriod;

    public MarketRegimeFilter(boolean enabled) {
        this(enabled, 14, 14, 50);
    }

    public MarketRegimeFilter(boolean enabled, int adxPeriod, int atrPeriod, int volumePeriod) {
        this.enabled = enabled;
        this.adxPeriod = adxPeriod;
        this.atrPeriod = atrPeriod;
        this.volumePeriod = volumePeriod;
    }

    public static class FilterResult {
        public final boolean canTrade;
        public final double confidence;
        public final double positionMultiplier;
        public final String reason;

        public FilterResult(
                boolean canTrade, double confidence, double positionMultiplier, String reason) {
            this.canTrade = canTrade;
            this.confidence = confidence;
            this.positionMultiplier = positionMultiplier;
            this.reason = reason;
        }
    }

    public FilterResult evaluate(List<Candle> candles) {
        return evaluate(candles, 20.0, 25.0, 30.0, 50.0, 4);
    }

    public FilterResult evaluate(
            List<Candle> candles,
            double adxRangeThreshold,
            double adxUnclearThreshold,
            double volumeRatioMin,
            double confidenceMin,
            int atrBars) {
        if (!enabled || candles == null || candles.size() < Math.max(adxPeriod * 2 + 10, 60)) {
            return new FilterResult(true, 100.0, 1.0, "DISABLED");
        }

        AdxValues adx = calculateAdx(candles, adxPeriod);

        double confidence;
        double posMult = 1.0;
        String regime;

        if (adx.adx < adxRangeThreshold) {
            return new FilterResult(false, 0.0, 0.0, "ADX_RANGE_" + (int) adx.adx);
        }

        if (adx.adx <= adxUnclearThreshold) {
            boolean diStrong = Math.max(adx.diPlus, adx.diMinus) > adx.adx;
            boolean atrGrowing = isAtrGrowing(candles, atrPeriod, atrBars);
            if (diStrong && atrGrowing) {
                confidence =
                        Math.min(
                                adxUnclearThreshold - 1,
                                adxRangeThreshold * 0.5 + (adx.adx - adxRangeThreshold) * 5);
                posMult = 0.5;
                regime = "TREND_EARLY";
            } else {
                return new FilterResult(false, 0.0, 0.0, "ADX_UNCLEAR_" + (int) adx.adx);
            }
        } else {
            confidence = Math.min(95, 50 + (adx.adx - adxUnclearThreshold) * 3);
            regime = "TREND";
        }

        double currentAtr = calculateAtr(candles, atrPeriod);
        double smoothedAtr = calculateSmoothedAtr(candles, atrPeriod);
        boolean atrExpanding =
                currentAtr > smoothedAtr && isAtrGrowing(candles, atrPeriod, atrBars);

        String atrStatus;
        if (atrExpanding) {
            atrStatus = "ATR_OK";
        } else {
            confidence *= 0.7;
            atrStatus = "ATR_DECLINING";
        }

        double volumeRatio = calculateVolumeRatio(candles, volumePeriod);
        if (volumeRatio < volumeRatioMin) {
            return new FilterResult(false, 0.0, 0.0, "VOLUME_LOW_" + (int) volumeRatio + "pct");
        }

        if (confidence < confidenceMin) {
            return new FilterResult(false, 0.0, 0.0, "LOW_CONFIDENCE_" + (int) confidence);
        }

        return new FilterResult(
                true,
                confidence,
                posMult,
                regime + "_" + atrStatus + "_V" + (int) volumeRatio + "_C" + (int) confidence);
    }

    private static class AdxValues {
        final double adx;
        final double diPlus;
        final double diMinus;

        AdxValues(double adx, double diPlus, double diMinus) {
            this.adx = adx;
            this.diPlus = diPlus;
            this.diMinus = diMinus;
        }
    }

    private AdxValues calculateAdx(List<Candle> candles, int period) {
        int start = candles.size() - period;
        double trSum = 0.0, pdSum = 0.0, mdSum = 0.0;
        for (int i = start; i < candles.size(); i++) {
            Candle c = candles.get(i);
            Candle p = candles.get(i - 1);
            double tr =
                    Math.max(
                            Math.max(c.high - c.low, Math.abs(c.high - p.close)),
                            Math.abs(c.low - p.close));
            trSum += tr;
            double up = c.high - p.high;
            double dn = p.low - c.low;
            pdSum += (up > dn && up > 0) ? up : 0.0;
            mdSum += (dn > up && dn > 0) ? dn : 0.0;
        }
        double atr = trSum / period;
        double diPlus = atr > 0 ? pdSum / period / atr * 100 : 0.0;
        double diMinus = atr > 0 ? mdSum / period / atr * 100 : 0.0;
        double adx =
                (diPlus + diMinus) > 0
                        ? Math.abs(diPlus - diMinus) / (diPlus + diMinus) * 100
                        : 0.0;
        return new AdxValues(adx, diPlus, diMinus);
    }

    private double calculateAtr(List<Candle> candles, int period) {
        if (candles.size() < period + 1) {
            return 0.0;
        }
        double sum = 0.0;
        for (int i = candles.size() - period; i < candles.size(); i++) {
            Candle c = candles.get(i);
            Candle p = candles.get(i - 1);
            sum +=
                    Math.max(
                            Math.max(c.high - c.low, Math.abs(c.high - p.close)),
                            Math.abs(c.low - p.close));
        }
        return sum / period;
    }

    private double calculateSmoothedAtr(List<Candle> candles, int period) {
        int count = Math.min(20, candles.size() - period - 1);
        if (count < 5) {
            return calculateAtr(candles, period);
        }
        double sum = 0.0;
        int validCount = 0;
        for (int i = 0; i < count; i++) {
            int end = candles.size() - i;
            if (end < period + 1) break;
            double atr = 0.0;
            for (int j = end - period; j < end; j++) {
                Candle c = candles.get(j);
                Candle p = candles.get(j - 1);
                atr +=
                        Math.max(
                                Math.max(c.high - c.low, Math.abs(c.high - p.close)),
                                Math.abs(c.low - p.close));
            }
            sum += atr / period;
            validCount++;
        }
        return validCount > 0 ? sum / validCount : calculateAtr(candles, period);
    }

    private boolean isAtrGrowing(List<Candle> candles, int period, int bars) {
        if (candles.size() < period + bars + 1) {
            return false;
        }
        double[] atrValues = new double[bars];
        for (int i = 0; i < bars; i++) {
            int end = candles.size() - bars + i + 1;
            double sum = 0.0;
            for (int j = end - period; j < end; j++) {
                Candle c = candles.get(j);
                Candle p = candles.get(j - 1);
                sum +=
                        Math.max(
                                Math.max(c.high - c.low, Math.abs(c.high - p.close)),
                                Math.abs(c.low - p.close));
            }
            atrValues[i] = sum / period;
        }
        for (int i = 1; i < bars; i++) {
            if (atrValues[i] <= atrValues[i - 1]) {
                return false;
            }
        }
        return true;
    }

    private double calculateVolumeRatio(List<Candle> candles, int period) {
        if (candles.size() < 2) {
            return 0.0;
        }
        Candle current = candles.get(candles.size() - 1);
        int lookback = Math.min(period, candles.size() - 1);
        long avgVolume = 0;
        for (int i = candles.size() - lookback; i < candles.size() - 1; i++) {
            avgVolume += candles.get(i).volume;
        }
        avgVolume /= lookback;
        if (avgVolume <= 0) return 100.0;
        return (double) current.volume / avgVolume * 100.0;
    }
}
