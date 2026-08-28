package com.github.shk0da.goldendragon.strategy.orderbook;

import com.github.shk0da.goldendragon.model.Candle;

import java.util.List;

/**
 * Classifies market regime using ATR (Average True Range) and ADX (Average Directional Index).
 *
 * <p>Three regimes are identified:
 * <ul>
 *   <li>TRENDING — strong directional movement (ADX above threshold)</li>
 *   <li>RANGING — weak or no directional movement (ADX below threshold)</li>
 *   <li>VOLATILE — rapidly expanding volatility (current ATR significantly above smoothed ATR)</li>
 * </ul>
 *
 * <p>Volatility check takes priority over trend/range classification because extreme
 * volatility changes trading characteristics regardless of trend presence.
 *
 * <p>Usage: create once with desired periods and thresholds, then call
 * {@link #detect(List)} with candle data to get the current regime.
 */
public final class MarketRegimeDetector {

    public enum MarketRegime {

        TRENDING,
        RANGING,
        VOLATILE
    }

    private final int atrPeriod;
    private final int adxPeriod;
    private final double adxTrendThreshold;
    private final double atrVolatilityMultiplier;

    public MarketRegimeDetector() {
        this(14, 14, 25.0, 1.5);
    }

    public MarketRegimeDetector(
            int atrPeriod,
            int adxPeriod,
            double adxTrendThreshold,
            double atrVolatilityMultiplier) {
        this.atrPeriod = Math.max(2, atrPeriod);
        this.adxPeriod = Math.max(2, adxPeriod);
        this.adxTrendThreshold = adxTrendThreshold;
        this.atrVolatilityMultiplier = Math.max(1.0, atrVolatilityMultiplier);
    }

    /**
     * Result of regime detection containing the classified regime and underlying indicator values.
     */
    public static final class RegimeResult {

        public final MarketRegime regime;
        public final double atr;
        public final double adx;
        public final double diPlus;
        public final double diMinus;

        public RegimeResult(
                MarketRegime regime,
                double atr,
                double adx,
                double diPlus,
                double diMinus) {
            this.regime = regime;
            this.atr = atr;
            this.adx = adx;
            this.diPlus = diPlus;
            this.diMinus = diMinus;
        }

        @Override
        public String toString() {
            return String.format(
                    "RegimeResult{regime=%s, atr=%.4f, adx=%.2f, diPlus=%.2f, diMinus=%.2f}",
                    regime, atr, adx, diPlus, diMinus);
        }
    }

    /**
     * Detect current market regime from candle data.
     *
     * <p>Requires at least {@code max(adxPeriod * 2 + 10, atrPeriod + smoothedLookback)} candles.
     * Returns RANGING with zero indicators if insufficient data.
     *
     * @param candles historical candle data (oldest first)
     * @return regime classification with indicator values
     */
    public RegimeResult detect(List<Candle> candles) {
        int minSize = Math.max(adxPeriod * 2 + 10, atrPeriod + 30);
        if (candles == null || candles.size() < minSize) {
            return new RegimeResult(MarketRegime.RANGING, 0.0, 0.0, 0.0, 0.0);
        }

        double currentAtr = calculateAtr(candles, atrPeriod);
        double smoothedAtr = calculateSmoothedAtr(candles, atrPeriod);
        AdxValues adx = calculateAdx(candles, adxPeriod);

        // volatility check takes priority — extreme ATR expansion overrides trend/range
        boolean isVolatile = smoothedAtr > 0.0
                && currentAtr > smoothedAtr * atrVolatilityMultiplier;

        MarketRegime regime;
        if (isVolatile) {
            regime = MarketRegime.VOLATILE;
        } else if (adx.adx >= adxTrendThreshold) {
            regime = MarketRegime.TRENDING;
        } else {
            regime = MarketRegime.RANGING;
        }

        return new RegimeResult(regime, currentAtr, adx.adx, adx.diPlus, adx.diMinus);
    }

    /**
     * Get configured ATR period.
     */
    public int getAtrPeriod() {
        return atrPeriod;
    }

    /**
     * Get configured ADX period.
     */
    public int getAdxPeriod() {
        return adxPeriod;
    }

    /**
     * Get configured ADX trend threshold.
     */
    public double getAdxTrendThreshold() {
        return adxTrendThreshold;
    }

    /**
     * Get configured ATR volatility multiplier.
     */
    public double getAtrVolatilityMultiplier() {
        return atrVolatilityMultiplier;
    }

    private static final class AdxValues {

        final double adx;
        final double diPlus;
        final double diMinus;

        AdxValues(double adx, double diPlus, double diMinus) {
            this.adx = adx;
            this.diPlus = diPlus;
            this.diMinus = diMinus;
        }
    }

    private double calculateAtr(List<Candle> candles, int period) {
        if (candles.size() < period + 1) {
            return 0.0;
        }
        double sum = 0.0;
        for (int i = candles.size() - period; i < candles.size(); i++) {
            sum += trueRange(candles.get(i), candles.get(i - 1));
        }
        return sum / period;
    }

    /**
     * Calculate smoothed (historical average) ATR for comparison with current ATR.
     *
     * <p>Averages ATR values over a lookback window to establish a baseline.
     */
    private double calculateSmoothedAtr(List<Candle> candles, int period) {
        int lookback = Math.min(20, candles.size() - period - 1);
        if (lookback < 5) {
            return calculateAtr(candles, period);
        }
        double sum = 0.0;
        int validCount = 0;
        for (int i = 0; i < lookback; i++) {
            int end = candles.size() - i;
            if (end < period + 1) {
                break;
            }
            double atr = 0.0;
            for (int j = end - period; j < end; j++) {
                atr += trueRange(candles.get(j), candles.get(j - 1));
            }
            sum += atr / period;
            validCount++;
        }
        return validCount > 0 ? sum / validCount : calculateAtr(candles, period);
    }

    private AdxValues calculateAdx(List<Candle> candles, int period) {
        int start = candles.size() - period;
        double trSum = 0.0;
        double pdSum = 0.0;
        double mdSum = 0.0;
        for (int i = start; i < candles.size(); i++) {
            Candle c = candles.get(i);
            Candle p = candles.get(i - 1);
            double tr = trueRange(c, p);
            trSum += tr;
            double up = c.high - p.high;
            double dn = p.low - c.low;
            pdSum += (up > dn && up > 0) ? up : 0.0;
            mdSum += (dn > up && dn > 0) ? dn : 0.0;
        }
        double atr = trSum / period;
        double diPlus = atr > 0 ? pdSum / period / atr * 100 : 0.0;
        double diMinus = atr > 0 ? mdSum / period / atr * 100 : 0.0;
        double adx = (diPlus + diMinus) > 0
                ? Math.abs(diPlus - diMinus) / (diPlus + diMinus) * 100
                : 0.0;
        return new AdxValues(adx, diPlus, diMinus);
    }

    private static double trueRange(Candle current, Candle previous) {
        return Math.max(
                Math.max(current.high - current.low, Math.abs(current.high - previous.close)),
                Math.abs(current.low - previous.close));
    }
}
