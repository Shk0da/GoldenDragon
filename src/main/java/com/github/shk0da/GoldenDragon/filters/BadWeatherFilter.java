package com.github.shk0da.goldendragon.filters;

import com.github.shk0da.goldendragon.model.Candle;
import java.util.List;

/**
 * "Bad Weather" market conditions filter. Prohibits opening new trades under unfavorable market
 * conditions.
 */
public class BadWeatherFilter {

  // Filter parameters (configurable)
  private final double lowVolumeThreshold; // Low volume threshold (multiplier of average)
  private final double lowAtrThreshold; // Low ATR threshold (multiplier of average)
  private final double minRangePercent; // Minimum candle range in %
  private final double highAtrThreshold; // High ATR threshold (multiplier of average)
  private final double maxSpreadPercent; // Maximum spread in %
  private final double maxWickRatio; // Maximum wick ratio
  private final double panicVolumeThreshold; // Panic volume threshold
  private final double minAvgDailyVolume; // Minimum average daily volume
  private final double atrSpikeThreshold; // ATR spike threshold

  // Whether filter is enabled
  private final boolean enabled;

  public static class Params {

    public final double lowVolumeThreshold;
    public final double lowAtrThreshold;
    public final double minRangePercent;
    public final double highAtrThreshold;
    public final double maxSpreadPercent;
    public final double maxWickRatio;
    public final double panicVolumeThreshold;
    public final double minAvgDailyVolume;
    public final double atrSpikeThreshold;

    public Params() {
      this(0.5, 0.7, 0.005, 2.0, 0.01, 0.4, 3.0, 100000, 2.5);
    }

    public Params(
        double lowVolumeThreshold,
        double lowAtrThreshold,
        double minRangePercent,
        double highAtrThreshold,
        double maxSpreadPercent,
        double maxWickRatio,
        double panicVolumeThreshold,
        double minAvgDailyVolume,
        double atrSpikeThreshold) {
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
  }

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
      double atrSpikeThreshold) {
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

  /** Создаёт фильтр с параметрами по умолчанию. */
  public BadWeatherFilter(boolean enabled) {
    this(
        enabled,
        0.5, // lowVolumeThreshold
        0.7, // lowAtrThreshold
        0.005, // minRangePercent (0.5%)
        2.0, // highAtrThreshold
        0.01, // maxSpreadPercent (1%)
        0.4, // maxWickRatio
        3.0, // panicVolumeThreshold
        100000, // minAvgDailyVolume
        2.5 // atrSpikeThreshold
        );
  }

  /**
   * Check if opening new trades is allowed.
   *
   * @return true if trading is allowed, false if "bad weather"
   */
  public boolean canTrade(List<Candle> candles, double currentPrice) {
    return canTrade(
        candles,
        currentPrice,
        new Params(
            lowVolumeThreshold,
            lowAtrThreshold,
            minRangePercent,
            highAtrThreshold,
            maxSpreadPercent,
            maxWickRatio,
            panicVolumeThreshold,
            minAvgDailyVolume,
            atrSpikeThreshold));
  }

  public boolean canTrade(List<Candle> candles, double currentPrice, Params params) {
    if (!enabled) {
      return true;
    }

    if (candles == null || candles.size() < 30) {
      return false;
    }

    if (isLowActivity(candles, params)) return false;
    if (isChaoticActivity(candles, currentPrice, params)) return false;
    if (isPoorLiquidity(candles, currentPrice, params)) return false;
    if (isTurbulentRegime(candles, params)) return false;

    return true;
  }

  /** 1. Too low activity */
  private boolean isLowActivity(List<Candle> candles, Params params) {
    int lookback = Math.min(20, candles.size() - 1);
    if (lookback < 10) return true;

    Candle current = candles.get(candles.size() - 1);

    long avgVolume = 0;
    for (int i = candles.size() - lookback; i < candles.size(); i++) {
      avgVolume += candles.get(i).volume;
    }
    avgVolume /= lookback;

    if (current.volume < avgVolume * params.lowVolumeThreshold) {
      return true;
    }

    double atr = calculateAtr(candles, lookback);
    double avgAtr = calculateAvgAtr(candles, lookback * 2);
    if (avgAtr > 0 && atr < avgAtr * params.lowAtrThreshold) {
      return true;
    }

    double rangePercent = (current.high - current.low) / current.close;
    if (rangePercent < params.minRangePercent) {
      return true;
    }

    return false;
  }

  /** 2. Too chaotic / dangerous activity */
  private boolean isChaoticActivity(List<Candle> candles, double currentPrice, Params params) {
    int lookback = Math.min(20, candles.size() - 1);
    if (lookback < 10) return false;

    Candle current = candles.get(candles.size() - 1);

    double atr = calculateAtr(candles, lookback);
    double avgAtr = calculateAvgAtr(candles, lookback * 2);
    if (avgAtr > 0 && atr > avgAtr * params.highAtrThreshold) {
      return true;
    }

    double body = Math.abs(current.close - current.open);
    double range = current.high - current.low;
    if (range > 0) {
      double upperWick = current.high - Math.max(current.open, current.close);
      double lowerWick = Math.min(current.open, current.close) - current.low;
      double wickRatio = Math.max(upperWick, lowerWick) / range;
      if (wickRatio > params.maxWickRatio) {
        return true;
      }
    }

    long avgVolume = 0;
    for (int i = candles.size() - lookback; i < candles.size(); i++) {
      avgVolume += candles.get(i).volume;
    }
    avgVolume /= lookback;

    if (current.volume > avgVolume * params.panicVolumeThreshold) {
      double bodyPercent = body / current.close;
      if (bodyPercent < 0.005) {
        return true;
      }
    }

    return false;
  }

  /** 3. Poor liquidity */
  private boolean isPoorLiquidity(List<Candle> candles, double currentPrice, Params params) {
    int lookback = Math.min(20, candles.size() - 1);
    if (lookback < 10) return true;

    long avgVolume = 0;
    for (int i = candles.size() - lookback; i < candles.size(); i++) {
      avgVolume += candles.get(i).volume;
    }
    avgVolume /= lookback;

    if (avgVolume < params.minAvgDailyVolume) {
      return true;
    }

    Candle current = candles.get(candles.size() - 1);
    double spreadPercent = (current.high - current.low) / current.close;
    if (spreadPercent > params.maxSpreadPercent) {
      return true;
    }

    return false;
  }

  /** 4. News / turbulent regime */
  private boolean isTurbulentRegime(List<Candle> candles, Params params) {
    int lookback = Math.min(10, candles.size() - 1);
    if (lookback < 5) return false;

    double currentAtr = calculateAtr(candles, lookback);
    double prevAvgAtr = calculateAvgAtr(candles.subList(0, candles.size() - lookback), lookback);

    if (prevAvgAtr > 0 && currentAtr > prevAvgAtr * params.atrSpikeThreshold) {
      return true;
    }

    long currentVolume = candles.get(candles.size() - 1).volume;
    long avgVolume = 0;
    int volLookback = Math.min(20, candles.size() - 1);
    for (int i = candles.size() - volLookback; i < candles.size() - 1; i++) {
      avgVolume += candles.get(i).volume;
    }
    avgVolume /= volLookback;

    if (avgVolume > 0 && currentVolume > avgVolume * params.panicVolumeThreshold) {
      return true;
    }

    return false;
  }

  /** Calculate ATR for period */
  private double calculateAtr(List<Candle> candles, int period) {
    if (candles.size() < period + 1) return 0.0;

    double sum = 0.0;
    for (int i = candles.size() - period; i < candles.size(); i++) {
      Candle c = candles.get(i);
      Candle p = candles.get(i - 1);
      double tr =
          Math.max(Math.max(c.high - c.low, Math.abs(c.high - p.close)), Math.abs(c.low - p.close));
      sum += tr;
    }
    return sum / period;
  }

  /** Calculate average ATR for period */
  private double calculateAvgAtr(List<Candle> candles, int period) {
    if (candles.size() < period + 1) return 0.0;

    int lookback = Math.min(period, candles.size() - 1);
    double sum = 0.0;
    int count = 0;

    for (int i = candles.size() - lookback; i < candles.size(); i++) {
      Candle c = candles.get(i);
      Candle p = candles.get(i - 1);
      double tr =
          Math.max(Math.max(c.high - c.low, Math.abs(c.high - p.close)), Math.abs(c.low - p.close));
      sum += tr;
      count++;
    }

    return count > 0 ? sum / count : 0.0;
  }

  /** Возвращает описание причины запрета торговли (для отладки) */
  public String getBlockReason(List<Candle> candles, double currentPrice) {
    return getBlockReason(
        candles,
        currentPrice,
        new Params(
            lowVolumeThreshold,
            lowAtrThreshold,
            minRangePercent,
            highAtrThreshold,
            maxSpreadPercent,
            maxWickRatio,
            panicVolumeThreshold,
            minAvgDailyVolume,
            atrSpikeThreshold));
  }

  public String getBlockReason(List<Candle> candles, double currentPrice, Params params) {
    if (!enabled) {
      return null;
    }

    if (candles == null || candles.size() < 30) {
      return "INSUFFICIENT_DATA";
    }

    if (isLowActivity(candles, params)) return "LOW_ACTIVITY";
    if (isChaoticActivity(candles, currentPrice, params)) return "CHAOTIC_ACTIVITY";
    if (isPoorLiquidity(candles, currentPrice, params)) return "POOR_LIQUIDITY";
    if (isTurbulentRegime(candles, params)) return "TURBULENT_REGIME";

    return null;
  }

  public boolean isEnabled() {
    return enabled;
  }
}
