package com.github.shk0da.goldendragon.ml;

import java.time.LocalDateTime;

/**
 * Trade features for ML model training. Collects market conditions, entry signals, and outcomes.
 */
public class TradeFeatures {

  // Market regime features
  public final double adx; // ADX(14) - trend strength
  public final double diPlus; // +DI directional indicator
  public final double diMinus; // -DI directional indicator
  public final double atr; // ATR(14) - volatility
  public final double atrRatio; // Current ATR / Average ATR

  // Price action features
  public final double rsi; // RSI(14)
  public final double emaFast; // EMA(9)
  public final double emaSlow; // EMA(21)
  public final double emaRatio; // EMA9 / EMA21
  public final double pricePosition; // Position in range (0-1)

  // Volume features
  public final double volumeRatio; // Current volume / Average volume
  public final double volumeTrend; // Volume trend (up/down)

  // Trade setup features
  public final double entryConfidence; // Strategy confidence score
  public final double riskRewardRatio; // Expected R:R
  public final double stopDistance; // Stop distance in %
  public final String breakoutType; // ORB, Range, Donchian, etc.
  public final double signalStrength; // Parsed signal score from decision reason
  public final double signalTypeTrend; // Trend-following signal flag
  public final double signalTypeFx; // Reversal/FX signal flag
  public final double signalTypeMixed; // Mixed signal flag
  public final double groupConfirmed; // Group confirmation flag
  public final double strongTrend; // Strong trend regime flag
  public final double rangeRegime; // Range regime flag

  // Time features
  public final int hourOfDay; // Hour of day (0-23)
  public final int dayOfWeek; // Day of week (1-5)
  public final boolean isMorning; // Morning session
  public final boolean isAfternoon; // Afternoon session

  // Outcome (label for training)
  public Double outcome; // Final PnL in R-multiples
  public Boolean isWinner; // True if profitable
  public Double maxProfit; // Max favorable excursion (MFE)
  public Double maxLoss; // Max adverse excursion (MAE)

  // Metadata
  public final String ticker;
  public final String strategy;
  public final LocalDateTime entryTime;
  public final double entryPrice;

  public TradeFeatures(
      double adx,
      double diPlus,
      double diMinus,
      double atr,
      double atrRatio,
      double rsi,
      double emaFast,
      double emaSlow,
      double emaRatio,
      double pricePosition,
      double volumeRatio,
      double volumeTrend,
      double entryConfidence,
      double riskRewardRatio,
      double stopDistance,
      String breakoutType,
      double signalStrength,
      double signalTypeTrend,
      double signalTypeFx,
      double signalTypeMixed,
      double groupConfirmed,
      double strongTrend,
      double rangeRegime,
      int hourOfDay,
      int dayOfWeek,
      String ticker,
      String strategy,
      LocalDateTime entryTime,
      double entryPrice) {

    this.adx = adx;
    this.diPlus = diPlus;
    this.diMinus = diMinus;
    this.atr = atr;
    this.atrRatio = atrRatio;
    this.rsi = rsi;
    this.emaFast = emaFast;
    this.emaSlow = emaSlow;
    this.emaRatio = emaRatio;
    this.pricePosition = pricePosition;
    this.volumeRatio = volumeRatio;
    this.volumeTrend = volumeTrend;
    this.entryConfidence = entryConfidence;
    this.riskRewardRatio = riskRewardRatio;
    this.stopDistance = stopDistance;
    this.breakoutType = breakoutType;
    this.signalStrength = signalStrength;
    this.signalTypeTrend = signalTypeTrend;
    this.signalTypeFx = signalTypeFx;
    this.signalTypeMixed = signalTypeMixed;
    this.groupConfirmed = groupConfirmed;
    this.strongTrend = strongTrend;
    this.rangeRegime = rangeRegime;
    this.hourOfDay = hourOfDay;
    this.dayOfWeek = dayOfWeek;
    this.isMorning = hourOfDay >= 10 && hourOfDay < 14;
    this.isAfternoon = hourOfDay >= 14 && hourOfDay < 18;
    this.ticker = ticker;
    this.strategy = strategy;
    this.entryTime = entryTime;
    this.entryPrice = entryPrice;
  }

  /** Classify trade quality based on outcome. */
  public int getQualityClass() {
    if (outcome == null) return -1; // Unknown

    if (outcome >= 2.0) return 3; // Excellent (>= 2R)
    if (outcome >= 1.0) return 2; // Good (>= 1R)
    if (outcome >= 0.0) return 1; // Break even / small profit
    return 0; // Loss
  }

  @Override
  public String toString() {
    return String.format(
        "TradeFeatures{ticker=%s, strategy=%s, adx=%.1f, rsi=%.1f, outcome=%.2fR}",
        ticker, strategy, adx, rsi, outcome != null ? outcome : 0.0);
  }
}
