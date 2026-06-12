package com.github.shk0da.goldendragon.money;

/**
 * Position sizer wrapper that uses SizingStrategy implementations. Provides unified interface for
 * position size calculation.
 */
public class PositionSizer {

  private final SizingStrategy sizingStrategy;
  private final int minLotSize;
  private final int lotStep;

  /**
   * Create position sizer with specified strategy.
   *
   * @param sizingStrategy sizing strategy implementation
   * @param minLotSize minimum position size (default: 1)
   * @param lotStep position size step (default: 1)
   */
  public PositionSizer(SizingStrategy sizingStrategy, int minLotSize, int lotStep) {
    this.sizingStrategy = sizingStrategy;
    this.minLotSize = minLotSize;
    this.lotStep = lotStep;
  }

  /**
   * Create position sizer with default lot settings.
   *
   * @param sizingStrategy sizing strategy implementation
   */
  public PositionSizer(SizingStrategy sizingStrategy) {
    this(sizingStrategy, 1, 1);
  }

  /**
   * Calculate position size.
   *
   * @param ticker ticker symbol
   * @param entry entry price
   * @param stopLoss stop loss price
   * @param balance available balance
   * @param atr current ATR value (used by volatility-adjusted sizing)
   * @return position size in units (0 if calculation fails or below minimum)
   */
  public int calculateSize(
      String ticker, double entry, double stopLoss, double balance, double atr) {
    int rawSize = sizingStrategy.calculateSize(ticker, entry, stopLoss, balance, atr);

    if (rawSize < minLotSize) {
      return 0;
    }

    // Round down to lot step
    return (rawSize / lotStep) * lotStep;
  }
}
