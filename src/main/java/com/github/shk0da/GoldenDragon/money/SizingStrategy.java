package com.github.shk0da.GoldenDragon.money;

/**
 * Interface for position sizing algorithms.
 * Implementations calculate optimal position size based on risk parameters.
 */
public interface SizingStrategy {

    /**
     * Calculate position size based on entry price, stop loss and available balance.
     *
     * @param ticker ticker symbol
     * @param entry entry price
     * @param stopLoss stop loss price
     * @param balance available balance for this position
     * @param atr current ATR value (optional, used by volatility-adjusted sizing)
     * @return position size in units (0 if calculation fails)
     */
    int calculateSize(String ticker, double entry, double stopLoss, double balance, double atr);
}
