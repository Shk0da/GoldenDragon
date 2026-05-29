package com.github.shk0da.GoldenDragon.money;

/**
 * Trading result holder for trade registration.
 * Used to pass trade outcomes to RiskManager and PerformanceTracker.
 */
public class TradingResult {

    private final String ticker;
    private final double pnl;
    private final double entryPrice;
    private final double exitPrice;
    private final int quantity;
    private final String direction;
    private final long exitTime;

    /**
     * Create trading result.
     *
     * @param ticker ticker symbol
     * @param pnl trade PnL (positive for win, negative for loss)
     * @param entryPrice entry price
     * @param exitPrice exit price
     * @param quantity position quantity
     * @param direction trade direction ("BUY" or "SELL")
     * @param exitTime exit timestamp
     */
    public TradingResult(String ticker, double pnl, double entryPrice, double exitPrice,
                         int quantity, String direction, long exitTime) {
        this.ticker = ticker;
        this.pnl = pnl;
        this.entryPrice = entryPrice;
        this.exitPrice = exitPrice;
        this.quantity = quantity;
        this.direction = direction;
        this.exitTime = exitTime;
    }

    /**
     * Create trading result with current timestamp.
     *
     * @param ticker ticker symbol
     * @param pnl trade PnL
     * @param entryPrice entry price
     * @param exitPrice exit price
     * @param quantity position quantity
     * @param direction trade direction
     */
    public TradingResult(String ticker, double pnl, double entryPrice, double exitPrice,
                         int quantity, String direction) {
        this(ticker, pnl, entryPrice, exitPrice, quantity, direction, System.currentTimeMillis());
    }

    /**
     * Get ticker symbol.
     *
     * @return ticker symbol
     */
    public String getTicker() {
        return ticker;
    }

    /**
     * Get trade PnL.
     *
     * @return PnL (positive for win, negative for loss)
     */
    public double getPnl() {
        return pnl;
    }

    /**
     * Get entry price.
     *
     * @return entry price
     */
    public double getEntryPrice() {
        return entryPrice;
    }

    /**
     * Get exit price.
     *
     * @return exit price
     */
    public double getExitPrice() {
        return exitPrice;
    }

    /**
     * Get position quantity.
     *
     * @return quantity
     */
    public int getQuantity() {
        return quantity;
    }

    /**
     * Get trade direction.
     *
     * @return "BUY" or "SELL"
     */
    public String getDirection() {
        return direction;
    }

    /**
     * Get exit timestamp.
     *
     * @return exit time in milliseconds
     */
    public long getExitTime() {
        return exitTime;
    }

    /**
     * Check if trade was profitable.
     *
     * @return true if PnL >= 0
     */
    public boolean isWin() {
        return pnl >= 0;
    }
}
