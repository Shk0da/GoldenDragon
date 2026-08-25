package com.github.shk0da.goldendragon.market;

import com.github.shk0da.goldendragon.model.Candle;

import java.util.List;

/**
 * Interface for market data providers.
 * Implementations can use historical data for backtesting or live API for real-time trading.
 */
public interface MarketDataProvider {

    /**
     * Get historical candles for a ticker.
     *
     * @param ticker ticker symbol
     * @param interval candle interval (HOUR, 5_MIN)
     * @return list of candles, may be empty but not null
     */
    List<Candle> getCandles(String ticker, String interval);

    /**
     * Get current bid/ask prices for a ticker.
     *
     * @param ticker ticker symbol
     * @return map with "bid" and "ask" keys, may be null if prices unavailable
     */
    MarketPrices getLivePrices(String ticker);

    /**
     * Check if provider is live (real-time) or backtest (historical).
     *
     * @return true if live, false if backtest
     */
    boolean isLive();

    /**
     * Get available liquidity for buying a ticker.
     *
     * @param ticker ticker symbol
     * @param side "bid" for buying (sell orders in book)
     * @return available quantity, 0 if unavailable
     */
    int getAvailableLiquidity(String ticker, String side);

    /**
     * Get current time from the data provider.
     *
     * @return current time as string
     */
    String getCurrentTime();
}
