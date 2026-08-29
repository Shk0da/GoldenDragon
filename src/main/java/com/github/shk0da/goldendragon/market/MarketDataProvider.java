package com.github.shk0da.goldendragon.market;

import com.github.shk0da.goldendragon.model.Candle;
import com.github.shk0da.goldendragon.model.PositionInfo;
import com.github.shk0da.goldendragon.model.TickerType;

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
     * Get current position for a ticker (backtest only).
     *
     * @param tickerType ticker type
     * @param tickerName ticker name
     * @return position info, or null if not found (live mode always returns null)
     */
    default PositionInfo getCurrentPositions(TickerType tickerType, String tickerName) {
        return null; // Default implementation for live mode
    }

    /**
     * Sell position by market price (backtest only).
     *
     * @param name ticker name
     * @param type ticker type
     * @param cashToSell cash amount to sell
     * @return true if successful
     */
    default boolean sellByMarket(String name, TickerType type, double cashToSell) {
        return false; // Default implementation for live mode
    }

    /**
     * Close long position by market (backtest only).
     *
     * @param ticker ticker name
     * @param type ticker type
     * @return true if successful
     */
    default boolean closeLongByMarket(String ticker, TickerType type) {
        return false; // Default implementation for live mode
    }
}
