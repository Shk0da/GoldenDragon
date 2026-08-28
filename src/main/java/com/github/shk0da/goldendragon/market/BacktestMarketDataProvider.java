package com.github.shk0da.goldendragon.market;

import com.github.shk0da.goldendragon.model.Candle;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Historical data provider for backtesting.
 * Reads candles from CSV files and simulates live prices from historical data.
 */
public class BacktestMarketDataProvider implements MarketDataProvider {

    private final String dataDir;
    private final Map<String, List<Candle>> cachedHourCandles = new HashMap<>();
    private final Map<String, List<Candle>> cachedMinuteCandles = new HashMap<>();

    public BacktestMarketDataProvider(String dataDir) {
        this.dataDir = dataDir != null ? dataDir : "data";
    }

    @Override
    public List<Candle> getCandles(String ticker, String interval) {
        // DataCollector removed - returning empty candles for backtest
        return Collections.emptyList();
    }

    @Override
    public MarketPrices getLivePrices(String ticker) {
        // In backtest, use last candle close price as "live" price
        List<Candle> candles = getCandles(ticker, "HOUR");
        if (candles.isEmpty()) {
            return new MarketPrices(null, null);
        }
        
        Candle last = candles.get(candles.size() - 1);
        // Simulate bid/ask spread (0.01% spread)
        double spread = last.close * 0.0001;
        return new MarketPrices(last.close - spread, last.close + spread);
    }

    @Override
    public boolean isLive() {
        return false;
    }

    @Override
    public int getAvailableLiquidity(String ticker, String side) {
        // In backtest, assume sufficient liquidity
        return Integer.MAX_VALUE;
    }

    @Override
    public String getCurrentTime() {
        // In backtest, return empty (time controlled by backtest engine)
        return "";
    }
}
