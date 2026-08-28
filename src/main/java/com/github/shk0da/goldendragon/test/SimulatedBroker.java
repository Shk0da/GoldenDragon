package com.github.shk0da.goldendragon.test;

import com.github.shk0da.goldendragon.market.MarketDataProvider;
import com.github.shk0da.goldendragon.market.MarketPrices;
import com.github.shk0da.goldendragon.model.Candle;
import com.github.shk0da.goldendragon.model.PositionInfo;
import com.github.shk0da.goldendragon.model.TickerCandle;
import com.github.shk0da.goldendragon.model.TickerInfo;
import com.github.shk0da.goldendragon.model.TickerType;
import com.github.shk0da.goldendragon.strategy.DataCollector;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Simulated broker and market data provider for backtest.
 * Combines broker state (cash, positions) with historical market data (candles, prices).
 * This is the single source of truth for backtest execution, ensuring parity with live trading.
 */
public class SimulatedBroker implements MarketDataProvider {

    private final String dataDir;
    private double availableCash;
    private final Map<TickerInfo.Key, PositionInfo> positions;
    private final Map<TickerInfo.Key, Double> currentPrices;
    private final Map<TickerInfo.Key, TickerInfo> tickerInfo;
    private final Map<String, List<Candle>> cachedHourCandles = new ConcurrentHashMap<>();
    private final Map<String, List<Candle>> cachedMinuteCandles = new ConcurrentHashMap<>();

    public SimulatedBroker(double initialCash, String dataDir) {
        this.dataDir = dataDir != null ? dataDir : "data";
        this.availableCash = initialCash;
        this.positions = new ConcurrentHashMap<>();
        this.currentPrices = new ConcurrentHashMap<>();
        this.tickerInfo = new HashMap<>();
    }

    // ========== Broker State Methods ==========

    public double getAvailableCash() {
        return availableCash;
    }

    public PositionInfo getCurrentPositions(TickerType tickerType, String tickerName) {
        for (Map.Entry<TickerInfo.Key, PositionInfo> entry : positions.entrySet()) {
            if (entry.getKey().getTicker().equalsIgnoreCase(tickerName) &&
                (tickerType == TickerType.ALL || entry.getKey().getType() == tickerType)) {
                return entry.getValue();
            }
        }
        return null;
    }

    public boolean sellByMarket(String name, TickerType type, double cashToSell) {
        TickerInfo.Key key = new TickerInfo.Key(name, type);
        PositionInfo pos = positions.get(key);
        if (pos == null || pos.getBalance() <= 0) {
            return false;
        }

        Double price = currentPrices.get(key);
        if (price == null || price <= 0) {
            price = pos.getAveragePositionPrice();
        }
        if (price == null || price <= 0) {
            return false;
        }

        int lots = pos.getLots() > 0 ? pos.getLots() : 1;
        double lotValue = price * lots;
        int lotsToSell = (int) Math.floor(cashToSell / lotValue);
        if (lotsToSell <= 0) {
            lotsToSell = 1;
        }
        lotsToSell = Math.min(lotsToSell, pos.getBalance());

        if (lotsToSell <= 0) {
            return false;
        }

        double proceeds = lotsToSell * lotValue;
        availableCash += proceeds;

        PositionInfo newPos = new PositionInfo(
            pos.getFigi(),
            pos.getTicker(),
            pos.getIsin(),
            pos.getInstrumentType().name(),
            pos.getBalance() - lotsToSell,
            pos.getExpectedYield(),
            pos.getLots(),
            pos.getAveragePositionPrice(),
            pos.getName()
        );

        if (newPos.getBalance() <= 0) {
            positions.remove(key);
        } else {
            positions.put(key, newPos);
        }

        return true;
    }

    public void registerTicker(TickerInfo info) {
        tickerInfo.put(info.getKey(), info);
    }

    // ========== Market Data Provider Methods (Candles, Prices) ==========

    @Override
    public List<Candle> getCandles(String ticker, String interval) {
        try {
            List<TickerCandle> cached;
            if ("HOUR".equals(interval)) {
                if (!cachedHourCandles.containsKey(ticker)) {
                    cachedHourCandles.put(ticker, loadCandlesFromCsv(ticker, "HOUR"));
                }
                return cachedHourCandles.get(ticker);
            } else if ("5_MIN".equals(interval)) {
                if (!cachedMinuteCandles.containsKey(ticker)) {
                    cachedMinuteCandles.put(ticker, loadCandlesFromCsv(ticker, "5_MIN"));
                }
                return cachedMinuteCandles.get(ticker);
            } else {
                return Collections.emptyList();
            }
        } catch (Exception e) {
            return Collections.emptyList();
        }
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

    // ========== Helper Methods ==========

    private List<Candle> loadCandlesFromCsv(String ticker, String intervalType) {
        try {
            ru.tinkoff.piapi.contract.v1.CandleInterval interval;
            if ("HOUR".equals(intervalType)) {
                interval = ru.tinkoff.piapi.contract.v1.CandleInterval.CANDLE_INTERVAL_HOUR;
            } else if ("5_MIN".equals(intervalType)) {
                interval = ru.tinkoff.piapi.contract.v1.CandleInterval.CANDLE_INTERVAL_5_MIN;
            } else {
                return Collections.emptyList();
            }

            List<TickerCandle> tcList = DataCollector.readCandlesFile(ticker, dataDir, interval);
            if (tcList == null || tcList.isEmpty()) {
                return Collections.emptyList();
            }

            List<Candle> candles = new ArrayList<>(tcList.size());
            for (TickerCandle tc : tcList) {
                candles.add(new Candle(
                    tc.getDate(),
                    tc.getOpen(),
                    tc.getHigh(),
                    tc.getLow(),
                    tc.getClose(),
                    tc.getVolume()
                ));
            }
            return candles;
        } catch (Exception e) {
            return Collections.emptyList();
        }
    }

    public boolean closeLongByMarket(String ticker, TickerType type) {
        return sellByMarket(ticker, type, Double.MAX_VALUE);
    }

    public double round(double value) {
        return Math.round(value * 100.0) / 100.0;
    }
}
