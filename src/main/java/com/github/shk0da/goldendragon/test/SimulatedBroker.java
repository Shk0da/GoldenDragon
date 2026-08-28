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

    public void setAvailableCash(double cash) {
        this.availableCash = cash;
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

    public Map<TickerInfo.Key, PositionInfo> getCurrentPositions(TickerType tickerType) {
        if (tickerType == TickerType.ALL) {
            return new HashMap<>(positions);
        }
        Map<TickerInfo.Key, PositionInfo> filtered = new HashMap<>();
        for (Map.Entry<TickerInfo.Key, PositionInfo> entry : positions.entrySet()) {
            if (entry.getKey().getType() == tickerType) {
                filtered.put(entry.getKey(), entry.getValue());
            }
        }
        return filtered;
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

    public boolean buyByMarket(String name, TickerType type, double cashToBuy) {
        TickerInfo.Key key = new TickerInfo.Key(name, type);
        TickerInfo info = tickerInfo.get(key);
        if (info == null) {
            return false;
        }

        Double price = currentPrices.get(key);
        if (price == null || price <= 0) {
            return false;
        }

        int lots = info.getLot() > 0 ? info.getLot() : 1;
        double lotValue = price * lots;
        int lotsToBuy = (int) Math.floor(cashToBuy / lotValue);

        if (lotsToBuy <= 0) {
            return false;
        }

        double cost = lotsToBuy * lotValue;
        if (cost > availableCash) {
            return false;
        }

        availableCash -= cost;

        PositionInfo existing = positions.get(key);
        int newBalance = (existing != null ? existing.getBalance() : 0) + lotsToBuy;

        PositionInfo newPos = new PositionInfo(
            info.getFigi(),
            info.getTicker(),
            info.getIsin(),
            info.getType().name(),
            newBalance,
            0.0,
            lots,
            price,
            info.getName()
        );
        positions.put(key, newPos);

        return true;
    }

    public double getTotalPortfolioCost() {
        double total = availableCash;
        for (PositionInfo pos : positions.values()) {
            TickerInfo.Key key = new TickerInfo.Key(pos.getTicker(), pos.getInstrumentType());
            Double price = currentPrices.get(key);
            if (price != null && price > 0) {
                total += pos.getBalance() * price * (pos.getLots() > 0 ? pos.getLots() : 1);
            }
        }
        return total;
    }

    public TickerInfo searchTicker(TickerInfo.Key key) {
        return tickerInfo.get(key);
    }

    public void registerTicker(TickerInfo info) {
        tickerInfo.put(info.getKey(), info);
    }

    public void updatePosition(String ticker, TickerType type, int balance, double avgPrice) {
        TickerInfo.Key key = new TickerInfo.Key(ticker, type);
        TickerInfo info = tickerInfo.get(key);
        if (info == null) {
            return;
        }

        PositionInfo newPos = new PositionInfo(
            info.getFigi(),
            info.getTicker(),
            info.getIsin(),
            info.getType().name(),
            balance,
            0.0,
            info.getLot(),
            avgPrice,
            info.getName()
        );

        if (balance <= 0) {
            positions.remove(key);
        } else {
            positions.put(key, newPos);
        }
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

    public void updatePrice(TickerInfo.Key key, double price) {
        currentPrices.put(key, price);
    }

    public Double getLiveAskPrice(TickerInfo.Key key) {
        Double price = currentPrices.get(key);
        if (price != null) {
            return price;
        }
        // Fallback: get from last candle
        TickerInfo info = tickerInfo.get(key);
        if (info != null) {
            List<Candle> candles = getCandles(info.getTicker(), "HOUR");
            if (!candles.isEmpty()) {
                return candles.get(candles.size() - 1).close;
            }
        }
        return null;
    }

    public Double getLiveBidPrice(TickerInfo.Key key) {
        Double ask = getLiveAskPrice(key);
        return ask != null ? ask * 0.9999 : null;
    }

    public Double getCurrentPrice(TickerInfo.Key key) {
        return getLiveAskPrice(key);
    }

    public boolean closeLongByMarket(String ticker, TickerType type) {
        return sellByMarket(ticker, type, Double.MAX_VALUE);
    }

    public boolean closeShortByMarket(String ticker, TickerType type) {
        return sellByMarket(ticker, type, Double.MAX_VALUE);
    }

    public int calculateTradeCount(TickerInfo.Key key, double cashToSell, double tickerPrice) {
        TickerInfo info = tickerInfo.get(key);
        int lots = (info != null && info.getLot() > 0) ? info.getLot() : 1;
        double effectivePrice = tickerPrice * 1.01; // Safety margin matching live
        double tradeUnitCost = effectivePrice * lots;
        if (cashToSell < tradeUnitCost) {
            return 0;
        }
        return (int) Math.floor(cashToSell / tradeUnitCost);
    }

    public double round(double value) {
        return Math.round(value * 100.0) / 100.0;
    }
}
