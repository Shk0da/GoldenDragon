package com.github.shk0da.goldendragon.test;

import com.github.shk0da.goldendragon.market.MarketDataProvider;
import com.github.shk0da.goldendragon.market.MarketPrices;
import com.github.shk0da.goldendragon.model.Candle;
import com.github.shk0da.goldendragon.model.PositionInfo;
import com.github.shk0da.goldendragon.model.TickerCandle;
import com.github.shk0da.goldendragon.model.TickerInfo;
import com.github.shk0da.goldendragon.model.TickerType;
import com.github.shk0da.goldendragon.strategy.DataCollector;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.ReentrantReadWriteLock;

/**
 * Simulated broker and market data provider for backtest.
 * Combines broker state (cash, positions) with historical market data (candles, prices).
 * This is the single source of truth for backtest execution, ensuring parity with live trading.
 * All position management, commission calculations, and portfolio valuation happen here.
 */
public class SimulatedBroker implements MarketDataProvider {

    private static final DateTimeFormatter DATE_TIME_FMT = DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm:ss");

    private final String dataDir;
    private final double commissionRate;
    private double availableCash;
    private final Map<TickerInfo.Key, PositionInfo> positions;
    private final Map<TickerInfo.Key, Double> currentPrices;
    private final Map<TickerInfo.Key, TickerInfo> tickerInfo;
    private final Map<String, List<Candle>> cachedHourCandles = new ConcurrentHashMap<>();
    private final Map<String, List<Candle>> cachedMinuteCandles = new ConcurrentHashMap<>();
    // Cache parsed LocalDateTime for each candle (parsed once at load time)
    private final Map<String, List<LocalDateTime>> cachedHourCandleTimes = new ConcurrentHashMap<>();
    private final Map<String, List<LocalDateTime>> cachedMinuteCandleTimes = new ConcurrentHashMap<>();
    private volatile LocalDateTime currentTime;
    // Read-write lock: buy/sell mutate cash + positions (write), all reads take shared lock.
    private final ReentrantReadWriteLock readWriteLock = new ReentrantReadWriteLock();

    public SimulatedBroker(double initialCash, String dataDir, double commissionRate) {
        this.dataDir = dataDir != null ? dataDir : "data";
        this.commissionRate = Math.max(0, commissionRate);
        this.availableCash = initialCash;
        this.positions = new ConcurrentHashMap<>();
        this.currentPrices = new ConcurrentHashMap<>();
        this.tickerInfo = new HashMap<>();
    }

    /**
     * Sets the current simulation time. All candle queries will be filtered to return
     * only candles with time <= this value (prevents look-ahead bias in backtest).
     */
    public void setCurrentTime(LocalDateTime currentTime) {
        this.currentTime = currentTime;
    }

    // ========== Broker State Methods ==========

    public double getAvailableCash() {
        readWriteLock.readLock().lock();
        try {
            return availableCash;
        } finally {
            readWriteLock.readLock().unlock();
        }
    }

    /**
     * Calculates total portfolio value: available cash + market value of all positions.
     * Uses the last known prices for positions (from currentPrices or candle data).
     */
    public double getPortfolioValue() {
        readWriteLock.readLock().lock();
        try {
            double totalValue = availableCash;
            for (PositionInfo pos : positions.values()) {
                if (pos.getBalance() > 0) {
                    // Reconstruct key from ticker and instrument type
                    TickerInfo.Key key = new TickerInfo.Key(pos.getTicker(), pos.getInstrumentType());
                    Double price = currentPrices.get(key);
                    if (price == null || price <= 0) {
                        // Fallback to last candle
                        List<Candle> candles = getCandles(pos.getTicker(), "HOUR");
                        if (!candles.isEmpty()) {
                            price = candles.get(candles.size() - 1).close;
                        } else {
                            price = pos.getAveragePositionPrice();
                        }
                    }
                    if (price != null && price > 0) {
                        totalValue += pos.getBalance() * price;
                    }
                }
            }
            return totalValue;
        } finally {
            readWriteLock.readLock().unlock();
        }
    }

    public PositionInfo getCurrentPositions(TickerType tickerType, String tickerName) {
        readWriteLock.readLock().lock();
        try {
            for (Map.Entry<TickerInfo.Key, PositionInfo> entry : positions.entrySet()) {
                if (entry.getKey().getTicker().equalsIgnoreCase(tickerName) &&
                    (tickerType == TickerType.ALL || entry.getKey().getType() == tickerType)) {
                    return entry.getValue();
                }
            }
            return null;
        } finally {
            readWriteLock.readLock().unlock();
        }
    }

    public boolean sellByMarket(String name, TickerType type, double cashToSell) {
        readWriteLock.writeLock().lock();
        try {
            TickerInfo.Key key = new TickerInfo.Key(name, type);
            PositionInfo pos = positions.get(key);

            // If not found with given type, try all types (handles type mismatches)
            if (pos == null || pos.getBalance() <= 0) {
                for (Map.Entry<TickerInfo.Key, PositionInfo> entry : positions.entrySet()) {
                    if (entry.getKey().getTicker().equalsIgnoreCase(name) &&
                        entry.getValue().getBalance() > 0) {
                        pos = entry.getValue();
                        key = entry.getKey();
                        break;
                    }
                }
            }
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

            // Position balance is stored in UNITS (shares), consistent with buyByQuantity
            int balanceUnits = pos.getBalance();
            // Determine max units to sell: either full position (close) or based on cashToSell
            int unitsToSell;
            if (cashToSell >= Double.MAX_VALUE / 2) {
                // Full close: sell entire position
                unitsToSell = balanceUnits;
            } else {
                // Partial sell based on cash amount: units = cash / price
                int unitsAffordable = (int) Math.floor(cashToSell / price);
                unitsToSell = Math.min(unitsAffordable, balanceUnits);
            }
            if (unitsToSell <= 0) {
                return false;
            }

            double proceeds = unitsToSell * price;
            double commission = proceeds * commissionRate;
            double netProceeds = proceeds - commission;
            availableCash += netProceeds;

            PositionInfo newPos = new PositionInfo(
                pos.getFigi(),
                pos.getTicker(),
                pos.getIsin(),
                pos.getInstrumentType().name(),
                balanceUnits - unitsToSell,
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
        } finally {
            readWriteLock.writeLock().unlock();
        }
    }

    /**
     * Buys a position using the specified quantity (number of lots/units).
     *
     * @param name ticker name
     * @param type ticker type
     * @param quantity number of units to buy
     * @return true if purchase was successful, false otherwise
     */
    public boolean buyByQuantity(String name, TickerType type, int quantity) {
        readWriteLock.writeLock().lock();
        try {
            if (quantity <= 0) {
                return false;
            }

            // Find ticker info by name, ignoring type (handles type mismatches)
            TickerInfo info = null;
            for (TickerInfo.Key key : tickerInfo.keySet()) {
                if (key.getTicker().equalsIgnoreCase(name)) {
                    info = tickerInfo.get(key);
                    break;
                }
            }
            if (info == null) {
                return false;
            }

            int lotSize = info.getLot() != null ? info.getLot() : 1;
            int totalUnits = quantity * lotSize;

            // Get current price
            TickerInfo.Key actualKey = info.getKey();
            Double price = currentPrices.get(actualKey);
            if (price == null || price <= 0) {
                // Use last candle close from filtered candles
                List<Candle> candles = getCandles(name, "HOUR");
                if (candles.isEmpty()) {
                    return false;
                }
                price = candles.get(candles.size() - 1).close;
                currentPrices.put(actualKey, price);
            }
            if (price == null || price <= 0) {
                return false;
            }

            double totalCost = totalUnits * price;
            double commission = totalCost * commissionRate;
            double totalCostWithCommission = totalCost + commission;

            if (totalCostWithCommission > availableCash) {
                // Partial fill: buy as many units as we can afford (including commission)
                int maxAffordableUnits = (int) Math.floor(availableCash / (price * (1 + commissionRate)));
                if (maxAffordableUnits <= 0) {
                    return false; // Cannot afford even 1 unit
                }
                // Round down to lot size
                int maxAffordableLots = maxAffordableUnits / lotSize;
                if (maxAffordableLots <= 0) {
                    return false; // Cannot afford even 1 lot
                }
                totalUnits = maxAffordableLots * lotSize;
                totalCost = totalUnits * price;
                commission = totalCost * commissionRate;
                totalCostWithCommission = totalCost + commission;
            }

            availableCash -= totalCostWithCommission;

            // Update or create position using actual key from ticker info
            PositionInfo existingPos = positions.get(actualKey);
            if (existingPos != null && existingPos.getBalance() > 0) {
                int newBalance = existingPos.getBalance() + totalUnits;
                double avgPrice = ((existingPos.getBalance() * existingPos.getAveragePositionPrice())
                                 + (totalUnits * price)) / newBalance;
                positions.put(actualKey, new PositionInfo(
                    existingPos.getFigi(),
                    existingPos.getTicker(),
                    existingPos.getIsin(),
                    existingPos.getInstrumentType().name(),
                    newBalance,
                    existingPos.getExpectedYield(),
                    existingPos.getLots(),
                    avgPrice,
                    existingPos.getName()
                ));
            } else {
                positions.put(actualKey, new PositionInfo(
                    info.getFigi(),
                    info.getTicker(),
                    info.getIsin(),
                    info.getType().name(),
                    totalUnits,
                    0.0,
                    lotSize,
                    price,
                    info.getName()
                ));
            }

            return true;
        } finally {
            readWriteLock.writeLock().unlock();
        }
    }

    /**
     * Gets the current price for a ticker (used for order execution).
     *
     * @param name ticker name
     * @param type ticker type
     * @param isAsk true for ask price (buy), false for bid price (sell)
     * @return current price, or null if not available
     */
    public Double getCurrentPrice(String name, TickerType type, boolean isAsk) {
        TickerInfo.Key key = new TickerInfo.Key(name, type);
        Double price = currentPrices.get(key);
        if (price != null && price > 0) {
            // Add small spread for realism
            double spread = price * 0.0001; // 0.01%
            return isAsk ? price + spread : price - spread;
        }

        // Fallback to last candle
        List<Candle> candles = getCandles(name, "HOUR");
        if (candles.isEmpty()) {
            return null;
        }
        price = candles.get(candles.size() - 1).close;
        currentPrices.put(key, price);
        double spread = price * 0.0001;
        return isAsk ? price + spread : price - spread;
    }

    public void registerTicker(TickerInfo info) {
        tickerInfo.put(info.getKey(), info);
    }

    /**
     * Updates the current price for a ticker (called on each tick in backtest).
     */
    public void updateCurrentPrice(String ticker, double price) {
        TickerInfo info = tickerInfo.get(new TickerInfo.Key(ticker, TickerType.STOCK));
        if (info == null) {
            info = tickerInfo.get(new TickerInfo.Key(ticker, TickerType.ETF));
        }
        if (info == null) {
            info = tickerInfo.get(new TickerInfo.Key(ticker, TickerType.FEATURE));
        }
        if (info != null) {
            currentPrices.put(info.getKey(), price);
        }
    }

    // ========== Market Data Provider Methods (Candles, Prices) ==========

    @Override
    public List<Candle> getCandles(String ticker, String interval) {
        try {
            List<Candle> allCandles;
            List<LocalDateTime> parsedTimes;

            if ("HOUR".equals(interval)) {
                if (!cachedHourCandles.containsKey(ticker)) {
                    cachedHourCandles.put(ticker, loadCandlesFromCsv(ticker, "HOUR"));
                }
                allCandles = cachedHourCandles.get(ticker);
                if (!cachedHourCandleTimes.containsKey(ticker)) {
                    cachedHourCandleTimes.put(ticker,
                        allCandles.stream().map(c -> LocalDateTime.parse(c.time, DATE_TIME_FMT)).toList());
                }
                parsedTimes = cachedHourCandleTimes.get(ticker);
            } else if ("5_MIN".equals(interval)) {
                if (!cachedMinuteCandles.containsKey(ticker)) {
                    cachedMinuteCandles.put(ticker, loadCandlesFromCsv(ticker, "5_MIN"));
                }
                allCandles = cachedMinuteCandles.get(ticker);
                if (!cachedMinuteCandleTimes.containsKey(ticker)) {
                    cachedMinuteCandleTimes.put(ticker,
                        allCandles.stream().map(c -> LocalDateTime.parse(c.time, DATE_TIME_FMT)).toList());
                }
                parsedTimes = cachedMinuteCandleTimes.get(ticker);
            } else {
                return Collections.emptyList();
            }

            // Filter candles to only include those up to current simulation time.
            // Use binary search (deterministic, no mutable cache state) to be safe
            // under parallel execution and any currentTime ordering.
            if (currentTime == null || allCandles.isEmpty()) {
                return Collections.unmodifiableList(new ArrayList<>(allCandles));
            }

            int endIdx = binarySearchCutoff(parsedTimes, currentTime);
            if (endIdx <= 0) {
                return Collections.emptyList();
            }
            // Return a defensive copy so callers never share a mutable view.
            return Collections.unmodifiableList(new ArrayList<>(allCandles.subList(0, endIdx)));
        } catch (Exception e) {
            return Collections.emptyList();
        }
    }

    /**
     * Returns the number of candle times that are &lt;= cutoff (parsedTimes is sorted ascending).
     */
    private static int binarySearchCutoff(List<LocalDateTime> parsedTimes, LocalDateTime cutoff) {
        int low = 0;
        int high = parsedTimes.size();
        while (low < high) {
            int mid = (low + high) >>> 1;
            if (parsedTimes.get(mid).compareTo(cutoff) <= 0) {
                low = mid + 1;
            } else {
                high = mid;
            }
        }
        return low;
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
        // Find position by ticker name regardless of type (handles type mismatches)
        PositionInfo pos = getCurrentPositions(TickerType.ALL, ticker);
        if (pos == null || pos.getBalance() <= 0) {
            return false;
        }
        // Use the actual type from the found position
        TickerType actualType = pos.getInstrumentType();
        return sellByMarket(ticker, actualType, Double.MAX_VALUE);
    }
}
