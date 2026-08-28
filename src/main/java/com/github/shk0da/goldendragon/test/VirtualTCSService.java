package com.github.shk0da.goldendragon.test;

import com.github.shk0da.goldendragon.config.MainConfig;
import com.github.shk0da.goldendragon.config.MarketConfig;
import com.github.shk0da.goldendragon.market.MarketDataProvider;
import com.github.shk0da.goldendragon.market.MarketPrices;
import com.github.shk0da.goldendragon.model.*;
import com.github.shk0da.goldendragon.service.TCSService;

import java.time.Duration;
import java.time.Instant;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Virtual TCSService for backtest execution.
 * Extends TCSService but overrides all market/portfolio methods to use virtual state.
 * Provides precise backtest simulation without real API calls.
 */
public class VirtualTCSService extends TCSService implements MarketDataProvider {

    private double availableCash;
    private final double initialBalance;
    private final double commissionRate;
    private final Map<TickerInfo.Key, PositionInfo> positions;
    private final Map<TickerInfo.Key, Double> currentPrices;
    private final Map<TickerInfo.Key, TickerInfo> tickerInfo;
    private final Map<String, List<Candle>> cachedHourCandles;
    private final Map<String, List<Candle>> cachedMinuteCandles;
    private final String dataDir;

    /**
     * Creates virtual TCSService with initial balance.
     * @param initialBalance starting cash (e.g., 1_000_000)
     * @param dataDir directory with CSV candle data
     * @param commissionRate commission rate (e.g., 0.0005)
     */
    public VirtualTCSService(double initialBalance, String dataDir, double commissionRate) {
        super(createMockConfig(), createMockMarketConfig());
        this.initialBalance = initialBalance;
        this.availableCash = initialBalance;
        this.commissionRate = commissionRate;
        this.dataDir = dataDir != null ? dataDir : "data";
        this.positions = new ConcurrentHashMap<>();
        this.currentPrices = new ConcurrentHashMap<>();
        this.tickerInfo = new ConcurrentHashMap<>();
        this.cachedHourCandles = new ConcurrentHashMap<>();
        this.cachedMinuteCandles = new ConcurrentHashMap<>();
    }

    /**
     * Creates virtual TCSService with default 1_000_000 balance.
     */
    public VirtualTCSService(String dataDir) {
        this(1_000_000.0, dataDir, 0.0005);
    }

    // ========== Mock Config Creation ==========

    private static MainConfig createMockConfig() {
        try {
            return new MainConfig();
        } catch (Exception e) {
            throw new RuntimeException("Failed to create mock MainConfig", e);
        }
    }

    private static MarketConfig createMockMarketConfig() {
        return MarketConfig.byMarket(Market.MOEX);
    }

    // ========== Overridden Portfolio Methods ==========

    @Override
    public Double getAvailableCash() {
        return availableCash;
    }

    public void setAvailableCash(double cash) {
        this.availableCash = cash;
    }

    @Override
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

    @Override
    public Map<TickerInfo.Key, PositionInfo> getCurrentPositions(TickerType tickerType) {
        if (tickerType == TickerType.ALL || tickerType == null) {
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

    @Override
    public PositionInfo getCurrentPositions(TickerType tickerType, String tickerName) {
        for (Map.Entry<TickerInfo.Key, PositionInfo> entry : positions.entrySet()) {
            if (entry.getKey().getTicker().equalsIgnoreCase(tickerName) &&
                (tickerType == TickerType.ALL || tickerType == null || entry.getKey().getType() == tickerType)) {
                return entry.getValue();
            }
        }
        return null;
    }

    @Override
    public int getCountOfCurrentPositions(TickerType tickerType, String tickerName) {
        PositionInfo pos = getCurrentPositions(tickerType, tickerName);
        return pos != null ? pos.getBalance() : 0;
    }

    // ========== Overridden Trading Methods ==========

    @Override
    public boolean buyByMarket(String name, TickerType type, double cashToBuy, double takeProfit, double stopLose) {
        return buyByMarket(name, type, cashToBuy, takeProfit, stopLose, false);
    }

    @Override
    public boolean buyByMarket(String name, TickerType type, double cashToBuy, double takeProfit, double stopLose, boolean isFullPrice) {
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

    @Override
    public boolean sellByMarket(String name, TickerType type, double cashToSell, double takeProfit, double stopLose) {
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

    @Override
    public boolean closeLongByMarket(String name, TickerType type) {
        return sellByMarket(name, type, Double.MAX_VALUE, 0.0, 0.0);
    }

    @Override
    public boolean closeShortByMarket(String name, TickerType type) {
        return sellByMarket(name, type, Double.MAX_VALUE, 0.0, 0.0);
    }

    @Override
    public void closeAllByMarket(TickerType type) {
        List<TickerInfo.Key> toClose = new ArrayList<>(positions.keySet());
        for (TickerInfo.Key key : toClose) {
            if (type == TickerType.ALL || type == null || key.getType() == type) {
                PositionInfo pos = positions.get(key);
                if (pos != null && pos.getBalance() > 0) {
                    sellByMarket(key.getTicker(), key.getType(), Double.MAX_VALUE, 0.0, 0.0);
                }
            }
        }
    }

    // ========== No-op Methods (not needed in backtest) ==========

    @Override
    public Position restoreProtectivePosition(String name, TickerType type, Position position) {
        // No-op in backtest - return position as-is
        return position;
    }

    @Override
    public void syncProtectiveOrders(String name, TickerType type, Position position) {
        // No-op in backtest - no real orders to sync
    }

    // ========== Market Data Methods ==========

    @Override
    public Map<String, Map<Double, Integer>> getCurrentPrices(TickerInfo.Key key, boolean isPrintGlass) {
        Double price = currentPrices.get(key);
        if (price == null) {
            // Fallback: get from last candle
            TickerInfo info = tickerInfo.get(key);
            if (info != null) {
                List<Candle> candles = loadCandlesFromCsv(info.getTicker(), "HOUR");
                if (!candles.isEmpty()) {
                    price = candles.get(candles.size() - 1).close;
                    currentPrices.put(key, price);
                }
            }
        }

        if (price == null) {
            Map<String, Map<Double, Integer>> empty = new HashMap<>();
            empty.put("bids", new HashMap<>());
            empty.put("asks", new HashMap<>());
            return empty;
        }

        // Simulate bid/ask spread (0.01%)
        double spread = price * 0.0001;
        Map<Double, Integer> bids = new LinkedHashMap<>();
        Map<Double, Integer> asks = new LinkedHashMap<>();
        bids.put(price - spread, 1000);
        asks.put(price + spread, 1000);

        Map<String, Map<Double, Integer>> result = new HashMap<>();
        result.put("bids", bids);
        result.put("asks", asks);
        return result;
    }

    @Override
    public double getLiveAskPrice(TickerInfo.Key key) {
        Map<String, Map<Double, Integer>> prices = getCurrentPrices(key, false);
        Map<Double, Integer> asks = prices.get("asks");
        if (asks == null || asks.isEmpty()) {
            return 0.0;
        }
        return asks.keySet().iterator().next();
    }

    @Override
    public double getLiveBidPrice(TickerInfo.Key key) {
        Map<String, Map<Double, Integer>> prices = getCurrentPrices(key, false);
        Map<Double, Integer> bids = prices.get("bids");
        if (bids == null || bids.isEmpty()) {
            return 0.0;
        }
        return bids.keySet().iterator().next();
    }

    @Override
    public double getAvailablePrice(TickerInfo.Key key, int count, String glassType, boolean isPrintGlass) {
        Map<String, Map<Double, Integer>> prices = getCurrentPrices(key, isPrintGlass);
        Map<Double, Integer> levels = prices.get(glassType);
        if (levels == null || levels.isEmpty()) {
            return 0.0;
        }
        return levels.keySet().iterator().next();
    }

    @Override
    public MarketDepthSnapshot getLastMarketDepth(TickerInfo.Key key) {
        // Not used in backtest
        return null;
    }

    @Override
    public List<MarketTradeTick> getRecentTrades(TickerInfo.Key key, Duration maxAge) {
        // Not used in backtest
        return Collections.emptyList();
    }

    // ========== Ticker Info Methods ==========

    @Override
    public TickerInfo searchTicker(TickerInfo.Key key) {
        TickerInfo info = tickerInfo.get(key);
        if (info != null) {
            return info;
        }
        // Fallback: try to create from known ticker
        TickerInfo fallback = new TickerInfo(
            null, key.getTicker(), null, 0.01, 1, "RUB", key.getTicker(), key.getType().name()
        );
        tickerInfo.put(key, fallback);
        return fallback;
    }

    public void registerTicker(TickerInfo info) {
        tickerInfo.put(info.getKey(), info);
    }

    public String figiByName(TickerInfo.Key key) {
        TickerInfo info = searchTicker(key);
        return info != null ? info.getFigi() : "FAKE_FIGI_" + key.getTicker();
    }

    // ========== Helper Methods ==========

    @Override
    public int calculateTradeCount(TickerInfo.Key key, double cashToSell, double tickerPrice) {
        TickerInfo info = tickerInfo.get(key);
        int lots = (info != null && info.getLot() > 0) ? info.getLot() : 1;
        // Apply 1% safety margin matching live trading
        double effectivePrice = tickerPrice * 1.01;
        double tradeUnitCost = effectivePrice * lots;
        if (cashToSell < tradeUnitCost) {
            return 0;
        }
        return (int) Math.floor(cashToSell / tradeUnitCost);
    }

    public void updatePrice(TickerInfo.Key key, double price) {
        currentPrices.put(key, price);
    }

    public void updatePosition(String ticker, TickerType type, int balance, double avgPrice) {
        TickerInfo.Key key = new TickerInfo.Key(ticker, type);
        TickerInfo info = tickerInfo.get(key);
        if (info == null) {
            info = new TickerInfo(null, ticker, null, 0.01, 1, "RUB", ticker, type.name());
            tickerInfo.put(key, info);
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

    public double getInitialBalance() {
        return initialBalance;
    }

    public double getCommissionRate() {
        return commissionRate;
    }

    public double getEffectiveCommission(String ticker) {
        if ("TMON@".equalsIgnoreCase(ticker)) {
            return 0.0;
        }
        return commissionRate;
    }

    private TickerInfo findTickerInfo(String ticker) {
        for (TickerInfo info : tickerInfo.values()) {
            if (info.getName().equalsIgnoreCase(ticker) || info.getTicker().equalsIgnoreCase(ticker)) {
                return info;
            }
        }
        return null;
    }

    // ========== Broker Position Tracking ==========

    public static class BrokerPosition {
        public final String ticker;
        public final TickerType type;
        public final String direction;
        public final int quantity;
        public final double entryPrice;
        public final Double stopLoss;
        public final Double takeProfit;
        public final int leverage;
        public final int candlesHeld;
        public final int cooldownRemaining;
        public double realizedPnl = 0.0;

        public BrokerPosition(
                String ticker, TickerType type, String direction, int quantity,
                double entryPrice, Double stopLoss, Double takeProfit,
                int leverage, int candlesHeld, int cooldownRemaining) {
            this.ticker = ticker;
            this.type = type;
            this.direction = direction;
            this.quantity = quantity;
            this.entryPrice = entryPrice;
            this.stopLoss = stopLoss;
            this.takeProfit = takeProfit;
            this.leverage = leverage;
            this.candlesHeld = candlesHeld;
            this.cooldownRemaining = cooldownRemaining;
        }
    }

    private final Map<String, BrokerPosition> openPositions = new ConcurrentHashMap<>();

    public boolean openPosition(
            String ticker, TickerType type, String direction, int quantity,
            double entryPrice, Double stopLoss, Double takeProfit,
            int leverage, int candlesHeld, int cooldownRemaining) {
        if (quantity <= 0 || entryPrice <= 0.0) {
            return false;
        }

        double margin = getRequiredMargin(ticker, quantity, entryPrice, leverage);
        double notional = getNotionalValue(quantity, entryPrice);
        double commission = notional * getEffectiveCommission(ticker);
        double totalRequired = margin + commission;

        if (totalRequired > availableCash) {
            return false;
        }

        availableCash -= totalRequired;

        BrokerPosition bp = new BrokerPosition(
                ticker, type, direction, quantity, entryPrice,
                stopLoss, takeProfit, leverage, candlesHeld, cooldownRemaining);
        openPositions.put(ticker, bp);

        // Also update PositionInfo for compatibility
        TickerInfo.Key key = new TickerInfo.Key(ticker, type);
        TickerInfo info = tickerInfo.get(key);
        if (info != null) {
            PositionInfo pi = new PositionInfo(
                    null, ticker, null, type.name(),
                    quantity, 0.0, 1, entryPrice, ticker);
            positions.put(key, pi);
        }

        return true;
    }

    public double closePosition(String ticker, double exitPrice, String reason) {
        BrokerPosition bp = openPositions.remove(ticker);
        if (bp == null || bp.quantity <= 0) {
            return 0.0;
        }

        boolean isShort = "SELL".equals(bp.direction);
        double entryNotional = getNotionalValue(bp.quantity, bp.entryPrice);
        double exitNotional = getNotionalValue(bp.quantity, exitPrice);
        double grossPnl = calculateGrossPnl(entryNotional, exitNotional, isShort);
        double entryCommission = entryNotional * getEffectiveCommission(ticker);
        double exitCommission = exitNotional * getEffectiveCommission(ticker);
        double pnl = grossPnl - entryCommission - exitCommission;

        double margin = getRequiredMargin(ticker, bp.quantity, bp.entryPrice, bp.leverage);
        availableCash += (margin + grossPnl - exitCommission);

        bp.realizedPnl += pnl;

        // Clear broker-level position info
        positions.remove(new TickerInfo.Key(ticker, bp.type));

        return pnl;
    }

    public BrokerPosition getOpenPosition(String ticker) {
        return openPositions.get(ticker);
    }

    public double getRealizedPnl(String ticker) {
        BrokerPosition bp = openPositions.get(ticker);
        return bp != null ? bp.realizedPnl : 0.0;
    }

    // ========== Order Execution Methods (for LiveOrderExecutor compatibility) ==========

    @Override
    public OrderExecutionResult buyByMarketWithDetails(
            String name, TickerType type, double cashToBuy, double takeProfit, double stopLose) {
        return buy(name, type, cashToBuy, true, takeProfit, stopLose, false);
    }

    @Override
    public OrderExecutionResult sellByMarketWithDetails(
            String name, TickerType type, double cashToSell, double takeProfit, double stopLose) {
        return sell(name, type, cashToSell, true, takeProfit, stopLose, false);
    }

    @Override
    public OrderExecutionResult closeLongByMarketWithDetails(String name, TickerType type) {
        int count = getCountOfCurrentPositions(type, name);
        if (count > 0) {
            // In test mode, simulate success
            if (true) { // Always test mode in backtest
                TickerInfo.Key key = new TickerInfo.Key(name, type);
                double price = getLiveAskPrice(key);
                return OrderExecutionResult.testSuccess(price, count);
            }
        }
        return OrderExecutionResult.failed();
    }

    @Override
    public OrderExecutionResult closeShortByMarketWithDetails(String name, TickerType type) {
        int count = getCountOfCurrentPositions(type, name);
        if (count < 0) {
            if (true) { // Always test mode in backtest
                TickerInfo.Key key = new TickerInfo.Key(name, type);
                double price = getLiveAskPrice(key);
                return OrderExecutionResult.testSuccess(price, Math.abs(count));
            }
        }
        return OrderExecutionResult.failed();
    }

    /**
     * Internal buy implementation with full control.
     */
    @Override
    public OrderExecutionResult buy(
            String name, TickerType type, double cashToBuy, boolean byMarket,
            double takeProfit, double stopLose, boolean isFullPrice) {
        TickerInfo.Key key = new TickerInfo.Key(name, type);
        TickerInfo info = tickerInfo.get(key);
        if (info == null) {
            return OrderExecutionResult.failed();
        }

        Double price = currentPrices.get(key);
        if (price == null || price <= 0) {
            return OrderExecutionResult.failed();
        }

        int lots = info.getLot() > 0 ? info.getLot() : 1;
        double lotValue = price * lots;
        int lotsToBuy = (int) Math.floor(cashToBuy / lotValue);

        if (lotsToBuy <= 0) {
            return OrderExecutionResult.failed();
        }

        double cost = lotsToBuy * lotValue;
        if (cost > availableCash) {
            return OrderExecutionResult.failed();
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

        return OrderExecutionResult.testSuccess(price, lotsToBuy);
    }

    /**
     * Internal sell implementation with full control.
     */
    @Override
    public OrderExecutionResult sell(
            String name, TickerType type, double cashToSell, boolean byMarket,
            double takeProfit, double stopLose, boolean isFullPrice) {
        TickerInfo.Key key = new TickerInfo.Key(name, type);
        PositionInfo pos = positions.get(key);
        if (pos == null || pos.getBalance() <= 0) {
            return OrderExecutionResult.failed();
        }

        Double price = currentPrices.get(key);
        if (price == null || price <= 0) {
            price = pos.getAveragePositionPrice();
        }
        if (price == null || price <= 0) {
            return OrderExecutionResult.failed();
        }

        int lots = pos.getLots() > 0 ? pos.getLots() : 1;
        double lotValue = price * lots;
        int lotsToSell = (int) Math.floor(cashToSell / lotValue);
        if (lotsToSell <= 0) {
            lotsToSell = 1;
        }
        lotsToSell = Math.min(lotsToSell, pos.getBalance());

        if (lotsToSell <= 0) {
            return OrderExecutionResult.failed();
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

        return OrderExecutionResult.testSuccess(price, lotsToSell);
    }

    // ========== Portfolio Accounting ==========

    public double getNotionalValue(int quantity, double price) {
        if (quantity <= 0 || price <= 0.0) {
            return 0.0;
        }
        return quantity * price;
    }

    public double getMarginMultiplier(String ticker, int leverage) {
        double marginMultiplier = 1.0;
        TickerInfo info = findTickerInfo(ticker);
        if (info != null && TickerType.FEATURE == info.getType()) {
            marginMultiplier = TCSService.FUTURES_MARGIN_RATE;
        }
        if (leverage > 1) {
            marginMultiplier /= leverage;
        }
        return marginMultiplier;
    }

    public double getRequiredMargin(String ticker, int quantity, double price, int leverage) {
        if (quantity <= 0 || price <= 0.0) {
            return 0.0;
        }
        return getNotionalValue(quantity, price) * getMarginMultiplier(ticker, leverage);
    }

    public double calculateGrossPnl(double entryNotional, double exitNotional, boolean isShort) {
        return isShort ? entryNotional - exitNotional : exitNotional - entryNotional;
    }

    public double getPositionMarketValue(
            String ticker, com.github.shk0da.goldendragon.model.Position position,
            double entryPrice, double currentPrice) {
        if (position == null || position.quantity <= 0) {
            return 0.0;
        }
        int leverage = resolveLeverageForPosition(position);
        boolean isShort = "SELL".equals(position.direction);
        double entryMargin = getRequiredMargin(ticker, position.quantity, entryPrice, leverage);
        double entryNotional = getNotionalValue(position.quantity, entryPrice);
        double markNotional = getNotionalValue(position.quantity, currentPrice);
        double grossPnl = calculateGrossPnl(entryNotional, markNotional, isShort);
        return entryMargin + grossPnl;
    }

    public double getTickerEquity(
            String ticker, com.github.shk0da.goldendragon.model.Position position,
            double entryPrice, double realizedPnl, double currentPrice) {
        double unrealizedPnl = 0.0;
        if (position != null && position.quantity > 0) {
            boolean isShort = "SELL".equals(position.direction);
            double entryNotional = getNotionalValue(position.quantity, entryPrice);
            double markNotional = getNotionalValue(position.quantity, currentPrice);
            double grossPnl = calculateGrossPnl(entryNotional, markNotional, isShort);
            double entryCommission = entryNotional * getEffectiveCommission(ticker);
            double exitCommission = markNotional * getEffectiveCommission(ticker);
            unrealizedPnl = grossPnl - entryCommission - exitCommission;
        }
        return initialBalance + realizedPnl + unrealizedPnl;
    }

    private int resolveLeverageForPosition(com.github.shk0da.goldendragon.model.Position position) {
        return position != null && position.quantity > 0
                ? Math.max(1, position.appliedLeverage)
                : 1;
    }

    public int resolvePositionLeverage(com.github.shk0da.goldendragon.model.Position position, int configuredLeverage) {
        if (position != null && position.quantity > 0) {
            return Math.max(1, position.appliedLeverage);
        }
        return Math.max(1, configuredLeverage);
    }

    // ========== Candle Loading ==========

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

            List<TickerCandle> tcList = com.github.shk0da.goldendragon.strategy.DataCollector.readCandlesFile(ticker, dataDir, interval);
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

    @Override
    public List<Candle> getCandles(String ticker, String interval) {
        if ("HOUR".equals(interval)) {
            if (!cachedHourCandles.containsKey(ticker)) {
                cachedHourCandles.put(ticker, loadCandlesFromCsv(ticker, "HOUR"));
            }
            return cachedHourCandles.get(ticker);
        }
        if ("5_MIN".equals(interval)) {
            if (!cachedMinuteCandles.containsKey(ticker)) {
                cachedMinuteCandles.put(ticker, loadCandlesFromCsv(ticker, "5_MIN"));
            }
            return cachedMinuteCandles.get(ticker);
        }
        return Collections.emptyList();
    }

    @Override
    public MarketPrices getLivePrices(String ticker) {
        TickerInfo info = findTickerInfo(ticker);
        if (info == null) {
            return new MarketPrices(null, null);
        }
        TickerInfo.Key key = info.getKey();
        double ask = getLiveAskPrice(key);
        double bid = getLiveBidPrice(key);
        if (ask <= 0.0 && bid <= 0.0) {
            return new MarketPrices(null, null);
        }
        return new MarketPrices(bid > 0.0 ? bid : null, ask > 0.0 ? ask : null);
    }

    @Override
    public boolean isLive() {
        return false;
    }

    @Override
    public int getAvailableLiquidity(String ticker, String side) {
        return Integer.MAX_VALUE;
    }

    @Override
    public String getCurrentTime() {
        return "";
    }

    public List<Candle> getHourCandles(String ticker) {
        return getCandles(ticker, "HOUR");
    }

    public List<Candle> getMinuteCandles(String ticker) {
        return getCandles(ticker, "5_MIN");
    }

    // ========== Monthly Deposit Support ==========

    /**
     * Adds monthly deposit to available cash.
     * @param amount deposit amount
     */
    public void addMonthlyDeposit(double amount) {
        availableCash += amount;
    }
}
