package com.github.shk0da.goldendragon.service;

import com.github.shk0da.goldendragon.model.Candle;
import com.github.shk0da.goldendragon.model.MarketDepthSnapshot;
import com.github.shk0da.goldendragon.model.MarketTickListener;
import com.github.shk0da.goldendragon.model.MarketTradeTick;
import com.github.shk0da.goldendragon.model.Position;
import com.github.shk0da.goldendragon.model.PositionInfo;
import com.github.shk0da.goldendragon.model.TickerCandle;
import com.github.shk0da.goldendragon.model.TickerInfo;
import com.github.shk0da.goldendragon.model.TickerType;

import java.time.Duration;
import java.time.Instant;
import java.time.OffsetDateTime;
import java.util.List;
import java.util.Map;

/**
 * Common interface for trading services (Tinkoff TCS, ByBit, etc.).
 * Provides unified API for market data, trading, and account operations.
 */
public interface TradingService {

    /** Default futures margin rate (40% of the full order cost). */
    double FUTURES_MARGIN_RATE = 0.40;

    /**
     * Result of an order execution with details on price, count, commission, and the
     * protective position that was created as part of a bracket order.
     */
    class OrderExecutionResult {

        private final boolean success;
        private final Double executedPrice;
        private final int executedCount;
        private final double commission;
        private final Position protectivePosition;
        private final int errorCode;
        private final String errorMessage;

        private OrderExecutionResult(
                boolean success,
                Double executedPrice,
                int executedCount,
                double commission,
                Position protectivePosition) {
            this(success, executedPrice, executedCount, commission, protectivePosition, 0, null);
        }

        private OrderExecutionResult(
                boolean success,
                Double executedPrice,
                int executedCount,
                double commission,
                Position protectivePosition,
                int errorCode) {
            this(success, executedPrice, executedCount, commission, protectivePosition, errorCode, null);
        }

        private OrderExecutionResult(
                boolean success,
                Double executedPrice,
                int executedCount,
                double commission,
                Position protectivePosition,
                int errorCode,
                String errorMessage) {
            this.success = success;
            this.executedPrice = executedPrice;
            this.executedCount = executedCount;
            this.commission = commission;
            this.protectivePosition = protectivePosition;
            this.errorCode = errorCode;
            this.errorMessage = errorMessage;
        }

        public static OrderExecutionResult success(
                Double executedPrice,
                int executedCount,
                double commission,
                Position protectivePosition) {
            return new OrderExecutionResult(
                    true, executedPrice, executedCount, commission, protectivePosition);
        }

        public static OrderExecutionResult testSuccess(Double executedPrice, int executedCount) {
            return new OrderExecutionResult(true, executedPrice, executedCount, 0.0, null);
        }

        public static OrderExecutionResult failed() {
            return new OrderExecutionResult(false, null, 0, 0.0, null);
        }

        public static OrderExecutionResult failed(int errorCode) {
            return new OrderExecutionResult(false, null, 0, 0.0, null, errorCode);
        }

        public static OrderExecutionResult failed(String errorMessage) {
            return new OrderExecutionResult(false, null, 0, 0.0, null, 0, errorMessage);
        }

        public static OrderExecutionResult failed(int errorCode, String errorMessage) {
            return new OrderExecutionResult(false, null, 0, 0.0, null, errorCode, errorMessage);
        }

        public boolean isSuccess() {
            return success;
        }

        public String getErrorMessage() {
            return errorMessage;
        }

        public Double getExecutedPrice() {
            return executedPrice;
        }

        public int getExecutedCount() {
            return executedCount;
        }

        public double getCommission() {
            return commission;
        }

        public Position getProtectivePosition() {
            return protectivePosition;
        }

        public int getErrorCode() {
            return errorCode;
        }
    }

    // ==================== INSTRUMENT METHODS ====================

    /**
     * Returns the list of all tradable futures.
     */
    Map<TickerInfo.Key, TickerInfo> getFuturesList();

    /**
     * Returns the list of all tradable stocks.
     */
    Map<TickerInfo.Key, TickerInfo> getStockList();

    /**
     * Returns the list of all tradable ETFs.
     */
    Map<TickerInfo.Key, TickerInfo> getEtfList();

    /**
     * Returns the list of all tradable bonds.
     */
    Map<TickerInfo.Key, TickerInfo> getBondList();

    /**
     * Returns the list of all tradable currencies.
     */
    Map<TickerInfo.Key, TickerInfo> getCurrenciesList();

    /**
     * Searches for a ticker by its key.
     */
    TickerInfo searchTicker(TickerInfo.Key key);

    /**
     * Returns the FIGI/symbol identifier for the given ticker key.
     */
    String figiByName(TickerInfo.Key key);

    /**
     * Checks whether the instrument can be traded.
     */
    boolean isTradableForAccount(TickerInfo info);

    /**
     * Logs account trading eligibility.
     */
    void logAccountTradingEligibility();

    /**
     * Logs current account positions.
     */
    void logAccountPositions();

    // ==================== MARKET DATA METHODS ====================

    /**
     * Retrieves historical candles for the given FIGI/symbol and time range.
     * Returns domain Candle objects (broker-agnostic).
     */
    List<Candle> getCandles(
            String figi, Instant start, Instant end,
            String interval);

    /**
     * Retrieves historical candles using OffsetDateTime parameters.
     * Returns domain Candle objects (broker-agnostic).
     */
    List<Candle> getCandles(
            String figi, OffsetDateTime start, OffsetDateTime end,
            String interval);

    /**
     * Returns the last {@code size} hourly candles as domain objects.
     */
    List<Candle> getLastCandles(
            String ticker, TickerType type, int size);

    /**
     * Returns the last hourly candles as TickerCandle domain objects.
     */
    List<TickerCandle> getLastCandlesAsTickerCandles(
            String ticker, TickerType type, int count);

    /**
     * Returns the current prices (bids and asks) for the given ticker.
     */
    Map<String, Map<Double, Long>> getCurrentPrices(TickerInfo.Key key, boolean isPrintGlass);

    /**
     * Returns the current prices (bids and asks) for the given ticker.
     */
    Map<String, Map<Double, Long>> getCurrentPrices(TickerInfo.Key key);

    /**
     * Returns the best (lowest) live ask price from the orderbook.
     */
    double getLiveAskPrice(TickerInfo.Key key);

    /**
     * Returns the best bid price from the current order book.
     */
    double getLiveBidPrice(TickerInfo.Key key);

    /**
     * Returns the best available price for a single instrument.
     */
    double getAvailablePrice(String name, TickerType type);

    /**
     * Returns the best available price for a single instrument.
     */
    double getAvailablePrice(TickerInfo.Key key);

    /**
     * Returns the best available price for a given quantity.
     */
    double getAvailablePrice(String name, TickerType type, int count, String glassType);

    /**
     * Returns the best available price for a given quantity.
     */
    double getAvailablePrice(String name, TickerType type, int count, String glassType, boolean isPrintGlass);

    /**
     * Returns the best available price for a given quantity.
     */
    double getAvailablePrice(TickerInfo.Key key, int count, boolean isPrintGlass);

    /**
     * Returns the best available price for a given quantity from the specified side.
     */
    double getAvailablePrice(TickerInfo.Key key, int count, String type, boolean isPrintGlass);

    /**
     * Returns recent trades from the real-time stream.
     */
    List<MarketTradeTick> getRecentTrades(TickerInfo.Key key, Duration maxAge);

    /**
     * Retrieves historical trades for the given ticker.
     */
    List<MarketTradeTick> getLastTrades(TickerInfo.Key key, Instant from, Instant to);

    /**
     * Returns the most recent market depth snapshot.
     */
    MarketDepthSnapshot getLastMarketDepth(TickerInfo.Key key);

    /**
     * Subscribes to real-time market data (order book and trades).
     */
    void subscribeMarketData(TickerInfo.Key key, int depth, MarketTickListener listener);

    /**
     * Unsubscribes from real-time market data.
     */
    void unsubscribeMarketData(TickerInfo.Key key, MarketTickListener listener);

    // ==================== ACCOUNT METHODS ====================

    /**
     * Returns the available cash balance.
     */
    Double getAvailableCash();

    /**
     * Returns the total portfolio value.
     */
    double getTotalPortfolioCost();

    /**
     * Returns the current balance of the position for the given ticker.
     */
    int getCountOfCurrentPositions(TickerType tickerType, String tickerName);

    /**
     * Returns the position info for the given ticker name.
     */
    PositionInfo getCurrentPositions(TickerType tickerType, String tickerName);

    /**
     * Returns all current positions.
     */
    Map<TickerInfo.Key, PositionInfo> getCurrentPositions(TickerType tickerType);

    // ==================== TRADING METHODS ====================

    /**
     * Calculates the maximum number of instruments that can be traded.
     */
    int calculateTradeCount(TickerInfo.Key key, double availableCash, double price);

    /**
     * Calculates the total cash required to trade.
     */
    double getRequiredCashForOrder(TickerInfo.Key key, int count, double price);

    /**
     * Creates an order and returns 1 on success or 0 on failure.
     */
    int createOrder(TickerInfo.Key key, double price, int count, String operation);

    /**
     * Creates an order with optional protective orders.
     */
    OrderExecutionResult createOrder(
            TickerInfo.Key key,
            double price,
            int count,
            String operation,
            double takeProfit,
            double stopLose,
            boolean isFullPrice);

    /**
     * Creates an order with the full set of parameters.
     */
    OrderExecutionResult createOrder(
            TickerInfo.Key key,
            double price,
            int count,
            String operation,
            double takeProfit,
            double stopLose,
            boolean isFullPrice,
            double cashToUse);

    /**
     * Buys by market price and returns execution details.
     */
    OrderExecutionResult buyByMarketWithDetails(
            String name, TickerType type, double cashToBuy, double takeProfit, double stopLose);

    /**
     * Sells by market price and returns execution details.
     */
    OrderExecutionResult sellByMarketWithDetails(
            String name, TickerType type, double cashToSell, double takeProfit, double stopLose);

    /**
     * Buys the given cash amount.
     */
    OrderExecutionResult buy(
            String name,
            TickerType type,
            double cashToBuy,
            boolean byMarket,
            double takeProfit,
            double stopLose,
            boolean isFullPrice);

    /**
     * Sells the given cash amount.
     */
    OrderExecutionResult sell(
            String name,
            TickerType type,
            double cashToSell,
            boolean byMarket,
            double takeProfit,
            double stopLose,
            boolean isFullPrice);

    /**
     * Closes the entire long position.
     */
    boolean closeLongByMarket(String name, TickerType type);

    /**
     * Closes the entire long position and returns execution details.
     */
    OrderExecutionResult closeLongByMarketWithDetails(String name, TickerType type);

    /**
     * Closes the entire short position.
     */
    boolean closeShortByMarket(String name, TickerType type);

    /**
     * Closes the entire short position and returns execution details.
     */
    OrderExecutionResult closeShortByMarketWithDetails(String name, TickerType type);

    /**
     * Closes all positions by market orders.
     */
    void closeAllByMarket(TickerType type);

    /**
     * Places a server-side stop-loss order.
     */
    StopLossOrderResult createStopLossOrder(
            TickerInfo.Key key,
            int units,
            double stopLossPrice,
            String operation);

    /**
     * Cancels a stop order.
     */
    void cancelStopOrder(TickerInfo.Key key, String stopOrderId, String orderTypeName);

    /**
     * Synchronizes protective orders.
     */
    void syncProtectiveOrders(String name, TickerType type, Position position);

    /**
     * Restores protective position from broker orders.
     */
    Position restoreProtectivePosition(String name, TickerType type, Position position);

    /**
     * Returns the last executed price for the given ticker.
     */
    Double getLastExecutedPrice(String name, TickerType type);

    /**
     * Result of a server-side stop-loss order placement.
     */
    class StopLossOrderResult {
        public final String orderId;
        private final boolean success;

        public StopLossOrderResult(String orderId, boolean success) {
            this.orderId = orderId;
            this.success = success;
        }

        public String getOrderId() {
            return orderId;
        }

        public boolean isSuccess() {
            return success;
        }

        public static StopLossOrderResult success(String orderId) {
            return new StopLossOrderResult(orderId, true);
        }

        public static StopLossOrderResult failed() {
            return new StopLossOrderResult(null, false);
        }
    }

    /**
     * Returns the trading service type that determines which instruments are traded:
     * {@link TradingServiceType#BYBIT} for crypto or {@link TradingServiceType#TINKOFF} for stocks.
     *
     * @return the configured trading service type
     */
    TradingServiceType getServiceType();

    /**
     * Checks if trading is in paper/simulation mode (no real money).
     * For TINKOFF: true when tcs.testMode=true.
     * For BYBIT: true when bybit.testMode=true.
     *
     * @return true if paper trading is enabled, false for live trading
     */
    boolean isPaperTrading();

    /**
     * Trading service types. BYBIT trades crypto (USDT perpetuals),
     * TINKOFF trades stocks/futures on the Moscow Exchange.
     */
    enum TradingServiceType {
        TINKOFF,
        BYBIT
    }
}
