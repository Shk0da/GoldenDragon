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
 * Common interface for trading services.
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

        public static OrderExecutionResult failed(String errorMessage) {
            return new OrderExecutionResult(false, null, 0, 0.0, null);
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
     * Returns the last {@code count} candles for the given symbol and interval.
     * Useful for live candle-based strategies that poll recent candles.
     */
    List<Candle> getCandles(String figi, String interval, int count);

    /**
     * Returns the current prices (bids and asks) for the given ticker.
     */
    Map<String, Map<Double, Long>> getCurrentPrices(TickerInfo.Key key, boolean isPrintGlass);

    /**
     * Returns the best available price for a single instrument.
     */
    double getAvailablePrice(TickerInfo.Key key);

    /**
     * Returns the best (lowest) live ask price from the orderbook.
     */
    double getLiveAskPrice(TickerInfo.Key key);

    /**
     * Returns the best bid price from the current order book.
     */
    double getLiveBidPrice(TickerInfo.Key key);

    /**
     * Returns the best available price for a given quantity from the specified side.
     */
    double getAvailablePrice(TickerInfo.Key key, int count, String type, boolean isPrintGlass);

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
     * Partially closes a long position by specified quantity.
     */
    OrderExecutionResult closeLongByMarketWithDetails(String name, TickerType type, int quantity);

    /**
     * Move stop-loss to breakeven after TP1 execution.
     */
    void moveStopLossToBreakeven(TickerInfo.Key key, double entryPrice, int quantity, String direction);

    /**
     * Closes the entire short position.
     */
    boolean closeShortByMarket(String name, TickerType type);

    /**
     * Closes the entire short position and returns execution details.
     */
    OrderExecutionResult closeShortByMarketWithDetails(String name, TickerType type);

    /**
     * Partially closes a short position by specified quantity.
     */
    OrderExecutionResult closeShortByMarketWithDetails(String name, TickerType type, int quantity);

    /**
     * Closes all positions by market orders.
     */
    void closeAllByMarket(TickerType type);

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
}
