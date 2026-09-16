package com.github.shk0da.goldendragon.service;

import com.github.shk0da.goldendragon.model.Candle;
import com.github.shk0da.goldendragon.model.Position;
import com.github.shk0da.goldendragon.model.PositionInfo;
import com.github.shk0da.goldendragon.model.TickerInfo;
import com.github.shk0da.goldendragon.model.TickerType;

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
    default Map<TickerInfo.Key, TickerInfo> getFuturesList() {
        throw new UnsupportedOperationException("getFuturesList not implemented");
    }

    /**
     * Returns the list of all tradable stocks.
     */
    default Map<TickerInfo.Key, TickerInfo> getStockList() {
        throw new UnsupportedOperationException("getStockList not implemented");
    }

    /**
     * Returns the list of all tradable ETFs.
     */
    default Map<TickerInfo.Key, TickerInfo> getEtfList() {
        throw new UnsupportedOperationException("getEtfList not implemented");
    }

    /**
     * Returns the list of all tradable bonds.
     */
    default Map<TickerInfo.Key, TickerInfo> getBondList() {
        throw new UnsupportedOperationException("getBondList not implemented");
    }

    /**
     * Returns the list of all tradable currencies.
     */
    default Map<TickerInfo.Key, TickerInfo> getCurrenciesList() {
        throw new UnsupportedOperationException("getCurrenciesList not implemented");
    }

    /**
     * Searches for a ticker by its key.
     */
    default TickerInfo searchTicker(TickerInfo.Key key) {
        throw new UnsupportedOperationException("searchTicker not implemented");
    }

    /**
     * Returns the FIGI/symbol identifier for the given ticker key.
     */
    default String figiByName(TickerInfo.Key key) {
        throw new UnsupportedOperationException("figiByName not implemented");
    }

    // ==================== MARKET DATA METHODS ====================

    /**
     * Retrieves historical candles for the given FIGI/symbol and time range.
     * Returns domain Candle objects (broker-agnostic).
     */
    default List<Candle> getCandles(
            String figi, Instant start, Instant end,
            String interval) {
        throw new UnsupportedOperationException("getCandles not implemented");
    }

    /**
     * Retrieves historical candles using OffsetDateTime parameters.
     * Returns domain Candle objects (broker-agnostic).
     */
    default List<Candle> getCandles(
            String figi, OffsetDateTime start, OffsetDateTime end,
            String interval) {
        throw new UnsupportedOperationException("getCandles not implemented");
    }

    /**
     * Returns the last {@code size} hourly candles as domain objects.
     */
    default List<Candle> getLastCandles(
            String ticker, TickerType type, int size) {
        throw new UnsupportedOperationException("getLastCandles not implemented");
    }

    /**
     * Returns the last {@code count} candles for the given symbol and interval.
     * Useful for live candle-based strategies that poll recent candles.
     */
    default List<Candle> getCandles(String figi, String interval, int count) {
        throw new UnsupportedOperationException("getCandles not implemented");
    }

    /**
     * Returns the current prices (bids and asks) for the given ticker.
     */
    default Map<String, Map<Double, Long>> getCurrentPrices(TickerInfo.Key key, boolean isPrintGlass) {
        throw new UnsupportedOperationException("getCurrentPrices not implemented");
    }

    /**
     * Returns the best available price for a single instrument.
     */
    default double getAvailablePrice(TickerInfo.Key key) {
        throw new UnsupportedOperationException("getAvailablePrice not implemented");
    }

    /**
     * Returns the best (lowest) live ask price from the orderbook.
     */
    default double getLiveAskPrice(TickerInfo.Key key) {
        throw new UnsupportedOperationException("getLiveAskPrice not implemented");
    }

    /**
     * Returns the best bid price from the current order book.
     */
    default double getLiveBidPrice(TickerInfo.Key key) {
        throw new UnsupportedOperationException("getLiveBidPrice not implemented");
    }

    /**
     * Returns the best available price for a given quantity from the specified side.
     */
    default double getAvailablePrice(TickerInfo.Key key, int count, String type, boolean isPrintGlass) {
        throw new UnsupportedOperationException("getAvailablePrice not implemented");
    }

    // ==================== ACCOUNT METHODS ====================

    /**
     * Returns the available cash balance.
     */
    default Double getAvailableCash() {
        throw new UnsupportedOperationException("getAvailableCash not implemented");
    }

    /**
     * Returns the initial balance (for backtest position sizing based on starting capital).
     * Default returns 0.0 (live trading uses current balance).
     */
    default double getInitialBalance() {
        throw new UnsupportedOperationException("getInitialBalance not implemented");
    }

    /**
     * Returns the total portfolio value (cash + unrealized PnL of all positions).
     */
    default double getTotalPortfolioValue() {
        throw new UnsupportedOperationException("getTotalPortfolioValue not implemented");
    }

    /**
     * Returns the global peak portfolio value across the entire session (never reset).
     */
    default double getGlobalPeakEquity() {
        throw new UnsupportedOperationException("getGlobalPeakEquity not implemented");
    }

    /**
     * Returns the total portfolio cost (legacy alias for getTotalPortfolioValue).
     */
    default double getTotalPortfolioCost() {
        throw new UnsupportedOperationException("getTotalPortfolioCost not implemented");
    }

    /**
     * Returns the current balance of the position for the given ticker.
     */
    default int getCountOfCurrentPositions(TickerType tickerType, String tickerName) {
        throw new UnsupportedOperationException("getCountOfCurrentPositions not implemented");
    }

    /**
     * Returns the position info for the given ticker name.
     */
    default PositionInfo getCurrentPositions(TickerType tickerType, String tickerName) {
        throw new UnsupportedOperationException("getCurrentPositions not implemented");
    }

    /**
     * Returns all current positions.
     */
    default Map<TickerInfo.Key, PositionInfo> getCurrentPositions(TickerType tickerType) {
        throw new UnsupportedOperationException("getCurrentPositions not implemented");
    }

    // ==================== TRADING METHODS ====================

    /**
     * Calculates the maximum number of instruments that can be traded.
     */
    default int calculateTradeCount(TickerInfo.Key key, double availableCash, double price) {
        throw new UnsupportedOperationException("calculateTradeCount not implemented");
    }

    /**
     * Calculates the total cash required to trade.
     */
    default double getRequiredCashForOrder(TickerInfo.Key key, int count, double price) {
        throw new UnsupportedOperationException("getRequiredCashForOrder not implemented");
    }

    /**
     * Creates an order and returns 1 on success or 0 on failure.
     */
    default int createOrder(TickerInfo.Key key, double price, int count, String operation) {
        throw new UnsupportedOperationException("createOrder not implemented");
    }

    /**
     * Creates an order with optional protective orders.
     */
    default OrderExecutionResult createOrder(
            TickerInfo.Key key,
            double price,
            int count,
            String operation,
            double takeProfit,
            double stopLose,
            boolean isFullPrice) {
        throw new UnsupportedOperationException("createOrder not implemented");
    }

    /**
     * Creates an order with the full set of parameters.
     */
    default OrderExecutionResult createOrder(
            TickerInfo.Key key,
            double price,
            int count,
            String operation,
            double takeProfit,
            double stopLose,
            boolean isFullPrice,
            double cashToUse) {
        throw new UnsupportedOperationException("createOrder not implemented");
    }

    /**
     * Buys by market price and returns execution details.
     */
    default OrderExecutionResult buyByMarketWithDetails(
            String name, TickerType type, double cashToBuy, double takeProfit, double stopLose) {
        throw new UnsupportedOperationException("buyByMarketWithDetails not implemented");
    }

    /**
     * Sells by market price and returns execution details.
     */
    default OrderExecutionResult sellByMarketWithDetails(
            String name, TickerType type, double cashToSell, double takeProfit, double stopLose) {
        throw new UnsupportedOperationException("sellByMarketWithDetails not implemented");
    }

    /**
     * Buys the given cash amount.
     */
    default OrderExecutionResult buy(
            String name,
            TickerType type,
            double cashToBuy,
            boolean byMarket,
            double takeProfit,
            double stopLose,
            boolean isFullPrice) {
        throw new UnsupportedOperationException("buy not implemented");
    }

    /**
     * Sells the given cash amount.
     */
    default OrderExecutionResult sell(
            String name,
            TickerType type,
            double cashToSell,
            boolean byMarket,
            double takeProfit,
            double stopLose,
            boolean isFullPrice) {
        throw new UnsupportedOperationException("sell not implemented");
    }

    /**
     * Closes the entire long position.
     */
    default boolean closeLongByMarket(String name, TickerType type) {
        throw new UnsupportedOperationException("closeLongByMarket not implemented");
    }

    /**
     * Closes the entire long position and returns execution details.
     */
    default OrderExecutionResult closeLongByMarketWithDetails(String name, TickerType type) {
        throw new UnsupportedOperationException("closeLongByMarketWithDetails not implemented");
    }

    /**
     * Partially closes a long position by specified quantity.
     */
    default OrderExecutionResult closeLongByMarketWithDetails(String name, TickerType type, int quantity) {
        throw new UnsupportedOperationException("closeLongByMarketWithDetails not implemented");
    }

    /**
     * Closes the entire short position.
     */
    default boolean closeShortByMarket(String name, TickerType type) {
        throw new UnsupportedOperationException("closeShortByMarket not implemented");
    }

    /**
     * Closes the entire short position and returns execution details.
     */
    default OrderExecutionResult closeShortByMarketWithDetails(String name, TickerType type) {
        throw new UnsupportedOperationException("closeShortByMarketWithDetails not implemented");
    }

    /**
     * Partially closes a short position by specified quantity.
     */
    default OrderExecutionResult closeShortByMarketWithDetails(String name, TickerType type, int quantity) {
        throw new UnsupportedOperationException("closeShortByMarketWithDetails not implemented");
    }

    /**
     * Closes all positions by market orders.
     */
    default void closeAllByMarket(TickerType type) {
        throw new UnsupportedOperationException("closeAllByMarket not implemented");
    }

    /**
     * Cancels a stop order.
     */
    default void cancelStopOrder(TickerInfo.Key key, String stopOrderId, String orderTypeName) {
        throw new UnsupportedOperationException("cancelStopOrder not implemented");
    }

    /**
     * Returns the initial margin (ГО) required for one futures contract.
     * Uses the maximum of marginOnBuy and marginOnSell for conservative risk management.
     *
     * @param figi instrument identifier
     * @return initial margin in RUB, or null if not available
     */
    default Double getSingleContractGo(String figi) {
        throw new UnsupportedOperationException("getSingleContractGo not implemented");
    }

    /**
     * Synchronizes protective orders.
     */
    default void syncProtectiveOrders(String name, TickerType type, Position position) {
        throw new UnsupportedOperationException("syncProtectiveOrders not implemented");
    }

    /**
     * Restores protective position from broker orders.
     */
    default Position restoreProtectivePosition(String name, TickerType type, Position position) {
        throw new UnsupportedOperationException("restoreProtectivePosition not implemented");
    }

    /**
     * Returns trade history from broker operations since the given timestamp.
     */
    default List<Map<String, Object>> getTradeHistory(Instant since) {
        throw new UnsupportedOperationException("getTradeHistory not implemented");
    }

    // ========================================================================
    // OrderExecutor compatibility methods (for backtest parity)
    // ========================================================================

    /**
     * Execute a market buy order with SL/TP percentages.
     */
    default OrderExecutionResult buy(String ticker, int quantity, Double stopLossPercent, Double takeProfitPercent) {
        throw new UnsupportedOperationException("buy not implemented");
    }

    /**
     * Execute a market sell order (for short positions) with SL/TP percentages.
     */
    default OrderExecutionResult sell(String ticker, int quantity, Double stopLossPercent, Double takeProfitPercent) {
        throw new UnsupportedOperationException("sell not implemented");
    }

    /**
     * Close a long position.
     */
    default OrderExecutionResult closeLong(String ticker) {
        throw new UnsupportedOperationException("closeLong not implemented");
    }

    /**
     * Close a short position.
     */
    default OrderExecutionResult closeShort(String ticker) {
        throw new UnsupportedOperationException("closeShort not implemented");
    }

    /**
     * Partially close a long position.
     */
    default OrderExecutionResult partialCloseLong(String ticker, int quantity) {
        throw new UnsupportedOperationException("partialCloseLong not implemented");
    }

    /**
     * Partially close a short position.
     */
    default OrderExecutionResult partialCloseShort(String ticker, int quantity) {
        throw new UnsupportedOperationException("partialCloseShort not implemented");
    }
}
