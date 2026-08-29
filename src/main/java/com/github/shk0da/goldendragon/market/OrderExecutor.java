package com.github.shk0da.goldendragon.market;

/**
 * Interface for order execution.
 * Implementations can simulate execution for backtesting or send real orders for live trading.
 */
public interface OrderExecutor {

    /**
     * Execute a market buy order.
     *
     * @param ticker ticker symbol
     * @param quantity quantity to buy
     * @param stopLossPercent stop loss percentage (optional)
     * @param takeProfitPercent take profit percentage (optional)
     * @return execution result
     */
    ExecutionResult buy(String ticker, int quantity, Double stopLossPercent, Double takeProfitPercent);

    /**
     * Execute a market sell order (for short positions).
     *
     * @param ticker ticker symbol
     * @param quantity quantity to sell
     * @param stopLossPercent stop loss percentage (optional)
     * @param takeProfitPercent take profit percentage (optional)
     * @return execution result
     */
    ExecutionResult sell(String ticker, int quantity, Double stopLossPercent, Double takeProfitPercent);

    /**
     * Close a long position.
     *
     * @param ticker ticker symbol
     * @return execution result
     */
    ExecutionResult closeLong(String ticker);

    /**
     * Close a short position.
     *
     * @param ticker ticker symbol
     * @return execution result
     */
    ExecutionResult closeShort(String ticker);

    /**
     * Get available cash balance.
     *
     * @return available cash
     */
    double getAvailableCash();

    /**
     * Container for order execution results.
     */
    public static class ExecutionResult {
        private final boolean success;
        private final int executedQuantity;
        private final Double executedPrice;
        private final String errorMessage;

        public ExecutionResult(boolean success, int executedQuantity, Double executedPrice, String errorMessage) {
            this.success = success;
            this.executedQuantity = executedQuantity;
            this.executedPrice = executedPrice;
            this.errorMessage = errorMessage;
        }

        public static ExecutionResult success(int quantity, double price) {
            return new ExecutionResult(true, quantity, price, null);
        }

        public static ExecutionResult failed(String error) {
            return new ExecutionResult(false, 0, null, error);
        }

        public boolean isSuccess() {
            return success;
        }

        public int getExecutedQuantity() {
            return executedQuantity;
        }

        public Double getExecutedPrice() {
            return executedPrice;
        }

        public String getErrorMessage() {
            return errorMessage;
        }
    }
}
