package com.github.shk0da.goldendragon.market;

import com.github.shk0da.goldendragon.model.TickerInfo;
import com.github.shk0da.goldendragon.repository.TickerRepository;

/**
 * Backtest order executor that simulates order execution.
 * Uses historical data to simulate fills with realistic slippage.
 */
public class BacktestOrderExecutor implements OrderExecutor {

    private final MarketDataProvider marketDataProvider;
    private final TickerRepository tickerRepository;
    private double balance;
    private int positions;

    public BacktestOrderExecutor(MarketDataProvider marketDataProvider, double initialBalance) {
        this.marketDataProvider = marketDataProvider;
        this.balance = initialBalance;
        this.positions = 0;
        this.tickerRepository = TickerRepository.INSTANCE;
    }

    @Override
    public ExecutionResult buy(String ticker, int quantity, Double stopLossPercent, Double takeProfitPercent) {
        try {
            TickerInfo info = tickerRepository.getByName(ticker);
            if (info == null) {
                return ExecutionResult.failed("Ticker not found: " + ticker);
            }

            MarketPrices prices = marketDataProvider.getLivePrices(ticker);
            if (prices.getAsk() == null) {
                return ExecutionResult.failed("No ask price for " + ticker);
            }

            double askPrice = prices.getAsk();
            double value = quantity * askPrice * info.getLot();

            if (value > balance) {
                return ExecutionResult.failed("Insufficient balance: needed " + value + ", available " + balance);
            }

            // Simulate slippage (0.01% to 0.05%)
            double slippage = askPrice * (0.0001 + Math.random() * 0.0004);
            double fillPrice = askPrice + slippage;

            balance -= value;
            positions++;

            return ExecutionResult.success(quantity, fillPrice);
        } catch (Exception e) {
            return ExecutionResult.failed("Exception: " + e.getMessage());
        }
    }

    @Override
    public ExecutionResult sell(String ticker, int quantity, Double stopLossPercent, Double takeProfitPercent) {
        try {
            TickerInfo info = tickerRepository.getByName(ticker);
            if (info == null) {
                return ExecutionResult.failed("Ticker not found: " + ticker);
            }

            MarketPrices prices = marketDataProvider.getLivePrices(ticker);
            if (prices.getBid() == null) {
                return ExecutionResult.failed("No bid price for " + ticker);
            }

            double bidPrice = prices.getBid();
            double value = quantity * bidPrice * info.getLot();

            balance += value;
            positions = Math.max(0, positions - 1);

            // Simulate slippage
            double slippage = bidPrice * (0.0001 + Math.random() * 0.0004);
            double fillPrice = bidPrice - slippage;

            return ExecutionResult.success(quantity, fillPrice);
        } catch (Exception e) {
            return ExecutionResult.failed("Exception: " + e.getMessage());
        }
    }

    @Override
    public ExecutionResult closeLong(String ticker) {
        return sell(ticker, 1, null, null); // Simplified for backtest
    }

    @Override
    public ExecutionResult closeShort(String ticker) {
        return sell(ticker, 1, null, null); // Simplified for backtest
    }

    @Override
    public double getAvailableCash() {
        return balance;
    }

    @Override
    public double getPortfolioValue() {
        return balance; // Simplified for backtest
    }

    @Override
    public boolean isLive() {
        return false;
    }

    /**
     * Get current balance.
     *
     * @return current balance
     */
    public double getBalance() {
        return balance;
    }
}
