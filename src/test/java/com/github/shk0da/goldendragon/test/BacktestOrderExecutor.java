package com.github.shk0da.goldendragon.market;

import com.github.shk0da.goldendragon.model.TickerInfo;
import com.github.shk0da.goldendragon.model.TickerType;
import com.github.shk0da.goldendragon.repository.TickerRepository;
import com.github.shk0da.goldendragon.test.SimulatedBroker;

/**
 * Backtest order executor that delegates all operations to SimulatedBroker.
 * SimulatedBroker is the single source of truth for cash and positions.
 */
public class BacktestOrderExecutor implements OrderExecutor {

    private final SimulatedBroker broker;
    private final TickerRepository tickerRepository;

    public BacktestOrderExecutor(SimulatedBroker broker) {
        this.broker = broker;
        this.tickerRepository = TickerRepository.INSTANCE;
    }

    @Override
    public ExecutionResult buy(String ticker, int quantity, Double stopLossPercent, Double takeProfitPercent) {
        try {
            TickerInfo info = tickerRepository.getByName(ticker);
            if (info == null) {
                return ExecutionResult.failed("Ticker not found: " + ticker);
            }

            Double askPrice = broker.getCurrentPrice(ticker, info.getType(), true);
            if (askPrice == null || askPrice <= 0) {
                return ExecutionResult.failed("No ask price for " + ticker);
            }

            if (!broker.buyByQuantity(ticker, info.getType(), quantity)) {
                return ExecutionResult.failed("Insufficient cash or invalid quantity for " + ticker);
            }

            return ExecutionResult.success(quantity, askPrice);
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

            Double bidPrice = broker.getCurrentPrice(ticker, info.getType(), false);
            if (bidPrice == null || bidPrice <= 0) {
                return ExecutionResult.failed("No bid price for " + ticker);
            }

            // Calculate cash value for selling qty units
            double positionValue = quantity * bidPrice * (info.getLot() != null ? info.getLot() : 1);
            boolean success = broker.sellByMarket(ticker, info.getType(), positionValue);

            if (!success) {
                return ExecutionResult.failed("Failed to sell " + ticker);
            }

            return ExecutionResult.success(quantity, bidPrice);
        } catch (Exception e) {
            return ExecutionResult.failed("Exception: " + e.getMessage());
        }
    }

    @Override
    public ExecutionResult closeLong(String ticker) {
        try {
            TickerInfo info = tickerRepository.getByName(ticker);
            if (info == null) {
                return ExecutionResult.failed("Ticker not found: " + ticker);
            }

            boolean success = broker.closeLongByMarket(ticker, info.getType());
            if (!success) {
                return ExecutionResult.failed("Failed to close long for " + ticker);
            }

            Double bidPrice = broker.getCurrentPrice(ticker, info.getType(), false);
            return success
                ? ExecutionResult.success(0, bidPrice)
                : ExecutionResult.failed("Failed to close long for " + ticker);
        } catch (Exception e) {
            return ExecutionResult.failed("Exception: " + e.getMessage());
        }
    }

    @Override
    public ExecutionResult closeShort(String ticker) {
        return closeLong(ticker);
    }

    @Override
    public double getAvailableCash() {
        return broker.getAvailableCash();
    }
}
