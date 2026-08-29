package com.github.shk0da.goldendragon.market;

import com.github.shk0da.goldendragon.model.TickerInfo;
import com.github.shk0da.goldendragon.repository.TickerRepository;
import com.github.shk0da.goldendragon.service.TCSService;

/**
 * Live order executor using TCS API.
 * Sends real orders to the Tinkoff Invest API.
 */
public class LiveOrderExecutor implements OrderExecutor {

    /**
     * Safety margin matching TCSService.calculateTradeCount() (1%) to prevent
     * insufficient funds (error 30049) and silent partial fills on market orders.
     */
    private static final double ORDER_QUANTITY_SAFETY_MARGIN = 1.01;

    private final TCSService tcsService;
    private final TickerRepository tickerRepository;

    public LiveOrderExecutor(TCSService tcsService) {
        this.tcsService = tcsService;
        this.tickerRepository = TickerRepository.INSTANCE;
    }

    @Override
    public ExecutionResult buy(String ticker, int quantity, Double stopLossPercent, Double takeProfitPercent) {
        try {
            TickerInfo info = tickerRepository.getByName(ticker);
            if (info == null) {
                return ExecutionResult.failed("Ticker not found: " + ticker);
            }

            TickerInfo.Key key = new TickerInfo.Key(ticker, info.getType());
            double cash = tcsService.getAvailableCash();
            double askPrice = tcsService.getLiveAskPrice(key);

            // Apply the same 1% safety margin as TCSService.calculateTradeCount() so the
            // executed quantity matches the requested quantity (avoids silently buying fewer).
            double value = quantity * askPrice * info.getLot() * ORDER_QUANTITY_SAFETY_MARGIN;
            if (value > cash) {
                return ExecutionResult.failed("Insufficient cash: needed " + value + ", available " + cash);
            }

            TCSService.OrderExecutionResult result = tcsService.buyByMarketWithDetails(
                    ticker, info.getType(), value, takeProfitPercent, stopLossPercent);

            if (!result.isSuccess()) {
                return ExecutionResult.failed("Order failed");
            }

            return ExecutionResult.success(result.getExecutedCount(), result.getExecutedPrice());
        } catch (Exception e) {
            return ExecutionResult.failed("Exception: " + e.getMessage());
        }
    }

    /** Margin requirement for short positions on MOEX (30% of position value). */
    private static final double SHORT_MARGIN_RATIO = 0.30;

    @Override
    public ExecutionResult sell(String ticker, int quantity, Double stopLossPercent, Double takeProfitPercent) {
        try {
            TickerInfo info = tickerRepository.getByName(ticker);
            if (info == null) {
                return ExecutionResult.failed("Ticker not found: " + ticker);
            }

            TickerInfo.Key key = new TickerInfo.Key(ticker, info.getType());
            double cash = tcsService.getAvailableCash();
            double askPrice = tcsService.getLiveAskPrice(key);

            double positionValue = quantity * askPrice * info.getLot();
            // short positions require margin, not full position value
            double requiredMargin = positionValue * SHORT_MARGIN_RATIO;
            if (requiredMargin > cash) {
                return ExecutionResult.failed(
                        "Insufficient margin for short: needed "
                                + String.format("%.2f", requiredMargin)
                                + ", available " + String.format("%.2f", cash));
            }

            TCSService.OrderExecutionResult result = tcsService.sellByMarketWithDetails(
                    ticker, info.getType(), positionValue, takeProfitPercent, stopLossPercent);

            if (!result.isSuccess()) {
                return ExecutionResult.failed("Order failed");
            }

            return ExecutionResult.success(result.getExecutedCount(), result.getExecutedPrice());
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

            TCSService.OrderExecutionResult result = tcsService.closeLongByMarketWithDetails(ticker, info.getType());

            if (!result.isSuccess()) {
                return ExecutionResult.failed("Close failed");
            }

            return ExecutionResult.success(result.getExecutedCount(), result.getExecutedPrice());
        } catch (Exception e) {
            return ExecutionResult.failed("Exception: " + e.getMessage());
        }
    }

    @Override
    public ExecutionResult closeShort(String ticker) {
        try {
            TickerInfo info = tickerRepository.getByName(ticker);
            if (info == null) {
                return ExecutionResult.failed("Ticker not found: " + ticker);
            }

            TCSService.OrderExecutionResult result = tcsService.closeShortByMarketWithDetails(ticker, info.getType());

            if (!result.isSuccess()) {
                return ExecutionResult.failed("Close failed");
            }

            return ExecutionResult.success(result.getExecutedCount(), result.getExecutedPrice());
        } catch (Exception e) {
            return ExecutionResult.failed("Exception: " + e.getMessage());
        }
    }

    @Override
    public double getAvailableCash() {
        try {
            return tcsService.getAvailableCash();
        } catch (Exception e) {
            return 0.0;
        }
    }
}
