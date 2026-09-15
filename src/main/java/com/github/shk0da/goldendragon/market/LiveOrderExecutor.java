package com.github.shk0da.goldendragon.market;

import com.github.shk0da.goldendragon.model.TickerInfo;
import com.github.shk0da.goldendragon.repository.TickerRepository;
import com.github.shk0da.goldendragon.service.TradingService;

/**
 * Live order executor using TCS API.
 * Sends real orders to the Tinkoff Invest API.
 */
public class LiveOrderExecutor implements OrderExecutor {

    /**
     * Safety margin matching TradingService.calculateTradeCount() (1%) to prevent
     * insufficient funds (error 30049) and silent partial fills on market orders.
     */
    private static final double ORDER_QUANTITY_SAFETY_MARGIN = 1.01;

    private final TradingService tcsService;
    private final TickerRepository tickerRepository;

    public LiveOrderExecutor(TradingService tcsService) {
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

            double value = quantity * askPrice * info.getLot() * ORDER_QUANTITY_SAFETY_MARGIN;
            if (value > cash && askPrice > 0 && info.getLot() > 0) {
                int maxQuantity = (int) (cash / (askPrice * info.getLot() * ORDER_QUANTITY_SAFETY_MARGIN));
                if (maxQuantity <= 0) {
                    return ExecutionResult.failed(
                            "Insufficient cash: needed "
                                    + String.format("%.2f", value)
                                    + ", available " + String.format("%.2f", cash));
                }
                quantity = maxQuantity;
                value = quantity * askPrice * info.getLot() * ORDER_QUANTITY_SAFETY_MARGIN;
            }

            TradingService.OrderExecutionResult result = tcsService.buyByMarketWithDetails(
                    ticker, info.getType(), value, takeProfitPercent, stopLossPercent);

            if (!result.isSuccess()) {
                return ExecutionResult.failed("Buy failed"
                    + (result.getErrorMessage() != null ? ": " + result.getErrorMessage() : ""));
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

            // sandbox may require full position value, not just margin — reduce quantity
            if (positionValue > cash && askPrice > 0 && info.getLot() > 0) {
                int maxQuantity = (int) (cash / (askPrice * info.getLot()));
                if (maxQuantity <= 0) {
                    return ExecutionResult.failed(
                            "Insufficient cash for short: needed "
                                    + String.format("%.2f", positionValue)
                                    + ", available " + String.format("%.2f", cash));
                }
                quantity = maxQuantity;
                positionValue = quantity * askPrice * info.getLot();
            }

            TradingService.OrderExecutionResult result = tcsService.sellByMarketWithDetails(
                    ticker, info.getType(), positionValue, takeProfitPercent, stopLossPercent);

            if (!result.isSuccess()) {
                return ExecutionResult.failed("Sell failed"
                    + (result.getErrorMessage() != null ? ": " + result.getErrorMessage() : ""));
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

            TradingService.OrderExecutionResult result = tcsService.closeLongByMarketWithDetails(ticker, info.getType());

            if (!result.isSuccess()) {
                return ExecutionResult.failed("Close long failed"
                    + (result.getErrorMessage() != null ? ": " + result.getErrorMessage() : ""));
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

            TradingService.OrderExecutionResult result = tcsService.closeShortByMarketWithDetails(ticker, info.getType());

            if (!result.isSuccess()) {
                return ExecutionResult.failed("Close short failed"
                    + (result.getErrorMessage() != null ? ": " + result.getErrorMessage() : ""));
            }

            return ExecutionResult.success(result.getExecutedCount(), result.getExecutedPrice());
        } catch (Exception e) {
            return ExecutionResult.failed("Exception: " + e.getMessage());
        }
    }

    @Override
    public ExecutionResult partialCloseLong(String ticker, int quantity) {
        try {
            TickerInfo info = tickerRepository.getByName(ticker);
            if (info == null) {
                return ExecutionResult.failed("Ticker not found: " + ticker);
            }

            TradingService.OrderExecutionResult result = tcsService.closeLongByMarketWithDetails(ticker, info.getType(), quantity);

            if (!result.isSuccess()) {
                return ExecutionResult.failed("Partial close long failed"
                    + (result.getErrorMessage() != null ? ": " + result.getErrorMessage() : ""));
            }

            return ExecutionResult.success(result.getExecutedCount(), result.getExecutedPrice());
        } catch (Exception e) {
            return ExecutionResult.failed("Exception: " + e.getMessage());
        }
    }

    @Override
    public ExecutionResult partialCloseShort(String ticker, int quantity) {
        try {
            TickerInfo info = tickerRepository.getByName(ticker);
            if (info == null) {
                return ExecutionResult.failed("Ticker not found: " + ticker);
            }

            TradingService.OrderExecutionResult result = tcsService.closeShortByMarketWithDetails(ticker, info.getType(), quantity);

            if (!result.isSuccess()) {
                return ExecutionResult.failed("Partial close short failed"
                    + (result.getErrorMessage() != null ? ": " + result.getErrorMessage() : ""));
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

    @Override
    public double getInitialBalance() {
        // Live trading: no fixed initial balance, use current balance via getAvailableCash()
        return 0.0;
    }
}
