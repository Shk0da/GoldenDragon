package com.github.shk0da.goldendragon.backtest;

import com.github.shk0da.goldendragon.market.OrderExecutor;
import com.github.shk0da.goldendragon.model.Candle;
import com.github.shk0da.goldendragon.model.OrderExecutionResult;
import com.github.shk0da.goldendragon.model.Position;
import com.github.shk0da.goldendragon.model.PositionInfo;
import com.github.shk0da.goldendragon.model.TickerInfo;
import com.github.shk0da.goldendragon.model.TickerType;
import com.github.shk0da.goldendragon.repository.TickerRepository;
import com.github.shk0da.goldendragon.service.TradingService;

import java.time.Instant;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * TradingService wrapper around SimulatedBroker for backtest parity with live trading.
 *
 * <p>All TradingService methods delegate to {@link SimulatedBroker}, so the strategy sees
 * the same data in backtest mode as it would from a live broker (TCSService).
 */
public class BacktestTradingService implements TradingService {

    private final SimulatedBroker broker;

    public BacktestTradingService(SimulatedBroker broker) {
        this.broker = broker;
    }

    @Override
    public Double getAvailableCash() {
        return broker.getSharedCash();
    }

    @Override
    public double getInitialBalance() {
        return broker.getInitialBalance();
    }

    @Override
    public double getTotalPortfolioValue() {
        return broker.getTotalPortfolioValue();
    }

    @Override
    public double getGlobalPeakEquity() {
        return broker.getGlobalPeakEquity();
    }

    @Override
    public int calculateTradeCount(TickerInfo.Key key, double availableCash, double price) {
        if (price <= 0.0 || availableCash <= 0.0) {
            return 0;
        }
        TickerInfo info = searchTicker(key);
        int lot = info != null && info.getLot() != null ? Math.max(1, info.getLot()) : 1;
        return (int) (Math.floor(availableCash / (price * lot)) * lot);
    }

    @Override
    public double getLiveAskPrice(TickerInfo.Key key) {
        Candle bar = broker.getCurrentCandle(key.getTicker(), "5_MIN");
        if (bar == null) {
            bar = broker.getCurrentCandle(key.getTicker(), "HOUR");
        }
        return bar != null ? bar.close : 0.0;
    }

    @Override
    public double getLiveBidPrice(TickerInfo.Key key) {
        Candle bar = broker.getCurrentCandle(key.getTicker(), "5_MIN");
        if (bar == null) {
            bar = broker.getCurrentCandle(key.getTicker(), "HOUR");
        }
        return bar != null ? bar.close * 0.9998 : 0.0;
    }

    @Override
    public TickerInfo searchTicker(TickerInfo.Key key) {
        return TickerRepository.INSTANCE.getByName(key.getTicker());
    }

    @Override
    public PositionInfo getCurrentPositions(TickerType tickerType, String tickerName) {
        return broker.getCurrentPositions(tickerType, tickerName);
    }

    @Override
    public Map<TickerInfo.Key, PositionInfo> getCurrentPositions(TickerType tickerType) {
        // Simplified - return empty map for backtest (not used in current implementation)
        return new HashMap<>();
    }

    @Override
    public int getCountOfCurrentPositions(TickerType tickerType, String tickerName) {
        PositionInfo info = getCurrentPositions(tickerType, tickerName);
        return info != null ? info.getBalance() : 0;
    }

    // Convenience method - not in TradingService interface
    public List<Candle> getCandles(String ticker, String interval) {
        return broker.getCandles(ticker, interval);
    }

    @Override
    public OrderExecutionResult buyByMarketWithDetails(
            String name, TickerType type, double cashToBuy, double takeProfit, double stopLose) {
        Candle bar = broker.getCurrentCandle(name, "5_MIN");
        if (bar == null) {
            return OrderExecutionResult.failed("No market data for " + name);
        }
        TickerInfo info = TickerRepository.INSTANCE.getByName(name);
        int lotSize = info != null && info.getLot() != null ? Math.max(1, info.getLot()) : 1;
        int quantity = (int) (cashToBuy / (bar.close * lotSize));
        if (quantity <= 0) {
            return OrderExecutionResult.failed("Insufficient cash");
        }
        return convertResult(broker.buy(name, quantity, stopLose, takeProfit));
    }

    @Override
    public OrderExecutionResult sellByMarketWithDetails(
            String name, TickerType type, double cashToSell, double takeProfit, double stopLose) {
        Candle bar = broker.getCurrentCandle(name, "5_MIN");
        if (bar == null) {
            return OrderExecutionResult.failed("No market data for " + name);
        }
        TickerInfo info = TickerRepository.INSTANCE.getByName(name);
        int lotSize = info != null && info.getLot() != null ? Math.max(1, info.getLot()) : 1;
        int quantity = (int) (cashToSell / (bar.close * lotSize));
        if (quantity <= 0) {
            return OrderExecutionResult.failed("Insufficient cash");
        }
        return convertResult(broker.sell(name, quantity, stopLose, takeProfit));
    }

    @Override
    public OrderExecutionResult buy(
            String name, TickerType type, double cashToBuy,
            boolean byMarket, double takeProfit, double stopLose, boolean isFullPrice) {
        if (!byMarket) {
            return null;
        }
        return buyByMarketWithDetails(name, type, cashToBuy, takeProfit, stopLose);
    }

    @Override
    public OrderExecutionResult sell(
            String name, TickerType type, double cashToSell,
            boolean byMarket, double takeProfit, double stopLose, boolean isFullPrice) {
        if (!byMarket) {
            return null;
        }
        return sellByMarketWithDetails(name, type, cashToSell, takeProfit, stopLose);
    }

    @Override
    public boolean closeLongByMarket(String name, TickerType type) {
        return broker.closeLongByMarket(name, type);
    }

    @Override
    public OrderExecutionResult closeLongByMarketWithDetails(String name, TickerType type) {
        return convertResult(broker.closeLong(name));
    }

    @Override
    public OrderExecutionResult closeLongByMarketWithDetails(String name, TickerType type, int quantity) {
        return convertResult(broker.partialCloseLong(name, quantity));
    }

    @Override
    public boolean closeShortByMarket(String name, TickerType type) {
        return broker.closeLongByMarket(name, type);
    }

    @Override
    public OrderExecutionResult closeShortByMarketWithDetails(String name, TickerType type) {
        return convertResult(broker.closeShort(name));
    }

    @Override
    public OrderExecutionResult closeShortByMarketWithDetails(String name, TickerType type, int quantity) {
        return convertResult(broker.partialCloseShort(name, quantity));
    }

    @Override
    public void closeAllByMarket(TickerType type) {
        broker.closeAll(type.toString());
    }

    @Override
    public List<Map<String, Object>> getTradeHistory(Instant since) {
        // Simplified - return empty list for backtest (trade history accessed directly from broker)
        return Collections.emptyList();
    }

    // ---- OrderExecutor compatibility methods (backtest parity) ----

    @Override
    public OrderExecutionResult buy(String ticker, int quantity, Double stopLossPercent, Double takeProfitPercent) {
        return convertResult(broker.buy(ticker, quantity, stopLossPercent, takeProfitPercent));
    }

    @Override
    public OrderExecutionResult sell(String ticker, int quantity, Double stopLossPercent, Double takeProfitPercent) {
        return convertResult(broker.sell(ticker, quantity, stopLossPercent, takeProfitPercent));
    }

    @Override
    public OrderExecutionResult closeLong(String ticker) {
        return convertResult(broker.closeLong(ticker));
    }

    @Override
    public OrderExecutionResult closeShort(String ticker) {
        return convertResult(broker.closeShort(ticker));
    }

    @Override
    public OrderExecutionResult partialCloseLong(String ticker, int quantity) {
        return convertResult(broker.partialCloseLong(ticker, quantity));
    }

    @Override
    public OrderExecutionResult partialCloseShort(String ticker, int quantity) {
        return convertResult(broker.partialCloseShort(ticker, quantity));
    }

    @Override
    public Position restoreProtectivePosition(String name, TickerType type, Position position) {
        // Backtest has no real protective orders; return position as-is
        return position;
    }

    @Override
    public void syncProtectiveOrders(String name, TickerType type, Position position) {
        // Backtest has no real protective orders; no-op
    }

    @Override
    public List<Candle> getLastCandles(String ticker, TickerType type, int size) {
        // Backtest has no real candles; return empty list
        return java.util.Collections.emptyList();
    }

    /**
     * Convert SimulatedBroker ExecutionResult to TradingService OrderExecutionResult.
     */
    private OrderExecutionResult convertResult(OrderExecutor.ExecutionResult result) {
        if (result == null || !result.isSuccess()) {
            return OrderExecutionResult.failed(result != null ? result.getErrorMessage() : "Unknown error");
        }
        return OrderExecutionResult.testSuccess(result.getExecutedPrice(), result.getExecutedQuantity());
    }
}
