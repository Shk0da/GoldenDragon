package com.github.shk0da.goldendragon.strategy;

import com.github.shk0da.goldendragon.config.OrderBookScalpConfig;
import com.github.shk0da.goldendragon.model.Config;
import com.github.shk0da.goldendragon.config.UnifiedTraderConfig;
import com.github.shk0da.goldendragon.model.Candle;
import com.github.shk0da.goldendragon.model.Position;
import com.github.shk0da.goldendragon.model.TradingDecision;
import com.github.shk0da.goldendragon.service.TradingService;
import com.github.shk0da.goldendragon.strategy.candle.CandleEntryDecision;
import com.github.shk0da.goldendragon.strategy.candle.CandleMarketContext;
import com.github.shk0da.goldendragon.strategy.candle.CandlePositionView;
import com.github.shk0da.goldendragon.strategy.candle.CandleScalpSignal;
import com.github.shk0da.goldendragon.strategy.candle.CandleSignalFactory;

import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Backtest-compatible wrapper for OrderBookScalpStrategy.
 * <p>
 * Adapts candle-based signals to the BaseStrategy interface used by BacktestRunner.
 * Uses 5-minute candles and CandleScalpSignal implementations for entry/exit decisions.
 */
public class OrderBookScalpBacktestStrategy extends BaseStrategy {

    private final OrderBookScalpConfig orderBookConfig;
    private final List<CandleScalpSignal> signals;
    private final Map<String, CandleMarketContext> contextByTicker = new ConcurrentHashMap<>();

    public OrderBookScalpBacktestStrategy(
            UnifiedTraderConfig unifiedTraderConfig,
            TradingService tradingService,
            OrderBookScalpConfig config) {
        super(unifiedTraderConfig, tradingService, new Config());
        this.orderBookConfig = config;
        this.signals = CandleSignalFactory.createEnabledSignals(tradingService, config);
    }

    @Override
    protected String getStrategyName() {
        return "OrderBookScalpStrategy";
    }

    @Override
    public TradingDecision decide(
            String ticker,
            List<Candle> hourCandles,
            List<Candle> minuteCandles,
            Position position,
            double balance,
            boolean incrementCandlesHeld) {

        // Build market context from 5-minute candles
        CandleMarketContext context = new CandleMarketContext(minuteCandles);
        contextByTicker.put(ticker, context);
        
        // Debug logging for backtest
        if (minuteCandles != null && !minuteCandles.isEmpty()) {
            Candle latest = minuteCandles.get(minuteCandles.size() - 1);
            System.out.println("[OrderBookScalp] " + ticker + " time=" + latest.time + 
                " O=" + latest.open + " H=" + latest.high + " L=" + latest.low + " C=" + latest.close +
                " V=" + latest.volume + " candles=" + minuteCandles.size());
        } else {
            System.out.println("[OrderBookScalp] " + ticker + " NO CANDLES!");
        }

        // Check exit conditions for existing position
        if (position != null && position.quantity > 0) {
            CandlePositionView positionView = new CandlePositionView(
                position.direction,
                position.entryPrice != null ? position.entryPrice : 0.0,
                position.quantity,
                null  // signalId not needed for exit evaluation
            );
            
            for (CandleScalpSignal signal : signals) {
                String exitReason = signal.evaluateExit(context, positionView, ticker);
                if (exitReason != null) {
                    return new TradingDecision("CLOSE", exitReason);
                }
            }
            
            // No exit signal - hold position
            return new TradingDecision("HOLD", "NO_EXIT_SIGNAL");
        }

        // Evaluate entry signals (LONG and SHORT)
        for (CandleScalpSignal signal : signals) {
            // Check LONG entry
            CandleEntryDecision longDecision = signal.evaluateEntry(context, ticker);
            if (longDecision.shouldEnter() && longDecision.quality() >= orderBookConfig.getEntryQualityThreshold()) {
                return new TradingDecision(
                    "OPEN",
                    longDecision.reason(),
                    longDecision.quality(),
                    0,  // quantity will be calculated by executor
                    0.0,  // stopLoss
                    0.0,  // takeProfit
                    context.getLatestClose(),
                    null
                );
            }

            // Check SHORT entry
            CandleEntryDecision shortDecision = signal.evaluateEntryShort(context, ticker);
            if (shortDecision.shouldEnter() && shortDecision.quality() >= orderBookConfig.getEntryQualityThreshold()) {
                return new TradingDecision(
                    "OPEN",
                    shortDecision.reason(),
                    shortDecision.quality(),
                    0,
                    0.0,
                    0.0,
                    context.getLatestClose(),
                    null
                );
            }
        }

        // No entry signal - hold
        return new TradingDecision("HOLD", "NO_SIGNAL");
    }

    @Override
    protected void onDailyReset() {
        // Reset per-ticker state if needed
        for (CandleScalpSignal signal : signals) {
            for (String ticker : contextByTicker.keySet()) {
                signal.reset(ticker);
            }
        }
    }

    @Override
    protected void onTradeClosed(
            String ticker,
            double pnl,
            double entryPrice,
            double exitPrice,
            int quantity,
            String direction) {
        // Log trade if needed
        log(String.format("OrderBookScalp: Closed %s %s qty=%d entry=%.2f exit=%.2f pnl=%.2f",
            ticker, direction, quantity, entryPrice, exitPrice, pnl));
    }

    @Override
    protected void closeAllPositions(TradingService tradingService, UnifiedTraderConfig config) {
        // Close all positions on strategy shutdown
    }
}
