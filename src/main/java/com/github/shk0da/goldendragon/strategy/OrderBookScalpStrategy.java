package com.github.shk0da.goldendragon.strategy;

import com.github.shk0da.goldendragon.config.MainConfig;
import com.github.shk0da.goldendragon.config.OrderBookScalpConfig;
import com.github.shk0da.goldendragon.model.MarketDepthSnapshot;
import com.github.shk0da.goldendragon.model.MarketTickListener;
import com.github.shk0da.goldendragon.model.MarketTradeTick;
import com.github.shk0da.goldendragon.service.TCSService;
import com.github.shk0da.goldendragon.strategy.orderbook.CumulativeDeltaScalpSignal;
import com.github.shk0da.goldendragon.strategy.orderbook.OrderBookSignal;
import com.github.shk0da.goldendragon.strategy.orderbook.OrderBookSignalFactory;
import com.github.shk0da.goldendragon.strategy.orderbook.OrderBookTradingEngine;
import java.util.List;

/**
 * High-frequency scalping strategy using cumulative delta and order book densities.
 *
 * <p>Implements two trading scenarios:
 * <ul>
 *   <li><b>Scenario A (Bounce):</b> Counter-trend bounce from large density with delta confirmation</li>
 *   <li><b>Scenario B (Breakout):</b> Impulse breakout when density is consumed</li>
 * </ul>
 *
 * <p>Key features:
 * <ul>
 *   <li>10-second cumulative delta calculation</li>
 *   <li>Dynamic density detection (3x/5x average volume)</li>
 *   <li>Spread protection (max 0.02%)</li>
 *   <li>Emergency exit on density disappearance</li>
 * </ul>
 *
 * <p>For multiple signals in one process use {@link OrderBookOrchestratorStrategy}.
 */
public class OrderBookScalpStrategy implements MarketTickListener {

    private final OrderBookTradingEngine engine;

    public OrderBookScalpStrategy(
            TCSService tcsService, MainConfig mainConfig, OrderBookScalpConfig config) {
        // Use factory to create signals based on configuration
        List<OrderBookSignal> signals = OrderBookSignalFactory.createEnabledSignals(
            tcsService, config);
        
        this.engine =
                new OrderBookTradingEngine(
                        tcsService,
                        mainConfig,
                        config,
                        signals,
                        "OrderBookScalpStrategy");
    }

    public void run() {
        engine.run();
    }

    @Override
    public void onOrderBook(MarketDepthSnapshot snapshot) {
        engine.onOrderBook(snapshot);
    }

    @Override
    public void onTrade(MarketTradeTick trade) {
        engine.onTrade(trade);
    }

    @Override
    public void onError(Throwable throwable) {
        engine.onError(throwable);
    }
}
