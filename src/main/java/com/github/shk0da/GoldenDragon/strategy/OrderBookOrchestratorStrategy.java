package com.github.shk0da.goldendragon.strategy;

import com.github.shk0da.goldendragon.config.MainConfig;
import com.github.shk0da.goldendragon.config.OrderBookScalpConfig;
import com.github.shk0da.goldendragon.model.MarketDepthSnapshot;
import com.github.shk0da.goldendragon.model.MarketTickListener;
import com.github.shk0da.goldendragon.model.MarketTradeTick;
import com.github.shk0da.goldendragon.service.TCSService;
import com.github.shk0da.goldendragon.strategy.orderbook.OrderBookSignalFactory;
import com.github.shk0da.goldendragon.strategy.orderbook.OrderBookTradingEngine;

/**
 * Multi-signal order-book orchestrator.
 *
 * <p>Runs one {@link com.github.shk0da.goldendragon.strategy.orderbook.OrderBookTradingEngine} with
 * several {@link com.github.shk0da.goldendragon.strategy.orderbook.OrderBookSignal}
 * implementations. Signals are evaluated in config order; the first entry wins. One position per
 * ticker.
 */
public class OrderBookOrchestratorStrategy implements MarketTickListener {

    private final OrderBookTradingEngine engine;

    public OrderBookOrchestratorStrategy(
            TCSService tcsService, MainConfig mainConfig, OrderBookScalpConfig config) {
        this.engine =
                new OrderBookTradingEngine(
                        tcsService,
                        mainConfig,
                        config,
                        OrderBookSignalFactory.createEnabledSignals(config),
                        "OrderBookOrchestratorStrategy");
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
