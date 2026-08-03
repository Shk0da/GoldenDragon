package com.github.shk0da.goldendragon.strategy;

import com.github.shk0da.goldendragon.config.MainConfig;
import com.github.shk0da.goldendragon.config.OrderBookScalpConfig;
import com.github.shk0da.goldendragon.model.MarketDepthSnapshot;
import com.github.shk0da.goldendragon.model.MarketTickListener;
import com.github.shk0da.goldendragon.model.MarketTradeTick;
import com.github.shk0da.goldendragon.service.TCSService;
import com.github.shk0da.goldendragon.strategy.orderbook.ObiScalpSignal;
import com.github.shk0da.goldendragon.strategy.orderbook.OrderBookTradingEngine;
import java.util.List;

/**
 * Single-signal order-book scalper (OBI). Kept for backward compatibility.
 *
 * <p>For multiple signals in one process use {@link OrderBookOrchestratorStrategy}.
 */
public class OrderBookScalpStrategy implements MarketTickListener {

    private final OrderBookTradingEngine engine;

    public OrderBookScalpStrategy(
            TCSService tcsService, MainConfig mainConfig, OrderBookScalpConfig config) {
        this.engine =
                new OrderBookTradingEngine(
                        tcsService,
                        mainConfig,
                        config,
                        List.of(new ObiScalpSignal(config)),
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
