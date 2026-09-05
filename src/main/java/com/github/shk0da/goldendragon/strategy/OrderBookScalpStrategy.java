package com.github.shk0da.goldendragon.strategy;

import com.github.shk0da.goldendragon.config.MainConfig;
import com.github.shk0da.goldendragon.config.OrderBookScalpConfig;
import com.github.shk0da.goldendragon.model.MarketDepthSnapshot;
import com.github.shk0da.goldendragon.model.MarketTickListener;
import com.github.shk0da.goldendragon.model.MarketTradeTick;
import com.github.shk0da.goldendragon.service.TradingService;
import com.github.shk0da.goldendragon.strategy.candle.CandleDeltaSignal;
import com.github.shk0da.goldendragon.strategy.candle.CandleSignalFactory;
import com.github.shk0da.goldendragon.strategy.candle.CandleTradingEngine;
import com.github.shk0da.goldendragon.strategy.candle.CandleScalpSignal;
import java.util.List;

/**
 * Scalping strategy using candle-based signals (5-minute timeframe).
 *
 * <p>Polls 5-minute candles from TradingService every 60 seconds and evaluates signals
 * for entry/exit decisions. Designed for both Tinkoff and ByBit trading services.
 *
 * <p>Configuration via OrderBookScalpConfig:
 * <ul>
 *   <li>positionCashPercent - percentage of available cash to use for positions (default 90)</li>
 *   <li>entryQualityThreshold - minimum quality for signal to trigger trade</li>
 *   <li>other trading parameters</li>
 * </ul>
 */
public class OrderBookScalpStrategy implements MarketTickListener {

    private final CandleTradingEngine engine;

    public OrderBookScalpStrategy(
            TradingService tradingService, MainConfig mainConfig, OrderBookScalpConfig config) {
        // Create candle-based signals using factory
        List<CandleScalpSignal> signals = CandleSignalFactory.createEnabledSignals(
            tradingService, config);
        
        // Load instruments from config
        List<String> instruments = config.getInstruments();
        
        this.engine =
                new CandleTradingEngine(
                        tradingService,
                        mainConfig,
                        config,
                        signals,
                        instruments,
                        "OrderBookScalpStrategy");
    }

    public void run() {
        engine.run();
    }

    @Override
    public void onOrderBook(MarketDepthSnapshot snapshot) {
        // Not used for candle-based strategy
    }

    @Override
    public void onTrade(MarketTradeTick trade) {
        // Not used for candle-based strategy
    }

    @Override
    public void onError(Throwable throwable) {
        // Not used for candle-based strategy
    }
}

