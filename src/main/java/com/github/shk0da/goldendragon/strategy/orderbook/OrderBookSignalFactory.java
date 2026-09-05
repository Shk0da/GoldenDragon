package com.github.shk0da.goldendragon.strategy.orderbook;

import com.github.shk0da.goldendragon.config.OrderBookScalpConfig;
import com.github.shk0da.goldendragon.service.TradingService;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/** Builds enabled {@link OrderBookSignal} instances from configuration. */
public final class OrderBookSignalFactory {

    private OrderBookSignalFactory() {}

    public static List<OrderBookSignal> createEnabledSignals(
            TradingService tradingService, OrderBookScalpConfig config) {
        // CumulativeDeltaScalpSignal is the primary signal for HFT scalping
        // It implements both bounce and breakout scenarios from the spec
        List<OrderBookSignal> allSignals = new ArrayList<>();
        allSignals.add(new CumulativeDeltaScalpSignal(tradingService, config));
        allSignals.add(new TradeFlowScalpSignal(config));
        allSignals.add(new MicropriceDriftSignal(config));
        allSignals.add(new DensityImbalanceSignal(config));
        
        // Add DensityScalpSignal with all required components
        allSignals.add(createDensityScalpSignal(tradingService, config));
        
        Map<String, OrderBookSignal> available = new LinkedHashMap<>();
        for (OrderBookSignal signal : allSignals) {
            available.put(signal.id().toLowerCase(Locale.ROOT), signal);
        }

        List<OrderBookSignal> enabled = new ArrayList<>();
        for (String signalId : config.getEnabledSignals()) {
            String normalized = signalId.trim().toLowerCase(Locale.ROOT);
            OrderBookSignal signal = available.get(normalized);
            if (signal != null) {
                enabled.add(signal);
            }
        }
        if (enabled.isEmpty()) {
            // Default to CumulativeDeltaScalpSignal if no signals configured
            enabled.add(new CumulativeDeltaScalpSignal(tradingService, config));
        }
        return enabled;
    }
    
    /**
     * Create DensityScalpSignal with all required analysis components.
     */
    private static DensityScalpSignal createDensityScalpSignal(
            TradingService tradingService, OrderBookScalpConfig config) {
        
        // Create asset pair analyzer
        AssetPairAnalyzer pairAnalyzer = new AssetPairAnalyzer(
                config.getLeaderLagSeconds(),
                config.getBasisAnomalySigma(),
                config.isDivergenceBlockEnabled());
        
        // Create trend analyzer
        TrendAnalyzer trendAnalyzer = new TrendAnalyzer(
                tradingService,
                config.getTrendTimeframeMinutes(),
                config.getTrendLookbackCandles(),
                config.getTrendCacheTtlMs());
        
        // Create level history
        LevelHistory levelHistory = new LevelHistory(
                config.getMinLevelVolumeRatio(),
                config.getMaxLevelAgeMinutes(),
                config.getLevelPriceToleranceBps());
        
        // Create compression detector
        CompressionDetector compressionDetector = new CompressionDetector(
                config.getCompressionSpreadBps(),
                config.getCompressionVolumeMultiplier(),
                config.getCompressionProximityBps(),
                config.getCompressionHistorySize());
        
        // Create micro impulse detector
        MicroImpulseDetector impulseDetector = new MicroImpulseDetector(
                config.getMicroImpulseMinTrades(),
                config.getMicroImpulseWindowMs(),
                config.getMicroImpulseVolumeMultiplier());
        
        return new DensityScalpSignal(
                tradingService,
                config,
                pairAnalyzer,
                trendAnalyzer,
                levelHistory,
                compressionDetector,
                impulseDetector);
    }
}
