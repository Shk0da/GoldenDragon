package com.github.shk0da.goldendragon.strategy.orderbook;

import com.github.shk0da.goldendragon.config.OrderBookScalpConfig;
import com.github.shk0da.goldendragon.model.MarketDepthSnapshot;
import com.github.shk0da.goldendragon.model.MarketTradeTick;
import com.github.shk0da.goldendragon.model.TickerInfo;
import com.github.shk0da.goldendragon.service.TradingService;
import com.github.shk0da.goldendragon.utils.LoggingUtils;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.BiConsumer;

/**
 * Advanced density scalping signal with multi-timeframe context and asset pair analysis.
 * 
 * <p>Entry logic for LONG:
 * <ol>
 *   <li>Higher timeframe trend: UP or SIDEWAYS</li>
 *   <li>Density cluster on bid side with significant volume</li>
 *   <li>Level has history (bounces) or high relative volume</li>
 *   <li>Compression detected: spread narrowing + volume accumulation</li>
 *   <li>Micro-impulse UP detected (final trigger)</li>
 *   <li>Asset pair analysis: no divergence, basis normal</li>
 * </ol>
 * 
 * <p>Entry logic for SHORT is symmetric.
 */
public final class DensityScalpSignal implements OrderBookSignal {

    public static final String SIGNAL_ID = "densityScalp";

    private final TradingService tradingService;
    private final OrderBookScalpConfig config;
    private final DensityAnalyzer densityAnalyzer;
    private final AssetPairAnalyzer pairAnalyzer;
    private final TrendAnalyzer trendAnalyzer;
    private final LevelHistory levelHistory;
    private final CompressionDetector compressionDetector;
    private final MicroImpulseDetector impulseDetector;
    
    // Callback for emitting diagnostic metrics
    private BiConsumer<String, Map<String, Object>> skipMetricsCallback;

    // Track entry state per ticker
    private final Map<String, EntryState> entryStateByTicker = new ConcurrentHashMap<>();

    public DensityScalpSignal(
            TradingService tradingService,
            OrderBookScalpConfig config,
            AssetPairAnalyzer pairAnalyzer,
            TrendAnalyzer trendAnalyzer,
            LevelHistory levelHistory,
            CompressionDetector compressionDetector,
            MicroImpulseDetector impulseDetector) {
        this.tradingService = tradingService;
        this.config = config;
        this.densityAnalyzer = new DensityAnalyzer(tradingService, config);
        this.pairAnalyzer = pairAnalyzer;
        this.trendAnalyzer = trendAnalyzer;
        this.levelHistory = levelHistory;
        this.compressionDetector = compressionDetector;
        this.impulseDetector = impulseDetector;
    }
    
    /**
     * Set callback for emitting skip diagnostic metrics.
     * The callback receives (ticker, metrics) where metrics contains skip reason and analysis data.
     */
    public void setSkipMetricsCallback(BiConsumer<String, Map<String, Object>> callback) {
        this.skipMetricsCallback = callback;
    }

    @Override
    public String id() {
        return SIGNAL_ID;
    }

    @Override
    public OrderBookEntryDecision evaluateEntry(OrderBookMarketContext context, String ticker) {
        return evaluateEntryInternal(context, ticker, "LONG");
    }

    @Override
    public OrderBookEntryDecision evaluateEntryShort(OrderBookMarketContext context, String ticker) {
        return evaluateEntryInternal(context, ticker, "SHORT");
    }

    private OrderBookEntryDecision evaluateEntryInternal(
            OrderBookMarketContext context, String ticker, String direction) {
        
        TickerInfo.Key key = context.getKey();
        MarketDepthSnapshot snapshot = context.getSnapshot();
        double currentPrice = "LONG".equals(direction) ? context.getBestBid() : context.getBestAsk();
        
        // Base metrics for diagnostics
        Map<String, Object> metrics = new HashMap<>();
        metrics.put("signalId", SIGNAL_ID);
        metrics.put("obi", context.getObi());
        metrics.put("microEdge", context.getMicroEdge());
        metrics.put("tradeDelta", context.getTradeDelta());
        metrics.put("spreadBps", context.getSpreadBps());

        // Step 1: Check higher timeframe trend
        TrendAnalyzer.Trend trend = trendAnalyzer.getTrend(key);
        metrics.put("trend", trend.name());
        if (trendAnalyzer.isCounterTrend(key, direction)) {
            metrics.put("skipReason", "counter_trend");
            emitSkipMetrics(ticker, metrics);
            LoggingUtils.log("densityScalp " + ticker + " " + direction + ": counter-trend (trend=" + trend + ")");
            return OrderBookEntryDecision.none();
        }

        // Step 2: Find density clusters
        boolean isBid = "LONG".equals(direction);
        List<DensityAnalyzer.ClusteredDensity> clusters = 
                densityAnalyzer.findClusteredDensities(snapshot, ticker, config.getClusterTicks());
        metrics.put("clusterCount", clusters.size());
        
        DensityAnalyzer.ClusteredDensity bestCluster = null;
        double bestStrength = 0.0;
        long avgVolume = densityAnalyzer.getAverageVolume(ticker);
        
        for (DensityAnalyzer.ClusteredDensity cluster : clusters) {
            if (cluster.isBid() != isBid) {
                continue; // Wrong side
            }
            
            // Calculate level strength
            double strength = levelHistory.getLevelStrength(
                    ticker, cluster.getPrice(), isBid, avgVolume);
            
            // Also consider cluster volume
            double volumeRatio = cluster.getVolume() / Math.max(1, avgVolume);
            strength += Math.min(volumeRatio / 5.0, 0.5);
            
            if (strength > bestStrength) {
                bestStrength = strength;
                bestCluster = cluster;
            }
        }
        metrics.put("levelStrength", bestStrength);

        if (bestCluster == null || bestStrength < 0.3) {
            metrics.put("skipReason", "no_significant_level");
            emitSkipMetrics(ticker, metrics);
            LoggingUtils.log("densityScalp " + ticker + " " + direction + ": no significant level (clusters=" + clusters.size() + ", bestStrength=" + String.format("%.2f", bestStrength) + ")");
            return OrderBookEntryDecision.none(); // No significant level
        }

        // Step 3: Record level for history
        levelHistory.recordLevel(ticker, bestCluster.getPrice(), bestCluster.getVolume(), 
                isBid, avgVolume);

        // Step 4: Check compression
        double compressionStrength = compressionDetector.getCompressionStrength(
                ticker, bestCluster.getPrice(), currentPrice);
        metrics.put("compressionStrength", compressionStrength);
        
        if (!compressionDetector.isCompressed(ticker, bestCluster.getPrice(), currentPrice)) {
            double spread = compressionDetector.getCurrentSpreadBps(ticker);
            double avgSpread = compressionDetector.getAverageSpreadBps(ticker);
            metrics.put("skipReason", "no_compression");
            metrics.put("currentSpread", spread);
            metrics.put("avgSpread", avgSpread);
            emitSkipMetrics(ticker, metrics);
            LoggingUtils.log("densityScalp " + ticker + " " + direction + ": no compression (spread=" + String.format("%.1f", spread) + "bps, avg=" + String.format("%.1f", avgSpread) + "bps)");
            return OrderBookEntryDecision.none(); // No compression
        }

        // Step 5: Check micro-impulse
        double impulseStrength = impulseDetector.getImpulseStrength(ticker, direction);
        metrics.put("impulseStrength", impulseStrength);
        
        if (!impulseDetector.detectImpulse(ticker, direction)) {
            String flowDir = impulseDetector.getFlowDirection(ticker);
            metrics.put("skipReason", "no_impulse");
            metrics.put("flowDirection", flowDir);
            emitSkipMetrics(ticker, metrics);
            LoggingUtils.log("densityScalp " + ticker + " " + direction + ": no impulse (flow=" + flowDir + ")");
            return OrderBookEntryDecision.none(); // No impulse yet
        }

        // Step 6: Asset pair analysis
        String pairedFuture = pairAnalyzer.getPairedFuture(ticker);
        if (pairedFuture != null) {
            // Check for divergence
            if (pairAnalyzer.detectDivergence(ticker, pairedFuture)) {
                metrics.put("skipReason", "divergence");
                metrics.put("pairedAsset", pairedFuture);
                emitSkipMetrics(ticker, metrics);
                LoggingUtils.log("densityScalp " + ticker + " " + direction + ": divergence with " + pairedFuture);
                return OrderBookEntryDecision.none(); // Divergence detected
            }
            
            // Check for basis anomaly
            if (pairAnalyzer.isBasisAnomalous(ticker)) {
                metrics.put("skipReason", "basis_anomaly");
                emitSkipMetrics(ticker, metrics);
                LoggingUtils.log("densityScalp " + ticker + " " + direction + ": basis anomaly");
                return OrderBookEntryDecision.none(); // Basis abnormal
            }
            
            // Check if leader has given impulse
            String leaderImpulse = pairAnalyzer.detectLeaderImpulse(pairedFuture);
            if (leaderImpulse != null && !leaderImpulse.equals(direction)) {
                metrics.put("skipReason", "leader_opposite");
                metrics.put("leaderImpulse", leaderImpulse);
                emitSkipMetrics(ticker, metrics);
                LoggingUtils.log("densityScalp " + ticker + " " + direction + ": leader " + pairedFuture + " going " + leaderImpulse);
                return OrderBookEntryDecision.none(); // Leader going opposite direction
            }
        }

        // Step 7: Calculate entry quality
        double quality = (bestStrength + compressionStrength + impulseStrength) / 3.0;
        metrics.put("quality", quality);

        // Step 8: Record bounce for this level
        levelHistory.recordBounce(ticker, bestCluster.getPrice(), isBid);

        // Build entry decision
        String reason = String.format(
                "%s densityScalp: level=%.4f strength=%.2f compression=%.2f impulse=%.2f quality=%.2f",
                direction,
                bestCluster.getPrice(),
                bestStrength,
                compressionStrength,
                impulseStrength,
                quality);

        LoggingUtils.log(reason);
        return OrderBookEntryDecision.enter(reason);
    }
    
    /**
     * Emit skip metrics via callback for CSV diagnostics.
     */
    private void emitSkipMetrics(String ticker, Map<String, Object> metrics) {
        if (skipMetricsCallback != null) {
            skipMetricsCallback.accept(ticker, metrics);
        }
    }

    @Override
    public String evaluateExit(
            OrderBookMarketContext context, OrderBookPositionView position, String ticker) {
        
        boolean isLong = "LONG".equals(position.getDirection());
        double currentPrice = isLong ? context.getBestBid() : context.getBestAsk();
        double entryPrice = position.getEntryPrice();

        // Check if density level still exists (spoofing detection)
        List<DensityAnalyzer.ClusteredDensity> clusters = 
                densityAnalyzer.findClusteredDensities(context.getSnapshot(), ticker, config.getClusterTicks());
        
        boolean levelExists = false;
        for (DensityAnalyzer.ClusteredDensity cluster : clusters) {
            if (cluster.isBid() == isLong) {
                // Check if cluster is near entry level
                double distance = Math.abs(cluster.getPrice() - entryPrice) / entryPrice * 10000.0;
                if (distance < 10.0) { // Within 10 bps
                    levelExists = true;
                    break;
                }
            }
        }

        if (!levelExists && Math.abs(currentPrice - entryPrice) / entryPrice < 0.001) {
            // Level disappeared and price hasn't moved much - likely spoofing
            return "density_spoofing";
        }

        // Check for flow reversal
        String flowDirection = impulseDetector.getFlowDirection(ticker);
        if (isLong && "DOWN".equals(flowDirection)) {
            return "flow_reversal";
        }
        if (!isLong && "UP".equals(flowDirection)) {
            return "flow_reversal";
        }

        return null; // No exit signal
    }

    @Override
    public void reset(String ticker) {
        entryStateByTicker.remove(ticker);
    }

    /**
     * Handle order book updates.
     */
    public void onOrderBook(MarketDepthSnapshot snapshot, String ticker) {
        compressionDetector.onOrderBook(ticker, snapshot);
    }

    /**
     * Handle trade updates.
     */
    public void onTrade(MarketTradeTick trade, String ticker) {
        impulseDetector.onTrade(ticker, trade);
        pairAnalyzer.onTrade(ticker, trade);
    }

    /**
     * Log signal status.
     */
    public void logStatus() {
        LoggingUtils.log("DensityScalpSignal status:");
        trendAnalyzer.logStatus();
        levelHistory.logStatus();
        compressionDetector.logStatus();
        impulseDetector.logStatus();
        pairAnalyzer.logStatus();
    }

    /**
     * Entry state tracking per ticker.
     */
    private static final class EntryState {
        String direction;
        double levelPrice;
        long entryAttemptMs;
        int attempts;

        EntryState(String direction, double levelPrice) {
            this.direction = direction;
            this.levelPrice = levelPrice;
            this.entryAttemptMs = System.currentTimeMillis();
            this.attempts = 1;
        }
    }
}
