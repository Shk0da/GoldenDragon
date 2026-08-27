package com.github.shk0da.goldendragon.strategy.orderbook;

import com.github.shk0da.goldendragon.model.MarketTradeTick;
import com.github.shk0da.goldendragon.utils.LoggingUtils;

import java.time.Instant;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Analyzes relationships between paired assets (stock <-> futures) for scalping signals.
 * 
 * <p>Key features:
 * <ul>
 *   <li>Leader-follower tracking: futures lead, stocks follow with 3-5 second lag</li>
 *   <li>Basis anomaly detection: skip trading when basis is abnormal</li>
 *   <li>Divergence detection: block entry when assets move in opposite directions</li>
 * </ul>
 */
public final class AssetPairAnalyzer {

    // Direct pairs: stock <-> its futures
    private static final Map<String, String> STOCK_TO_FUTURE = Map.ofEntries(
            Map.entry("SBER", "SBERF"),
            Map.entry("GAZP", "GAZPF"),
            Map.entry("GMKN", "GMKNF"),
            Map.entry("VTBR", "VTBRF"),
            Map.entry("LKOH", "LKOHF"),
            Map.entry("ROSN", "ROSNF"),
            Map.entry("YDEX", "YDEXF"),
            Map.entry("TATN", "TATNF"),
            Map.entry("NVTK", "NVTKF")
    );

    // Index futures for basket stocks
    private static final String INDEX_FUTURE = "IMOEXF";
    
    // Stocks that are major IMOEX components
    private static final List<String> INDEX_COMPONENTS = List.of(
            "SBER", "GAZP", "GMKN", "LKOH", "ROSN", "VTBR",
            "YDEX", "MGNT", "PLZL", "MTSS", "TATN", "NVTK"
    );

    private final int leaderLagSeconds;
    private final double basisAnomalySigma;
    private final boolean divergenceBlockEnabled;

    // Track recent price movements for each ticker
    private final Map<String, PriceMovementTracker> movementByTicker = new ConcurrentHashMap<>();
    
    // Track basis history for anomaly detection
    private final Map<String, BasisTracker> basisByPair = new ConcurrentHashMap<>();

    public AssetPairAnalyzer(int leaderLagSeconds, double basisAnomalySigma, boolean divergenceBlockEnabled) {
        this.leaderLagSeconds = leaderLagSeconds;
        this.basisAnomalySigma = basisAnomalySigma;
        this.divergenceBlockEnabled = divergenceBlockEnabled;
    }

    /**
     * Get the paired futures for a stock, or null if no pair exists.
     */
    public String getPairedFuture(String stockTicker) {
        if (stockTicker == null) {
            return null;
        }
        String normalized = stockTicker.toUpperCase();
        // Direct pair
        String directFuture = STOCK_TO_FUTURE.get(normalized);
        if (directFuture != null) {
            return directFuture;
        }
        // Index future for components
        if (INDEX_COMPONENTS.contains(normalized)) {
            return INDEX_FUTURE;
        }
        return null;
    }

    /**
     * Get the paired stock for a futures ticker, or null if no pair exists.
     */
    public String getPairedStock(String futureTicker) {
        if (futureTicker == null) {
            return null;
        }
        String normalized = futureTicker.toUpperCase();
        // Check direct pairs (reverse lookup)
        for (Map.Entry<String, String> entry : STOCK_TO_FUTURE.entrySet()) {
            if (entry.getValue().equals(normalized)) {
                return entry.getKey();
            }
        }
        // Index future maps to multiple stocks - return null as it's many-to-one
        if (INDEX_FUTURE.equals(normalized)) {
            return null; // Multiple stocks, handled differently
        }
        return null;
    }

    /**
     * Check if a ticker is a futures contract.
     */
    public boolean isFutures(String ticker) {
        if (ticker == null) {
            return false;
        }
        String normalized = ticker.toUpperCase();
        return normalized.endsWith("F") || STOCK_TO_FUTURE.containsValue(normalized);
    }

    /**
     * Check if a ticker is a stock.
     */
    public boolean isStock(String ticker) {
        if (ticker == null) {
            return false;
        }
        String normalized = ticker.toUpperCase();
        return STOCK_TO_FUTURE.containsKey(normalized) || INDEX_COMPONENTS.contains(normalized);
    }

    /**
     * Record a trade tick for movement tracking.
     */
    public void onTrade(String ticker, MarketTradeTick trade) {
        movementByTicker.computeIfAbsent(ticker, k -> new PriceMovementTracker(leaderLagSeconds))
                .onTrade(trade);
    }

    /**
     * Record current price for basis tracking.
     */
    public void onPriceUpdate(String stockTicker, double stockPrice, double futurePrice) {
        String futureTicker = getPairedFuture(stockTicker);
        if (futureTicker == null) {
            return;
        }
        String pairKey = stockTicker + "_" + futureTicker;
        basisByPair.computeIfAbsent(pairKey, k -> new BasisTracker())
                .onPriceUpdate(stockPrice, futurePrice);
    }

    /**
     * Check if the leader (futures) has given an impulse signal.
     * Returns the direction: "UP", "DOWN", or null if no impulse.
     */
    public String detectLeaderImpulse(String leaderTicker) {
        PriceMovementTracker tracker = movementByTicker.get(leaderTicker);
        if (tracker == null) {
            return null;
        }
        return tracker.getRecentImpulse();
    }

    /**
     * Check if the follower (stock) is ready for entry (hasn't moved yet after leader impulse).
     * Returns true if follower is lagging behind leader.
     */
    public boolean isFollowerReady(String followerTicker, String leaderDirection) {
        PriceMovementTracker tracker = movementByTicker.get(followerTicker);
        if (tracker == null) {
            return true; // No data yet, assume ready
        }
        String followerDirection = tracker.getRecentDirection();
        // Follower is ready if it hasn't moved in the same direction yet
        return followerDirection == null || !followerDirection.equals(leaderDirection);
    }

    /**
     * Detect divergence between paired assets.
     * Returns true if assets are moving in opposite directions.
     */
    public boolean detectDivergence(String ticker1, String ticker2) {
        if (!divergenceBlockEnabled) {
            return false;
        }
        PriceMovementTracker tracker1 = movementByTicker.get(ticker1);
        PriceMovementTracker tracker2 = movementByTicker.get(ticker2);
        if (tracker1 == null || tracker2 == null) {
            return false; // Not enough data
        }
        String dir1 = tracker1.getRecentDirection();
        String dir2 = tracker2.getRecentDirection();
        if (dir1 == null || dir2 == null) {
            return false;
        }
        // Divergence: one UP, other DOWN
        return !dir1.equals(dir2);
    }

    /**
     * Check if basis is anomalous (too far from mean).
     * Returns true if basis is abnormal and trading should be avoided.
     */
    public boolean isBasisAnomalous(String stockTicker) {
        String futureTicker = getPairedFuture(stockTicker);
        if (futureTicker == null) {
            return false;
        }
        String pairKey = stockTicker + "_" + futureTicker;
        BasisTracker tracker = basisByPair.get(pairKey);
        if (tracker == null) {
            return false; // Not enough data
        }
        return tracker.isAnomalous(basisAnomalySigma);
    }

    /**
     * Get the pair strength (correlation of movements).
     * Returns value from 0.0 (no correlation) to 1.0 (perfect correlation).
     */
    public double getPairStrength(String stockTicker, String futureTicker) {
        PriceMovementTracker stockTracker = movementByTicker.get(stockTicker);
        PriceMovementTracker futureTracker = movementByTicker.get(futureTicker);
        if (stockTracker == null || futureTracker == null) {
            return 0.0;
        }
        return stockTracker.getCorrelation(futureTracker);
    }

    /**
     * Log current pair analysis status.
     */
    public void logStatus() {
        LoggingUtils.log("AssetPairAnalyzer status:");
        LoggingUtils.log("  Tracked tickers: " + movementByTicker.size());
        LoggingUtils.log("  Tracked pairs: " + basisByPair.size());
        for (Map.Entry<String, PriceMovementTracker> entry : movementByTicker.entrySet()) {
            String direction = entry.getValue().getRecentDirection();
            LoggingUtils.log("  " + entry.getKey() + ": " + (direction != null ? direction : "neutral"));
        }
    }

    /**
     * Reset all tracking data.
     */
    public void reset() {
        movementByTicker.clear();
        basisByPair.clear();
    }

    /**
     * Tracks price movements and detects impulses for a single ticker.
     */
    private static final class PriceMovementTracker {
        private final int windowSeconds;
        private final Map<Long, Double> priceByTime = new ConcurrentHashMap<>();
        private double lastPrice = 0.0;
        private Instant lastImpulseTime = Instant.EPOCH;
        private String lastImpulseDirection = null;

        PriceMovementTracker(int windowSeconds) {
            this.windowSeconds = windowSeconds;
        }

        void onTrade(MarketTradeTick trade) {
            long timeMs = trade.getTime().toEpochMilli();
            double price = trade.getPrice();
            priceByTime.put(timeMs, price);
            lastPrice = price;
            
            // Clean old data
            long cutoff = timeMs - windowSeconds * 1000L;
            priceByTime.entrySet().removeIf(e -> e.getKey() < cutoff);
        }

        String getRecentDirection() {
            if (priceByTime.size() < 2) {
                return null;
            }
            double firstPrice = priceByTime.values().stream().findFirst().orElse(0.0);
            double lastPrice = priceByTime.values().stream()
                    .reduce((first, second) -> second)
                    .orElse(0.0);
            double change = (lastPrice - firstPrice) / firstPrice;
            if (change > 0.0005) { // 0.05% threshold
                return "UP";
            } else if (change < -0.0005) {
                return "DOWN";
            }
            return "NEUTRAL";
        }

        String getRecentImpulse() {
            // Check for strong recent movement (> 0.1% in window)
            if (priceByTime.size() < 2) {
                return null;
            }
            double firstPrice = priceByTime.values().stream().findFirst().orElse(0.0);
            double lastPrice = priceByTime.values().stream()
                    .reduce((first, second) -> second)
                    .orElse(0.0);
            double change = (lastPrice - firstPrice) / firstPrice;
            if (change > 0.001) { // 0.1% threshold for impulse
                return "UP";
            } else if (change < -0.001) {
                return "DOWN";
            }
            return null;
        }

        double getCorrelation(PriceMovementTracker other) {
            // Simple correlation based on direction alignment
            String thisDir = getRecentDirection();
            String otherDir = other.getRecentDirection();
            if (thisDir == null || otherDir == null) {
                return 0.0;
            }
            if (thisDir.equals(otherDir)) {
                return 1.0;
            }
            if (thisDir.equals("NEUTRAL") || otherDir.equals("NEUTRAL")) {
                return 0.5;
            }
            return 0.0; // Opposite directions
        }
    }

    /**
     * Tracks basis (futures - stock) for anomaly detection.
     */
    private static final class BasisTracker {
        private final Map<Long, Double> basisByTime = new ConcurrentHashMap<>();
        private double meanBasis = 0.0;
        private double basisStdDev = 0.0;
        private int sampleCount = 0;

        void onPriceUpdate(double stockPrice, double futurePrice) {
            long timeMs = System.currentTimeMillis();
            double basis = futurePrice - stockPrice;
            basisByTime.put(timeMs, basis);
            
            // Update statistics
            sampleCount++;
            if (sampleCount == 1) {
                meanBasis = basis;
                basisStdDev = 0.0;
            } else {
                double oldMean = meanBasis;
                meanBasis = oldMean + (basis - oldMean) / sampleCount;
                basisStdDev = basisStdDev + (basis - oldMean) * (basis - meanBasis);
                if (sampleCount > 1) {
                    basisStdDev = Math.sqrt(basisStdDev / (sampleCount - 1));
                }
            }
            
            // Clean old data (keep last 1000 samples)
            if (basisByTime.size() > 1000) {
                long cutoff = basisByTime.keySet().stream()
                        .sorted()
                        .limit(basisByTime.size() - 1000)
                        .findFirst()
                        .orElse(0L);
                basisByTime.entrySet().removeIf(e -> e.getKey() <= cutoff);
            }
        }

        boolean isAnomalous(double sigmaThreshold) {
            if (sampleCount < 10 || basisStdDev < 0.0001) {
                return false; // Not enough data or too stable
            }
            double currentBasis = basisByTime.values().stream()
                    .reduce((first, second) -> second)
                    .orElse(0.0);
            double zScore = Math.abs(currentBasis - meanBasis) / basisStdDev;
            return zScore > sigmaThreshold;
        }
    }
}
