package com.github.shk0da.goldendragon.strategy.orderbook;

import com.github.shk0da.goldendragon.model.MarketDepthSnapshot;
import com.github.shk0da.goldendragon.model.MarketDepthLevel;
import com.github.shk0da.goldendragon.utils.LoggingUtils;

import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Detects compression (поджатие) patterns in the order book.
 * 
 * <p>Compression is characterized by:
 * <ul>
 *   <li>Spread narrowing below threshold</li>
 *   <li>Volume accumulation at key levels</li>
 *   <li>Price approaching a significant level</li>
 * </ul>
 * 
 * <p>This is a key signal for entry timing in density scalping.
 */
public final class CompressionDetector {

    private final double compressionSpreadBps;
    private final double volumeMultiplier;
    private final double proximityToleranceBps;
    private final int historySize;

    // Track spread and volume history by ticker
    private final Map<String, SpreadHistory> spreadByTicker = new ConcurrentHashMap<>();
    private final Map<String, VolumeHistory> volumeByTicker = new ConcurrentHashMap<>();

    public CompressionDetector(double compressionSpreadBps, double volumeMultiplier, 
                                double proximityToleranceBps, int historySize) {
        this.compressionSpreadBps = compressionSpreadBps;
        this.volumeMultiplier = volumeMultiplier;
        this.proximityToleranceBps = proximityToleranceBps;
        this.historySize = historySize;
    }

    /**
     * Update compression tracking with new order book data.
     */
    public void onOrderBook(String ticker, MarketDepthSnapshot snapshot) {
        if (snapshot == null) {
            return;
        }

        // Update spread history
        Double bestBid = snapshot.getBestBid();
        Double bestAsk = snapshot.getBestAsk();
        if (bestBid != null && bestAsk != null && bestAsk > bestBid) {
            double mid = (bestBid + bestAsk) / 2.0;
            double spreadBps = (bestAsk - bestBid) / mid * 10000.0;
            spreadByTicker.computeIfAbsent(ticker, k -> new SpreadHistory(historySize))
                    .addSpread(spreadBps);
        }

        // Update volume history at top of book
        int topVolume = 0;
        List<MarketDepthLevel> bids = snapshot.getBids();
        List<MarketDepthLevel> asks = snapshot.getAsks();
        if (bids != null && !bids.isEmpty()) {
            topVolume += bids.get(0).getQuantity();
        }
        if (asks != null && !asks.isEmpty()) {
            topVolume += asks.get(0).getQuantity();
        }
        volumeByTicker.computeIfAbsent(ticker, k -> new VolumeHistory(historySize))
                .addVolume(topVolume);
    }

    /**
     * Check if compression is detected.
     * 
     * @param ticker instrument ticker
     * @param keyLevelPrice significant level price (from LevelHistory)
     * @param currentPrice current market price
     * @return true if compression pattern is detected
     */
    public boolean isCompressed(String ticker, double keyLevelPrice, double currentPrice) {
        SpreadHistory spreadHistory = spreadByTicker.get(ticker);
        VolumeHistory volumeHistory = volumeByTicker.get(ticker);

        if (spreadHistory == null || volumeHistory == null) {
            return false;
        }

        // Check 1: Spread is compressed (below threshold)
        double currentSpread = spreadHistory.getCurrentSpread();
        if (currentSpread > compressionSpreadBps) {
            return false;
        }

        // Check 2: Spread is narrower than average
        double avgSpread = spreadHistory.getAverageSpread();
        if (currentSpread > avgSpread * 0.7) { // 30% tighter than average
            return false;
        }

        // Check 3: Volume is accumulating (above average)
        int currentVolume = volumeHistory.getCurrentVolume();
        int avgVolume = volumeHistory.getAverageVolume();
        if (currentVolume < avgVolume * volumeMultiplier) {
            return false;
        }

        // Check 4: Price is near key level
        double distance = Math.abs(currentPrice - keyLevelPrice) / keyLevelPrice * 10000.0;
        if (distance > proximityToleranceBps) {
            return false;
        }

        return true;
    }

    /**
     * Get compression strength (0.0 to 1.0).
     * Higher values indicate stronger compression pattern.
     */
    public double getCompressionStrength(String ticker, double keyLevelPrice, double currentPrice) {
        SpreadHistory spreadHistory = spreadByTicker.get(ticker);
        VolumeHistory volumeHistory = volumeByTicker.get(ticker);

        if (spreadHistory == null || volumeHistory == null) {
            return 0.0;
        }

        // Spread component (0.0 to 0.4)
        double currentSpread = spreadHistory.getCurrentSpread();
        double avgSpread = spreadHistory.getAverageSpread();
        double spreadRatio = avgSpread > 0 ? currentSpread / avgSpread : 1.0;
        double spreadScore = Math.max(0.0, (1.0 - spreadRatio)) * 0.4;

        // Volume component (0.0 to 0.3)
        int currentVolume = volumeHistory.getCurrentVolume();
        int avgVolume = volumeHistory.getAverageVolume();
        double volumeRatio = avgVolume > 0 ? (double) currentVolume / avgVolume : 1.0;
        double volumeScore = Math.min((volumeRatio - 1.0) / 2.0, 1.0) * 0.3;

        // Proximity component (0.0 to 0.3)
        double distance = Math.abs(currentPrice - keyLevelPrice) / keyLevelPrice * 10000.0;
        double proximityScore = Math.max(0.0, (proximityToleranceBps - distance) / proximityToleranceBps) * 0.3;

        return spreadScore + volumeScore + proximityScore;
    }

    /**
     * Get current spread in bps.
     */
    public double getCurrentSpreadBps(String ticker) {
        SpreadHistory history = spreadByTicker.get(ticker);
        return history != null ? history.getCurrentSpread() : 0.0;
    }

    /**
     * Get average spread in bps.
     */
    public double getAverageSpreadBps(String ticker) {
        SpreadHistory history = spreadByTicker.get(ticker);
        return history != null ? history.getAverageSpread() : 0.0;
    }

    /**
     * Log compression status.
     */
    public void logStatus() {
        LoggingUtils.log("CompressionDetector status:");
        for (Map.Entry<String, SpreadHistory> entry : spreadByTicker.entrySet()) {
            SpreadHistory spread = entry.getValue();
            VolumeHistory volume = volumeByTicker.get(entry.getKey());
            LoggingUtils.log("  " + entry.getKey() + 
                    ": spread=" + String.format("%.1f", spread.getCurrentSpread()) + "bps" +
                    " (avg=" + String.format("%.1f", spread.getAverageSpread()) + ")" +
                    " volume=" + (volume != null ? volume.getCurrentVolume() : 0) +
                    " (avg=" + (volume != null ? volume.getAverageVolume() : 0) + ")");
        }
    }

    /**
     * Reset all tracking data.
     */
    public void reset() {
        spreadByTicker.clear();
        volumeByTicker.clear();
    }

    /**
     * Tracks spread history for a ticker.
     */
    private static final class SpreadHistory {
        private final double[] spreads;
        private int index;
        private int count;

        SpreadHistory(int size) {
            this.spreads = new double[size];
            this.index = 0;
            this.count = 0;
        }

        void addSpread(double spreadBps) {
            spreads[index] = spreadBps;
            index = (index + 1) % spreads.length;
            count = Math.min(count + 1, spreads.length);
        }

        double getCurrentSpread() {
            if (count == 0) return 0.0;
            int lastIdx = (index - 1 + spreads.length) % spreads.length;
            return spreads[lastIdx];
        }

        double getAverageSpread() {
            if (count == 0) return 0.0;
            double sum = 0.0;
            for (int i = 0; i < count; i++) {
                sum += spreads[i];
            }
            return sum / count;
        }
    }

    /**
     * Tracks volume history for a ticker.
     */
    private static final class VolumeHistory {
        private final int[] volumes;
        private int index;
        private int count;

        VolumeHistory(int size) {
            this.volumes = new int[size];
            this.index = 0;
            this.count = 0;
        }

        void addVolume(int volume) {
            volumes[index] = volume;
            index = (index + 1) % volumes.length;
            count = Math.min(count + 1, volumes.length);
        }

        int getCurrentVolume() {
            if (count == 0) return 0;
            int lastIdx = (index - 1 + volumes.length) % volumes.length;
            return volumes[lastIdx];
        }

        int getAverageVolume() {
            if (count == 0) return 0;
            long sum = 0;
            for (int i = 0; i < count; i++) {
                sum += volumes[i];
            }
            return (int) (sum / count);
        }
    }
}
