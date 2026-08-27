package com.github.shk0da.goldendragon.strategy.orderbook;

import com.github.shk0da.goldendragon.model.MarketTradeTick;
import com.github.shk0da.goldendragon.utils.LoggingUtils;

import java.time.Instant;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Detects micro-impulses in trade flow for precise entry timing.
 * 
 * <p>A micro-impulse is characterized by:
 * <ul>
 *   <li>N trades in one direction within T milliseconds</li>
 *   <li>Total volume significantly above average</li>
 *   <li>Consistent direction (all or most trades in same direction)</li>
 * </ul>
 * 
 * <p>This is the final trigger for entry after compression is detected.
 */
public final class MicroImpulseDetector {

    private final int minTrades;
    private final int windowMs;
    private final double volumeMultiplier;

    // Track trade history by ticker
    private final Map<String, TradeHistory> historyByTicker = new ConcurrentHashMap<>();

    public MicroImpulseDetector(int minTrades, int windowMs, double volumeMultiplier) {
        this.minTrades = minTrades;
        this.windowMs = windowMs;
        this.volumeMultiplier = volumeMultiplier;
    }

    /**
     * Record a trade tick for impulse detection.
     */
    public void onTrade(String ticker, MarketTradeTick trade) {
        if (trade == null) {
            return;
        }
        historyByTicker.computeIfAbsent(ticker, k -> new TradeHistory(windowMs))
                .addTrade(trade);
    }

    /**
     * Detect if a micro-impulse is occurring.
     * 
     * @param ticker instrument ticker
     * @param expectedDirection expected direction: "UP" or "DOWN"
     * @return true if impulse detected in expected direction
     */
    public boolean detectImpulse(String ticker, String expectedDirection) {
        TradeHistory history = historyByTicker.get(ticker);
        if (history == null) {
            return false;
        }

        // Get trades in window
        TradeStats stats = history.getRecentStats();
        if (stats == null || stats.tradeCount < minTrades) {
            return false;
        }

        // Check volume threshold
        if (stats.totalVolume < stats.avgVolumePerTrade * minTrades * volumeMultiplier) {
            return false;
        }

        // Check direction
        if ("UP".equals(expectedDirection)) {
            return stats.buyRatio > 0.7; // 70%+ buy trades
        } else if ("DOWN".equals(expectedDirection)) {
            return stats.sellRatio > 0.7; // 70%+ sell trades
        }

        return false;
    }

    /**
     * Get impulse strength (0.0 to 1.0).
     * Higher values indicate stronger impulse.
     */
    public double getImpulseStrength(String ticker, String expectedDirection) {
        TradeHistory history = historyByTicker.get(ticker);
        if (history == null) {
            return 0.0;
        }

        TradeStats stats = history.getRecentStats();
        if (stats == null || stats.tradeCount < minTrades) {
            return 0.0;
        }

        // Trade count component (0.0 to 0.3)
        double countRatio = Math.min((double) stats.tradeCount / (minTrades * 2), 1.0) * 0.3;

        // Volume component (0.0 to 0.4)
        double expectedVolume = stats.avgVolumePerTrade * minTrades;
        double volumeRatio = expectedVolume > 0 
                ? Math.min((double) stats.totalVolume / expectedVolume / volumeMultiplier, 1.0) * 0.4
                : 0.0;

        // Direction component (0.0 to 0.3)
        double directionScore = 0.0;
        if ("UP".equals(expectedDirection)) {
            directionScore = Math.max(0.0, (stats.buyRatio - 0.5) * 2.0) * 0.3;
        } else if ("DOWN".equals(expectedDirection)) {
            directionScore = Math.max(0.0, (stats.sellRatio - 0.5) * 2.0) * 0.3;
        }

        return countRatio + volumeRatio + directionScore;
    }

    /**
     * Get current trade flow direction.
     * 
     * @return "UP", "DOWN", or null if no clear direction
     */
    public String getFlowDirection(String ticker) {
        TradeHistory history = historyByTicker.get(ticker);
        if (history == null) {
            return null;
        }

        TradeStats stats = history.getRecentStats();
        if (stats == null || stats.tradeCount < 3) {
            return null;
        }

        if (stats.buyRatio > 0.6) {
            return "UP";
        } else if (stats.sellRatio > 0.6) {
            return "DOWN";
        }
        return "NEUTRAL";
    }

    /**
     * Get current buy ratio (0.0 to 1.0).
     */
    public double getBuyRatio(String ticker) {
        TradeHistory history = historyByTicker.get(ticker);
        if (history == null) {
            return 0.5;
        }
        TradeStats stats = history.getRecentStats();
        return stats != null ? stats.buyRatio : 0.5;
    }

    /**
     * Log impulse status.
     */
    public void logStatus() {
        LoggingUtils.log("MicroImpulseDetector status:");
        for (Map.Entry<String, TradeHistory> entry : historyByTicker.entrySet()) {
            TradeStats stats = entry.getValue().getRecentStats();
            if (stats != null) {
                LoggingUtils.log("  " + entry.getKey() + 
                        ": trades=" + stats.tradeCount +
                        " buyRatio=" + String.format("%.2f", stats.buyRatio) +
                        " volume=" + stats.totalVolume);
            }
        }
    }

    /**
     * Reset all tracking data.
     */
    public void reset() {
        historyByTicker.clear();
    }

    /**
     * Tracks trade history for a ticker.
     */
    private static final class TradeHistory {
        private final int windowMs;
        private final java.util.Deque<TradeRecord> trades = new java.util.concurrent.ConcurrentLinkedDeque<>();

        TradeHistory(int windowMs) {
            this.windowMs = windowMs;
        }

        void addTrade(MarketTradeTick trade) {
            long timeMs = trade.getTime().toEpochMilli();
            boolean isBuy = isBuyDirection(trade.getDirection());
            trades.addLast(new TradeRecord(timeMs, trade.getQuantity(), isBuy));
            
            // Clean old trades
            long cutoff = timeMs - windowMs;
            while (!trades.isEmpty() && trades.getFirst().timeMs < cutoff) {
                trades.removeFirst();
            }
        }

        TradeStats getRecentStats() {
            if (trades.isEmpty()) {
                return null;
            }

            int tradeCount = 0;
            long totalVolume = 0;
            long buyVolume = 0;
            long sellVolume = 0;

            for (TradeRecord trade : trades) {
                tradeCount++;
                totalVolume += trade.volume;
                if (trade.isBuy) {
                    buyVolume += trade.volume;
                } else {
                    sellVolume += trade.volume;
                }
            }

            if (tradeCount == 0) {
                return null;
            }

            double avgVolumePerTrade = (double) totalVolume / tradeCount;
            double buyRatio = totalVolume > 0 ? (double) buyVolume / totalVolume : 0.5;
            double sellRatio = totalVolume > 0 ? (double) sellVolume / totalVolume : 0.5;

            return new TradeStats(tradeCount, totalVolume, avgVolumePerTrade, buyRatio, sellRatio);
        }

        private static boolean isBuyDirection(String direction) {
            if (direction == null) {
                return false;
            }
            String normalized = direction.toUpperCase();
            return normalized.contains("BUY") || normalized.equals("B");
        }
    }

    /**
     * Single trade record.
     */
    private static final class TradeRecord {
        final long timeMs;
        final long volume;
        final boolean isBuy;

        TradeRecord(long timeMs, long volume, boolean isBuy) {
            this.timeMs = timeMs;
            this.volume = volume;
            this.isBuy = isBuy;
        }
    }

    /**
     * Statistics for recent trades.
     */
    private static final class TradeStats {
        final int tradeCount;
        final long totalVolume;
        final double avgVolumePerTrade;
        final double buyRatio;
        final double sellRatio;

        TradeStats(int tradeCount, long totalVolume, double avgVolumePerTrade, 
                   double buyRatio, double sellRatio) {
            this.tradeCount = tradeCount;
            this.totalVolume = totalVolume;
            this.avgVolumePerTrade = avgVolumePerTrade;
            this.buyRatio = buyRatio;
            this.sellRatio = sellRatio;
        }
    }
}
