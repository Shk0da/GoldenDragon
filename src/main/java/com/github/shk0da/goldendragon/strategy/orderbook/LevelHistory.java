package com.github.shk0da.goldendragon.strategy.orderbook;

import com.github.shk0da.goldendragon.utils.LoggingUtils;

import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Tracks key price levels (density clusters) and their bounce history.
 * 
 * <p>Key features:
 * <ul>
 *   <li>Records significant price levels from density clusters</li>
 *   <li>Tracks bounce count for each level</li>
 *   <li>Calculates level strength based on volume and bounce history</li>
 *   <li>Detects spoofing when levels disappear</li>
 * </ul>
 */
public final class LevelHistory {

    private final double minLevelVolumeRatio;
    private final int maxLevelAgeMinutes;
    private final double priceToleranceBps;

    // Track levels by ticker
    private final Map<String, List<LevelRecord>> levelsByTicker = new ConcurrentHashMap<>();

    public LevelHistory(double minLevelVolumeRatio, int maxLevelAgeMinutes, double priceToleranceBps) {
        this.minLevelVolumeRatio = minLevelVolumeRatio;
        this.maxLevelAgeMinutes = maxLevelAgeMinutes;
        this.priceToleranceBps = priceToleranceBps;
    }

    /**
     * Record a new price level from density cluster.
     */
    public void recordLevel(String ticker, double price, long volume, boolean isBid, double avgVolume) {
        // Check if volume is significant
        if (avgVolume <= 0 || volume < avgVolume * minLevelVolumeRatio) {
            return; // Not significant enough
        }

        List<LevelRecord> levels = levelsByTicker.computeIfAbsent(ticker, k -> new ArrayList<>());
        
        // Check if level already exists (within tolerance)
        double tolerance = price * priceToleranceBps / 10000.0;
        for (LevelRecord level : levels) {
            if (Math.abs(level.price - price) <= tolerance && level.isBid == isBid) {
                // Update existing level
                level.lastSeenMs = System.currentTimeMillis();
                level.volume = Math.max(level.volume, volume);
                return;
            }
        }

        // Add new level
        levels.add(new LevelRecord(price, volume, isBid, System.currentTimeMillis()));
        
        // Clean old levels
        cleanOldLevels(levels);
    }

    /**
     * Record a bounce from a level.
     */
    public void recordBounce(String ticker, double price, boolean isBid) {
        List<LevelRecord> levels = levelsByTicker.get(ticker);
        if (levels == null) {
            return;
        }

        double tolerance = price * priceToleranceBps / 10000.0;
        for (LevelRecord level : levels) {
            if (Math.abs(level.price - price) <= tolerance && level.isBid == isBid) {
                level.bounceCount++;
                level.lastBounceMs = System.currentTimeMillis();
                return;
            }
        }
    }

    /**
     * Get bounce count for a level near the given price.
     */
    public int getBounceCount(String ticker, double price, boolean isBid) {
        List<LevelRecord> levels = levelsByTicker.get(ticker);
        if (levels == null) {
            return 0;
        }

        double tolerance = price * priceToleranceBps / 10000.0;
        for (LevelRecord level : levels) {
            if (Math.abs(level.price - price) <= tolerance && level.isBid == isBid) {
                return level.bounceCount;
            }
        }
        return 0;
    }

    /**
     * Get level strength (0.0 to 1.0) based on volume and bounce history.
     */
    public double getLevelStrength(String ticker, double price, boolean isBid, double avgVolume) {
        List<LevelRecord> levels = levelsByTicker.get(ticker);
        if (levels == null) {
            return 0.0;
        }

        double tolerance = price * priceToleranceBps / 10000.0;
        for (LevelRecord level : levels) {
            if (Math.abs(level.price - price) <= tolerance && level.isBid == isBid) {
                // Volume component (0.0 to 0.5)
                double volumeRatio = avgVolume > 0 ? level.volume / avgVolume : 0.0;
                double volumeScore = Math.min(volumeRatio / 10.0, 1.0) * 0.5;
                
                // Bounce component (0.0 to 0.5)
                double bounceScore = Math.min(level.bounceCount / 3.0, 1.0) * 0.5;
                
                return volumeScore + bounceScore;
            }
        }
        return 0.0;
    }

    /**
     * Check if a level still exists (not spoofed).
     */
    public boolean levelExists(String ticker, double price, boolean isBid, long currentVolume) {
        List<LevelRecord> levels = levelsByTicker.get(ticker);
        if (levels == null) {
            return false;
        }

        double tolerance = price * priceToleranceBps / 10000.0;
        for (LevelRecord level : levels) {
            if (Math.abs(level.price - price) <= tolerance && level.isBid == isBid) {
                // Level exists if volume is still significant (> 50% of recorded)
                return currentVolume >= level.volume * 0.5;
            }
        }
        return false;
    }

    /**
     * Get all significant levels for a ticker.
     */
    public List<LevelRecord> getLevels(String ticker) {
        List<LevelRecord> levels = levelsByTicker.get(ticker);
        if (levels == null) {
            return new ArrayList<>();
        }
        cleanOldLevels(levels);
        return new ArrayList<>(levels);
    }

    /**
     * Clean old levels that haven't been seen recently.
     */
    private void cleanOldLevels(List<LevelRecord> levels) {
        long cutoffMs = System.currentTimeMillis() - maxLevelAgeMinutes * 60_000L;
        levels.removeIf(level -> level.lastSeenMs < cutoffMs);
    }

    /**
     * Log current level status.
     */
    public void logStatus() {
        LoggingUtils.log("LevelHistory status:");
        for (Map.Entry<String, List<LevelRecord>> entry : levelsByTicker.entrySet()) {
            LoggingUtils.log("  " + entry.getKey() + ": " + entry.getValue().size() + " levels");
            for (LevelRecord level : entry.getValue().stream().limit(5).toList()) {
                LoggingUtils.log("    " + level.price + " (" + (level.isBid ? "bid" : "ask") + 
                        ") vol=" + level.volume + " bounces=" + level.bounceCount);
            }
        }
    }

    /**
     * Reset all level data.
     */
    public void reset() {
        levelsByTicker.clear();
    }

    /**
     * Record of a significant price level.
     */
    public static final class LevelRecord {
        public final double price;
        public long volume;
        public final boolean isBid;
        public final long createdMs;
        public long lastSeenMs;
        public int bounceCount;
        public long lastBounceMs;

        LevelRecord(double price, long volume, boolean isBid, long createdMs) {
            this.price = price;
            this.volume = volume;
            this.isBid = isBid;
            this.createdMs = createdMs;
            this.lastSeenMs = createdMs;
            this.bounceCount = 0;
            this.lastBounceMs = 0;
        }

        public double getPrice() {
            return price;
        }

        public long getVolume() {
            return volume;
        }

        public boolean isBid() {
            return isBid;
        }

        public int getBounceCount() {
            return bounceCount;
        }

        public boolean isExpired(int maxAgeMinutes) {
            return System.currentTimeMillis() - lastSeenMs > maxAgeMinutes * 60_000L;
        }
    }
}
