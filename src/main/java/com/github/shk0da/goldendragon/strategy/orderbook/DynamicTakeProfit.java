package com.github.shk0da.goldendragon.strategy.orderbook;

import com.github.shk0da.goldendragon.model.MarketDepthSnapshot;

import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.NavigableMap;

/**
 * Calculates take profit targets based on significant density levels
 * instead of fixed spread multiples.
 *
 * <p>For LONG positions, the TP is placed at the nearest ask-side density
 * (resistance) above the entry price. For SHORT positions, the TP is placed
 * at the nearest bid-side density (support) below the entry price.
 *
 * <p>If no significant density level is found within the configured maximum
 * distance, the calculator falls back to a minimum fixed spread distance.
 */
public final class DynamicTakeProfit {

    private final DensityAnalyzer densityAnalyzer;
    private final VolumeProfileTracker volumeProfileTracker;
    private final int clusterTicks;
    private final double maxTpDistanceBps;
    private final double minTpDistanceBps;

    /**
     * Creates a dynamic take profit calculator.
     *
     * @param densityAnalyzer     analyzer for detecting density clusters in the order book
     * @param volumeProfileTracker tracker for volume distribution by price (may be null)
     * @param clusterTicks         number of ticks for density clustering
     * @param maxTpDistanceBps     maximum TP distance in basis points to prevent excessive targets
     * @param minTpDistanceBps     minimum TP distance in basis points as fallback when no density found
     */
    public DynamicTakeProfit(
            DensityAnalyzer densityAnalyzer,
            VolumeProfileTracker volumeProfileTracker,
            int clusterTicks,
            double maxTpDistanceBps,
            double minTpDistanceBps) {
        if (densityAnalyzer == null) {
            throw new IllegalArgumentException("densityAnalyzer must not be null");
        }
        if (clusterTicks <= 0) {
            throw new IllegalArgumentException("clusterTicks must be positive");
        }
        if (maxTpDistanceBps <= 0) {
            throw new IllegalArgumentException("maxTpDistanceBps must be positive");
        }
        if (minTpDistanceBps <= 0) {
            throw new IllegalArgumentException("minTpDistanceBps must be positive");
        }
        this.densityAnalyzer = densityAnalyzer;
        this.volumeProfileTracker = volumeProfileTracker;
        this.clusterTicks = clusterTicks;
        this.maxTpDistanceBps = maxTpDistanceBps;
        this.minTpDistanceBps = minTpDistanceBps;
    }

    /**
     * Calculates the take profit price for the given entry.
     *
     * <p>For LONG: finds the nearest resistance density (ask-side cluster) above entry.
     * For SHORT: finds the nearest support density (bid-side cluster) below entry.
     * Falls back to minimum fixed spread if no density level is found within range.
     *
     * @param entryPrice the entry price of the position
     * @param isLong     true for long position, false for short
     * @param snapshot   current order book snapshot
     * @param ticker     ticker symbol
     * @return calculated take profit price
     */
    public double calculateTakeProfit(
            double entryPrice,
            boolean isLong,
            MarketDepthSnapshot snapshot,
            String ticker) {

        if (entryPrice <= 0) {
            return 0.0;
        }

        // Try to find TP target from clustered densities first
        double densityTp = findDensityBasedTp(entryPrice, isLong, snapshot, ticker);
        if (densityTp > 0) {
            return densityTp;
        }

        // Try volume profile POC/value area as secondary source
        double profileTp = findVolumeProfileTp(entryPrice, isLong);
        if (profileTp > 0) {
            return profileTp;
        }

        // Fallback to minimum fixed spread
        return calculateFallbackTp(entryPrice, isLong);
    }

    /**
     * Finds a TP target based on clustered density levels from the order book.
     *
     * <p>For LONG: looks for ask-side clusters (resistance) above entry price.
     * For SHORT: looks for bid-side clusters (support) below entry price.
     */
    private double findDensityBasedTp(
            double entryPrice,
            boolean isLong,
            MarketDepthSnapshot snapshot,
            String ticker) {

        List<DensityAnalyzer.ClusteredDensity> clusters =
                densityAnalyzer.findClusteredDensities(snapshot, ticker, clusterTicks);
        if (clusters.isEmpty()) {
            return 0.0;
        }

        double maxDistance = entryPrice * maxTpDistanceBps / 10000.0;

        if (isLong) {
            // LONG: find nearest ask-side cluster (resistance) above entry
            return clusters.stream()
                    .filter(c -> !c.isBid())
                    .filter(c -> c.getPrice() > entryPrice)
                    .filter(c -> (c.getPrice() - entryPrice) <= maxDistance)
                    .min(Comparator.comparingDouble(c -> c.getPrice() - entryPrice))
                    .map(DensityAnalyzer.ClusteredDensity::getPrice)
                    .orElse(0.0);
        } else {
            // SHORT: find nearest bid-side cluster (support) below entry
            return clusters.stream()
                    .filter(DensityAnalyzer.ClusteredDensity::isBid)
                    .filter(c -> c.getPrice() < entryPrice)
                    .filter(c -> (entryPrice - c.getPrice()) <= maxDistance)
                    .min(Comparator.comparingDouble(c -> entryPrice - c.getPrice()))
                    .map(DensityAnalyzer.ClusteredDensity::getPrice)
                    .orElse(0.0);
        }
    }

    /**
     * Finds a TP target based on volume profile levels (POC, value area boundaries).
     *
     * <p>For LONG: uses POC or value area high as resistance target.
     * For SHORT: uses POC or value area low as support target.
     */
    private double findVolumeProfileTp(double entryPrice, boolean isLong) {
        if (volumeProfileTracker == null) {
            return 0.0;
        }
        if (volumeProfileTracker.getTotalVolume() <= 0) {
            return 0.0;
        }

        double maxDistance = entryPrice * maxTpDistanceBps / 10000.0;

        if (isLong) {
            // Look for POC above entry as a magnet/target
            double poc = volumeProfileTracker.getPoc();
            if (poc > entryPrice && (poc - entryPrice) <= maxDistance) {
                return poc;
            }
            // Try value area high
            double vah = volumeProfileTracker.getValueAreaHigh();
            if (vah > entryPrice && (vah - entryPrice) <= maxDistance) {
                return vah;
            }
        } else {
            // Look for POC below entry as a magnet/target
            double poc = volumeProfileTracker.getPoc();
            if (poc > 0 && poc < entryPrice && (entryPrice - poc) <= maxDistance) {
                return poc;
            }
            // Try value area low
            double val = volumeProfileTracker.getValueAreaLow();
            if (val > 0 && val < entryPrice && (entryPrice - val) <= maxDistance) {
                return val;
            }
        }

        // Try to find high-volume price level from the profile as target
        return findHighVolumeLevelTp(entryPrice, isLong, maxDistance);
    }

    /**
     * Scans the volume profile for a significant volume node to use as TP target.
     * Looks for the price level with the highest volume on the target side of entry.
     */
    private double findHighVolumeLevelTp(
            double entryPrice,
            boolean isLong,
            double maxDistance) {

        NavigableMap<Double, Long> profile = volumeProfileTracker.getVolumeProfile();
        if (profile.isEmpty()) {
            return 0.0;
        }

        double bestPrice = 0.0;
        long bestVolume = 0;

        for (Map.Entry<Double, Long> entry : profile.entrySet()) {
            double levelPrice = entry.getKey();
            long levelVolume = entry.getValue();

            if (isLong) {
                if (levelPrice <= entryPrice) {
                    continue;
                }
                if ((levelPrice - entryPrice) > maxDistance) {
                    continue;
                }
            } else {
                if (levelPrice >= entryPrice) {
                    continue;
                }
                if ((entryPrice - levelPrice) > maxDistance) {
                    continue;
                }
            }

            if (levelVolume > bestVolume) {
                bestVolume = levelVolume;
                bestPrice = levelPrice;
            }
        }

        return bestPrice;
    }

    /**
     * Calculates fallback TP using minimum fixed spread distance.
     */
    private double calculateFallbackTp(double entryPrice, boolean isLong) {
        double distance = entryPrice * minTpDistanceBps / 10000.0;
        if (isLong) {
            return entryPrice + distance;
        } else {
            return Math.max(0.0, entryPrice - distance);
        }
    }
}
