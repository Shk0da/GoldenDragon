package com.github.shk0da.goldendragon.strategy.orderbook;

import com.github.shk0da.goldendragon.config.OrderBookScalpConfig;
import com.github.shk0da.goldendragon.model.Candle;
import com.github.shk0da.goldendragon.model.MarketDepthLevel;
import com.github.shk0da.goldendragon.model.MarketDepthSnapshot;
import com.github.shk0da.goldendragon.service.TCSService;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Deque;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Analyzes order book for large limit order densities.
 *
 * <p>Detects two types of densities:
 * <ul>
 *   <li>Density_1 (Local): Volume >= Average_Volume_5m * 3</li>
 *   <li>Density_2 (Anomalous): Volume >= Average_Volume_5m * 5</li>
 * </ul>
 *
 * <p>Additionally supports cluster detection (TODO.md Section 2):
 * groups nearby density levels into clusters using cluster_ticks.
 */
public final class DensityAnalyzer {

    private static final int AVERAGE_VOLUME_WINDOW = 24; // 2 hours of 5-min candles

    private static final double DENSITY_1_MULTIPLIER = 3.0;
    private static final double DENSITY_2_MULTIPLIER = 5.0;
    private static final double DENSITY_PROXIMITY_PERCENT = 0.0005; // 0.05%

    // Snapshot-based fallback thresholds (used when history unavailable)
    // These are absolute volume levels that indicate significant liquidity
    private static final long SNAPSHOT_DENSITY_THRESHOLD = 1000L; // Local density
    private static final long SNAPSHOT_ANOMALOUS_THRESHOLD = 5000L; // Anomalous density

    private final TCSService tcsService;
    private final OrderBookScalpConfig config;
    private final Map<String, VolumeHistory> volumeHistories = new ConcurrentHashMap<>();

    /**
     * Density information at a specific price level.
     */
    public static final class Density {
        private final double price;
        private final long volume;
        private final DensityType type;
        private final boolean isBid;

        public Density(double price, long volume, DensityType type, boolean isBid) {
            this.price = price;
            this.volume = volume;
            this.type = type;
            this.isBid = isBid;
        }

        public double getPrice() {
            return price;
        }

        public long getVolume() {
            return volume;
        }

        public DensityType getType() {
            return type;
        }

        public boolean isBid() {
            return isBid;
        }

        public boolean isAnomalous() {
            return type == DensityType.ANOMALOUS;
        }
    }

    /**
     * Density type classification.
     */
    public enum DensityType {
        LOCAL,      // 3x average volume
        ANOMALOUS   // 5x average volume
    }

    /**
     * Rolling window of 5-minute volumes for average calculation.
     */
    private static final class VolumeHistory {
        private final Deque<Long> volumes = new ArrayDeque<>();
        private final int maxSize;

        VolumeHistory(int maxSize) {
            this.maxSize = maxSize;
        }

        void addVolume(long volume) {
            volumes.addLast(volume);
            if (volumes.size() > maxSize) {
                volumes.removeFirst();
            }
        }

        long getVolume(Candle candle) {
            return candle.volume;
        }

        long getAverageVolume() {
            if (volumes.isEmpty()) {
                return 0;
            }
            long sum = volumes.stream().mapToLong(Long::longValue).sum();
            return sum / volumes.size();
        }

        boolean hasSufficientData() {
            return volumes.size() >= maxSize * 0.8; // At least 80% of window
        }
    }

    public DensityAnalyzer(TCSService tcsService, OrderBookScalpConfig config) {
        this.tcsService = tcsService;
        this.config = config;
    }

    /**
     * Update volume history with latest 5-minute candle.
     * Should be called periodically (every 5 minutes).
     */
    public void updateVolumeHistory(String ticker, Candle candle) {
        VolumeHistory history = volumeHistories.computeIfAbsent(
            ticker,
            k -> new VolumeHistory(AVERAGE_VOLUME_WINDOW)
        );
        history.addVolume(candle.volume);
    }

    /**
     * Initialize volume history from historical candles.
     */
    public void initializeVolumeHistory(String ticker, List<Candle> candles) {
        VolumeHistory history = new VolumeHistory(AVERAGE_VOLUME_WINDOW);

        // Take last N 5-minute candles
        int start = Math.max(0, candles.size() - AVERAGE_VOLUME_WINDOW);
        for (int i = start; i < candles.size(); i++) {
            history.addVolume(candles.get(i).volume);
        }

        volumeHistories.put(ticker, history);
    }

    /**
     * Find densities in the order book.
     *
     * @param snapshot current order book snapshot
     * @param ticker ticker symbol
     * @return list of detected densities
     */
    public List<Density> findDensities(MarketDepthSnapshot snapshot, String ticker) {
        VolumeHistory history = volumeHistories.get(ticker);
        if (history == null || !history.hasSufficientData()) {
            // Fallback: use absolute volume threshold when history is unavailable
            // (e.g. first 2 hours of trading, cold start)
            return findDensitiesFromSnapshot(snapshot);
        }

        long averageVolume = history.getAverageVolume();
        long density1Threshold = (long) (averageVolume * DENSITY_1_MULTIPLIER);
        long density2Threshold = (long) (averageVolume * DENSITY_2_MULTIPLIER);

        List<Density> densities = new ArrayList<>();

        // Scan bid side
        for (MarketDepthLevel bid : snapshot.getBids()) {
            long volume = bid.getQuantity();

            if (volume >= density2Threshold) {
                densities.add(new Density(bid.getPrice(), volume, DensityType.ANOMALOUS, true));
            } else if (volume >= density1Threshold) {
                densities.add(new Density(bid.getPrice(), volume, DensityType.LOCAL, true));
            }
        }

        // Scan ask side
        for (MarketDepthLevel ask : snapshot.getAsks()) {
            long volume = ask.getQuantity();

            if (volume >= density2Threshold) {
                densities.add(new Density(ask.getPrice(), volume, DensityType.ANOMALOUS, false));
            } else if (volume >= density1Threshold) {
                densities.add(new Density(ask.getPrice(), volume, DensityType.LOCAL, false));
            }
        }

        return densities;
    }

    /**
     * Fallback density detection using absolute volume thresholds.
     *
     * <p>Used when volume history is unavailable (cold start, first 2 hours).
     * Treats any order book level with quantity above a hardcoded threshold
     * as a valid density level, allowing the signal to fire immediately.
     *
     * @param snapshot current order book snapshot
     * @return list of detected densities
     */
    private List<Density> findDensitiesFromSnapshot(MarketDepthSnapshot snapshot) {
        List<Density> densities = new ArrayList<>();

        // Scan bid side with absolute thresholds
        for (MarketDepthLevel bid : snapshot.getBids()) {
            long volume = bid.getQuantity();
            if (volume >= SNAPSHOT_ANOMALOUS_THRESHOLD) {
                densities.add(new Density(bid.getPrice(), volume, DensityType.ANOMALOUS, true));
            } else if (volume >= SNAPSHOT_DENSITY_THRESHOLD) {
                densities.add(new Density(bid.getPrice(), volume, DensityType.LOCAL, true));
            }
        }

        // Scan ask side with absolute thresholds
        for (MarketDepthLevel ask : snapshot.getAsks()) {
            long volume = ask.getQuantity();
            if (volume >= SNAPSHOT_ANOMALOUS_THRESHOLD) {
                densities.add(new Density(ask.getPrice(), volume, DensityType.ANOMALOUS, false));
            } else if (volume >= SNAPSHOT_DENSITY_THRESHOLD) {
                densities.add(new Density(ask.getPrice(), volume, DensityType.LOCAL, false));
            }
        }

        return densities;
    }

    /**
     * Check if price is approaching a density level.
     *
     * @param currentPrice current market price
     * @param density density to check
     * @return true if price is within 0.05% of density
     */
    public boolean isApproachingDensity(double currentPrice, Density density) {
        double distance = Math.abs(currentPrice - density.getPrice());
        double distancePercent = distance / currentPrice;
        return distancePercent <= DENSITY_PROXIMITY_PERCENT;
    }

    /**
     * Find the nearest density on the specified side.
     *
     * @param snapshot order book snapshot
     * @param ticker ticker symbol
     * @param isBid true for bid side, false for ask side
     * @return nearest density or null if none found
     */
    public Density findNearestDensity(MarketDepthSnapshot snapshot, String ticker, boolean isBid) {
        List<Density> densities = findDensities(snapshot, ticker);

        double currentPrice = isBid ? snapshot.getBestBid() : snapshot.getBestAsk();
        if (currentPrice <= 0) {
            return null;
        }

        return densities.stream()
            .filter(d -> d.isBid() == isBid)
            .filter(d -> isApproachingDensity(currentPrice, d))
            .min(Comparator.comparingDouble(d -> Math.abs(d.getPrice() - currentPrice)))
            .orElse(null);
    }

    /**
     * Find anomalous density (Density_2) on the specified side.
     *
     * @param snapshot order book snapshot
     * @param ticker ticker symbol
     * @param isBid true for bid side, false for ask side
     * @return anomalous density or null if none found
     */
    public Density findAnomalousDensity(MarketDepthSnapshot snapshot, String ticker, boolean isBid) {
        List<Density> densities = findDensities(snapshot, ticker);

        return densities.stream()
            .filter(d -> d.isBid() == isBid)
            .filter(Density::isAnomalous)
            .findFirst()
            .orElse(null);
    }

    /**
     * Get average volume for ticker.
     */
    public long getAverageVolume(String ticker) {
        VolumeHistory history = volumeHistories.get(ticker);
        return history != null ? history.getAverageVolume() : 0;
    }

    /**
     * Check if volume history has sufficient data.
     */
    public boolean hasSufficientData(String ticker) {
        VolumeHistory history = volumeHistories.get(ticker);
        return history != null && history.hasSufficientData();
    }

    /**
     * Find clustered densities - groups nearby density levels into clusters (TODO.md Section 2).
     *
     * <p>Clusters volumes within cluster_ticks price range.
     * Useful for detecting "spread" large orders.
     *
     * @param snapshot current order book
     * @param ticker ticker symbol
     * @param clusterTicks number of ticks for clustering
     * @return list of clustered densities
     */
    public List<ClusteredDensity> findClusteredDensities(MarketDepthSnapshot snapshot, String ticker, int clusterTicks) {
        List<Density> densities = findDensities(snapshot, ticker);
        if (densities.isEmpty()) {
            return Collections.emptyList();
        }

        // Group by side
        List<Density> bidDensities = densities.stream().filter(Density::isBid).toList();
        List<Density> askDensities = densities.stream().filter(d -> !d.isBid()).toList();

        List<ClusteredDensity> clusters = new ArrayList<>();

        clusters.addAll(groupDensities(bidDensities, clusterTicks, true));
        clusters.addAll(groupDensities(askDensities, clusterTicks, false));

        return clusters;
    }

    private List<ClusteredDensity> groupDensities(List<Density> densities, int clusterTicks, boolean isBid) {
        if (densities.isEmpty()) {
            return Collections.emptyList();
        }

        // Sort by price
        densities.sort(Comparator.comparingDouble(Density::getPrice));

        List<ClusteredDensity> clusters = new ArrayList<>();
        double clusterPriceStep = calculateClusterStep(densities.get(0).getPrice(), clusterTicks);

        // Simple greedy clustering
        List<Density> remaining = new ArrayList<>(densities);
        while (!remaining.isEmpty()) {
            Density first = remaining.get(0);
            double clusterPrice = first.getPrice();
            long totalVolume = 0;
            int count = 0;

            Iterator<Density> it = remaining.iterator();
            while (it.hasNext()) {
                Density d = it.next();
                if (Math.abs(d.getPrice() - clusterPrice) <= clusterPriceStep) {
                    totalVolume += d.getVolume();
                    count++;
                    it.remove();
                }
            }

            if (count > 0 && totalVolume > 0) {
                double avgVolume = totalVolume / count;
                long avgVol = getAverageVolume();
                DensityType type = avgVolume >= (avgVol * DENSITY_2_MULTIPLIER)
                    ? DensityType.ANOMALOUS
                    : DensityType.LOCAL;

                clusters.add(new ClusteredDensity(clusterPrice, totalVolume, type, isBid, count));
            }
        }

        return clusters;
    }

    private long getAverageVolume() {
        // Use overall average from histories
        double avg = volumeHistories.values().stream()
            .mapToLong(VolumeHistory::getAverageVolume)
            .average()
            .orElse(0.0);
        return (long) avg;
    }

    private double calculateClusterStep(double price, int ticks) {
        // Cluster step must be based on tick size, not arbitrary decimals.
        // For futures (price > 1000) the tick size is 1.0 RUB, not 0.01.
        double tickSize = getTickSizeForPrice(price);
        return tickSize * ticks;
    }

    /**
     * Estimated tick size by price level.
     *
     * <p>Matches typical exchange tick sizes:
     * <ul>
     *   <li>price &gt; 10000 → 0.1 (e.g. some indices)</li>
     *   <li>price &gt; 1000 → 1.0 (futures, e.g. VBU6 @ 5100)</li>
     *   <li>price &gt; 100 → 0.1 (e.g. stocks)</li>
     *   <li>else → 0.01 (e.g. currencies)</li>
     * </ul>
     */
    private double getTickSizeForPrice(double price) {
        if (price > 10000) return 0.1;
        if (price > 1000) return 1.0;
        if (price > 100) return 0.1;
        return 0.01;
    }

    /**
     * Clustered density - group of nearby density levels.
     */
    public static final class ClusteredDensity {
        private final double price;
        private final long volume;
        private final DensityType type;
        private final boolean isBid;
        private final int densityCount;

        public ClusteredDensity(double price, long volume, DensityType type, boolean isBid, int densityCount) {
            this.price = price;
            this.volume = volume;
            this.type = type;
            this.isBid = isBid;
            this.densityCount = densityCount;
        }

        public double getPrice() {
            return price;
        }

        public long getVolume() {
            return volume;
        }

        public DensityType getType() {
            return type;
        }

        public boolean isBid() {
            return isBid;
        }

        public int getDensityCount() {
            return densityCount;
        }

        public boolean isAnomalous() {
            return type == DensityType.ANOMALOUS;
        }
    }
}
