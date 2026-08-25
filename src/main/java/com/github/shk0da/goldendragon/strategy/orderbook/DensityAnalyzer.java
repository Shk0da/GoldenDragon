package com.github.shk0da.goldendragon.strategy.orderbook;

import com.github.shk0da.goldendragon.config.OrderBookScalpConfig;
import com.github.shk0da.goldendragon.model.Candle;
import com.github.shk0da.goldendragon.model.MarketDepthLevel;
import com.github.shk0da.goldendragon.model.MarketDepthSnapshot;
import com.github.shk0da.goldendragon.service.TCSService;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Analyzes order book for large limit order densities.
 * 
 * <p>Detects two types of densities:
 * <ul>
 *   <li>Density_1 (Local): Volume >= Average_Volume_5m * 3</li>
 *   <li>Density_2 (Anomalous): Volume >= Average_Volume_5m * 5</li>
 * </ul>
 */
public final class DensityAnalyzer {
    
    private static final int AVERAGE_VOLUME_WINDOW = 24; // 2 hours of 5-min candles
    
    private static final double DENSITY_1_MULTIPLIER = 3.0;
    private static final double DENSITY_2_MULTIPLIER = 5.0;
    private static final double DENSITY_PROXIMITY_PERCENT = 0.0005; // 0.05%
    
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
            return Collections.emptyList();
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
}
