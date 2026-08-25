package com.github.shk0da.goldendragon.strategy.orderbook;

import com.github.shk0da.goldendragon.model.MarketTradeTick;

import java.time.Duration;
import java.time.Instant;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Tracks cumulative delta (Volume_Ask - Volume_Bid) within 10-second bars.
 * 
 * <p>Detects:
 * <ul>
 *   <li>Delta decay - decreasing bar heights</li>
 *   <li>Delta divergence - price moves but delta doesn't confirm</li>
 *   <li>Exponential delta growth - aggressive market orders</li>
 * </ul>
 */
public final class CumulativeDeltaTracker {
    
    private static final long BAR_DURATION_MS = 10_000; // 10 seconds
    private static final int RECENT_BARS_COUNT = 20; // Keep last 20 bars for analysis
    private static final int DECAY_BARS_COUNT = 2; // Check last 2 bars for decay
    
    private final Map<String, TickerDeltaState> statesByTicker = new ConcurrentHashMap<>();
    
    /**
     * State for a single ticker's delta tracking.
     */
    private static final class TickerDeltaState {
        final Deque<DeltaBar> bars = new ArrayDeque<>();
        DeltaBar currentBar;
        Instant lastBarEndTime;
        
        TickerDeltaState() {
            this.currentBar = new DeltaBar(Instant.now());
            this.lastBarEndTime = Instant.now();
        }
    }
    
    /**
     * A single 10-second delta bar.
     */
    public static final class DeltaBar {
        private final Instant startTime;
        private long askVolume;
        private long bidVolume;
        private long cumulativeDelta;
        
        DeltaBar(Instant startTime) {
            this.startTime = startTime;
            this.askVolume = 0;
            this.bidVolume = 0;
            this.cumulativeDelta = 0;
        }
        
        void addTrade(MarketTradeTick trade) {
            long volume = trade.getQuantity();
            if ("BUY".equalsIgnoreCase(trade.getDirection()) || 
                "ASK".equalsIgnoreCase(trade.getDirection())) {
                askVolume += volume;
                cumulativeDelta += volume;
            } else {
                bidVolume += volume;
                cumulativeDelta -= volume;
            }
        }
        
        public Instant getStartTime() {
            return startTime;
        }
        
        public long getAskVolume() {
            return askVolume;
        }
        
        public long getBidVolume() {
            return bidVolume;
        }
        
        public long getCumulativeDelta() {
            return cumulativeDelta;
        }
        
        public long getTotalVolume() {
            return askVolume + bidVolume;
        }
    }
    
    /**
     * Result of delta analysis.
     */
    public static final class DeltaAnalysis {
        private final long currentDelta;
        private final boolean isDecaying;
        private final boolean isDiverging;
        private final boolean isExponentialGrowth;
        private final double growthRate;
        
        DeltaAnalysis(long currentDelta, boolean isDecaying, boolean isDiverging, 
                     boolean isExponentialGrowth, double growthRate) {
            this.currentDelta = currentDelta;
            this.isDecaying = isDecaying;
            this.isDiverging = isDiverging;
            this.isExponentialGrowth = isExponentialGrowth;
            this.growthRate = growthRate;
        }
        
        public long getCurrentDelta() {
            return currentDelta;
        }
        
        public boolean isDecaying() {
            return isDecaying;
        }
        
        public boolean isDiverging() {
            return isDiverging;
        }
        
        public boolean isExponentialGrowth() {
            return isExponentialGrowth;
        }
        
        public double getGrowthRate() {
            return growthRate;
        }
    }
    
    /**
     * Process a trade tick and update delta tracking.
     */
    public void onTrade(String ticker, MarketTradeTick trade) {
        TickerDeltaState state = statesByTicker.computeIfAbsent(
            ticker, 
            k -> new TickerDeltaState()
        );
        
        Instant now = trade.getTime();
        
        // Check if we need to start a new bar
        if (Duration.between(state.currentBar.startTime, now).toMillis() >= BAR_DURATION_MS) {
            // Save current bar
            state.bars.addLast(state.currentBar);
            if (state.bars.size() > RECENT_BARS_COUNT) {
                state.bars.removeFirst();
            }
            
            // Start new bar
            state.currentBar = new DeltaBar(now);
            state.lastBarEndTime = now;
        }
        
        // Add trade to current bar
        state.currentBar.addTrade(trade);
    }
    
    /**
     * Get current delta for ticker.
     */
    public long getCurrentDelta(String ticker) {
        TickerDeltaState state = statesByTicker.get(ticker);
        return state != null ? state.currentBar.getCumulativeDelta() : 0;
    }
    
    /**
     * Analyze delta for decay, divergence, and exponential growth.
     * 
     * @param ticker ticker symbol
     * @param priceDirection 1 for up, -1 for down, 0 for neutral
     * @return delta analysis result
     */
    public DeltaAnalysis analyzeDelta(String ticker, int priceDirection) {
        TickerDeltaState state = statesByTicker.get(ticker);
        if (state == null || state.bars.isEmpty()) {
            return new DeltaAnalysis(0, false, false, false, 0.0);
        }
        
        List<DeltaBar> recentBars = new ArrayList<>(state.bars);
        if (recentBars.size() < DECAY_BARS_COUNT) {
            return new DeltaAnalysis(
                state.currentBar.getCumulativeDelta(),
                false,
                false,
                false,
                0.0
            );
        }
        
        // Check for decay (decreasing bar heights)
        boolean isDecaying = checkDecay(recentBars);
        
        // Check for divergence (price moves but delta doesn't confirm)
        boolean isDiverging = checkDivergence(recentBars, priceDirection);
        
        // Check for exponential growth
        double growthRate = calculateGrowthRate(recentBars);
        boolean isExponentialGrowth = growthRate > 2.0; // Exponential if > 2x growth
        
        return new DeltaAnalysis(
            state.currentBar.getCumulativeDelta(),
            isDecaying,
            isDiverging,
            isExponentialGrowth,
            growthRate
        );
    }
    
    private boolean checkDecay(List<DeltaBar> bars) {
        if (bars.size() < DECAY_BARS_COUNT) {
            return false;
        }
        
        // Check if last N bars are decreasing in height
        for (int i = bars.size() - DECAY_BARS_COUNT; i < bars.size() - 1; i++) {
            DeltaBar current = bars.get(i);
            DeltaBar next = bars.get(i + 1);
            
            if (Math.abs(next.getTotalVolume()) >= Math.abs(current.getTotalVolume())) {
                return false;
            }
        }
        
        return true;
    }
    
    private boolean checkDivergence(List<DeltaBar> bars, int priceDirection) {
        if (bars.size() < DECAY_BARS_COUNT || priceDirection == 0) {
            return false;
        }
        
        // Check if delta direction contradicts price direction
        DeltaBar lastBar = bars.get(bars.size() - 1);
        long deltaSign = Long.signum(lastBar.getCumulativeDelta());
        
        // Divergence: price up but delta negative, or price down but delta positive
        return (priceDirection > 0 && deltaSign < 0) || 
               (priceDirection < 0 && deltaSign > 0);
    }
    
    private double calculateGrowthRate(List<DeltaBar> bars) {
        if (bars.size() < 2) {
            return 1.0;
        }
        
        DeltaBar first = bars.get(bars.size() - DECAY_BARS_COUNT);
        DeltaBar last = bars.get(bars.size() - 1);
        
        if (first.getTotalVolume() == 0) {
            return last.getTotalVolume() > 0 ? Double.MAX_VALUE : 1.0;
        }
        
        return (double) last.getTotalVolume() / first.getTotalVolume();
    }
    
    /**
     * Get recent delta bars for analysis.
     */
    public List<DeltaBar> getRecentBars(String ticker, int count) {
        TickerDeltaState state = statesByTicker.get(ticker);
        if (state == null) {
            return Collections.emptyList();
        }
        
        List<DeltaBar> bars = new ArrayList<>(state.bars);
        int size = Math.min(count, bars.size());
        return bars.subList(bars.size() - size, bars.size());
    }
    
    /**
     * Reset tracking for ticker.
     */
    public void reset(String ticker) {
        statesByTicker.remove(ticker);
    }
}
