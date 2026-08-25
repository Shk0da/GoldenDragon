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
     * <p>Formulas per TODO.md Section 3:
     * <ul>
     *   <li>Decay (3.2): |d0| < |d1| * (1 - fade_ratio) for same-sign deltas</li>
     *   <li>Divergence (3.2): delta reverses sign (d0 > 0 after d1 < 0)</li>
     *   <li>Exponential Growth (3.4): |d0| > |d1| * accel_ratio</li>
     * </ul>
     * 
     * @param ticker ticker symbol
     * @param fadeRatio TODO.md fade_ratio (default 0.3)
     * @param accelRatio TODO.md accel_ratio (default 1.5)
     * @param deltaBarsLookback number of bars to analyze (default 2)
     * @param priceDirection 1 for up, -1 for down, 0 for neutral
     * @return delta analysis result
     */
    public DeltaAnalysis analyzeDelta(String ticker, int priceDirection, 
                                      double fadeRatio, double accelRatio, 
                                      int deltaBarsLookback) {
        TickerDeltaState state = statesByTicker.get(ticker);
        if (state == null || state.bars.isEmpty()) {
            return new DeltaAnalysis(0, false, false, false, 0.0);
        }
        
        List<DeltaBar> recentBars = new ArrayList<>(state.bars);
        if (recentBars.size() < deltaBarsLookback) {
            return new DeltaAnalysis(
                state.currentBar.getCumulativeDelta(),
                false,
                false,
                false,
                0.0
            );
        }
        
        // Get last N bars for analysis
        int fromIndex = Math.max(0, recentBars.size() - deltaBarsLookback);
        List<DeltaBar> analysisBars = recentBars.subList(fromIndex, recentBars.size());
        
        // Check for decay (TODO.md 3.2): |d0| < |d1| * (1 - fade_ratio)
        boolean isDecaying = checkDecay(analysisBars, fadeRatio);
        
        // Check for divergence (TODO.md 3.2): delta reverses sign
        boolean isDiverging = checkDivergence(analysisBars, priceDirection);
        
        // Check for exponential growth (TODO.md 3.4): |d0| > |d1| * accel_ratio
        double growthRate = calculateGrowthRate(analysisBars);
        boolean isExponentialGrowth = growthRate > accelRatio;
        
        return new DeltaAnalysis(
            state.currentBar.getCumulativeDelta(),
            isDecaying,
            isDiverging,
            isExponentialGrowth,
            growthRate
        );
    }
    
    private boolean checkDecay(List<DeltaBar> bars, double fadeRatio) {
        if (bars.size() < 2) {
            return false;
        }
        
        // Get last two bars: d0 (current), d1 (previous)
        DeltaBar d1 = bars.get(bars.size() - 2);
        DeltaBar d0 = bars.get(bars.size() - 1);
        
        long absD0 = Math.abs(d0.getCumulativeDelta());
        long absD1 = Math.abs(d1.getCumulativeDelta());
        
        // TODO.md 3.2: |d0| < |d1| * (1 - fade_ratio) for same-sign deltas
        boolean sameSign = Long.signum(d0.getCumulativeDelta()) == Long.signum(d1.getCumulativeDelta());
        if (!sameSign) {
            return false; // Can't measure decay if signs differ (that's divergence)
        }
        
        double threshold = absD1 * (1.0 - fadeRatio);
        return absD0 < threshold;
    }
    
    private boolean checkDivergence(List<DeltaBar> bars, int priceDirection) {
        if (bars.size() < 2 || priceDirection == 0) {
            return false;
        }
        
        // Get last two bars
        DeltaBar d1 = bars.get(bars.size() - 2);
        DeltaBar d0 = bars.get(bars.size() - 1);
        
        // TODO.md 3.2: Divergence when delta reverses sign (d0 > 0 after d1 < 0)
        boolean reversal = (d1.getCumulativeDelta() < 0 && d0.getCumulativeDelta() > 0) ||
                           (d1.getCumulativeDelta() > 0 && d0.getCumulativeDelta() < 0);
        
        // Also check if price direction is contradicted by delta
        boolean priceContradiction = false;
        if (priceDirection > 0) {
            // Price went up, but delta is going down (negative)
            priceContradiction = d0.getCumulativeDelta() < d1.getCumulativeDelta();
        } else if (priceDirection < 0) {
            // Price went down, but delta is going up (positive)
            priceContradiction = d0.getCumulativeDelta() > d1.getCumulativeDelta();
        }
        
        return reversal || priceContradiction;
    }
    
    private double calculateGrowthRate(List<DeltaBar> bars) {
        if (bars.size() < 2) {
            return 1.0;
        }
        
        // Get last two bars
        DeltaBar d1 = bars.get(bars.size() - 2);
        DeltaBar d0 = bars.get(bars.size() - 1);
        
        long absD0 = Math.abs(d0.getCumulativeDelta());
        long absD1 = Math.abs(d1.getCumulativeDelta());
        
        if (absD1 == 0) {
            return absD0 > 0 ? Double.MAX_VALUE : 1.0;
        }
        
        // TODO.md 3.4: growth rate = |d0| / |d1|
        return (double) absD0 / absD1;
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
