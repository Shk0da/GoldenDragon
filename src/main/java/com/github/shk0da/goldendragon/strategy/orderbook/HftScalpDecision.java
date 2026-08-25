package com.github.shk0da.goldendragon.strategy.orderbook;

import com.github.shk0da.goldendragon.config.OrderBookScalpConfig;
import com.github.shk0da.goldendragon.strategy.orderbook.CumulativeDeltaTracker.DeltaAnalysis;
import com.github.shk0da.goldendragon.strategy.orderbook.DensityAnalyzer.Density;
import com.github.shk0da.goldendragon.strategy.orderbook.DensityAnalyzer.DensityType;

import java.time.Instant;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Makes entry/exit decisions for HFT scalping strategy.
 * 
 * <p>Implements two scenarios from the spec:
 * <ul>
 *   <li><b>Scenario A (Bounce):</b> Counter-trend bounce from large density with delta confirmation</li>
 *   <li><b>Scenario B (Breakout):</b> Impulse breakout when density is consumed</li>
 * </ul>
 * 
 * <p><b>Priority:</b> Scenario A has priority over Scenario B as per specification.
 * This means if both signals trigger simultaneously, Scenario A entry is preferred.
 */
public final class HftScalpDecision {
    
    private static final double SPREAD_MAX_PERCENT = 0.0002; // 0.02%
    private static final int BOUNCE_TP_RR = 2; // 1:2 RR
    private static final int BOUNCE_TP_RR_MAX = 3; // 1:3 RR
    private static final int BREAKOUT_MAX_BARS = 4; // Max 4 bars (1 minute)
    private static final int BREAKOUT_MAX_SECONDS = 60; // Max 60 seconds
    private static final int BREAKOUT_DENSITY_CONSUMED_PERCENT = 75; // 75-80% consumed
    
    // Configurable parameters (default values - overridden by config)
    private static int stickinessBarsThreshold = 2;
    private static int minNetProfitTicks = 2;
    private static double fadeRatio = 0.3;
    private static double accelRatio = 1.5;
    private static double eatenRatioEntry = 0.75;
    private static double tickSizeForCalc = 0.0001;
    private static double densityPullExitRatio = 0.5;
    
    /**
     * Tracks density stickiness for Scenario B breakout detection.
     */
    private static final class DensityStickinessTracker {
        private final String ticker;
        private final double densityPrice;
        private final boolean isBid;
        private int stickyBars;
        private Instant lastBarTime;
        
        DensityStickinessTracker(String ticker, double densityPrice, boolean isBid) {
            this.ticker = ticker;
            this.densityPrice = densityPrice;
            this.isBid = isBid;
            this.stickyBars = 0;
            this.lastBarTime = Instant.now();
        }
        
        void update(Instant barTime, double currentPrice) {
            // Check if still near same density
            double distance = Math.abs(currentPrice - densityPrice);
            double distancePercent = distance / currentPrice;
            
            // Consider sticky if within 0.05% (same as bounce proximity)
            if (distancePercent <= 0.0005) {
                if (barTime.getEpochSecond() > lastBarTime.getEpochSecond()) {
                    stickyBars++;
                    lastBarTime = barTime;
                }
            } else {
                // Reset if price moved away
                stickyBars = 0;
                lastBarTime = barTime;
            }
        }
        
        boolean isSticky() {
            return stickyBars >= stickinessBarsThreshold;
        }
        
        int getStickyBars() {
            return stickyBars;
        }
    }
    
    private final ConcurrentHashMap<String, DensityStickinessTracker> stickinessTrackers = new ConcurrentHashMap<>();
    
    /**
     * Decision result for entry/exit.
     */
    public static final class Decision {
        private final boolean enter;
        private final boolean isLong;
        private final String reason;
        private final double takeProfit;
        private final double stopLoss;
        private final String exitReason;
        private final boolean emergencyExit;
        private final boolean isLimitOrder; // true for maker (Scenario A), false for taker (Scenario B)
        private final String scenario; // "bounce" or "breakout"
        
        Decision(boolean enter, boolean isLong, String reason, 
                 double takeProfit, double stopLoss, 
                 String exitReason, boolean emergencyExit,
                 boolean isLimitOrder, String scenario) {
            this.enter = enter;
            this.isLong = isLong;
            this.reason = reason;
            this.takeProfit = takeProfit;
            this.stopLoss = stopLoss;
            this.exitReason = exitReason;
            this.emergencyExit = emergencyExit;
            this.isLimitOrder = isLimitOrder;
            this.scenario = scenario;
        }
        
        public static Decision none() {
            return new Decision(false, false, null, 0, 0, null, false, false, null);
        }
        
        public static Decision enterBounce(String reason, double tp, double sl) {
            return new Decision(true, false, reason, tp, sl, null, false, true, "bounce");
        }
        
        public static Decision enterBreakout(String reason, double tp, double sl) {
            return new Decision(true, false, reason, tp, sl, null, false, false, "breakout");
        }
        
        public static Decision exit(String exitReason) {
            return new Decision(false, false, null, 0, 0, exitReason, false, false, null);
        }
        
        public static Decision emergencyExit() {
            return new Decision(false, false, null, 0, 0, "emergency_exit", true, false, null);
        }
        
        public boolean isEnter() {
            return enter;
        }
        
        public boolean isExit() {
            return exitReason != null;
        }
        
        public boolean isEmergencyExit() {
            return emergencyExit;
        }
        
        public String getReason() {
            return reason;
        }
        
        public String getExitReason() {
            return exitReason;
        }
        
        public boolean isLimitOrder() {
            return isLimitOrder;
        }
        
        public String getScenario() {
            return scenario;
        }
    }
    
    /**
     * Configure HftScalpDecision with parameters from OrderBookScalpConfig.
     */
    public static void configure(OrderBookScalpConfig config) {
        stickinessBarsThreshold = config.getStickBars();
        minNetProfitTicks = config.getMinNetProfitTicks();
        fadeRatio = config.getFadeRatio();
        accelRatio = config.getAccelRatio();
        eatenRatioEntry = config.getEatenRatioEntry();
        tickSizeForCalc = config.getTickSize() > 0 ? config.getTickSize() : 0.0001;
        densityPullExitRatio = config.getDensityPullExit();
    }
    
    /**
     * Calculate tick size for instrument (typical values).
     * In real implementation, this would query instrument characteristics.
     */
    public static double calculateTickSize(double price) {
        // Approximate tick size based on price level
        if (price > 10000) return 0.1;
        if (price > 1000) return 0.01;
        if (price > 100) return 0.001;
        return 0.0001;
    }
    
    /**
     * Evaluate entry for Scenario A: Bounce from density.
     * 
     * <p>Priority: Scenario A has higher priority than Scenario B.
     * If both trigger, Scenario A should be taken first.
     * 
     * <p>Conditions:
     * <ul>
     *   <li>Price approaches Density_2 (within 0.05%)</li>
     *   <li>Delta decay or divergence in last 2 bars (TODO.md 3.2)</li>
     *   <li>Spread protection (max 0.02%)</li>
     *   <li>Minimum net profit in ticks (TODO.md 2: min_net_profit_ticks)</li>
     * </ul>
     * 
     * <p>Entry: Limit order (Maker) 1 tick before density
     * <p>SL: 1 tick behind density
     * <p>TP: RR 1:2 to 1:3
     */
    public static Decision evaluateBounceEntry(
            Density density,
            double currentPrice,
            DeltaAnalysis deltaAnalysis,
            double spread,
            boolean isLong,
            int minNetProfitTicks,
            double tickSize) {
        
        if (density == null || density.getType() != DensityType.ANOMALOUS) {
            return Decision.none();
        }
        
        // Check spread protection (max 0.02%)
        double spreadPercent = spread / currentPrice;
        if (spreadPercent > SPREAD_MAX_PERCENT) {
            return Decision.none();
        }
        
        // Check density proximity (within 0.05%)
        double distance = Math.abs(currentPrice - density.getPrice());
        double distancePercent = distance / currentPrice;
        if (distancePercent > 0.0005) {
            return Decision.none();
        }
        
        // Check delta filter: decay or divergence (TODO.md 3.2)
        if (!deltaAnalysis.isDecaying() && !deltaAnalysis.isDiverging()) {
            return Decision.none();
        }
        
        // Calculate tick size for precise placement
        if (tickSize <= 0) tickSize = calculateTickSize(currentPrice);
        
        // Calculate TP/SL with tick precision
        double distanceToDensity = Math.abs(currentPrice - density.getPrice());
        double tp, sl;
        
        if (isLong) {
            // Long: entry before density, SL behind density, TP with RR 1:2-1:3
            sl = density.getPrice() - tickSize; // 1 tick behind density
            tp = density.getPrice() + distanceToDensity * BOUNCE_TP_RR_MAX;
        } else {
            // Short: entry before density, SL behind density, TP with RR 1:2-1:3
            sl = density.getPrice() + tickSize;
            tp = density.getPrice() - distanceToDensity * BOUNCE_TP_RR_MAX;
        }
        
        // Check minimum net profit (TODO.md: min_net_profit_ticks)
        double profitDistance = isLong ? (tp - currentPrice) : (currentPrice - tp);
        double minProfitInTicks = minNetProfitTicks * tickSize;
        if (profitDistance < minProfitInTicks) {
            return Decision.none();
        }
        
        String reason = String.format(
            "Bounce entry (Scenario A): density=%.2f type=ANOMALOUS " +
            "deltaDecay=%b deltaDivergence=%b tickSize=%.4f minProfit=%d ticks",
            density.getPrice(),
            deltaAnalysis.isDecaying(),
            deltaAnalysis.isDiverging(),
            tickSize,
            minNetProfitTicks
        );
        
        return Decision.enterBounce(reason, tp, sl);
    }
    
    /**
     * Evaluate entry for Scenario B: Density breakout.
     * 
     * <p>Conditions:
     * <ul>
     *   <li>Price sticks to density for stickBars (configurable) bars</li>
     *   <li>Exponential delta growth in breakout direction (TODO.md 3.4)</li>
     *   <li>Density volume consumed >= eaten_ratio_entry (TODO.md 3.5)</li>
     *   <li>Spread protection (max 0.02%)</li>
     * </ul>
     * 
     * <p>Entry: Market order (Taker) when density consumed
     * <p>SL: Behind broken density level (mirror support/resistance)
     * <p>TP: 1-minute time stop or impulse exhaustion
     */
    public Decision evaluateBreakoutEntry(
            String ticker,
            Density density,
            double currentPrice,
            double originalDensityVolume,
            double currentDensityVolume,
            DeltaAnalysis deltaAnalysis,
            double spread,
            boolean isLong,
            Instant barTime) {
        
        if (density == null) {
            return Decision.none();
        }
        
        // Update stickiness tracker for this ticker
        DensityStickinessTracker tracker = stickinessTrackers.computeIfAbsent(
            ticker + "_" + (isLong ? "long" : "short") + "_" + (int)density.getPrice(),
            k -> new DensityStickinessTracker(ticker, density.getPrice(), density.isBid())
        );
        tracker.update(barTime, currentPrice);
        
        // Check spread protection (max 0.02%)
        double spreadPercent = spread / currentPrice;
        if (spreadPercent > SPREAD_MAX_PERCENT) {
            return Decision.none();
        }
        
        // Check price stickiness (configurable stickBars threshold)
        if (!tracker.isSticky()) {
            return Decision.none();
        }
        
        // Check density consumption (TODO.md 3.5: eaten >= eaten_ratio_entry)
        double eatenRatio = 1.0 - (currentDensityVolume / originalDensityVolume);
        if (eatenRatio < eatenRatioEntry) {
            return Decision.none();
        }
        
        // Check exponential delta growth (TODO.md 3.4: |d0| > |d1| * accel_ratio)
        if (!deltaAnalysis.isExponentialGrowth()) {
            return Decision.none();
        }
        
        // Calculate tick size
        double tickSize = tickSizeForCalc > 0 ? tickSizeForCalc : calculateTickSize(currentPrice);
        
        // Calculate TP/SL for breakout
        double tp, sl;
        
        if (isLong) {
            // Long breakout: SL behind broken density, TP at impulse
            sl = density.getPrice() - tickSize; // Mirror level
            tp = currentPrice + (currentPrice - density.getPrice()) * BOUNCE_TP_RR;
        } else {
            // Short breakout: SL behind broken density
            sl = density.getPrice() + tickSize;
            tp = currentPrice - (density.getPrice() - currentPrice) * BOUNCE_TP_RR;
        }
        
        String reason = String.format(
            "Breakout entry (Scenario B): density=%.2f eatenRatio=%.0f%% " +
            "stickyBars=%d exponentialGrowth=%b growthRate=%.2f tickSize=%.4f",
            density.getPrice(),
            eatenRatio * 100,
            tracker.getStickyBars(),
            deltaAnalysis.isExponentialGrowth(),
            deltaAnalysis.getGrowthRate(),
            tickSize
        );
        
        return Decision.enterBreakout(reason, tp, sl);
    }
    
    /**
     * Evaluate exit conditions.
     * 
     * <p>Exits:
     * <ul>
     *   <li>Take Profit: TP/SL levels hit</li>
     *   <li>Emergency exit: Density disappears (spoofing detection)</li>
     *   <li>Time stop: Max hold time exceeded (Scenario B)</li>
     *   <li>Breakout decay: delta stopped growing for 2 bars (Scenario B)</li>
     *   <li>Density pull exit: density decreased more than density_pull_exit (spoofing)</li>
     * </ul>
     * 
     * <p>Spoofing vs Eating (TODO.md 3.6):
     * <ul>
     *   <li><b>Eating:</b> density volume decreases SYNCHRONOUSLY with trade volume at that price</li>
     *   <li><b>Spoofing:</b> density volume decreases WITHOUT corresponding trades → emergency exit</li>
     * </ul>
     */
    public static Decision evaluateExit(
            boolean isLong,
            double currentPrice,
            double entryPrice,
            double takeProfit,
            double stopLoss,
            Instant entryTime,
            Density density,
            double currentDensityVolume,
            double originalDensityVolume,
            double densityPullExitRatio) {
        
        // Check TP/SL
        if (isLong) {
            if (currentPrice >= takeProfit) {
                return Decision.exit("take_profit");
            }
            if (currentPrice <= stopLoss) {
                return Decision.exit("stop_loss");
            }
        } else {
            if (currentPrice <= takeProfit) {
                return Decision.exit("take_profit");
            }
            if (currentPrice >= stopLoss) {
                return Decision.exit("stop_loss");
            }
        }
        
        // Check time stop for Scenario B (max 1 minute)
        long holdSeconds = java.time.Duration.between(entryTime, Instant.now()).getSeconds();
        if (holdSeconds > BREAKOUT_MAX_SECONDS) {
            return Decision.exit("time_stop");
        }
        
        // Check spoofing vs eating (TODO.md 3.6)
        if (density != null && originalDensityVolume > 0) {
            double remainingPercent = currentDensityVolume / originalDensityVolume;
            
            // Spoofing: density disappeared rapidly (> density_pull_exit ratio)
            if (remainingPercent < (1.0 - densityPullExitRatio)) {
                return Decision.emergencyExit();
            }
        }
        
        return Decision.none();
    }
    
    /**
     * Evaluate breakout decay exit (TODO.md Section 4, item 122).
     * Exit position when delta stops growing for 2 bars in Scenario B.
     * 
     * @param deltaAnalysis current delta analysis
     * @return exit decision if delta has decayed
     */
    public static Decision evaluateBreakoutDecayExit(DeltaAnalysis deltaAnalysis) {
        // If delta is decaying, exit breakout position
        if (deltaAnalysis.isDecaying()) {
            return Decision.exit("delta_decay_exit");
        }
        
        return Decision.none();
    }
    
    /**
     * Reset stickiness tracker for ticker.
     */
    public void resetTicker(String ticker) {
        stickinessTrackers.keySet().removeIf(key -> key.startsWith(ticker));
    }
}
