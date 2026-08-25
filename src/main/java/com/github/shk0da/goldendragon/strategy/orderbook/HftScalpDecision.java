package com.github.shk0da.goldendragon.strategy.orderbook;

import com.github.shk0da.goldendragon.strategy.orderbook.CumulativeDeltaTracker.DeltaAnalysis;
import com.github.shk0da.goldendragon.strategy.orderbook.DensityAnalyzer.Density;
import com.github.shk0da.goldendragon.strategy.orderbook.DensityAnalyzer.DensityType;

import java.time.Instant;

/**
 * Makes entry/exit decisions for HFT scalping strategy.
 * 
 * <p>Implements two scenarios from the spec:
 * <ul>
 *   <li>Scenario A: Bounce from large density (counter-trend)</li>
 *   <li>Scenario B: Density breakout (impulse)</li>
 * </ul>
 */
public final class HftScalpDecision {
    
    private static final double SPREAD_MAX_PERCENT = 0.0002; // 0.02%
    private static final int BOUNCE_TP_RR = 2; // 1:2 RR
    private static final int BOUNCE_TP_RR_MAX = 3; // 1:3 RR
    private static final int BREAKOUT_MAX_BARS = 4; // Max 4 bars (1 minute)
    private static final int BREAKOUT_MAX_SECONDS = 60; // Max 60 seconds
    private static final int BREAKOUT_DENSITY_CONSUMED_PERCENT = 75; // 75-80% consumed
    
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
        
        Decision(boolean enter, boolean isLong, String reason, 
                 double takeProfit, double stopLoss, 
                 String exitReason, boolean emergencyExit) {
            this.enter = enter;
            this.isLong = isLong;
            this.reason = reason;
            this.takeProfit = takeProfit;
            this.stopLoss = stopLoss;
            this.exitReason = exitReason;
            this.emergencyExit = emergencyExit;
        }
        
        public static Decision none() {
            return new Decision(false, false, null, 0, 0, null, false);
        }
        
        public static Decision enterBounce(String reason) {
            return new Decision(true, false, reason, 0, 0, null, false);
        }
        
        public static Decision enterBreakout(String reason) {
            return new Decision(true, false, reason, 0, 0, null, false);
        }
        
        public static Decision exit(String exitReason) {
            return new Decision(false, false, null, 0, 0, exitReason, false);
        }
        
        public static Decision emergencyExit() {
            return new Decision(false, false, null, 0, 0, "emergency_exit", true);
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
    }
    
    /**
     * Evaluate entry for Scenario A: Bounce from density.
     * 
     * <p>Conditions:
     * <ul>
     *   <li>Price approaches Density_2 (within 0.05%)</li>
     *   <li>Delta decay or divergence in last 2 bars</li>
     * </ul>
     */
    public static Decision evaluateBounceEntry(
            Density density,
            double currentPrice,
            DeltaAnalysis deltaAnalysis,
            double spread,
            boolean isLong) {
        
        if (density == null || density.getType() != DensityType.ANOMALOUS) {
            return Decision.none();
        }
        
        // Check spread
        double spreadPercent = spread / currentPrice;
        if (spreadPercent > SPREAD_MAX_PERCENT) {
            return Decision.none();
        }
        
        // Check density proximity
        double distance = Math.abs(currentPrice - density.getPrice());
        double distancePercent = distance / currentPrice;
        if (distancePercent > 0.0005) { // 0.05%
            return Decision.none();
        }
        
        // Check delta filter: decay or divergence
        if (!deltaAnalysis.isDecaying() && !deltaAnalysis.isDiverging()) {
            return Decision.none();
        }
        
        // Calculate TP and SL
        double tp, sl;
        double stopDistance = (currentPrice - density.getPrice()) * (isLong ? -1 : 1);
        
        if (isLong) {
            // For long: SL 1 tick behind density, TP with RR 1:2 or 1:3
            sl = density.getPrice() - (spread / 2); // 1 tick behind density
            tp = density.getPrice() + Math.abs(currentPrice - density.getPrice()) * BOUNCE_TP_RR_MAX;
        } else {
            // For short: SL 1 tick behind density, TP with RR 1:2 or 1:3
            sl = density.getPrice() + (spread / 2);
            tp = density.getPrice() - Math.abs(currentPrice - density.getPrice()) * BOUNCE_TP_RR_MAX;
        }
        
        String reason = String.format(
            "Bounce entry: density=%.2f type=ANOMALOUS deltaDecay=%b deltaDivergence=%b",
            density.getPrice(),
            deltaAnalysis.isDecaying(),
            deltaAnalysis.isDiverging()
        );
        
        return Decision.enterBounce(reason);
    }
    
    /**
     * Evaluate entry for Scenario B: Density breakout.
     * 
     * <p>Conditions:
     * <ul>
     *   <li>Price sticks to Density_1 or Density_2 (2-3 bars)</li>
     *   <li>Exponential delta growth</li>
     *   <li>Density volume consumed 75-80%</li>
     * </ul>
     */
    public static Decision evaluateBreakoutEntry(
            Density density,
            double currentPrice,
            double originalDensityVolume,
            double currentDensityVolume,
            DeltaAnalysis deltaAnalysis,
            double spread,
            boolean isLong) {
        
        if (density == null) {
            return Decision.none();
        }
        
        // Check spread
        double spreadPercent = spread / currentPrice;
        if (spreadPercent > SPREAD_MAX_PERCENT) {
            return Decision.none();
        }
        
        // Check density consumption (75-80% consumed)
        double consumedPercent = 1.0 - (currentDensityVolume / originalDensityVolume);
        if (consumedPercent < 0.75) {
            return Decision.none();
        }
        
        // Check exponential delta growth
        if (!deltaAnalysis.isExponentialGrowth()) {
            return Decision.none();
        }
        
        // Calculate TP and SL
        double tp, sl;
        
        if (isLong) {
            // For long breakout: SL behind broken density, TP at first impulse
            sl = density.getPrice() - (spread / 2); // Breakout level
            tp = currentPrice + (currentPrice - density.getPrice()) * BOUNCE_TP_RR;
        } else {
            // For short breakout: SL behind broken density, TP at first impulse
            sl = density.getPrice() + (spread / 2);
            tp = currentPrice - (density.getPrice() - currentPrice) * BOUNCE_TP_RR;
        }
        
        String reason = String.format(
            "Breakout entry: density=%.2f consumed=%.0f%% exponentialGrowth=%b",
            density.getPrice(),
            consumedPercent * 100,
            deltaAnalysis.isExponentialGrowth()
        );
        
        return Decision.enterBreakout(reason);
    }
    
    /**
     * Evaluate exit conditions.
     * 
     * <p>Exits:
     * <ul>
     *   <li>Take Profit: TP/SL levels hit</li>
     *   <li>Emergency exit: Density disappears</li>
     *   <li>Time stop: Max hold time exceeded</li>
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
            double originalDensityVolume) {
        
        // Check TP
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
        
        // Check time stop for breakout
        long holdSeconds = java.time.Duration.between(entryTime, Instant.now()).getSeconds();
        if (holdSeconds > BREAKOUT_MAX_SECONDS) {
            return Decision.exit("time_stop");
        }
        
        // Check emergency exit: density disappeared
        if (density != null && originalDensityVolume > 0) {
            double remainingPercent = currentDensityVolume / originalDensityVolume;
            if (remainingPercent < 0.1) { // 90% gone
                return Decision.emergencyExit();
            }
        }
        
        return Decision.none();
    }
}
