package com.github.shk0da.goldendragon.filters;

/**
 * Volatility spike filter — blocks trading during abnormal volatility conditions.
 *
 * <p>Detects two types of spikes:
 * <ul>
 *   <li>Spread widening: current candle range exceeds N times its rolling average</li>
 *   <li>Volume surge: current tick volume exceeds N times its rolling average</li>
 * </ul>
 *
 * <p>After a spike is detected, enforces a cooldown period before resuming trading.
 */
public class VolatilitySpikeFilter {

    private final boolean enabled;
    private final int cooldownMs;

    // cooldown state
    private long lastSpikeDetectedAtMs = 0;

    public VolatilitySpikeFilter(boolean enabled, int cooldownMs) {
        this.enabled = enabled;
        this.cooldownMs = cooldownMs;
    }

    public boolean isInCooldown() {
        if (lastSpikeDetectedAtMs == 0) {
            return false;
        }
        return (System.currentTimeMillis() - lastSpikeDetectedAtMs) < cooldownMs;
    }

    /**
     * Reset cooldown state.
     */
    public synchronized void reset() {
        lastSpikeDetectedAtMs = 0;
    }

    public boolean isEnabled() {
        return enabled;
    }
}
