package com.github.shk0da.goldendragon.filters;

import com.github.shk0da.goldendragon.model.Candle;
import com.github.shk0da.goldendragon.utils.LoggingUtils;
import java.util.List;

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
    private final double spreadSpikeMultiplier;
    private final double volumeSpikeMultiplier;
    private final int cooldownMs;
    private final int lookbackPeriod;

    // cooldown state
    private long lastSpikeDetectedAtMs = 0;

    public VolatilitySpikeFilter(boolean enabled) {
        this(enabled, 3.0, 4.0, 60_000, 20);
    }

    public VolatilitySpikeFilter(
            boolean enabled,
            double spreadSpikeMultiplier,
            double volumeSpikeMultiplier,
            int cooldownMs,
            int lookbackPeriod) {
        this.enabled = enabled;
        this.spreadSpikeMultiplier = spreadSpikeMultiplier;
        this.volumeSpikeMultiplier = volumeSpikeMultiplier;
        this.cooldownMs = cooldownMs;
        this.lookbackPeriod = lookbackPeriod;
    }

    /**
     * Check if opening new trades is allowed.
     *
     * @return true if trading is allowed, false if volatility spike detected
     */
    public boolean canTrade(List<Candle> candles, double currentPrice) {
        return getBlockReason(candles, currentPrice) == null;
    }

    /**
     * Returns the reason why trading is blocked, or null if trading is allowed.
     *
     * @return block reason string with spike metrics, or null
     */
    public String getBlockReason(List<Candle> candles, double currentPrice) {
        if (!enabled) {
            return null;
        }

        if (candles == null || candles.size() < lookbackPeriod + 1) {
            return null;
        }

        if (isInCooldown()) {
            long remainingMs = cooldownMs - (System.currentTimeMillis() - lastSpikeDetectedAtMs);
            return "COOLDOWN_ACTIVE_" + (remainingMs / 1000) + "s";
        }

        Candle current = candles.get(candles.size() - 1);

        if (isSpreadSpike(candles, current)) {
            recordSpike(candles, current, "SPREAD_SPIKE");
            return "SPREAD_SPIKE";
        }

        if (isVolumeSpike(candles, current)) {
            recordSpike(candles, current, "VOLUME_SPIKE");
            return "VOLUME_SPIKE";
        }

        return null;
    }

    private boolean isSpreadSpike(List<Candle> candles, Candle current) {
        double avgSpread = calculateAverageSpread(candles);
        if (avgSpread <= 0.0) {
            return false;
        }

        double currentSpread = current.high - current.low;
        return currentSpread > avgSpread * spreadSpikeMultiplier;
    }

    private boolean isVolumeSpike(List<Candle> candles, Candle current) {
        long avgVolume = calculateAverageVolume(candles);
        if (avgVolume <= 0) {
            return false;
        }

        return current.volume > avgVolume * volumeSpikeMultiplier;
    }

    public boolean isInCooldown() {
        if (lastSpikeDetectedAtMs == 0) {
            return false;
        }
        return (System.currentTimeMillis() - lastSpikeDetectedAtMs) < cooldownMs;
    }

    private void recordSpike(List<Candle> candles, Candle current, String spikeType) {
        lastSpikeDetectedAtMs = System.currentTimeMillis();

        double avgSpread = calculateAverageSpread(candles);
        long avgVolume = calculateAverageVolume(candles);
        double currentSpread = current.high - current.low;

        String spreadRatio = avgSpread > 0
                ? String.format("%.1fx", currentSpread / avgSpread)
                : "n/a";
        String volumeRatio = avgVolume > 0
                ? String.format("%.1fx", (double) current.volume / avgVolume)
                : "n/a";

        LoggingUtils.log(
                "VolatilitySpikeFilter: " + spikeType
                        + " detected, spread=" + String.format("%.4f", currentSpread)
                        + " (avg=" + String.format("%.4f", avgSpread) + ", " + spreadRatio + ")"
                        + ", volume=" + current.volume
                        + " (avg=" + avgVolume + ", " + volumeRatio + ")"
                        + ", cooldown=" + (cooldownMs / 1000) + "s");
    }

    private double calculateAverageSpread(List<Candle> candles) {
        int end = candles.size() - 1;
        int start = Math.max(0, end - lookbackPeriod);
        int count = 0;
        double sum = 0.0;

        for (int i = start; i < end; i++) {
            Candle c = candles.get(i);
            if (c.close > 0) {
                sum += (c.high - c.low) / c.close;
                count++;
            }
        }

        return count > 0 ? sum / count : 0.0;
    }

    private long calculateAverageVolume(List<Candle> candles) {
        int end = candles.size() - 1;
        int start = Math.max(0, end - lookbackPeriod);
        int count = 0;
        long sum = 0;

        for (int i = start; i < end; i++) {
            sum += candles.get(i).volume;
            count++;
        }

        return count > 0 ? sum / count : 0;
    }

    /** Reset cooldown state. */
    public synchronized void reset() {
        lastSpikeDetectedAtMs = 0;
    }

    public boolean isEnabled() {
        return enabled;
    }

    public double getSpreadSpikeMultiplier() {
        return spreadSpikeMultiplier;
    }

    public double getVolumeSpikeMultiplier() {
        return volumeSpikeMultiplier;
    }

    public int getCooldownMs() {
        return cooldownMs;
    }

    public int getLookbackPeriod() {
        return lookbackPeriod;
    }
}
