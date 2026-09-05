package com.github.shk0da.goldendragon.strategy.candle;

/**
 * Entry decision from a candle-based signal.
 */
public class CandleEntryDecision {
    private final boolean shouldEnter;
    private final String reason;
    private final double quality;

    private CandleEntryDecision(boolean shouldEnter, String reason, double quality) {
        this.shouldEnter = shouldEnter;
        this.reason = reason;
        this.quality = quality;
    }

    /** Create a decision to enter with given quality. */
    public static CandleEntryDecision enter(String reason, double quality) {
        return new CandleEntryDecision(true, reason, Math.max(0.0, Math.min(1.0, quality)));
    }

    /** Create a decision not to enter. */
    public static CandleEntryDecision none() {
        return new CandleEntryDecision(false, null, 0.0);
    }

    public boolean shouldEnter() {
        return shouldEnter;
    }

    public String reason() {
        return reason;
    }

    public double quality() {
        return quality;
    }
}
