package com.github.shk0da.goldendragon.strategy.orderbook;

/**
 * Adjusts entry thresholds based on rolling win rate from {@link SignalPerformanceTracker}.
 *
 * <p>When win rate is high (above {@code highWinRateThreshold}), entry criteria are loosened
 * to capture more trades. When win rate is low (below {@code lowWinRateThreshold}), criteria
 * are tightened to reduce losses. Between the thresholds, adjustment is linearly interpolated.
 *
 * <p>All adjustments use exponential moving average (EMA) smoothing to prevent abrupt parameter
 * changes. Output values are clamped to configurable min/max bounds.
 */
public final class AdaptiveParameters {

    private final SignalPerformanceTracker performanceTracker;
    private final Config config;

    private double emaDeltaFactor = 1.0;
    private double emaDensityFactor = 1.0;
    private double emaConfidenceFactor = 1.0;

    public AdaptiveParameters(SignalPerformanceTracker performanceTracker, Config config) {
        if (performanceTracker == null) {
            throw new IllegalArgumentException("performanceTracker must not be null");
        }
        if (config == null) {
            throw new IllegalArgumentException("config must not be null");
        }
        this.performanceTracker = performanceTracker;
        this.config = config;
    }

    /**
     * Update EMA-smoothed adjustment factors using the current win rate for a signal.
     *
     * @param signalId signal identifier to read win rate from
     */
    public synchronized void update(String signalId) {
        double winRate = performanceTracker.getWinRate(signalId);
        int tradeCount = performanceTracker.getTradeCount(signalId);

        if (tradeCount < config.minTradesForAdaptation) {
            return;
        }

        double targetDeltaFactor = computeTargetFactor(winRate, config.loosenDeltaFactor, config.tightenDeltaFactor);
        double targetDensityFactor = computeTargetFactor(winRate, config.loosenDensityFactor, config.tightenDensityFactor);
        double targetConfidenceFactor = computeTargetFactor(winRate, config.loosenConfidenceFactor, config.tightenConfidenceFactor);

        emaDeltaFactor = applyEma(emaDeltaFactor, targetDeltaFactor);
        emaDensityFactor = applyEma(emaDensityFactor, targetDensityFactor);
        emaConfidenceFactor = applyEma(emaConfidenceFactor, targetConfidenceFactor);
    }

    /**
     * Get adjusted entry thresholds for a given signal.
     *
     * @param signalId signal identifier to read win rate from
     * @return adjusted thresholds with EMA smoothing and bounds clamping applied
     */
    public synchronized AdjustedThresholds getAdjustedThresholds(String signalId) {
        double rawMinDelta = config.baseMinDelta * emaDeltaFactor;
        double rawMinDensity = config.baseMinDensity * emaDensityFactor;
        double rawConfidence = config.baseConfidenceFloor * emaConfidenceFactor;

        double minDelta = clamp(rawMinDelta, config.minDeltaMin, config.minDeltaMax);
        double minDensity = clamp(rawMinDensity, config.minDensityMin, config.minDensityMax);
        double confidence = clamp(rawConfidence, config.confidenceFloorMin, config.confidenceFloorMax);

        double winRate = performanceTracker.getWinRate(signalId);
        return new AdjustedThresholds(minDelta, minDensity, confidence, winRate, emaDeltaFactor);
    }

    /**
     * Reset all EMA state to neutral (factor = 1.0).
     */
    public synchronized void reset() {
        emaDeltaFactor = 1.0;
        emaDensityFactor = 1.0;
        emaConfidenceFactor = 1.0;
    }

    /** Current EMA factor for min delta threshold (for diagnostics). */
    public synchronized double getDeltaFactor() {
        return emaDeltaFactor;
    }

    /** Current EMA factor for min density threshold (for diagnostics). */
    public synchronized double getDensityFactor() {
        return emaDensityFactor;
    }

    /** Current EMA factor for confidence floor (for diagnostics). */
    public synchronized double getConfidenceFactor() {
        return emaConfidenceFactor;
    }

    /**
     * Compute target adjustment factor based on win rate position relative to thresholds.
     *
     * <p>Above high threshold: loosen factor (less than 1.0 lowers thresholds).
     * Below low threshold: tighten factor (greater than 1.0 raises thresholds).
     * Between: linear interpolation.
     */
    private double computeTargetFactor(double winRate, double loosenFactor, double tightenFactor) {
        if (winRate >= config.highWinRateThreshold) {
            return loosenFactor;
        }
        if (winRate <= config.lowWinRateThreshold) {
            return tightenFactor;
        }
        double range = config.highWinRateThreshold - config.lowWinRateThreshold;
        if (range <= 0.0) {
            return 1.0;
        }
        double position = (winRate - config.lowWinRateThreshold) / range;
        return tightenFactor + position * (loosenFactor - tightenFactor);
    }

    private double applyEma(double currentEma, double target) {
        return config.emaAlpha * target + (1.0 - config.emaAlpha) * currentEma;
    }

    private static double clamp(double value, double min, double max) {
        return Math.max(min, Math.min(max, value));
    }

    /** Adjusted threshold values returned by {@link #getAdjustedThresholds}. */
    public static final class AdjustedThresholds {

        private final double minDeltaThreshold;
        private final double minDensityThreshold;
        private final double confidenceFloor;
        private final double currentWinRate;
        private final double deltaFactor;

        AdjustedThresholds(
                double minDeltaThreshold,
                double minDensityThreshold,
                double confidenceFloor,
                double currentWinRate,
                double deltaFactor) {
            this.minDeltaThreshold = minDeltaThreshold;
            this.minDensityThreshold = minDensityThreshold;
            this.confidenceFloor = confidenceFloor;
            this.currentWinRate = currentWinRate;
            this.deltaFactor = deltaFactor;
        }

        public double getMinDeltaThreshold() {
            return minDeltaThreshold;
        }

        public double getMinDensityThreshold() {
            return minDensityThreshold;
        }

        public double getConfidenceFloor() {
            return confidenceFloor;
        }

        public double getCurrentWinRate() {
            return currentWinRate;
        }

        public double getDeltaFactor() {
            return deltaFactor;
        }
    }

    /** Configuration for adaptive parameter adjustment. */
    public static final class Config {

        private final double baseMinDelta;
        private final double baseMinDensity;
        private final double baseConfidenceFloor;

        private final double highWinRateThreshold;
        private final double lowWinRateThreshold;
        private final double emaAlpha;
        private final int minTradesForAdaptation;

        private final double loosenDeltaFactor;
        private final double tightenDeltaFactor;
        private final double minDeltaMin;
        private final double minDeltaMax;

        private final double loosenDensityFactor;
        private final double tightenDensityFactor;
        private final double minDensityMin;
        private final double minDensityMax;

        private final double loosenConfidenceFactor;
        private final double tightenConfidenceFactor;
        private final double confidenceFloorMin;
        private final double confidenceFloorMax;

        private Config(Builder builder) {
            this.baseMinDelta = builder.baseMinDelta;
            this.baseMinDensity = builder.baseMinDensity;
            this.baseConfidenceFloor = builder.baseConfidenceFloor;
            this.highWinRateThreshold = builder.highWinRateThreshold;
            this.lowWinRateThreshold = builder.lowWinRateThreshold;
            this.emaAlpha = builder.emaAlpha;
            this.minTradesForAdaptation = builder.minTradesForAdaptation;
            this.loosenDeltaFactor = builder.loosenDeltaFactor;
            this.tightenDeltaFactor = builder.tightenDeltaFactor;
            this.minDeltaMin = builder.minDeltaMin;
            this.minDeltaMax = builder.minDeltaMax;
            this.loosenDensityFactor = builder.loosenDensityFactor;
            this.tightenDensityFactor = builder.tightenDensityFactor;
            this.minDensityMin = builder.minDensityMin;
            this.minDensityMax = builder.minDensityMax;
            this.loosenConfidenceFactor = builder.loosenConfidenceFactor;
            this.tightenConfidenceFactor = builder.tightenConfidenceFactor;
            this.confidenceFloorMin = builder.confidenceFloorMin;
            this.confidenceFloorMax = builder.confidenceFloorMax;
        }

        public static Builder builder() {
            return new Builder();
        }

        /** Builder for {@link Config} with sensible defaults. */
        public static final class Builder {

            private double baseMinDelta = 2.5;
            private double baseMinDensity = 0.25;
            private double baseConfidenceFloor = 0.5;

            private double highWinRateThreshold = 60.0;
            private double lowWinRateThreshold = 40.0;
            private double emaAlpha = 0.15;
            private int minTradesForAdaptation = 10;

            private double loosenDeltaFactor = 0.7;
            private double tightenDeltaFactor = 1.3;
            private double minDeltaMin = 1.0;
            private double minDeltaMax = 5.0;

            private double loosenDensityFactor = 0.75;
            private double tightenDensityFactor = 1.25;
            private double minDensityMin = 0.10;
            private double minDensityMax = 0.50;

            private double loosenConfidenceFactor = 0.8;
            private double tightenConfidenceFactor = 1.2;
            private double confidenceFloorMin = 0.2;
            private double confidenceFloorMax = 0.9;

            private Builder() {
            }

            public Builder baseMinDelta(double value) {
                this.baseMinDelta = value;
                return this;
            }

            public Builder baseMinDensity(double value) {
                this.baseMinDensity = value;
                return this;
            }

            public Builder baseConfidenceFloor(double value) {
                this.baseConfidenceFloor = value;
                return this;
            }

            public Builder highWinRateThreshold(double value) {
                this.highWinRateThreshold = value;
                return this;
            }

            public Builder lowWinRateThreshold(double value) {
                this.lowWinRateThreshold = value;
                return this;
            }

            public Builder emaAlpha(double value) {
                this.emaAlpha = value;
                return this;
            }

            public Builder minTradesForAdaptation(int value) {
                this.minTradesForAdaptation = value;
                return this;
            }

            public Builder loosenDeltaFactor(double value) {
                this.loosenDeltaFactor = value;
                return this;
            }

            public Builder tightenDeltaFactor(double value) {
                this.tightenDeltaFactor = value;
                return this;
            }

            public Builder minDeltaBounds(double min, double max) {
                this.minDeltaMin = min;
                this.minDeltaMax = max;
                return this;
            }

            public Builder loosenDensityFactor(double value) {
                this.loosenDensityFactor = value;
                return this;
            }

            public Builder tightenDensityFactor(double value) {
                this.tightenDensityFactor = value;
                return this;
            }

            public Builder minDensityBounds(double min, double max) {
                this.minDensityMin = min;
                this.minDensityMax = max;
                return this;
            }

            public Builder loosenConfidenceFactor(double value) {
                this.loosenConfidenceFactor = value;
                return this;
            }

            public Builder tightenConfidenceFactor(double value) {
                this.tightenConfidenceFactor = value;
                return this;
            }

            public Builder confidenceFloorBounds(double min, double max) {
                this.confidenceFloorMin = min;
                this.confidenceFloorMax = max;
                return this;
            }

            public Config build() {
                return new Config(this);
            }
        }
    }
}
