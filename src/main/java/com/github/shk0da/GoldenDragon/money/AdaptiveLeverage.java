package com.github.shk0da.goldendragon.money;

/** Computes entry leverage dynamically from market regime, volatility and risk state. */
public final class AdaptiveLeverage {

    private AdaptiveLeverage() {}

    /** Inputs for a single entry decision. */
    public static final class Context {

        private final int maxLeverage;
        private final int minLeverage;
        private final double adx;
        private final double atr;
        private final double avgAtr;
        private final double regimeConfidence;
        private final double adaptiveRiskMultiplier;
        private final boolean strongTrend;
        private final boolean rangeRegime;
        private final String signal;

        public Context(
                int maxLeverage,
                int minLeverage,
                double adx,
                double atr,
                double avgAtr,
                double regimeConfidence,
                double adaptiveRiskMultiplier,
                boolean strongTrend,
                boolean rangeRegime,
                String signal) {
            this.maxLeverage = maxLeverage;
            this.minLeverage = minLeverage;
            this.adx = adx;
            this.atr = atr;
            this.avgAtr = avgAtr;
            this.regimeConfidence = regimeConfidence;
            this.adaptiveRiskMultiplier = adaptiveRiskMultiplier;
            this.strongTrend = strongTrend;
            this.rangeRegime = rangeRegime;
            this.signal = signal;
        }
    }

    /**
     * Resolve integer leverage in [{@code minLeverage}, {@code maxLeverage}].
     *
     * <p>Returns 1 when {@code maxLeverage <= 1}.
     */
    public static int resolve(Context context) {
        int maxLev = Math.max(1, context.maxLeverage);
        if (maxLev <= 1) {
            return 1;
        }
        int minLev = Math.max(1, Math.min(context.minLeverage, maxLev));

        double adxScore = clamp((context.adx - 15.0) / 25.0, 0.0, 1.0);
        double volRatio = context.avgAtr > 0.0 ? context.atr / context.avgAtr : 1.0;
        double volScore = 1.0 / clamp(volRatio, 0.75, 1.8);
        double confScore = clamp(context.regimeConfidence / 100.0, 0.0, 1.0);
        double signalScore = signalStrength(context.signal);
        double riskScore = clamp(context.adaptiveRiskMultiplier, 0.5, 1.0);

        double score = adxScore * 0.35 + confScore * 0.25 + signalScore * 0.20 + volScore * 0.20;
        score *= riskScore;
        if (context.rangeRegime) {
            score *= 0.65;
        }
        if (context.strongTrend) {
            score = Math.min(1.0, score * 1.12);
        }

        int leverage = minLev + (int) Math.round((maxLev - minLev) * clamp(score, 0.0, 1.0));
        return Math.max(minLev, Math.min(maxLev, leverage));
    }

    private static double signalStrength(String signal) {
        if (signal == null || signal.isEmpty()) {
            return 0.5;
        }
        if (signal.startsWith("TB_6") || signal.startsWith("MXB") || signal.startsWith("MXS")) {
            return 1.0;
        }
        if (signal.startsWith("TB_5")) {
            return 0.9;
        }
        if (signal.startsWith("TB_4")) {
            return 0.75;
        }
        if (signal.startsWith("FX")) {
            return 0.7;
        }
        return 0.8;
    }

    private static double clamp(double value, double min, double max) {
        return Math.max(min, Math.min(max, value));
    }
}
