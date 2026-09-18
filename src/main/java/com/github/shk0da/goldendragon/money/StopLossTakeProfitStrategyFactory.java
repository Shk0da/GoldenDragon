package com.github.shk0da.goldendragon.money;

/**
 * Factory for creating StopLossTakeProfitStrategy instances.
 * Selects algorithm based on algorithm type string from configuration.
 */
public class StopLossTakeProfitStrategyFactory {

    public static final String PERCENTAGE = "PERCENTAGE";
    public static final String ATR = "ATR";
    public static final String WAVE_ATR = "WAVE_ATR";
    public static final String HYBRID = "HYBRID";
    public static final String VOLATILITY_ADAPTIVE = "VOLATILITY_ADAPTIVE";
    public static final String LEVELS = "LEVELS";
    public static final String TIGHT_RANGE = "TIGHT_RANGE";

    /**
     * Create a SL/TP strategy based on algorithm type.
     *
     * @param algorithmType algorithm type from config
     * @return strategy instance, defaults to LevelStrategy if unknown
     */
    public static StopLossTakeProfitStrategy create(String algorithmType) {
        if (algorithmType == null || algorithmType.isEmpty()) {
            return new LevelStrategy();
        }

        switch (algorithmType.toUpperCase()) {
            case ATR:
                return new AtrStrategy();
            case WAVE_ATR:
                return new WaveAtrStrategy();
            case HYBRID:
                return new HybridStrategy();
            case VOLATILITY_ADAPTIVE:
                return new VolatilityAdaptiveStrategy();
            case LEVELS:
                return new LevelStrategy();
            case TIGHT_RANGE:
                return new TightRangeStrategy();
            case PERCENTAGE:
            default:
                return new PercentageStrategy();
        }
    }
}
