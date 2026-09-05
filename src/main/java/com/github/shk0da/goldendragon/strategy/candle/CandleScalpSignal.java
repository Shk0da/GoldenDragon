package com.github.shk0da.goldendragon.strategy.candle;

/**
 * Pluggable entry/exit logic for candle-based scalping.
 *
 * <p>Each implementation keeps per-ticker state internally. The engine calls signals in priority
 * order for entries; only the opening signal is consulted for signal-specific exits.
 */
public interface CandleScalpSignal {

    /** Unique identifier for this signal (e.g. "candle_delta", "candle_trend"). */
    String id();

    /**
     * Evaluate whether to enter a LONG position based on the latest candles and indicators.
     *
     * @param context market context containing latest candles and computed indicators
     * @param ticker ticker symbol being evaluated
     * @return entry decision with reason, or none() if no signal
     */
    CandleEntryDecision evaluateEntry(CandleMarketContext context, String ticker);

    /**
     * Evaluate whether to enter a SHORT position.
     *
     * @param context market context containing latest candles and computed indicators
     * @param ticker ticker symbol being evaluated
     * @return entry decision with reason, or none() if no signal
     */
    default CandleEntryDecision evaluateEntryShort(CandleMarketContext context, String ticker) {
        return CandleEntryDecision.none();
    }

    /**
     * Evaluate whether to exit an existing position.
     *
     * @param context market context containing latest candles and computed indicators
     * @param position current position information
     * @param ticker ticker symbol being evaluated
     * @return exit reason if should exit, null otherwise
     */
    String evaluateExit(CandleMarketContext context, CandlePositionView position, String ticker);

    /**
     * Reset per-ticker state (called when ticker is removed or on strategy reset).
     */
    void reset(String ticker);
}
