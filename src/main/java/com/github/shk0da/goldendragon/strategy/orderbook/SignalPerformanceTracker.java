package com.github.shk0da.goldendragon.strategy.orderbook;

import java.util.ArrayDeque;
import java.util.Collections;
import java.util.Deque;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Tracks per-signal win-rate and PnL statistics using a rolling window.
 *
 * <p>Each signal type (identified by {@link OrderBookSignal#id()}) maintains its own
 * bounded deque of trade outcomes. The tracker is thread-safe and designed to be
 * called from the trading engine when positions are closed.
 */
public final class SignalPerformanceTracker {

    /** Default rolling window size per signal type. */
    private static final int DEFAULT_WINDOW_SIZE = 100;

    /** Minimum trades required for a meaningful confidence level. */
    private static final int MIN_CONFIDENT_TRADES = 10;

    private final int windowSize;
    private final Map<String, Deque<TradeOutcome>> outcomesBySignal = new ConcurrentHashMap<>();

    public SignalPerformanceTracker() {
        this(DEFAULT_WINDOW_SIZE);
    }

    public SignalPerformanceTracker(int windowSize) {
        if (windowSize <= 0) {
            throw new IllegalArgumentException("windowSize must be positive, got " + windowSize);
        }
        this.windowSize = windowSize;
    }

    /**
     * Record a trade outcome for a given signal type.
     *
     * @param signalId signal identifier (e.g. "cumulativeDelta", "densityScalp")
     * @param pnl      net realized PnL of the trade
     */
    public void recordTrade(String signalId, double pnl) {
        if (signalId == null || signalId.isEmpty()) {
            return;
        }
        Deque<TradeOutcome> deque = outcomesBySignal.computeIfAbsent(
                signalId, k -> new ArrayDeque<>());
        synchronized (deque) {
            deque.addLast(new TradeOutcome(pnl, System.currentTimeMillis()));
            while (deque.size() > windowSize) {
                deque.removeFirst();
            }
        }
    }

    /**
     * Get win rate for a signal type as a percentage (0.0 to 100.0).
     *
     * @param signalId signal identifier
     * @return win rate percentage, or 0.0 if no trades recorded
     */
    public double getWinRate(String signalId) {
        Deque<TradeOutcome> deque = outcomesBySignal.get(signalId);
        if (deque == null) {
            return 0.0;
        }
        int total;
        int wins;
        synchronized (deque) {
            total = deque.size();
            if (total == 0) {
                return 0.0;
            }
            wins = 0;
            for (TradeOutcome outcome : deque) {
                if (outcome.pnl > 0.0) {
                    wins++;
                }
            }
        }
        return wins * 100.0 / total;
    }

    /**
     * Get average PnL per trade for a signal type.
     *
     * @param signalId signal identifier
     * @return average PnL, or 0.0 if no trades recorded
     */
    public double getAveragePnl(String signalId) {
        Deque<TradeOutcome> deque = outcomesBySignal.get(signalId);
        if (deque == null) {
            return 0.0;
        }
        int count;
        double sum;
        synchronized (deque) {
            count = deque.size();
            if (count == 0) {
                return 0.0;
            }
            sum = 0.0;
            for (TradeOutcome outcome : deque) {
                sum += outcome.pnl;
            }
        }
        return sum / count;
    }

    /**
     * Get the number of trades recorded for a signal type within the rolling window.
     *
     * @param signalId signal identifier
     * @return trade count
     */
    public int getTradeCount(String signalId) {
        Deque<TradeOutcome> deque = outcomesBySignal.get(signalId);
        if (deque == null) {
            return 0;
        }
        synchronized (deque) {
            return deque.size();
        }
    }

    /**
     * Get total PnL for a signal type within the rolling window.
     *
     * @param signalId signal identifier
     * @return sum of all PnL values
     */
    public double getTotalPnl(String signalId) {
        Deque<TradeOutcome> deque = outcomesBySignal.get(signalId);
        if (deque == null) {
            return 0.0;
        }
        double sum;
        synchronized (deque) {
            sum = 0.0;
            for (TradeOutcome outcome : deque) {
                sum += outcome.pnl;
            }
        }
        return sum;
    }

    /**
     * Calculate confidence factor for a signal type based on sample size.
     *
     * <p>Returns a value between 0.0 and 1.0 representing how much statistical
     * confidence we can place in the win-rate estimate. With fewer than
     * {@value #MIN_CONFIDENT_TRADES} trades the confidence scales linearly;
     * beyond that threshold it returns 1.0.
     *
     * @param signalId signal identifier
     * @return confidence factor from 0.0 (no data) to 1.0 (fully confident)
     */
    public double getConfidence(String signalId) {
        int count = getTradeCount(signalId);
        if (count == 0) {
            return 0.0;
        }
        if (count >= MIN_CONFIDENT_TRADES) {
            return 1.0;
        }
        return (double) count / MIN_CONFIDENT_TRADES;
    }

    /**
     * Get all tracked signal identifiers.
     *
     * @return unmodifiable view of signal IDs with recorded trades
     */
    public Set<String> getTrackedSignals() {
        return Collections.unmodifiableSet(outcomesBySignal.keySet());
    }

    /**
     * Reset all tracking data for a specific signal.
     *
     * @param signalId signal identifier
     */
    public void reset(String signalId) {
        outcomesBySignal.remove(signalId);
    }

    /**
     * Reset all tracking data.
     */
    public void resetAll() {
        outcomesBySignal.clear();
    }

    /** Single trade outcome record. */
    private static final class TradeOutcome {

        final double pnl;
        final long timestampMs;

        TradeOutcome(double pnl, long timestampMs) {
            this.pnl = pnl;
            this.timestampMs = timestampMs;
        }
    }
}
