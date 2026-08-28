package com.github.shk0da.goldendragon.strategy.orderbook;

import com.github.shk0da.goldendragon.model.MarketDepthLevel;
import com.github.shk0da.goldendragon.model.MarketDepthSnapshot;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Tracks fill rate and eaten ratio of order book levels across snapshots.
 *
 * <p>Key features:
 * <ul>
 *   <li>Records volume at each price level on every order book update</li>
 *   <li>Calculates fill rate — how quickly volume at a level gets consumed (units/sec)</li>
 *   <li>Calculates eaten ratio — percentage of level volume removed between snapshots</li>
 *   <li>Distinguishes natural decay (cancellations) from aggressive eating (trades)</li>
 *   <li>Maintains per-level history of size changes over configurable window</li>
 *   <li>Provides aggregate metrics: average fill rate, fast/slow level classification</li>
 * </ul>
 *
 * <p>The heuristic for distinguishing decay types:
 * <ul>
 *   <li>Aggressive eating: volume decreased and the level was within the trade zone
 *       (price between best bid and best ask of the previous snapshot, or at the top levels)</li>
 *   <li>Natural decay: volume decreased without being in the active trade zone</li>
 * </ul>
 */
public final class QueueDynamicsTracker {

    private static final int DEFAULT_HISTORY_WINDOW = 20;
    private static final double DEFAULT_PRICE_TOLERANCE_BPS = 1.0;
    private static final double DEFAULT_FAST_FILL_THRESHOLD = 0.5;
    private static final int TOP_LEVELS_TRADE_ZONE = 3;

    private final int historyWindow;
    private final double priceToleranceBps;
    private final double fastFillThreshold;

    private final Map<String, Map<Double, LevelQueueState>> bidQueuesByTicker = new ConcurrentHashMap<>();
    private final Map<String, Map<Double, LevelQueueState>> askQueuesByTicker = new ConcurrentHashMap<>();

    private final Map<String, Double> lastBestBidByTicker = new ConcurrentHashMap<>();
    private final Map<String, Double> lastBestAskByTicker = new ConcurrentHashMap<>();

    public QueueDynamicsTracker() {
        this(DEFAULT_HISTORY_WINDOW, DEFAULT_PRICE_TOLERANCE_BPS, DEFAULT_FAST_FILL_THRESHOLD);
    }

    public QueueDynamicsTracker(int historyWindow, double priceToleranceBps, double fastFillThreshold) {
        this.historyWindow = Math.max(5, historyWindow);
        this.priceToleranceBps = priceToleranceBps;
        this.fastFillThreshold = fastFillThreshold;
    }

    /**
     * Update queue tracking with a new order book snapshot.
     *
     * @param ticker ticker symbol
     * @param snapshot current order book snapshot
     */
    public void update(String ticker, MarketDepthSnapshot snapshot) {
        if (snapshot == null || !snapshot.isConsistent()) {
            return;
        }

        double currentBestBid = snapshot.getBestBid() != null ? snapshot.getBestBid() : 0.0;
        double currentBestAsk = snapshot.getBestAsk() != null ? snapshot.getBestAsk() : 0.0;
        if (currentBestBid <= 0.0 || currentBestAsk <= 0.0) {
            return;
        }

        Double prevBestBid = lastBestBidByTicker.get(ticker);
        Double prevBestAsk = lastBestAskByTicker.get(ticker);
        long nowMs = System.currentTimeMillis();

        UpdateContext ctx = new UpdateContext(nowMs, prevBestBid, prevBestAsk);

        Map<Double, LevelQueueState> bidQueues = bidQueuesByTicker.computeIfAbsent(ticker, k -> new HashMap<>());
        Map<Double, LevelQueueState> askQueues = askQueuesByTicker.computeIfAbsent(ticker, k -> new HashMap<>());

        updateLevels(snapshot.getBids(), bidQueues, true, ctx);
        updateLevels(snapshot.getAsks(), askQueues, false, ctx);

        lastBestBidByTicker.put(ticker, currentBestBid);
        lastBestAskByTicker.put(ticker, currentBestAsk);
    }

    private void updateLevels(
            List<MarketDepthLevel> levels,
            Map<Double, LevelQueueState> queues,
            boolean isBid,
            UpdateContext ctx) {
        if (levels == null || levels.isEmpty()) {
            return;
        }

        for (int i = 0; i < levels.size(); i++) {
            MarketDepthLevel level = levels.get(i);
            double price = level.getPrice();
            int quantity = level.getQuantity();

            LevelQueueState state = findOrCreateState(queues, price);
            int prevQuantity = state.lastQuantity;
            long prevTimestamp = state.lastTimestampMs;

            if (prevQuantity > 0 && prevTimestamp > 0) {
                int delta = quantity - prevQuantity;
                if (delta < 0) {
                    int volumeRemoved = -delta;
                    boolean wasInTradeZone = isInTradeZone(price, isBid, ctx.prevBestBid, ctx.prevBestAsk, i);
                    if (wasInTradeZone) {
                        state.totalEaten += volumeRemoved;
                    } else {
                        state.totalCancelled += volumeRemoved;
                    }
                }

                double elapsedSec = (ctx.nowMs - prevTimestamp) / 1000.0;
                if (elapsedSec > 0.0 && delta < 0) {
                    double fillRate = (-delta) / elapsedSec;
                    state.fillRateSamples.addLast(fillRate);
                    state.fillRateSum += fillRate;
                    if (state.fillRateSamples.size() > historyWindow) {
                        state.fillRateSum -= state.fillRateSamples.removeFirst();
                    }
                }
            }

            state.lastQuantity = quantity;
            state.lastTimestampMs = ctx.nowMs;
            state.snapshotCount++;

            QueueSnapshot qs = new QueueSnapshot(ctx.nowMs, quantity, quantity - prevQuantity);
            state.history.addLast(qs);
            if (state.history.size() > historyWindow) {
                state.history.removeFirst();
            }
        }

        // remove stale levels not present in current snapshot
        queues.entrySet().removeIf(entry -> {
            LevelQueueState state = entry.getValue();
            return (ctx.nowMs - state.lastTimestampMs) > 30_000L;
        });
    }

    private LevelQueueState findOrCreateState(Map<Double, LevelQueueState> queues, double price) {
        // exact match first
        LevelQueueState state = queues.get(price);
        if (state != null) {
            return state;
        }

        // tolerance match
        double tolerance = price * priceToleranceBps / 10_000.0;
        for (Map.Entry<Double, LevelQueueState> entry : queues.entrySet()) {
            if (Math.abs(entry.getKey() - price) <= tolerance) {
                return entry.getValue();
            }
        }

        state = new LevelQueueState(historyWindow);
        queues.put(price, state);
        return state;
    }

    /**
     * Check if a price level was in the active trade zone.
     *
     * <p>A level is in the trade zone if it was at the top levels or between
     * the best bid and best ask of the previous snapshot.
     */
    private boolean isInTradeZone(
            double price, boolean isBid,
            Double prevBestBid, Double prevBestAsk,
            int levelIndex) {
        if (prevBestBid == null || prevBestAsk == null) {
            return levelIndex < TOP_LEVELS_TRADE_ZONE;
        }
        if (levelIndex < TOP_LEVELS_TRADE_ZONE) {
            return true;
        }
        // level was between or at the edges of the previous spread
        return price >= prevBestBid && price <= prevBestAsk;
    }

    /**
     * Get the eaten ratio for a specific level — percentage of volume removed by trades.
     *
     * @return ratio from 0.0 to 1.0, or 0.0 if no data
     */
    public double getEatenRatio(String ticker, double price, boolean isBid) {
        LevelQueueState state = findState(ticker, price, isBid);
        if (state == null) {
            return 0.0;
        }
        long totalRemoved = state.totalEaten + state.totalCancelled;
        if (totalRemoved <= 0) {
            return 0.0;
        }
        return (double) state.totalEaten / totalRemoved;
    }

    /**
     * Get the fill rate for a specific level — volume consumed per second.
     *
     * @return average fill rate in units/sec, or 0.0 if no data
     */
    public double getFillRate(String ticker, double price, boolean isBid) {
        LevelQueueState state = findState(ticker, price, isBid);
        if (state == null || state.fillRateSamples.isEmpty()) {
            return 0.0;
        }
        return state.fillRateSum / state.fillRateSamples.size();
    }

    /**
     * Get the average fill rate across all tracked levels for a ticker side.
     *
     * @return average fill rate in units/sec
     */
    public double getAverageFillRate(String ticker, boolean isBid) {
        Map<Double, LevelQueueState> queues = isBid
                ? bidQueuesByTicker.get(ticker)
                : askQueuesByTicker.get(ticker);
        if (queues == null || queues.isEmpty()) {
            return 0.0;
        }

        double sum = 0.0;
        int count = 0;
        for (LevelQueueState state : queues.values()) {
            if (!state.fillRateSamples.isEmpty()) {
                sum += state.fillRateSum / state.fillRateSamples.size();
                count++;
            }
        }
        return count > 0 ? sum / count : 0.0;
    }

    /**
     * Check if a level is classified as fast-filling.
     *
     * <p>A level is fast if its eaten ratio exceeds the configured threshold,
     * meaning most of the volume removal was from aggressive trades rather than cancellations.
     */
    public boolean isFastLevel(String ticker, double price, boolean isBid) {
        return getEatenRatio(ticker, price, isBid) >= fastFillThreshold;
    }

    /**
     * Get the number of snapshots recorded for a specific level.
     */
    public int getSnapshotCount(String ticker, double price, boolean isBid) {
        LevelQueueState state = findState(ticker, price, isBid);
        return state != null ? state.snapshotCount : 0;
    }

    /**
     * Get the current volume at a tracked level.
     *
     * @return current quantity, or 0 if level not tracked
     */
    public int getCurrentQuantity(String ticker, double price, boolean isBid) {
        LevelQueueState state = findState(ticker, price, isBid);
        return state != null ? state.lastQuantity : 0;
    }

    /**
     * Get the total volume eaten (by trades) at a level since tracking began.
     */
    public long getTotalEaten(String ticker, double price, boolean isBid) {
        LevelQueueState state = findState(ticker, price, isBid);
        return state != null ? state.totalEaten : 0;
    }

    /**
     * Get the total volume cancelled at a level since tracking began.
     */
    public long getTotalCancelled(String ticker, double price, boolean isBid) {
        LevelQueueState state = findState(ticker, price, isBid);
        return state != null ? state.totalCancelled : 0;
    }

    /**
     * Get the count of tracked levels for a ticker side.
     */
    public int getTrackedLevelCount(String ticker, boolean isBid) {
        Map<Double, LevelQueueState> queues = isBid
                ? bidQueuesByTicker.get(ticker)
                : askQueuesByTicker.get(ticker);
        return queues != null ? queues.size() : 0;
    }

    /** Reset all tracking data for a ticker. */
    public void reset(String ticker) {
        bidQueuesByTicker.remove(ticker);
        askQueuesByTicker.remove(ticker);
        lastBestBidByTicker.remove(ticker);
        lastBestAskByTicker.remove(ticker);
    }

    /** Reset all tracking data. */
    public void reset() {
        bidQueuesByTicker.clear();
        askQueuesByTicker.clear();
        lastBestBidByTicker.clear();
        lastBestAskByTicker.clear();
    }

    private LevelQueueState findState(String ticker, double price, boolean isBid) {
        Map<Double, LevelQueueState> queues = isBid
                ? bidQueuesByTicker.get(ticker)
                : askQueuesByTicker.get(ticker);
        if (queues == null) {
            return null;
        }

        LevelQueueState state = queues.get(price);
        if (state != null) {
            return state;
        }

        double tolerance = price * priceToleranceBps / 10_000.0;
        for (Map.Entry<Double, LevelQueueState> entry : queues.entrySet()) {
            if (Math.abs(entry.getKey() - price) <= tolerance) {
                return entry.getValue();
            }
        }
        return null;
    }

    /**
     * Context passed to updateLevels to avoid excessive parameter count.
     */
    private static final class UpdateContext {

        final long nowMs;
        final Double prevBestBid;
        final Double prevBestAsk;

        UpdateContext(long nowMs, Double prevBestBid, Double prevBestAsk) {
            this.nowMs = nowMs;
            this.prevBestBid = prevBestBid;
            this.prevBestAsk = prevBestAsk;
        }
    }

    /**
     * Per-level queue state tracking volume changes over time.
     */
    static final class LevelQueueState {

        final Deque<QueueSnapshot> history;
        final Deque<Double> fillRateSamples;

        int lastQuantity;
        long lastTimestampMs;
        long totalEaten;
        long totalCancelled;
        int snapshotCount;
        double fillRateSum;

        LevelQueueState(int historyWindow) {
            this.history = new ArrayDeque<>(historyWindow);
            this.fillRateSamples = new ArrayDeque<>(historyWindow);
            this.lastQuantity = 0;
            this.lastTimestampMs = 0;
            this.totalEaten = 0;
            this.totalCancelled = 0;
            this.snapshotCount = 0;
            this.fillRateSum = 0.0;
        }
    }

    /**
     * Single snapshot of queue state at a point in time.
     */
    static final class QueueSnapshot {

        final long timestampMs;
        final int quantity;
        final int delta;

        QueueSnapshot(long timestampMs, int quantity, int delta) {
            this.timestampMs = timestampMs;
            this.quantity = quantity;
            this.delta = delta;
        }
    }
}
