package com.github.shk0da.goldendragon.strategy.orderbook;

import com.github.shk0da.goldendragon.model.MarketTradeTick;

import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.ArrayDeque;
import java.util.Collections;
import java.util.Deque;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Detects block trades (unusually large individual trades) by comparing each trade volume
 * against a rolling median of recent trade volumes.
 *
 * <p>A block trade is flagged when its volume exceeds {@code multiplier * median}.
 * Each block trade is classified by aggressor side (BUY or SELL) based on the trade direction.
 */
public final class TapeReader {

    private static final int DEFAULT_WINDOW_SIZE = 200;
    private static final double DEFAULT_BLOCK_MULTIPLIER = 3.0;
    private static final long DEFAULT_QUERY_WINDOW_MS = 60_000L;

    private final int windowSize;
    private final double blockMultiplier;
    private final Map<String, TickerTapeState> statesByTicker = new ConcurrentHashMap<>();

    /**
     * Creates a TapeReader with default settings (200-trade window, 3x multiplier).
     */
    public TapeReader() {
        this(DEFAULT_WINDOW_SIZE, DEFAULT_BLOCK_MULTIPLIER);
    }

    /**
     * Creates a TapeReader with custom window size and block detection multiplier.
     *
     * @param windowSize     number of recent trades to keep for median calculation
     * @param blockMultiplier trade volume must exceed {@code multiplier * median} to be a block trade
     */
    public TapeReader(int windowSize, double blockMultiplier) {
        this.windowSize = windowSize;
        this.blockMultiplier = blockMultiplier;
    }

    /**
     * Per-ticker tape state: rolling volume window and detected block trades.
     */
    private static final class TickerTapeState {

        final Deque<Long> volumeWindow = new ArrayDeque<>();
        final Deque<BlockTrade> blockTrades = new ArrayDeque<>();

        // pre-sorted copy for median calculation, rebuilt lazily
        private final List<Long> sortedVolumes = new ArrayList<>();
        private boolean sortedDirty = true;

        void addTradeVolume(long volume) {
            volumeWindow.addLast(volume);
            sortedDirty = true;
        }

        void evictOldest(int maxSize) {
            while (volumeWindow.size() > maxSize) {
                volumeWindow.removeFirst();
                sortedDirty = true;
            }
        }

        double computeMedian() {
            if (volumeWindow.isEmpty()) {
                return 0.0;
            }
            if (sortedDirty) {
                sortedVolumes.clear();
                sortedVolumes.addAll(volumeWindow);
                Collections.sort(sortedVolumes);
                sortedDirty = false;
            }
            int size = sortedVolumes.size();
            if (size % 2 == 1) {
                return sortedVolumes.get(size / 2);
            }
            return (sortedVolumes.get(size / 2 - 1) + sortedVolumes.get(size / 2)) / 2.0;
        }

        void addBlockTrade(BlockTrade blockTrade) {
            blockTrades.addLast(blockTrade);
        }

        void evictStaleBlocks(Instant cutoff) {
            while (!blockTrades.isEmpty() && blockTrades.peekFirst().getTime().isBefore(cutoff)) {
                blockTrades.removeFirst();
            }
        }
    }

    /**
     * A detected block trade record.
     */
    public static final class BlockTrade {

        private final Instant time;
        private final long volume;
        private final double price;
        private final String aggressorSide;

        BlockTrade(Instant time, long volume, double price, String aggressorSide) {
            this.time = time;
            this.volume = volume;
            this.price = price;
            this.aggressorSide = aggressorSide;
        }

        public Instant getTime() {
            return time;
        }

        public long getVolume() {
            return volume;
        }

        public double getPrice() {
            return price;
        }

        public String getAggressorSide() {
            return aggressorSide;
        }

        @Override
        public String toString() {
            return "BlockTrade{"
                    + "time=" + time
                    + ", volume=" + volume
                    + ", price=" + price
                    + ", side=" + aggressorSide
                    + '}';
        }
    }

    /**
     * Result of processing a single trade tick through the tape reader.
     */
    public static final class TapeResult {

        private final boolean isBlockTrade;
        private final long volume;
        private final double median;
        private final double ratio;
        private final String aggressorSide;

        TapeResult(boolean isBlockTrade, long volume, double median, double ratio, String aggressorSide) {
            this.isBlockTrade = isBlockTrade;
            this.volume = volume;
            this.median = median;
            this.ratio = ratio;
            this.aggressorSide = aggressorSide;
        }

        public boolean isBlockTrade() {
            return isBlockTrade;
        }

        public long getVolume() {
            return volume;
        }

        public double getMedian() {
            return median;
        }

        public double getRatio() {
            return ratio;
        }

        public String getAggressorSide() {
            return aggressorSide;
        }
    }

    /**
     * Process a trade tick: update rolling window and detect block trades.
     *
     * @param ticker ticker symbol
     * @param trade  trade tick to process
     * @return tape result indicating whether this trade is a block trade
     */
    public TapeResult onTrade(String ticker, MarketTradeTick trade) {
        TickerTapeState state = statesByTicker.computeIfAbsent(ticker, k -> new TickerTapeState());

        long volume = trade.getQuantity();
        double median = state.computeMedian();
        String side = resolveAggressorSide(trade.getDirection());

        boolean isBlock = false;
        double ratio = 0.0;

        if (median > 0) {
            ratio = (double) volume / median;
            isBlock = volume > blockMultiplier * median;
        } else if (volume > 0) {
            // first trade or all-zero window: cannot detect block without baseline
            ratio = 0.0;
        }

        // add volume to rolling window AFTER median check so current trade doesn't shift its own baseline
        state.addTradeVolume(volume);
        state.evictOldest(windowSize);

        if (isBlock) {
            state.addBlockTrade(new BlockTrade(trade.getTime(), volume, trade.getPrice(), side));
        }

        return new TapeResult(isBlock, volume, median, ratio, side);
    }

    /**
     * Query recent block trades for a ticker within the specified time window.
     *
     * @param ticker       ticker symbol
     * @param windowMillis lookback window in milliseconds from now
     * @return list of block trades within the window, ordered oldest first
     */
    public List<BlockTrade> getRecentBlockTrades(String ticker, long windowMillis) {
        TickerTapeState state = statesByTicker.get(ticker);
        if (state == null) {
            return Collections.emptyList();
        }

        Instant cutoff = Instant.now().minus(Duration.ofMillis(windowMillis));
        state.evictStaleBlocks(cutoff);

        return Collections.unmodifiableList(new ArrayList<>(state.blockTrades));
    }

    /**
     * Query recent block trades using the default 60-second window.
     *
     * @param ticker ticker symbol
     * @return list of recent block trades, ordered oldest first
     */
    public List<BlockTrade> getRecentBlockTrades(String ticker) {
        return getRecentBlockTrades(ticker, DEFAULT_QUERY_WINDOW_MS);
    }

    /**
     * Get the current rolling median trade volume for a ticker.
     *
     * @param ticker ticker symbol
     * @return current median volume, or 0.0 if no data
     */
    public double getCurrentMedian(String ticker) {
        TickerTapeState state = statesByTicker.get(ticker);
        if (state == null) {
            return 0.0;
        }
        return state.computeMedian();
    }

    /**
     * Count block trades detected for a ticker within the specified time window.
     *
     * @param ticker       ticker symbol
     * @param windowMillis lookback window in milliseconds
     * @return number of block trades in the window
     */
    public int countRecentBlocks(String ticker, long windowMillis) {
        return getRecentBlockTrades(ticker, windowMillis).size();
    }

    /**
     * Check whether there is net buy-side or sell-side block pressure.
     *
     * @param ticker       ticker symbol
     * @param windowMillis lookback window in milliseconds
     * @return positive value means buy-side block pressure, negative means sell-side
     */
    public long getBlockPressure(String ticker, long windowMillis) {
        List<BlockTrade> blocks = getRecentBlockTrades(ticker, windowMillis);
        long buyPressure = 0;
        long sellPressure = 0;
        for (BlockTrade block : blocks) {
            if ("BUY".equals(block.getAggressorSide())) {
                buyPressure += block.getVolume();
            } else {
                sellPressure += block.getVolume();
            }
        }
        return buyPressure - sellPressure;
    }

    /**
     * Reset tracking for a ticker.
     */
    public void reset(String ticker) {
        statesByTicker.remove(ticker);
    }

    private static String resolveAggressorSide(String direction) {
        if (direction == null) {
            return "SELL";
        }
        // direction "BUY" or "ASK" means aggressor was buyer (trade at ask)
        // direction "SELL" or "BID" means aggressor was seller (trade at bid)
        if ("BUY".equalsIgnoreCase(direction) || "ASK".equalsIgnoreCase(direction)) {
            return "BUY";
        }
        return "SELL";
    }
}
