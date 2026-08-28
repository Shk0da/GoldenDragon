package com.github.shk0da.goldendragon.strategy.orderbook;

import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

/**
 * Tracks tickers that are temporarily blocked from trading due to broker errors
 * (e.g., confirmation required, insufficient funds for specific instrument).
 *
 * <p>Each blocked ticker has an expiration timestamp. Expired entries are lazily
 * cleaned up on access.
 */
public final class TickerBlocklist {

    private final Map<String, Long> blockedUntilMs = new ConcurrentHashMap<>();

    /**
     * Blocks a ticker for the specified duration starting from now.
     *
     * @param ticker ticker symbol to block
     * @param durationMs block duration in milliseconds
     */
    public void block(String ticker, long durationMs) {
        if (ticker == null || ticker.isEmpty() || durationMs <= 0) {
            return;
        }
        long until = System.currentTimeMillis() + durationMs;
        blockedUntilMs.put(ticker, until);
    }

    /**
     * Checks whether a ticker is currently blocked.
     * Expired entries are removed on access.
     *
     * @param ticker ticker symbol to check
     * @return true if the ticker is blocked and the block has not expired
     */
    public boolean isBlocked(String ticker) {
        if (ticker == null) {
            return false;
        }
        Long until = blockedUntilMs.get(ticker);
        if (until == null) {
            return false;
        }
        if (System.currentTimeMillis() >= until) {
            blockedUntilMs.remove(ticker);
            return false;
        }
        return true;
    }

    /**
     * Returns the remaining block time in milliseconds, or 0 if not blocked.
     *
     * @param ticker ticker symbol
     * @return remaining milliseconds, or 0
     */
    public long getRemainingMs(String ticker) {
        if (ticker == null) {
            return 0;
        }
        Long until = blockedUntilMs.get(ticker);
        if (until == null) {
            return 0;
        }
        long remaining = until - System.currentTimeMillis();
        return remaining > 0 ? remaining : 0;
    }

    /**
     * Returns the set of currently blocked tickers (excluding expired).
     */
    public Set<String> getBlockedTickers() {
        long now = System.currentTimeMillis();
        return blockedUntilMs.entrySet().stream()
                .filter(e -> e.getValue() > now)
                .map(Map.Entry::getKey)
                .collect(Collectors.toSet());
    }

    /**
     * Returns the number of currently active (non-expired) blocked tickers.
     */
    public int size() {
        return getBlockedTickers().size();
    }

    /**
     * Removes a ticker from the blocklist.
     *
     * @param ticker ticker symbol to unblock
     */
    public void unblock(String ticker) {
        if (ticker != null) {
            blockedUntilMs.remove(ticker);
        }
    }

    /**
     * Removes all expired entries from the blocklist.
     */
    public void cleanup() {
        long now = System.currentTimeMillis();
        blockedUntilMs.entrySet().removeIf(e -> e.getValue() <= now);
    }
}
