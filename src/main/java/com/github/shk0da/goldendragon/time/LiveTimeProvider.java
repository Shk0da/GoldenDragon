package com.github.shk0da.goldendragon.time;

import java.time.LocalDateTime;
import java.time.OffsetDateTime;

/**
 * Live implementation of TimeProvider using system clock.
 * Used in production trading.
 */
public class LiveTimeProvider implements TimeProvider {

    @Override
    public LocalDateTime now() {
        return LocalDateTime.now();
    }

    @Override
    public long currentTimeMillis() {
        return System.currentTimeMillis();
    }

    @Override
    public OffsetDateTime nowOffset() {
        return OffsetDateTime.now();
    }

    @Override
    public boolean isLive() {
        return true;
    }

    @Override
    public void advanceByHours(int hours) {
        // No-op in live mode - time advances naturally
    }

    @Override
    public void setStart(LocalDateTime start) {
        // No-op in live mode
    }

    @Override
    public void reset() {
        // No-op in live mode
    }
}
