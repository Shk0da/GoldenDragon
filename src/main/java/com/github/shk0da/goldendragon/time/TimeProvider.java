package com.github.shk0da.goldendragon.time;

import java.time.LocalDateTime;
import java.time.OffsetDateTime;

/**
 * Abstraction over time sources for strategies.
 * Implementations allow switching between live (system) time and
 * backtest (virtual, controlled) time.
 */
public interface TimeProvider {

    /**
     * Returns the current local date-time.
     */
    LocalDateTime now();

    /**
     * Returns the current time in milliseconds (equivalent to System.currentTimeMillis()).
     */
    long currentTimeMillis();

    /**
     * Returns the current date-time with offset.
     */
    OffsetDateTime nowOffset();

    /**
     * Returns true if this provider is a live (system clock) provider.
     */
    boolean isLive();

    /**
     * Advances the virtual time by the specified number of hours.
     * Only meaningful for backtest implementations.
     */
    void advanceByHours(int hours);

    /**
     * Resets the virtual clock to a specific starting time.
     */
    void setStart(LocalDateTime start);

    /**
     * Resets the virtual clock to the current system time.
     */
    void reset();
}
