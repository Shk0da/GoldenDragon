package com.github.shk0da.GoldenDragon.strategy.gerchik;

import java.time.LocalTime;

/**
 * Time of day filter for Gerchik strategy.
 * Controls trading windows and EOD close.
 */
public class TimeOfDayFilter {

    private final LocalTime sessionStart;
    private final LocalTime sessionEnd;
    private final LocalTime noEntryAfter;
    private final LocalTime eodCloseTime;

    /**
     * Create time of day filter.
     *
     * @param sessionStart trading session start (e.g., 10:00)
     * @param sessionEnd trading session end (e.g., 21:00)
     * @param noEntryAfter no new entries after this time (e.g., 19:00)
     * @param eodCloseTime force close all positions at this time (e.g., 20:50)
     */
    public TimeOfDayFilter(
            LocalTime sessionStart,
            LocalTime sessionEnd,
            LocalTime noEntryAfter,
            LocalTime eodCloseTime) {
        this.sessionStart = sessionStart;
        this.sessionEnd = sessionEnd;
        this.noEntryAfter = noEntryAfter;
        this.eodCloseTime = eodCloseTime;
    }

    /**
     * Check if current time is within trading window.
     *
     * @param currentTime current time
     * @return true if within trading window
     */
    public boolean isWithinTradingWindow(LocalTime currentTime) {
        return !currentTime.isBefore(sessionStart) && !currentTime.isAfter(sessionEnd);
    }

    /**
     * Check if can enter new position.
     *
     * @param currentTime current time
     * @return true if can enter (before noEntryAfter)
     */
    public boolean canEnterPosition(LocalTime currentTime) {
        return isWithinTradingWindow(currentTime) && !currentTime.isAfter(noEntryAfter);
    }

    /**
     * Check if should close all positions (EOD).
     *
     * @param currentTime current time
     * @return true if should close all
     */
    public boolean shouldCloseAll(LocalTime currentTime) {
        return !currentTime.isBefore(eodCloseTime);
    }

    /**
     * Get session start time.
     */
    public LocalTime getSessionStart() {
        return sessionStart;
    }

    /**
     * Get session end time.
     */
    public LocalTime getSessionEnd() {
        return sessionEnd;
    }

    /**
     * Get no entry after time.
     */
    public LocalTime getNoEntryAfter() {
        return noEntryAfter;
    }

    /**
     * Get EOD close time.
     */
    public LocalTime getEodCloseTime() {
        return eodCloseTime;
    }
}
