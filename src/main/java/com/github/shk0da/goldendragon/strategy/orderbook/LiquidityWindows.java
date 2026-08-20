package com.github.shk0da.goldendragon.strategy.orderbook;

import com.github.shk0da.goldendragon.config.OrderBookScalpConfig;

import java.time.LocalTime;

/**
 * MOEX liquidity windows for time-of-day filtering.
 *
 * <p>MOEX has distinct liquidity periods:
 * <ul>
 *   <li>08:30-10:00 — Opening auction / early session (medium liquidity)</li>
 *   <li>10:00-14:00 — Morning session (peak liquidity)</li>
 *   <li>14:00-15:00 — Lunch break (low liquidity)</li>
 *   <li>15:00-18:45 — Afternoon session (medium-high liquidity)</li>
 *   <li>18:45-20:00 — Evening session start (medium liquidity)</li>
 *   <li>20:00-23:50 — Late evening (low liquidity for most instruments)</li>
 * </ul>
 *
 * <p>The liquidity multiplier adjusts position sizing and signal thresholds:
 * <ul>
 *   <li>High liquidity (1.5x) → can trade larger sizes, lower thresholds</li>
 *   <li>Low liquidity (0.5x) → must trade smaller sizes, higher thresholds</li>
 * </ul>
 */
public final class LiquidityWindows {

    // MOEX session boundaries (MSK timezone assumed)
    private static final LocalTime MORNING_START = LocalTime.of(8, 30);
    private static final LocalTime PEAK_START = LocalTime.of(10, 0);
    private static final LocalTime LUNCH_START = LocalTime.of(14, 0);
    private static final LocalTime AFTERNOON_START = LocalTime.of(15, 0);
    private static final LocalTime EVENING_START = LocalTime.of(18, 45);
    private static final LocalTime LATE_EVENING_START = LocalTime.of(20, 0);
    private static final LocalTime SESSION_END = LocalTime.of(23, 50);

    private final OrderBookScalpConfig config;

    public LiquidityWindows(OrderBookScalpConfig config) {
        this.config = config;
    }

    /**
     * Get liquidity multiplier for the current time.
     *
     * @param currentTime current time (assumed MSK)
     * @return multiplier in range [0.5, 1.5]
     */
    public double getLiquidityMultiplier(LocalTime currentTime) {
        if (!config.isTimeOfDayFilterEnabled()) {
            return 1.0;
        }

        if (isBefore(currentTime, MORNING_START) || isAfter(currentTime, SESSION_END)) {
            // Outside trading hours
            return 0.0;
        }

        if (isPeakHours(currentTime)) {
            return config.getMorningLiquidityMultiplier();
        }

        if (isLunchHours(currentTime)) {
            return config.getLunchLiquidityMultiplier();
        }

        if (isEveningHours(currentTime)) {
            return config.getEveningLiquidityMultiplier();
        }

        // Default for morning early and afternoon
        return 1.0;
    }

    /** Peak liquidity hours: 10:00-14:00 */
    public boolean isPeakHours(LocalTime time) {
        return !isBefore(time, PEAK_START) && isBefore(time, LUNCH_START);
    }

    /** Lunch hours (low liquidity): 14:00-15:00 */
    public boolean isLunchHours(LocalTime time) {
        return !isBefore(time, LUNCH_START) && isBefore(time, AFTERNOON_START);
    }

    /** Evening hours (reduced liquidity): 18:45-23:50 */
    public boolean isEveningHours(LocalTime time) {
        return !isBefore(time, EVENING_START) && !isAfter(time, SESSION_END);
    }

    /** Whether we're in a good trading window (not lunch or late evening). */
    public boolean isGoodTradingWindow(LocalTime time) {
        double multiplier = getLiquidityMultiplier(time);
        return multiplier >= 0.8;
    }

    /** Whether we should reduce position size due to low liquidity. */
    public boolean shouldReduceSize(LocalTime time) {
        double multiplier = getLiquidityMultiplier(time);
        return multiplier < 0.8;
    }

    /** Get session label for logging. */
    public String getSessionLabel(LocalTime time) {
        if (isPeakHours(time)) {
            return "PEAK";
        }
        if (isLunchHours(time)) {
            return "LUNCH";
        }
        if (isEveningHours(time)) {
            return "EVENING";
        }
        if (isBefore(time, PEAK_START)) {
            return "EARLY_MORNING";
        }
        return "AFTERNOON";
    }

    private static boolean isBefore(LocalTime a, LocalTime b) {
        return a.isBefore(b);
    }

    private static boolean isAfter(LocalTime a, LocalTime b) {
        return a.isAfter(b);
    }
}
