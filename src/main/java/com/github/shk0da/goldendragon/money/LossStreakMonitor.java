package com.github.shk0da.goldendragon.money;

import com.github.shk0da.goldendragon.service.TradingService;

import java.time.Instant;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;

import static com.github.shk0da.goldendragon.utils.LoggingUtils.log;

/**
 * Monitors today's broker trade history and halts trading after a configurable number of
 * consecutive losing trades. Runs as a daemon thread started from BaseStrategy.
 */
public class LossStreakMonitor implements Runnable {

    private final TradingService tradingService;
    private final int maxConsecutiveLosses;
    private final long checkIntervalMs;
    private final String excludedTicker;
    private final Runnable onHalt;

    private volatile boolean running = true;
    private volatile boolean halted = false;
    private volatile int consecutiveLosses = 0;

    /**
     * Create loss streak monitor.
     *
     * @param tradingService trading service providing trade history
     * @param maxConsecutiveLosses consecutive losses before halt
     * @param checkIntervalMs interval between history checks
     * @param excludedTicker ticker to ignore when counting losses (cash parking),
     *                       null or empty disables exclusion
     * @param onHalt callback invoked when the streak threshold is reached
     */
    public LossStreakMonitor(
            TradingService tradingService,
            int maxConsecutiveLosses,
            long checkIntervalMs,
            String excludedTicker,
            Runnable onHalt) {
        this.tradingService = tradingService;
        this.maxConsecutiveLosses = maxConsecutiveLosses;
        this.checkIntervalMs = checkIntervalMs;
        this.excludedTicker = excludedTicker != null ? excludedTicker : "";
        this.onHalt = onHalt;
    }

    @Override
    public void run() {
        log(
                "LOSS_STREAK: Started with interval " + (checkIntervalMs / 1000)
                        + "s, threshold " + maxConsecutiveLosses);

        while (running) {
            try {
                checkLossStreak();
                Thread.sleep(checkIntervalMs);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                break;
            } catch (Exception e) {
                log("LOSS_STREAK: Error - " + e.getMessage());
                try {
                    Thread.sleep(checkIntervalMs);
                } catch (InterruptedException ie) {
                    Thread.currentThread().interrupt();
                    break;
                }
            }
        }

        log("LOSS_STREAK: Stopped");
    }

    /** Check today's trade history and halt when the losing streak reaches the threshold. */
    void checkLossStreak() {
        if (halted) {
            return;
        }
        try {
            Instant todayStart = Instant.now().atZone(ZoneId.systemDefault()).toLocalDate()
                    .atStartOfDay(ZoneId.systemDefault()).toInstant();
            List<Map<String, Object>> trades = tradingService.getTradeHistory(todayStart);
            int streak = countConsecutiveLosses(trades, excludedTicker);
            consecutiveLosses = streak;
            if (streak >= 2) {
                log("LOSS_STREAK: consecutive losses today = " + streak);
            }

            if (streak >= maxConsecutiveLosses) {
                halted = true;
                log(
                        "LOSS_STREAK: HALTING trading after " + streak
                                + " consecutive losses (threshold " + maxConsecutiveLosses + ")");
                onHalt.run();
            }
        } catch (Exception e) {
            log("LOSS_STREAK: Failed to check trade history - " + e.getMessage());
        }
    }

    /**
     * Count consecutive losing trades from the end of the history. Operations with zero pnl
     * (opening operations) and operations of the excluded ticker (cash parking) are skipped;
     * a winning trade breaks the streak.
     *
     * @param trades trade history entries with a "time", "ticker" and "pnl" key
     * @param excludedTicker ticker to ignore when counting losses, null or empty disables exclusion
     * @return number of consecutive losses ending at the latest trade
     */
    static int countConsecutiveLosses(List<Map<String, Object>> trades, String excludedTicker) {
        if (trades == null || trades.isEmpty()) {
            return 0;
        }
        List<Map<String, Object>> sorted = new ArrayList<>(trades);
        sorted.sort(Comparator.comparing(t -> String.valueOf(t.get("time"))));

        int streak = 0;
        for (int i = sorted.size() - 1; i >= 0; i--) {
            if (isExcludedTicker(sorted.get(i), excludedTicker)) {
                continue;
            }
            Object pnlObj = sorted.get(i).get("pnl");
            double pnl = pnlObj instanceof Number ? ((Number) pnlObj).doubleValue() : 0.0;
            if (pnl < 0) {
                streak++;
            } else if (pnl > 0) {
                break;
            }
            // pnl == 0: opening operation or breakeven close, skip
        }
        return streak;
    }

    private static boolean isExcludedTicker(Map<String, Object> trade, String excludedTicker) {
        if (excludedTicker == null || excludedTicker.isEmpty()) {
            return false;
        }
        Object ticker = trade.get("ticker");
        return ticker != null && excludedTicker.equalsIgnoreCase(String.valueOf(ticker));
    }

    /**
     * Check if trading has been halted.
     *
     * @return true if the streak threshold was reached
     */
    public boolean isHalted() {
        return halted;
    }

    /**
     * Get the last computed consecutive loss count.
     *
     * @return number of consecutive losses
     */
    public int getConsecutiveLosses() {
        return consecutiveLosses;
    }

    /** Stop the monitoring thread. */
    public void stop() {
        running = false;
    }
}
