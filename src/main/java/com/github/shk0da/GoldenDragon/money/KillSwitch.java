package com.github.shk0da.GoldenDragon.money;

import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;

/**
 * Kill switch for emergency trading halt.
 * Triggers on connection loss, abnormal spread, critical drawdown, or manual override.
 */
public class KillSwitch {

    private volatile boolean tradingAllowed = true;
    private final AtomicReference<String> triggerReason = new AtomicReference<>(null);
    private final AtomicLong triggerTime = new AtomicLong(0);
    private final double criticalDrawdownPercent;

    /**
     * Create kill switch with specified parameters.
     *
     * @param criticalDrawdownPercent critical drawdown to halt trading (e.g., 0.10 for 10%)
     */
    public KillSwitch(double criticalDrawdownPercent) {
        this.criticalDrawdownPercent = criticalDrawdownPercent;
    }

    /**
     * Check if trading is allowed.
     *
     * @return true if trading is allowed, false if kill switch triggered
     */
    public boolean isTradingAllowed() {
        return tradingAllowed;
    }

    /**
     * Trigger kill switch.
     *
     * @param reason reason for halt
     */
    public void trigger(String reason) {
        tradingAllowed = false;
        triggerReason.set(reason);
        triggerTime.set(System.currentTimeMillis());
    }

    /**
     * Trigger kill switch on critical drawdown.
     *
     * @param currentDrawdown current drawdown as decimal
     */
    public void checkDrawdown(double currentDrawdown) {
        if (currentDrawdown >= criticalDrawdownPercent && tradingAllowed) {
            trigger("CRITICAL_DD_" + (int) (currentDrawdown * 100) + "%");
        }
    }

    /**
     * Reset kill switch and allow trading.
     */
    public void reset() {
        tradingAllowed = true;
        triggerReason.set(null);
        triggerTime.set(0);
    }

    /**
     * Get last trigger reason.
     *
     * @return trigger reason or null if not triggered
     */
    public String getTriggerReason() {
        return triggerReason.get();
    }

    /**
     * Get trigger time.
     *
     * @return timestamp when kill switch was triggered (0 if not triggered)
     */
    public long getTriggerTime() {
        return triggerTime.get();
    }

    /**
     * Get critical drawdown parameter.
     *
     * @return critical drawdown as decimal
     */
    public double getCriticalDrawdownPercent() {
        return criticalDrawdownPercent;
    }

    /**
     * Check if kill switch was triggered by drawdown.
     *
     * @return true if triggered by drawdown
     */
    public boolean isDrawdownTriggered() {
        String reason = triggerReason.get();
        return reason != null && reason.startsWith("CRITICAL_DD");
    }
}
