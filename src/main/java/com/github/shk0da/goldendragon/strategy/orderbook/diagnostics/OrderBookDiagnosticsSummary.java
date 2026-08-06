package com.github.shk0da.goldendragon.strategy.orderbook.diagnostics;

import java.util.Collections;
import java.util.Map;

public class OrderBookDiagnosticsSummary {

    private final int openedCount;
    private final int skippedCount;
    private final int closedCount;
    private final int recoveryCount;
    private final double averageEntryQuality;
    private final double grossPnl;
    private final double netPnl;
    private final double feeDrag;
    private final double averageHoldSeconds;
    private final Map<String, Integer> skipReasons;
    private final Map<String, Integer> closeReasons;
    private final Map<String, Integer> skippedTickers;

    public OrderBookDiagnosticsSummary(
            int openedCount,
            int skippedCount,
            int closedCount,
            int recoveryCount,
            double averageEntryQuality,
            double grossPnl,
            double netPnl,
            double feeDrag,
            double averageHoldSeconds,
            Map<String, Integer> skipReasons,
            Map<String, Integer> closeReasons,
            Map<String, Integer> skippedTickers) {
        this.openedCount = openedCount;
        this.skippedCount = skippedCount;
        this.closedCount = closedCount;
        this.recoveryCount = recoveryCount;
        this.averageEntryQuality = averageEntryQuality;
        this.grossPnl = grossPnl;
        this.netPnl = netPnl;
        this.feeDrag = feeDrag;
        this.averageHoldSeconds = averageHoldSeconds;
        this.skipReasons =
                skipReasons == null
                        ? Collections.emptyMap()
                        : Collections.unmodifiableMap(skipReasons);
        this.closeReasons =
                closeReasons == null
                        ? Collections.emptyMap()
                        : Collections.unmodifiableMap(closeReasons);
        this.skippedTickers =
                skippedTickers == null
                        ? Collections.emptyMap()
                        : Collections.unmodifiableMap(skippedTickers);
    }

    public int getOpenedCount() {
        return openedCount;
    }

    public int getSkippedCount() {
        return skippedCount;
    }

    public int getClosedCount() {
        return closedCount;
    }

    public int getRecoveryCount() {
        return recoveryCount;
    }

    public double getAverageEntryQuality() {
        return averageEntryQuality;
    }

    public double getGrossPnl() {
        return grossPnl;
    }

    public double getNetPnl() {
        return netPnl;
    }

    public double getFeeDrag() {
        return feeDrag;
    }

    public double getAverageHoldSeconds() {
        return averageHoldSeconds;
    }

    public Map<String, Integer> getSkipReasons() {
        return skipReasons;
    }

    public Map<String, Integer> getCloseReasons() {
        return closeReasons;
    }

    public Map<String, Integer> getSkippedTickers() {
        return skippedTickers;
    }
}
