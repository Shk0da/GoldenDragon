package com.github.shk0da.goldendragon.strategy.orderbook.diagnostics;

public enum OrderBookDiagnosticEventType {
    ENTRY_EVALUATED,
    ENTRY_SKIPPED,
    ENTRY_OPENED,
    POSITION_UPDATED,
    EXIT_TRIGGERED,
    POSITION_CLOSED,
    STREAM_RECOVERY_STARTED,
    STREAM_RECOVERY_FINISHED,
    STREAM_OUTAGE_EXIT,
    DIAGNOSTIC,
    SKIP_QUAL,
    SUMMARY
}
