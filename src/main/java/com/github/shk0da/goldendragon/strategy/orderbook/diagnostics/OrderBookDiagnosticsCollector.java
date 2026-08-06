package com.github.shk0da.goldendragon.strategy.orderbook.diagnostics;

import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CopyOnWriteArrayList;

public class OrderBookDiagnosticsCollector {

    private final List<OrderBookDiagnosticEvent> events = new CopyOnWriteArrayList<>();

    public void record(OrderBookDiagnosticEvent event) {
        if (event != null) {
            events.add(event);
        }
    }

    public List<OrderBookDiagnosticEvent> getEvents() {
        return new ArrayList<>(events);
    }

    public OrderBookDiagnosticsSummary summarize() {
        return summarizeSince(null);
    }

    public OrderBookDiagnosticsSummary summarizeSince(Instant since) {
        int opened = 0;
        int skipped = 0;
        int closed = 0;
        int recovery = 0;
        double qualitySum = 0.0;
        int qualityCount = 0;
        double grossPnl = 0.0;
        double netPnl = 0.0;
        double feeDrag = 0.0;
        double totalHoldSeconds = 0.0;
        int holdCount = 0;
        Map<String, Integer> skipReasons = new HashMap<>();
        Map<String, Integer> closeReasons = new HashMap<>();
        Map<String, Integer> skippedTickers = new HashMap<>();
        Map<String, Instant> openTimes = new HashMap<>();

        for (OrderBookDiagnosticEvent event : events) {
            if (since != null && event.getTimestamp().isBefore(since)) {
                continue;
            }
            switch (event.getType()) {
                case ENTRY_OPENED:
                    opened++;
                    openTimes.put(event.getTicker(), event.getTimestamp());
                    Object quality = event.getMetrics().get("quality");
                    if (quality instanceof Number) {
                        qualitySum += ((Number) quality).doubleValue();
                        qualityCount++;
                    }
                    break;
                case ENTRY_SKIPPED:
                    skipped++;
                    skipReasons.merge(event.getReason(), 1, Integer::sum);
                    skippedTickers.merge(event.getTicker(), 1, Integer::sum);
                    break;
                case POSITION_CLOSED:
                case STREAM_OUTAGE_EXIT:
                    closed++;
                    closeReasons.merge(event.getReason(), 1, Integer::sum);
                    Object gross = event.getMetrics().get("grossPnl");
                    if (gross instanceof Number) {
                        grossPnl += ((Number) gross).doubleValue();
                    }
                    Object net = event.getMetrics().get("netPnl");
                    if (net instanceof Number) {
                        netPnl += ((Number) net).doubleValue();
                    }
                    Object fees = event.getMetrics().get("fees");
                    if (fees instanceof Number) {
                        feeDrag += ((Number) fees).doubleValue();
                    }
                    Instant openTime = openTimes.remove(event.getTicker());
                    if (openTime != null) {
                        totalHoldSeconds +=
                                Duration.between(openTime, event.getTimestamp()).getSeconds();
                        holdCount++;
                    }
                    break;
                case STREAM_RECOVERY_STARTED:
                    recovery++;
                    break;
                default:
                    break;
            }
        }

        return new OrderBookDiagnosticsSummary(
                opened,
                skipped,
                closed,
                recovery,
                qualityCount > 0 ? qualitySum / qualityCount : 0.0,
                grossPnl,
                netPnl,
                feeDrag,
                holdCount > 0 ? totalHoldSeconds / holdCount : 0.0,
                skipReasons,
                closeReasons,
                skippedTickers);
    }
}
