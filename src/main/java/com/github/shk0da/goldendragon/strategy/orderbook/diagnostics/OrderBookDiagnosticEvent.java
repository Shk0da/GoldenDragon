package com.github.shk0da.goldendragon.strategy.orderbook.diagnostics;

import java.time.Instant;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

public class OrderBookDiagnosticEvent {

    private final Instant timestamp;
    private final OrderBookDiagnosticEventType type;
    private final String ticker;
    private final String reason;
    private final Map<String, Object> metrics;

    public OrderBookDiagnosticEvent(
            Instant timestamp,
            OrderBookDiagnosticEventType type,
            String ticker,
            String reason,
            Map<String, Object> metrics) {
        this.timestamp = timestamp;
        this.type = type;
        this.ticker = ticker;
        this.reason = reason;
        this.metrics =
                metrics == null
                        ? Collections.emptyMap()
                        : Collections.unmodifiableMap(new HashMap<>(metrics));
    }

    public Instant getTimestamp() {
        return timestamp;
    }

    public OrderBookDiagnosticEventType getType() {
        return type;
    }

    public String getTicker() {
        return ticker;
    }

    public String getReason() {
        return reason;
    }

    public Map<String, Object> getMetrics() {
        return metrics;
    }
}
