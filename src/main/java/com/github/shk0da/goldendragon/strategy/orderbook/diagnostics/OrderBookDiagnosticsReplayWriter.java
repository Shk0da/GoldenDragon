package com.github.shk0da.goldendragon.strategy.orderbook.diagnostics;

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.Map;
import java.util.TreeMap;

public class OrderBookDiagnosticsReplayWriter implements AutoCloseable {

    private final BufferedWriter writer;

    public OrderBookDiagnosticsReplayWriter(String filePath) {
        try {
            Path path = Paths.get(filePath);
            Path parent = path.getParent();
            if (parent != null) {
                Files.createDirectories(parent);
            }
            this.writer =
                    Files.newBufferedWriter(
                            path,
                            StandardCharsets.UTF_8,
                            StandardOpenOption.CREATE,
                            StandardOpenOption.TRUNCATE_EXISTING,
                            StandardOpenOption.WRITE);
        } catch (IOException e) {
            throw new IllegalStateException("Failed to initialize diagnostics replay writer", e);
        }
    }

    public synchronized void write(OrderBookDiagnosticEvent event) {
        try {
            writer.write(serialize(event));
            writer.newLine();
            writer.flush();
        } catch (IOException e) {
            throw new IllegalStateException("Failed to write diagnostics replay event", e);
        }
    }

    private String serialize(OrderBookDiagnosticEvent event) {
        StringBuilder builder = new StringBuilder();
        builder.append(event.getTimestamp());
        builder.append('|').append(event.getType());
        builder.append('|').append(sanitize(event.getTicker()));
        builder.append('|').append(sanitize(event.getReason()));
        builder.append('|').append(serializeMetrics(event.getMetrics()));
        return builder.toString();
    }

    private String serializeMetrics(Map<String, Object> metrics) {
        if (metrics == null || metrics.isEmpty()) {
            return "";
        }
        StringBuilder builder = new StringBuilder();
        boolean first = true;
        for (Map.Entry<String, Object> entry : new TreeMap<>(metrics).entrySet()) {
            if (!first) {
                builder.append(';');
            }
            builder.append(sanitize(entry.getKey()));
            builder.append('=');
            builder.append(sanitize(String.valueOf(entry.getValue())));
            first = false;
        }
        return builder.toString();
    }

    private String sanitize(String value) {
        if (value == null) {
            return "";
        }
        return value.replace("|", "/").replace(";", ",").replace('\n', ' ').replace('\r', ' ');
    }

    @Override
    public synchronized void close() {
        try {
            writer.close();
        } catch (IOException e) {
            throw new IllegalStateException("Failed to close diagnostics replay writer", e);
        }
    }
}
