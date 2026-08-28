package com.github.shk0da.goldendragon.strategy.orderbook.diagnostics;

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.time.Instant;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;

/**
 * Writes order-book strategy metrics to CSV for offline analysis.
 *
 * <p>Columns: timestamp, type, ticker, reason, obi, microEdge, tradeDelta, spreadBps, quality,
 * entryPrice, exitPrice, grossPnl, netPnl, fees, holdSeconds, units, direction,
 * signalId, trend, levelStrength, compressionStrength, impulseStrength, clusterCount, skipReason,
 * regime, atr, adx, blockTradeCount, blockTradeVolume, vpin, vwap, poc, valueAreaHigh, valueAreaLow,
 * avgFillRate, eatenRatio, signalWinRate, signalAvgPnl, signalTradeCount, dynamicTpPrice, tpSource,
 * maxCorrelation, correlatedTicker, spreadSpikeRatio, volumeSpikeRatio, inCooldown,
 * adjustedMinDelta, adjustedMinDensity, adjustedConfidence, avgSlippage, maxSlippage,
 * fillRatio, unfilledQty
 */
public class OrderBookMetricsCsvWriter implements AutoCloseable {

    private static final DateTimeFormatter TIMESTAMP_FMT =
            DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss").withZone(ZoneId.systemDefault());

    private static final String HEADER =
            "timestamp,type,ticker,reason,obi,microEdge,tradeDelta,spreadBps,quality,"
                    + "entryPrice,exitPrice,grossPnl,netPnl,fees,holdSeconds,units,direction,"
                    + "signalId,trend,levelStrength,compressionStrength,impulseStrength,clusterCount,skipReason,"
                    // Market regime
                    + "regime,atr,adx,"
                    // Tape reader
                    + "blockTradeCount,blockTradeVolume,"
                    // VPIN
                    + "vpin,"
                    // Volume profile
                    + "vwap,poc,valueAreaHigh,valueAreaLow,"
                    // Queue dynamics
                    + "avgFillRate,eatenRatio,"
                    // Signal performance
                    + "signalWinRate,signalAvgPnl,signalTradeCount,"
                    // Dynamic TP
                    + "dynamicTpPrice,tpSource,"
                    // Correlation filter
                    + "maxCorrelation,correlatedTicker,"
                    // Volatility spike filter
                    + "spreadSpikeRatio,volumeSpikeRatio,inCooldown,"
                    // Adaptive parameters
                    + "adjustedMinDelta,adjustedMinDensity,adjustedConfidence,"
                    // Slippage tracker
                    + "avgSlippage,maxSlippage,"
                    // Partial fill handler
                    + "fillRatio,unfilledQty";

    private final BufferedWriter writer;

    public OrderBookMetricsCsvWriter(String filePath) {
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
            writer.write(HEADER);
            writer.newLine();
        } catch (IOException e) {
            throw new IllegalStateException("Failed to initialize metrics CSV writer", e);
        }
    }

    public synchronized void write(OrderBookDiagnosticEvent event) {
        try {
            writer.write(serialize(event));
            writer.newLine();
            writer.flush();
        } catch (IOException e) {
            // skip write failure silently
        }
    }

    private String serialize(OrderBookDiagnosticEvent event) {
        StringBuilder sb = new StringBuilder();
        sb.append(formatTimestamp(event.getTimestamp()));
        sb.append(',').append(csvEscape(event.getType().name()));
        sb.append(',').append(csvEscape(event.getTicker()));
        sb.append(',').append(csvEscape(event.getReason()));

        // core metrics
        sb.append(',').append(getDouble(event, "obi"));
        sb.append(',').append(getDouble(event, "microEdge"));
        sb.append(',').append(getDouble(event, "tradeDelta"));
        sb.append(',').append(getDouble(event, "spreadBps"));
        sb.append(',').append(getDouble(event, "quality"));
        sb.append(',').append(getDouble(event, "entryPrice"));
        sb.append(',').append(getDouble(event, "exitPrice"));
        sb.append(',').append(getDouble(event, "grossPnl"));
        sb.append(',').append(getDouble(event, "netPnl"));
        sb.append(',').append(getDouble(event, "fees"));
        sb.append(',').append(getDouble(event, "holdSeconds"));
        sb.append(',').append(getInt(event, "units"));
        sb.append(',').append(getString(event, "direction"));
        // densityScalp metrics
        sb.append(',').append(getString(event, "signalId"));
        sb.append(',').append(getString(event, "trend"));
        sb.append(',').append(getDouble(event, "levelStrength"));
        sb.append(',').append(getDouble(event, "compressionStrength"));
        sb.append(',').append(getDouble(event, "impulseStrength"));
        sb.append(',').append(getInt(event, "clusterCount"));
        sb.append(',').append(getString(event, "skipReason"));
        // market regime
        sb.append(',').append(getString(event, "regime"));
        sb.append(',').append(getDouble(event, "atr"));
        sb.append(',').append(getDouble(event, "adx"));
        // tape reader
        sb.append(',').append(getInt(event, "blockTradeCount"));
        sb.append(',').append(getDouble(event, "blockTradeVolume"));
        // vpin
        sb.append(',').append(getDouble(event, "vpin"));
        // volume profile
        sb.append(',').append(getDouble(event, "vwap"));
        sb.append(',').append(getDouble(event, "poc"));
        sb.append(',').append(getDouble(event, "valueAreaHigh"));
        sb.append(',').append(getDouble(event, "valueAreaLow"));
        // queue dynamics
        sb.append(',').append(getDouble(event, "avgFillRate"));
        sb.append(',').append(getDouble(event, "eatenRatio"));
        // signal performance
        sb.append(',').append(getDouble(event, "signalWinRate"));
        sb.append(',').append(getDouble(event, "signalAvgPnl"));
        sb.append(',').append(getInt(event, "signalTradeCount"));
        // dynamic tp
        sb.append(',').append(getDouble(event, "dynamicTpPrice"));
        sb.append(',').append(getString(event, "tpSource"));
        // correlation filter
        sb.append(',').append(getDouble(event, "maxCorrelation"));
        sb.append(',').append(getString(event, "correlatedTicker"));
        // volatility spike filter
        sb.append(',').append(getDouble(event, "spreadSpikeRatio"));
        sb.append(',').append(getDouble(event, "volumeSpikeRatio"));
        sb.append(',').append(getBoolean(event, "inCooldown"));
        // adaptive parameters
        sb.append(',').append(getDouble(event, "adjustedMinDelta"));
        sb.append(',').append(getDouble(event, "adjustedMinDensity"));
        sb.append(',').append(getDouble(event, "adjustedConfidence"));
        // slippage tracker
        sb.append(',').append(getDouble(event, "avgSlippage"));
        sb.append(',').append(getDouble(event, "maxSlippage"));
        // partial fill handler
        sb.append(',').append(getDouble(event, "fillRatio"));
        sb.append(',').append(getInt(event, "unfilledQty"));
        return sb.toString();
    }

    private String formatTimestamp(Instant timestamp) {
        return TIMESTAMP_FMT.format(timestamp);
    }

    private String csvEscape(String value) {
        if (value == null) {
            return "";
        }
        return value.replace(",", ";").replace("\n", " ").replace("\r", " ");
    }

    private double getDouble(OrderBookDiagnosticEvent event, String key) {
        Object value = event.getMetrics().get(key);
        if (value instanceof Number) {
            return ((Number) value).doubleValue();
        }
        return Double.NaN;
    }

    private int getInt(OrderBookDiagnosticEvent event, String key) {
        Object value = event.getMetrics().get(key);
        if (value instanceof Number) {
            return ((Number) value).intValue();
        }
        return 0;
    }

    private String getString(OrderBookDiagnosticEvent event, String key) {
        Object value = event.getMetrics().get(key);
        return value != null ? value.toString() : "";
    }

    private String getBoolean(OrderBookDiagnosticEvent event, String key) {
        Object value = event.getMetrics().get(key);
        if (value instanceof Boolean) {
            return value.toString();
        }
        return "";
    }

    @Override
    public synchronized void close() {
        try {
            writer.close();
        } catch (IOException e) {
            // ignore
        }
    }
}
