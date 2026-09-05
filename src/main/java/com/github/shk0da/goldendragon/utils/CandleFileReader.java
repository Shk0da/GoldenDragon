package com.github.shk0da.goldendragon.utils;

import com.github.shk0da.goldendragon.model.TickerCandle;

import java.io.BufferedReader;
import java.io.FileReader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

/**
 * Utility for reading candle data from CSV files.
 */
public final class CandleFileReader {

    private CandleFileReader() {
    }

    /**
     * Reads candle data from a CSV file.
     *
     * @param name  ticker name
     * @param dir   data directory
     * @param period candle period (e.g. "5_MIN", "HOUR")
     * @return list of ticker candles, or empty list if file doesn't exist or on error
     */
    public static List<TickerCandle> readCandlesFile(String name, String dir, String period) {
        List<TickerCandle> candles = new ArrayList<>();
        String filePath = dir + "/" + name + "/candles" + period + ".txt";

        // Return empty list if file doesn't exist (first run)
        if (!Files.exists(Path.of(filePath))) {
            return candles;
        }

        try (BufferedReader br = new BufferedReader(new FileReader(filePath))) {
            boolean skipHeader = true;
            String line = br.readLine();
            while (line != null) {
                if (skipHeader) {
                    skipHeader = false;
                    line = br.readLine();
                    continue;
                }

                String[] values = line.split(",");
                candles.add(
                        new TickerCandle(
                                name,
                                values[0],
                                Double.valueOf(values[1]),
                                Double.valueOf(values[2]),
                                Double.valueOf(values[3]),
                                Double.valueOf(values[4]),
                                Double.valueOf(values[4]),
                                Long.valueOf(values[5])));
                line = br.readLine();
            }
        } catch (Exception ex) {
            System.out.println(ex.getMessage());
        }
        return candles;
    }
}
