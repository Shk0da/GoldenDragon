package com.github.shk0da.goldendragon.config;

import static java.util.Arrays.stream;
import static java.util.stream.Collectors.toList;

import com.github.shk0da.goldendragon.utils.PropertiesUtils;
import java.io.IOException;
import java.util.List;
import java.util.Properties;

/**
 * Configuration for DataCollector strategy. Defines data directory, instrument list, and replace
 * mode for historical data collection.
 */
public class DataCollectorConfig {

    private final String dataDir;
    private final List<String> instruments;
    private final Boolean replace;
    private final Integer historyDays;

    public DataCollectorConfig() throws IOException {
        final Properties properties = PropertiesUtils.loadProperties();
        dataDir = properties.getProperty("datacollector.dataDir", "data");
        instruments =
                stream(properties.getProperty("datacollector.instruments").split(","))
                        .collect(toList());
        replace = Boolean.valueOf(properties.getProperty("datacollector.replace", "true"));
        historyDays = Integer.valueOf(properties.getProperty("datacollector.historyDays", "365"));
    }

    public String getDataDir() {
        return dataDir;
    }

    public List<String> getInstruments() {
        return instruments;
    }

    public Boolean isReplace() {
        return replace;
    }

    public Integer getHistoryDays() {
        return historyDays;
    }

    @Override
    public String toString() {
        return "DataCollectorConfig{"
                + "dataDir='"
                + dataDir
                + '\''
                + ", instruments="
                + instruments
                + ", replace="
                + replace
                + ", historyDays="
                + historyDays
                + '}';
    }
}
