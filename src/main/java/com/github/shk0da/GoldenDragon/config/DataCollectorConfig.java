package com.github.shk0da.GoldenDragon.config;

import com.github.shk0da.GoldenDragon.utils.PropertiesUtils;

import java.io.IOException;
import java.util.List;
import java.util.Properties;

import static java.util.Arrays.stream;
import static java.util.stream.Collectors.toList;

public class DataCollectorConfig {

    private String dataDir;
    private List<String> stocks;
    private Boolean replace;

    public DataCollectorConfig() throws IOException {
        final Properties properties = PropertiesUtils.loadProperties();
        dataDir = properties.getProperty("datacollector.dataDir", "data");
        stocks = stream(properties.getProperty("datacollector.stocks").split(",")).collect(toList());
        replace = Boolean.valueOf(properties.getProperty("datacollector.replace", "true"));
    }

    public String getDataDir() {
        return dataDir;
    }

    public List<String> getStocks() {
        return stocks;
    }

    public Boolean isReplace() {
        return replace;
    }

    @Override
    public String toString() {
        return "DataCollectorConfig{" +
                "dataDir='" + dataDir + '\'' +
                ", stocks=" + stocks +
                ", replace=" + replace +
                '}';
    }
}
