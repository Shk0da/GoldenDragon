package com.github.shk0da.GoldenDragon.config;

import com.github.shk0da.GoldenDragon.utils.PropertiesUtils;
import java.io.IOException;
import java.util.List;
import java.util.Properties;


import static java.util.Arrays.stream;
import static java.util.stream.Collectors.toList;

public class UnifiedTraderConfig {

    private String dataDir;
    private List<String> stocks;
    private Double averagePositionCost;

    public UnifiedTraderConfig() throws IOException {
        final Properties properties = PropertiesUtils.loadProperties();
        dataDir = properties.getProperty("datacollector.dataDir", "data");
        stocks = stream(properties.getProperty(
                "levelTrader.stocks",
                properties.getProperty("datacollector.stocks")
        ).split(",")).collect(toList());
        averagePositionCost = Double.valueOf(properties.getProperty("unifiedTrader.averagePositionCost", "10000"));
    }

    public void setDataDir(String dataDir) {
        this.dataDir = dataDir;
    }

    public void setStocks(List<String> stocks) {
        this.stocks = stocks;
    }

    public String getDataDir() {
        return dataDir;
    }

    public List<String> getStocks() {
        return stocks;
    }

    public Double getAveragePositionCost() {
        return averagePositionCost;
    }

    @Override
    public String toString() {
        return "LevelTraderConfig{" +
                "dataDir='" + dataDir + '\'' +
                ", stocks=" + stocks +
                '}';
    }
}
