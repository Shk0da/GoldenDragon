package com.github.shk0da.GoldenDragon.config;

import com.github.shk0da.GoldenDragon.utils.PropertiesUtils;

import java.util.List;
import java.util.Properties;

import static java.util.Arrays.stream;
import static java.util.stream.Collectors.toList;

public class AILConfig {

    private final String dataDir;
    private final List<String> stocks;

    public AILConfig() throws Exception {
        final Properties properties = PropertiesUtils.loadProperties();
        dataDir = properties.getProperty("ail.dataDir");
        stocks = stream(properties.getProperty("ail.stocks").split(",")).collect(toList());
    }

    public String getDataDir() {
        return dataDir;
    }

    public List<String> getStocks() {
        return stocks;
    }
}
