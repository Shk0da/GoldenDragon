package com.github.shk0da.goldendragon.config;

import com.github.shk0da.goldendragon.utils.PropertiesUtils;
import java.io.IOException;
import java.util.Properties;

/**
 * Configuration for TMON Averaging strategy. Independent from UnifiedTraderConfig — manages only
 * TMON@ instrument with its own parameters.
 */
public class TmonAveragingConfig {

    public static final String TICKER = "TMON@";
    public static final String SERIALIZE_NAME = "tmonAveraging.json";

    private final String dataDir;
    private final double initialCash;
    private final int maxEntrySteps;
    private final int maxExitSteps;
    private final double profitTarget;
    private final int atrPeriod;
    private final boolean enabled;

    public TmonAveragingConfig() {
        final Properties properties;
        try {
            properties = PropertiesUtils.loadProperties();
        } catch (IOException e) {
            throw new RuntimeException("Failed to load properties", e);
        }
        dataDir = properties.getProperty("datacollector.dataDir", "data");
        initialCash =
                Double.parseDouble(properties.getProperty("tmonAveraging.initialCash", "1000000"));
        maxEntrySteps =
                Integer.parseInt(properties.getProperty("tmonAveraging.maxEntrySteps", "5"));
        maxExitSteps = Integer.parseInt(properties.getProperty("tmonAveraging.maxExitSteps", "5"));
        profitTarget =
                Double.parseDouble(properties.getProperty("tmonAveraging.profitTarget", "0.005"));
        atrPeriod = Integer.parseInt(properties.getProperty("tmonAveraging.atrPeriod", "14"));
        enabled = Boolean.parseBoolean(properties.getProperty("tmonAveraging.enabled", "true"));
    }

    public String getDataDir() {
        return dataDir;
    }

    public double getInitialCash() {
        return initialCash;
    }

    public int getMaxEntrySteps() {
        return maxEntrySteps;
    }

    public int getMaxExitSteps() {
        return maxExitSteps;
    }

    public double getProfitTarget() {
        return profitTarget;
    }

    public int getAtrPeriod() {
        return atrPeriod;
    }

    public boolean isEnabled() {
        return enabled;
    }
}
