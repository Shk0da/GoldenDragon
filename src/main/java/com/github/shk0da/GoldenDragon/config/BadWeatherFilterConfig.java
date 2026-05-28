package com.github.shk0da.GoldenDragon.config;

import com.github.shk0da.GoldenDragon.model.Config;
import com.github.shk0da.GoldenDragon.utils.PropertiesUtils;
import java.io.IOException;
import java.util.Properties;

/**
 * Factory for creating Config with Bad Weather Filter parameters from properties.
 */
public class BadWeatherFilterConfig {

    private static final String PREFIX = "unifiedTrader.badWeatherFilter.";

    public static Config createFromProperties() throws IOException {
        Properties props = PropertiesUtils.loadProperties();
        return createFromProperties(props);
    }

    public static Config createFromProperties(Properties props) {
        boolean enabled = Boolean.parseBoolean(
                props.getProperty(PREFIX + "enabled", "false")
        );

        double lowVolumeThreshold = Double.parseDouble(
                props.getProperty(PREFIX + "lowVolumeThreshold", "0.5")
        );

        double lowAtrThreshold = Double.parseDouble(
                props.getProperty(PREFIX + "lowAtrThreshold", "0.7")
        );

        double minRangePercent = Double.parseDouble(
                props.getProperty(PREFIX + "minRangePercent", "0.005")
        );

        double highAtrThreshold = Double.parseDouble(
                props.getProperty(PREFIX + "highAtrThreshold", "2.0")
        );

        double maxSpreadPercent = Double.parseDouble(
                props.getProperty(PREFIX + "maxSpreadPercent", "0.01")
        );

        double maxWickRatio = Double.parseDouble(
                props.getProperty(PREFIX + "maxWickRatio", "0.4")
        );

        double panicVolumeThreshold = Double.parseDouble(
                props.getProperty(PREFIX + "panicVolumeThreshold", "3.0")
        );

        double minAvgDailyVolume = Double.parseDouble(
                props.getProperty(PREFIX + "minAvgDailyVolume", "100000")
        );

        double atrSpikeThreshold = Double.parseDouble(
                props.getProperty(PREFIX + "atrSpikeThreshold", "2.5")
        );

        return new Config(
                enabled,
                lowVolumeThreshold,
                lowAtrThreshold,
                minRangePercent,
                highAtrThreshold,
                maxSpreadPercent,
                maxWickRatio,
                panicVolumeThreshold,
                minAvgDailyVolume,
                atrSpikeThreshold
        );
    }
}
