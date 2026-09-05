package com.github.shk0da.goldendragon.config;

import com.github.shk0da.goldendragon.utils.PropertiesUtils;
import java.util.Properties;

/**
 * ByBit API configuration. Loads ByBit API credentials and test mode flags.
 */
public class ByBitConfig {

    private final boolean isTestMode;
    private final String apiKey;
    private final String apiSecret;

    public ByBitConfig() throws Exception {
        final Properties properties = PropertiesUtils.loadProperties();
        this.isTestMode = Boolean.parseBoolean(properties.getProperty("bybit.testMode", "false"));
        this.apiKey = properties.getProperty("bybit.apiKey", "");
        this.apiSecret = properties.getProperty("bybit.apiSecret", "");
    }

    public boolean isTestMode() {
        return isTestMode;
    }

    public String getApiKey() {
        return apiKey;
    }

    public String getApiSecret() {
        return apiSecret;
    }
}
