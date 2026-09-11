package com.github.shk0da.goldendragon.config;

import com.github.shk0da.goldendragon.utils.PropertiesUtils;

import java.net.http.HttpClient;
import java.time.Duration;
import java.util.Properties;

/**
 * Main application configuration. Loads Tinkoff Invest API settings, test mode flags, and HTTP
 * client configuration.
 */
public class MainConfig {

    public static final HttpClient httpClient =
            HttpClient.newBuilder()
                    .version(HttpClient.Version.HTTP_2)
                    .followRedirects(HttpClient.Redirect.NORMAL)
                    .connectTimeout(Duration.ofSeconds(10))
                    .build();

    private final boolean isTestMode;
    private final boolean isSandbox;
    private final boolean writeMarketDepthTicks;
    private final java.util.Map<String, Integer> tickerLotOverrides;
    private final boolean lossStreakEnabled;
    private final int lossStreakThreshold;
    private final int lossStreakCheckIntervalMinutes;

    private String tcsAccountId;
    private final String tcsApiKey;

    public MainConfig() throws Exception {
        final Properties properties = PropertiesUtils.loadProperties();
        this.isTestMode = Boolean.parseBoolean(properties.getProperty("tcs.testMode", "false"));
        this.isSandbox = Boolean.parseBoolean(properties.getProperty("tcs.isSandbox", "false"));
        this.writeMarketDepthTicks =
                Boolean.parseBoolean(properties.getProperty("tcs.marketData.writeTicks", "false"));
        this.tcsAccountId = properties.getProperty("tcs.accountId");
        this.tcsApiKey = properties.getProperty("tcs.apiKey");
        this.tickerLotOverrides = loadTickerLotOverrides(properties);
        this.lossStreakEnabled =
                Boolean.parseBoolean(properties.getProperty("killswitch.lossStreak.enabled", "true"));
        this.lossStreakThreshold =
                Integer.parseInt(properties.getProperty("killswitch.lossStreak.threshold", "3"));
        this.lossStreakCheckIntervalMinutes =
                Integer.parseInt(properties.getProperty("killswitch.lossStreak.checkIntervalMinutes", "5"));
    }

    private java.util.Map<String, Integer> loadTickerLotOverrides(Properties properties) {
        java.util.Map<String, Integer> overrides = new java.util.HashMap<>();
        for (String key : properties.stringPropertyNames()) {
            if (key.startsWith("market.moex.") && key.endsWith(".lot")) {
                String ticker =
                        key.substring("market.moex.".length(), key.length() - ".lot".length());
                int lot = Integer.parseInt(properties.getProperty(key));
                overrides.put(ticker, lot);
            }
        }
        return overrides;
    }

    public boolean isTestMode() {
        return isTestMode;
    }

    public boolean isSandbox() {
        return isSandbox;
    }

    public String getTcsAccountId() {
        return tcsAccountId;
    }

    public boolean isWriteMarketDepthTicks() {
        return writeMarketDepthTicks;
    }

    public java.util.Map<String, Integer> getTickerLotOverrides() {
        return tickerLotOverrides;
    }

    public boolean isLossStreakEnabled() {
        return lossStreakEnabled;
    }

    public int getLossStreakThreshold() {
        return lossStreakThreshold;
    }

    public int getLossStreakCheckIntervalMinutes() {
        return lossStreakCheckIntervalMinutes;
    }

    public MainConfig withAccountId(String accountId) {
        this.tcsAccountId = accountId;
        return this;
    }

    public String getTcsApiKey() {
        return tcsApiKey;
    }
}
