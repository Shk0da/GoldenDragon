package com.github.shk0da.goldendragon.config;

import com.github.shk0da.goldendragon.utils.PropertiesUtils;
import java.net.http.HttpClient;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

/**
 * Main application configuration. Loads Tinkoff Invest API settings, test mode flags, and HTTP
 * client configuration.
 */
public class MainConfig {

    public static final String HEADER_COOKIES = "Cookie";
    public static final String HEADER_USER_AGENT = "User-Agent";
    public static final String USER_AGENT =
            "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/87.0.4280.88 Safari/537.36";

    public static final List<Integer> CALENDAR_WORK_DAYS =
            new ArrayList<>() {
                {
                    add(2);
                    add(3);
                    add(4);
                    add(5);
                    add(6);
                }
            };

    public static final DateFormat dateFormat = new SimpleDateFormat("dd.MM.yyyy");
    public static final DateFormat dateTimeFormat = new SimpleDateFormat("dd.MM.yyyy HH:mm:ss");
    public static final DateFormat dateFormatUs = new SimpleDateFormat("yyyy-MM-dd");

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

    public MainConfig withAccountId(String accountId) {
        this.tcsAccountId = accountId;
        return this;
    }

    public String getTcsApiKey() {
        return tcsApiKey;
    }
}
