package com.github.shk0da.GoldenDragon.config;

import com.github.shk0da.GoldenDragon.utils.PropertiesUtils;

import java.net.http.HttpClient;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

public class MainConfig {

    public static final String HEADER_USER_AGENT = "User-Agent";
    public static final String USER_AGENT = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/87.0.4280.88 Safari/537.36";

    public static final List<Integer> CALENDAR_WORK_DAYS = new ArrayList<>() {{
        add(2);
        add(3);
        add(4);
        add(5);
        add(6);
    }};

    public static final DateFormat dateFormat = new SimpleDateFormat("dd.MM.yyyy");
    public static final DateFormat dateFormatUs = new SimpleDateFormat("yyyy-MM-dd");

    public static final HttpClient httpClient = HttpClient.newBuilder()
            .version(HttpClient.Version.HTTP_2)
            .followRedirects(HttpClient.Redirect.NORMAL)
            .connectTimeout(Duration.ofSeconds(10))
            .build();

    private final boolean isTestMode;
    private final boolean isSandbox;

    private String tcsAccountId;
    private final String tcsApiKey;

    public MainConfig() throws Exception {
        final Properties properties = PropertiesUtils.loadProperties();
        this.isTestMode = Boolean.parseBoolean(properties.getProperty("tcs.testMode", "false"));
        this.isSandbox = Boolean.parseBoolean(properties.getProperty("tcs.isSandbox", "false"));
        this.tcsAccountId = properties.getProperty("tcs.accountId");
        this.tcsApiKey = properties.getProperty("tcs.apiKey");
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

    public MainConfig withAccountId(String accountId) {
        this.tcsAccountId = accountId;
        return this;
    }

    public String getTcsApiKey() {
        return tcsApiKey;
    }
}
