package com.github.shk0da.GoldenDragon.config;

import com.github.shk0da.GoldenDragon.model.Market;

import java.io.InputStream;
import java.net.http.HttpClient;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Properties;

public class MainConfig {

    public static final String HEADER_AUTHORIZATION = "Authorization";
    public static final String HEADER_USER_AGENT = "User-Agent";
    public static final String USER_AGENT = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/87.0.4280.88 Safari/537.36";

    public static final String SMART_LAB_DIV_CALENDAR = "https://smart-lab.ru/dividends/index/order_by_cut_off_date/desc/?is_approved=1";
    public static final String DOHOD_DIV_CALENDAR = "https://www.dohod.ru/ik/analytics/dividend";
    public static final String INVESTING_DIV_CALENDAR = "https://ru.investing.com/dividends-calendar/Service/getCalendarFilteredData";

    public static final String SCAN_US = "https://scanner.tradingview.com/america/scan";
    public static final String SCAN_MOEX = "https://scanner.tradingview.com/russia/scan";

    public static final String TCS_STREAMING_API = "wss://api-invest.tinkoff.ru/openapi/md/v1/md-openapi/ws";

    private static final String TCS_API = "https://api-invest.tinkoff.ru/openapi/";
    private static final String TCS_SANDBOX_API = "https://api-invest.tinkoff.ru/openapi/sandbox/";

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

    private final String tcsAccountId;
    private final String tcsAuthorization;
    private final String tcsApi;

    private final MarketConfig marketConfig;

    public MainConfig(Market market) throws Exception {
        InputStream inputStream = getClass().getClassLoader().getResourceAsStream("application.properties");
        final Properties properties = new Properties();
        properties.load(Objects.requireNonNull(inputStream));

        this.isTestMode = Boolean.parseBoolean(properties.getProperty("tcs.testMode", "false"));
        this.isSandbox = Boolean.parseBoolean(properties.getProperty("tcs.isSandbox", "false"));
        this.tcsAccountId = properties.getProperty("tcs.accountId");
        this.tcsAuthorization = "Bearer " + properties.getProperty("tcs.apiKey");
        this.tcsApi = isSandbox() ? TCS_SANDBOX_API : TCS_API;

        MarketConfig marketConfig;
        switch (market) {
            case US:
                marketConfig = new MarketConfig(Market.US, 16, 19, 1_000, "USD");
                break;
            case MOEX:
                marketConfig = new MarketConfig(Market.MOEX, 10, 18, 100_000, "RUB");
                break;
            default:
                marketConfig = MarketConfig.createDefault();
        }
        this.marketConfig = marketConfig;
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

    public String getTcsAuthorization() {
        return tcsAuthorization;
    }

    public String getTcsApi() {
        return tcsApi;
    }

    public MarketConfig getMarketConfig() {
        return marketConfig;
    }
}
