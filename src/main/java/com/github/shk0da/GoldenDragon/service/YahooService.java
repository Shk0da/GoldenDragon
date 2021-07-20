package com.github.shk0da.GoldenDragon.service;

import com.github.shk0da.GoldenDragon.model.Fundamental;
import com.github.shk0da.GoldenDragon.model.TickerCandle;
import com.github.shk0da.GoldenDragon.model.TickerScan;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

import java.net.URI;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Duration;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import static com.github.shk0da.GoldenDragon.config.MainConfig.HEADER_USER_AGENT;
import static com.github.shk0da.GoldenDragon.config.MainConfig.USER_AGENT;
import static com.github.shk0da.GoldenDragon.config.MainConfig.dateFormat;
import static com.github.shk0da.GoldenDragon.config.MainConfig.httpClient;
import static com.github.shk0da.GoldenDragon.utils.RequestUtils.requestWithRetry;
import static java.lang.System.currentTimeMillis;
import static java.lang.System.out;
import static java.util.concurrent.ThreadLocalRandom.current;
import static java.util.concurrent.TimeUnit.DAYS;

public class YahooService {

    public static final YahooService INSTANCE = new YahooService();

    private static final String HISTORY_DATA_URI = "https://query1.finance.yahoo.com/v7/finance/download";
    private static final String BALANCE_SHEET_URI = "https://query1.finance.yahoo.com/ws/fundamentals-timeseries/v1/finance/timeseries";

    public TickerCandle getLastCandle(String symbol) {
        try {
            List<TickerCandle> historyData = getHistoryData(symbol, new Date(currentTimeMillis() - DAYS.toMillis(daysFromSize(1))), new Date());
            return historyData.isEmpty() ? null : historyData.get(historyData.size() - 1);
        } catch (Exception ex) {
            out.printf("Failed downloaded new candles for '%s', %s\n", symbol, ex.getMessage());
            return null;
        }
    }

    public List<TickerCandle> getLastCandles(String symbol, int count) {
        try {
            List<TickerCandle> historyData = getHistoryData(symbol, new Date(currentTimeMillis() - DAYS.toMillis(daysFromSize(count))), new Date());
            out.printf("Downloaded %s new candles for '%s'\n", historyData.size(), symbol);
            return historyData;
        } catch (Exception ex) {
            out.printf("Failed downloaded new candles for '%s', %s\n", symbol, ex.getMessage());
            return List.of();
        }
    }

    public double[] getLongTermDebtEquityAndTotalDebtEquityRatio(TickerScan ticker) {
        var options = List.of(
                "quarterlyRetainedEarnings",
                "quarterlyStockholdersEquity",
                "quarterlyLongTermDebtAndCapitalLeaseObligation",
                "quarterlyCurrentDebtAndCapitalLeaseObligation",
                "quarterlyLongTermDebtAndCapitalLeaseObligation"
        );
        var fundamental = getTickerFundamental(ticker, options);
        var currentDebtAndCapitalLeaseObligation = fundamental.get("quarterlyCurrentDebtAndCapitalLeaseObligation");
        var longTermDebtAndCapitalLeaseObligation = fundamental.get("quarterlyLongTermDebtAndCapitalLeaseObligation");
        var stockholdersEquity = fundamental.get("quarterlyStockholdersEquity");
        var retainedEarnings = fundamental.get("quarterlyRetainedEarnings");

        var longTermDebtEquityRatio = 0.0;
        if (longTermDebtAndCapitalLeaseObligation != null && stockholdersEquity != null && retainedEarnings != null) {
            var diff = (stockholdersEquity.getValueRaw() - retainedEarnings.getValueRaw());
            longTermDebtEquityRatio = longTermDebtAndCapitalLeaseObligation.getValueRaw() / diff;
        }

        var totalDebtEquityRatio = 0.0;
        if (currentDebtAndCapitalLeaseObligation != null && longTermDebtAndCapitalLeaseObligation != null
                && stockholdersEquity != null && retainedEarnings != null) {
            var debtAndCapitalLeaseObligation = currentDebtAndCapitalLeaseObligation.getValueRaw() + longTermDebtAndCapitalLeaseObligation.getValueRaw();
            totalDebtEquityRatio = debtAndCapitalLeaseObligation / (stockholdersEquity.getValueRaw() - retainedEarnings.getValueRaw());
        }

        return new double[]{longTermDebtEquityRatio, totalDebtEquityRatio};
    }

    private List<TickerCandle> getHistoryData(String symbol, Date periodStart, Date periodEnd) {
        try {
            int randomDelay = current().nextInt(150, 350);
            TimeUnit.MILLISECONDS.sleep(randomDelay);
        } catch (InterruptedException ex) {
            out.println("Error: " + ex.getMessage());
        }

        HttpResponse<String> response = requestWithRetry(() -> {
            String uri = String.format(
                    "%s/%s?period1=%s&period2=%s&interval=%s&events=history",
                    HISTORY_DATA_URI, symbol, periodStart.getTime() / 1000, periodEnd.getTime() / 1000, "1d"
            );
            HttpRequest request = HttpRequest.newBuilder()
                    .GET()
                    .uri(URI.create(uri))
                    .setHeader(HEADER_USER_AGENT, USER_AGENT)
                    .timeout(Duration.of(10, ChronoUnit.SECONDS))
                    .build();
            try {
                return httpClient.send(request, HttpResponse.BodyHandlers.ofString());
            } catch (Exception ex) {
                out.println("Error: " + ex.getMessage());
                return null;
            }
        });

        return response.body().lines()
                .skip(1)
                .map(it -> it.split(","))
                .filter(it -> null != it[0] && !it[0].isBlank() && !it[0].equals("null"))
                .map(it -> new TickerCandle(
                        symbol,
                        it[0],
                        parseDouble(it[1]),
                        parseDouble(it[2]),
                        parseDouble(it[3]),
                        parseDouble(it[4]),
                        parseDouble(it[5]),
                        parseInt(it[6])
                ))
                .filter(it -> null != it.getClose() && it.getClose() > 0.0)
                .collect(Collectors.toList());
    }

    private Map<String, Fundamental> getTickerFundamental(TickerScan ticker, List<String> options) {
        try {
            int randomDelay = current().nextInt(200, 500);
            TimeUnit.MILLISECONDS.sleep(randomDelay);
        } catch (InterruptedException ex) {
            out.println("Error: " + ex.getMessage());
        }

        Map<String, Fundamental> result = new HashMap<>();
        try {
            String type = String.join(",", options);
            HttpResponse<String> response = requestWithRetry(() -> {
                long periodStart = new Date(currentTimeMillis() - DAYS.toMillis(90)).getTime() / 1000;
                long periodEnd = new Date(currentTimeMillis() + DAYS.toMillis(365)).getTime() / 1000;
                String uri = String.format(
                        "%s/%s?region=%s&symbol=%s&type=%s&lang=en-US&padTimeSeries=true&merge=false&period1=%s&period2=%s&corsDomain=finance.yahoo.com",
                        BALANCE_SHEET_URI, ticker.getName(), "US", ticker.getName(), type, periodStart, periodEnd
                );
                HttpRequest request = HttpRequest.newBuilder()
                        .GET()
                        .uri(URI.create(uri))
                        .setHeader(HEADER_USER_AGENT, USER_AGENT)
                        .timeout(Duration.of(10, ChronoUnit.SECONDS))
                        .build();
                try {
                    return httpClient.send(request, HttpResponse.BodyHandlers.ofString());
                } catch (Exception ex) {
                    out.println("Error: " + ex.getMessage());
                    return null;
                }
            });
            out.printf("Downloaded new fundamentals for '%s' [%s]\n", ticker.getName(), ticker.getDescription());

            JsonObject payload = JsonParser.parseString(response.body()).getAsJsonObject();
            JsonArray timeSeries = payload.get("timeseries").getAsJsonObject().get("result").getAsJsonArray();
            for (JsonElement element : timeSeries) {
                for (String option : options) {
                    if (!element.getAsJsonObject().has(option)) continue;

                    JsonArray values = element.getAsJsonObject().get(option).getAsJsonArray();
                    List<JsonElement> elements = new ArrayList<>();
                    values.iterator().forEachRemaining(it -> {
                        if (null != it) {
                            elements.add(it);
                        }
                    });
                    if (elements.isEmpty()) continue;

                    var item = elements.get(elements.size() - 1).getAsJsonObject();
                    Date date;
                    try {
                        date = null != item.get("asOfDate") ? dateFormat.parse(item.get("asOfDate").getAsString()) : new Date();
                    } catch (Exception skip) {
                        date = new Date();
                    }
                    JsonObject value = item.get("reportedValue").getAsJsonObject();
                    Fundamental fundamental = new Fundamental(
                            ticker.getName(),
                            option,
                            date,
                            item.get("periodType").getAsString(),
                            item.get("currencyCode").getAsString(),
                            value.get("raw").getAsDouble(),
                            value.get("fmt").getAsString()
                    );
                    if (!result.containsKey(option) || result.get(option).getDateTime().before(date)) {
                        result.put(option, fundamental);
                    }
                }
            }
        } catch (Exception ex) {
            out.println("Error get TickerFundamental: " + ex.getMessage());
        }
        return result;
    }

    private long daysFromSize(int size) {
        var days = 0;
        var workDays = 5;
        for (int i = 1; i < size; i++) {
            days++;
            if (--workDays == 0) {
                workDays = 5;
                days += 2;
            }
        }
        return (days + 10);
    }

    private int parseInt(String it) {
        return null != it && !it.isBlank() && !it.equals("null") ? Integer.parseInt(it) : 0;
    }

    private double parseDouble(String it) {
        return null != it && !it.isBlank() && !it.equals("null") ? Double.parseDouble(it) : 0.0;
    }
}