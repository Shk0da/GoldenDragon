package com.github.shk0da.GoldenDragon.service;

import com.github.shk0da.GoldenDragon.model.Market;
import com.github.shk0da.GoldenDragon.model.ScanRequest;
import com.github.shk0da.GoldenDragon.model.ScanRequest.Filter;
import com.github.shk0da.GoldenDragon.model.ScanRequest.Options;
import com.github.shk0da.GoldenDragon.model.ScanRequest.Symbols;
import com.github.shk0da.GoldenDragon.model.TickerScan;
import com.google.gson.Gson;
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
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.TimeUnit;

import static com.github.shk0da.GoldenDragon.config.MainConfig.HEADER_USER_AGENT;
import static com.github.shk0da.GoldenDragon.config.MainConfig.USER_AGENT;
import static com.github.shk0da.GoldenDragon.config.MainConfig.httpClient;
import static com.github.shk0da.GoldenDragon.utils.RequestUtils.requestWithRetry;
import static java.lang.System.out;
import static java.util.stream.Collectors.toList;

public class TradingViewService {

    public static final TradingViewService INSTANCE = new TradingViewService();

    private static final String US_SCAN_URI = "https://scanner.tradingview.com/america/scan";
    private static final String MOEX_SCAN_URI = "https://scanner.tradingview.com/russia/scan";

    public List<TickerScan> scanMarket(Market market, int size) {
        ScanRequest scanRequest = new ScanRequest(
                List.of(
                        new Filter("debt_to_equity", "nempty"),
                        new Filter("type", "in_range", List.of("stock")),
                        new Filter("subtype", "in_range", List.of("common")),
                        new Filter("market_cap_basic", "egreater", 50_000_000),
                        new Filter("Recommend.All|1M", "egreater", 0.5),
                        new Filter("debt_to_equity", "in_range", List.of(-50, 3)),
                        new Filter("total_revenue", "egreater", 0),
                        new Filter("number_of_employees", "egreater", 1000)
                ),
                new Options("en"),
                List.of(
                        "name",
                        "description",
                        "total_debt",
                        "debt_to_equity",
                        "type",
                        "subtype",
                        "Recommend.All|1M"
                ),
                new ScanRequest.Sort("debt_to_equity", "asc"),
                new int[]{0, size}
        );
        return scanMarket(market, scanRequest);
    }

    public List<TickerScan> scanMarket(Market market, Collection<String> symbols) {
        List<String> tickers = new ArrayList<>(symbols.size() * 2);
        if (Market.US == market) {
            tickers.addAll(symbols.stream().map(name -> "NASDAQ:" + name).collect(toList()));
            tickers.addAll(symbols.stream().map(name -> "NYSE:" + name).collect(toList()));
        }
        if (Market.MOEX == market) {
            tickers.addAll(symbols.stream().map(name -> "MOEX:" + name).collect(toList()));
        }

        List<Filter> filters = new ArrayList<>();
        if (Market.US == market) {
            filters.add(new Filter("debt_to_equity", "nempty"));
            filters.add(new Filter("type", "in_range", List.of("stock")));
            filters.add(new Filter("subtype", "in_range", List.of("common")));
            filters.add(new Filter("market_cap_basic", "egreater", 50_000_000));
            filters.add(new Filter("Recommend.All|1M", "egreater", 0.5));
            filters.add(new Filter("debt_to_equity", "in_range", List.of(-50, 3)));
            filters.add(new Filter("number_of_employees", "egreater", 1000));
        }
        if (Market.MOEX == market) {
            filters.add(new Filter("Recommend.All|1M", "egreater", 0.4));
        }
        filters.add(new Filter("total_revenue", "egreater", 0));

        ScanRequest scanRequest = new ScanRequest(
                filters,
                new Options("en"),
                new Symbols(tickers),
                List.of(
                        "name",
                        "description",
                        "total_debt",
                        "debt_to_equity",
                        "type",
                        "subtype",
                        "Recommend.All|1M"
                )
        );
        return scanMarket(market, scanRequest);
    }

    public List<TickerScan> scanMarket(Market market, ScanRequest scanRequest) {
        try {
            int randomDelay = ThreadLocalRandom.current().nextInt(800, 1000);
            TimeUnit.MILLISECONDS.sleep(randomDelay);
        } catch (InterruptedException ex) {
            out.println("Error: " + ex.getMessage());
        }

        HttpResponse<String> response = requestWithRetry(() -> {
            String json = new Gson().toJson(scanRequest);
            HttpRequest request = HttpRequest.newBuilder()
                    .uri(URI.create(Market.MOEX == market ? MOEX_SCAN_URI : US_SCAN_URI))
                    .setHeader(HEADER_USER_AGENT, USER_AGENT)
                    .POST(HttpRequest.BodyPublishers.ofString(json))
                    .timeout(Duration.of(10, ChronoUnit.SECONDS))
                    .build();
            try {
                return httpClient.send(request, HttpResponse.BodyHandlers.ofString());
            } catch (Exception ex) {
                out.println("Error: " + ex.getMessage());
                return null;
            }
        });

        JsonObject payload = JsonParser.parseString(response.body()).getAsJsonObject();
        int totalCount = payload.get("totalCount").getAsInt();
        out.printf("The scan found %s tickers\n", totalCount);

        if (totalCount == 0) {
            return List.of();
        }

        int resultCount = null != scanRequest.getRange() ? Math.min(scanRequest.getRange()[1], totalCount) : totalCount;
        List<TickerScan> result = new ArrayList<>(resultCount);
        JsonArray data = payload.get("data").getAsJsonArray();
        for (JsonElement item : data) {
            var obj = item.getAsJsonObject();
            var values = obj.get("d").getAsJsonArray();
            var nameWithMarket = obj.get("s").getAsString().split(":");
            result.add(new TickerScan(
                    nameWithMarket[1],
                    nameWithMarket[0],
                    values.get(1).getAsString(),
                    values.get(2).getAsLong(),
                    values.get(3).getAsDouble(),
                    values.get(4).getAsString(),
                    values.get(5).getAsString(),
                    values.get(6).getAsDouble(),
                    new Date()
            ));
        }
        return result;
    }
}
