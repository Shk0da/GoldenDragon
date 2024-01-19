package com.github.shk0da.GoldenDragon.strategy;

import com.github.shk0da.GoldenDragon.config.NewsPusherConfig;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

import java.net.URI;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Map;
import java.util.TreeMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

import static com.github.shk0da.GoldenDragon.config.MainConfig.HEADER_USER_AGENT;
import static com.github.shk0da.GoldenDragon.config.MainConfig.USER_AGENT;
import static com.github.shk0da.GoldenDragon.config.MainConfig.httpClient;
import static com.github.shk0da.GoldenDragon.service.TelegramNotifyService.telegramNotifyService;
import static com.github.shk0da.GoldenDragon.utils.RequestUtils.requestWithRetry;
import static java.lang.System.out;
import static java.time.format.DateTimeFormatter.ISO_LOCAL_DATE_TIME;

public class NewsPusher {

    private final NewsPusherConfig newsPusherConfig;

    private final ExecutorService newsFeeder = Executors.newSingleThreadExecutor();

    public NewsPusher(NewsPusherConfig newsPusherConfig) {
        this.newsPusherConfig = newsPusherConfig;
    }

    public void run() {
        runNewsFeeder(newsPusherConfig.getNewsSource());
    }

    private void runNewsFeeder(String newsSource) {
        if (null == newsSource || newsSource.isBlank()) return;

        out.printf("Start news feeder for=%s\n", newsSource);
        newsFeeder.execute(() -> {
            AtomicReference<LocalDateTime> lastWatch = new AtomicReference<>(LocalDateTime.now());
            while (true) {
                try {
                    TimeUnit.SECONDS.sleep(1);

                    String response = executeHttpGet(newsSource);
                    if (null == response) continue;

                    Map<LocalDateTime, JsonObject> news = new TreeMap<>((k1, k2) -> k2.isBefore(k1) ? 1 : k2.equals(k1) ? 0 : -1);
                    JsonArray items = JsonParser.parseString(response).getAsJsonArray();
                    items.forEach(item -> {
                        var publishDate = LocalDateTime.parse(item.getAsJsonObject().get("publishDate").getAsString(), ISO_LOCAL_DATE_TIME);
                        news.put(publishDate, item.getAsJsonObject());
                    });

                    news.forEach((publishDate, it) -> {
                        if (publishDate.isAfter(lastWatch.get())) {
                            var symbols = new ArrayList<String>();
                            for (JsonElement symbol : it.getAsJsonObject().get("symbols").getAsJsonArray()) {
                                if (!symbol.getAsString().isBlank()) {
                                    symbols.add(symbol.getAsString());
                                }
                            }
                            if (!symbols.isEmpty()) {
                                var tickers = new StringBuilder();
                                symbols.forEach(symbol -> tickers.append("#").append(symbol).append(" "));
                                var header = it.getAsJsonObject().get("header").getAsString();
                                var message = String.format("News: %s %s [%s]", publishDate, header, tickers);
                                telegramNotifyService.sendMessage(message);
                                out.println(message);
                            }
                            lastWatch.set(publishDate);
                        }
                    });
                } catch (InterruptedException ex) {
                    out.println("Error: " + ex.getMessage());
                }
            }
        });
    }

    private static String executeHttpGet(String url) {
        randomSleep();
        HttpResponse<String> response = requestWithRetry(() -> {
            HttpRequest request = HttpRequest.newBuilder()
                    .GET()
                    .uri(URI.create(url))
                    .setHeader(HEADER_USER_AGENT, USER_AGENT)
                    .build();
            try {
                return httpClient.send(request, HttpResponse.BodyHandlers.ofString());
            } catch (Exception ex) {
                out.println("Error: " + ex.getMessage());
                return null;
            }
        });
        if (response.statusCode() >= 400) {
            out.printf("Error execute %s: %d\n", url, response.statusCode());
            return null;
        }
        return response.body();
    }

    private static void randomSleep() {
        try {
            TimeUnit.MILLISECONDS.sleep(ThreadLocalRandom.current().nextInt(750, 1_000));
        } catch (InterruptedException ex) {
            out.println("Error: " + ex.getMessage());
        }
    }
}
