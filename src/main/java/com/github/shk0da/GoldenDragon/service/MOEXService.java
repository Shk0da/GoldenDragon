package com.github.shk0da.GoldenDragon.service;

import com.github.shk0da.GoldenDragon.model.TickerCandle;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.text.SimpleDateFormat;
import java.time.Duration;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import static java.lang.System.out;

public class MOEXService {

    public static final MOEXService INSTANCE = new MOEXService();

    private final SimpleDateFormat dateFormatter = new SimpleDateFormat("yyyy-MM-dd");
    private final HttpClient httpClient = HttpClient.newHttpClient();

    public List<TickerCandle> getLastCandles(String symbol, int size) {
        Date now = new Date();
        List<TickerCandle> candles = new ArrayList<>();
        Date startDate = new Date(now.getTime() - Duration.ofDays(daysFromSize(size)).toMillis());
        addCandles(symbol, startDate, now, candles);
        return candles.stream()
                .sorted((c1, c2) -> c2.getDate().compareTo(c1.getDate()))
                .limit(size)
                .sorted(Comparator.comparing(TickerCandle::getDate))
                .collect(Collectors.toList());
    }

    private long daysFromSize(int size) {
        int days = 0;
        int workDays = 5;
        for (int day = 1; day <= size; day++) {
            days++;
            if (--workDays == 0) {
                workDays = 5;
                days += 2;
            }
        }
        return (days + 10);
    }

    private void addCandles(String symbol, Date lastCandle, Date now, List<TickerCandle> candles) {
        try {
            Thread.sleep((long) (Math.random() * 5 + 1) * 1000);
            List<TickerCandle> historyData = historyData(symbol, lastCandle, now);
            out.printf("Downloaded %d new candles for '%s'\n", historyData.size(), symbol);
            candles.addAll(historyData);
        } catch (Exception ex) {
            out.printf("Failed downloaded new candles for '%s', %s\n", symbol, ex.getMessage());
        }
    }

    private List<TickerCandle> historyData(String symbol, Date lastCandle, Date now) throws IOException, InterruptedException {
        String from = dateFormatter.format(lastCandle);
        String till = dateFormatter.format(now);
        String url = "http://iss.moex.com/iss/history/engines/stock/markets/shares/boards/tqbr/securities/" + symbol + ".csv?from=" + from + "&till=" + till;
        HttpRequest historyRequest = HttpRequest.newBuilder()
                .uri(URI.create(url))
                .timeout(Duration.of(10, ChronoUnit.SECONDS))
                .GET()
                .build();
        HttpResponse<String> response = httpClient.send(historyRequest, HttpResponse.BodyHandlers.ofString());
        // try ETFs
        if (response.body().lines().skip(3).map(String::trim).filter(s -> !s.isBlank()).findFirst().isEmpty()) {
            url = "http://iss.moex.com/iss/history/engines/stock/markets/shares/boards/tqtf/securities/" + symbol + ".csv?from=" + from + "&till=" + till;
            historyRequest = HttpRequest.newBuilder()
                    .uri(URI.create(url))
                    .timeout(Duration.of(10, ChronoUnit.SECONDS))
                    .GET()
                    .build();
            response = httpClient.send(historyRequest, HttpResponse.BodyHandlers.ofString());
        }
        return response.body().lines()
                .skip(3)
                .filter(s -> !s.trim().isBlank())
                .map(line -> line.split(";"))
                .map(arr -> new TickerCandle(
                        symbol,
                        arr[1],
                        Double.parseDouble(arr[6]),
                        Double.parseDouble(arr[8]),
                        Double.parseDouble(arr[7]),
                        Double.parseDouble(arr[11]),
                        Double.parseDouble(arr[9]),
                        (int) Double.parseDouble(arr[5])
                ))
                .collect(Collectors.toList());
    }
}