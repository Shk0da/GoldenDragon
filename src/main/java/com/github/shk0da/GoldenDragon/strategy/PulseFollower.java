package com.github.shk0da.GoldenDragon.strategy;

import com.github.shk0da.GoldenDragon.config.PulseConfig;
import com.github.shk0da.GoldenDragon.model.InstrumentInfo;
import com.github.shk0da.GoldenDragon.model.OperationInfo;
import com.github.shk0da.GoldenDragon.service.TCSService;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.sun.net.httpserver.HttpServer;

import java.io.IOException;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.net.URI;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.OffsetDateTime;
import java.time.temporal.ChronoUnit;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
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
import static java.util.Map.entry;

public class PulseFollower {

    public static final String PING_API = "https://www.tinkoff.ru/api/common/v1/ping?appName=invest&appVersion=2.0.0&sessionid=${sessionId}";
    public static final String SESSION_STATUS_API = "https://www.tinkoff.ru/api/common/v1/session_status?appName=invest&appVersion=2.0.0&sessionid=${sessionId}";

    public static final String PULSE_INSTRUMENT_API = "https://api-invest-gw.tinkoff.ru/social/v1/profile/${profileId}/instrument?limit=30&sessionId=${sessionId}&appName=socialweb&appVersion=1.383.0&origin=web&platform=web";
    public static final String PULSE_OPERATION_API = "https://api-invest-gw.tinkoff.ru/social/v1/profile/${profileId}/operation/instrument/${tickerName}/${tickerClass}?limit=30&sessionId=${sessionId}&appName=socialweb&appVersion=1.383.0&origin=web&platform=web";

    private final PulseConfig pulseConfig;
    private final TCSService tcsService;

    private final AtomicReference<String> sessionId = new AtomicReference<>();
    private final ExecutorService sessionWatcher = Executors.newSingleThreadExecutor();
    private final ExecutorService httpServer = Executors.newSingleThreadExecutor();

    public PulseFollower(PulseConfig pulseConfig, TCSService tcsService) {
        this.pulseConfig = pulseConfig;
        this.tcsService = tcsService;
        this.sessionId.set(pulseConfig.getFollowSessionId());
    }

    public void run() {
        runSessionWatcher();
        runHttpServer(pulseConfig.getHttpPort());
        runFollow(pulseConfig.getFollowProfileId(), pulseConfig.getMaxPositions());
    }

    private void runFollow(String[] profileIds, int maxPositions) {
        out.printf("Start follow for profileIds=%s\n", Arrays.toString(profileIds));

        Map<String, OffsetDateTime> lastWatchedTrade = new HashMap<>();
        for (String profileId : profileIds) {
            lastWatchedTrade.put(profileId, OffsetDateTime.now());
        }

        while (true) {
            for (String profileId : profileIds) {
                try {
                    Map<OffsetDateTime, OperationInfo> operationsByDateTime = new TreeMap<>();
                    Map<OffsetDateTime, InstrumentInfo> instrumentsByDateTime = getInstruments(profileId, sessionId.get());
                    instrumentsByDateTime.forEach((dateTme, item) -> {
                        if (lastWatchedTrade.get(profileId).isBefore(dateTme)) {
                            operationsByDateTime.putAll(getOperations(profileId, sessionId.get(), item));
                        }
                    });

                    Set<Map.Entry<String, InstrumentInfo>> uniqueCheck = new HashSet<>();
                    operationsByDateTime.forEach((operationDateTme, operation) -> {
                        InstrumentInfo instrument = operation.getInstrument();
                        boolean hasSameTrade = uniqueCheck.contains(entry(operation.getAction(), instrument));
                        if (lastWatchedTrade.get(profileId).isBefore(operationDateTme) && !hasSameTrade) {
                            uniqueCheck.add(entry(operation.getAction(), instrument));
                            out.printf("[%s] Operation [%s]: %s\n", profileId, instrument.getTicker(), operation);
                            lastWatchedTrade.put(profileId, operationDateTme);
                            telegramNotifyService.sendMessage(
                                    String.format(
                                            "PulseFollower: time=%s, profileId=%s, action=%s, ticker=%s",
                                            new Date(), profileId, operation.getAction(), instrument.getTicker()
                                    )
                            );
                            handleOperation(operation, instrument, maxPositions);
                        }
                    });
                    TimeUnit.SECONDS.sleep(5);
                } catch (Exception ex) {
                    out.println("Error: " + ex.getMessage());
                }
            }
        }
    }

    private void handleOperation(OperationInfo operation, InstrumentInfo instrument, int maxPositions) {
        switch (operation.getAction()) {
            case "buy":
                int countOfCurrentPositions = tcsService.getCountOfCurrentPositions();
                if (countOfCurrentPositions < maxPositions) {
                    double availableCash = tcsService.getAvailableCash();
                    double totalPortfolioCost = tcsService.getTotalPortfolioCost();
                    int availablePositions = maxPositions - countOfCurrentPositions;
                    double cost = Math.min(totalPortfolioCost / availablePositions, availableCash);
                    out.printf(
                            "availableCash=%f, totalPortfolioCost=%f, availablePositions=%d, cost=%f\n",
                            availableCash, totalPortfolioCost, availablePositions, cost
                    );
                    tcsService.buy(instrument.getTicker(), instrument.getTickerType(), cost);
                }
                break;
            case "sell":
                tcsService.sellAllByMarket(instrument.getTicker(), instrument.getTickerType());
                break;
        }
    }

    private static Map<OffsetDateTime, InstrumentInfo> getInstruments(String profileId, String sessionId) {
        String instrumentsJson = executeHttpGet(
                PULSE_INSTRUMENT_API
                        .replace("${profileId}", profileId)
                        .replace("${sessionId}", sessionId)
        );
        if (null == instrumentsJson || instrumentsJson.isBlank()) {
            throw new RuntimeException("Failed get instruments");
        }

        JsonObject payload = JsonParser.parseString(instrumentsJson)
                .getAsJsonObject()
                .get("payload")
                .getAsJsonObject();
        if (null != payload.get("code") && "Error".equals(payload.get("code").getAsString())) {
            throw new RuntimeException("Failed get instruments [" + profileId + "]: " + payload.get("message").getAsString());
        }

        JsonArray instruments = payload.get("items").getAsJsonArray();
        Map<OffsetDateTime, InstrumentInfo> instrumentsByDateTime = new TreeMap<>();
        for (JsonElement instrument : instruments) {
            try {
                JsonObject item = instrument.getAsJsonObject();
                InstrumentInfo instrumentInfo = InstrumentInfo.of(item);
                instrumentsByDateTime.put(instrumentInfo.getMaxTradeDateTime(), instrumentInfo);
            } catch (Exception ex) {
                out.println("Error: Failed parse payload: " + ex.getMessage());
            }
        }
        return instrumentsByDateTime;
    }

    private static Map<OffsetDateTime, OperationInfo> getOperations(String profileId, String sessionId, InstrumentInfo instrumentInfo) {
        String operationsJson = executeHttpGet(
                PULSE_OPERATION_API
                        .replace("${profileId}", profileId)
                        .replace("${sessionId}", sessionId)
                        .replace("${tickerName}", instrumentInfo.getTicker())
                        .replace("${tickerClass}", instrumentInfo.getClassCode())
        );
        if (null == operationsJson || operationsJson.isBlank()) {
            return new TreeMap<>();
        }

        JsonObject payload = JsonParser.parseString(operationsJson)
                .getAsJsonObject()
                .get("payload")
                .getAsJsonObject();
        if (null != payload.get("code") && "Error".equals(payload.get("code").getAsString())) {
            throw new RuntimeException("Failed get operations: " + payload.get("message").getAsString());
        }

        JsonArray operations = payload.get("items").getAsJsonArray();
        Map<OffsetDateTime, OperationInfo> operationsByDateTime = new TreeMap<>();
        for (JsonElement instrument : operations) {
            try {
                JsonObject item = instrument.getAsJsonObject();
                OperationInfo operationInfo = OperationInfo.of(item).withInstrument(instrumentInfo);
                operationsByDateTime.put(operationInfo.getTradeDateTime(), operationInfo);
            } catch (Exception ex) {
                out.println("Error: Failed parse payload: " + ex.getMessage());
            }
        }
        return operationsByDateTime;
    }

    private void runHttpServer(int serverPort) {
        out.printf("Start http server: //0.0.0.0:%d/api/update?sessionId=${sessionId}\n", serverPort);
        try {
            HttpServer server = HttpServer.create(new InetSocketAddress(serverPort), 0);
            server.createContext("/api/update", (exchange -> {
                String newSessionId = exchange.getRequestURI().getQuery().split("=")[1];
                sessionId.set(newSessionId);

                String respText = "New sessionId=" + newSessionId;
                exchange.sendResponseHeaders(200, respText.getBytes().length);
                OutputStream output = exchange.getResponseBody();
                output.write(respText.getBytes());
                output.flush();
                exchange.close();
            }));
            server.setExecutor(httpServer);
            server.start();
        } catch (IOException ex) {
            out.println("Error: " + ex.getMessage());
        }
    }

    private void runSessionWatcher() {
        out.printf("Start session watcher for sessionId=%s\n", sessionId.get());
        sessionWatcher.execute(() -> {
            long startTime = 0L;
            while (true) {
                try {
                    executePing(sessionId.get());
                    if (0L == startTime || System.currentTimeMillis() - startTime >= 2 * 60 * 1000) {
                        executeSessionStatus(sessionId.get());
                        startTime = System.currentTimeMillis();
                    }
                    TimeUnit.MINUTES.sleep(1);
                } catch (InterruptedException ex) {
                    out.println("Error: " + ex.getMessage());
                }
            }
        });
    }

    private void executePing(String sessionId) {
        String response = executeHttpGet(PING_API.replace("${sessionId}", sessionId));
        out.printf("Ping: %s\n", response);
    }

    private void executeSessionStatus(String sessionId) {
        String response = executeHttpGet(SESSION_STATUS_API.replace("${sessionId}", sessionId));
        out.printf("SessionStatus: %s\n", response);
        if (null == response) return;

        JsonObject json = JsonParser.parseString(response).getAsJsonObject();
        if (json.has("errorMessage")) {
            telegramNotifyService.sendMessage("PulseFollower: " + json.get("plainMessage"));
        }
        if (json.has("payload")) {
            JsonObject payload = json.get("payload").getAsJsonObject();
            int ssoTokenExpiresIn = payload.get("ssoTokenExpiresIn").getAsInt();
            if (ssoTokenExpiresIn <= 2_000) {
                telegramNotifyService.sendMessage("PulseFollower: ssoTokenExpiresIn=" + ssoTokenExpiresIn);
            }
        }
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
            TimeUnit.MILLISECONDS.sleep(ThreadLocalRandom.current().nextInt(1_000, 2_000));
        } catch (InterruptedException ex) {
            out.println("Error: " + ex.getMessage());
        }
    }
}
