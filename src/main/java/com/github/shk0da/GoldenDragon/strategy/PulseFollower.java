package com.github.shk0da.GoldenDragon.strategy;

import com.github.shk0da.GoldenDragon.config.PulseConfig;
import com.github.shk0da.GoldenDragon.model.InstrumentInfo;
import com.github.shk0da.GoldenDragon.model.OperationInfo;
import com.github.shk0da.GoldenDragon.service.TCSService;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

import java.net.URI;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.OffsetDateTime;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.TreeMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

import static com.github.shk0da.GoldenDragon.config.MainConfig.HEADER_COOKIES;
import static com.github.shk0da.GoldenDragon.config.MainConfig.HEADER_USER_AGENT;
import static com.github.shk0da.GoldenDragon.config.MainConfig.USER_AGENT;
import static com.github.shk0da.GoldenDragon.config.MainConfig.httpClient;
import static com.github.shk0da.GoldenDragon.utils.RequestUtils.requestWithRetry;
import static java.lang.System.out;

public class PulseFollower {

    public static final String RE_AUTHORIZE_API = "https://www.tinkoff.ru/api/common/v1/session/authorize?prompt=none&origin=web,ib5,platform,mdep";
    public static final String PING_API = "https://www.tinkoff.ru/api/common/v1/ping?appName=invest&appVersion=2.0.0&sessionid=${sessionId}";
    public static final String SESSION_STATUS_API = "https://www.tinkoff.ru/api/common/v1/session_status?appName=invest&appVersion=2.0.0&sessionid=${sessionId}";

    public static final String PULSE_INSTRUMENT_API = "https://api-invest-gw.tinkoff.ru/social/v1/profile/${profileId}/instrument?limit=30&sessionId=${sessionId}&appName=socialweb&appVersion=1.383.0&origin=web&platform=web";
    public static final String PULSE_OPERATION_API = "https://api-invest-gw.tinkoff.ru/social/v1/profile/${profileId}/operation/instrument/${tickerName}/${tickerClass}?limit=30&sessionId=${sessionId}&appName=socialweb&appVersion=1.383.0&origin=web&platform=web";

    private final PulseConfig pulseConfig;
    private final TCSService tcsService;
    private final ExecutorService sessionWatcher;

    private final AtomicReference<String> cookies = new AtomicReference<>();
    private final AtomicReference<String> sessionId = new AtomicReference<>();

    public PulseFollower(PulseConfig pulseConfig, TCSService tcsService) {
        this.pulseConfig = pulseConfig;
        this.tcsService = tcsService;
        this.sessionWatcher = Executors.newSingleThreadExecutor();

        this.cookies.set(pulseConfig.getCookies());
        this.sessionId.set(pulseConfig.getFollowSessionId());
    }

    public void run() {
        int maxPositions = pulseConfig.getMaxPositions();
        String[] profileIds = pulseConfig.getFollowProfileId();

        runSessionWatcher();
        runFollow(profileIds, maxPositions);
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

                    operationsByDateTime.forEach((operationDateTme, operation) -> {
                        if (lastWatchedTrade.get(profileId).isBefore(operationDateTme)) {
                            InstrumentInfo instrument = operation.getInstrument();
                            out.printf("[%s] Operation [%s]: %s\n", profileId, instrument.getTicker(), operation);
                            lastWatchedTrade.put(profileId, operationDateTme);
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
                    double cost = Math.min(availableCash, Math.abs(totalPortfolioCost / availablePositions));
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
            throw new RuntimeException("Failed get instruments: " + payload.get("message").getAsString());
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

    private void runSessionWatcher() {
        out.printf("Start session watcher for sessionId=%s\n", sessionId.get());
        sessionWatcher.execute(() -> {
            long startTime = System.currentTimeMillis();
            while (true) {
                try {
                    executePing(sessionId.get());
                    if (System.currentTimeMillis() - startTime >= 1.9 * 60 * 1000) {
                        executeSessionStatus(sessionId.get(), cookies.get());
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

    private void executeSessionStatus(String sessionId, String cookies) {
        String response = executeHttpGet(SESSION_STATUS_API.replace("${sessionId}", sessionId));
        out.printf("SessionStatus: %s\n", response);
        if (null == response) return;

        JsonObject payload = JsonParser.parseString(response)
                .getAsJsonObject()
                .get("payload")
                .getAsJsonObject();
        int ssoTokenExpiresIn = payload.get("ssoTokenExpiresIn").getAsInt();
        if (ssoTokenExpiresIn <= 2000) {
            String html = executeHttpGet(RE_AUTHORIZE_API, cookies.replace("${sessionId}", sessionId));
            if (null != html && !html.isBlank()) {
                int sessionIdStart = html.indexOf("\"sessionId\":\"") + 13;
                int sessionIdEnd = html.indexOf("\",\"accessLevel\":");
                String sessionIdFromFrame = html.substring(sessionIdStart, sessionIdEnd);
                out.printf("New sessionId=%s\n", sessionIdFromFrame);
                this.sessionId.set(sessionIdFromFrame);
            }
        }
    }

    private static String executeHttpGet(String url) {
        return executeHttpGet(url, null);
    }

    private static String executeHttpGet(String url, String cookies) {
        randomSleep();
        HttpResponse<String> response = requestWithRetry(() -> {
            HttpRequest.Builder requestBuilder = HttpRequest.newBuilder()
                    .GET()
                    .uri(URI.create(url))
                    .setHeader(HEADER_USER_AGENT, USER_AGENT);
            if (null != cookies) {
                requestBuilder.setHeader(HEADER_COOKIES, cookies);
            }
            HttpRequest request = requestBuilder.build();
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
