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
import java.util.Map;
import java.util.TreeMap;
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.TimeUnit;

import static com.github.shk0da.GoldenDragon.config.MainConfig.HEADER_USER_AGENT;
import static com.github.shk0da.GoldenDragon.config.MainConfig.USER_AGENT;
import static com.github.shk0da.GoldenDragon.config.MainConfig.httpClient;
import static com.github.shk0da.GoldenDragon.utils.RequestUtils.requestWithRetry;
import static java.lang.System.out;

public class PulseFollower {

    public static final String PING_API = "https://www.tinkoff.ru/api/common/v1/ping?appName=invest&appVersion=1.383.0&origin=web%2Cib5%2Cplatform&sessionid=${sessionId}&wuid=912317975229ba1b4694cef6d31d0f6d";
    public static final String SESSION_STATUS_API = "https://www.tinkoff.ru/api/common/v1/session_status?appName=invest&appVersion=1.383.0&origin=web%2Cib5%2Cplatform&sessionid=${sessionId}&wuid=912317975229ba1b4694cef6d31d0f6d";

    public static final String PULSE_INSTRUMENT_API = "https://api-invest-gw.tinkoff.ru/social/v1/profile/${profileId}/instrument?limit=30&sessionId=${sessionId}&appName=socialweb&appVersion=1.383.0&origin=web&platform=web";
    public static final String PULSE_OPERATION_API = "https://api-invest-gw.tinkoff.ru/social/v1/profile/${profileId}/operation/instrument/${tickerName}/${tickerClass}?limit=1&sessionId=${sessionId}&appName=socialweb&appVersion=1.383.0&origin=web&platform=web";

    private final PulseConfig pulseConfig;
    private final TCSService tcsService;

    public PulseFollower(PulseConfig pulseConfig, TCSService tcsService) {
        this.pulseConfig = pulseConfig;
        this.tcsService = tcsService;
    }

    public void run() {
        String profileId = pulseConfig.getFollowProfileId();
        String sessionId = pulseConfig.getFollowSessionId();

        executePing(sessionId);
        executeSessionStatus(sessionId);

        Map<OffsetDateTime, InstrumentInfo> instrumentsByDateTime = getInstruments(profileId, sessionId);
        instrumentsByDateTime.forEach((dateTme, item) -> {
            Map<OffsetDateTime, OperationInfo> operations = getOperation(profileId, sessionId, item);
            operations.forEach((operationDateTme, operation) -> {
                out.printf("operation [%s]: %s\n", item.getTicker(), operation);
            });
        });
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

    private static Map<OffsetDateTime, OperationInfo> getOperation(String profileId, String sessionId, InstrumentInfo instrumentInfo) {
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
                OperationInfo operationInfo = OperationInfo.of(item);
                operationsByDateTime.put(operationInfo.getTradeDateTime(), operationInfo);
            } catch (Exception ex) {
                out.println("Error: Failed parse payload: " + ex.getMessage());
            }
        }
        return operationsByDateTime;
    }

    private static void executePing(String sessionId) {
        String response = executeHttpGet(PING_API.replace("${sessionId}", sessionId));
        out.printf("Ping: %s\n", response);
    }

    private static void executeSessionStatus(String sessionId) {
        String response = executeHttpGet(SESSION_STATUS_API.replace("${sessionId}", sessionId));
        out.printf("SessionStatus: %s\n", response);
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
        return response.body();
    }

    private static void randomSleep() {
        try {
            TimeUnit.MILLISECONDS.sleep(ThreadLocalRandom.current().nextInt(50, 350));
        } catch (InterruptedException ex) {
            out.println("Error: " + ex.getMessage());
        }
    }
}
