package com.github.shk0da.GoldenDragon.strategy;

import com.github.shk0da.GoldenDragon.config.PulseConfig;
import com.github.shk0da.GoldenDragon.model.InstrumentInfo;
import com.github.shk0da.GoldenDragon.model.OperationInfo;
import com.github.shk0da.GoldenDragon.model.TickerType;
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
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.concurrent.ConcurrentHashMap;
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

    private final AtomicReference<Boolean> tradeStart = new AtomicReference<>();
    private final AtomicReference<String> sessionId = new AtomicReference<>();
    private final Map<String, OffsetDateTime> profileIds = new ConcurrentHashMap<>();

    private final ExecutorService sessionWatcher = Executors.newSingleThreadExecutor();
    private final ExecutorService httpServer = Executors.newSingleThreadExecutor();

    public PulseFollower(PulseConfig pulseConfig, TCSService tcsService) {
        this.pulseConfig = pulseConfig;
        this.tcsService = tcsService;

        Map<String, OffsetDateTime> lastWatchedTrade = new HashMap<>();
        for (String profileId : pulseConfig.getFollowProfileId()) {
            lastWatchedTrade.put(profileId, OffsetDateTime.now());
        }
        this.profileIds.putAll(lastWatchedTrade);
        this.sessionId.set(pulseConfig.getFollowSessionId());
        this.tradeStart.set(true);
    }

    public void run() {
        runSessionWatcher();
        runHttpServer(pulseConfig.getHttpPort());
        runFollow(pulseConfig.getMaxPositions());
    }

    private void runFollow(int maxPositions) {
        out.printf("Start follow for profileIds=%s\n", profileIds.keySet());
        while (true) {
            if (!tradeStart.get()) continue;

            Map<OffsetDateTime, OperationInfo> operationsWithoutDuplicates = new TreeMap<>();
            for (String profileId : profileIds.keySet()) {
                try {
                    Map<OffsetDateTime, OperationInfo> operationsByDateTime = new TreeMap<>();
                    Map<OffsetDateTime, InstrumentInfo> instrumentsByDateTime = getInstruments(profileId, sessionId.get());
                    instrumentsByDateTime.forEach((dateTme, item) -> {
                        if (profileIds.get(profileId).isBefore(dateTme)) {
                            operationsByDateTime.putAll(getOperations(profileId, sessionId.get(), item));
                        }
                    });

                    Set<Map.Entry<String, InstrumentInfo>> uniqueCheck = new HashSet<>();
                    operationsByDateTime.forEach((operationDateTme, operation) -> {
                        InstrumentInfo instrument = operation.getInstrument();
                        boolean hasSameTrade = uniqueCheck.contains(entry(operation.getAction(), instrument));
                        if (profileIds.get(profileId).isBefore(operationDateTme) && !hasSameTrade) {
                            out.printf("%s [%s] Operation [%s]: %s\n", OffsetDateTime.now(), profileId, instrument.getTicker(), operation);
                            if ("sell".equals(operation.getAction()) && uniqueCheck.contains(entry("buy", instrument))) {
                                OperationInfo opToRemove = null;
                                OffsetDateTime keyToRemove = null;
                                for (Map.Entry<OffsetDateTime, OperationInfo> entry : operationsWithoutDuplicates.entrySet()) {
                                    OperationInfo op = entry.getValue();
                                    if (op.getAction().equals("buy") && op.getInstrument().equals(instrument)) {
                                        opToRemove = op;
                                        keyToRemove = op.getTradeDateTime();
                                        break;
                                    }
                                }
                                if (null != keyToRemove) {
                                    operationsWithoutDuplicates.remove(keyToRemove, opToRemove);
                                }
                            }
                            uniqueCheck.add(entry(operation.getAction(), instrument));
                            operationsWithoutDuplicates.put(operationDateTme, operation);
                            profileIds.put(profileId, operationDateTme);
                        }
                    });
                    TimeUnit.MILLISECONDS.sleep(3_000);
                } catch (Exception ex) {
                    out.println("Error: " + ex.getMessage());
                }
            }

            operationsWithoutDuplicates.forEach((operationDateTme, operation) -> {
                InstrumentInfo instrument = operation.getInstrument();
                try {
                    handleOperation(operation, instrument, maxPositions);
                } catch (Exception ex) {
                    out.println("Failed handle " + instrument.getTicker() + ": " + ex.getMessage());
                }
            });
        }
    }

    private void handleOperation(OperationInfo operation, InstrumentInfo instrument, int maxPositions) {
        switch (operation.getAction()) {
            case "buy":
                if (tcsService.getCountOfCurrentPositions(instrument.getTickerType(), instrument.getTicker()) > 0) {
                    out.println("The portfolio already contains this ticker: " + instrument.getTicker());
                    return;
                }

                int countOfCurrentPositions = tcsService.getCountOfCurrentPositions();
                if (countOfCurrentPositions < maxPositions) {
                    double availableCash = tcsService.getAvailableCash();
                    double totalPortfolioCost = tcsService.getTotalPortfolioCost();
                    int availablePositions = maxPositions - countOfCurrentPositions;
                    double cost = Math.min(totalPortfolioCost / maxPositions, availableCash / availablePositions);
                    out.printf(
                            "availableCash=%f, totalPortfolioCost=%f, availablePositions=%d, cost=%f\n",
                            availableCash, totalPortfolioCost, availablePositions, cost
                    );
                    tcsService.buyByMarket(
                            instrument.getTicker(), instrument.getTickerType(),
                            cost, pulseConfig.getTakeProfit(), pulseConfig.getStopLose()
                    );
                }
                break;
            case "sell":
                tcsService.sellAllByMarket(instrument.getTicker(), instrument.getTickerType());
                break;
        }
    }

    private Map<OffsetDateTime, InstrumentInfo> getInstruments(String profileId, String sessionId) {
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
            String errorMessage = payload.get("message").getAsString();
            if ("Необходимо наличие публичного профиля".equals(errorMessage) && profileIds.size() > 1) {
                profileIds.remove(profileId);
                telegramNotifyService.sendMessage("Profile [" + profileId + "] was removed from the following");
            }
            throw new RuntimeException("Failed get instruments [" + profileId + "]: " + errorMessage);
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

    private Map<OffsetDateTime, OperationInfo> getOperations(String profileId, String sessionId, InstrumentInfo instrumentInfo) {
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
        try {
            HttpServer server = HttpServer.create(new InetSocketAddress(serverPort), 0);

            out.printf("Start API: //0.0.0.0:%d/api/update?sessionId=${sessionId}\n", serverPort);
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

            out.printf("Start API: //0.0.0.0:%d/api/add_profileId?profileId=${profileId}\n", serverPort);
            server.createContext("/api/add_profileId", (exchange -> {
                String profileId = exchange.getRequestURI().getQuery().split("=")[1];
                profileIds.put(profileId, OffsetDateTime.now());

                String respText = "Add profileId=" + profileId;
                exchange.sendResponseHeaders(200, respText.getBytes().length);
                OutputStream output = exchange.getResponseBody();
                output.write(respText.getBytes());
                output.flush();
                exchange.close();
            }));

            out.printf("Start API: //0.0.0.0:%d/api/remove_profileId?profileId=${profileId}\n", serverPort);
            server.createContext("/api/remove_profileId", (exchange -> {
                String profileId = exchange.getRequestURI().getQuery().split("=")[1];
                profileIds.remove(profileId);

                String respText = "Remove profileId=" + profileId;
                exchange.sendResponseHeaders(200, respText.getBytes().length);
                OutputStream output = exchange.getResponseBody();
                output.write(respText.getBytes());
                output.flush();
                exchange.close();
            }));

            out.printf("Start API: //0.0.0.0:%d/api/sell_all?tickerType=${tickerType}\n", serverPort);
            server.createContext("/api/sell_all", (exchange -> {
                TickerType type = TickerType.byName(exchange.getRequestURI().getQuery().split("=")[1]);
                tcsService.sellAllByMarket(type);

                String respText = "Everything is sold";
                exchange.sendResponseHeaders(200, respText.getBytes().length);
                OutputStream output = exchange.getResponseBody();
                output.write(respText.getBytes());
                output.flush();
                exchange.close();
            }));

            out.printf("Start API: //0.0.0.0:%d/api/balance\n", serverPort);
            server.createContext("/api/balance", (exchange -> {
                String respText = String.format("Balance: %f", tcsService.getTotalPortfolioCost());
                exchange.sendResponseHeaders(200, respText.getBytes().length);
                OutputStream output = exchange.getResponseBody();
                output.write(respText.getBytes());
                output.flush();
                exchange.close();
            }));

            out.printf("Start API: //0.0.0.0:%d/api/ping\n", serverPort);
            server.createContext("/api/ping", (exchange -> {
                String respText = "OK";
                exchange.sendResponseHeaders(200, respText.getBytes().length);
                OutputStream output = exchange.getResponseBody();
                output.write(respText.getBytes());
                output.flush();
                exchange.close();
            }));

            out.printf("Start API: //0.0.0.0:%d/api/trade_start={true|false}\n", serverPort);
            server.createContext("/api/trade_start", (exchange -> {
                String tradeStart = exchange.getRequestURI().getQuery().split("=")[1];
                this.tradeStart.set(Boolean.valueOf(tradeStart));

                String respText = "Trade=" + tradeStart;
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
        executeHttpGet(PING_API.replace("${sessionId}", sessionId));
    }

    private void executeSessionStatus(String sessionId) {
        String response = executeHttpGet(SESSION_STATUS_API.replace("${sessionId}", sessionId));
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
