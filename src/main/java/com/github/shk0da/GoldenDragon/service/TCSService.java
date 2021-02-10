package com.github.shk0da.GoldenDragon.service;

import com.github.shk0da.GoldenDragon.config.MainConfig;
import com.github.shk0da.GoldenDragon.config.MarketConfig;
import com.github.shk0da.GoldenDragon.repository.FigiRepository;
import com.github.shk0da.GoldenDragon.repository.PricesRepository;
import com.github.shk0da.GoldenDragon.repository.Repository;
import com.github.shk0da.GoldenDragon.repository.TickerRepository;
import com.github.shk0da.GoldenDragon.utils.RequestUtils;
import com.google.gson.Gson;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

import java.net.URI;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.TreeMap;
import java.util.concurrent.TimeUnit;
import java.util.function.BiConsumer;
import java.util.function.Supplier;

import static com.github.shk0da.GoldenDragon.config.MainConfig.HEADER_AUTHORIZATION;
import static com.github.shk0da.GoldenDragon.config.MainConfig.HEADER_USER_AGENT;
import static com.github.shk0da.GoldenDragon.config.MainConfig.USER_AGENT;
import static com.github.shk0da.GoldenDragon.config.MainConfig.httpClient;
import static com.github.shk0da.GoldenDragon.utils.PrintUtils.printGlassOfPrices;
import static com.github.shk0da.GoldenDragon.utils.RequestUtils.requestWithRetry;
import static java.lang.System.out;

public class TCSService {

    private final MainConfig mainConfig;
    private final MarketConfig marketConfig;

    private final Repository<String, String> figiRepository = FigiRepository.INSTANCE;
    private final Repository<String, Map<String, Object>> tickerRepository = TickerRepository.INSTANCE;
    private final Repository<String, Map<String, Map<Double, Integer>>> pricesRepository = PricesRepository.INSTANCE;

    public TCSService(MainConfig mainConfig) {
        this.mainConfig = mainConfig;
        this.marketConfig = mainConfig.getMarketConfig();
    }

    public int createOrder(String tickerName, double price, int count, String operation) {
        String figi = figiByName(tickerName);
        Map<String, Object> data = new HashMap<>();
        if (price > 0.0) {
            data.put("price", price);
        }
        int lot = (int) searchTicker(tickerName).get("lot");
        data.put("lots", (count / lot));
        data.put("operation", operation);
        String json = new Gson().toJson(data);

        String orderType = price >= 0.0 ? "limit-order" : "market-order";
        HttpResponse<String> response = RequestUtils.requestWithRetry(() -> {
            HttpRequest request = HttpRequest.newBuilder()
                    .POST(HttpRequest.BodyPublishers.ofString(json))
                    .uri(URI.create(mainConfig.getTcsApi() + "/orders/" + orderType + "?brokerAccountId" + mainConfig.getTcsAccountId() + "&figi=" + figi))
                    .setHeader(HEADER_USER_AGENT, USER_AGENT)
                    .setHeader(HEADER_AUTHORIZATION, mainConfig.getTcsAuthorization())
                    .header("Content-Type", "application/json")
                    .build();
            try {
                return httpClient.send(request, HttpResponse.BodyHandlers.ofString());
            } catch (Exception ex) {
                out.println("Error: " + ex.getMessage());
                return null;
            }
        });

        if (null == response) {
            return 0;
        }
        if (response.statusCode() >= 400) {
            out.println("Failed create order: " + response.body());
            return 0;
        }
        out.println("Created order: " + response.body());
        return 1;
    }

    public Map<String, Map<String, Object>> getStockList() {
        out.println("Loading current stocks from TCS...");
        try {
            TimeUnit.MILLISECONDS.sleep(550);
        } catch (InterruptedException ex) {
            out.println("Error: " + ex.getMessage());
        }

        HttpResponse<String> response = requestWithRetry(() -> {
            HttpRequest request = HttpRequest.newBuilder()
                    .GET()
                    .uri(URI.create(mainConfig.getTcsApi() + "market/stocks"))
                    .setHeader(HEADER_USER_AGENT, USER_AGENT)
                    .setHeader(HEADER_AUTHORIZATION, mainConfig.getTcsAuthorization())
                    .build();
            try {
                return httpClient.send(request, HttpResponse.BodyHandlers.ofString());
            } catch (Exception ex) {
                out.println("Error: " + ex.getMessage());
                return null;
            }
        });

        Map<String, Map<String, Object>> stockList = new HashMap<>();
        JsonObject payload = JsonParser.parseString(response.body()).getAsJsonObject();
        JsonArray instruments = payload.get("payload").getAsJsonObject().get("instruments").getAsJsonArray();
        for (JsonElement instrument : instruments) {
            try {
                JsonObject item = instrument.getAsJsonObject();
                String ticker = item.get("ticker").getAsString();
                stockList.put(ticker, new LinkedHashMap<>() {{
                    BiConsumer<String, Supplier<?>> putField = (name, value) -> {
                        if (item.has(name)) {
                            put(name, value.get());
                        }
                    };
                    put("ticker", ticker);
                    putField.accept("type", () -> item.get("type").getAsString());
                    putField.accept("name", () -> item.get("name").getAsString());
                    putField.accept("figi", () -> item.get("figi").getAsString());
                    putField.accept("isin", () -> item.get("isin").getAsString());
                    putField.accept("minPriceIncrement", () -> item.get("minPriceIncrement").getAsDouble());
                    putField.accept("lot", () -> item.get("lot").getAsInt());
                    putField.accept("currency", () -> item.get("currency").getAsString());
                }});
            } catch (Exception ex) {
                out.println("Error: Failed parse payload: " + ex.getMessage());
            }
        }
        out.println();
        return stockList;
    }

    public Double getAvailableCash() {
        out.println("Loading currencies from TCS...");
        try {
            TimeUnit.MILLISECONDS.sleep(550);
        } catch (InterruptedException ex) {
            out.println("Error: " + ex.getMessage());
        }

        HttpResponse<String> response = requestWithRetry(() -> {
            HttpRequest request = HttpRequest.newBuilder()
                    .GET()
                    .uri(URI.create(mainConfig.getTcsApi() + "portfolio/currencies?brokerAccountId=" + mainConfig.getTcsAccountId()))
                    .setHeader(HEADER_USER_AGENT, USER_AGENT)
                    .setHeader(HEADER_AUTHORIZATION, mainConfig.getTcsAuthorization())
                    .build();
            try {
                return httpClient.send(request, HttpResponse.BodyHandlers.ofString());
            } catch (Exception ex) {
                out.println("Error: " + ex.getMessage());
                return null;
            }
        });

        double availableCash = 0.0;
        JsonObject payload = JsonParser.parseString(response.body()).getAsJsonObject();
        JsonArray currencies = payload.get("payload").getAsJsonObject().get("currencies").getAsJsonArray();
        for (JsonElement currency : currencies) {
            try {
                String currencyName = currency.getAsJsonObject().get("currency").getAsString();
                double balance = currency.getAsJsonObject().get("balance").getAsDouble();
                if (marketConfig.getCurrency().equals(currencyName)) {
                    availableCash = balance;
                    out.println(mainConfig.getTcsAccountId() + ": " + balance + " " + currencyName);
                    break;
                }
            } catch (Exception ex) {
                out.println("Error: Failed parse payload: " + ex.getMessage());
            }
        }
        out.println();
        return availableCash;
    }

    public Map<String, Object> searchTicker(String name) {
        if (tickerRepository.containsKey(name)) {
            return tickerRepository.getById(name);
        }
        out.println("Search ticker '" + name + "' from TCS...");
        try {
            TimeUnit.MILLISECONDS.sleep(550);
        } catch (InterruptedException ex) {
            out.println("Error: " + ex.getMessage());
        }

        HttpResponse<String> response = requestWithRetry(() -> {
            HttpRequest request = HttpRequest.newBuilder()
                    .GET()
                    .uri(URI.create(mainConfig.getTcsApi() + "market/search/by-ticker?ticker=" + name))
                    .setHeader(HEADER_USER_AGENT, USER_AGENT)
                    .setHeader(HEADER_AUTHORIZATION, mainConfig.getTcsAuthorization())
                    .build();
            try {
                return httpClient.send(request, HttpResponse.BodyHandlers.ofString());
            } catch (Exception ex) {
                out.println("Error: " + ex.getMessage());
                return null;
            }
        });

        Map<String, Object> tickerInfo = new LinkedHashMap<>();
        JsonObject payload = JsonParser.parseString(response.body()).getAsJsonObject();
        JsonArray instruments = payload.get("payload").getAsJsonObject().get("instruments").getAsJsonArray();
        for (JsonElement instrument : instruments) {
            try {
                JsonObject item = instrument.getAsJsonObject();
                String type = item.get("type").getAsString();
                String ticker = item.get("ticker").getAsString();
                if ("Stock".equals(type) && name.equals(ticker)) {
                    tickerInfo.put("type", type);
                    tickerInfo.put("ticker", ticker);
                    tickerInfo.put("name", item.get("name").getAsString());
                    tickerInfo.put("figi", item.get("figi").getAsString());
                    tickerInfo.put("isin", item.get("isin").getAsString());
                    tickerInfo.put("minPriceIncrement", item.get("minPriceIncrement").getAsDouble());
                    tickerInfo.put("lot", item.get("lot").getAsInt());
                    tickerInfo.put("currency", item.get("currency").getAsString());
                    break;
                }
            } catch (Exception ex) {
                out.println("Error: Failed parse payload: " + ex.getMessage());
            }
        }
        if (!tickerInfo.containsKey("figi")) {
            throw new RuntimeException("Ticker '" + name + "' not found in TCS");
        }
        out.println(name + ": " + tickerInfo);
        tickerRepository.insert(name, tickerInfo);
        return tickerInfo;
    }

    public Map<String, Map<String, Object>> getCurrentPositions() {
        out.println("Loading current positions from TCS...");
        try {
            TimeUnit.MILLISECONDS.sleep(550);
        } catch (InterruptedException ex) {
            out.println("Error: " + ex.getMessage());
        }

        HttpResponse<String> response = requestWithRetry(() -> {
            HttpRequest request = HttpRequest.newBuilder()
                    .GET()
                    .uri(URI.create(mainConfig.getTcsApi() + "portfolio/?brokerAccountId=" + mainConfig.getTcsAccountId()))
                    .setHeader(HEADER_USER_AGENT, USER_AGENT)
                    .setHeader(HEADER_AUTHORIZATION, mainConfig.getTcsAuthorization())
                    .build();
            try {
                return httpClient.send(request, HttpResponse.BodyHandlers.ofString());
            } catch (Exception ex) {
                out.println("Error: " + ex.getMessage());
                return null;
            }
        });

        Map<String, Map<String, Object>> positionInfoList = new HashMap<>();
        JsonObject payload = JsonParser.parseString(response.body()).getAsJsonObject();
        JsonArray positions = payload.get("payload").getAsJsonObject().get("positions").getAsJsonArray();
        for (JsonElement position : positions) {
            try {
                JsonObject item = position.getAsJsonObject();

                String type = item.get("instrumentType").getAsString();
                if (!"Stock".equals(type)) continue;

                String ticker = item.get("ticker").getAsString();

                String currency = searchTicker(ticker).get("currency").toString();
                if (!marketConfig.getCurrency().equals(currency)) continue;

                positionInfoList.put(ticker, new LinkedHashMap<>() {{
                    BiConsumer<String, Supplier<?>> putField = (name, value) -> {
                        if (item.has(name)) {
                            put(name, value.get());
                        }
                    };
                    put("ticker", ticker);
                    putField.accept("figi", () -> item.get("figi").getAsString());
                    putField.accept("isin", () -> item.get("isin").getAsString());
                    putField.accept("name", () -> item.get("name").getAsString());
                    putField.accept("instrumentType", () -> type);
                    putField.accept("balance", () -> item.get("balance").getAsInt());
                    putField.accept("blocked", () -> item.get("blocked").getAsInt());
                    putField.accept("lots", () -> item.get("lots").getAsInt());
                    putField.accept("expectedYield", () -> item.get("expectedYield").getAsString());
                    putField.accept("averagePositionPrice", () -> item.get("averagePositionPrice").getAsString());
                    putField.accept("averagePositionPriceNoNkd", () -> item.get("averagePositionPriceNoNkd").getAsString());
                }});
            } catch (Exception ex) {
                out.println("Error: Failed parse payload: " + ex.getMessage());
            }
        }
        return positionInfoList;
    }

    public String figiByName(String tickerName) {
        if (figiRepository.containsKey(tickerName)) {
            return figiRepository.getById(tickerName);
        }
        var ticker = searchTicker(tickerName);
        var figi = ticker.get("figi").toString();
        figiRepository.insert(tickerName, figi);
        return figi;
    }

    public double getAvailablePrice(final String ticker, int count, boolean isPrintGlass) {
        int value = count;
        double tickerPrice = 0.0;
        for (Map.Entry<Double, Integer> bid : getCurrentPrices(ticker, isPrintGlass).get("bids").entrySet()) {
            tickerPrice = bid.getKey();
            value = value - bid.getValue();
            if (value <= 0) break;
        }
        return tickerPrice;
    }

    public Map<String, Map<Double, Integer>> getCurrentPrices(String tickerName) {
        return getCurrentPrices(tickerName, true);
    }

    public Map<String, Map<Double, Integer>> getCurrentPrices(String tickerName, boolean isPrintGlass) {
        if (!isPrintGlass && pricesRepository.containsKey(tickerName)) {
            return pricesRepository.getById(tickerName);
        }
        String figi = figiByName(tickerName);
        if (isPrintGlass) {
            out.println("Loading current price '" + tickerName + "' from TCS...");
        }
        try {
            TimeUnit.MILLISECONDS.sleep(550);
        } catch (InterruptedException ex) {
            out.println("Error: " + ex.getMessage());
        }

        HttpResponse<String> response = requestWithRetry(() -> {
            HttpRequest request = HttpRequest.newBuilder()
                    .GET()
                    .uri(URI.create(mainConfig.getTcsApi() + "market/orderbook?depth=10&figi=" + figi))
                    .setHeader(HEADER_USER_AGENT, USER_AGENT)
                    .setHeader(HEADER_AUTHORIZATION, mainConfig.getTcsAuthorization())
                    .build();
            try {
                return httpClient.send(request, HttpResponse.BodyHandlers.ofString());
            } catch (Exception ex) {
                out.println("Error: " + ex.getMessage());
                return null;
            }
        });

        Map<String, Map<Double, Integer>> currentPrices = new TreeMap<>();
        JsonObject payload = JsonParser.parseString(response.body()).getAsJsonObject().get("payload").getAsJsonObject();
        JsonArray bids = payload.get("bids").getAsJsonArray(); // for sell
        Map<Double, Integer> bidsValues = new LinkedHashMap<>(10);
        for (JsonElement bid : bids) {
            try {
                JsonObject item = bid.getAsJsonObject();
                bidsValues.put(item.get("price").getAsDouble(), item.get("quantity").getAsInt());
            } catch (Exception ex) {
                out.println("Error: Failed parse payload: " + ex.getMessage());
            }
        }
        currentPrices.put("bids", bidsValues);

        JsonArray asks = payload.get("asks").getAsJsonArray(); // for buy
        Map<Double, Integer> asksValues = new LinkedHashMap<>(10);
        for (JsonElement ask : asks) {
            try {
                JsonObject item = ask.getAsJsonObject();
                asksValues.put(item.get("price").getAsDouble(), item.get("quantity").getAsInt());
            } catch (Exception ex) {
                out.println("Error: Failed parse payload: " + ex.getMessage());
            }
        }
        currentPrices.put("asks", asksValues);

        if (isPrintGlass) {
            printGlassOfPrices(tickerName, currentPrices);
        }
        pricesRepository.insert(tickerName, currentPrices);
        return currentPrices;
    }
}
