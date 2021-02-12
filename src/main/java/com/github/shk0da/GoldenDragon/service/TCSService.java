package com.github.shk0da.GoldenDragon.service;

import com.github.shk0da.GoldenDragon.config.MainConfig;
import com.github.shk0da.GoldenDragon.config.MarketConfig;
import com.github.shk0da.GoldenDragon.model.PositionInfo;
import com.github.shk0da.GoldenDragon.model.TickerInfo;
import com.github.shk0da.GoldenDragon.model.TickerType;
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

import static com.github.shk0da.GoldenDragon.config.MainConfig.HEADER_AUTHORIZATION;
import static com.github.shk0da.GoldenDragon.config.MainConfig.HEADER_USER_AGENT;
import static com.github.shk0da.GoldenDragon.config.MainConfig.USER_AGENT;
import static com.github.shk0da.GoldenDragon.config.MainConfig.httpClient;
import static com.github.shk0da.GoldenDragon.dictionary.CurrenciesDictionary.getTickerName;
import static com.github.shk0da.GoldenDragon.utils.PrintUtils.printGlassOfPrices;
import static com.github.shk0da.GoldenDragon.utils.RequestUtils.requestWithRetry;
import static java.lang.Math.round;
import static java.lang.System.out;

public class TCSService {

    private final MainConfig mainConfig;
    private final MarketConfig marketConfig;

    private final Repository<TickerInfo.Key, String> figiRepository = FigiRepository.INSTANCE;
    private final Repository<TickerInfo.Key, TickerInfo> tickerRepository = TickerRepository.INSTANCE;
    private final Repository<TickerInfo.Key, Map<String, Map<Double, Integer>>> pricesRepository = PricesRepository.INSTANCE;

    public TCSService(MainConfig mainConfig, MarketConfig marketConfig) {
        this.mainConfig = mainConfig;
        this.marketConfig = marketConfig;
    }

    public int createOrder(TickerInfo.Key key, double price, int count, String operation) {
        String figi = figiByName(key);
        Map<String, Object> data = new HashMap<>();
        if (price > 0.0) {
            data.put("price", price);
        }
        int lot = searchTicker(key).getLot();
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

    public Map<TickerInfo.Key, TickerInfo> getStockList() {
        out.println("Loading current stocks from TCS...");
        return getMarketList("stocks");
    }

    public Map<TickerInfo.Key, TickerInfo> getBondList() {
        out.println("Loading current bonds from TCS...");
        return getMarketList("bonds");
    }

    public Map<TickerInfo.Key, TickerInfo> getEtfList() {
        out.println("Loading current etfs from TCS...");
        return getMarketList("etfs");
    }

    public Map<TickerInfo.Key, TickerInfo> getCurrenciesList() {
        out.println("Loading current currencies from TCS...");
        return getMarketList("currencies");
    }

    public Map<TickerInfo.Key, TickerInfo> getMarketList(String type) {
        try {
            TimeUnit.MILLISECONDS.sleep(550);
        } catch (InterruptedException ex) {
            out.println("Error: " + ex.getMessage());
        }

        HttpResponse<String> response = requestWithRetry(() -> {
            HttpRequest request = HttpRequest.newBuilder()
                    .GET()
                    .uri(URI.create(mainConfig.getTcsApi() + "market/" + type))
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

        Map<TickerInfo.Key, TickerInfo> stockList = new HashMap<>();
        JsonObject payload = JsonParser.parseString(response.body()).getAsJsonObject();
        JsonArray instruments = payload.get("payload").getAsJsonObject().get("instruments").getAsJsonArray();
        for (JsonElement instrument : instruments) {
            try {
                JsonObject item = instrument.getAsJsonObject();
                TickerInfo tickerInfo = TickerInfo.of(item);
                stockList.put(tickerInfo.getKey(), tickerInfo);
            } catch (Exception ex) {
                out.println("Error: Failed parse payload: " + ex.getMessage());
            }
        }
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

    public TickerInfo searchTicker(TickerInfo.Key key) {
        if (tickerRepository.containsKey(key)) {
            return tickerRepository.getById(key);
        }
        out.println("Search ticker '" + key.getTicker() + "' from TCS...");
        try {
            TimeUnit.MILLISECONDS.sleep(550);
        } catch (InterruptedException ex) {
            out.println("Error: " + ex.getMessage());
        }

        HttpResponse<String> response = requestWithRetry(() -> {
            HttpRequest request = HttpRequest.newBuilder()
                    .GET()
                    .uri(URI.create(mainConfig.getTcsApi() + "market/search/by-ticker?ticker=" + key.getTicker()))
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

        JsonObject payload = JsonParser.parseString(response.body()).getAsJsonObject();
        JsonArray instruments = payload.get("payload").getAsJsonObject().get("instruments").getAsJsonArray();
        for (JsonElement instrument : instruments) {
            try {
                JsonObject item = instrument.getAsJsonObject();
                TickerInfo tickerInfo = TickerInfo.of(item);
                tickerRepository.insert(key, tickerInfo);

                if (key.getType() == tickerInfo.getType()) {
                    out.println(key.getTicker() + ": " + tickerInfo);
                    return tickerInfo;
                }
            } catch (Exception ex) {
                out.println("Error: Failed parse payload: " + ex.getMessage());
            }
        }
        throw new RuntimeException("Ticker '" + key.getTicker() + "' not found in TCS");
    }

    public Map<TickerInfo.Key, PositionInfo> getCurrentPositions(TickerType tickerType) {
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

        Map<TickerInfo.Key, PositionInfo> positionInfoList = new HashMap<>();
        JsonObject payload = JsonParser.parseString(response.body()).getAsJsonObject();
        JsonArray positions = payload.get("payload").getAsJsonObject().get("positions").getAsJsonArray();
        for (JsonElement position : positions) {
            try {
                JsonObject item = position.getAsJsonObject();
                PositionInfo positionInfo = PositionInfo.of(item);

                if (TickerType.ALL != tickerType && !tickerType.equals(positionInfo.getInstrumentType())) {
                    continue;
                }

                String ticker = positionInfo.getTicker();
                var key = new TickerInfo.Key(ticker, positionInfo.getInstrumentType());
                String currency = searchTicker(key).getCurrency();
                if (!marketConfig.getCurrency().equals(currency)) continue;

                positionInfoList.put(key, positionInfo);
            } catch (Exception ex) {
                out.println("Error: Failed parse payload: " + ex.getMessage());
            }
        }
        return positionInfoList;
    }

    public String figiByName(TickerInfo.Key key) {
        if (figiRepository.containsKey(key)) {
            return figiRepository.getById(key);
        }
        var ticker = searchTicker(key);
        var figi = ticker.getFigi();
        figiRepository.insert(key, figi);
        return figi;
    }

    public double getPriceInCurrentCurrency(TickerInfo.Key key, int qty, String basicCurrency) {
        double price = getAvailablePrice(key, qty, false);
        String currency = searchTicker(key).getCurrency();
        if (!basicCurrency.equals(currency)) {
            price = convertCurrencies(currency, basicCurrency, price);
        }
        return price;
    }

    public double getAvailablePrice(TickerInfo.Key key) {
        return getAvailablePrice(key, 1, false);
    }

    public double getAvailablePrice(TickerInfo.Key key, int count, boolean isPrintGlass) {
        int value = count;
        double tickerPrice = 0.0;
        for (Map.Entry<Double, Integer> bid : getCurrentPrices(key, isPrintGlass).get("bids").entrySet()) {
            tickerPrice = bid.getKey();
            value = value - bid.getValue();
            if (value <= 0) break;
        }
        return tickerPrice;
    }

    public Map<String, Map<Double, Integer>> getCurrentPrices(TickerInfo.Key key) {
        return getCurrentPrices(key, true);
    }

    public Map<String, Map<Double, Integer>> getCurrentPrices(TickerInfo.Key key, boolean isPrintGlass) {
        if (!isPrintGlass && pricesRepository.containsKey(key)) {
            return pricesRepository.getById(key);
        }
        String figi = figiByName(key);
        if (isPrintGlass) {
            out.println("Loading current price '" + key + "' from TCS...");
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
            printGlassOfPrices(key.getTicker(), currentPrices);
        }
        pricesRepository.insert(key, currentPrices);
        return currentPrices;
    }

    public double convertCurrencies(String currency, String basicCurrency, double price) {
        if (basicCurrency.equals(currency)) {
            return price;
        }

        String currencyTicker = getTickerName(currency);
        if (currencyTicker.equals(currency)) {
            return round(((price / getAvailablePrice(new TickerInfo.Key(getTickerName(basicCurrency), TickerType.CURRENCY)))) * 1000) / 1000.0;
        }

        var key = new TickerInfo.Key(currencyTicker, TickerType.CURRENCY);
        TickerInfo currencyTickerInfo = searchTicker(key);
        if (basicCurrency.equals(currencyTickerInfo.getCurrency())) {
            return round(((price / getAvailablePrice(key))) * 100000) / 100000.0;
        }
        return price;
    }
}
