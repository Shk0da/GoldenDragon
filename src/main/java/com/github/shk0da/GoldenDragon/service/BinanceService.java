package com.github.shk0da.GoldenDragon.service;

/*import com.google.common.collect.Lists;
import com.google.gson.*;

import io.grpc.netty.shaded.io.netty.handler.codec.http.HttpMethod;

import org.apache.commons.codec.binary.Hex;

import javax.crypto.Mac;
import javax.crypto.spec.SecretKeySpec;

import java.net.HttpRetryException;
import java.net.http.HttpClient;
import java.net.http.HttpHeaders;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Duration;
import java.time.temporal.ChronoUnit;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import static com.github.shk0da.GoldenDragon.config.MainConfig.*;
import static com.github.shk0da.GoldenDragon.utils.RequestUtils.requestWithRetry;
import static java.lang.System.out;

import java.io.IOException;*/

public class BinanceService {

    /*private static String API_KEY = "J2gUmObpBLvGhFbvpgwKUVhvFsLg60ZkpBKXRjnxZXF0S9A0kZQWktqcHSMstenJ";
    private static String SECRET_KEY = "xu7uYYEGPr1zALA5QRTuR1OCCUWioxCTlf0l0zERmOT5txvZKLbiWX26JemYWKj5";

    public static final String BASE_URL_V1 = "https://www.binance.com/api/v1/";
    public static final String BASE_URL_V3 = "https://www.binance.com/api/v3/";

    private final JsonParser jsonParser = new JsonParser();
    private final RestTemplate restTemplate = new RestTemplate();

    private static final class RestTemplate {
    
        public void lol() {
            HttpResponse<String> response = requestWithRetry(() -> {
                String json = new Gson().toJson(scanRequest);
                HttpRequest request = HttpRequest.newBuilder()
                        .uri(marketUrl)
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
        }

        public <T> T getForEntity(String url, T type) {

        }
    }

    public static void main(String[] args) {

    }

    public static class Candle {
        private String symbol;
        private Double price;
        private Double bid;
        private Double ask;
        private Date dateTime;
    }

    public static class Wallet {
        private String name;
        private Double free;
        private Double locked;
    }

    public static class ActiveOrder {
        private Long id;
        private String symbol;
        private String side;
        private String type;
        private Double amount;
        private Double price;
        private Long timestamp;
        private String status;
    }

    public static class HistoryOrder {
        private Long id;
        private String symbol;
        private String side;
        private String type;
        private Double amount;
        private Double price;
        private Long timestamp;
    }

    public static class Order {

        public enum Side {SELL, BUY}

        public enum Type {LIMIT, MARKET, STOP_LOSS, STOP_LOSS_LIMIT, TAKE_PROFIT, TAKE_PROFIT_LIMIT, LIMIT_MAKER}

        private String symbol;
        private Side side;
        private Type type;
        private Double quantity;
        private Double price;

        public JsonObject toJsonObject() {
            JsonObject jsonObject = new JsonObject();
            jsonObject.addProperty("symbol", symbol.toLowerCase());
            jsonObject.addProperty("amount", String.format("%f", quantity));
            jsonObject.addProperty("price", String.format("%f", price != null ? price : 0.01));
            jsonObject.addProperty("side", side.name().toLowerCase());
            jsonObject.addProperty("type", type.name().toLowerCase());

            return jsonObject;
        }

        public String toQuery() {
            String query = "symbol=%s&side=%s&type=%s&quantity=%.7f&";
            if (price != null && price > 0) {
                query += "price=" + Double.toString(price) + "&";
            }
            return String.format(query, symbol, side.name(), type.name(), quantity);
        }

        @Override
        public String toString() {
            return toJsonObject().toString();
        }
    }

    public List<Candle> getCandles(String symbol) {
        Candle candle = null;
        try {
            String jsonPrice = restTemplate
                    .getForEntity(BASE_URL_V3 + "ticker/price?symbol=" + symbol, String.class)
                    .getBody();
            JsonObject responsePrice = (JsonObject) jsonParser.parse(jsonPrice);

            String jsonBookTicker = restTemplate
                    .getForEntity(BASE_URL_V3 + "ticker/bookTicker?symbol=" + symbol, String.class)
                    .getBody();
            JsonObject responseBookTicker = (JsonObject) jsonParser.parse(jsonBookTicker);

            candle = Candle.builder()
                    .symbol(symbol)
                    .price(responsePrice.get("price").getAsDouble())
                    .ask(responseBookTicker.get("askPrice").getAsDouble())
                    .bid(responseBookTicker.get("bidPrice").getAsDouble())
                    .dateTime(new Date())
                    .build();

            out.println(candle.toString());
        } catch (Exception ex) {
            out.println(ex.getMessage());
        }

        return Lists.newArrayList(candle);
    }

    public Object createOrder(Order order) {
        if (!tradeIsAllowed) return new HashMap<String, String>() {{
            put("tradeIsAllowed", "false");
        }};

        try {
            return sendPostRequest("order", order.toQuery()).getBody();
        } catch (HttpClientErrorException ex) {
            out.println(ex.getResponseBodyAsString());
        }

        return null;
    }

    public Object cancelOrder(String symbol, Long orderId) {
        try {
            String query = "symbol=" + symbol + "&";
            if (orderId != null && orderId > 0) {
                query += "orderId=" + orderId + "&";
            }
            return restTemplate
                    .exchange(
                            BASE_URL_V3 + "order?" + query + getSignatureParam(query),
                            HttpMethod.DELETE,
                            new HttpEntity<>(getAuthHeader()),
                            Object.class
                    ).getBody();
        } catch (HttpClientErrorException ex) {
            out.println(ex.getResponseBodyAsString());
        }

        return null;
    }

    private long getServerTime() {
        try {
            String jsonPrice = restTemplate
                    .exchange(BASE_URL_V1 + "time", HttpMethod.GET, null, String.class)
                    .getBody();

            long serverTime = jsonParser.parse(jsonPrice)
                    .getAsJsonObject()
                    .get("serverTime")
                    .getAsLong();

            out.println("ServerTime: {}", serverTime);
            return serverTime;
        } catch (Exception ex) {
            out.println(ex.getMessage());
        }

        return System.currentTimeMillis();
    }

    public List<Wallet> getInfo() {
        try {
            Map<String, Object> accountData = (Map<String, Object>) sendGetRequest("account").getBody();
            JsonArray balances = new GsonBuilder()
                    .create()
                    .toJsonTree(accountData.get("balances"))
                    .getAsJsonArray();

            List<Wallet> wallets = Lists.newArrayList();
            balances.forEach(jsonElement ->
                    wallets.add(Wallet.builder()
                            .name(jsonElement.getAsJsonObject().get("asset").getAsString())
                            .free(jsonElement.getAsJsonObject().get("free").getAsDouble())
                            .locked(jsonElement.getAsJsonObject().get("locked").getAsDouble())
                            .build())
            );

            return wallets.stream()
                    .filter(wallet -> wallet.getFree() > 0 || wallet.getLocked() > 0)
                    .collect(Collectors.toList());
        } catch (HttpClientErrorException ex) {
            out.println(ex.getResponseBodyAsString());
        } catch (Exception ex) {
            out.println(ex.getMessage());
        }

        return Lists.newArrayList();
    }

    public List<ActiveOrder> getActiveOrders(int limit) {
        try {
            List<Object> response = (List<Object>) restTemplate
                    .exchange(
                            BASE_URL_V3 + "openOrders?" + getSignatureParam(),
                            HttpMethod.GET,
                            new HttpEntity<>(getAuthHeader()),
                            Object.class
                    ).getBody();

            JsonArray orders = new GsonBuilder()
                    .create()
                    .toJsonTree(response)
                    .getAsJsonArray();

            List<ActiveOrder> result = Lists.newArrayList();
            orders.forEach(jsonElement -> result.add(ActiveOrder.builder()
                    .id(jsonElement.getAsJsonObject().get("orderId").getAsLong())
                    .symbol(jsonElement.getAsJsonObject().get("symbol").getAsString())
                    .side(jsonElement.getAsJsonObject().get("side").getAsString())
                    .type(jsonElement.getAsJsonObject().get("type").getAsString())
                    .amount(jsonElement.getAsJsonObject().get("origQty").getAsDouble())
                    .price(jsonElement.getAsJsonObject().get("price").getAsDouble())
                    .timestamp(jsonElement.getAsJsonObject().get("time").getAsLong())
                    .status(jsonElement.getAsJsonObject().get("status").getAsString())
                    .build()));

            return result.subList(0, result.size() > limit ? limit : result.size())
                    .stream()
                    .sorted((f1, f2) -> Long.compare(f2.getTimestamp(), f1.getTimestamp()))
                    .collect(Collectors.toList());
        } catch (HttpClientErrorException ex) {
            out.println(ex.getResponseBodyAsString());
        } catch (Exception ex) {
            out.println(ex.getMessage());
        }

        return null;
    }

    public List<HistoryOrder> getHistoryOrders(int limit) {
        try {
            List<HistoryOrder> result = Lists.newArrayList();
            instrumentRepository.getSymbolsByMarket(StockMarket.Binance).forEach(symbol -> {
                String query = "symbol=" + symbol + "&limit=" + limit + "&";
                List<Object> response = (List<Object>) restTemplate
                        .exchange(
                                BASE_URL_V3 + "allOrders?" + query + getSignatureParam(query),
                                HttpMethod.GET,
                                new HttpEntity<>(getAuthHeader()),
                                Object.class
                        ).getBody();

                JsonArray orders = new GsonBuilder()
                        .create()
                        .toJsonTree(response)
                        .getAsJsonArray();

                orders.forEach(jsonElement ->
                        result.add(HistoryOrder.builder()
                                .id(jsonElement.getAsJsonObject().get("orderId").getAsLong())
                                .symbol(jsonElement.getAsJsonObject().get("symbol").getAsString())
                                .side(jsonElement.getAsJsonObject().get("side").getAsString())
                                .type(jsonElement.getAsJsonObject().get("type").getAsString())
                                .amount(jsonElement.getAsJsonObject().get("origQty").getAsDouble())
                                .price(jsonElement.getAsJsonObject().get("price").getAsDouble())
                                .timestamp(jsonElement.getAsJsonObject().get("time").getAsLong())
                                .build())
                );
            });

            return result.subList(0, result.size() > limit ? limit : result.size())
                    .stream()
                    .sorted((f1, f2) -> Long.compare(f2.getTimestamp(), f1.getTimestamp()))
                    .collect(Collectors.toList());
        } catch (Exception ex) {
            out.println(ex.getMessage());
        }

        return null;
    }

    private ResponseEntity<Object> sendGetRequest(String uri) {
        return sendGetRequest(uri, "");
    }

    private ResponseEntity<Object> sendGetRequest(String uri, String query) {
        return restTemplate
                .exchange(
                        BASE_URL_V3 + uri + "?" + query + getSignatureParam(query),
                        HttpMethod.GET,
                        new HttpEntity<>(getAuthHeader()),
                        Object.class
                );
    }

    private ResponseEntity<Object> sendPostRequest(String uri, String query) {
        return restTemplate
                .exchange(
                        BASE_URL_V3 + uri + "?" + query + getSignatureParam(query),
                        HttpMethod.POST,
                        new HttpEntity<>(getAuthHeader()),
                        Object.class
                );
    }

    private HttpHeaders getAuthHeader() {
        HttpHeaders headers = new HttpHeaders();
        headers.set("Content-Type", "application/json");
        headers.set("Accept", "application/json");
        headers.set("X-MBX-APIKEY", apiKey);

        return headers;
    }

    private String getSignatureParam() {
        return getSignatureParam("");
    }

    private String getSignatureParam(String payload) {
        String query = "recvWindow=15000&timestamp=" + getServerTime();
        return query + "&signature=" + getSignature(payload + query, secretKey);
    }

    private String getSignature(String payload, String secretKey) {
        try {
            Mac mac = Mac.getInstance("HmacSHA256");
            SecretKeySpec secretKeySpec = new SecretKeySpec(secretKey.getBytes(), "HmacSHA256");
            mac.init(secretKeySpec);
            return new String(Hex.encodeHex(mac.doFinal(payload.getBytes())));
        } catch (Exception e) {
            throw new RuntimeException("Unable to sign message.", e);
        }
    }*/
}
