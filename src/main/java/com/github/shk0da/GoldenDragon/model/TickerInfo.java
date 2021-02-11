package com.github.shk0da.GoldenDragon.model;

import com.google.gson.JsonObject;

import java.util.LinkedHashMap;
import java.util.Objects;
import java.util.function.BiConsumer;
import java.util.function.Supplier;

public class TickerInfo {

    public static class Key {

        private final String ticker;
        private final TickerType type;

        public Key(String tickerWithType) {
            String[] key = tickerWithType.split("/");
            this.ticker = key[1];
            this.type = TickerType.byName(key[0]);
        }

        public Key(String ticker, TickerType type) {
            this.ticker = ticker;
            this.type = type;
        }

        public String getTicker() {
            return ticker;
        }

        public TickerType getType() {
            return type;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Key key = (Key) o;
            return Objects.equals(ticker, key.ticker) && type == key.type;
        }

        @Override
        public int hashCode() {
            return Objects.hash(ticker, type);
        }

        @Override
        public String toString() {
            return type + "/" + ticker;
        }
    }

    private final String figi;
    private final String ticker;
    private final String isin;
    private final Double minPriceIncrement;
    private final Integer lot;
    private final String currency;
    private final String name;
    private final TickerType type;

    public TickerInfo(String figi, String ticker, String isin,
                      Double minPriceIncrement, Integer lot,
                      String currency, String name, String type) {
        this.figi = figi;
        this.ticker = ticker;
        this.isin = isin;
        this.minPriceIncrement = minPriceIncrement;
        this.lot = lot;
        this.currency = currency;
        this.name = name;
        this.type = TickerType.byName(type);
    }

    public static TickerInfo of(JsonObject jsonObject) {
        var values = new LinkedHashMap<>() {{
            BiConsumer<String, Supplier<?>> putField = (name, value) -> {
                if (jsonObject.has(name)) {
                    put(name, value.get());
                }
            };
            putField.accept("ticker", () -> jsonObject.get("ticker").getAsString());
            putField.accept("type", () -> jsonObject.get("type").getAsString());
            putField.accept("name", () -> jsonObject.get("name").getAsString());
            putField.accept("figi", () -> jsonObject.get("figi").getAsString());
            putField.accept("isin", () -> jsonObject.get("isin").getAsString());
            putField.accept("minPriceIncrement", () -> jsonObject.get("minPriceIncrement").getAsDouble());
            putField.accept("lot", () -> jsonObject.get("lot").getAsInt());
            putField.accept("currency", () -> jsonObject.get("currency").getAsString());
        }};
        return new TickerInfo(
                (String) values.get("figi"),
                (String) values.get("ticker"),
                (String) values.get("isin"),
                (Double) values.get("minPriceIncrement"),
                (Integer) values.get("lot"),
                (String) values.get("currency"),
                (String) values.get("name"),
                (String) values.get("type")
        );
    }

    public String getFigi() {
        return figi;
    }

    public String getTicker() {
        return ticker;
    }

    public String getIsin() {
        return isin;
    }

    public Double getMinPriceIncrement() {
        return minPriceIncrement;
    }

    public Integer getLot() {
        return lot;
    }

    public String getCurrency() {
        return currency;
    }

    public String getName() {
        return name;
    }

    public TickerType getType() {
        return type;
    }

    public TickerInfo.Key getKey() {
        return new TickerInfo.Key(ticker, type);
    }

    @Override
    public String toString() {
        return "TickerInfo{" +
                "figi='" + figi + '\'' +
                ", ticker='" + ticker + '\'' +
                ", isin='" + isin + '\'' +
                ", minPriceIncrement=" + minPriceIncrement +
                ", lot=" + lot +
                ", currency='" + currency + '\'' +
                ", name='" + name + '\'' +
                ", type=" + type +
                '}';
    }
}
