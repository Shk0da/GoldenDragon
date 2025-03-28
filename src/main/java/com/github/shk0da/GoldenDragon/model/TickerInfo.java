package com.github.shk0da.GoldenDragon.model;

import com.fasterxml.jackson.annotation.JsonIgnore;

import java.util.Objects;

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

    private String figi;
    private String ticker;
    private String isin;
    private Double minPriceIncrement;
    private Integer lot;
    private String currency;
    private String name;
    private TickerType type;

    public TickerInfo() {}

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
        return currency.toUpperCase();
    }

    public String getName() {
        return name;
    }

    public TickerType getType() {
        return type;
    }

    @JsonIgnore
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
