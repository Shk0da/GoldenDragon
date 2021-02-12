package com.github.shk0da.GoldenDragon.model;

import java.util.Objects;

public class TickerCandle {

    private final String symbol;
    private final String date;
    private final Double open;
    private final Double high;
    private final Double low;
    private final Double close;
    private final Double adjClose;
    private final Integer volume;

    public TickerCandle(String symbol, String date, Double open, Double high, Double low, Double close, Double adjClose, Integer volume) {
        this.symbol = symbol;
        this.date = date;
        this.open = open;
        this.high = high;
        this.low = low;
        this.close = close;
        this.adjClose = adjClose;
        this.volume = volume;
    }

    public String getSymbol() {
        return symbol;
    }

    public String getDate() {
        return date;
    }

    public Double getOpen() {
        return open;
    }

    public Double getHigh() {
        return high;
    }

    public Double getLow() {
        return low;
    }

    public Double getClose() {
        return close;
    }

    public Double getAdjClose() {
        return adjClose;
    }

    public Integer getVolume() {
        return volume;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        TickerCandle that = (TickerCandle) o;
        return Objects.equals(symbol, that.symbol) &&
                Objects.equals(date, that.date);
    }

    @Override
    public int hashCode() {
        return Objects.hash(symbol, date);
    }

    @Override
    public String toString() {
        return "TickerCandle{" +
                "symbol='" + symbol + '\'' +
                ", date='" + date + '\'' +
                ", open=" + open +
                ", high=" + high +
                ", low=" + low +
                ", close=" + close +
                ", adjClose=" + adjClose +
                ", volume=" + volume +
                '}';
    }
}
