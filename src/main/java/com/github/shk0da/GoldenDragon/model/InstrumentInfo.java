package com.github.shk0da.GoldenDragon.model;

import com.google.gson.JsonObject;

import java.time.OffsetDateTime;
import java.util.LinkedHashMap;
import java.util.function.BiConsumer;
import java.util.function.Supplier;

public class InstrumentInfo {

    private final String type;
    private final String ticker;
    private final String classCode;
    private final String showName;
    private final String currency;
    private final OffsetDateTime maxTradeDateTime;

    public InstrumentInfo(String type, String ticker, String classCode, String showName, String currency, OffsetDateTime maxTradeDateTime) {
        this.type = type;
        this.ticker = ticker;
        this.classCode = classCode;
        this.showName = showName;
        this.currency = currency;
        this.maxTradeDateTime = maxTradeDateTime;
    }

    public String getType() {
        return type;
    }

    public TickerType getTickerType() {
        if (type.equals("share")) {
            return TickerType.STOCK;
        }
        return TickerType.byName(type);
    }

    public String getTicker() {
        return ticker;
    }

    public String getClassCode() {
        return classCode;
    }

    public String getShowName() {
        return showName;
    }

    public String getCurrency() {
        return currency;
    }

    public OffsetDateTime getMaxTradeDateTime() {
        return maxTradeDateTime;
    }

    public static InstrumentInfo of(JsonObject jsonObject) {
        var values = new LinkedHashMap<>() {{
            BiConsumer<String, Supplier<?>> putField = (name, value) -> {
                if (jsonObject.has(name)) {
                    put(name, value.get());
                }
            };
            putField.accept("type", () -> jsonObject.get("type").getAsString());
            putField.accept("ticker", () -> jsonObject.get("ticker").getAsString());
            putField.accept("classCode", () -> jsonObject.get("classCode").getAsString());
            putField.accept("showName", () -> jsonObject.get("showName").getAsString());
            putField.accept("currency", () -> jsonObject.get("currency").getAsString());
            putField.accept("statistics", () -> jsonObject.get("statistics").getAsJsonObject());
        }};
        return new InstrumentInfo(
                (String) values.get("type"),
                (String) values.get("ticker"),
                (String) values.get("classCode"),
                (String) values.get("showName"),
                (String) values.get("currency"),
                OffsetDateTime.parse(((JsonObject) values.get("statistics")).get("maxTradeDateTime").getAsString())
        );
    }

    @Override
    public String toString() {
        return "InstrumentInfo{" +
                "type='" + type + '\'' +
                ", ticker='" + ticker + '\'' +
                ", classCode='" + classCode + '\'' +
                ", showName='" + showName + '\'' +
                ", currency='" + currency + '\'' +
                ", maxTradeDateTime=" + maxTradeDateTime +
                '}';
    }
}
