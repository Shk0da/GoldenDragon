package com.github.shk0da.GoldenDragon.model;

import com.google.gson.JsonObject;

import java.util.LinkedHashMap;
import java.util.function.BiConsumer;
import java.util.function.Supplier;

public class PositionInfo {

    private final String figi;
    private final String ticker;
    private final String isin;
    private final TickerType instrumentType;
    private final Integer balance;
    private final Double expectedYield;
    private final Integer lots;
    private final Double averagePositionPrice;
    private final String name;

    public PositionInfo(String figi, String ticker, String isin,
                        String instrumentType,
                        Integer balance, Double expectedYield, Integer lots,
                        Double averagePositionPrice,
                        String name) {
        this.figi = figi;
        this.ticker = ticker;
        this.isin = isin;
        this.instrumentType = TickerType.byName(instrumentType);
        this.balance = balance;
        this.expectedYield = expectedYield;
        this.lots = lots;
        this.averagePositionPrice = averagePositionPrice;
        this.name = name;
    }

    public static PositionInfo of(JsonObject jsonObject) {
        var values = new LinkedHashMap<>() {{
            BiConsumer<String, Supplier<?>> putField = (name, value) -> {
                if (jsonObject.has(name)) {
                    put(name, value.get());
                }
            };
            putField.accept("figi", () -> jsonObject.get("figi").getAsString());
            putField.accept("ticker", () -> jsonObject.get("ticker").getAsString());
            putField.accept("isin", () -> jsonObject.get("isin").getAsString());
            putField.accept("instrumentType", () -> jsonObject.get("instrumentType").getAsString());
            putField.accept("balance", () -> jsonObject.get("balance").getAsInt());
            putField.accept("expectedYield", () -> jsonObject.get("expectedYield").getAsDouble());
            putField.accept("lots", () -> jsonObject.get("lots").getAsInt());
            putField.accept("averagePositionPrice", () -> jsonObject.get("averagePositionPrice").getAsDouble());
            putField.accept("name", () -> jsonObject.get("name").getAsString());
        }};
        return new PositionInfo(
                (String) values.get("figi"),
                (String) values.get("ticker"),
                (String) values.get("isin"),
                (String) values.get("instrumentType"),
                (Integer) values.get("balance"),
                (Double) values.get("expectedYield"),
                (Integer) values.get("lots"),
                (Double) values.get("averagePositionPrice"),
                (String) values.get("name")
        );
    }

    public String getFigi() {
        return figi;
    }

    public String getTicker() {
        return ticker != null ? ticker : "none";
    }

    public String getIsin() {
        return isin != null ? isin : "";
    }

    public TickerType getInstrumentType() {
        return instrumentType;
    }

    public Integer getBalance() {
        return balance != null ? balance : 0;
    }

    public Double getExpectedYield() {
        return expectedYield != null ? expectedYield : 0;
    }

    public Integer getLots() {
        return lots != null ? lots : 0;
    }

    public Double getAveragePositionPrice() {
        return averagePositionPrice;
    }

    public String getName() {
        return name;
    }
}
