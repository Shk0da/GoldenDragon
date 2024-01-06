package com.github.shk0da.GoldenDragon.model;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;

import java.time.OffsetDateTime;
import java.util.LinkedHashMap;
import java.util.function.BiConsumer;
import java.util.function.Supplier;

public class OperationInfo {

    private InstrumentInfo instrumentInfo;

    private final String action;
    private final Double relativeYield;
    private final Double averagePrice;
    private final OffsetDateTime tradeDateTime;

    public OperationInfo(String action, Double relativeYield, Double averagePrice, OffsetDateTime tradeDateTime) {
        this.action = action;
        this.relativeYield = relativeYield;
        this.averagePrice = averagePrice;
        this.tradeDateTime = tradeDateTime;
    }

    public String getAction() {
        return action;
    }

    public Double getRelativeYield() {
        return relativeYield;
    }

    public Double getAveragePrice() {
        return averagePrice;
    }

    public OffsetDateTime getTradeDateTime() {
        return tradeDateTime;
    }

    public InstrumentInfo getInstrument() {
        return instrumentInfo;
    }

    public OperationInfo withInstrument(InstrumentInfo instrumentInfo) {
        this.instrumentInfo = instrumentInfo;;
        return this;
    }

    public static OperationInfo of(JsonObject jsonObject) {
        var values = new LinkedHashMap<>() {{
            BiConsumer<String, Supplier<?>> putField = (name, value) -> {
                if (jsonObject.has(name)) {
                    put(name, value.get());
                }
            };
            putField.accept("action", () -> jsonObject.get("action").getAsString());
            putField.accept("relativeYield", () -> jsonObject.get("relativeYield"));
            putField.accept("averagePrice", () -> jsonObject.get("averagePrice").getAsString());
            putField.accept("tradeDateTime", () -> jsonObject.get("tradeDateTime").getAsString());
        }};
        return new OperationInfo(
                (String) values.get("action"),
                Double.valueOf(null != values.get("relativeYield") ? ((JsonElement) values.get("relativeYield")).getAsString() : "0.0"),
                Double.valueOf((String) values.get("averagePrice")),
                OffsetDateTime.parse((String) values.get("tradeDateTime"))
        );
    }

    @Override
    public String toString() {
        return "OperationInfo{" +
                "ticker='" + (null != instrumentInfo ? instrumentInfo.getTicker() : "-") + '\'' +
                ", action='" + action + '\'' +
                ", relativeYield=" + relativeYield +
                ", averagePrice=" + averagePrice +
                ", tradeDateTime=" + tradeDateTime +
                '}';
    }
}
