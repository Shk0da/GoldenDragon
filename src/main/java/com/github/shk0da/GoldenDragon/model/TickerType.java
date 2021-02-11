package com.github.shk0da.GoldenDragon.model;

public enum TickerType {

    BOND, STOCK, ETF, CURRENCY, UNKNOWN;

    public static TickerType ALL = null;

    public static TickerType byName(String name) {
        if (null == name) {
            return UNKNOWN;
        }
        for (TickerType value : values()) {
            if (value.name().equalsIgnoreCase(name)) {
                return value;
            }
        }
        return UNKNOWN;
    }

    @Override
    public String toString() {
        return name();
    }
}
