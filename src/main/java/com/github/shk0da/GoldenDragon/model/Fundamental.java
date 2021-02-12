package com.github.shk0da.GoldenDragon.model;

import java.util.Date;

public class Fundamental {

    private final String symbol;
    private final String name;
    private final Date dateTime;
    private final String periodType;
    private final String currencyCode;
    private final Double valueRaw;
    private final String valueFmt;

    public Fundamental(String symbol, String name, Date dateTime, String periodType, String currencyCode, Double valueRaw, String valueFmt) {
        this.symbol = symbol;
        this.name = name;
        this.dateTime = dateTime;
        this.periodType = periodType;
        this.currencyCode = currencyCode;
        this.valueRaw = valueRaw;
        this.valueFmt = valueFmt;
    }

    public String getSymbol() {
        return symbol;
    }

    public String getName() {
        return name;
    }

    public Date getDateTime() {
        return dateTime;
    }

    public String getPeriodType() {
        return periodType;
    }

    public String getCurrencyCode() {
        return currencyCode;
    }

    public Double getValueRaw() {
        return valueRaw;
    }

    public String getValueFmt() {
        return valueFmt;
    }
}
