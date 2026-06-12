package com.github.shk0da.goldendragon.model;

import java.util.Date;
import java.util.Objects;

public class TickerScan {

    private final String name;
    private final String description;
    private final Long totalDebt;
    private final Double debtToEquity;
    private final String type;
    private final String subtype;
    private final Double recommend1M;
    private final Date dateTime;

    public TickerScan(
            String name,
            String description,
            Long totalDebt,
            Double debtToEquity,
            String type,
            String subtype,
            Double recommend1M,
            Date dateTime) {
        this.name = name;
        this.description = description;
        this.totalDebt = totalDebt;
        this.debtToEquity = debtToEquity;
        this.type = type;
        this.subtype = subtype;
        this.recommend1M = recommend1M;
        this.dateTime = dateTime;
    }

    public String getName() {
        return name;
    }

    public String getDescription() {
        return description;
    }

    public Long getTotalDebt() {
        return totalDebt;
    }

    public Double getDebtToEquity() {
        return debtToEquity;
    }

    public String getType() {
        return type;
    }

    public String getSubtype() {
        return subtype;
    }

    public Double getRecommend1M() {
        return recommend1M;
    }

    public Date getDateTime() {
        return dateTime;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        TickerScan that = (TickerScan) o;
        return Objects.equals(name, that.name) && Objects.equals(type, that.type);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name, type);
    }

    @Override
    public String toString() {
        return "TickerScan{"
                + "name='"
                + name
                + '\''
                + ", description='"
                + description
                + '\''
                + ", totalDebt="
                + totalDebt
                + ", debtToEquity="
                + debtToEquity
                + ", type='"
                + type
                + '\''
                + ", subtype='"
                + subtype
                + '\''
                + ", recommend1M="
                + recommend1M
                + ", dateTime="
                + dateTime
                + '}';
    }
}
