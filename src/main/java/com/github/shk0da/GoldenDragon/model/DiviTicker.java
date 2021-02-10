package com.github.shk0da.GoldenDragon.model;

import java.util.Date;
import java.util.Objects;

import static com.github.shk0da.GoldenDragon.config.MainConfig.dateFormat;
import static java.lang.Math.min;
import static java.lang.System.out;

public class DiviTicker {

    private final String description;
    private final String tickerCode;
    private final String lastDate;
    private final String closeDate;
    private final String dividend;
    private final String percent;
    private final String closePrice;

    public DiviTicker(String description, String tickerCode, String lastDate, String closeDate, String dividend, String percent, String closePrice) {
        this.description = description;
        this.tickerCode = tickerCode;
        this.lastDate = lastDate;
        this.closeDate = closeDate;
        this.dividend = dividend;
        this.percent = percent;
        this.closePrice = closePrice;
    }

    public String getTickerCode() {
        return tickerCode;
    }

    public String getDescription() {
        return description != null && !description.isBlank()
                ? description.substring(0, min(description.length(), 12))
                : "";
    }

    public Date getLastBuyDate() {
        return parseDate(this.lastDate);
    }

    public String getCloseDate() {
        return closeDate;
    }

    public Double getDividend() {
        return parseDouble(dividend);
    }

    public Double getPercent() {
        return parseDouble(percent);
    }

    public Double getPrice() {
        return parseDouble(closePrice);
    }

    private Date parseDate(String value) {
        try {
            return dateFormat.parse(value);
        } catch (Exception ex) {
            out.println("Error: Failed parse date from [" + this + "]: " + ex.getMessage());
            return null;
        }
    }

    private Double parseDouble(String value) {
        if (null == value || value.isBlank() || "n/a".equals(value) || "-".equals(value)) {
            return 0.0;
        }
        try {
            return Double.valueOf(value.replaceAll(",", ".").replaceAll("%", ""));
        } catch (Exception ex) {
            out.println("Error: Failed parse double from [" + this + "]: " + ex.getMessage());
            return 0.0;
        }
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        DiviTicker that = (DiviTicker) o;
        return Objects.equals(tickerCode, that.tickerCode) && Objects.equals(closeDate, that.closeDate);
    }

    @Override
    public int hashCode() {
        return Objects.hash(tickerCode, closeDate);
    }

    @Override
    public String toString() {
        return "DiviTicker{" +
                "code='" + getTickerCode() + '\'' +
                ", description='" + getDescription() + '\'' +
                ", lastBuyDate='" + getLastBuyDate() + '\'' +
                ", closeDate='" + getCloseDate() + '\'' +
                ", dividend='" + getDividend() + '\'' +
                ", price='" + getPrice() + '\'' +
                '}';
    }
}
