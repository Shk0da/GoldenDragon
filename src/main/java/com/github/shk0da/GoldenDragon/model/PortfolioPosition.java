package com.github.shk0da.GoldenDragon.model;

import java.util.Objects;

public class PortfolioPosition {

    private final String name;
    private final TickerType type;
    private final Double percent;

    public PortfolioPosition(String name, TickerType type, Double percent) {
        this.name = name;
        this.type = type;
        this.percent = percent;
    }

    public String getName() {
        return name;
    }

    public TickerType getType() {
        return type;
    }

    public Double getPercent() {
        return percent;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        PortfolioPosition that = (PortfolioPosition) o;
        return Objects.equals(name, that.name) &&
                type == that.type;
    }

    @Override
    public int hashCode() {
        return Objects.hash(name, type);
    }

    @Override
    public String toString() {
        return "PortfolioPosition{" +
                "name='" + name + '\'' +
                ", type=" + type +
                ", percent=" + percent +
                '}';
    }
}
