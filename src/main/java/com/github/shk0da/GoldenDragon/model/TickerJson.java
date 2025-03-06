package com.github.shk0da.GoldenDragon.model;

import java.util.List;

public class TickerJson {

    private TickerInfo ticker;
    private List<Double> levels;
    private Double atr;

    public TickerJson() {
    }

    public TickerJson(TickerInfo ticker, List<Double> levels, Double atr) {
        this.ticker = ticker;
        this.levels = levels;
        this.atr = atr;
    }

    public TickerInfo getTicker() {
        return ticker;
    }

    public List<Double> getLevels() {
        return levels;
    }

    public Double getAtr() {
        return atr;
    }
}
