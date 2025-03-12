package com.github.shk0da.GoldenDragon.model;

import java.util.List;

public class TickerJson {

    private TickerInfo ticker;
    private List<Double> levels;

    public TickerJson() {
    }

    public TickerJson(TickerInfo ticker, List<Double> levels) {
        this.ticker = ticker;
        this.levels = levels;
    }

    public TickerInfo getTicker() {
        return ticker;
    }

    public List<Double> getLevels() {
        return levels;
    }
}
