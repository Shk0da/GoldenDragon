package com.github.shk0da.GoldenDragon.config;

import com.github.shk0da.GoldenDragon.model.Market;

public final class MarketConfig {

    private final Market market;
    private final int startWorkHour;
    private final int endWorkHour;
    private final int maxPositionCostToBuy;
    private final String currency;

    public MarketConfig(Market market, int startWorkHour, int endWorkHour, int maxPositionCostToBuy, String currency) {
        this.market = market;
        this.startWorkHour = startWorkHour;
        this.endWorkHour = endWorkHour;
        this.maxPositionCostToBuy = maxPositionCostToBuy;
        this.currency = currency;
    }

    public static MarketConfig createDefault() {
        return new MarketConfig(Market.MOEX, 10, 18, 100_000, "RUB");
    }

    public Market getMarket() {
        return market;
    }

    public int getStartWorkHour() {
        return startWorkHour;
    }

    public int getEndWorkHour() {
        return endWorkHour;
    }

    public int getMaxPositionCostToBuy() {
        return maxPositionCostToBuy;
    }

    public String getCurrency() {
        return currency;
    }

    @Override
    public String toString() {
        return "MarketConfig{" +
                "market=" + market +
                ", startWorkHour=" + startWorkHour +
                ", endWorkHour=" + endWorkHour +
                ", maxPositionCostToBuy=" + maxPositionCostToBuy +
                ", currency='" + currency + '\'' +
                '}';
    }
}
