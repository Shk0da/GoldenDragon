package com.github.shk0da.GoldenDragon.config;

import com.github.shk0da.GoldenDragon.model.Market;
import com.github.shk0da.GoldenDragon.utils.PropertiesUtils;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;


import static java.lang.System.out;

public final class MarketConfig {

    private static final Map<Market, MarketConfig> defaultMarketConfigs = new HashMap<>();

    private final Market market;
    private final int maxPositionCostToBuy;
    private final String currency;

    static {
        try {
            final Properties properties = PropertiesUtils.loadProperties();
            defaultMarketConfigs.put(
                    Market.MOEX,
                    new MarketConfig(
                            Market.MOEX,
                            Integer.parseInt(properties.getProperty("market.moex.maxPositionCostToBuy", "100000")),
                            properties.getProperty("market.moex.currency", "RUB"))
            );
        } catch (Exception ex) {
            out.println("Failed load Market config: " + ex.getMessage());
            defaultMarketConfigs.put(Market.MOEX, createDefaultMOEX());
        }
    }

    private MarketConfig(Market market, int maxPositionCostToBuy, String currency) {
        this.market = market;
        this.maxPositionCostToBuy = maxPositionCostToBuy;
        this.currency = currency;
    }

    public static MarketConfig byMarket(Market market) {
        return defaultMarketConfigs.getOrDefault(market, MarketConfig.createDefaultMOEX());
    }

    private static MarketConfig createDefaultMOEX() {
        return new MarketConfig(Market.MOEX, 100_000, "RUB");
    }

    public Market getMarket() {
        return market;
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
                ", maxPositionCostToBuy=" + maxPositionCostToBuy +
                ", currency='" + currency + '\'' +
                '}';
    }
}
