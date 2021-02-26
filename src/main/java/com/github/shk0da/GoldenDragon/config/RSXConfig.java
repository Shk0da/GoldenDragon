package com.github.shk0da.GoldenDragon.config;

import com.github.shk0da.GoldenDragon.utils.PropertiesUtils;

import java.util.Properties;

public class RSXConfig {

    public static final String SERIALIZE_NAME = "rsx.json";

    private final String trendStock;
    private final int stockPortfolioMaxSize;

    public RSXConfig() throws Exception {
        final Properties properties = PropertiesUtils.loadProperties();
        trendStock = properties.getProperty("rsx.trendStock", "IWM");
        stockPortfolioMaxSize = Integer.parseInt(properties.getProperty("rsx.stockPortfolioMaxSize", "5"));
    }

    public String getTrendStock() {
        return trendStock;
    }

    public int getStockPortfolioMaxSize() {
        return stockPortfolioMaxSize;
    }
}
