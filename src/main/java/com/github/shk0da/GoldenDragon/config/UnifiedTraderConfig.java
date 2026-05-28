package com.github.shk0da.GoldenDragon.config;

import com.github.shk0da.GoldenDragon.utils.PropertiesUtils;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;


import static java.util.Arrays.stream;
import static java.util.stream.Collectors.toList;

public class UnifiedTraderConfig {

    public static class TickerParams {

        public final String group;
        public final double slMult;
        public final double tpMult;
        public final double riskP;
        public final boolean useMinuteCandles;

        public TickerParams(String group, double slMult, double tpMult, double riskP, boolean useMinuteCandles) {
            this.group = group;
            this.slMult = slMult;
            this.tpMult = tpMult;
            this.riskP = riskP;
            this.useMinuteCandles = useMinuteCandles;
        }
    }

    private String dataDir;
    private List<String> stocks;
    private Double averagePositionCost;
    private final Map<String, TickerParams> tickerParams;
    private final Properties properties;

    public UnifiedTraderConfig() throws IOException {
        final Properties properties = PropertiesUtils.loadProperties();
        this.properties = properties;
        dataDir = properties.getProperty("datacollector.dataDir", "data");
        stocks = stream(properties.getProperty(
                "levelTrader.stocks",
                properties.getProperty("datacollector.stocks")
        ).split(",")).collect(toList());
        averagePositionCost = Double.valueOf(properties.getProperty("unifiedTrader.averagePositionCost", "10000"));
        this.tickerParams = loadTickerParams(properties);
    }

    private Map<String, TickerParams> loadTickerParams(Properties properties) {
        Map<String, TickerParams> result = new HashMap<>();
        for (String stock : stocks) {
            String prefix = "unifiedTrader.ticker." + stock + ".";
            String group = properties.getProperty(prefix + "group", "TREND");
            double slMult = Double.parseDouble(properties.getProperty(
                    prefix + "slMult", getGroupDefault(properties, group, "slMult", "1.2")));
            double tpMult = Double.parseDouble(properties.getProperty(
                    prefix + "tpMult", getGroupDefault(properties, group, "tpMult", "2.5")));
            double riskP = Double.parseDouble(properties.getProperty(
                    prefix + "riskP", getGroupDefault(properties, group, "riskP", "0.01")));
            boolean useMinuteCandles = Boolean.parseBoolean(properties.getProperty(
                    prefix + "useMinuteCandles", "true"));
            result.put(stock, new TickerParams(group, slMult, tpMult, riskP, useMinuteCandles));
        }
        return result;
    }

    private String getGroupDefault(Properties properties, String group, String field, String defaultValue) {
        String prefix = "unifiedTrader.group." + group + ".";
        String value = properties.getProperty(prefix + field);
        return value != null ? value : defaultValue;
    }

    public TickerParams getTickerParams(String ticker) {
        TickerParams params = tickerParams.get(ticker);
        if (params != null) return params;
        return loadTickerParamsFor(ticker);
    }

    public String getTickerGroup(String ticker) {
        return getTickerParams(ticker).group;
    }

    private TickerParams loadTickerParamsFor(String ticker) {
        String prefix = "unifiedTrader.ticker." + ticker + ".";
        String group = properties.getProperty(prefix + "group", "TREND");
        double slMult = Double.parseDouble(properties.getProperty(
                prefix + "slMult", getGroupDefault(properties, group, "slMult", "1.2")));
        double tpMult = Double.parseDouble(properties.getProperty(
                prefix + "tpMult", getGroupDefault(properties, group, "tpMult", "2.5")));
        double riskP = Double.parseDouble(properties.getProperty(
                prefix + "riskP", getGroupDefault(properties, group, "riskP", "0.01")));
        boolean useMinuteCandles = Boolean.parseBoolean(properties.getProperty(
                prefix + "useMinuteCandles", "true"));
        TickerParams tp = new TickerParams(group, slMult, tpMult, riskP, useMinuteCandles);
        tickerParams.put(ticker, tp);
        return tp;
    }

    public void setDataDir(String dataDir) {
        this.dataDir = dataDir;
    }

    public void setStocks(List<String> stocks) {
        this.stocks = stocks;
    }

    public String getDataDir() {
        return dataDir;
    }

    public List<String> getStocks() {
        return stocks;
    }

    public Double getAveragePositionCost() {
        return averagePositionCost;
    }

    @Override
    public String toString() {
        return "UnifiedTraderConfig{" +
                "dataDir='" + dataDir + '\'' +
                ", stocks=" + stocks +
                '}';
    }
}
