package com.github.shk0da.GoldenDragon.config;

import com.github.shk0da.GoldenDragon.filters.BadWeatherFilter;
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
        public final double marketRegimeAdxRangeThreshold;
        public final double marketRegimeAdxUnclearThreshold;
        public final double marketRegimeVolumeRatioMin;
        public final double marketRegimeConfidenceMin;
        public final int marketRegimeAtrBars;
        public final BadWeatherFilter.Params badWeatherParams;

        public TickerParams(String group, double slMult, double tpMult, double riskP, boolean useMinuteCandles) {
            this(group, slMult, tpMult, riskP, useMinuteCandles, 20.0, 25.0, 30.0, 50.0, 4, new BadWeatherFilter.Params());
        }

        public TickerParams(String group, double slMult, double tpMult, double riskP, boolean useMinuteCandles,
                            double marketRegimeAdxRangeThreshold, double marketRegimeAdxUnclearThreshold,
                            double marketRegimeVolumeRatioMin, double marketRegimeConfidenceMin,
                            int marketRegimeAtrBars, BadWeatherFilter.Params badWeatherParams) {
            this.group = group;
            this.slMult = slMult;
            this.tpMult = tpMult;
            this.riskP = riskP;
            this.useMinuteCandles = useMinuteCandles;
            this.marketRegimeAdxRangeThreshold = marketRegimeAdxRangeThreshold;
            this.marketRegimeAdxUnclearThreshold = marketRegimeAdxUnclearThreshold;
            this.marketRegimeVolumeRatioMin = marketRegimeVolumeRatioMin;
            this.marketRegimeConfidenceMin = marketRegimeConfidenceMin;
            this.marketRegimeAtrBars = marketRegimeAtrBars;
            this.badWeatherParams = badWeatherParams;
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
            result.put(stock, loadSingleTickerParams(properties, stock));
        }
        return result;
    }

    private TickerParams loadSingleTickerParams(Properties properties, String ticker) {
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

        double adxRangeThreshold = Double.parseDouble(properties.getProperty(
                prefix + "marketRegimeAdxRangeThreshold", "20.0"));
        double adxUnclearThreshold = Double.parseDouble(properties.getProperty(
                prefix + "marketRegimeAdxUnclearThreshold", "25.0"));
        double volumeRatioMin = Double.parseDouble(properties.getProperty(
                prefix + "marketRegimeVolumeRatioMin", "30.0"));
        double confidenceMin = Double.parseDouble(properties.getProperty(
                prefix + "marketRegimeConfidenceMin", "50.0"));
        int atrBars = Integer.parseInt(properties.getProperty(
                prefix + "marketRegimeAtrBars", "4"));

        double lowVolumeThreshold = Double.parseDouble(properties.getProperty(
                prefix + "badWeatherLowVolumeThreshold", "0.5"));
        double lowAtrThreshold = Double.parseDouble(properties.getProperty(
                prefix + "badWeatherLowAtrThreshold", "0.7"));
        double minRangePercent = Double.parseDouble(properties.getProperty(
                prefix + "badWeatherMinRangePercent", "0.005"));
        double highAtrThreshold = Double.parseDouble(properties.getProperty(
                prefix + "badWeatherHighAtrThreshold", "2.0"));
        double maxSpreadPercent = Double.parseDouble(properties.getProperty(
                prefix + "badWeatherMaxSpreadPercent", "0.01"));
        double maxWickRatio = Double.parseDouble(properties.getProperty(
                prefix + "badWeatherMaxWickRatio", "0.4"));
        double panicVolumeThreshold = Double.parseDouble(properties.getProperty(
                prefix + "badWeatherPanicVolumeThreshold", "3.0"));
        double minAvgDailyVolume = Double.parseDouble(properties.getProperty(
                prefix + "badWeatherMinAvgDailyVolume", "100000"));
        double atrSpikeThreshold = Double.parseDouble(properties.getProperty(
                prefix + "badWeatherAtrSpikeThreshold", "2.5"));

        BadWeatherFilter.Params badWeatherParams = new BadWeatherFilter.Params(
                lowVolumeThreshold, lowAtrThreshold, minRangePercent,
                highAtrThreshold, maxSpreadPercent, maxWickRatio,
                panicVolumeThreshold, minAvgDailyVolume, atrSpikeThreshold
        );

        return new TickerParams(group, slMult, tpMult, riskP, useMinuteCandles,
                adxRangeThreshold, adxUnclearThreshold, volumeRatioMin, confidenceMin, atrBars,
                badWeatherParams);
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
        TickerParams tp = loadSingleTickerParams(properties, ticker);
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
