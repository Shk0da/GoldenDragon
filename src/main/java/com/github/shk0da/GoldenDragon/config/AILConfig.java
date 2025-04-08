package com.github.shk0da.GoldenDragon.config;

import com.github.shk0da.GoldenDragon.utils.PropertiesUtils;

import java.util.List;
import java.util.Properties;

import static java.util.Arrays.stream;
import static java.util.stream.Collectors.toList;

public class AILConfig {

    private final String dataDir;
    private final List<String> stocks;

    private final Boolean slEnabled;
    private final Double slPercent;
    private final Boolean slAuto;

    private final Boolean tpEnabled;
    private final Double tpPercent;
    private final Boolean tpAuto;

    private final Double balanceRiskPercent;
    private final Double averagePositionCostToBuy;

    public AILConfig() throws Exception {
        final Properties properties = PropertiesUtils.loadProperties();
        dataDir = properties.getProperty("ail.dataDir");
        stocks = stream(properties.getProperty("ail.stocks").split(",")).collect(toList());
        slEnabled = Boolean.valueOf(properties.getProperty("ail.sl.enabled", "true"));
        slPercent = Double.valueOf(properties.getProperty("ail.sl.percent", "0.3"));
        slAuto = Boolean.valueOf(properties.getProperty("ail.sl.auto", "false"));
        tpEnabled = Boolean.valueOf(properties.getProperty("ail.tp.enabled", "true"));
        tpPercent = Double.valueOf(properties.getProperty("ail.tp.percent", "0.9"));
        tpAuto = Boolean.valueOf(properties.getProperty("ail.tp.auto", "false"));
        balanceRiskPercent = Double.valueOf(properties.getProperty("ail.balanceRiskPercent", "10.0"));
        averagePositionCostToBuy = Double.valueOf(properties.getProperty("ail.averagePositionCostToBuy", "10000"));
    }

    public String getDataDir() {
        return dataDir;
    }

    public List<String> getStocks() {
        return stocks;
    }

    public Boolean isSlEnabled() {
        return slEnabled;
    }

    public Double getSlPercent() {
        return slPercent;
    }

    public Boolean isSlAuto() {
        return slAuto;
    }

    public Boolean isTpEnabled() {
        return tpEnabled;
    }

    public Double getTpPercent() {
        return tpPercent;
    }

    public Boolean isTpAuto() {
        return tpAuto;
    }

    public Double getBalanceRiskPercent() {
        return balanceRiskPercent;
    }

    public Double getAveragePositionCostToBuy() {
        return averagePositionCostToBuy;
    }
}
