package com.github.shk0da.GoldenDragon.config;

import com.github.shk0da.GoldenDragon.utils.PropertiesUtils;
import java.io.IOException;
import java.util.List;
import java.util.Properties;


import static java.util.Arrays.stream;
import static java.util.stream.Collectors.toList;

public class LevelTraderConfig {

    private String dataDir;
    private List<String> stocks;

    private Boolean slEnabled;
    private Double slPercent;
    private Boolean slAuto;

    private Boolean tpEnabled;
    private Double tpPercent;
    private Boolean tpAuto;

    private Double balanceRiskPercent;
    private Double averagePositionCost;

    public int levelConfirmationTouches;
    public int confirmationCandles;

    public LevelTraderConfig() throws IOException {
        final Properties properties = PropertiesUtils.loadProperties();

        dataDir = properties.getProperty("datacollector.dataDir", "data");
        stocks = stream(properties.getProperty(
                "levelTrader.stocks",
                properties.getProperty("datacollector.stocks")
        ).split(",")).collect(toList());

        slEnabled = Boolean.valueOf(properties.getProperty("levelTrader.sl.enabled", "true"));
        slPercent = Double.valueOf(properties.getProperty("levelTrader.sl.percent", "0.3"));
        slAuto = Boolean.valueOf(properties.getProperty("levelTrader.sl.auto", "false"));
        tpEnabled = Boolean.valueOf(properties.getProperty("levelTrader.tp.enabled", "true"));
        tpPercent = Double.valueOf(properties.getProperty("levelTrader.tp.percent", "0.9"));
        tpAuto = Boolean.valueOf(properties.getProperty("levelTrader.tp.auto", "false"));
        balanceRiskPercent = Double.valueOf(properties.getProperty("levelTrader.balanceRiskPercent", "30.0"));
        averagePositionCost = Double.valueOf(properties.getProperty("levelTrader.averagePositionCost", "10000"));

        levelConfirmationTouches = Integer.parseInt(properties.getProperty("levelTrader.levelConfirmationTouches", "3"));
        confirmationCandles = Integer.parseInt(properties.getProperty("levelTrader.confirmationCandles", "3"));
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

    public Double getAveragePositionCost() {
        return averagePositionCost;
    }

    public void setSlEnabled(Boolean slEnabled) {
        this.slEnabled = slEnabled;
    }

    public void setSlPercent(Double slPercent) {
        this.slPercent = slPercent;
    }

    public void setSlAuto(Boolean slAuto) {
        this.slAuto = slAuto;
    }

    public void setTpEnabled(Boolean tpEnabled) {
        this.tpEnabled = tpEnabled;
    }

    public void setTpPercent(Double tpPercent) {
        this.tpPercent = tpPercent;
    }

    public void setTpAuto(Boolean tpAuto) {
        this.tpAuto = tpAuto;
    }

    public void setBalanceRiskPercent(Double balanceRiskPercent) {
        this.balanceRiskPercent = balanceRiskPercent;
    }

    public void setAveragePositionCost(Double averagePositionCost) {
        this.averagePositionCost = averagePositionCost;
    }

    @Override
    public String toString() {
        return "LevelTraderConfig{" +
                "dataDir='" + dataDir + '\'' +
                ", stocks=" + stocks +
                ", slEnabled=" + slEnabled +
                ", slPercent=" + slPercent +
                ", slAuto=" + slAuto +
                ", tpEnabled=" + tpEnabled +
                ", tpPercent=" + tpPercent +
                ", tpAuto=" + tpAuto +
                ", balanceRiskPercent=" + balanceRiskPercent +
                ", averagePositionCost=" + averagePositionCost +
                '}';
    }
}
