package com.github.shk0da.GoldenDragon.config;

import com.github.shk0da.GoldenDragon.utils.PropertiesUtils;

import java.io.IOException;
import java.util.List;
import java.util.Properties;

import static java.util.Arrays.stream;
import static java.util.stream.Collectors.toList;

public class AILConfig {

    private String dataDir;
    private List<String> stocks;

    private Double sensitivityLong;
    private Double sensitivityShort;

    private Boolean slEnabled;
    private Double slPercent;
    private Boolean slAuto;

    private Boolean tpEnabled;
    private Double tpPercent;
    private Boolean tpAuto;

    private Double balanceRiskPercent;
    private Double averagePositionCost;

    private Boolean test;
    private Double learningRate;
    private Integer iterations;
    private Integer seed;
    private Integer score;
    private Integer lstmLayer1Size;
    private Integer lstmLayer2Size;
    private Integer denseLayerSize;
    private Double dropoutRatio;
    private Integer truncatedBPTTLength;

    public AILConfig() throws IOException {
        final Properties properties = PropertiesUtils.loadProperties();
        dataDir = properties.getProperty("ail.dataDir");
        stocks = stream(properties.getProperty("ail.stocks").split(",")).collect(toList());
        sensitivityLong = Double.valueOf(properties.getProperty("ail.sensitivity.long", "0.3"));
        sensitivityShort = Double.valueOf(properties.getProperty("ail.sensitivity.short", "0.1"));
        slEnabled = Boolean.valueOf(properties.getProperty("ail.sl.enabled", "true"));
        slPercent = Double.valueOf(properties.getProperty("ail.sl.percent", "0.3"));
        slAuto = Boolean.valueOf(properties.getProperty("ail.sl.auto", "false"));
        tpEnabled = Boolean.valueOf(properties.getProperty("ail.tp.enabled", "true"));
        tpPercent = Double.valueOf(properties.getProperty("ail.tp.percent", "0.9"));
        tpAuto = Boolean.valueOf(properties.getProperty("ail.tp.auto", "false"));
        balanceRiskPercent = Double.valueOf(properties.getProperty("ail.balanceRiskPercent", "30.0"));
        averagePositionCost = Double.valueOf(properties.getProperty("ail.averagePositionCost", "10000"));
        test = Boolean.valueOf(properties.getProperty("ail.nn.test", "false"));
        learningRate = Double.valueOf(properties.getProperty("ail.nn.learningRate", "0.05"));
        iterations = Integer.valueOf(properties.getProperty("ail.nn.iterations", "1"));
        seed = Integer.valueOf(properties.getProperty("ail.nn.seed", "777"));
        score = Integer.valueOf(properties.getProperty("ail.nn.score", "100"));
        lstmLayer1Size = Integer.valueOf(properties.getProperty("ail.nn.lstmLayer1Size", "256"));
        lstmLayer2Size = Integer.valueOf(properties.getProperty("ail.nn.lstmLayer2Size", "256"));
        denseLayerSize = Integer.valueOf(properties.getProperty("ail.nn.denseLayerSize", "32"));
        dropoutRatio = Double.valueOf(properties.getProperty("ail.nn.dropoutRatio", "0.2"));
        truncatedBPTTLength = Integer.valueOf(properties.getProperty("ail.nn.truncatedBPTTLength", "22"));
    }

    public static final class NetworkProperties {

        private final Boolean test;
        private final Double learningRate;
        private final Integer iterations;
        private final Integer seed;
        private final Integer score;
        private final Integer lstmLayer1Size;
        private final Integer lstmLayer2Size;
        private final Integer denseLayerSize;
        private final Double dropoutRatio;
        private final Integer truncatedBPTTLength;

        public NetworkProperties(Boolean test, Double learningRate, Integer iterations, Integer seed,
                                 Integer score, Integer lstmLayer1Size, Integer lstmLayer2Size,
                                 Integer denseLayerSize, Double dropoutRatio, Integer truncatedBPTTLength) {
            this.test = test;
            this.learningRate = learningRate;
            this.iterations = iterations;
            this.seed = seed;
            this.score = score;
            this.lstmLayer1Size = lstmLayer1Size;
            this.lstmLayer2Size = lstmLayer2Size;
            this.denseLayerSize = denseLayerSize;
            this.dropoutRatio = dropoutRatio;
            this.truncatedBPTTLength = truncatedBPTTLength;
        }

        public Boolean isTest() {
            return test;
        }

        public Double getLearningRate() {
            return learningRate;
        }

        public Integer getIterations() {
            return iterations;
        }

        public Integer getSeed() {
            return seed;
        }

        public Integer getScore() {
            return score;
        }

        public Integer getLstmLayer1Size() {
            return lstmLayer1Size;
        }

        public Integer getLstmLayer2Size() {
            return lstmLayer2Size;
        }

        public Integer getDenseLayerSize() {
            return denseLayerSize;
        }

        public Double getDropoutRatio() {
            return dropoutRatio;
        }

        public Integer getTruncatedBPTTLength() {
            return truncatedBPTTLength;
        }

        @Override
        public String toString() {
            return "NetworkProperties{" +
                    "test=" + test +
                    ", learningRate=" + learningRate +
                    ", iterations=" + iterations +
                    ", seed=" + seed +
                    ", score=" + score +
                    ", lstmLayer1Size=" + lstmLayer1Size +
                    ", lstmLayer2Size=" + lstmLayer2Size +
                    ", denseLayerSize=" + denseLayerSize +
                    ", dropoutRatio=" + dropoutRatio +
                    ", truncatedBPTTLength=" + truncatedBPTTLength +
                    '}';
        }
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

    public Double getSensitivityLong() {
        return sensitivityLong;
    }

    public Double getSensitivityShort() {
        return sensitivityShort;
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

    public void setSensitivityLong(Double sensitivityLong) {
        this.sensitivityLong = sensitivityLong;
    }

    public void setSensitivityShort(Double sensitivityShort) {
        this.sensitivityShort = sensitivityShort;
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

    public void setTest(Boolean test) {
        this.test = test;
    }

    public void setLearningRate(Double learningRate) {
        this.learningRate = learningRate;
    }

    public void setIterations(Integer iterations) {
        this.iterations = iterations;
    }

    public void setSeed(Integer seed) {
        this.seed = seed;
    }

    public void setScore(Integer score) {
        this.score = score;
    }

    public void setLstmLayer1Size(Integer lstmLayer1Size) {
        this.lstmLayer1Size = lstmLayer1Size;
    }

    public void setLstmLayer2Size(Integer lstmLayer2Size) {
        this.lstmLayer2Size = lstmLayer2Size;
    }

    public void setDenseLayerSize(Integer denseLayerSize) {
        this.denseLayerSize = denseLayerSize;
    }

    public void setDropoutRatio(Double dropoutRatio) {
        this.dropoutRatio = dropoutRatio;
    }

    public void setTruncatedBPTTLength(Integer truncatedBPTTLength) {
        this.truncatedBPTTLength = truncatedBPTTLength;
    }

    public NetworkProperties getNetworkProperties() {
        return new NetworkProperties(
                test,
                learningRate,
                iterations,
                seed,
                score,
                lstmLayer1Size,
                lstmLayer2Size,
                denseLayerSize,
                dropoutRatio,
                truncatedBPTTLength
        );
    }

    @Override
    public String toString() {
        return "AILConfig{" +
                "dataDir='" + dataDir + '\'' +
                ", stocks=" + stocks +
                ", sensitivityLong=" + sensitivityLong +
                ", sensitivityShort=" + sensitivityShort +
                ", slEnabled=" + slEnabled +
                ", slPercent=" + slPercent +
                ", slAuto=" + slAuto +
                ", tpEnabled=" + tpEnabled +
                ", tpPercent=" + tpPercent +
                ", tpAuto=" + tpAuto +
                ", balanceRiskPercent=" + balanceRiskPercent +
                ", averagePositionCost=" + averagePositionCost +
                ", test=" + test +
                ", learningRate=" + learningRate +
                ", iterations=" + iterations +
                ", seed=" + seed +
                ", score=" + score +
                ", lstmLayer1Size=" + lstmLayer1Size +
                ", lstmLayer2Size=" + lstmLayer2Size +
                ", denseLayerSize=" + denseLayerSize +
                ", dropoutRatio=" + dropoutRatio +
                ", truncatedBPTTLength=" + truncatedBPTTLength +
                '}';
    }
}
