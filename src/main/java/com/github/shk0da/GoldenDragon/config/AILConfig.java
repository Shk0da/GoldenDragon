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

    private Double boosterSensitivityLong;
    private Double boosterSensitivityShort;

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
    private Double l2;
    private Integer iterations;
    private Integer seed;
    private Integer score;
    private Integer lstmLayer1Size;
    private Integer lstmLayer2Size;
    private Integer denseLayerSize;
    private Double dropoutRatio;
    private Integer truncatedBPTTLength;

    private String booster;
    private Double eta;
    private Integer maxDepth;
    private Integer minChildWeight;
    private Double gamma;
    private Integer alpha;
    private Double subsample;
    private Double colsampleBytree;
    private Integer lambda;
    private String objective;
    private String evalMetric;
    private Double scalePosWeight;
    private Double baseScore;
    private Double boosterLearningRate;
    private Integer boosterEstimators;

    private Double sumOfDecision;

    public AILConfig() throws IOException {
        final Properties properties = PropertiesUtils.loadProperties();
        dataDir = properties.getProperty("ail.dataDir");
        stocks = stream(properties.getProperty("ail.stocks").split(",")).collect(toList());
        sensitivityLong = Double.valueOf(properties.getProperty("ail.sensitivity.long", "0.3"));
        sensitivityShort = Double.valueOf(properties.getProperty("ail.sensitivity.short", "0.1"));
        boosterSensitivityLong = Double.valueOf(properties.getProperty("ail.booster.sensitivity.long", "0.05"));
        boosterSensitivityShort = Double.valueOf(properties.getProperty("ail.booster.sensitivity.short", "0.05"));
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
        l2 = Double.valueOf(properties.getProperty("ail.nn.l2", "0.0001"));
        iterations = Integer.valueOf(properties.getProperty("ail.nn.iterations", "1"));
        seed = Integer.valueOf(properties.getProperty("ail.nn.seed", "777"));
        score = Integer.valueOf(properties.getProperty("ail.nn.score", "100"));
        lstmLayer1Size = Integer.valueOf(properties.getProperty("ail.nn.lstmLayer1Size", "256"));
        lstmLayer2Size = Integer.valueOf(properties.getProperty("ail.nn.lstmLayer2Size", "256"));
        denseLayerSize = Integer.valueOf(properties.getProperty("ail.nn.denseLayerSize", "32"));
        dropoutRatio = Double.valueOf(properties.getProperty("ail.nn.dropoutRatio", "0.2"));
        truncatedBPTTLength = Integer.valueOf(properties.getProperty("ail.nn.truncatedBPTTLength", "22"));
        booster = properties.getProperty("ail.booster.booster", "gbtree");
        eta = Double.valueOf(properties.getProperty("ail.booster.eta", "0.01"));
        maxDepth = Integer.valueOf(properties.getProperty("ail.booster.maxDepth", "16"));
        minChildWeight = Integer.valueOf(properties.getProperty("ail.booster.minChildWeight", "1"));
        gamma = Double.valueOf(properties.getProperty("ail.booster.gamma", "0"));
        alpha = Integer.valueOf(properties.getProperty("ail.booster.alpha", "0"));
        subsample = Double.valueOf(properties.getProperty("ail.booster.subsample", "0.8"));
        colsampleBytree = Double.valueOf(properties.getProperty("ail.booster.colsampleBytree", "0.8"));
        lambda = Integer.valueOf(properties.getProperty("ail.booster.lambda", "1"));
        objective = String.valueOf(properties.getProperty("ail.booster.objective", "reg:squarederror"));
        evalMetric = String.valueOf(properties.getProperty("ail.booster.evalMetric", "logloss"));
        scalePosWeight = Double.valueOf(properties.getProperty("ail.booster.scalePosWeight", "1"));
        baseScore = Double.valueOf(properties.getProperty("ail.booster.baseScore", "0.5"));
        boosterLearningRate = Double.valueOf(properties.getProperty("ail.booster.learningRate", "0.001"));
        boosterEstimators = Integer.valueOf(properties.getProperty("ail.booster.estimators", "100"));
        sumOfDecision = Double.valueOf(properties.getProperty("ail.sumOfDecision", "0.05"));
    }

    public static final class NetworkProperties {

        private final Boolean test;
        private final Double learningRate;
        private final Double l2;
        private final Integer iterations;
        private final Integer seed;
        private final Integer score;
        private final Integer lstmLayer1Size;
        private final Integer lstmLayer2Size;
        private final Integer denseLayerSize;
        private final Double dropoutRatio;
        private final Integer truncatedBPTTLength;
        private final Double sensitivityLong;
        private final Double sensitivityShort;

        public NetworkProperties(Boolean test, Double learningRate, Double l2, Integer iterations, Integer seed,
                                 Integer score, Integer lstmLayer1Size, Integer lstmLayer2Size,
                                 Integer denseLayerSize, Double dropoutRatio, Integer truncatedBPTTLength,
                                 Double sensitivityLong, Double sensitivityShort) {
            this.test = test;
            this.learningRate = learningRate;
            this.l2 = l2;
            this.iterations = iterations;
            this.seed = seed;
            this.score = score;
            this.lstmLayer1Size = lstmLayer1Size;
            this.lstmLayer2Size = lstmLayer2Size;
            this.denseLayerSize = denseLayerSize;
            this.dropoutRatio = dropoutRatio;
            this.truncatedBPTTLength = truncatedBPTTLength;
            this.sensitivityLong = sensitivityLong;
            this.sensitivityShort = sensitivityShort;
        }

        public Boolean isTest() {
            return test;
        }

        public Double getLearningRate() {
            return learningRate;
        }

        public Double getL2() {
            return l2;
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

        public Double getSensitivityLong() {
            return sensitivityLong;
        }

        public Double getSensitivityShort() {
            return sensitivityShort;
        }

        @Override
        public String toString() {
            return "NetworkProperties{" +
                    "test=" + test +
                    ", learningRate=" + learningRate +
                    ", l2=" + l2 +
                    ", iterations=" + iterations +
                    ", seed=" + seed +
                    ", score=" + score +
                    ", lstmLayer1Size=" + lstmLayer1Size +
                    ", lstmLayer2Size=" + lstmLayer2Size +
                    ", denseLayerSize=" + denseLayerSize +
                    ", dropoutRatio=" + dropoutRatio +
                    ", truncatedBPTTLength=" + truncatedBPTTLength +
                    ", sensitivityLong=" + sensitivityLong +
                    ", sensitivityShort=" + sensitivityShort +
                    '}';
        }
    }

    public static final class BoosterProperties {

        private final Integer numRounds = 100;
        private final String booster;
        private final Double eta;
        private final Integer maxDepth;
        private final Integer minChildWeight;
        private final Double gamma;
        private final Integer alpha;
        private final Double subsample;
        private final Double colsampleBytree;
        private final Integer lambda;
        private final String objective;
        private final String evalMetric;
        private final Double scalePosWeight;
        private final Double baseScore;
        private final Double boosterLearningRate;
        private final Integer boosterEstimators;
        private final Double boosterSensitivityLong;
        private final Double boosterSensitivityShort;

        public BoosterProperties(String booster, Double eta, Integer maxDepth, Integer minChildWeight, Double gamma,
                                 Integer alpha, Double subsample, Double colsampleBytree, Integer lambda, String objective,
                                 String evalMetric, Double scalePosWeight, Double baseScore, Double boosterLearningRate,
                                 Integer boosterEstimators, Double boosterSensitivityLong, Double boosterSensitivityShort) {
            this.booster = booster;
            this.eta = eta;
            this.maxDepth = maxDepth;
            this.minChildWeight = minChildWeight;
            this.gamma = gamma;
            this.alpha = alpha;
            this.subsample = subsample;
            this.colsampleBytree = colsampleBytree;
            this.lambda = lambda;
            this.objective = objective;
            this.evalMetric = evalMetric;
            this.scalePosWeight = scalePosWeight;
            this.baseScore = baseScore;
            this.boosterLearningRate = boosterLearningRate;
            this.boosterEstimators = boosterEstimators;
            this.boosterSensitivityLong = boosterSensitivityLong;
            this.boosterSensitivityShort = boosterSensitivityShort;
        }

        public Integer getAlpha() {
            return alpha;
        }

        public Integer getNumRounds() {
            return numRounds;
        }

        public String getBooster() {
            return booster;
        }

        public Double getEta() {
            return eta;
        }

        public Integer getMaxDepth() {
            return maxDepth;
        }

        public Integer getMinChildWeight() {
            return minChildWeight;
        }

        public Double getGamma() {
            return gamma;
        }

        public Double getSubsample() {
            return subsample;
        }

        public Double getColsampleBytree() {
            return colsampleBytree;
        }

        public Integer getLambda() {
            return lambda;
        }

        public String getObjective() {
            return objective;
        }

        public String getEvalMetric() {
            return evalMetric;
        }

        public Double getScalePosWeight() {
            return scalePosWeight;
        }

        public Double getBaseScore() {
            return baseScore;
        }

        public Double getBoosterLearningRate() {
            return boosterLearningRate;
        }

        public Integer getBoosterEstimators() {
            return boosterEstimators;
        }

        public Double getBoosterSensitivityLong() {
            return boosterSensitivityLong;
        }

        public Double getBoosterSensitivityShort() {
            return boosterSensitivityShort;
        }

        @Override
        public String toString() {
            return "BoosterProperties{" +
                    "numRounds=" + numRounds +
                    ", booster='" + booster + '\'' +
                    ", eta=" + eta +
                    ", maxDepth=" + maxDepth +
                    ", minChildWeight=" + minChildWeight +
                    ", alpha=" + alpha +
                    ", gamma=" + gamma +
                    ", subsample=" + subsample +
                    ", colsampleBytree=" + colsampleBytree +
                    ", lambda=" + lambda +
                    ", objective='" + objective + '\'' +
                    ", evalMetric='" + evalMetric + '\'' +
                    ", scalePosWeight=" + scalePosWeight +
                    ", baseScore=" + baseScore +
                    ", boosterLearningRate=" + boosterLearningRate +
                    ", boosterEstimators=" + boosterEstimators +
                    ", boosterSensitivityLong=" + boosterSensitivityLong +
                    ", boosterSensitivityShort=" + boosterSensitivityShort +
                    '}';
        }
    }

    public void setDataDir(String dataDir) {
        this.dataDir = dataDir;
    }

    public void setStocks(List<String> stocks) {
        this.stocks = stocks;
    }

    public Double getSumOfDecision() {
        return sumOfDecision;
    }

    public void setSumOfDecision(Double sumOfDecision) {
        this.sumOfDecision = sumOfDecision;
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

    public Double getBoosterSensitivityLong() {
        return boosterSensitivityLong;
    }

    public Double getBoosterSensitivityShort() {
        return boosterSensitivityShort;
    }

    public void setBoosterSensitivityLong(Double boosterSensitivityLong) {
        this.boosterSensitivityLong = boosterSensitivityLong;
    }

    public void setBoosterSensitivityShort(Double boosterSensitivityShort) {
        this.boosterSensitivityShort = boosterSensitivityShort;
    }

    public Boolean isSlEnabled() {
        return slEnabled;
    }

    public void setBooster(String booster) {
        this.booster = booster;
    }

    public void setEta(Double eta) {
        this.eta = eta;
    }

    public void setMaxDepth(Integer maxDepth) {
        this.maxDepth = maxDepth;
    }

    public void setMinChildWeight(Integer minChildWeight) {
        this.minChildWeight = minChildWeight;
    }

    public void setGamma(Double gamma) {
        this.gamma = gamma;
    }

    public void setAlpha(Integer alpha) {
        this.alpha = alpha;
    }

    public void setSubsample(Double subsample) {
        this.subsample = subsample;
    }

    public void setColsampleBytree(Double colsampleBytree) {
        this.colsampleBytree = colsampleBytree;
    }

    public void setLambda(Integer lambda) {
        this.lambda = lambda;
    }

    public void setObjective(String objective) {
        this.objective = objective;
    }

    public void setEvalMetric(String evalMetric) {
        this.evalMetric = evalMetric;
    }

    public void setScalePosWeight(Double scalePosWeight) {
        this.scalePosWeight = scalePosWeight;
    }

    public void setBaseScore(Double baseScore) {
        this.baseScore = baseScore;
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

    public Integer getAlpha() {
        return alpha;
    }

    public void setBoosterEstimators(Integer boosterEstimators) {
        this.boosterEstimators = boosterEstimators;
    }

    public void setAveragePositionCost(Double averagePositionCost) {
        this.averagePositionCost = averagePositionCost;
    }

    public void setTest(Boolean test) {
        this.test = test;
    }

    public void setL2(Double l2) {
        this.l2 = l2;
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

    public void setBoosterLearningRate(Double boosterLearningRate) {
        this.boosterLearningRate = boosterLearningRate;
    }

    public NetworkProperties getNetworkProperties() {
        return new NetworkProperties(
                test,
                learningRate,
                l2,
                iterations,
                seed,
                score,
                lstmLayer1Size,
                lstmLayer2Size,
                denseLayerSize,
                dropoutRatio,
                truncatedBPTTLength,
                sensitivityLong,
                sensitivityShort
        );
    }

    public BoosterProperties getBoosterProperties() {
        return new BoosterProperties(
                booster,
                eta,
                maxDepth,
                minChildWeight,
                gamma,
                alpha,
                subsample,
                colsampleBytree,
                lambda,
                objective,
                evalMetric,
                scalePosWeight,
                baseScore,
                boosterLearningRate,
                boosterEstimators,
                boosterSensitivityLong,
                boosterSensitivityShort
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
                ", sumOfDecision=" + sumOfDecision +
                '}';
    }
}
