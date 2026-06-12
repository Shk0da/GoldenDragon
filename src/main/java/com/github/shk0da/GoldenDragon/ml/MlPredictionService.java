package com.github.shk0da.GoldenDragon.ml;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Simple ML prediction service using logistic regression. Implements model directly in Java for
 * fast inference. Supports per-ticker models for better prediction accuracy.
 */
public class MlPredictionService {

  // Per-ticker models: ticker -> ModelData
  private final Map<String, ModelData> tickerModels;
  private final Map<String, Double> defaultCoefficients;
  private Double defaultIntercept;
  private final Map<String, Double> defaultFeatureMeans;
  private final Map<String, Double> defaultFeatureStds;

  // Thresholds
  private double probabilityThreshold = 0.55;
  private double minConfidence = 0.3;

  public MlPredictionService() {
    this.tickerModels = new ConcurrentHashMap<>();
    this.defaultCoefficients = new HashMap<>();
    this.defaultIntercept = 0.0;
    this.defaultFeatureMeans = new HashMap<>();
    this.defaultFeatureStds = new HashMap<>();
    initializeDefaultModel();
  }

  public MlPredictionService(String modelFile) {
    this.tickerModels = new ConcurrentHashMap<>();
    this.defaultCoefficients = new HashMap<>();
    this.defaultIntercept = 0.0;
    this.defaultFeatureMeans = new HashMap<>();
    this.defaultFeatureStds = new HashMap<>();
    loadDefaultModel(modelFile);
  }

  /**
   * Load model for specific ticker.
   *
   * @param ticker ticker symbol (e.g., "SBER", "GAZP")
   * @param modelFile path to model file
   */
  public void loadModelForTicker(String ticker, String modelFile) {
    ModelData modelData = new ModelData();
    loadModelFromFile(modelFile, modelData);
    tickerModels.put(ticker, modelData);
  }

  /** Get or create model data for ticker. */
  private ModelData getModelData(String ticker) {
    ModelData modelData = tickerModels.get(ticker);
    if (modelData == null) {
      // Return default model if ticker-specific model not found
      return new ModelData(
          defaultCoefficients, defaultIntercept, defaultFeatureMeans, defaultFeatureStds);
    }
    return modelData;
  }

  private void initializeDefaultModel() {
    // Default coefficients based on trained Random Forest feature importances
    defaultCoefficients.put("entry_confidence", 0.43);
    defaultCoefficients.put("risk_reward_ratio", 0.21);
    defaultCoefficients.put("adx", 0.12);
    defaultCoefficients.put("rsi", 0.05);
    defaultCoefficients.put("ema_ratio", 0.04);
    defaultCoefficients.put("atr_ratio", 0.04);
    defaultCoefficients.put("volume_ratio", 0.04);
    defaultCoefficients.put("stop_distance", 0.03);
    defaultCoefficients.put("hour_of_day", 0.02);
    defaultCoefficients.put("day_of_week", 0.01);

    defaultIntercept = 0.0; // Base score

    // Normalization parameters based on typical trading data
    defaultFeatureMeans.put("adx", 25.0);
    defaultFeatureMeans.put("rsi", 50.0);
    defaultFeatureMeans.put("atr_ratio", 1.0);
    defaultFeatureMeans.put("ema_ratio", 1.0);
    defaultFeatureMeans.put("volume_ratio", 1.0);
    defaultFeatureMeans.put("entry_confidence", 0.6);
    defaultFeatureMeans.put("risk_reward_ratio", 2.5);
    defaultFeatureMeans.put("stop_distance", 2.0);
    defaultFeatureMeans.put("hour_of_day", 14.0);
    defaultFeatureMeans.put("day_of_week", 3.0);

    defaultFeatureStds.put("adx", 10.0);
    defaultFeatureStds.put("rsi", 15.0);
    defaultFeatureStds.put("atr_ratio", 0.5);
    defaultFeatureStds.put("ema_ratio", 0.05);
    defaultFeatureStds.put("volume_ratio", 0.5);
    defaultFeatureStds.put("entry_confidence", 0.2);
    defaultFeatureStds.put("risk_reward_ratio", 1.0);
    defaultFeatureStds.put("stop_distance", 1.0);
    defaultFeatureStds.put("hour_of_day", 3.0);
    defaultFeatureStds.put("day_of_week", 1.4);
  }

  /**
   * Predict probability of trade success using default model. Uses weighted sum based on feature
   * importances.
   */
  public double predictProbability(TradeFeatures features) {
    return predictProbability(null, features);
  }

  /**
   * Predict probability of trade success for specific ticker. Uses ticker-specific model if
   * available, otherwise falls back to default model.
   *
   * @param ticker ticker symbol (can be null for default model)
   * @param features trade features
   * @return probability of success
   */
  public double predictProbability(String ticker, TradeFeatures features) {
    ModelData modelData = getModelData(ticker);
    double score = modelData.intercept != null ? modelData.intercept : 0.0;

    for (Map.Entry<String, Double> entry : modelData.coefficients.entrySet()) {
      String feature = entry.getKey();
      double weight = entry.getValue();
      double value = getFeatureValue(features, feature);

      // Normalize feature value
      double mean = modelData.featureMeans.getOrDefault(feature, 0.0);
      double std = modelData.featureStds.getOrDefault(feature, 1.0);
      double normalized = std > 0 ? (value - mean) / std : 0.0;

      // Add weighted contribution (use absolute normalized value for importance-based scoring)
      score += weight * normalized;
    }

    // Convert to probability using sigmoid
    return 1.0 / (1.0 + Math.exp(-score));
  }

  /** Predict if trade should be taken using default model. */
  public boolean shouldTakeTrade(TradeFeatures features) {
    return shouldTakeTrade(null, features);
  }

  /** Predict if trade should be taken for specific ticker. */
  public boolean shouldTakeTrade(String ticker, TradeFeatures features) {
    double prob = predictProbability(ticker, features);
    return prob >= probabilityThreshold && features.entryConfidence >= minConfidence;
  }

  /** Get recommended position size multiplier (0.5x to 2.0x) using default model. */
  public double getPositionSizeMultiplier(TradeFeatures features) {
    return getPositionSizeMultiplier(null, features);
  }

  /** Get recommended position size multiplier for specific ticker. */
  public double getPositionSizeMultiplier(String ticker, TradeFeatures features) {
    double prob = predictProbability(ticker, features);

    // More aggressive sizing with trained model
    if (prob < 0.35) return 0.3; // Very low confidence
    if (prob < 0.45) return 0.5; // Low confidence
    if (prob < 0.55) return 0.75; // Medium confidence
    if (prob < 0.65) return 1.0; // Good confidence
    if (prob < 0.75) return 1.25; // High confidence
    return 1.5; // Very high confidence
  }

  /** Get recommended stop loss multiplier using default model. */
  public double getStopLossMultiplier(TradeFeatures features) {
    return getStopLossMultiplier(null, features);
  }

  /** Get recommended stop loss multiplier for specific ticker. */
  public double getStopLossMultiplier(String ticker, TradeFeatures features) {
    double prob = predictProbability(ticker, features);

    // Higher confidence = wider stop
    if (prob > 0.7) return 3.0;
    if (prob > 0.6) return 2.5;
    if (prob > 0.5) return 2.0;
    return 1.5;
  }

  private double getFeatureValue(TradeFeatures features, String name) {
    switch (name) {
      case "adx":
        return features.adx;
      case "di_plus":
        return features.diPlus;
      case "di_minus":
        return features.diMinus;
      case "atr":
        return features.atr;
      case "rsi":
        return features.rsi;
      case "ema_fast":
        return features.emaFast;
      case "ema_slow":
        return features.emaSlow;
      case "atr_ratio":
        return features.atrRatio;
      case "ema_ratio":
        return features.emaRatio;
      case "price_position":
        return features.pricePosition;
      case "volume_ratio":
        return features.volumeRatio;
      case "volume_trend":
        return features.volumeTrend;
      case "entry_confidence":
        return features.entryConfidence;
      case "risk_reward_ratio":
        return features.riskRewardRatio;
      case "stop_distance":
        return features.stopDistance;
      case "signal_strength":
        return features.signalStrength;
      case "signal_type_trend":
        return features.signalTypeTrend;
      case "signal_type_fx":
        return features.signalTypeFx;
      case "signal_type_mixed":
        return features.signalTypeMixed;
      case "group_confirmed":
        return features.groupConfirmed;
      case "strong_trend":
        return features.strongTrend;
      case "range_regime":
        return features.rangeRegime;
      case "hour_of_day":
        return features.hourOfDay;
      case "day_of_week":
        return features.dayOfWeek;
      case "is_morning":
        return features.isMorning ? 1.0 : 0.0;
      case "is_afternoon":
        return features.isAfternoon ? 1.0 : 0.0;
      default:
        return 0.0;
    }
  }

  private void loadDefaultModel(String modelFile) {
    loadModelFromFile(
        modelFile,
        new ModelData(
            defaultCoefficients, defaultIntercept, defaultFeatureMeans, defaultFeatureStds));
  }

  private void loadModelFromFile(String modelFile, ModelData modelData) {
    modelData.coefficients.clear();
    modelData.featureMeans.clear();
    modelData.featureStds.clear();
    File file = new File(modelFile);
    if (!file.exists()) {
      System.out.println("Model file not found: " + modelFile + ", using default model");
      initializeDefaultModel();
      return;
    }

    try (BufferedReader reader = new BufferedReader(new FileReader(file))) {
      String line;
      String section = null;

      while ((line = reader.readLine()) != null) {
        line = line.trim();
        if (line.isEmpty() || line.startsWith("#")) continue;

        if (line.startsWith("[")) {
          section = line;
          continue;
        }

        if ("[intercept]".equals(section)) {
          modelData.intercept = Double.parseDouble(line);
          continue;
        }

        String[] parts = line.split("=");
        if (parts.length != 2) continue;

        String key = parts[0].trim();
        double value = Double.parseDouble(parts[1].trim());

        if ("[coefficients]".equals(section)) {
          modelData.coefficients.put(key, value);
        } else if ("[intercept]".equals(section)) {
          modelData.intercept = value;
        } else if ("[means]".equals(section)) {
          modelData.featureMeans.put(key, value);
        } else if ("[stds]".equals(section)) {
          modelData.featureStds.put(key, value);
        }
      }
    } catch (IOException | NumberFormatException e) {
      System.err.println("Error loading model: " + e.getMessage());
      initializeDefaultModel();
    }
  }

  public void saveModel(String modelFile) {
    saveModel(
        modelFile, defaultCoefficients, defaultIntercept, defaultFeatureMeans, defaultFeatureStds);
  }

  /** Save model for specific ticker. */
  public void saveModelForTicker(String ticker, String modelFile) {
    ModelData modelData = tickerModels.get(ticker);
    if (modelData != null) {
      saveModel(
          modelFile,
          modelData.coefficients,
          modelData.intercept,
          modelData.featureMeans,
          modelData.featureStds);
    } else {
      System.err.println("No model found for ticker: " + ticker);
    }
  }

  private void saveModel(
      String modelFile,
      Map<String, Double> coefficients,
      Double intercept,
      Map<String, Double> featureMeans,
      Map<String, Double> featureStds) {
    try (PrintWriter writer = new PrintWriter(new FileWriter(modelFile))) {
      writer.println("[coefficients]");
      for (Map.Entry<String, Double> entry : coefficients.entrySet()) {
        writer.println(entry.getKey() + "=" + entry.getValue());
      }

      writer.println("\n[intercept]");
      writer.println(intercept);

      writer.println("\n[means]");
      for (Map.Entry<String, Double> entry : featureMeans.entrySet()) {
        writer.println(entry.getKey() + "=" + entry.getValue());
      }

      writer.println("\n[stds]");
      for (Map.Entry<String, Double> entry : featureStds.entrySet()) {
        writer.println(entry.getKey() + "=" + entry.getValue());
      }

      System.out.println("ML Model saved to: " + modelFile);
    } catch (IOException e) {
      System.err.println("Error saving model: " + e.getMessage());
    }
  }

  // Getters/Setters
  public void setProbabilityThreshold(double threshold) {
    this.probabilityThreshold = threshold;
  }

  public void setMinConfidence(double confidence) {
    this.minConfidence = confidence;
  }

  public void reloadModel(String modelFile) {
    loadDefaultModel(modelFile);
  }

  /** Reload model for specific ticker. */
  public void reloadModelForTicker(String ticker, String modelFile) {
    loadModelForTicker(ticker, modelFile);
  }

  public Map<String, Double> getCoefficients() {
    return Collections.unmodifiableMap(defaultCoefficients);
  }

  public Double getIntercept() {
    return defaultIntercept;
  }

  public double getProbabilityThreshold() {
    return probabilityThreshold;
  }

  /** Internal class to hold model data for a specific ticker. */
  private static class ModelData {
    final Map<String, Double> coefficients;
    Double intercept;
    final Map<String, Double> featureMeans;
    final Map<String, Double> featureStds;

    ModelData() {
      this.coefficients = new HashMap<>();
      this.intercept = 0.0;
      this.featureMeans = new HashMap<>();
      this.featureStds = new HashMap<>();
    }

    ModelData(
        Map<String, Double> coefficients,
        Double intercept,
        Map<String, Double> featureMeans,
        Map<String, Double> featureStds) {
      this.coefficients = new HashMap<>(coefficients);
      this.intercept = intercept;
      this.featureMeans = new HashMap<>(featureMeans);
      this.featureStds = new HashMap<>(featureStds);
    }
  }
}
