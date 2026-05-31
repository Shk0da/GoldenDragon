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

/**
 * Simple ML prediction service using logistic regression.
 * Implements model directly in Java for fast inference.
 */
public class MlPredictionService {
    
    // Model coefficients (to be trained and updated)
    private final Map<String, Double> coefficients;
    private Double intercept;
    private final Map<String, Double> featureMeans;
    private final Map<String, Double> featureStds;
    
    // Thresholds
    private double probabilityThreshold = 0.55;
    private double minConfidence = 0.3;
    
    public MlPredictionService() {
        // Initialize with dummy coefficients
        this.coefficients = new HashMap<>();
        this.intercept = 0.0;
        this.featureMeans = new HashMap<>();
        this.featureStds = new HashMap<>();
        initializeDefaultModel();
    }
    
    public MlPredictionService(String modelFile) {
        this.coefficients = new HashMap<>();
        this.intercept = 0.0;
        this.featureMeans = new HashMap<>();
        this.featureStds = new HashMap<>();
        loadModel(modelFile);
    }
    
    private void initializeDefaultModel() {
        // Default coefficients based on trained Random Forest feature importances
        coefficients.put("entry_confidence", 0.43);
        coefficients.put("risk_reward_ratio", 0.21);
        coefficients.put("adx", 0.12);
        coefficients.put("rsi", 0.05);
        coefficients.put("ema_ratio", 0.04);
        coefficients.put("atr_ratio", 0.04);
        coefficients.put("volume_ratio", 0.04);
        coefficients.put("stop_distance", 0.03);
        coefficients.put("hour_of_day", 0.02);
        coefficients.put("day_of_week", 0.01);
        
        intercept = 0.0;  // Base score
        
        // Normalization parameters based on typical trading data
        featureMeans.put("adx", 25.0);
        featureMeans.put("rsi", 50.0);
        featureMeans.put("atr_ratio", 1.0);
        featureMeans.put("ema_ratio", 1.0);
        featureMeans.put("volume_ratio", 1.0);
        featureMeans.put("entry_confidence", 0.6);
        featureMeans.put("risk_reward_ratio", 2.5);
        featureMeans.put("stop_distance", 2.0);
        featureMeans.put("hour_of_day", 14.0);
        featureMeans.put("day_of_week", 3.0);
        
        featureStds.put("adx", 10.0);
        featureStds.put("rsi", 15.0);
        featureStds.put("atr_ratio", 0.5);
        featureStds.put("ema_ratio", 0.05);
        featureStds.put("volume_ratio", 0.5);
        featureStds.put("entry_confidence", 0.2);
        featureStds.put("risk_reward_ratio", 1.0);
        featureStds.put("stop_distance", 1.0);
        featureStds.put("hour_of_day", 3.0);
        featureStds.put("day_of_week", 1.4);
    }
    
    /**
     * Predict probability of trade success.
     * Uses weighted sum based on feature importances.
     */
    public double predictProbability(TradeFeatures features) {
        double score = intercept != null ? intercept : 0.0;
        
        for (Map.Entry<String, Double> entry : coefficients.entrySet()) {
            String feature = entry.getKey();
            double weight = entry.getValue();
            double value = getFeatureValue(features, feature);
            
            // Normalize feature value
            double mean = featureMeans.getOrDefault(feature, 0.0);
            double std = featureStds.getOrDefault(feature, 1.0);
            double normalized = std > 0 ? (value - mean) / std : 0.0;
            
            // Add weighted contribution (use absolute normalized value for importance-based scoring)
            score += weight * normalized;
        }
        
        // Convert to probability using sigmoid
        return 1.0 / (1.0 + Math.exp(-score));
    }
    
    /**
     * Predict if trade should be taken.
     */
    public boolean shouldTakeTrade(TradeFeatures features) {
        double prob = predictProbability(features);
        return prob >= probabilityThreshold && features.entryConfidence >= minConfidence;
    }
    
    /**
     * Get recommended position size multiplier (0.5x to 2.0x).
     */
    public double getPositionSizeMultiplier(TradeFeatures features) {
        double prob = predictProbability(features);
        
        // More aggressive sizing with trained model
        if (prob < 0.35) return 0.3;   // Very low confidence
        if (prob < 0.45) return 0.5;   // Low confidence
        if (prob < 0.55) return 0.75;  // Medium confidence
        if (prob < 0.65) return 1.0;   // Good confidence
        if (prob < 0.75) return 1.25;  // High confidence
        return 1.5;                     // Very high confidence
    }
    
    /**
     * Get recommended stop loss multiplier.
     */
    public double getStopLossMultiplier(TradeFeatures features) {
        double prob = predictProbability(features);
        
        // Higher confidence = wider stop
        if (prob > 0.7) return 3.0;
        if (prob > 0.6) return 2.5;
        if (prob > 0.5) return 2.0;
        return 1.5;
    }
    

    private double getFeatureValue(TradeFeatures features, String name) {
        switch (name) {
            case "adx": return features.adx;
            case "di_plus": return features.diPlus;
            case "di_minus": return features.diMinus;
            case "atr": return features.atr;
            case "rsi": return features.rsi;
            case "ema_fast": return features.emaFast;
            case "ema_slow": return features.emaSlow;
            case "atr_ratio": return features.atrRatio;
            case "ema_ratio": return features.emaRatio;
            case "price_position": return features.pricePosition;
            case "volume_ratio": return features.volumeRatio;
            case "volume_trend": return features.volumeTrend;
            case "entry_confidence": return features.entryConfidence;
            case "risk_reward_ratio": return features.riskRewardRatio;
            case "stop_distance": return features.stopDistance;
            case "signal_strength": return features.signalStrength;
            case "signal_type_trend": return features.signalTypeTrend;
            case "signal_type_fx": return features.signalTypeFx;
            case "signal_type_mixed": return features.signalTypeMixed;
            case "group_confirmed": return features.groupConfirmed;
            case "strong_trend": return features.strongTrend;
            case "range_regime": return features.rangeRegime;
            case "hour_of_day": return features.hourOfDay;
            case "day_of_week": return features.dayOfWeek;
            case "is_morning": return features.isMorning ? 1.0 : 0.0;
            case "is_afternoon": return features.isAfternoon ? 1.0 : 0.0;
            default: return 0.0;
        }
    }
    
    private void loadModel(String modelFile) {
        coefficients.clear();
        featureMeans.clear();
        featureStds.clear();
        File file = new File(modelFile);
        if (!file.exists()) {
            System.out.println("Model file not found, using default model");
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
                    this.intercept = Double.parseDouble(line);
                    continue;
                }
                
                String[] parts = line.split("=");
                if (parts.length != 2) continue;
                
                String key = parts[0].trim();
                double value = Double.parseDouble(parts[1].trim());
                
                if ("[coefficients]".equals(section)) {
                    coefficients.put(key, value);
                } else if ("[intercept]".equals(section)) {
                    this.intercept = value;
                } else if ("[means]".equals(section)) {
                    featureMeans.put(key, value);
                } else if ("[stds]".equals(section)) {
                    featureStds.put(key, value);
                }
            }
        } catch (IOException | NumberFormatException e) {
            System.err.println("Error loading model: " + e.getMessage());
            initializeDefaultModel();
        }
    }
    
    public void saveModel(String modelFile) {
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
        loadModel(modelFile);
    }
    
    public Map<String, Double> getCoefficients() {
        return Collections.unmodifiableMap(coefficients);
    }
    
    public Double getIntercept() {
        return intercept;
    }

    public double getProbabilityThreshold() {
        return probabilityThreshold;
    }
}
