package com.github.shk0da.GoldenDragon.ml;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.time.LocalDateTime;
import java.time.YearMonth;
import java.time.format.DateTimeParseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

public class MlModelTrainer {

    private static final int RANDOM_SEED = 42;
    private static final int MIN_SAMPLES = 100;
    private static final double DEFAULT_THRESHOLD = 0.55;
    private static final int DEFAULT_EPOCHS = 1500;
    private static final double DEFAULT_LEARNING_RATE = 0.05;
    private static final double DEFAULT_L2 = 0.0005;
    private static final int EARLY_STOPPING_PATIENCE = 100;
    private static final double MIN_THRESHOLD = 0.45;
    private static final double MAX_THRESHOLD = 0.75;
    private static final double THRESHOLD_STEP = 0.01;
    private static final List<String> FEATURE_COLUMNS = List.of(
            "adx", "di_plus", "di_minus", "atr", "atr_ratio", "rsi", "ema_fast", "ema_slow",
            "ema_ratio", "price_position", "volume_ratio", "volume_trend",
            "entry_confidence", "risk_reward_ratio", "stop_distance",
            "signal_strength", "signal_type_trend", "signal_type_fx", "signal_type_mixed",
            "group_confirmed", "strong_trend", "range_regime",
            "hour_of_day", "day_of_week", "is_morning", "is_afternoon"
    );

    public static void main(String[] args) throws IOException {
        Map<String, String> options = parseArgs(args);
        String dataPath = options.getOrDefault("data", "ml_strategy/data_pipeline/trades.csv");
        String outputPath = options.getOrDefault("output", "ml_strategy/models/trade_classifier_v2.txt");
        String reportDir = options.getOrDefault("report-dir", "ml_strategy");
        String ticker = options.get("ticker");

        System.out.println("============================================================");
        System.out.println("GOLDENDRAGON ML MODEL TRAINING (JAVA)");
        System.out.println("============================================================");
        System.out.println("Random seed: " + RANDOM_SEED);
        System.out.println("Ticker: " + (ticker != null ? ticker : "ALL (global model)"));

        TrainingArtifacts artifacts = train(dataPath, outputPath, reportDir, ticker);

        System.out.println("Filtered trades: " + artifacts.totalSamples);
        System.out.println("Features: " + FEATURE_COLUMNS.size());
        System.out.println("Train size: " + artifacts.split.train.size() + ", Validation size: " + artifacts.split.validation.size() + ", Test size: " + artifacts.split.test.size());
        System.out.println("Validation accuracy: " + formatDouble(artifacts.validationMetrics.accuracy));
        System.out.println("Test accuracy: " + formatDouble(artifacts.testMetrics.accuracy));
        System.out.println("Test AUC: " + formatDouble(artifacts.testMetrics.auc));
        System.out.println("CV AUC mean: " + formatDouble(artifacts.cvMetrics.mean));
        System.out.println("Recommended threshold: " + formatDouble(artifacts.recommendedThreshold));

        System.out.println("Model exported to: " + outputPath);
        System.out.println("Training report written to: " + new File(reportDir, "reports/training_report_" + (ticker != null ? ticker : "all") + ".json").getPath());
    }

    public static TrainingArtifacts train(String dataPath, String outputPath, String reportDir) throws IOException {
        return train(dataPath, outputPath, reportDir, null);
    }
    
    /**
     * Train model for specific ticker.
     * @param dataPath path to trades.csv
     * @param outputPath path to output model file
     * @param reportDir path to reports directory
     * @param ticker ticker symbol to filter by (null for all tickers)
     * @return training artifacts
     * @throws IOException if training fails
     */
    public static TrainingArtifacts train(String dataPath, String outputPath, String reportDir, String ticker) throws IOException {
        List<TradeSample> allSamples = loadSamples(dataPath);
        List<TradeSample> samples = ticker != null ? filterByTicker(allSamples, ticker) : allSamples;
        
        System.out.println("Filtered trades for ticker " + (ticker != null ? ticker : "ALL") + ": " + samples.size() + 
                " (from " + allSamples.size() + " total)");
        
        if (samples.size() < MIN_SAMPLES) {
            throw new IOException("Not enough data for training " + (ticker != null ? ticker : "model") + 
                    ". Need at least " + MIN_SAMPLES + " trades, got " + samples.size());
        }

        DatasetSplit split = splitChronologically(samples);
        if (split.train.isEmpty() || split.validation.isEmpty() || split.test.isEmpty()) {
            throw new IOException("Train/validation/test split produced empty dataset parts.");
        }

        FeatureStats stats = calculateFeatureStats(split.train);
        LogisticModel model = trainModel(split.train, split.validation, stats);

        List<ScoredSample> validationScores = scoreSamples(split.validation, stats, model);
        List<ScoredSample> testScores = scoreSamples(split.test, stats, model);
        ThresholdSelection bestThreshold = selectBestThreshold(validationScores);

        Metrics validationMetrics = evaluate(validationScores, bestThreshold.threshold);
        Metrics testMetrics = evaluate(testScores, bestThreshold.threshold);
        CrossValidationMetrics cvMetrics = timeSeriesCrossValidation(concat(split.train, split.validation));

        File reportRoot = new File(reportDir);
        File dataPipelineDir = new File(reportRoot, "data_pipeline");
        File reportsDir = new File(reportRoot, "reports");
        ensureDirectory(dataPipelineDir);
        ensureDirectory(reportsDir);
        ensureDirectory(new File(outputPath).getParentFile());

        if (ticker == null) {
            writeDatasetCsv(new File(dataPipelineDir, "train.csv"), split.train);
            writeDatasetCsv(new File(dataPipelineDir, "validation.csv"), split.validation);
            writeDatasetCsv(new File(dataPipelineDir, "test.csv"), split.test);
        }
        writeModel(outputPath, stats, model);
        writeTrainingReport(new File(reportsDir, "training_report_" + (ticker != null ? ticker : "all") + ".json"), 
                split, testMetrics, validationMetrics, cvMetrics, bestThreshold);

        return new TrainingArtifacts(samples.size(), split, validationMetrics, testMetrics, cvMetrics, bestThreshold.threshold);
    }
    
    private static List<TradeSample> filterByTicker(List<TradeSample> samples, String ticker) {
        List<TradeSample> filtered = new ArrayList<>();
        for (TradeSample sample : samples) {
            if (ticker.equals(sample.ticker)) {
                filtered.add(sample);
            }
        }
        System.out.println("Filtered " + filtered.size() + " trades for ticker " + ticker + 
                " from total " + samples.size());
        return filtered;
    }

    public static final class TrainingArtifacts {

        public final int totalSamples;
        public final DatasetSplit split;
        public final Metrics validationMetrics;
        public final Metrics testMetrics;
        public final CrossValidationMetrics cvMetrics;
        public final double recommendedThreshold;

        public TrainingArtifacts(int totalSamples,
                                 DatasetSplit split,
                                 Metrics validationMetrics,
                                 Metrics testMetrics,
                                 CrossValidationMetrics cvMetrics,
                                 double recommendedThreshold) {
            this.totalSamples = totalSamples;
            this.split = split;
            this.validationMetrics = validationMetrics;
            this.testMetrics = testMetrics;
            this.cvMetrics = cvMetrics;
            this.recommendedThreshold = recommendedThreshold;
        }
    }

    private static Map<String, String> parseArgs(String[] args) {
        Map<String, String> options = new HashMap<>();
        for (int i = 0; i < args.length; i++) {
            String arg = args[i];
            if (!arg.startsWith("--") || i + 1 >= args.length) {
                continue;
            }
            options.put(arg.substring(2), args[++i]);
        }
        return options;
    }

    private static List<TradeSample> loadSamples(String path) throws IOException {
        File file = new File(path);
        if (!file.exists()) {
            throw new IOException("Training data not found: " + path);
        }

        System.out.println("Loading samples from: " + path + " (size: " + file.length() + " bytes)");
        List<TradeSample> samples = new ArrayList<>();
        try (BufferedReader reader = new BufferedReader(new FileReader(file))) {
            String headerLine = reader.readLine();
            if (headerLine == null || headerLine.trim().isEmpty()) {
                return Collections.emptyList();
            }

            String[] headers = headerLine.split(",");
            Map<String, Integer> indexes = new HashMap<>();
            for (int i = 0; i < headers.length; i++) {
                indexes.put(headers[i].trim(), i);
            }

            String line;
            int lineNumber = 0;
            while ((line = reader.readLine()) != null) {
                lineNumber++;
                if (line.trim().isEmpty()) {
                    continue;
                }

                String[] parts = line.split(",", -1);
                TradeSample sample = parseSample(parts, indexes);
                if (sample != null) {
                    samples.add(sample);
                }
                
                if (lineNumber % 10000 == 0) {
                    System.out.println("Read " + lineNumber + " lines, samples: " + samples.size());
                }
            }
        }

        System.out.println("Total samples loaded: " + samples.size());
        samples.sort((a, b) -> a.timestamp.compareTo(b.timestamp));
        return samples;
    }

    private static TradeSample parseSample(String[] parts, Map<String, Integer> indexes) {
        try {
            Double label = readDouble(parts, indexes, "outcome");
            String winnerText = readString(parts, indexes, "is_winner");
            Integer winner = winnerText == null || winnerText.isEmpty()
                    ? (label != null && label > 0.0 ? 1 : 0)
                    : (Boolean.parseBoolean(winnerText) ? 1 : 0);
            if (label == null && winner == null) {
                return null;
            }

            double[] features = new double[FEATURE_COLUMNS.size()];
            for (int i = 0; i < FEATURE_COLUMNS.size(); i++) {
                Double value = readDouble(parts, indexes, FEATURE_COLUMNS.get(i));
                features[i] = value != null ? value : 0.0;
            }

            String timestampText = readString(parts, indexes, "timestamp");
            if (timestampText == null || timestampText.isEmpty()) {
                return null;
            }

            // Skip section headers (e.g., "2023-07", "2024-01")
            if (timestampText.length() < 10 || !timestampText.contains("T")) {
                return null;
            }

            LocalDateTime timestamp;
            try {
                // Handle timestamps with timezone offset and microseconds
                // e.g., 2021-12-09T10:51:56.342+03:00 or 2024-01-08T19:03:43.315266+03:00
                String ts = timestampText;
                // Only remove timezone if it's after the time (after 'T')
                int tIndex = ts.indexOf('T');
                if (tIndex > 0) {
                    String timePart = ts.substring(tIndex + 1);
                    if (timePart.contains("+") || timePart.contains("-")) {
                        int tzIndex = Math.max(timePart.lastIndexOf('+'), timePart.lastIndexOf('-'));
                        if (tzIndex > 0 && Character.isDigit(timePart.charAt(tzIndex - 1))) {
                            ts = ts.substring(0, tIndex + 1 + tzIndex);
                        }
                    }
                }
                // Remove microseconds if present (keep only milliseconds)
                int dotIndex = ts.indexOf('.');
                if (dotIndex > 0) {
                    // Keep only 3 digits after dot (milliseconds)
                    if (ts.length() > dotIndex + 4) {
                        ts = ts.substring(0, dotIndex + 4);
                    }
                }
                timestamp = LocalDateTime.parse(ts);
            } catch (Exception e) {
                return null;
            }
            
            return new TradeSample(
                    timestamp,
                    readString(parts, indexes, "ticker"),
                    readString(parts, indexes, "strategy"),
                    features,
                    label != null ? label : 0.0,
                    winner
            );
        } catch (DateTimeParseException ignored) {
            return null;
        }
    }

    private static DatasetSplit splitChronologically(List<TradeSample> samples) {
        int total = samples.size();
        int trainEnd = (int) (total * 0.70);
        int validationEnd = (int) (total * 0.85);
        return new DatasetSplit(
                new ArrayList<>(samples.subList(0, trainEnd)),
                new ArrayList<>(samples.subList(trainEnd, validationEnd)),
                new ArrayList<>(samples.subList(validationEnd, total))
        );
    }

    private static FeatureStats calculateFeatureStats(List<TradeSample> samples) {
        int featuresCount = FEATURE_COLUMNS.size();
        double[] means = new double[featuresCount];
        double[] stds = new double[featuresCount];

        for (TradeSample sample : samples) {
            for (int i = 0; i < featuresCount; i++) {
                means[i] += sample.features[i];
            }
        }
        for (int i = 0; i < featuresCount; i++) {
            means[i] /= samples.size();
        }

        for (TradeSample sample : samples) {
            for (int i = 0; i < featuresCount; i++) {
                double diff = sample.features[i] - means[i];
                stds[i] += diff * diff;
            }
        }
        for (int i = 0; i < featuresCount; i++) {
            stds[i] = Math.sqrt(stds[i] / Math.max(1, samples.size() - 1));
            if (stds[i] == 0.0 || Double.isNaN(stds[i])) {
                stds[i] = 1.0;
            }
        }

        return new FeatureStats(means, stds);
    }

    private static LogisticModel trainModel(List<TradeSample> samples,
                                            List<TradeSample> validationSamples,
                                            FeatureStats stats) {
        int featuresCount = FEATURE_COLUMNS.size();
        double[] weights = new double[featuresCount];
        double[] bestWeights = new double[featuresCount];
        double intercept = 0.0;
        double bestIntercept = 0.0;
        double bestValidationLoss = Double.POSITIVE_INFINITY;
        int epochsWithoutImprovement = 0;

        double positives = samples.stream().filter(sample -> sample.isWinner == 1).count();
        double negatives = samples.size() - positives;
        double positiveWeight = positives > 0 ? samples.size() / (2.0 * positives) : 1.0;
        double negativeWeight = negatives > 0 ? samples.size() / (2.0 * negatives) : 1.0;

        for (int epoch = 0; epoch < DEFAULT_EPOCHS; epoch++) {
            double[] gradient = new double[featuresCount];
            double interceptGradient = 0.0;

            for (TradeSample sample : samples) {
                double[] normalized = normalize(sample.features, stats);
                double prediction = sigmoid(score(normalized, weights, intercept));
                double error = prediction - sample.isWinner;
                double sampleWeight = sample.isWinner == 1 ? positiveWeight : negativeWeight;

                for (int i = 0; i < featuresCount; i++) {
                    gradient[i] += error * normalized[i] * sampleWeight;
                }
                interceptGradient += error * sampleWeight;
            }

            double scale = 1.0 / samples.size();
            for (int i = 0; i < featuresCount; i++) {
                double regularized = gradient[i] * scale + DEFAULT_L2 * weights[i];
                weights[i] -= DEFAULT_LEARNING_RATE * regularized;
            }
            intercept -= DEFAULT_LEARNING_RATE * interceptGradient * scale;

            double validationLoss = calculateLogLoss(validationSamples, stats, weights, intercept);
            if (validationLoss + 1e-9 < bestValidationLoss) {
                bestValidationLoss = validationLoss;
                bestIntercept = intercept;
                System.arraycopy(weights, 0, bestWeights, 0, weights.length);
                epochsWithoutImprovement = 0;
            } else {
                epochsWithoutImprovement++;
                if (epochsWithoutImprovement >= EARLY_STOPPING_PATIENCE) {
                    break;
                }
            }
        }

        return new LogisticModel(bestWeights, bestIntercept);
    }

    private static List<ScoredSample> scoreSamples(List<TradeSample> samples, FeatureStats stats, LogisticModel model) {
        List<ScoredSample> scored = new ArrayList<>(samples.size());
        for (TradeSample sample : samples) {
            double probability = sigmoid(score(normalize(sample.features, stats), model.weights, model.intercept));
            scored.add(new ScoredSample(sample, probability));
        }
        return scored;
    }

    private static Metrics evaluate(List<ScoredSample> scoredSamples, double threshold) {
        if (scoredSamples.isEmpty()) {
            return new Metrics();
        }

        int correct = 0;
        int tp = 0;
        int fp = 0;
        int fn = 0;
        int predictedPositive = 0;
        List<Double> positives = new ArrayList<>();
        List<Double> negatives = new ArrayList<>();

        for (ScoredSample scored : scoredSamples) {
            int prediction = scored.probability >= threshold ? 1 : 0;
            if (prediction == scored.sample.isWinner) {
                correct++;
            }
            if (prediction == 1) {
                predictedPositive++;
            }
            if (prediction == 1 && scored.sample.isWinner == 1) {
                tp++;
            } else if (prediction == 1) {
                fp++;
            } else if (scored.sample.isWinner == 1) {
                fn++;
            }

            if (scored.sample.isWinner == 1) {
                positives.add(scored.probability);
            } else {
                negatives.add(scored.probability);
            }
        }

        Metrics metrics = new Metrics();
        metrics.accuracy = (double) correct / scoredSamples.size();
        metrics.precision = tp + fp > 0 ? (double) tp / (tp + fp) : 0.0;
        metrics.recall = tp + fn > 0 ? (double) tp / (tp + fn) : 0.0;
        metrics.winRate = (double) predictedPositive / scoredSamples.size();
        metrics.auc = calculateAuc(positives, negatives);
        metrics.trading = calculateTradingMetrics(scoredSamples, threshold);
        return metrics;
    }

    private static TradingMetrics calculateTradingMetrics(List<ScoredSample> scoredSamples, double threshold) {
        TradingMetrics metrics = new TradingMetrics();
        List<ScoredSample> chosen = new ArrayList<>();
        for (ScoredSample scored : scoredSamples) {
            if (scored.probability >= threshold) {
                chosen.add(scored);
            }
        }
        if (chosen.isEmpty()) {
            return metrics;
        }

        double totalPnl = 0.0;
        double grossProfit = 0.0;
        double grossLoss = 0.0;
        double equity = 0.0;
        double peak = 0.0;
        int wins = 0;
        Map<YearMonth, Double> monthlyPnl = new LinkedHashMap<>();
        List<Double> pnlSeries = new ArrayList<>();

        for (ScoredSample scored : chosen) {
            double pnl = scored.sample.outcome;
            pnlSeries.add(pnl);
            totalPnl += pnl;
            equity += pnl;
            if (equity > peak) {
                peak = equity;
            }
            double drawdown = peak - equity;
            if (drawdown > metrics.maxDrawdown) {
                metrics.maxDrawdown = drawdown;
            }
            if (pnl > 0.0) {
                wins++;
                grossProfit += pnl;
            } else if (pnl < 0.0) {
                grossLoss += Math.abs(pnl);
            }

            YearMonth month = YearMonth.from(scored.sample.timestamp);
            monthlyPnl.merge(month, pnl, Double::sum);
        }

        metrics.tradesTaken = chosen.size();
        metrics.avgPnl = totalPnl / chosen.size();
        metrics.totalPnl = totalPnl;
        metrics.winRate = (double) wins / chosen.size();
        metrics.profitFactor = grossLoss > 0.0 ? grossProfit / grossLoss : grossProfit;
        metrics.sharpe = calculateSharpe(pnlSeries);
        for (Map.Entry<YearMonth, Double> entry : monthlyPnl.entrySet()) {
            metrics.monthlyPnl.put(entry.getKey().toString(), entry.getValue());
        }
        return metrics;
    }

    private static CrossValidationMetrics timeSeriesCrossValidation(List<TradeSample> samples) {
        int splits = Math.min(5, Math.max(2, samples.size() / 50));
        List<Double> aucs = new ArrayList<>();
        int foldSize = samples.size() / (splits + 1);
        if (foldSize <= 0) {
            return new CrossValidationMetrics(0.0, 0.0);
        }

        for (int i = 1; i <= splits; i++) {
            int trainEnd = foldSize * i;
            int testEnd = Math.min(samples.size(), trainEnd + foldSize);
            if (trainEnd < MIN_SAMPLES / 2 || testEnd <= trainEnd) {
                continue;
            }

            List<TradeSample> train = new ArrayList<>(samples.subList(0, trainEnd));
            List<TradeSample> test = new ArrayList<>(samples.subList(trainEnd, testEnd));
            FeatureStats stats = calculateFeatureStats(train);
            int validationStart = Math.max(1, (int) (train.size() * 0.85));
            List<TradeSample> effectiveTrain = new ArrayList<>(train.subList(0, validationStart));
            List<TradeSample> validation = new ArrayList<>(train.subList(validationStart, train.size()));
            LogisticModel model = trainModel(effectiveTrain, validation, stats);
            Metrics metrics = evaluate(scoreSamples(test, stats, model), DEFAULT_THRESHOLD);
            aucs.add(metrics.auc);
        }

        if (aucs.isEmpty()) {
            return new CrossValidationMetrics(0.0, 0.0);
        }

        double mean = aucs.stream().mapToDouble(Double::doubleValue).average().orElse(0.0);
        double variance = aucs.stream().mapToDouble(value -> {
            double diff = value - mean;
            return diff * diff;
        }).average().orElse(0.0);
        return new CrossValidationMetrics(mean, Math.sqrt(variance));
    }

    private static void writeModel(String outputPath, FeatureStats stats, LogisticModel model) throws IOException {
        try (PrintWriter writer = new PrintWriter(new FileWriter(outputPath))) {
            writer.println("# GoldenDragon ML Model");
            writer.println("# Model: LogisticRegressionJava");
            writer.println("# Features: " + String.join(", ", FEATURE_COLUMNS));
            writer.println();
            writer.println("[coefficients]");
            for (int i = 0; i < FEATURE_COLUMNS.size(); i++) {
                writer.println(FEATURE_COLUMNS.get(i) + "=" + formatDouble(model.weights[i]));
            }
            writer.println();
            writer.println("[intercept]");
            writer.println(formatDouble(model.intercept));
            writer.println();
            writer.println("[means]");
            for (int i = 0; i < FEATURE_COLUMNS.size(); i++) {
                writer.println(FEATURE_COLUMNS.get(i) + "=" + formatDouble(stats.means[i]));
            }
            writer.println();
            writer.println("[stds]");
            for (int i = 0; i < FEATURE_COLUMNS.size(); i++) {
                writer.println(FEATURE_COLUMNS.get(i) + "=" + formatDouble(stats.stds[i]));
            }
            writer.println();
            writer.println("# Note: Normalize features before prediction");
            writer.println("# Formula: normalized = (value - mean) / std");
        }
    }

    private static void writeTrainingReport(File file,
                                            DatasetSplit split,
                                            Metrics testMetrics,
                                            Metrics validationMetrics,
                                            CrossValidationMetrics cvMetrics,
                                            ThresholdSelection bestThreshold) throws IOException {
        ensureDirectory(file.getParentFile());
        try (PrintWriter writer = new PrintWriter(new FileWriter(file))) {
            writer.println("{");
            writer.println("  \"random_seed\": " + RANDOM_SEED + ",");
            writer.println("  \"model\": \"LogisticRegressionJava\",");
            writer.println("  \"features\": [");
            for (int i = 0; i < FEATURE_COLUMNS.size(); i++) {
                String suffix = i + 1 < FEATURE_COLUMNS.size() ? "," : "";
                writer.println("    \"" + FEATURE_COLUMNS.get(i) + "\"" + suffix);
            }
            writer.println("  ],");
            writer.println("  \"samples\": {");
            writer.println("    \"train\": " + split.train.size() + ",");
            writer.println("    \"validation\": " + split.validation.size() + ",");
            writer.println("    \"test\": " + split.test.size());
            writer.println("  },");
            writer.println("  \"metrics\": {");
            writer.println("    \"recommended_threshold\": " + formatDouble(bestThreshold.threshold) + ",");
            writer.println("    \"validation_score\": " + formatDouble(bestThreshold.score) + ",");
            writer.println("    \"auc\": " + formatDouble(testMetrics.auc) + ",");
            writer.println("    \"cv_auc_mean\": " + formatDouble(cvMetrics.mean) + ",");
            writer.println("    \"cv_auc_std\": " + formatDouble(cvMetrics.std) + ",");
            writer.println("    \"validation_classification\": {");
            writer.println("      \"accuracy\": " + formatDouble(validationMetrics.accuracy) + ",");
            writer.println("      \"precision\": " + formatDouble(validationMetrics.precision) + ",");
            writer.println("      \"recall\": " + formatDouble(validationMetrics.recall) + ",");
            writer.println("      \"win_rate\": " + formatDouble(validationMetrics.winRate));
            writer.println("    },");
            writer.println("    \"classification\": {");
            writer.println("      \"accuracy\": " + formatDouble(testMetrics.accuracy) + ",");
            writer.println("      \"precision\": " + formatDouble(testMetrics.precision) + ",");
            writer.println("      \"recall\": " + formatDouble(testMetrics.recall) + ",");
            writer.println("      \"win_rate\": " + formatDouble(testMetrics.winRate));
            writer.println("    },");
            writer.println("    \"trading\": " + tradingMetricsToJson(testMetrics.trading));
            writer.println("  }");
            writer.println("}");
        }
    }

    private static String tradingMetricsToJson(TradingMetrics metrics) {
        StringBuilder monthly = new StringBuilder();
        monthly.append("{");
        int index = 0;
        for (Map.Entry<String, Double> entry : metrics.monthlyPnl.entrySet()) {
            if (index++ > 0) {
                monthly.append(", ");
            }
            monthly.append("\"").append(entry.getKey()).append("\": ").append(formatDouble(entry.getValue()));
        }
        monthly.append("}");

        return "{" +
                "\"trades_taken\": " + metrics.tradesTaken + ", " +
                "\"avg_pnl\": " + formatDouble(metrics.avgPnl) + ", " +
                "\"total_pnl\": " + formatDouble(metrics.totalPnl) + ", " +
                "\"win_rate\": " + formatDouble(metrics.winRate) + ", " +
                "\"profit_factor\": " + formatDouble(metrics.profitFactor) + ", " +
                "\"max_drawdown\": " + formatDouble(metrics.maxDrawdown) + ", " +
                "\"sharpe\": " + formatDouble(metrics.sharpe) + ", " +
                "\"monthly_pnl\": " + monthly +
                "}";
    }

    private static void writeDatasetCsv(File file, List<TradeSample> samples) throws IOException {
        ensureDirectory(file.getParentFile());
        try (PrintWriter writer = new PrintWriter(new FileWriter(file))) {
            writer.println("timestamp,ticker,strategy," + String.join(",", FEATURE_COLUMNS) + ",outcome,is_winner");
            for (TradeSample sample : samples) {
                StringBuilder row = new StringBuilder();
                row.append(sample.timestamp).append(',')
                        .append(sample.ticker != null ? sample.ticker : "").append(',')
                        .append(sample.strategy != null ? sample.strategy : "");
                for (double feature : sample.features) {
                    row.append(',').append(formatDouble(feature));
                }
                row.append(',').append(formatDouble(sample.outcome)).append(',').append(sample.isWinner == 1 ? "true" : "false");
                writer.println(row);
            }
        }
    }

    private static void ensureDirectory(File dir) throws IOException {
        if (dir != null && !dir.exists() && !dir.mkdirs()) {
            throw new IOException("Failed to create directory: " + dir.getPath());
        }
    }

    private static double[] normalize(double[] values, FeatureStats stats) {
        double[] normalized = Arrays.copyOf(values, values.length);
        for (int i = 0; i < normalized.length; i++) {
            normalized[i] = (normalized[i] - stats.means[i]) / stats.stds[i];
        }
        return normalized;
    }

    private static double score(double[] features, double[] weights, double intercept) {
        double sum = intercept;
        for (int i = 0; i < features.length; i++) {
            sum += features[i] * weights[i];
        }
        return sum;
    }

    private static double sigmoid(double value) {
        if (value >= 0) {
            double exp = Math.exp(-value);
            return 1.0 / (1.0 + exp);
        }
        double exp = Math.exp(value);
        return exp / (1.0 + exp);
    }

    private static double calculateAuc(List<Double> positives, List<Double> negatives) {
        if (positives.isEmpty() || negatives.isEmpty()) {
            return 0.5;
        }

        double wins = 0.0;
        for (double positive : positives) {
            for (double negative : negatives) {
                if (positive > negative) {
                    wins += 1.0;
                } else if (positive == negative) {
                    wins += 0.5;
                }
            }
        }
        return wins / (positives.size() * negatives.size());
    }

    private static double calculateSharpe(List<Double> pnlSeries) {
        if (pnlSeries.size() < 2) {
            return 0.0;
        }

        double mean = pnlSeries.stream().mapToDouble(Double::doubleValue).average().orElse(0.0);
        double variance = pnlSeries.stream().mapToDouble(value -> {
            double diff = value - mean;
            return diff * diff;
        }).average().orElse(0.0);
        double std = Math.sqrt(variance);
        return std > 0.0 ? mean / std : 0.0;
    }

    private static double calculateLogLoss(List<TradeSample> samples,
                                           FeatureStats stats,
                                           double[] weights,
                                           double intercept) {
        if (samples.isEmpty()) {
            return Double.POSITIVE_INFINITY;
        }

        double loss = 0.0;
        for (TradeSample sample : samples) {
            double probability = sigmoid(score(normalize(sample.features, stats), weights, intercept));
            double clipped = Math.max(1e-9, Math.min(1.0 - 1e-9, probability));
            loss += -(sample.isWinner * Math.log(clipped) + (1 - sample.isWinner) * Math.log(1.0 - clipped));
        }
        return loss / samples.size();
    }

    private static ThresholdSelection selectBestThreshold(List<ScoredSample> validationScores) {
        ThresholdSelection best = new ThresholdSelection(DEFAULT_THRESHOLD, Double.NEGATIVE_INFINITY);
        for (double threshold = MIN_THRESHOLD; threshold <= MAX_THRESHOLD + 1e-9; threshold += THRESHOLD_STEP) {
            Metrics metrics = evaluate(validationScores, threshold);
            double score = scoreThreshold(metrics.trading);
            if (score > best.score) {
                best = new ThresholdSelection(roundThreshold(threshold), score);
            }
        }
        return best;
    }

    private static double scoreThreshold(TradingMetrics metrics) {
        if (metrics.tradesTaken < 20) {
            return Double.NEGATIVE_INFINITY;
        }

        return metrics.totalPnl * 0.001
                + metrics.winRate * 20.0
                + Math.min(metrics.profitFactor, 3.0) * 5.0
                + Math.max(metrics.sharpe, 0.0) * 5.0
                - metrics.maxDrawdown * 0.002;
    }

    private static double roundThreshold(double value) {
        return Math.round(value * 100.0) / 100.0;
    }

    private static List<TradeSample> concat(List<TradeSample> first, List<TradeSample> second) {
        List<TradeSample> merged = new ArrayList<>(first.size() + second.size());
        merged.addAll(first);
        merged.addAll(second);
        return merged;
    }

    private static String readString(String[] parts, Map<String, Integer> indexes, String column) {
        Integer index = indexes.get(column);
        if (index == null || index >= parts.length) {
            return null;
        }
        return parts[index].trim();
    }

    private static Double readDouble(String[] parts, Map<String, Integer> indexes, String column) {
        String value = readString(parts, indexes, column);
        if (value == null || value.isEmpty()) {
            return null;
        }
        try {
            return Double.parseDouble(value);
        } catch (NumberFormatException ignored) {
            return null;
        }
    }

    private static String formatDouble(double value) {
        return String.format(Locale.US, "%.6f", value);
    }

    private static class TradeSample {
        final LocalDateTime timestamp;
        final String ticker;
        final String strategy;
        final double[] features;
        final double outcome;
        final int isWinner;

        TradeSample(LocalDateTime timestamp,
                    String ticker,
                    String strategy,
                    double[] features,
                    double outcome,
                    int isWinner) {
            this.timestamp = timestamp;
            this.ticker = ticker;
            this.strategy = strategy;
            this.features = features;
            this.outcome = outcome;
            this.isWinner = isWinner;
        }
    }

    private static class DatasetSplit {
        final List<TradeSample> train;
        final List<TradeSample> validation;
        final List<TradeSample> test;

        DatasetSplit(List<TradeSample> train,
                     List<TradeSample> validation,
                     List<TradeSample> test) {
            this.train = train;
            this.validation = validation;
            this.test = test;
        }
    }

    private static class FeatureStats {
        final double[] means;
        final double[] stds;

        FeatureStats(double[] means, double[] stds) {
            this.means = means;
            this.stds = stds;
        }
    }

    private static class LogisticModel {
        final double[] weights;
        final double intercept;

        LogisticModel(double[] weights, double intercept) {
            this.weights = weights;
            this.intercept = intercept;
        }
    }

    private static class ScoredSample {
        final TradeSample sample;
        final double probability;

        ScoredSample(TradeSample sample, double probability) {
            this.sample = sample;
            this.probability = probability;
        }
    }

    private static class Metrics {
        double accuracy;
        double precision;
        double recall;
        double winRate;
        double auc = 0.5;
        TradingMetrics trading = new TradingMetrics();
    }

    private static class TradingMetrics {
        int tradesTaken;
        double avgPnl;
        double totalPnl;
        double winRate;
        double profitFactor;
        double maxDrawdown;
        double sharpe;
        final Map<String, Double> monthlyPnl = new LinkedHashMap<>();
    }

    private static class CrossValidationMetrics {
        final double mean;
        final double std;

        CrossValidationMetrics(double mean, double std) {
            this.mean = mean;
            this.std = std;
        }
    }

    private static class ThresholdSelection {
        final double threshold;
        final double score;

        ThresholdSelection(double threshold, double score) {
            this.threshold = threshold;
            this.score = score;
        }
    }
}
