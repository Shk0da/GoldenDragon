package com.github.shk0da.GoldenDragon.ml;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.time.Duration;
import java.time.Instant;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.Date;
import java.util.concurrent.atomic.AtomicBoolean;

public class MlAutoTrainingService {

    private static final int MIN_TRADES_TO_RETRAIN = 500;
    private static final Duration DEFAULT_RETRAIN_INTERVAL = Duration.ofHours(6);
    private static final DateTimeFormatter INSTANT_FORMATTER = DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm:ss.SSS")
            .withZone(ZoneId.systemDefault());

    private final Path tradesPath;
    private final Path modelPath;
    private final Path reportDir;
    private final Duration retrainInterval;
    private final AtomicBoolean trainingInProgress = new AtomicBoolean(false);

    private volatile Instant lastSuccessfulTraining = Instant.EPOCH;
    private volatile long lastObservedTrades = 0;

    public MlAutoTrainingService(String tradesPath, String modelPath, String reportDir) {
        this(Path.of(tradesPath), Path.of(modelPath), Path.of(reportDir), DEFAULT_RETRAIN_INTERVAL);
    }

    public MlAutoTrainingService(Path tradesPath,
                                 Path modelPath,
                                 Path reportDir,
                                 Duration retrainInterval) {
        this.tradesPath = tradesPath;
        this.modelPath = modelPath;
        this.reportDir = reportDir;
        this.retrainInterval = retrainInterval;
    }

    public void tryRetrain(MlPredictionService predictionService) {
        if (predictionService == null || trainingInProgress.get()) {
            return;
        }

        long tradeCount = countTrades();
        if (tradeCount < MIN_TRADES_TO_RETRAIN) {
            return;
        }

        Instant now = Instant.now();
        if (tradeCount == lastObservedTrades && now.isBefore(lastSuccessfulTraining.plus(retrainInterval))) {
            return;
        }

        if (!trainingInProgress.compareAndSet(false, true)) {
            return;
        }

        try {
            Path temporaryModel = modelPath.resolveSibling(modelPath.getFileName() + ".tmp");
            log("Auto retraining started at " + formatInstant(now)
                    + ". trades=" + tradeCount
                    + ", lastTraining=" + formatInstant(lastSuccessfulTraining));
            MlModelTrainer.TrainingArtifacts artifacts = MlModelTrainer.train(
                    tradesPath.toString(),
                    temporaryModel.toString(),
                    reportDir.toString()
            );
            Files.move(temporaryModel, modelPath, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE);
            predictionService.reloadModel(modelPath.toString());
            predictionService.setProbabilityThreshold(artifacts.recommendedThreshold);
            lastObservedTrades = tradeCount;
            lastSuccessfulTraining = now;
            log("Auto retraining finished at " + formatInstant(lastSuccessfulTraining)
                    + ". threshold=" + artifacts.recommendedThreshold
                    + ", nextAllowed=" + formatInstant(lastSuccessfulTraining.plus(retrainInterval)));
        } catch (IOException ex) {
            logError("Auto retraining failed at " + formatInstant(now) + ": " + ex.getMessage());
        } finally {
            trainingInProgress.set(false);
        }
    }

    private String formatInstant(Instant instant) {
        if (instant == null || Instant.EPOCH.equals(instant)) {
            return "n/a";
        }
        return INSTANT_FORMATTER.format(instant);
    }

    private void log(String message) {
        System.out.println("[" + INSTANT_FORMATTER.format(new Date().toInstant()) + "] " + message);
    }

    private void logError(String message) {
        System.err.println("[" + INSTANT_FORMATTER.format(new Date().toInstant()) + "] " + message);
    }

    private long countTrades() {
        if (!Files.exists(tradesPath)) {
            return 0;
        }

        try (var lines = Files.lines(tradesPath)) {
            return Math.max(0, lines.count() - 1);
        } catch (IOException ex) {
            return 0;
        }
    }
}
