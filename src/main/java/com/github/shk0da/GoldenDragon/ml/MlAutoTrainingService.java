package com.github.shk0da.GoldenDragon.ml;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.time.Duration;
import java.time.Instant;
import java.util.concurrent.atomic.AtomicBoolean;

public class MlAutoTrainingService {

    private static final int MIN_TRADES_TO_RETRAIN = 500;
    private static final Duration DEFAULT_RETRAIN_INTERVAL = Duration.ofHours(6);

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
            System.out.println("Auto retraining started. trades=" + tradeCount);
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
            System.out.println("Auto retraining finished. threshold=" + artifacts.recommendedThreshold);
        } catch (IOException ex) {
            System.err.println("Auto retraining failed: " + ex.getMessage());
        } finally {
            trainingInProgress.set(false);
        }
    }

    private long countTrades() {
        if (!Files.exists(tradesPath)) {
            return 0;
        }

        try {
            return Math.max(0, Files.lines(tradesPath).count() - 1);
        } catch (IOException ex) {
            return 0;
        }
    }
}
