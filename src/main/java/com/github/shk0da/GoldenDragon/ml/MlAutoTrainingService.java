package com.github.shk0da.GoldenDragon.ml;

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.time.Duration;
import java.time.Instant;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicBoolean;

public class MlAutoTrainingService {

  private static final int MIN_TRADES_TO_RETRAIN = 50;
  private static final int MIN_TRADES_TO_RETRAIN_GLOBAL = 500;
  private static final Duration DEFAULT_RETRAIN_INTERVAL = Duration.ofHours(6);
  private static final DateTimeFormatter INSTANT_FORMATTER =
      DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm:ss.SSS").withZone(ZoneId.systemDefault());

  private final Path tradesPath;
  private final Path modelPath;
  private final Path reportDir;
  private final Duration retrainInterval;
  private final AtomicBoolean trainingInProgress = new AtomicBoolean(false);
  private final boolean perTickerTraining;

  private volatile Instant lastSuccessfulTraining = Instant.EPOCH;
  private volatile long lastObservedTrades = 0;
  private final Map<String, Instant> lastTrainingByTicker = new ConcurrentHashMap<>();
  private final Map<String, Long> lastTradeCountByTicker = new ConcurrentHashMap<>();

  private static class PrimitiveLong {
    final long value;

    PrimitiveLong(long value) {
      this.value = value;
    }

    boolean equals(long other) {
      return this.value == other;
    }
  }

  public MlAutoTrainingService(String tradesPath, String modelPath, String reportDir) {
    this(
        Path.of(tradesPath),
        Path.of(modelPath),
        Path.of(reportDir),
        DEFAULT_RETRAIN_INTERVAL,
        false);
  }

  public MlAutoTrainingService(
      String tradesPath, String modelPath, String reportDir, boolean perTickerTraining) {
    this(
        Path.of(tradesPath),
        Path.of(modelPath),
        Path.of(reportDir),
        DEFAULT_RETRAIN_INTERVAL,
        perTickerTraining);
  }

  public MlAutoTrainingService(
      Path tradesPath, Path modelPath, Path reportDir, Duration retrainInterval) {
    this(tradesPath, modelPath, reportDir, retrainInterval, false);
  }

  public MlAutoTrainingService(
      Path tradesPath,
      Path modelPath,
      Path reportDir,
      Duration retrainInterval,
      boolean perTickerTraining) {
    this.tradesPath = tradesPath;
    this.modelPath = modelPath;
    this.reportDir = reportDir;
    this.retrainInterval = retrainInterval;
    this.perTickerTraining = perTickerTraining;
  }

  public void tryRetrain(MlPredictionService predictionService) {
    if (predictionService == null || trainingInProgress.get()) {
      return;
    }

    if (perTickerTraining) {
      tryRetrainPerTicker(predictionService);
    } else {
      tryRetrainGlobal(predictionService);
    }
  }

  private void tryRetrainGlobal(MlPredictionService predictionService) {
    long tradeCount = countTrades();
    if (tradeCount < MIN_TRADES_TO_RETRAIN_GLOBAL) {
      return;
    }

    Instant now = Instant.now();
    if (tradeCount == lastObservedTrades
        && now.isBefore(lastSuccessfulTraining.plus(retrainInterval))) {
      return;
    }

    if (!trainingInProgress.compareAndSet(false, true)) {
      return;
    }

    try {
      Path temporaryModel = modelPath.resolveSibling(modelPath.getFileName() + ".tmp");
      log(
          "Auto retraining (global) started at "
              + formatInstant(now)
              + ". trades="
              + tradeCount
              + ", lastTraining="
              + formatInstant(lastSuccessfulTraining));
      MlModelTrainer.TrainingArtifacts artifacts =
          MlModelTrainer.train(
              tradesPath.toString(), temporaryModel.toString(), reportDir.toString());
      Files.move(
          temporaryModel,
          modelPath,
          StandardCopyOption.REPLACE_EXISTING,
          StandardCopyOption.ATOMIC_MOVE);
      predictionService.reloadModel(modelPath.toString());
      predictionService.setProbabilityThreshold(artifacts.recommendedThreshold);
      lastObservedTrades = tradeCount;
      lastSuccessfulTraining = now;
      log(
          "Auto retraining (global) finished at "
              + formatInstant(lastSuccessfulTraining)
              + ". threshold="
              + artifacts.recommendedThreshold
              + ", nextAllowed="
              + formatInstant(lastSuccessfulTraining.plus(retrainInterval)));
    } catch (IOException ex) {
      logError("Auto retraining (global) failed at " + formatInstant(now) + ": " + ex.getMessage());
    } finally {
      trainingInProgress.set(false);
    }
  }

  private void tryRetrainPerTicker(MlPredictionService predictionService) {
    Instant now = Instant.now();
    Map<String, Long> tickerTradeCounts = countTradesByTicker();

    if (!trainingInProgress.compareAndSet(false, true)) {
      return;
    }

    try {
      int trainedCount = 0;
      for (Map.Entry<String, Long> entry : tickerTradeCounts.entrySet()) {
        String ticker = entry.getKey();
        long tradeCount = entry.getValue();

        if (tradeCount < MIN_TRADES_TO_RETRAIN) {
          continue;
        }

        Long lastCountObj = lastTradeCountByTicker.get(ticker);
        Instant lastTraining = lastTrainingByTicker.get(ticker);

        if ((lastCountObj != null && tradeCount == lastCountObj.longValue())
            && lastTraining != null
            && now.isBefore(lastTraining.plus(retrainInterval))) {
          continue;
        }

        Path temporaryModel = modelPath.resolveSibling("trade_classifier_" + ticker + ".tmp");
        Path finalModel = modelPath.resolveSibling("trade_classifier_" + ticker + ".txt");

        log(
            "Auto retraining for ticker "
                + ticker
                + " started at "
                + formatInstant(now)
                + ". trades="
                + tradeCount);

        MlModelTrainer.TrainingArtifacts artifacts =
            MlModelTrainer.train(
                tradesPath.toString(), temporaryModel.toString(), reportDir.toString(), ticker);

        Files.move(
            temporaryModel,
            finalModel,
            StandardCopyOption.REPLACE_EXISTING,
            StandardCopyOption.ATOMIC_MOVE);
        predictionService.loadModelForTicker(ticker, finalModel.toString());

        lastTradeCountByTicker.put(ticker, tradeCount);
        lastTrainingByTicker.put(ticker, now);
        trainedCount++;

        log(
            "Auto retraining for ticker "
                + ticker
                + " finished. threshold="
                + artifacts.recommendedThreshold);
      }

      if (trainedCount > 0) {
        lastSuccessfulTraining = now;
        log("Auto retraining (per-ticker) completed: " + trainedCount + " models updated");
      }
    } catch (IOException ex) {
      logError(
          "Auto retraining (per-ticker) failed at " + formatInstant(now) + ": " + ex.getMessage());
      ex.printStackTrace();
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

  private Map<String, Long> countTradesByTicker() {
    Map<String, Long> tickerCounts = new HashMap<>();
    if (!Files.exists(tradesPath)) {
      return tickerCounts;
    }

    try (BufferedReader reader = Files.newBufferedReader(tradesPath)) {
      String headerLine = reader.readLine();
      if (headerLine == null) {
        return tickerCounts;
      }

      String[] headers = headerLine.split(",");
      int tickerIndex = -1;
      for (int i = 0; i < headers.length; i++) {
        if ("ticker".equals(headers[i].trim())) {
          tickerIndex = i;
          break;
        }
      }

      if (tickerIndex < 0) {
        return tickerCounts;
      }

      String line;
      while ((line = reader.readLine()) != null) {
        if (line.trim().isEmpty()) {
          continue;
        }
        String[] parts = line.split(",");
        if (parts.length > tickerIndex) {
          String ticker = parts[tickerIndex].trim();
          if (!ticker.isEmpty()) {
            tickerCounts.merge(ticker, 1L, Long::sum);
          }
        }
      }
    } catch (IOException ex) {
      logError("Failed to count trades by ticker: " + ex.getMessage());
    }

    return tickerCounts;
  }
}
