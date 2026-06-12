package com.github.shk0da.goldendragon.ml;

import com.github.shk0da.goldendragon.model.Candle;
import com.github.shk0da.goldendragon.model.Position;
import com.github.shk0da.goldendragon.model.TradingDecision;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/** ML Trade Data Collector. Collects trade features and outcomes for training. */
public class TradeDataCollector {

  private static final ThreadLocal<SimpleDateFormat> CANDLE_TIME_FORMAT =
      ThreadLocal.withInitial(() -> new SimpleDateFormat("dd.MM.yyyy HH:mm:ss"));

  private static final String CSV_HEADER =
      String.join(
          ",",
          "timestamp",
          "ticker",
          "strategy",
          "adx",
          "di_plus",
          "di_minus",
          "atr",
          "atr_ratio",
          "rsi",
          "ema_fast",
          "ema_slow",
          "ema_ratio",
          "price_position",
          "volume_ratio",
          "volume_trend",
          "entry_confidence",
          "risk_reward_ratio",
          "stop_distance",
          "signal_strength",
          "signal_type_trend",
          "signal_type_fx",
          "signal_type_mixed",
          "group_confirmed",
          "strong_trend",
          "range_regime",
          "hour_of_day",
          "day_of_week",
          "is_morning",
          "is_afternoon",
          "entry_price",
          "stop_loss",
          "take_profit",
          "quantity",
          "outcome",
          "is_winner");

  private final List<TradeFeatures> tradeHistory = new ArrayList<>();
  private final Map<String, TradeFeatures> openTrades = new HashMap<>();
  private final String dataFile;

  public TradeDataCollector(String dataFile) {
    this.dataFile = dataFile;
    loadExistingData();
  }

  /** Record trade entry with features. */
  public synchronized void recordTradeEntry(
      String ticker,
      String strategy,
      List<Candle> candles,
      double entryPrice,
      double stopLoss,
      double takeProfit,
      double entryConfidence,
      String breakoutType) {

    if (candles == null || candles.size() < 30) {
      return; // Not enough data
    }

    // Calculate features
    double adx = calculateAdx(candles, 14);
    double[] diValues = calculateDI(candles, 14);
    double atr = calculateAtr(candles, 14);
    double atrAvg = calculateAverageAtr(candles, 50);
    double rsi = calculateRsi(candles, 14);
    double emaFast = calculateEma(candles, 9);
    double emaSlow = calculateEma(candles, 21);
    double pricePosition = calculatePricePosition(candles);
    double volumeRatio = calculateVolumeRatio(candles, 20);

    double riskRewardRatio =
        Math.abs(takeProfit - entryPrice) / Math.max(Math.abs(entryPrice - stopLoss), 0.0001);
    double stopDistance = Math.abs(entryPrice - stopLoss) / entryPrice * 100.0;
    String normalizedSignal = extractSignalToken(breakoutType);
    double signalStrength = parseSignalStrength(normalizedSignal);
    double signalTypeTrend = normalizedSignal.startsWith("TB") ? 1.0 : 0.0;
    double signalTypeFx = normalizedSignal.startsWith("FX") ? 1.0 : 0.0;
    double signalTypeMixed = normalizedSignal.startsWith("MX") ? 1.0 : 0.0;
    double groupConfirmed =
        breakoutType != null && breakoutType.contains("noGroupConf") ? 0.0 : 1.0;
    double strongTrend = adx >= 30.0 ? 1.0 : 0.0;
    double rangeRegime = adx > 0.0 && adx <= 15.0 ? 1.0 : 0.0;

    LocalDateTime now = LocalDateTime.now();

    TradeFeatures features =
        new TradeFeatures(
            adx,
            diValues[0],
            diValues[1],
            atr,
            atr / atrAvg,
            rsi,
            emaFast,
            emaSlow,
            emaFast / emaSlow,
            pricePosition,
            volumeRatio,
            0.0, // Volume trend calculated separately
            entryConfidence,
            riskRewardRatio,
            stopDistance,
            breakoutType,
            signalStrength,
            signalTypeTrend,
            signalTypeFx,
            signalTypeMixed,
            groupConfirmed,
            strongTrend,
            rangeRegime,
            now.getHour(),
            now.getDayOfWeek().getValue(),
            ticker,
            strategy,
            now,
            entryPrice);

    tradeHistory.add(features);
    openTrades.put(buildTradeKey(ticker, strategy, features.entryTime), features);
  }

  public synchronized void recordTradeEntry(
      String ticker, String strategy, List<Candle> candles, TradingDecision decision) {
    recordTradeEntry(ticker, strategy, candles, decision, resolveEntryTime(candles));
  }

  public synchronized void recordTradeEntry(
      String ticker,
      String strategy,
      List<Candle> candles,
      TradingDecision decision,
      LocalDateTime entryTime) {
    if (decision == null || decision.updatedPosition == null) {
      return;
    }

    Position position = decision.updatedPosition;
    if (position.entryPrice == null || position.stopLoss == null || position.takeProfit == null) {
      return;
    }

    recordTradeEntry(
        ticker,
        strategy,
        candles,
        position.entryPrice,
        position.stopLoss,
        position.takeProfit,
        decision.confidence,
        decision.reason,
        entryTime);
  }

  public synchronized void recordTradeEntry(
      String ticker,
      String strategy,
      List<Candle> candles,
      double entryPrice,
      double stopLoss,
      double takeProfit,
      double entryConfidence,
      String breakoutType,
      LocalDateTime entryTime) {

    if (candles == null || candles.size() < 30) {
      return;
    }

    double adx = calculateAdx(candles, 14);
    double[] diValues = calculateDI(candles, 14);
    double atr = calculateAtr(candles, 14);
    double atrAvg = calculateAverageAtr(candles, 50);
    double rsi = calculateRsi(candles, 14);
    double emaFast = calculateEma(candles, 9);
    double emaSlow = calculateEma(candles, 21);
    double pricePosition = calculatePricePositionForEntry(candles, entryPrice);
    double volumeRatio = calculateVolumeRatio(candles, 20);
    double volumeTrend = calculateVolumeTrend(candles, 5);

    double riskRewardRatio =
        Math.abs(takeProfit - entryPrice) / Math.max(Math.abs(entryPrice - stopLoss), 0.0001);
    double stopDistance = Math.abs(entryPrice - stopLoss) / entryPrice * 100.0;
    String normalizedSignal = extractSignalToken(breakoutType);
    double signalStrength = parseSignalStrength(normalizedSignal);
    double signalTypeTrend = normalizedSignal.startsWith("TB") ? 1.0 : 0.0;
    double signalTypeFx = normalizedSignal.startsWith("FX") ? 1.0 : 0.0;
    double signalTypeMixed = normalizedSignal.startsWith("MX") ? 1.0 : 0.0;
    double groupConfirmed =
        breakoutType != null && breakoutType.contains("noGroupConf") ? 0.0 : 1.0;
    double strongTrend = adx >= 30.0 ? 1.0 : 0.0;
    double rangeRegime = adx > 0.0 && adx <= 15.0 ? 1.0 : 0.0;
    LocalDateTime resolvedEntryTime = entryTime != null ? entryTime : resolveEntryTime(candles);

    TradeFeatures features =
        new TradeFeatures(
            adx,
            diValues[0],
            diValues[1],
            atr,
            atr / Math.max(atrAvg, 0.0001),
            rsi,
            emaFast,
            emaSlow,
            emaFast / Math.max(emaSlow, 0.01),
            pricePosition,
            volumeRatio,
            volumeTrend,
            entryConfidence,
            riskRewardRatio,
            stopDistance,
            breakoutType,
            signalStrength,
            signalTypeTrend,
            signalTypeFx,
            signalTypeMixed,
            groupConfirmed,
            strongTrend,
            rangeRegime,
            resolvedEntryTime.getHour(),
            resolvedEntryTime.getDayOfWeek().getValue(),
            ticker,
            strategy,
            resolvedEntryTime,
            entryPrice);

    tradeHistory.add(features);
    openTrades.put(buildTradeKey(ticker, strategy, features.entryTime), features);
  }

  public synchronized void recordTradeOutcome(
      String ticker,
      String strategy,
      double pnlRubles,
      double entryPrice,
      double stopLoss,
      int quantity) {
    double risk = Math.abs(entryPrice - stopLoss);
    double pnlR = risk > 0 && quantity > 0 ? pnlRubles / (risk * Math.max(1.0, quantity)) : 0.0;

    TradeFeatures trade = removeLatestOpenTrade(ticker, strategy);
    if (trade == null) {
      for (int i = tradeHistory.size() - 1; i >= 0; i--) {
        TradeFeatures item = tradeHistory.get(i);
        if (item.ticker.equals(ticker) && item.strategy.equals(strategy) && item.outcome == null) {
          trade = item;
          break;
        }
      }
    }

    if (trade != null) {
      trade.outcome = pnlR;
      trade.isWinner = pnlR > 0;
      appendTrade(trade);
    }
  }

  /** Get training data filtered by quality. */
  public synchronized List<TradeFeatures> getGoodTrades() {
    List<TradeFeatures> good = new ArrayList<>();
    for (TradeFeatures t : tradeHistory) {
      if (t.outcome != null && t.outcome >= 1.0) {
        good.add(t);
      }
    }
    return good;
  }

  public synchronized List<TradeFeatures> getBadTrades() {
    List<TradeFeatures> bad = new ArrayList<>();
    for (TradeFeatures t : tradeHistory) {
      if (t.outcome != null && t.outcome < 0.0) {
        bad.add(t);
      }
    }
    return bad;
  }

  /** Get statistics. */
  public synchronized Map<String, Object> getStatistics() {
    Map<String, Object> stats = new HashMap<>();
    stats.put("total", tradeHistory.size());
    stats.put("withOutcome", (int) tradeHistory.stream().filter(t -> t.outcome != null).count());
    stats.put(
        "winners",
        (int) tradeHistory.stream().filter(t -> t.isWinner != null && t.isWinner).count());
    stats.put(
        "losers",
        (int) tradeHistory.stream().filter(t -> t.isWinner != null && !t.isWinner).count());

    double avgOutcome =
        tradeHistory.stream()
            .filter(t -> t.outcome != null)
            .mapToDouble(t -> t.outcome)
            .average()
            .orElse(0.0);
    stats.put("avgOutcome", avgOutcome);

    return stats;
  }

  // === Technical Analysis Helpers ===

  private double calculateAdx(List<Candle> candles, int period) {
    if (candles.size() < period * 2) return 0.0;

    double trSum = 0, pdSum = 0, mdSum = 0;
    for (int i = candles.size() - period; i < candles.size(); i++) {
      Candle c = candles.get(i);
      Candle p = candles.get(i - 1);

      double tr =
          Math.max(Math.max(c.high - c.low, Math.abs(c.high - p.close)), Math.abs(c.low - p.close));
      trSum += tr;

      double up = c.high - p.high;
      double dn = p.low - c.low;
      if (up > dn && up > 0) pdSum += up;
      if (dn > up && dn > 0) mdSum += dn;
    }

    double atr = trSum / period;
    double diPlus = atr > 0 ? (pdSum / period) / atr * 100 : 0;
    double diMinus = atr > 0 ? (mdSum / period) / atr * 100 : 0;

    return (diPlus + diMinus) > 0 ? Math.abs(diPlus - diMinus) / (diPlus + diMinus) * 100 : 0;
  }

  private double[] calculateDI(List<Candle> candles, int period) {
    double trSum = 0, pdSum = 0, mdSum = 0;
    for (int i = candles.size() - period; i < candles.size(); i++) {
      Candle c = candles.get(i);
      Candle p = candles.get(i - 1);

      double tr =
          Math.max(Math.max(c.high - c.low, Math.abs(c.high - p.close)), Math.abs(c.low - p.close));
      trSum += tr;

      double up = c.high - p.high;
      double dn = p.low - c.low;
      if (up > dn && up > 0) pdSum += up;
      if (dn > up && dn > 0) mdSum += dn;
    }

    double atr = trSum / period;
    double diPlus = atr > 0 ? (pdSum / period) / atr * 100 : 0;
    double diMinus = atr > 0 ? (mdSum / period) / atr * 100 : 0;

    return new double[] {diPlus, diMinus};
  }

  private double calculateAtr(List<Candle> candles, int period) {
    if (candles.size() < period + 1) return 0.0;
    double sum = 0.0;
    for (int i = candles.size() - period; i < candles.size(); i++) {
      Candle c = candles.get(i);
      Candle p = candles.get(i - 1);
      sum +=
          Math.max(Math.max(c.high - c.low, Math.abs(c.high - p.close)), Math.abs(c.low - p.close));
    }
    return sum / period;
  }

  private double calculateAverageAtr(List<Candle> candles, int period) {
    if (candles.size() < period + 1) return 0.0;
    double sum = 0.0;
    for (int i = candles.size() - period; i < candles.size(); i++) {
      sum += calculateAtr(candles.subList(0, i + 1), 14);
    }
    return sum / period;
  }

  private double calculateRsi(List<Candle> candles, int period) {
    if (candles.size() < period + 1) return 50.0;

    double gains = 0.0, losses = 0.0;
    for (int i = candles.size() - period; i < candles.size(); i++) {
      double change = candles.get(i).close - candles.get(i - 1).close;
      if (change > 0) gains += change;
      else losses -= change;
    }

    double rs = losses == 0 ? 100 : gains / losses;
    return 100.0 - (100.0 / (1.0 + rs));
  }

  private double calculateEma(List<Candle> candles, int period) {
    if (candles.size() < period) return candles.get(candles.size() - 1).close;

    double multiplier = 2.0 / (period + 1);
    double ema = candles.get(0).close;

    for (int i = 1; i < candles.size(); i++) {
      ema = (candles.get(i).close - ema) * multiplier + ema;
    }

    return ema;
  }

  private double calculatePricePosition(List<Candle> candles) {
    if (candles.isEmpty()) return 0.5;

    double highest = candles.stream().mapToDouble(c -> c.high).max().orElse(0);
    double lowest = candles.stream().mapToDouble(c -> c.low).min().orElse(0);
    double current = candles.get(candles.size() - 1).close;

    return (highest - lowest) > 0 ? (current - lowest) / (highest - lowest) : 0.5;
  }

  private double calculatePricePositionForEntry(List<Candle> candles, double entryPrice) {
    if (candles.isEmpty()) return 0.5;

    double highest = candles.stream().mapToDouble(c -> c.high).max().orElse(0.0);
    double lowest = candles.stream().mapToDouble(c -> c.low).min().orElse(0.0);
    return (highest - lowest) > 0 ? (entryPrice - lowest) / (highest - lowest) : 0.5;
  }

  private double calculateVolumeRatio(List<Candle> candles, int period) {
    if (candles.size() < period + 1) return 1.0;

    long currentVolume = candles.get(candles.size() - 1).volume;
    long avgVolume = 0;
    for (int i = candles.size() - period - 1; i < candles.size() - 1; i++) {
      avgVolume += candles.get(i).volume;
    }
    avgVolume /= period;

    return avgVolume > 0 ? (double) currentVolume / avgVolume : 1.0;
  }

  private double calculateVolumeTrend(List<Candle> candles, int lookback) {
    if (candles.size() <= lookback) {
      return 0.0;
    }

    double recent = 0.0;
    double previous = 0.0;
    for (int i = candles.size() - lookback; i < candles.size(); i++) {
      recent += candles.get(i).volume;
      previous += candles.get(i - lookback).volume;
    }

    return 0.0 != previous ? (recent - previous) / previous : 0.0;
  }

  // === Persistence ===

  private void loadExistingData() {
    File file = new File(dataFile);
    if (!file.exists()) return;

    try (BufferedReader reader = new BufferedReader(new FileReader(file))) {
      String line;
      boolean isHeader = true;
      while ((line = reader.readLine()) != null) {
        if (isHeader) {
          isHeader = false;
          continue;
        }
        if (line.trim().isEmpty()) {
          continue;
        }

        String[] parts = line.split(",", -1);
        if (parts.length < 36) {
          continue;
        }

        try {
          LocalDateTime entryTime = LocalDateTime.parse(parts[0]);
          TradeFeatures trade =
              new TradeFeatures(
                  parseDouble(parts[3]),
                  parseDouble(parts[4]),
                  parseDouble(parts[5]),
                  parseDouble(parts[6]),
                  parseDouble(parts[7]),
                  parseDouble(parts[8]),
                  parseDouble(parts[9]),
                  parseDouble(parts[10]),
                  parseDouble(parts[11]),
                  parseDouble(parts[12]),
                  parseDouble(parts[13]),
                  parseDouble(parts[14]),
                  parseDouble(parts[15]),
                  parseDouble(parts[16]),
                  parseDouble(parts[17]),
                  "IMPORTED",
                  parseDouble(parts[18]),
                  parseDouble(parts[19]),
                  parseDouble(parts[20]),
                  parseDouble(parts[21]),
                  parseDouble(parts[22]),
                  parseDouble(parts[23]),
                  parseDouble(parts[24]),
                  parseInt(parts[25]),
                  parseInt(parts[26]),
                  parts[1],
                  parts[2],
                  entryTime,
                  parseDouble(parts[29]));
          if (!parts[33].isEmpty()) {
            trade.outcome = parseDouble(parts[33]);
          }
          if (!parts[34].isEmpty()) {
            trade.isWinner = Boolean.parseBoolean(parts[34]);
          }
          tradeHistory.add(trade);
          if (trade.outcome == null) {
            openTrades.put(buildTradeKey(trade.ticker, trade.strategy, trade.entryTime), trade);
          }
        } catch (RuntimeException ignored) {
          // skip malformed row
        }
      }
    } catch (IOException e) {
      System.err.println("Error loading trade data: " + e.getMessage());
    }
  }

  /** Проверяет, отсортированы ли записи по времени. */
  private boolean isSortedByTime() {
    if (tradeHistory.size() <= 1) {
      return true;
    }
    TradeFeatures previous = tradeHistory.get(0);
    for (int i = 1; i < tradeHistory.size(); i++) {
      TradeFeatures current = tradeHistory.get(i);
      if (current.entryTime.isBefore(previous.entryTime)) {
        return false;
      }
      previous = current;
    }
    return true;
  }

  private void saveData() {
    deduplicateTradeHistory();
    tradeHistory.sort(
        Comparator.comparing((TradeFeatures trade) -> trade.entryTime)
            .thenComparing(trade -> trade.strategy)
            .thenComparing(trade -> trade.ticker));

    File file = new File(dataFile);
    File parent = file.getParentFile();
    if (parent != null && !parent.exists()) {
      parent.mkdirs();
    }
    List<String> existingRows = readExistingRows(file);
    boolean writeHeader = !file.exists() || 0L == file.length();

    try (PrintWriter writer = new PrintWriter(new FileWriter(file, true))) {
      if (writeHeader) {
        writer.println(CSV_HEADER);
      }

      for (TradeFeatures trade : tradeHistory) {
        String row = formatTradeRow(trade);
        if (existingRows.contains(row)) {
          continue;
        }
        writer.println(row);
        existingRows.add(row);
      }
    } catch (IOException e) {
      System.err.println("Error saving trade data: " + e.getMessage());
    }
  }

  private void appendTrade(TradeFeatures trade) {
    deduplicateTradeHistory();

    // Check if sorting and full rewrite is needed
    if (isSortedByTime()) {
      // Records are sorted, just append the new one at the end
      appendTradeRow(trade);
    } else {
      // Sorting and full rewrite required
      saveData();
    }
  }

  /** Добавляет одну строку сделки в конец файла. */
  private void appendTradeRow(TradeFeatures trade) {
    File file = new File(dataFile);
    File parent = file.getParentFile();
    if (parent != null && !parent.exists()) {
      parent.mkdirs();
    }

    boolean writeHeader = !file.exists() || 0L == file.length();
    try (PrintWriter writer = new PrintWriter(new FileWriter(file, true))) {
      if (writeHeader) {
        writer.println(CSV_HEADER);
      }
      writer.println(formatTradeRow(trade));
    } catch (IOException e) {
      System.err.println("Error appending trade data: " + e.getMessage());
    }
  }

  private List<String> readExistingRows(File file) {
    List<String> rows = new ArrayList<>();
    if (!file.exists()) {
      return rows;
    }

    try (BufferedReader reader = new BufferedReader(new FileReader(file))) {
      String line;
      boolean skipHeader = true;
      while ((line = reader.readLine()) != null) {
        if (skipHeader) {
          skipHeader = false;
          if (CSV_HEADER.equals(line)) {
            continue;
          }
        }
        if (!line.isBlank()) {
          rows.add(line);
        }
      }
    } catch (IOException e) {
      System.err.println("Error reading existing trade data: " + e.getMessage());
    }
    return rows;
  }

  private TradeFeatures removeLatestOpenTrade(String ticker, String strategy) {
    String keyPrefix = buildTradeKeyPrefix(ticker, strategy);
    TradeFeatures latestTrade = null;
    String latestKey = null;

    for (Map.Entry<String, TradeFeatures> entry : openTrades.entrySet()) {
      if (!entry.getKey().startsWith(keyPrefix)) {
        continue;
      }
      TradeFeatures candidate = entry.getValue();
      if (latestTrade == null || candidate.entryTime.isAfter(latestTrade.entryTime)) {
        latestTrade = candidate;
        latestKey = entry.getKey();
      }
    }

    if (latestKey != null) {
      openTrades.remove(latestKey);
    }
    return latestTrade;
  }

  private void deduplicateTradeHistory() {
    Map<String, TradeFeatures> uniqueTrades = new LinkedHashMap<>();
    for (TradeFeatures trade : tradeHistory) {
      String key = buildDedupKey(trade);
      TradeFeatures existing = uniqueTrades.get(key);
      // Приоритет: сделка с outcome > сделка без outcome
      if (existing == null) {
        uniqueTrades.put(key, trade);
      } else if (trade.outcome != null && existing.outcome == null) {
        // Заменяем сделку без outcome на сделку с outcome
        uniqueTrades.put(key, trade);
      }
      // Если обе сделки с outcome или обе без - оставляем первую (старую)
    }

    if (uniqueTrades.size() == tradeHistory.size()) {
      return;
    }

    tradeHistory.clear();
    tradeHistory.addAll(uniqueTrades.values());
  }

  private String formatTradeRow(TradeFeatures trade) {
    boolean isShort = isShortTrade(trade);
    double stopLoss =
        isShort
            ? trade.entryPrice * (1.0 + trade.stopDistance / 100.0)
            : trade.entryPrice * (1.0 - trade.stopDistance / 100.0);
    double takeProfit =
        isShort
            ? trade.entryPrice * (1.0 - (trade.stopDistance / 100.0) * trade.riskRewardRatio)
            : trade.entryPrice * (1.0 + (trade.stopDistance / 100.0) * trade.riskRewardRatio);

    return String.format(
        Locale.US,
        "%s,%s,%s,%.6f,%.6f,%.6f,%.6f,%.6f,%.6f,%.6f,%.6f,%.6f,%.6f,%.6f,%.6f,%.6f,%.6f,%.6f,%.6f,%.6f,%.6f,%.6f,%.6f,%.6f,%.6f,%d,%d,%d,%d,%.6f,%.6f,%.6f,%d,%s,%s",
        trade.entryTime,
        trade.ticker,
        trade.strategy,
        trade.adx,
        trade.diPlus,
        trade.diMinus,
        trade.atr,
        trade.atrRatio,
        trade.rsi,
        trade.emaFast,
        trade.emaSlow,
        trade.emaRatio,
        trade.pricePosition,
        trade.volumeRatio,
        trade.volumeTrend,
        trade.entryConfidence,
        trade.riskRewardRatio,
        trade.stopDistance,
        trade.signalStrength,
        trade.signalTypeTrend,
        trade.signalTypeFx,
        trade.signalTypeMixed,
        trade.groupConfirmed,
        trade.strongTrend,
        trade.rangeRegime,
        trade.hourOfDay,
        trade.dayOfWeek,
        trade.isMorning ? 1 : 0,
        trade.isAfternoon ? 1 : 0,
        trade.entryPrice,
        stopLoss,
        takeProfit,
        1,
        trade.outcome != null ? String.format(Locale.US, "%.6f", trade.outcome) : "",
        trade.isWinner != null ? trade.isWinner.toString() : "");
  }

  private String buildDedupKey(TradeFeatures trade) {
    boolean isShort = isShortTrade(trade);
    double stopLoss =
        isShort
            ? trade.entryPrice * (1.0 + trade.stopDistance / 100.0)
            : trade.entryPrice * (1.0 - trade.stopDistance / 100.0);
    double takeProfit =
        isShort
            ? trade.entryPrice * (1.0 - (trade.stopDistance / 100.0) * trade.riskRewardRatio)
            : trade.entryPrice * (1.0 + (trade.stopDistance / 100.0) * trade.riskRewardRatio);
    return String.format(
        Locale.US,
        "%s|%s|%.6f|%.6f|%.6f",
        trade.entryTime,
        trade.ticker,
        trade.entryPrice,
        stopLoss,
        takeProfit);
  }

  private String buildTradeKey(String ticker, String strategy, LocalDateTime entryTime) {
    return buildTradeKeyPrefix(ticker, strategy) + entryTime;
  }

  private String buildTradeKeyPrefix(String ticker, String strategy) {
    return strategy + "::" + ticker + "::";
  }

  private boolean isShortTrade(TradeFeatures trade) {
    if (trade.breakoutType == null) {
      return false;
    }
    String signal = trade.breakoutType.toUpperCase(Locale.ROOT);
    return signal.contains("FXS") || signal.contains("MXS") || signal.contains("_S_");
  }

  private double parseDouble(String value) {
    return value == null || value.isEmpty() ? 0.0 : Double.parseDouble(value);
  }

  private int parseInt(String value) {
    return value == null || value.isEmpty() ? 0 : Integer.parseInt(value);
  }

  private double parseSignalStrength(String breakoutType) {
    if (breakoutType == null || breakoutType.isEmpty()) {
      return 0.0;
    }

    String[] parts = breakoutType.split("_");
    if (parts.length < 2) {
      return 0.0;
    }

    try {
      return Double.parseDouble(parts[1]);
    } catch (NumberFormatException ex) {
      return 0.0;
    }
  }

  private String extractSignalToken(String breakoutType) {
    if (breakoutType == null || breakoutType.isEmpty()) {
      return "";
    }

    for (String part : breakoutType.split("_")) {
      if ("TB".equals(part)
          || "FXB".equals(part)
          || "FXS".equals(part)
          || "MXB".equals(part)
          || "MXS".equals(part)) {
        return part;
      }
    }

    if (breakoutType.contains("TB_")) {
      return breakoutType.substring(breakoutType.indexOf("TB_"));
    }
    if (breakoutType.contains("FX")) {
      return breakoutType.substring(breakoutType.indexOf("FX"));
    }
    if (breakoutType.contains("MX")) {
      return breakoutType.substring(breakoutType.indexOf("MX"));
    }
    return breakoutType;
  }

  private LocalDateTime resolveEntryTime(List<Candle> candles) {
    if (candles == null || candles.isEmpty()) {
      return LocalDateTime.now();
    }

    try {
      Date date = CANDLE_TIME_FORMAT.get().parse(candles.get(candles.size() - 1).time);
      return LocalDateTime.ofInstant(date.toInstant(), ZoneId.systemDefault());
    } catch (Exception ignored) {
      return LocalDateTime.now();
    }
  }
}
