package com.github.shk0da.GoldenDragon.test;

import com.github.shk0da.GoldenDragon.model.TickerCandle;
import com.github.shk0da.GoldenDragon.model.TickerInfo;
import com.github.shk0da.GoldenDragon.model.TickerInfo.Key;
import com.github.shk0da.GoldenDragon.repository.Repository;
import com.github.shk0da.GoldenDragon.repository.TickerRepository;
import com.github.shk0da.GoldenDragon.utils.GerchikUtils;
import com.github.shk0da.GoldenDragon.utils.LevelUtils;
import com.github.shk0da.GoldenDragon.utils.LevelUtils.Level;
import com.google.gson.reflect.TypeToken;
import java.io.BufferedReader;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.stream.Collectors;


import static com.github.shk0da.GoldenDragon.repository.TickerRepository.SERIALIZE_NAME;
import static com.github.shk0da.GoldenDragon.utils.IndicatorsUtil.INDICATORS_SHIFT;
import static com.github.shk0da.GoldenDragon.utils.SerializationUtils.loadDataFromDisk;
import static java.lang.System.out;

public class TestLevelTrader {

    private static final double K2 = 0.025;
    private static final double COMISSION = 0.05;
    private static final double TP = 1.5;
    private static final double SL = 0.5;
    private static final String[] TICKERS = {
        "CNYRUBF", "USDRUBF", "SBER", "GAZP", "LKOH", "MTSS", "NVTK", "ROSN", "HEAD", "RTKM", "PLZL"
    };
    private static final boolean SAVE_BEST_CONFIGS = true;
    private static final String CONFIG_DIR = ".";
    private static final int THREADS = Runtime.getRuntime().availableProcessors() / 3;

    private static final Map<String, List<TickerCandle>> candleRegister = new ConcurrentHashMap<>();
    private static final Map<String, List<Double>> levelsRegister = new ConcurrentHashMap<>();
    private static final Map<String, List<TickerCandle>> backtestCandlesRegister = new ConcurrentHashMap<>();

    private static class Result {
        private final double profit;
        private final double winrate;
        private final int tradeCount;

        Result(double profit, double winrate, int tradeCount) {
            this.profit = profit;
            this.winrate = winrate;
            this.tradeCount = tradeCount;
        }

        double profit() { return profit; }
        double winrate() { return winrate; }
        int tradeCount() { return tradeCount; }
    }

    public static void main(String[] args) {
        Repository<Key, TickerInfo> tickerRepository = TickerRepository.INSTANCE;
        Map<TickerInfo.Key, TickerInfo> dataFromDisk = loadDataFromDisk(SERIALIZE_NAME, new TypeToken<>() {});
        tickerRepository.putAll(dataFromDisk);

        // Предзагрузка данных для всех тикеров
        preloadData();

        optimizeParallel();
    }

    /**
     * Предзагружает свечи и уровни для всех тикеров один раз.
     */
    private static void preloadData() {
        out.println("Предзагрузка данных для тикеров: " + String.join(", ", TICKERS) + "\n");
        
        // Параллельная предзагрузка
        ExecutorService loader = Executors.newFixedThreadPool(THREADS);
        List<Future<?>> futures = new ArrayList<>();
        
        for (String ticker : TICKERS) {
            futures.add(loader.submit(() -> {
                // Загружаем свечи для уровней (HOUR)
                getLevels(ticker.toLowerCase());
                // Загружаем свечи для бэктеста (5_MIN)
                getBacktestCandles(ticker.toLowerCase());
            }));
        }
        
        for (Future<?> f : futures) {
            try { f.get(); } catch (Exception ignore) {}
        }
        loader.shutdown();
        
        out.println("Данные загружены.\n");
    }

    private static List<TickerCandle> getBacktestCandles(String name) {
        return backtestCandlesRegister.computeIfAbsent(name, n -> {
            List<TickerCandle> full = readCandlesFile(n.toUpperCase(), "data", "candles5_MIN.txt");
            int cut = (int) (full.size() - (full.size() * K2));
            return full.subList(Math.max(0, cut - (int)(full.size() * K2)), full.size());
        });
    }

    private static List<Double> getLevels(String name) {
        return levelsRegister.computeIfAbsent(name, n -> {
            List<TickerCandle> candles = readCandlesFile(n.toUpperCase(), "data", "candlesHOUR.txt");
            int cut = (int) (candles.size() - candles.size() * K2);
            if (cut <= 0) cut = candles.size();
            return new LevelUtils().identifyKeyLevels(candles.subList(0, cut))
                    .stream()
                    .map(Level::getPrice)
                    .sorted()
                    .collect(Collectors.toList());
        });
    }

    private static List<TickerCandle> readCandlesFile(String name, String dataDir, String file) {
        var key = name + dataDir + file;
        List<TickerCandle> cached = candleRegister.get(key);
        if (cached != null) return new ArrayList<>(cached);

        List<TickerCandle> tickers = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader(dataDir + "/" + name + "/" + file))) {
            String line;
            boolean isFirstLine = true;
            while ((line = br.readLine()) != null) {
                if (line.trim().isEmpty() || isFirstLine) {
                    isFirstLine = false;
                    continue;
                }
                String[] values = line.split(",");
                if (values.length < 6) continue;
                try {
                    tickers.add(new TickerCandle(
                            name, values[0],
                            Double.parseDouble(values[1]),
                            Double.parseDouble(values[2]),
                            Double.parseDouble(values[3]),
                            Double.parseDouble(values[4]),
                            Double.parseDouble(values[4]),
                            Integer.parseInt(values[5])));
                } catch (NumberFormatException ignore) {}
            }
        } catch (Exception ex) {
            out.println("Error reading file: " + ex.getMessage());
            throw new RuntimeException(ex);
        }
        candleRegister.put(key, tickers);
        return new ArrayList<>(tickers);
    }

    private static void optimizeParallel() {
        out.println("Поиск оптимальных параметров GerchikUtils (параллельно, потоков: " + THREADS + ")...\n");

        int[] touchesValues = {2, 3, 4};
        double[] zoneValues = {0.005, 0.008, 0.01, 0.012};
        int[] candlesValues = {2, 3, 4};
        double[] patternValues = {0.8, 1.0, 1.2};

        List<GerchikUtils> configs = new ArrayList<>();
        for (int touches : touchesValues)
            for (double zone : zoneValues)
                for (int candles : candlesValues)
                    for (double pattern : patternValues)
                        configs.add(new GerchikUtils(touches, zone, candles, pattern));

        ExecutorService executor = Executors.newFixedThreadPool(THREADS);
        Map<String, GerchikUtils> bestConfigPerTicker = new ConcurrentHashMap<>();
        Map<String, Double> bestProfitPerTicker = new ConcurrentHashMap<>();
        Map<String, Integer> bestTradesPerTicker = new ConcurrentHashMap<>();

        try {
            List<Future<Void>> futures = new ArrayList<>();

            for (GerchikUtils config : configs) {
                futures.add(executor.submit(() -> {
                    for (String ticker : TICKERS) {
                        var result = runLite(config, ticker);
                        if (result.tradeCount() > 0) {
                            double tickerBestProfit = bestProfitPerTicker.getOrDefault(ticker, Double.NEGATIVE_INFINITY);
                            int tickerBestTrades = bestTradesPerTicker.getOrDefault(ticker, 0);
                            boolean isBetter = (result.tradeCount() > tickerBestTrades) ||
                                              (result.tradeCount() == tickerBestTrades && result.profit() > tickerBestProfit);
                            if (isBetter) {
                                bestConfigPerTicker.put(ticker, config);
                                bestProfitPerTicker.put(ticker, result.profit());
                                bestTradesPerTicker.put(ticker, result.tradeCount());
                            }
                        }
                    }
                    return null;
                }));
            }

            for (Future<Void> future : futures) {
                future.get();
            }
        } catch (Exception e) {
            throw new RuntimeException(e);
        } finally {
            executor.shutdown();
        }

        out.println("\n==============================================");
        out.println("ЛУЧШАЯ КОНФИГУРАЦИЯ ПО ТИКЕРАМ");
        out.println("==============================================");

        for (String ticker : TICKERS) {
            GerchikUtils cfg = bestConfigPerTicker.get(ticker);
            if (cfg != null) {
                out.printf("%-10s | Touches=%d, Zone=%.3f, Candles=%d, Pattern=%.1f | Profit: %,.0f | Trades: %d%n",
                        ticker, cfg.levelConfirmationTouches, cfg.levelZonePercent,
                        cfg.confirmationCandles, cfg.minPatternStrength,
                        bestProfitPerTicker.get(ticker), bestTradesPerTicker.get(ticker));
            } else {
                out.printf("%-10s | No results%n", ticker);
            }
        }

        if (SAVE_BEST_CONFIGS) {
            out.println("\n==============================================");
            out.println("СОХРАНЕНИЕ КОНФИГУРАЦИЙ");
            out.println("==============================================");
            for (String ticker : TICKERS) {
                GerchikUtils cfg = bestConfigPerTicker.get(ticker);
                if (cfg != null) {
                    saveConfig(ticker, cfg);
                }
            }
        }

        out.println("\n==============================================");
        out.println("ФИНАЛЬНЫЙ ПРОГОН НА ЛУЧШИХ КОНФИГУРАЦИЯХ");
        out.println("==============================================");

        double grandTotalProfit = 0.0;
        int grandTotalTrades = 0;
        double grandTotalWinRate = 0.0;
        int validTickerCount = 0;

        for (String ticker : TICKERS) {
            GerchikUtils cfg = bestConfigPerTicker.get(ticker);
            if (cfg != null) {
                var result = runLite(cfg, ticker);
                out.printf("%-10s | Profit: %12,.0f | Trades: %4d | WinRate: %6.1f%%%n",
                        ticker, result.profit(), result.tradeCount(), result.winrate());
                grandTotalProfit += result.profit();
                grandTotalTrades += result.tradeCount();
                grandTotalWinRate += result.winrate();
                validTickerCount++;
            } else {
                out.printf("%-10s | No trades%n", ticker);
            }
        }

        out.println("\n==============================================");
        out.println("ОБЩИЕ РЕЗУЛЬТАТЫ");
        out.println("==============================================");
        out.printf("  Total Profit: %,.0f RUB%n", grandTotalProfit);
        out.printf("  Avg WinRate: %.1f%%%n", validTickerCount > 0 ? grandTotalWinRate / validTickerCount : 0);
        out.printf("  Total Trades: %d%n", grandTotalTrades);
        out.printf("  Tickers Tested: %d/%d%n", validTickerCount, TICKERS.length);
    }

    private static void saveConfig(String ticker, GerchikUtils config) {
        Properties props = new Properties();
        props.setProperty("levelTrader.levelConfirmationTouches", String.valueOf(config.levelConfirmationTouches));
        props.setProperty("levelTrader.levelZonePercent", String.valueOf(config.levelZonePercent));
        props.setProperty("levelTrader.confirmationCandles", String.valueOf(config.confirmationCandles));
        props.setProperty("levelTrader.minPatternStrength", String.valueOf(config.minPatternStrength));

        String fileName = CONFIG_DIR + "/" + ticker + ".properties";
        try (FileOutputStream fos = new FileOutputStream(fileName)) {
            props.store(fos, "LevelTrader Config");
            out.println("  -> Сохранено: " + fileName);
        } catch (IOException e) {
            out.println("  -> Ошибка сохранения " + fileName + ": " + e.getMessage());
        }
    }

    private static Result runLite(GerchikUtils config, String ticker) {
        List<TickerCandle> candles = getBacktestCandles(ticker.toLowerCase());

        if (candles.size() < 100) {
            return new Result(0.0, 0.0, 0);
        }

        var levels = getLevels(ticker.toLowerCase());
        return backtestLite(candles, levels, config);
    }

    private static Result backtestLite(List<TickerCandle> candles, List<Double> levels, GerchikUtils config) {
        double balance = 100_000.0;
        double cashOpen = 0.0;
        int count = 0;
        double prevClose = 0.0;
        int winCounter = 0, loseCounter = 0;
        double grossProfit = 0.0, grossLoss = 0.0;

        for (int i = INDICATORS_SHIFT; i < candles.size(); i++) {
            var candle = candles.get(i);
            var close = candle.getClose();

            if (i == INDICATORS_SHIFT) {
                prevClose = close;
                continue;
            }

            if (count != 0) {
                var min = candle.getLow();
                var max = candle.getHigh();

                boolean exit = false;
                double price = 0;

                if (count > 0) {
                    if (max >= prevClose + (prevClose / 100) * TP) {
                        price = prevClose + (prevClose / 100) * TP;
                        exit = true;
                    } else if (min <= prevClose - (prevClose / 100) * SL) {
                        price = prevClose - (prevClose / 100) * SL;
                        exit = true;
                    }
                } else {
                    if (min <= prevClose - (prevClose / 100) * TP) {
                        price = prevClose - (prevClose / 100) * TP;
                        exit = true;
                    } else if (max >= prevClose + (prevClose / 100) * SL) {
                        price = prevClose + (prevClose / 100) * SL;
                        exit = true;
                    }
                }

                if (exit) {
                    var cashClose = count * price;
                    var operationResult = round(cashClose - cashOpen, 4);
                    var commission = round(Math.abs(cashClose / 100) * COMISSION, 4);
                    var resultWithCommission = operationResult - commission;

                    if (resultWithCommission > 0) {
                        winCounter++;
                        grossProfit += resultWithCommission;
                    } else {
                        loseCounter++;
                        grossLoss += Math.abs(resultWithCommission);
                    }

                    balance = round(balance + resultWithCommission, 4);
                    cashOpen = 0.0;
                    count = 0;
                }
            }

            if (count == 0) {
                var action = config.getLevelAction(candles.subList(0, i + 1), levels);
                if (action.isLong() && close > prevClose) {
                    count = (int) Math.round((balance * K2 * 100) / close);
                    if (count > 0) {
                        cashOpen = count * close;
                        balance -= round(Math.abs(cashOpen / 100) * COMISSION, 4);
                    }
                } else if (action.isShort() && close < prevClose) {
                    count = (int) Math.round((balance * K2 * 100) / close);
                    if (count > 0) {
                        cashOpen = count * close;
                        balance -= round(Math.abs(cashOpen / 100) * COMISSION, 4);
                    }
                }
            }

            prevClose = close;
        }

        int totalTrades = winCounter + loseCounter;
        double winRate = totalTrades > 0 ? (double) winCounter / totalTrades * 100 : 0.0;
        return new Result(balance, winRate, totalTrades);
    }

    private static double round(double value, int places) {
        long factor = (long) Math.pow(10, places);
        return Math.round(value * factor) / (double) factor;
    }
}
