import com.github.shk0da.GoldenDragon.model.TickerCandle;
import com.github.shk0da.GoldenDragon.model.TickerInfo;
import com.github.shk0da.GoldenDragon.model.TickerInfo.Key;
import com.github.shk0da.GoldenDragon.repository.Repository;
import com.github.shk0da.GoldenDragon.repository.TickerRepository;
import com.github.shk0da.GoldenDragon.utils.GerchikUtils;
import com.github.shk0da.GoldenDragon.utils.IndicatorsUtil;
import com.github.shk0da.GoldenDragon.utils.LevelUtils;
import com.github.shk0da.GoldenDragon.utils.LevelUtils.Level;
import com.google.gson.reflect.TypeToken;
import java.awt.Color;
import java.io.BufferedReader;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.nio.file.Path;
import java.text.DecimalFormat;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import ml.dmlc.xgboost4j.java.Booster;
import ml.dmlc.xgboost4j.java.DMatrix;
import ml.dmlc.xgboost4j.java.XGBoost;
import ml.dmlc.xgboost4j.java.XGBoostError;
import org.apache.commons.lang3.tuple.Pair;
import org.deeplearning4j.nn.multilayer.MultiLayerNetwork;
import org.deeplearning4j.util.ModelSerializer;
import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartUtilities;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.ValueMarker;
import org.jfree.chart.plot.XYPlot;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;
import org.nd4j.linalg.factory.Nd4j;

import static com.github.shk0da.GoldenDragon.repository.TickerRepository.SERIALIZE_NAME;
import static com.github.shk0da.GoldenDragon.utils.DataLearningUtils.StockDataSetIterator.getBoosterInput;
import static com.github.shk0da.GoldenDragon.utils.DataLearningUtils.StockDataSetIterator.getNetworkInput;
import static com.github.shk0da.GoldenDragon.utils.DataLearningUtils.createInput;
import static com.github.shk0da.GoldenDragon.utils.IndicatorsUtil.INDICATORS_SHIFT;
import static com.github.shk0da.GoldenDragon.utils.IndicatorsUtil.calculateATR;
import static com.github.shk0da.GoldenDragon.utils.IndicatorsUtil.convertCandles;
import static com.github.shk0da.GoldenDragon.utils.SerializationUtils.loadDataFromDisk;
import static java.lang.System.out;
import static java.nio.file.Files.deleteIfExists;

public class TestLevelTrader {

    private static final Double K2 = 0.40;
    private static final Double COMISSION = 0.05;
    private static final Double TP = 0.9;
    private static final Double SL = 0.3;
    private static final Double RISK = 30.0;
    private static final Boolean createPlot = false;
    private static final Boolean debugLogging = false;
    private static final Boolean useNN = false;
    private static final Double initBalance = 100_000.00;
    private static final Double averagePositionCost = 10_000.00;
    private static final List<String> stocks = List.of("CNYRUBF", "USDRUBF", "HEAD", "LKOH", "MTSS", "PLZL", "RTKM",
            "SBER");

    private static final DecimalFormat df = new DecimalFormat("#.##");
    private static final DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm:ss");

    private static final Map<String, List<TickerCandle>> candleRegister = new ConcurrentHashMap<>();
    private static final Map<String, List<Double>> levelsRegister = new ConcurrentHashMap<>();
    private static final Map<String, MultiLayerNetwork> multiLayerNetworkRegister = new ConcurrentHashMap<>();
    private static final Map<String, Booster> boosterRegister = new ConcurrentHashMap<>();

    private static final class Result {

        private final Double profit;
        private final Double winrate;
        private final String message;

        public Result(Double profit, Double winrate, String message) {
            this.profit = profit;
            this.winrate = winrate;
            this.message = message;
        }

        public Double getProfit() {
            return profit;
        }

        public Double getWinrate() {
            return winrate;
        }

        public String getMessage() {
            return message;
        }
    }

    static class Individual {
        GerchikUtils config;
        double fitness;
        double profit;

        public Individual(GerchikUtils config) {
            this.config = config;
        }

        public void evaluate() {
            Pair<Double, Double> result = run(config);
            this.fitness = result.getLeft();
            this.profit = result.getRight();
        }
    }

    public static void main(String[] args) {
        Repository<Key, TickerInfo> tickerRepository = TickerRepository.INSTANCE;
        Map<TickerInfo.Key, TickerInfo> dataFromDisk = loadDataFromDisk(SERIALIZE_NAME, new TypeToken<>() {
        });
        tickerRepository.putAll(dataFromDisk);

        final ThreadLocal<DecimalFormat> df = ThreadLocal.withInitial(() -> new DecimalFormat("#.#####"));

        int populationSize = 50;
        int generations = 200;
        double mutationRate = 0.15;
        int tournamentSize = 5;
        int eliteCount = 5;
        int patience = 10; // остановка, если нет улучшений в течение N поколений

        List<Individual> population = createInitialPopulation(populationSize);
        double bestFitness = 0;
        double bestProfit = 0;
        int noImprovementCount = 0;

        for (int generation = 0; generation < generations; generation++) {
            // Параллельная оценка
            population.parallelStream().forEach(Individual::evaluate);

            // Сортировка по приспособленности
            population
                    .sort(Comparator
                            .comparingDouble((Individual ind) -> -ind.profit)
                            .thenComparingDouble(ind -> -ind.fitness));
            Individual best = population.get(0);

            System.out.printf("%nGeneration %d | Best Fitness: %.2f%% | Profit: %.2f RUB%n%s%n",
                    generation,
                    best.fitness,
                    best.profit,
                    best.config);

            // Ранняя остановка
            if (best.fitness > bestFitness || best.profit > bestProfit) {
                if (best.fitness > bestFitness) {
                    bestFitness = best.fitness;
                }
                if (best.profit > bestProfit) {
                    bestProfit = best.profit;
                }
                noImprovementCount = 0;
            } else {
                noImprovementCount++;
                if (noImprovementCount >= patience) {
                    System.out.println("Early stopping at generation " + generation);
                    break;
                }
            }

            // Элитизм
            List<Individual> newPopulation = new ArrayList<>(population.subList(0, eliteCount));

            // Селекция родителей
            List<Individual> parents = selectParents(population, tournamentSize);

            // Кроссовер и мутация
            while (newPopulation.size() < populationSize) {
                Individual parent1 = parents.get(ThreadLocalRandom.current().nextInt(parents.size()));
                Individual parent2 = parents.get(ThreadLocalRandom.current().nextInt(parents.size()));
                Individual child = crossover(parent1, parent2);
                mutate(child, mutationRate);
                newPopulation.add(child);
            }

            population = newPopulation;
        }

        // Финальный результат
        population
                .sort(Comparator
                        .comparingDouble((Individual ind) -> -ind.profit)
                        .thenComparingDouble(ind -> -ind.fitness));
        Individual best = population.get(0);
        System.out.println("\n Лучшая конфигурация:");
        System.out.println("Fitness: " + df.get().format(best.fitness) + "%");
        System.out.println("Profit: " + df.get().format(best.profit) + " RUB");
        System.out.println("Config: " + best.config);
    }

    static void mutate(Individual individual, double mutationRate) {
        GerchikUtils c = individual.config;

        if (ThreadLocalRandom.current().nextDouble() < mutationRate) {
            c.levelConfirmationTouches = ThreadLocalRandom.current().nextInt(0, 6);
        }
        if (ThreadLocalRandom.current().nextDouble() < mutationRate) {
            c.levelZonePercent = ThreadLocalRandom.current().nextDouble(0.0005, 0.0975);
        }
        if (ThreadLocalRandom.current().nextDouble() < mutationRate) {
            c.breakoutConfirmationPercent = ThreadLocalRandom.current().nextDouble(0.00, 0.005);
        }
        if (ThreadLocalRandom.current().nextDouble() < mutationRate) {
            c.falseBreakoutThreshold = ThreadLocalRandom.current().nextDouble(0.00005, 0.00125);
        }
        if (ThreadLocalRandom.current().nextDouble() < mutationRate) {
            c.volumeMultiplier = ThreadLocalRandom.current().nextDouble(0.05, 0.95);
        }
        if (ThreadLocalRandom.current().nextDouble() < mutationRate) {
            c.confirmationCandles = ThreadLocalRandom.current().nextInt(0, 6);
        }
        if (ThreadLocalRandom.current().nextDouble() < mutationRate) {
            c.maxSignalAge = ThreadLocalRandom.current().nextInt(0, 11);
        }
        if (ThreadLocalRandom.current().nextDouble() < mutationRate) {
            c.volumeConfirmationThreshold = ThreadLocalRandom.current().nextDouble(0.8, 3.0);
        }
    }

    static List<Individual> createInitialPopulation(int populationSize) {
        List<Individual> population = new ArrayList<>();
        population.add(new Individual(new GerchikUtils()));
        population.add(new Individual(new GerchikUtils(
                3,
                0.007,
                0.002,
                0.0005,
                0.5,
                3,
                6,
                2.2)));
        population.add(new Individual(new GerchikUtils(
                2,
                0.0078,
                0.0019,
                0.0005,
                0.40,
                3,
                5,
                2.40)));
        population.add(new Individual(new GerchikUtils(
                2,
                0.0078,
                0.0019,
                0.0005,
                0.40,
                3,
                5,
                2.40)));
        population.add(new Individual(new GerchikUtils(
                4,
                0.01,
                0.004,
                0.001,
                0.7,
                4,
                7,
                2.6)));
        population.add(new Individual(new GerchikUtils(
                2,
                0.003,
                0.002,
                0.0005,
                0.3,
                2,
                4,
                1.5)));
        population.add(new Individual(new GerchikUtils(
                3,
                0.007,
                0.003,
                0.0007,
                0.6,
                3,
                6,
                2.4)));
        IntStream.range(0, populationSize)
                .mapToObj(i -> {
                    int levelConfirmationTouches = ThreadLocalRandom.current().nextInt(0, 11);
                    double levelZonePercent = ThreadLocalRandom.current().nextDouble(0.00005, 1.5);
                    double breakoutConfirmationPercent = ThreadLocalRandom.current().nextDouble(0.0003, 0.05);
                    double falseBreakoutThreshold = ThreadLocalRandom.current().nextDouble(0.00008, 0.00125);
                    double volumeMultiplier = ThreadLocalRandom.current().nextDouble(0.01, 1.0);
                    int confirmationCandles = ThreadLocalRandom.current().nextInt(0, 11);
                    int maxSignalAge = ThreadLocalRandom.current().nextInt(0, 21);
                    double volumeConfirmationThreshold = ThreadLocalRandom.current().nextDouble(0.01, 10.0);

                    GerchikUtils config = new GerchikUtils(
                            levelConfirmationTouches,
                            levelZonePercent,
                            breakoutConfirmationPercent,
                            falseBreakoutThreshold,
                            volumeMultiplier,
                            confirmationCandles,
                            maxSignalAge,
                            volumeConfirmationThreshold);

                    return new Individual(config);
                })
                .collect(Collectors.toList());
        return population;
    }

    static List<Individual> selectParents(List<Individual> population, int tournamentSize) {
        return population.parallelStream().map(ind -> {
            List<Individual> tournament = new ArrayList<>();
            for (int i = 0; i < tournamentSize; i++) {
                tournament.add(population.get(ThreadLocalRandom.current().nextInt(population.size())));
            }
            return tournament.stream()
                    .max(Comparator
                            .comparingDouble((Individual i) -> i.profit)
                            .thenComparingDouble(i -> i.fitness))
                    .orElse(ind);
        }).collect(Collectors.toList());
    }

    static Individual crossover(Individual parent1, Individual parent2) {
        GerchikUtils c1 = parent1.config;
        GerchikUtils c2 = parent2.config;
        GerchikUtils childConfig = new GerchikUtils(
                (c1.levelConfirmationTouches + c2.levelConfirmationTouches) / 2,
                (c1.levelZonePercent + c2.levelZonePercent) / 2,
                (c1.breakoutConfirmationPercent + c2.breakoutConfirmationPercent) / 2,
                (c1.falseBreakoutThreshold + c2.falseBreakoutThreshold) / 2,
                (c1.volumeMultiplier + c2.volumeMultiplier) / 2,
                (c1.confirmationCandles + c2.confirmationCandles) / 2,
                (c1.maxSignalAge + c2.maxSignalAge) / 2,
                (c1.volumeConfirmationThreshold + c2.volumeConfirmationThreshold) / 2
        );
        return new Individual(childConfig);
    }

    public static Pair<Double, Double> run(GerchikUtils config) {
        List<Double> results = new ArrayList<>(stocks.size());
        List<Double> profits = new ArrayList<>(stocks.size());
        stocks.forEach(tickerName -> {
            AtomicReference<Double> balance = new AtomicReference<>(initBalance);
            try {
                String name = tickerName.toLowerCase();
                var currentBalance = balance.get();
                var levels = levelsRegister.computeIfAbsent(
                        name,
                        it -> new LevelUtils()
                                .identifyKeyLevels(readCandlesFile(name.toUpperCase(), "data", "candlesHOUR.txt"))
                                .stream()
                                .map(Level::getPrice)
                                .sorted()
                                .collect(Collectors.toList()));
                var result = run(tickerName, currentBalance, levels, config);
                balance.set(result.getProfit());
                results.add(result.getWinrate());
                profits.add(result.getProfit());
            } catch (Exception skip) {
                // nothing
            }
        });
        var winRate = (results.stream().mapToDouble(it -> it).sum() / (double) results.size());
        var profit = (profits.stream().mapToDouble(it -> it).sum() / (double) profits.size());
        return Pair.of(winRate, profit);
    }

    public static Result run(String name, double balance, List<Double> levels, GerchikUtils config) throws Exception {
        List<TickerCandle> full = readCandlesFile(name, "data", "candles5_MIN.txt");
        var M5 = full.subList((int) (full.size() - (full.size() * K2)), full.size());
        if (M5.isEmpty()) {
            return new Result(balance, 0.0, "");
        }

        Boolean hasTrendUp = null;
        List<Integer> longTrades = new ArrayList<>();
        List<Integer> shortTrades = new ArrayList<>();
        TickerCandle startOfDay = M5.get(0);

        var network = useNN ? getNetwork("data", name) : null;
        var booster = useNN ? getBooster("data", name) : null;

        for (int i = INDICATORS_SHIFT + 2016, x = 0; i < M5.size(); i++, x++) {
            LocalDateTime currentDateTime = LocalDateTime.parse(M5.get(x).getDate(), formatter);
            LocalDateTime startOfDayDateTime = LocalDateTime.parse(startOfDay.getDate(), formatter);
            if (currentDateTime.getDayOfYear() > startOfDayDateTime.getDayOfYear()) {
                startOfDay = M5.get(x);
            }

            if (x > 80 * (60 / 5)) {
                var subList = M5.subList(x - 80 * (60 / 5), x);
                List<TickerCandle> H1;
                try {
                    H1 = convertCandles(subList, 1, ChronoUnit.HOURS);
                } catch (Exception skip) {
                    H1 = new ArrayList<>();
                }
                hasTrendUp = isHasTrendUp(H1);
            }

            if (Boolean.TRUE.equals(hasTrendUp)) {
                var candle5 = M5.get(i);
                var tp = (M5.get(i).getClose() / 100) * TP;
                var subList = M5.subList(i - (INDICATORS_SHIFT + 2016), i);
                var atr = calculateATR(subList, 7);
                var hasUpATR = (startOfDay.getLow() + (atr - (atr * 0.2))) > (candle5.getClose() + tp);
                if (hasUpATR) {
                    if (config.getLevelAction(subList, levels).getLeft()) {
                        if (useNN) {
                            var data = createInput(M5, i, startOfDay.getClose(), levels);
                            var input = getNetworkInput(data);
                            var output = network.rnnTimeStep(Nd4j.create(input).reshape(1, input.length)).getDouble(0);
                            var labels = getBoosterInput(data);
                            var vector = new DMatrix(labels, 1, labels.length, Float.NaN);
                            var predict = booster.predict(vector)[0][0];
                            if (output > 0 || predict > 0) {
                                longTrades.add(i);
                            }
                        } else {
                            longTrades.add(i);
                        }
                    }
                }
            }

            if (Boolean.FALSE.equals(hasTrendUp)) {
                var candle5 = M5.get(i);
                var tp = (M5.get(i).getClose() / 100) * TP;
                var subList = M5.subList(i - (INDICATORS_SHIFT + 2016), i);
                var atr = calculateATR(subList, 7);
                var hasDownATR = (candle5.getClose() - tp) + (atr - (atr * 0.2)) < startOfDay.getHigh();
                if (hasDownATR) {
                    if (config.getLevelAction(subList, levels).getRight()) {
                        if (useNN) {
                            var data = createInput(M5, i, startOfDay.getClose(), levels);
                            var input = getNetworkInput(data);
                            var output = network.rnnTimeStep(Nd4j.create(input).reshape(1, input.length)).getDouble(0);
                            var labels = getBoosterInput(data);
                            var vector = new DMatrix(labels, 1, labels.length, Float.NaN);
                            var predict = booster.predict(vector)[0][0];
                            if (output < -0 || predict < -0) {
                                shortTrades.add(i);
                            }
                        } else {
                            shortTrades.add(i);
                        }
                    }
                }
            }
        }

        List<TickerCandle> finalM5 = M5;
        if (createPlot) {
            plotChart(name, finalM5, levels, longTrades, shortTrades);
        }
        return calculateTrades(finalM5, longTrades, shortTrades, balance);
    }

    private synchronized static List<TickerCandle> readCandlesFile(String name, String dataDir, String file) {
        var key = name + dataDir + file;
        if (candleRegister.containsKey(key)) {
            return new ArrayList<>(candleRegister.get(key));
        }
        out.println("Read candles file: " + name + "/" + file);
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
                if (values.length < 6 ||
                        values[1].isEmpty() || values[2].isEmpty() ||
                        values[3].isEmpty() || values[4].isEmpty() || values[5].isEmpty()) {
                    System.err.println("Skipping invalid line: " + line);
                    continue;
                }

                try {
                    tickers.add(new TickerCandle(
                            name,
                            values[0],
                            Double.parseDouble(values[1]),
                            Double.parseDouble(values[2]),
                            Double.parseDouble(values[3]),
                            Double.parseDouble(values[4]),
                            Double.parseDouble(values[4]),
                            Integer.parseInt(values[5])));
                } catch (NumberFormatException e) {
                    System.err.println("Number format error in line: " + line);
                    e.printStackTrace();
                }
            }
        } catch (Exception ex) {
            out.println("Error reading file: " + ex.getMessage());
            throw new RuntimeException(ex);
        }
        return candleRegister.put(key, tickers);
    }

    private synchronized static MultiLayerNetwork getNetwork(String dataDir, String name) throws IOException {
        if (multiLayerNetworkRegister.containsKey(name)) {
            return multiLayerNetworkRegister.get(name);
        }
        out.println("Get network: " + name);
        String filePath = dataDir + "/" + name + "/network.nn";
        return multiLayerNetworkRegister.put(name, ModelSerializer.restoreMultiLayerNetwork(filePath));
    }

    private synchronized static Booster getBooster(String dataDir, String name) throws XGBoostError {
        if (boosterRegister.containsKey(name)) {
            return boosterRegister.get(name);
        }
        out.println("Get booster: " + name);
        String filePath = dataDir + "/" + name + "/booster.nn";
        return boosterRegister.put(name, XGBoost.loadModel(filePath));
    }

    private static Result calculateTrades(List<TickerCandle> candles,
            List<Integer> longTrades, List<Integer> shortTrades,
            Double balance) {
        if (longTrades.isEmpty() && shortTrades.isEmpty()) {
            return new Result(balance, 0.0, "");
        }

        double maxDropDown = 0.0;
        double initBalance = balance;
        double minimalBalance = balance;

        int count = 0;
        double cashOpen = 0.0;
        double prevClose = 0.0;
        int winRateCounter = 0;
        int failRateCounter = 0;
        for (int i = 0; i < candles.size(); i++) {
            var cash = ((balance / 100) * RISK);
            if (cash > averagePositionCost) {
                cash = averagePositionCost;
            } else
                continue;

            var close = candles.get(i).getClose();
            if (longTrades.contains(i) && 0 == count) {
                count = (int) (cash / close);
                cashOpen = count * close;
                prevClose = close;

                var prevBalance = balance;
                var commission = round(Math.abs((cashOpen / 100) * COMISSION), 4);
                balance = round(balance - commission, 4);
                if (debugLogging) {
                    out.println(candles.get(i).getDate() + " BUY -" + commission + ", BALANCE: " + prevBalance + " -> "
                            + balance + " (" + round(balance - prevBalance, 4) + ")");
                }
            }
            if (shortTrades.contains(i) && 0 == count) {
                count = (-1) * ((int) (cash / close));
                cashOpen = count * close;
                prevClose = close;

                var prevBalance = balance;
                var commission = round(Math.abs((cashOpen / 100) * COMISSION), 4);
                balance = round(balance - commission, 4);
                if (debugLogging) {
                    out.println(candles.get(i).getDate() + " SELL -" + commission + ", BALANCE: " + prevBalance + " -> "
                            + balance + " (" + round(balance - prevBalance, 4) + ")");
                }
            }

            var min = candles.get(i).getLow();
            var max = candles.get(i).getHigh();
            var longTP = (count > 0 && max >= (prevClose + ((prevClose / 100) * TP)));
            var longSL = (count > 0 && max < (prevClose - ((prevClose / 100) * SL)));
            var shortTP = (count < 0 && min <= (prevClose - ((prevClose / 100) * TP)));
            var shortSL = (count < 0 && min > (prevClose + ((prevClose / 100) * SL)));

            if (longTP || longSL || shortTP || shortSL) {
                var price = close;
                if (longTP)
                    price = (prevClose + ((prevClose / 100) * TP));
                if (shortTP)
                    price = (prevClose - ((prevClose / 100) * TP));
                if (longSL)
                    price = (prevClose - ((prevClose / 100) * SL));
                if (shortSL)
                    price = (prevClose + ((prevClose / 100) * SL));
                var cashClose = count * price;
                var operationResult = round(cashClose - cashOpen, 4);
                if (operationResult > 0)
                    winRateCounter++;
                if (operationResult < 0)
                    failRateCounter++;
                var commission = round(Math.abs((cashClose / 100) * COMISSION), 4);
                var prevBalance = balance;
                var operationResultWithCommission = round(operationResult - commission, 4);
                balance = round(balance + operationResultWithCommission, 4);
                if (balance < initBalance && (balance - initBalance) < maxDropDown) {
                    maxDropDown = balance - initBalance;
                }
                if (balance < minimalBalance) {
                    minimalBalance = balance;
                }

                if (debugLogging) {
                    out.println(candles.get(i).getDate() + " " + (count > 0 ? "SELL: " : "BUY: ") + operationResult
                            + " - " + commission + " = " + operationResultWithCommission + ", BALANCE: " + prevBalance
                            + " -> " + balance + " (" + round(balance - prevBalance, 4) + ")");
                }

                cashOpen = 0.0;
                count = 0;
            }
        }

        var maxDropDownPercent = 100 - ((initBalance - Math.abs(maxDropDown)) / initBalance * 100);
        var messageMaxDropDown = "MaxDropDown: " + df.format(minimalBalance) + " (" + df.format(maxDropDownPercent)
                + "%)";
        var winRatePercent = (double) winRateCounter / (winRateCounter + failRateCounter) * 100;
        var messageWinRate = "WIN/LOSE: " + winRateCounter + "/" + failRateCounter + " (" + df.format(winRatePercent)
                + "%)";
        var statTradesMessage = "LONG/SHORT: " + longTrades.size() + "/" + shortTrades.size();
        var resultMessage = statTradesMessage + ", " + messageWinRate + ", " + messageMaxDropDown;
        return new Result(balance, winRatePercent, resultMessage);
    }

    private static void plotChart(String ticker,
            List<TickerCandle> candles, List<Double> levelValues,
            List<Integer> longTrades, List<Integer> shortTrades) {
        if (longTrades.isEmpty() && shortTrades.isEmpty()) {
            return;
        }

        var max = Double.MIN_VALUE;
        var min = Double.MAX_VALUE;
        XYSeries close = new XYSeries("close");
        List<XYSeries> levels = new ArrayList<>(levelValues.size());
        for (int x = 0; x < levelValues.size(); x++) {
            levels.add(new XYSeries("level_" + x));
        }
        for (int i = 0; i < candles.size(); i++) {
            close.add(i, candles.get(i).getClose());
            for (int x = 0; x < levelValues.size(); x++) {
                levels.get(x).add(i, levelValues.get(x));
            }

            if (candles.get(i).getClose() > max)
                max = candles.get(i).getClose();
            if (candles.get(i).getClose() < min)
                min = candles.get(i).getClose();
        }

        XYSeriesCollection data = new XYSeriesCollection();
        data.addSeries(close);
        levels.forEach(data::addSeries);

        JFreeChart chart = ChartFactory.createXYLineChart(
                ticker,
                "Ticks",
                "ClosePrice",
                data,
                PlotOrientation.VERTICAL,
                true,
                true,
                false);

        for (Integer trade : longTrades) {
            ValueMarker marker = new ValueMarker(trade);
            marker.setPaint(Color.GREEN);
            XYPlot plot = (XYPlot) chart.getPlot();
            plot.addDomainMarker(marker);
        }

        for (Integer trade : shortTrades) {
            ValueMarker marker = new ValueMarker(trade);
            marker.setPaint(Color.RED);
            XYPlot plot = (XYPlot) chart.getPlot();
            plot.addDomainMarker(marker);
        }

        var range = ((NumberAxis) ((XYPlot) chart.getPlot()).getRangeAxis());
        range.setRange(min - (min * 0.1), max + (max * 0.1));

        try {
            deleteIfExists(Path.of("data/" + ticker + "/chart.png"));
            FileOutputStream out = new FileOutputStream("data/" + ticker + "/chart.png");
            ChartUtilities.writeChartAsPNG(out, chart, 1024 * 8, 600 * 8);
        } catch (Exception ex) {
            out.println(ex.getMessage());
        }
    }

    private static boolean isHasTrendUp(List<TickerCandle> candles) {
        boolean hasTrendUp;
        int idx = 0;
        double[] inClose = new double[candles.size()];
        for (TickerCandle candle : candles) {
            inClose[idx++] = candle.getClose();
        }
        var maWhite = IndicatorsUtil.movingAverageWhite(inClose);
        var maBlack = IndicatorsUtil.movingAverageBlack(inClose);
        hasTrendUp = maWhite >= maBlack;
        return hasTrendUp;
    }

    private static double round(double value, int places) {
        long factor = (long) Math.pow(10, places);
        var newValue = value * factor;
        long tmp = Math.round(newValue);
        return (double) tmp / factor;
    }
}
