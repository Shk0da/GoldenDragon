package com.github.shk0da.GoldenDragon.strategy;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.shk0da.GoldenDragon.config.AILConfig;
import com.github.shk0da.GoldenDragon.model.TickerCandle;
import com.github.shk0da.GoldenDragon.model.TickerInfo;
import com.github.shk0da.GoldenDragon.model.TickerJson;
import com.github.shk0da.GoldenDragon.model.TickerType;
import com.github.shk0da.GoldenDragon.repository.Repository;
import com.github.shk0da.GoldenDragon.repository.TickerRepository;
import com.github.shk0da.GoldenDragon.service.TCSService;
import com.github.shk0da.GoldenDragon.utils.IndicatorsUtil;
import ml.dmlc.xgboost4j.java.Booster;
import ml.dmlc.xgboost4j.java.DMatrix;
import ml.dmlc.xgboost4j.java.XGBoost;
import ml.dmlc.xgboost4j.java.XGBoostError;
import org.deeplearning4j.nn.multilayer.MultiLayerNetwork;
import org.deeplearning4j.util.ModelSerializer;
import org.nd4j.linalg.factory.Nd4j;
import ru.tinkoff.piapi.contract.v1.CandleInterval;
import ru.tinkoff.piapi.contract.v1.HistoricCandle;

import java.io.File;
import java.io.IOException;
import java.sql.Timestamp;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import static com.github.shk0da.GoldenDragon.service.TelegramNotifyService.telegramNotifyService;
import static com.github.shk0da.GoldenDragon.utils.DataLearningUtils.StockDataSetIterator.getBoosterInput;
import static com.github.shk0da.GoldenDragon.utils.DataLearningUtils.StockDataSetIterator.getNetworkInput;
import static com.github.shk0da.GoldenDragon.utils.DataLearningUtils.createInput;
import static com.github.shk0da.GoldenDragon.utils.IndicatorsUtil.INDICATORS_SHIFT;
import static com.github.shk0da.GoldenDragon.utils.IndicatorsUtil.calculateATR;
import static com.github.shk0da.GoldenDragon.utils.IndicatorsUtil.toDouble;
import static com.github.shk0da.GoldenDragon.utils.TimeUtils.sleep;
import static java.lang.System.out;
import static java.time.OffsetDateTime.now;
import static java.util.concurrent.CompletableFuture.allOf;
import static java.util.concurrent.CompletableFuture.runAsync;
import static ru.tinkoff.piapi.contract.v1.CandleInterval.CANDLE_INTERVAL_5_MIN;
import static ru.tinkoff.piapi.contract.v1.CandleInterval.CANDLE_INTERVAL_HOUR;

public class AITrader {

    private static final ObjectMapper objectMapper = new ObjectMapper();
    private static final DecimalFormat decimalFormat = new DecimalFormat("#.##");
    private static final DateFormat dateTimeFormat = new SimpleDateFormat("dd.MM.yyyy HH:mm:ss");
    private static final DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm:ss");
    private static final Repository<TickerInfo.Key, TickerInfo> tickerRepository = TickerRepository.INSTANCE;

    private final TCSService tcsService;
    private final AILConfig ailConfig;

    private final Double tpPercent;
    private final Double slPercent;
    private final Double balanceRiskPercent;
    private final Double averagePositionCost;

    private final AtomicInteger winCounter = new AtomicInteger(0);
    private final AtomicInteger loseCounter = new AtomicInteger(0);

    public AITrader(AILConfig ailConfig, TCSService tcsService) {
        this.tcsService = tcsService;
        this.ailConfig = ailConfig;
        this.tpPercent = ailConfig.getTpPercent();
        this.slPercent = ailConfig.getSlPercent();
        this.balanceRiskPercent = ailConfig.getBalanceRiskPercent();
        this.averagePositionCost = ailConfig.getAveragePositionCost();
    }

    public static class TickerDayInfo {

        public static final Map<String, TickerDayInfo> register = new ConcurrentHashMap<>();

        private final String name;
        private final TickerCandle startOfDay;
        private final Double atr;
        private final TickerJson tickerJson;
        private final MultiLayerNetwork network;
        private final Booster booster;

        public TickerDayInfo(String name, TickerCandle startOfDay, Double atr, TickerJson tickerJson,
                             MultiLayerNetwork network, Booster booster) {
            this.name = name;
            this.startOfDay = startOfDay;
            this.atr = atr;
            this.tickerJson = tickerJson;
            this.network = network;
            this.booster = booster;
        }

        public String getName() {
            return name;
        }

        public TickerCandle getStartOfDay() {
            return startOfDay;
        }

        public Double getAtr() {
            return atr;
        }

        public TickerJson getTickerJson() {
            return tickerJson;
        }

        public MultiLayerNetwork getNetwork() {
            return network;
        }

        public Booster getBooster() {
            return booster;
        }
    }

    public void run() {
        var initPortfolioCost = tcsService.getTotalPortfolioCost();
        var infoMessage = "Total Portfolio Cost: " + initPortfolioCost;
        telegramNotifyService.sendMessage(infoMessage);
        out.println(infoMessage);

        List<CompletableFuture<Void>> tasks = new ArrayList<>();
        ExecutorService executor = Executors.newFixedThreadPool(ailConfig.getStocks().size());
        for (String name : ailConfig.getStocks()) {
            tasks.add(
                    runAsync(() -> {
                        while (isWorkingHours()) {
                            handleTicker(name);
                            sleep(30_000);
                        }
                    }, executor)
            );
            sleep(1_000);
        }
        allOf(tasks.toArray(new CompletableFuture[]{})).join();
        if (!isWorkingHours()) {
            var buyMessage = "Not working hours! Current Time: " + new Date() + ".\n";
            telegramNotifyService.sendMessage(buyMessage);
            out.println(buyMessage);

            shutdownExecutor(executor);
            tcsService.closeAllByMarket(TickerType.STOCK);

            var profit = tcsService.getTotalPortfolioCost() - initPortfolioCost;
            var profitInPercents = (tcsService.getTotalPortfolioCost() - initPortfolioCost) / initPortfolioCost * 100;
            var statsMessage = "Day profit: " + decimalFormat.format(profit) + "(" + profitInPercents + "%).\n";

            if (winCounter.get() > 0 && loseCounter.get() > 0) {
                var winRatePercent = (double) winCounter.get() / (winCounter.get() + loseCounter.get()) * 100;
                statsMessage += "Wins/Lose: " + winCounter.get() + "/" + loseCounter.get() + " (" + decimalFormat.format(winRatePercent) + "%).\n";
            }

            telegramNotifyService.sendMessage(statsMessage);
            out.println(statsMessage);
        }
    }

    private boolean isTradingHours() {
        var calendar = new GregorianCalendar();
        var hour = calendar.get(Calendar.HOUR_OF_DAY);
        return !(hour >= 18);
    }

    private boolean isWorkingHours() {
        var calendar = new GregorianCalendar();
        var hour = calendar.get(Calendar.HOUR_OF_DAY);
        var minute = calendar.get(Calendar.MINUTE);
        return !(hour == 18 && minute >= 30 || hour >= 19);
    }

    private void handleTicker(String name) {
        try {
            var tickerDayInfo = TickerDayInfo.register.computeIfAbsent(name, it -> {
                var weekCandles = getTickerCandles(name, (INDICATORS_SHIFT + 2419), CANDLE_INTERVAL_HOUR, 0);
                return new TickerDayInfo(
                        name,
                        findStartOfDay(weekCandles),
                        calculateATR(weekCandles, 7),
                        readTickerFile(name, ailConfig.getDataDir()),
                        getNetwork(name, ailConfig.getDataDir()),
                        getBooster(name, ailConfig.getDataDir())
                );
            });
            var decision = decision(tickerDayInfo);
            var type = tickerDayInfo.getTickerJson().getTicker().getType();
            var currentPosition = tcsService.getCurrentPositions(type, name);
            var currentPositionBalance = null == currentPosition ? 0.0 : currentPosition.getBalance();
            if (0 != decision && isTradingHours()) {
                if (0 == currentPositionBalance) {
                    var balance = tcsService.getAvailableCash();
                    var cashToOrder = (balance / 100) * balanceRiskPercent;
                    if (cashToOrder > averagePositionCost) {
                        cashToOrder = averagePositionCost;
                    } else return;

                    var sl = 0.0;
                    if (ailConfig.isSlEnabled() && ailConfig.isSlAuto()) {
                        sl = slPercent;
                    }

                    var tp = 0.0;
                    if (ailConfig.isTpEnabled() && ailConfig.isTpAuto()) {
                        tp = tpPercent;
                    }

                    if (decision > 0) {
                        tcsService.buyByMarket(name, type, cashToOrder, tp, sl);
                    }
                    if (decision < 0) {
                        tcsService.sellByMarket(name, type, cashToOrder, tp, sl);
                    }
                }
            }

            if (0 != currentPositionBalance && (ailConfig.isSlEnabled() || ailConfig.isTpEnabled())) {
                var count = currentPosition.getBalance();
                var expectedYield = currentPosition.getExpectedYield();
                var positionPrice = currentPosition.getAveragePositionPrice();
                out.printf("%s: Yield=%,.2f%s\n", name, expectedYield, "%");
                if (currentPositionBalance > 0 && (expectedYield > tpPercent || expectedYield < ((-1) * slPercent))) {
                    var currentPrice = tcsService.getAvailablePrice(name, type, count, "bids");
                    expectedYield = (currentPrice - positionPrice) / positionPrice * 100;
                    var expectedYieldMessage = name + ": " + String.format("%,.2f%s", expectedYield, "%");
                    if (expectedYield > tpPercent && ailConfig.isTpEnabled() && !ailConfig.isTpAuto()) {
                        var closeMessage = "LONG TP " + expectedYieldMessage;
                        out.println(closeMessage + "\n");
                        tcsService.closeLongByMarket(name, type);
                        telegramNotifyService.sendMessage(closeMessage);
                        winCounter.incrementAndGet();
                    }
                    if (expectedYield < (-1) * slPercent && ailConfig.isSlEnabled() && !ailConfig.isSlAuto()) {
                        var closeMessage = "LONG SL " + expectedYieldMessage;
                        out.println(closeMessage);
                        tcsService.closeLongByMarket(name, type);
                        telegramNotifyService.sendMessage(closeMessage);
                        loseCounter.incrementAndGet();
                    }
                }
                if (currentPositionBalance < 0 && (expectedYield > tpPercent || expectedYield < ((-1) * slPercent))) {
                    var currentPrice = tcsService.getAvailablePrice(name, type, count, "asks");
                    expectedYield = (positionPrice - currentPrice) / currentPrice * 100;
                    var expectedYieldMessage = name + ": " + String.format("%,.2f%s", expectedYield, "%");
                    if (expectedYield > tpPercent && ailConfig.isTpEnabled() && !ailConfig.isTpAuto()) {
                        var closeMessage = "SHORT TP " + expectedYieldMessage;
                        out.println(closeMessage);
                        tcsService.closeShortByMarket(name, type);
                        telegramNotifyService.sendMessage(closeMessage);
                        winCounter.incrementAndGet();
                    }
                    if (expectedYield < (-1) * slPercent && ailConfig.isSlEnabled() && !ailConfig.isSlAuto()) {
                        var closeMessage = "SHORT SL " + expectedYieldMessage;
                        out.println(closeMessage);
                        tcsService.closeShortByMarket(name, type);
                        telegramNotifyService.sendMessage(closeMessage);
                        loseCounter.incrementAndGet();
                    }
                }
            }
        } catch (Exception ex) {
            var message = "Failed handle " + name + ": " + ex.getMessage();
            telegramNotifyService.sendMessage(message);
            out.println(message);
            ex.printStackTrace();
        }
    }

    private int decision(TickerDayInfo tickerDayInfo) throws Exception {
        var startOfDay = tickerDayInfo.getStartOfDay();
        var atr = tickerDayInfo.getAtr();
        var levels = tickerDayInfo.getTickerJson().getLevels();
        var network = tickerDayInfo.getNetwork();
        var booster = tickerDayInfo.getBooster();
        var hasTrendUp = isHasTrendUp(tickerDayInfo.getName());

        var candles = getTickerCandles(tickerDayInfo.getName(), (INDICATORS_SHIFT + 1008), CANDLE_INTERVAL_5_MIN, 0);
        var lastCandle = candles.get(candles.size() - 1);
        var tp = (lastCandle.getClose() / 100) * tpPercent;

        if (Boolean.TRUE.equals(hasTrendUp)) {
            var hasUpATR = (startOfDay.getLow() + (atr - (atr * 0.2))) > (lastCandle.getClose() + tp);
            if (hasUpATR) {
                var data = createInput(candles, candles.size() - 1, startOfDay.getClose(), levels);
                var input = getNetworkInput(data);
                var output = network.rnnTimeStep(Nd4j.create(input)).getDouble(0);
                var labels = getBoosterInput(data);
                var vector = new DMatrix(labels, 1, labels.length, Float.NaN);
                var predict = booster.predict(vector)[0][0];
                if ((output + predict) >= ailConfig.getSumOfDecision()
                        && (output > ailConfig.getSensitivityLong()
                        || predict > ailConfig.getBoosterSensitivityLong())) {
                    return 1;
                }
            }
        }

        if (Boolean.FALSE.equals(hasTrendUp)) {
            var hasDownATR = (lastCandle.getClose() - tp) + (atr - (atr * 0.2)) < startOfDay.getHigh();
            if (hasDownATR) {
                var data = createInput(candles, candles.size() - 1, startOfDay.getClose(), levels);
                var input = getNetworkInput(data);
                var output = network.rnnTimeStep(Nd4j.create(input)).getDouble(0);
                var labels = getBoosterInput(data);
                var vector = new DMatrix(labels, 1, labels.length, Float.NaN);
                var predict = booster.predict(vector)[0][0];
                if ((output + predict) <= ((-1) * ailConfig.getSumOfDecision())
                        && (output < ((-1) * ailConfig.getSensitivityShort())
                        || predict < ((-1) * ailConfig.getBoosterSensitivityShort()))) {
                    return -1;
                }
            }
        }

        return 0;
    }

    private static TickerJson readTickerFile(String name, String dataDir) {
        out.println("Read ticker file: " + name);
        try {
            return objectMapper.readValue(new File(dataDir + "/" + name + "/ticker.json"), TickerJson.class);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    private static MultiLayerNetwork getNetwork(String name, String dataDir) {
        out.println("Get network: " + name);
        String filePath = dataDir + "/" + name + "/network.nn";
        try {
            return ModelSerializer.restoreMultiLayerNetwork(filePath);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    private static Booster getBooster(String name, String dataDir) {
        out.println("Get booster: " + name);
        String filePath = dataDir + "/" + name + "/booster.nn";
        try {
            return XGBoost.loadModel(filePath);
        } catch (XGBoostError e) {
            throw new RuntimeException(e);
        }
    }

    private List<TickerCandle> getTickerCandles(String name, int size, CandleInterval period, int counter) {
        sleep(1_550);
        List<TickerCandle> candles = new ArrayList<>();
        try {
            var currentTime = now();
            var ticker = tickerRepository.getAll().values().stream()
                    .filter(it -> it.getType().equals(TickerType.STOCK))
                    .filter(it -> it.getName().equalsIgnoreCase(name) || it.getTicker().equalsIgnoreCase(name))
                    .map(TickerInfo::getFigi)
                    .findFirst()
                    .orElseThrow();
            var periodCandles = tcsService.getCandles(
                    ticker,
                    currentTime.minusMinutes(size * 5L),
                    currentTime,
                    period
            );

            if (periodCandles.isEmpty()) {
                throw new RuntimeException("tcsService.getCandles return is empty");
            }

            for (HistoricCandle candle : periodCandles) {
                var dateTime = new Timestamp(candle.getTime().getSeconds() * 1000);
                var open = toDouble(candle.getOpen());
                var high = toDouble(candle.getHigh());
                var low = toDouble(candle.getLow());
                var close = toDouble(candle.getClose());
                var volume = candle.getVolume();
                candles.add(
                        new TickerCandle(
                                name,
                                dateTimeFormat.format(dateTime),
                                open,
                                high,
                                low,
                                close,
                                close,
                                (int) volume
                        )
                );
            }
        } catch (Exception ex) {
            out.println("Failed getTickerCandles: " + ex.getMessage());
            if (counter++ < 2) {
                sleep(1_200);
                return getTickerCandles(name, size, period, counter);
            } else {
                throw new RuntimeException(ex);
            }
        }
        return candles;
    }

    private boolean isHasTrendUp(String tickerName) {
        int idx = 0;
        var candles = getTickerCandles(tickerName, 80 * (60/5), CANDLE_INTERVAL_HOUR, 0);
        double[] inClose = new double[candles.size()];
        for (TickerCandle candle : candles) {
            inClose[idx++] = candle.getClose();
        }
        var maWhite = IndicatorsUtil.movingAverageWhite(inClose);
        var maBlack = IndicatorsUtil.movingAverageBlack(inClose);
        return maWhite >= maBlack;
    }

    private static TickerCandle findStartOfDay(List<TickerCandle> candles) {
        TickerCandle startOfDay = candles.get(candles.size() - 1);
        for (int i = candles.size() - 1; i >= 0; i--) {
            var candle = candles.get(i);
            LocalDateTime startOfDayDateTime = LocalDateTime.parse(startOfDay.getDate(), formatter);
            LocalDateTime currentDateTime = LocalDateTime.parse(candle.getDate(), formatter);
            if (currentDateTime.getDayOfYear() < startOfDayDateTime.getDayOfYear()) {
                startOfDay = candles.get(i + 1);
                break;
            }
        }
        return startOfDay;
    }

    private static void shutdownExecutor(ExecutorService executor) {
        try {
            if (!executor.awaitTermination(5, TimeUnit.SECONDS)) {
                executor.shutdownNow();
            }
        } catch (InterruptedException skip) {
            executor.shutdownNow();
        }
    }
}
