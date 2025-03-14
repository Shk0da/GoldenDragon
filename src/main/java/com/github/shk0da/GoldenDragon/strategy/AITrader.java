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
import org.deeplearning4j.nn.multilayer.MultiLayerNetwork;
import org.deeplearning4j.util.ModelSerializer;
import org.nd4j.linalg.factory.Nd4j;
import ru.tinkoff.piapi.contract.v1.CandleInterval;
import ru.tinkoff.piapi.contract.v1.HistoricCandle;

import java.io.File;
import java.io.IOException;
import java.sql.Timestamp;
import java.text.DateFormat;
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
import java.util.function.Supplier;

import static com.github.shk0da.GoldenDragon.service.TelegramNotifyService.telegramNotifyService;
import static com.github.shk0da.GoldenDragon.utils.DataLearningUtils.StockDataSetIterator.getNetworkInput;
import static com.github.shk0da.GoldenDragon.utils.IndicatorsUtil.INDICATORS_SHIFT;
import static com.github.shk0da.GoldenDragon.utils.IndicatorsUtil.calculateATR;
import static com.github.shk0da.GoldenDragon.utils.IndicatorsUtil.toDouble;
import static java.lang.System.out;
import static java.time.OffsetDateTime.now;
import static java.util.concurrent.CompletableFuture.allOf;
import static java.util.concurrent.CompletableFuture.runAsync;
import static ru.tinkoff.piapi.contract.v1.CandleInterval.CANDLE_INTERVAL_5_MIN;
import static ru.tinkoff.piapi.contract.v1.CandleInterval.CANDLE_INTERVAL_HOUR;

public class AITrader {

    private static final Double tpPercent = 0.9;
    private static final Double slPercent = 0.3;
    private static final Double balanceRiskPercent = 10.0;

    private static final ObjectMapper objectMapper = new ObjectMapper();
    private static final DateFormat dateTimeFormat = new SimpleDateFormat("dd.MM.yyyy HH:mm:ss");
    private static final DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm:ss");
    private static final Repository<TickerInfo.Key, TickerInfo> tickerRepository = TickerRepository.INSTANCE;

    private final TCSService tcsService;
    private final AILConfig ailConfig;

    public AITrader(AILConfig ailConfig, TCSService tcsService) {
        this.tcsService = tcsService;
        this.ailConfig = ailConfig;
    }

    public static class TickerDayInfo {

        public static final Map<String, TickerDayInfo> register = new ConcurrentHashMap<>();

        private final String name;
        private final TickerCandle startOfDay;
        private final Double atr;
        private final TickerJson tickerJson;
        private final MultiLayerNetwork network;

        public TickerDayInfo(String name, TickerCandle startOfDay, Double atr, TickerJson tickerJson, MultiLayerNetwork network) {
            this.name = name;
            this.startOfDay = startOfDay;
            this.atr = atr;
            this.tickerJson = tickerJson;
            this.network = network;
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
    }

    public void run() {
        out.println("Total Portfolio Cost: " + tcsService.getTotalPortfolioCost());
        List<CompletableFuture<Void>> tasks = new ArrayList<>();
        ExecutorService executor = Executors.newFixedThreadPool(ailConfig.getStocks().size());
        Supplier<Boolean> isNotWorkingHours = () -> {
            var calendar = new GregorianCalendar();
            var hour = calendar.get(Calendar.HOUR_OF_DAY);
            var minute = calendar.get(Calendar.MINUTE);
            return ((hour == 18 && minute >= 30) || (hour >= 19)) || (hour < 9 || (hour == 9 && minute < 45));
        };
        for (String name : ailConfig.getStocks()) {
            tasks.add(
                    runAsync(() -> {
                        while (!isNotWorkingHours.get()) {
                            handleTicker(name);
                            sleep(30_000);
                        }
                    }, executor)
            );
            sleep(1_000);
        }
        allOf(tasks.toArray(new CompletableFuture[]{})).join();
        if (isNotWorkingHours.get()) {
            executor.shutdown();
            tcsService.closeAllByMarket(TickerType.STOCK);
            out.println("Not working hours! Current Time: " + new Date() + ". Exit...");
        }
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
                        getNetwork(name, ailConfig.getDataDir())
                );
            });
            var decision = decision(tickerDayInfo);
            var type = tickerDayInfo.getTickerJson().getTicker().getType();
            var currentPosition = tcsService.getCurrentPositions(type, name);
            var currentPositionBalance = null == currentPosition ? 0.0 : currentPosition.getBalance();
            if (0 != decision) {
                if (0 == currentPositionBalance) {
                    var balance = tcsService.getAvailableCash();
                    var cashToOrder = (balance / 100) * balanceRiskPercent;
                    if (decision > 0) {
                        tcsService.buyByMarket(name, type, cashToOrder, tpPercent, slPercent);
                    }
                    if (decision < 0) {
                        tcsService.sellByMarket(name, type, cashToOrder, tpPercent, slPercent);
                    }
                }
            }

            if (0 != currentPositionBalance) {
                var count = currentPosition.getBalance();
                var expectedYield = currentPosition.getExpectedYield();
                var positionPrice = currentPosition.getAveragePositionPrice();
                if (currentPositionBalance > 0) {
                    var currentPrice = tcsService.getAvailablePrice(name, type, count, "asks");
                    expectedYield = (currentPrice - positionPrice) / positionPrice * 100;
                    if (expectedYield > tpPercent || expectedYield < slPercent) {
                        tcsService.closeLongByMarket(name, type);
                    }
                }
                if (currentPositionBalance < 0) {
                    var currentPrice = tcsService.getAvailablePrice(name, type, count, "bids");
                    expectedYield =  (positionPrice - currentPrice) / currentPrice * 100;
                    if (expectedYield > tpPercent || expectedYield < slPercent) {
                        tcsService.closeShortByMarket(name, type);
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

    private int decision(TickerDayInfo tickerDayInfo) {
        var startOfDay = tickerDayInfo.getStartOfDay();
        var atr = tickerDayInfo.getAtr();
        var levels = tickerDayInfo.getTickerJson().getLevels();
        var network = tickerDayInfo.getNetwork();

        var candles = getTickerCandles(tickerDayInfo.getName(), (INDICATORS_SHIFT + 1008), CANDLE_INTERVAL_5_MIN, 0);
        var hasTrendUp = isHasTrendUp(candles);
        var lastCandle = candles.get(candles.size() - 1);
        var tp = (lastCandle.getClose() / 100) * tpPercent;

        if (Boolean.TRUE.equals(hasTrendUp)) {
            var hasUpATR = (startOfDay.getLow() + (atr - (atr * 0.2))) > (lastCandle.getClose() + tp);
            if (hasUpATR) {
                var input = getNetworkInput(candles, candles.size() - 1, startOfDay.getClose(), levels);
                var output = network.rnnTimeStep(Nd4j.create(input));
                if (output.getDouble(0) > 0.03) {
                    return 1;
                }
            }
        }

        if (Boolean.FALSE.equals(hasTrendUp)) {
            var hasDownATR = (lastCandle.getClose() - tp) + (atr - (atr * 0.2)) < startOfDay.getHigh();
            if (hasDownATR) {
                var input = getNetworkInput(candles, candles.size() - 1, startOfDay.getClose(), levels);
                var output = network.rnnTimeStep(Nd4j.create(input));
                if (output.getDouble(0) < -0.01) {
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

    private List<TickerCandle> getTickerCandles(String name, int size, CandleInterval period, int counter) {
        try {
            TimeUnit.MILLISECONDS.sleep(1550);
        } catch (InterruptedException ex) {
            out.println("Error: " + ex.getMessage());
        }
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

    private static void sleep(long time) {
        try {
            TimeUnit.MILLISECONDS.sleep(time);
        } catch (InterruptedException skip) {
            // nothing
        }
    }
}
