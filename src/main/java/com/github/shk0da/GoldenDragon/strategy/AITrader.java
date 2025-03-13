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
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import static com.github.shk0da.GoldenDragon.utils.DataLearningUtils.StockDataSetIterator.getNetworkInput;
import static com.github.shk0da.GoldenDragon.utils.IndicatorsUtil.INDICATORS_SHIFT;
import static com.github.shk0da.GoldenDragon.utils.IndicatorsUtil.calculateATR;
import static com.github.shk0da.GoldenDragon.utils.IndicatorsUtil.toDouble;
import static java.lang.System.out;
import static java.time.OffsetDateTime.now;
import static ru.tinkoff.piapi.contract.v1.CandleInterval.CANDLE_INTERVAL_5_MIN;

public class AITrader {

    private static final Double tpPercent = 0.9;
    private static final Double slPercent = 0.3;
    private static final Double balanceRiskPercent = 30.0;

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

        public static final Map<String, TickerDayInfo> register = new HashMap<>();

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
        while (true) {
            for (String name : ailConfig.getStocks()) {
                try {
                    var tickerDayInfo = TickerDayInfo.register.computeIfAbsent(name, it -> {
                        var weekCandles = getTickerCandles(name, (INDICATORS_SHIFT + 2016), 0); // week
                        return new TickerDayInfo(
                                name,
                                findStartOfDay(weekCandles),
                                calculateATR(weekCandles, 7),
                                readTickerFile(name, ailConfig.getDataDir()),
                                getNetwork(name, ailConfig.getDataDir())
                        );
                    });
                    var decision = decision(tickerDayInfo);
                    if (decision > 0) {
                        var type = tickerDayInfo.getTickerJson().getTicker().getType();
                        var currentPosition = tcsService.getCountOfCurrentPositions(type, name);
                        if (!(currentPosition > 0)) {
                            var balance = tcsService.getAvailableCash();
                            var cashToBuy = (balance / 100) * balanceRiskPercent;
                            tcsService.buyByMarket(name, type, cashToBuy, tpPercent, slPercent);
                        }
                    }
                    if (decision < 0) {
                        // TBD
                    }
                } catch (Exception ex) {
                    out.println("Failed handle " + name + ": " + ex.getMessage());
                    ex.printStackTrace();
                }
                try {
                    TimeUnit.MILLISECONDS.sleep(1000);
                } catch (InterruptedException skip) {
                    // nothing
                }
            }

            var currentCalendar = new GregorianCalendar();
            int currentHour = currentCalendar.get(Calendar.HOUR_OF_DAY);
            if (currentHour >= 18) {
                int currentMinute = currentCalendar.get(Calendar.MINUTE);
                out.println("Not working hours! Current Time: " + currentHour + ":" + currentMinute + ". Exit...");
                break;
            }
        }
    }

    private int decision(TickerDayInfo tickerDayInfo) {
        var startOfDay = tickerDayInfo.getStartOfDay();
        var atr = tickerDayInfo.getAtr();
        var levels = tickerDayInfo.getTickerJson().getLevels();
        var network = tickerDayInfo.getNetwork();

        var candles = getTickerCandles(tickerDayInfo.getName(), INDICATORS_SHIFT, 0);
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

    private List<TickerCandle> getTickerCandles(String name, int size, int counter) {
        List<TickerCandle> candles = new ArrayList<>();
        try {
            var currentTime = now();
            var ticker = tickerRepository.getAll().values().stream()
                    .filter(it -> it.getType().equals(TickerType.STOCK))
                    .filter(it -> it.getName().equalsIgnoreCase(name) || it.getTicker().equalsIgnoreCase(name))
                    .map(TickerInfo::getFigi)
                    .findFirst()
                    .orElseThrow();
            var stock = tcsService.getMoexShares().stream()
                    .filter(it -> ticker.equals(it.getFigi()))
                    .findFirst()
                    .orElseThrow();
            var m5candles = tcsService.getCandles(
                    stock.getFigi(),
                    currentTime.minusMinutes(size * 5L),
                    currentTime,
                    CANDLE_INTERVAL_5_MIN
            );
            for (HistoricCandle candle : m5candles) {
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
            if (counter++ < 2) {
                try {
                    TimeUnit.MILLISECONDS.sleep(100);
                } catch (InterruptedException skip) {
                    // nothing
                }
                return getTickerCandles(name, size, counter);
            } else {
                out.println(ex.getMessage());
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
}
