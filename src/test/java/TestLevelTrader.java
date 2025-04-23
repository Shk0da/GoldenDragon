import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.shk0da.GoldenDragon.model.TickerCandle;
import com.github.shk0da.GoldenDragon.model.TickerInfo;
import com.github.shk0da.GoldenDragon.model.TickerJson;
import com.github.shk0da.GoldenDragon.model.TickerType;
import com.github.shk0da.GoldenDragon.repository.Repository;
import com.github.shk0da.GoldenDragon.repository.TickerRepository;
import com.github.shk0da.GoldenDragon.service.TelegramNotifyService;
import com.github.shk0da.GoldenDragon.utils.GerchikUtils;
import com.github.shk0da.GoldenDragon.utils.IndicatorsUtil;
import com.google.gson.GsonBuilder;
import com.google.gson.reflect.TypeToken;
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

import java.awt.*;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.file.Path;
import java.text.DecimalFormat;
import java.time.Duration;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;

import static com.github.shk0da.GoldenDragon.repository.TickerRepository.SERIALIZE_NAME;
import static com.github.shk0da.GoldenDragon.utils.DataLearningUtils.StockDataSetIterator.getBoosterInput;
import static com.github.shk0da.GoldenDragon.utils.DataLearningUtils.StockDataSetIterator.getNetworkInput;
import static com.github.shk0da.GoldenDragon.utils.DataLearningUtils.createInput;
import static com.github.shk0da.GoldenDragon.utils.IndicatorsUtil.INDICATORS_SHIFT;
import static com.github.shk0da.GoldenDragon.utils.IndicatorsUtil.calculateATR;
import static com.github.shk0da.GoldenDragon.utils.IndicatorsUtil.convertCandles;
import static com.github.shk0da.GoldenDragon.utils.SerializationUtils.loadDataFromDisk;
import static com.github.shk0da.GoldenDragon.utils.TimeUtils.sleep;
import static java.lang.System.out;
import static java.nio.file.Files.deleteIfExists;
import static java.time.OffsetDateTime.now;

public class TestLevelTrader {

    private static final Double K2 = 0.05;
    private static final Double COMISSION = 0.05;
    private static final Double TP = 0.9;
    private static final Double SL = 0.3;
    private static final Double RISK = 30.0;
    private static final Boolean createPlot = false;
    private static final Boolean debugLogging = false;
    private static final Double initBalance = 100_000.00;
    private static final Double averagePositionCost = 10_000.00;
    private static final List<String> stocks = List.of("GAZP", "LKOH", "MGNT", "NLMK", "PIKK", "ROSN", "RTKM", "SBER");

    private static final DecimalFormat df = new DecimalFormat("#.##");
    private static final ObjectMapper objectMapper = new ObjectMapper();
    private static final DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm:ss");
    private static final TelegramNotifyService telegramNotifyService = new TelegramNotifyService();

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

    public static void main(String[] args) throws Exception {
        // [
        //  [
        //    1499040000000,      // Kline open time
        //    "0.01634790",       // Open price
        //    "0.80000000",       // High price
        //    "0.01575800",       // Low price
        //    "0.01577100",       // Close price
        //    "148976.11427815",  // Volume
        //    1499644799999,      // Kline close time
        //    "2434.19055334",    // Quote asset volume
        //    308,                // Number of trades
        //    "1756.87402397",    // Taker buy base asset volume
        //    "28.46694368",      // Taker buy quote asset volume
        //    "0"                 // Unused field. Ignore.
        //  ]
        //]

        // https://github.com/binance/binance-spot-api-docs/blob/master/rest-api.md
        // https://api.binance.com/api/v3/klines?symbol=BTCUSDT&interval=1h&startTime=1499040000000&limit=100
        getTickerCandles("BTCUSDT", "1h", Date.from(Instant.now().minus(1, ChronoUnit.DAYS)), 0);
    }


    private static final HttpClient httpClient = HttpClient.newHttpClient();
    private static List<TickerCandle> getTickerCandles(String name, String period, Date start, int counter) {
        Set<TickerCandle> candles = new LinkedHashSet<>();
        try {
            Instant currentTime = now().toInstant();
            Instant startTime = start.toInstant();
            while (startTime.isBefore(currentTime)) {
                var end = startTime.plus(1, ChronoUnit.DAYS);
                out.println("Loading: " + name + "[" + start + " -> " + end + "]");

                var url = "https://api.binance.com/api/v3/klines?symbol="+name+"&interval="+period+"&startTime="+startTime.toEpochMilli()+"&endTime="+end.toEpochMilli()+"&limit=100";
                HttpRequest historyRequest = HttpRequest.newBuilder()
                        .uri(URI.create(url))
                        .timeout(Duration.of(10, ChronoUnit.SECONDS))
                        .GET()
                        .build();
                HttpResponse<String> response = httpClient.send(historyRequest, HttpResponse.BodyHandlers.ofString());
                startTime = end;

                List<Map<String, String>> example = new ArrayList<Map<String, String>>();
                var content = new GsonBuilder().create().fromJson(response.body(), example.getClass());

                out.println(content);

                /*periodCandles.forEach(candle -> {
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
                });*/
                sleep(100);
            }
        } catch (Exception ex) {
            if (counter++ < 2) {
                return getTickerCandles(name, period, start, counter);
            } else {
                out.println(ex.getMessage());
            }
        }
        return new ArrayList<>(candles);
    }


    public static void main2(String[] args) throws Exception {
        var bestResult = 0.0D;
        var bestProfit = 0.0D;
        var bestConfig = new GerchikUtils();

        int _levelConfirmationTouches = 0;
        double _levelZonePercent = 0.0075;
        double _breakoutConfirmationPercent = 0.01;
        double _falseBreakoutThreshold = 0.00025;
        double _volumeMultiplier = 0.65;
        int _confirmationCandles = 3;

        {
            var config = new GerchikUtils(
                    _levelConfirmationTouches,
                    _levelZonePercent,
                    _breakoutConfirmationPercent,
                    _falseBreakoutThreshold,
                    _volumeMultiplier,
                    _confirmationCandles
            );
            var result = run(config);
            if (result.getLeft() > bestResult) {
                bestResult = result.getLeft();
                bestConfig = config;
                out.println(bestResult + "%: " + config);
                telegramNotifyService.sendMessage(df.format(bestResult) + "% (" + df.format(result.getRight()) + " RUB): " + config);
            }
            if (result.getRight() > bestProfit) {
                bestProfit = result.getRight();
                bestConfig = config;
                out.println(bestProfit + " RUB: " + config);
                telegramNotifyService.sendMessage(df.format(bestProfit) + " RUB (" + df.format(result.getLeft()) + "%): " + config);
            }
        }
        out.println("Finish test. Best result:" + df.format(bestProfit) + " RUB (" + df.format(bestResult) + "%): " + bestConfig);
        telegramNotifyService.sendMessage("Finish test. Best result:" + df.format(bestProfit) + " RUB (" + df.format(bestResult) + "%): " + bestConfig);
    }

    public static Pair<Double, Double> run(GerchikUtils config) throws Exception {
        Repository<TickerInfo.Key, TickerInfo> tickerRepository = TickerRepository.INSTANCE;
        Map<TickerInfo.Key, TickerInfo> dataFromDisk = loadDataFromDisk(SERIALIZE_NAME, new TypeToken<>() {});
        tickerRepository.putAll(dataFromDisk);
        List<Double> results = new ArrayList<>(stocks.size());
        List<Double> profits = new ArrayList<>(stocks.size());
        stocks.forEach(tickerName -> {
            AtomicReference<Double> balance = new AtomicReference<>(initBalance);
            try {
                String name = tickerName.toLowerCase();
                String ticker = tickerRepository.getAll().values().stream()
                        .filter(it -> it.getType().equals(TickerType.STOCK))
                        .filter(it -> it.getName().equalsIgnoreCase(name) || it.getTicker().equalsIgnoreCase(name))
                        .map(TickerInfo::getFigi)
                        .findFirst()
                        .orElseThrow();

                out.println("\nTICKER: " + tickerName + " (" + ticker + ")");
                var currentBalance = balance.get();
                out.println("BALANCE START: " + currentBalance);
                var tickerInfo = readTickerFile(tickerName, "data");
                var result = run(tickerName, currentBalance, tickerInfo.getLevels(), config);
                balance.set(result.getProfit());

                var endBalance = balance.get();
                out.println("PROFIT: " + (endBalance - currentBalance));
                out.println("BALANCE END: " + endBalance);
                out.println(
                        "Test [" + tickerName + "] | " + "Profit: " + df.format(endBalance - currentBalance) + ", " + result.getMessage()
                );
                results.add(result.getWinrate());
                profits.add(result.getProfit());
            } catch (Exception ex) {
                out.println("Skip " + tickerName + ":" + ex.getMessage());
                ex.printStackTrace();
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

        var network = getNetwork("data", name);
        var booster = getBooster("data", name);

        for (int i = INDICATORS_SHIFT + 2016, x = 0; i < M5.size(); i++, x++) {
            LocalDateTime currentDateTime = LocalDateTime.parse(M5.get(x).getDate(), formatter);
            LocalDateTime startOfDayDateTime = LocalDateTime.parse(startOfDay.getDate(), formatter);
            if (currentDateTime.getDayOfYear() > startOfDayDateTime.getDayOfYear()) {
                startOfDay = M5.get(x);
            }

            if (x > 80 * (60 / 5)) {
                var subList = M5.subList(x - 80 * (60 / 5), x);
                List<TickerCandle> H1 = convertCandles(subList, 1, ChronoUnit.HOURS);
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
                        var data = createInput(M5, i, startOfDay.getClose(), levels);
                        var input = getNetworkInput(data);
                        var output = network.rnnTimeStep(Nd4j.create(input)).getDouble(0);
                        var labels = getBoosterInput(data);
                        var vector = new DMatrix(labels, 1, labels.length, Float.NaN);
                        var predict = booster.predict(vector)[0][0];
                        if ((output + predict) >= 0.05 && (output > 0.035 || predict > 0.005)) {
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
                        var data = createInput(M5, i, startOfDay.getClose(), levels);
                        var input = getNetworkInput(data);
                        var output = network.rnnTimeStep(Nd4j.create(input)).getDouble(0);
                        var labels = getBoosterInput(data);
                        var vector = new DMatrix(labels, 1, labels.length, Float.NaN);
                        var predict = booster.predict(vector)[0][0];
                        if ((output + predict) <= -0.05 && (output < -0.005 || predict < -0.005)) {
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

    private static TickerJson readTickerFile(String name, String dataDir) throws Exception {
        out.println("Read ticker file: " + name);
        return objectMapper.readValue(new File(dataDir + "/" + name + "/ticker.json"), TickerJson.class);
    }

    private static List<TickerCandle> readCandlesFile(String name, String dataDir, String file) {
        out.println("Read candles file: " + name + "/" + file);
        List<TickerCandle> tickers = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader(dataDir + "/" + name + "/" + file))) {
            boolean skipHeader = true;
            String line = br.readLine();
            while (line != null) {
                if (skipHeader) {
                    skipHeader = false;
                    line = br.readLine();
                    continue;
                }

                String[] values = line.split(",");
                tickers.add(new TickerCandle(
                        name,
                        values[0],
                        Double.valueOf(values[1]),
                        Double.valueOf(values[2]),
                        Double.valueOf(values[3]),
                        Double.valueOf(values[4]),
                        Double.valueOf(values[4]),
                        Integer.valueOf(values[5])
                ));
                line = br.readLine();
            }
        } catch (Exception ex) {
            out.println(ex.getMessage());
            throw new RuntimeException(ex);
        }
        return tickers;
    }

    private static MultiLayerNetwork getNetwork(String dataDir, String name) throws IOException {
        out.println("Get network: " + name);
        String filePath = dataDir + "/" + name + "/network.nn";
        return ModelSerializer.restoreMultiLayerNetwork(filePath);
    }

    private static Booster getBooster(String dataDir, String name) throws XGBoostError {
        out.println("Get booster: " + name);
        String filePath = dataDir + "/" + name + "/booster.nn";
        return XGBoost.loadModel(filePath);
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
            } else continue;

            var close = candles.get(i).getClose();
            if (longTrades.contains(i) && 0 == count) {
                count = (int) (cash / close);
                cashOpen = count * close;
                prevClose = close;

                var prevBalance = balance;
                var commission = round(Math.abs((cashOpen / 100) * COMISSION), 4);
                balance = round(balance - commission, 4);
                if (debugLogging) {
                    out.println(candles.get(i).getDate() + " BUY -" + commission + ", BALANCE: " + prevBalance + " -> " + balance + " (" + round(balance - prevBalance, 4) + ")");
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
                    out.println(candles.get(i).getDate() + " SELL -" + commission + ", BALANCE: " + prevBalance + " -> " + balance + " (" + round(balance - prevBalance, 4) + ")");
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
                if (longTP) price = (prevClose + ((prevClose / 100) * TP));
                if (shortTP) price = (prevClose - ((prevClose / 100) * TP));
                if (longSL) price = (prevClose - ((prevClose / 100) * SL));
                if (shortSL) price = (prevClose + ((prevClose / 100) * SL));
                var cashClose = count * price;
                var operationResult = round(cashClose - cashOpen, 4);
                if (operationResult > 0) winRateCounter++;
                if (operationResult < 0) failRateCounter++;
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
                    out.println(candles.get(i).getDate() + " " + (count > 0 ? "SELL: " : "BUY: ") + operationResult + " - " + commission + " = " + operationResultWithCommission + ", BALANCE: " + prevBalance + " -> " + balance + " (" + round(balance - prevBalance, 4) + ")");
                }

                cashOpen = 0.0;
                count = 0;
            }
        }

        var maxDropDownPercent = 100 - ((initBalance - Math.abs(maxDropDown)) / initBalance * 100);
        var messageMaxDropDown = "MaxDropDown: " + df.format(minimalBalance) + " (" + df.format(maxDropDownPercent) + "%)";
        var winRatePercent = (double) winRateCounter / (winRateCounter + failRateCounter) * 100;
        var messageWinRate = "WIN/LOSE: " + winRateCounter + "/" + failRateCounter + " (" + df.format(winRatePercent) + "%)";
        var statTradesMessage = "LONG/SHORT: " + longTrades.size() + "/" + shortTrades.size();
        var resultMessage = statTradesMessage + ", " + messageWinRate + ", " + messageMaxDropDown;
        out.println(resultMessage);
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

            if (candles.get(i).getClose() > max) max = candles.get(i).getClose();
            if (candles.get(i).getClose() < min) min = candles.get(i).getClose();
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
                false
        );

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
