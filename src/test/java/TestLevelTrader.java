import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.shk0da.GoldenDragon.config.AILConfig;
import com.github.shk0da.GoldenDragon.model.TickerCandle;
import com.github.shk0da.GoldenDragon.model.TickerInfo;
import com.github.shk0da.GoldenDragon.model.TickerJson;
import com.github.shk0da.GoldenDragon.model.TickerType;
import com.github.shk0da.GoldenDragon.repository.Repository;
import com.github.shk0da.GoldenDragon.repository.TickerRepository;
import com.github.shk0da.GoldenDragon.service.TelegramNotifyService;
import com.github.shk0da.GoldenDragon.strategy.DataLearning;
import com.github.shk0da.GoldenDragon.utils.IndicatorsUtil;
import com.google.gson.reflect.TypeToken;
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
import java.nio.file.Path;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;

import static com.github.shk0da.GoldenDragon.repository.TickerRepository.SERIALIZE_NAME;
import static com.github.shk0da.GoldenDragon.utils.DataLearningUtils.StockDataSetIterator.getNetworkInput;
import static com.github.shk0da.GoldenDragon.utils.IndicatorsUtil.INDICATORS_SHIFT;
import static com.github.shk0da.GoldenDragon.utils.IndicatorsUtil.calculateATR;
import static com.github.shk0da.GoldenDragon.utils.SerializationUtils.loadDataFromDisk;
import static java.lang.System.out;
import static java.nio.file.Files.deleteIfExists;

public class TestLevelTrader {

    private static final Double K1 = 0.7;
    private static final Double K2 = 1 - K1;
    private static final Double COMISSION = 0.05;
    private static final Boolean createPlot = false;
    private static final Boolean debugLogging = false;
    private static final Boolean needLearn = false;
    private static final Double initBalance = 100_000.00;

    private static final ObjectMapper objectMapper = new ObjectMapper();
    private static final DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm:ss");
    public static final TelegramNotifyService telegramNotifyService = new TelegramNotifyService();

    private static AILConfig configGenerator() throws IOException {
        AILConfig ailConfig = new AILConfig();
        ailConfig.setTest(true);
        ailConfig.setStocks(List.of("GAZP", "LKOH", "MGNT", "NLMK", "PIKK", "ROSN", "RTKM", "SBER"));
        ailConfig.setBalanceRiskPercent(30.0);
        ailConfig.setAveragePositionCost(10_000.0);
        ailConfig.setSensitivityLong(0.0);
        ailConfig.setSensitivityShort(0.0);
        return ailConfig;
    }

    public static void main(String[] args) throws Exception {
        Repository<TickerInfo.Key, TickerInfo> tickerRepository = TickerRepository.INSTANCE;
        Map<TickerInfo.Key, TickerInfo> dataFromDisk = loadDataFromDisk(SERIALIZE_NAME, new TypeToken<>() {});
        tickerRepository.putAll(dataFromDisk);
        var ailConfig = configGenerator();
        if (needLearn) {
            new DataLearning(ailConfig).run();
        }
        ailConfig.getStocks().forEach(tickerName -> {
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
                var result = run(tickerName, currentBalance, tickerInfo.getLevels(), ailConfig);
                balance.set(result.getLeft());

                var endBalance = balance.get();
                out.println("PROFIT: " + (endBalance - currentBalance));
                out.println("BALANCE END: " + endBalance);
                telegramNotifyService.sendMessage(
                        "Test [" + tickerName + "] | " + "Profit: " + (endBalance - currentBalance) + ", " + result.getRight()
                );
            } catch (Exception ex) {
                out.println("Skip " + tickerName + ":" + ex.getMessage());
                ex.printStackTrace();
            }
        });
    }

    public static Pair<Double, String> run(String name, double balance, List<Double> levels, AILConfig ailConfig) throws Exception {
        List<TickerCandle> M5 = readCandlesFile(name, "data", "candles5_MIN.txt");
        M5 = M5.subList((int) (M5.size() - (M5.size() * K2)), M5.size());
        if (M5.isEmpty()) {
            return Pair.of(balance, "");
        }

        Boolean hasTrendUp = null;
        List<Integer> longTrades = new ArrayList<>();
        List<Integer> shortTrades = new ArrayList<>();
        TickerCandle startOfDay = M5.get(0);
        var network = getNetwork("data", name);
        for (int i = INDICATORS_SHIFT + 2016, x = 0; i < M5.size(); i++, x++) {
            LocalDateTime currentDateTime = LocalDateTime.parse(M5.get(x).getDate(), formatter);
            LocalDateTime startOfDayDateTime = LocalDateTime.parse(startOfDay.getDate(), formatter);
            if (currentDateTime.getDayOfYear() > startOfDayDateTime.getDayOfYear()) {
                startOfDay = M5.get(x);
            }

            if (x > 80) {
                var subList = M5.subList(x - 80, x);
                hasTrendUp = isHasTrendUp(subList);
            }

            if (Boolean.TRUE.equals(hasTrendUp)) {
                var candle5 = M5.get(i);
                var tp = (M5.get(i).getClose() / 100) * ailConfig.getTpPercent();
                var subList = M5.subList(i - (INDICATORS_SHIFT + 2016), i);
                var atr = calculateATR(subList, 7);
                var hasUpATR = (startOfDay.getLow() + (atr - (atr * 0.2))) > (candle5.getClose() + tp);
                if (hasUpATR) {
                    var input = getNetworkInput(M5, i, startOfDay.getClose(), levels);
                    var output = network.rnnTimeStep(Nd4j.create(input));
                    if (output.getDouble(0) > ailConfig.getSensitivityLong()) {
                        longTrades.add(i);
                    }
                }
            }

            if (Boolean.FALSE.equals(hasTrendUp)) {
                var candle5 = M5.get(i);
                var tp = (M5.get(i).getClose() / 100) * ailConfig.getTpPercent();
                var subList = M5.subList(i - (INDICATORS_SHIFT + 2016), i);
                var atr = calculateATR(subList, 7);
                var hasDownATR = (candle5.getClose() - tp) + (atr - (atr * 0.2)) < startOfDay.getHigh();
                if (hasDownATR) {
                    var input = getNetworkInput(M5, i, startOfDay.getClose(), levels);
                    var output = network.rnnTimeStep(Nd4j.create(input));
                    if (output.getDouble(0) < ((-1) * ailConfig.getSensitivityShort())) {
                        shortTrades.add(i);
                    }
                }
            }
        }

        List<TickerCandle> finalM5 = M5;
        if (createPlot) {
            plotChart(name, finalM5, levels, longTrades, shortTrades);
        }
        return calculateTrades(finalM5, longTrades, shortTrades, balance, ailConfig);
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

    private static Pair<Double, String> calculateTrades(List<TickerCandle> candles,
                                                        List<Integer> longTrades, List<Integer> shortTrades,
                                                        Double balance, AILConfig ailConfig) {
        if (longTrades.isEmpty() && shortTrades.isEmpty()) {
            return Pair.of(balance, "");
        }

        int count = 0;
        double cashOpen = 0.0;
        double prevClose = 0.0;
        int winRateCounter = 0;
        int failRateCounter = 0;
        for (int i = 0; i < candles.size(); i++) {
            var cash = ((balance / 100) * ailConfig.getBalanceRiskPercent());
            if (cash > ailConfig.getAveragePositionCost()) {
                cash = ailConfig.getAveragePositionCost();
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
            var longTP = (count > 0 && max >= (prevClose + ((prevClose / 100) * ailConfig.getTpPercent())));
            var longSL = (count > 0 && max < (prevClose - ((prevClose / 100) * ailConfig.getSlPercent())));
            var shortTP = (count < 0 && min <= (prevClose - ((prevClose / 100) * ailConfig.getTpPercent())));
            var shortSL = (count < 0 && min > (prevClose + ((prevClose / 100) * ailConfig.getSlPercent())));

            if (longTP || longSL || shortTP || shortSL) {
                var price = close;
                if (longTP) price = (prevClose + ((prevClose / 100) * ailConfig.getTpPercent()));
                if (shortTP) price = (prevClose - ((prevClose / 100) * ailConfig.getTpPercent()));
                if (longSL) price = (prevClose - ((prevClose / 100) * ailConfig.getSlPercent()));
                if (shortSL) price = (prevClose + ((prevClose / 100) * ailConfig.getSlPercent()));
                var cashClose = count * price;
                var operationResult = round(cashClose - cashOpen, 4);
                if (operationResult > 0) winRateCounter++;
                if (operationResult < 0) failRateCounter++;
                var commission = round(Math.abs((cashClose / 100) * COMISSION), 4);
                var prevBalance = balance;
                var operationResultWithCommission = round(operationResult - commission, 4);
                balance = round(balance + operationResultWithCommission, 4);

                if (debugLogging) {
                    out.println(candles.get(i).getDate() + " " + (count > 0 ? "SELL: " : "BUY: ") + operationResult + " - " + commission + " = " + operationResultWithCommission + ", BALANCE: " + prevBalance + " -> " + balance + " (" + round(balance - prevBalance, 4) + ")");
                }

                cashOpen = 0.0;
                count = 0;
            }
        }
        var messageWinRate = "WIN/LOSE: " + winRateCounter + "/" + failRateCounter;
        out.println(messageWinRate);
        return Pair.of(balance, messageWinRate);
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
