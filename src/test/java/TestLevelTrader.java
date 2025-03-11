import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.shk0da.GoldenDragon.model.TickerCandle;
import com.github.shk0da.GoldenDragon.model.TickerInfo;
import com.github.shk0da.GoldenDragon.model.TickerJson;
import com.github.shk0da.GoldenDragon.model.TickerType;
import com.github.shk0da.GoldenDragon.repository.Repository;
import com.github.shk0da.GoldenDragon.repository.TickerRepository;
import com.github.shk0da.GoldenDragon.utils.IndicatorsUtil;
import com.google.gson.reflect.TypeToken;
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
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

import static com.github.shk0da.GoldenDragon.repository.TickerRepository.SERIALIZE_NAME;
import static com.github.shk0da.GoldenDragon.utils.SerializationUtils.loadDataFromDisk;
import static java.lang.System.out;
import static java.nio.file.Files.deleteIfExists;
import static java.util.stream.Collectors.toList;

public class TestLevelTrader {

    private static final Boolean ALL_LEVELS = false;
    private static final Double K1 = 0.7;
    private static final Double K2 = 1 - K1;
    private static final Double COMISSION = 0.05;
    private static final Double tpPercent = 0.9;

    private static final ObjectMapper objectMapper = new ObjectMapper();
    private static final DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm:ss");

    public static void main(String[] args) throws Exception {
        Repository<TickerInfo.Key, TickerInfo> tickerRepository = TickerRepository.INSTANCE;
        Map<TickerInfo.Key, TickerInfo> dataFromDisk = loadDataFromDisk(SERIALIZE_NAME, new TypeToken<>() {});
        tickerRepository.putAll(dataFromDisk);

        AtomicReference<Double> balance = new AtomicReference<>(100_000.00);
        List.of("GAZP", "ROSN", "LKOH", "NLMK", "PIKK", "RTKM", "MGNT").forEach(tickerName -> {
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
                balance.set(run(tickerName, currentBalance, tickerInfo.getLevels()));

                var endBalance = balance.get();
                out.println("BALANCE END: " + endBalance);
                out.println("BALANCE DIFF: " + (endBalance - currentBalance));
            } catch (Exception ex) {
                out.println("Skip " + tickerName + ":" + ex.getMessage());
                ex.printStackTrace();
            }
        });
        out.println("\nTOTAL BALANCE: " + balance);
    }

    public static Double run(String name, Double balance, List<Double> levels) throws Exception {
        var network = getNetwork("data", name);
        List<TickerCandle> M5 = readCandlesFile(name, "data", "candlesM5.txt");
        List<TickerCandle> H1 = readCandlesFile(name, "data", "candles.txt");
        List<TickerCandle> D1 = convertCandles(H1, 24, ChronoUnit.HOURS);

        Boolean hasTrendUp = null;
        double atr = calculateATR(D1, 7);
        out.println("ATR: " + atr);

        out.println("LEVELS 1: " + Arrays.toString(levels.toArray(new Double[]{})));

        List<Double> levelValues = new ArrayList<>(levels);
        if (ALL_LEVELS) {
            levelValues.addAll(findSupportAndResistanceLevels(H1.subList(0, (int) (H1.size() * K1)), atr));
        }
        levelValues = levelValues.stream().sorted().collect(toList());
        out.println("LEVELS 2: " + Arrays.toString(levelValues.toArray(new Double[]{})));

        M5 = M5.subList((int) (M5.size() - (M5.size() * K2)), M5.size());
        if (M5.isEmpty()) {
            return balance;
        }

        List<Integer> longTrades = new ArrayList<>();
        List<Integer> shortTrades = new ArrayList<>();
        TickerCandle startOfDay = M5.get(0);

        for (int i = 6, x = 0; i < M5.size(); i++, x++) {
            LocalDateTime currentDateTime = LocalDateTime.parse(M5.get(x).getDate(), formatter);
            LocalDateTime startOfDayDateTime = LocalDateTime.parse(startOfDay.getDate(), formatter);
            if (currentDateTime.getDayOfYear() > startOfDayDateTime.getDayOfYear()) {
                startOfDay = M5.get(x);
            }

            var candle5 = M5.get(i);

            if (x > 80) {
                var subList = M5.subList(x - 80, x);
                hasTrendUp = isHasTrendUp(subList);
            }

            var tp = (candle5.getClose() / 100) * tpPercent;
            var hasUpATR = (startOfDay.getLow() + (atr - (atr * 0.2))) > (candle5.getClose() + tp);

            if (Boolean.TRUE.equals(hasTrendUp) && hasUpATR) {
                var input = getNetworkInput(M5, i, startOfDay.getClose(), levels, atr);
                var output = network.rnnTimeStep(Nd4j.create(input));
                if (output.getDouble(0) > 0.75) {
                    longTrades.add(i);
                }
            }
        }

        List<TickerCandle> finalM5 = M5;
        var buyTrades = new ArrayList<Double>();
        longTrades.forEach(tradeId -> buyTrades.add(finalM5.get(tradeId).getClose()));
        out.println("BUY TRADES: " + Arrays.toString(buyTrades.toArray(new Double[]{})));

        var sellTrades = new ArrayList<Double>();
        shortTrades.forEach(tradeId -> sellTrades.add(finalM5.get(tradeId).getClose()));
        out.println("SELL TRADES: " + Arrays.toString(sellTrades.toArray(new Double[]{})));

        plotChart(name, finalM5, levelValues, longTrades, shortTrades);

        return calculateTrades(finalM5, longTrades, shortTrades, balance, tpPercent);
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

    private static double[] getNetworkInput(List<TickerCandle> stockDataList, int i,
                                            Double startPrice, List<Double> levels, Double atr) {
        var candle5 = stockDataList.get(i - 5); // свеча 5 назад
        var candle4 = stockDataList.get(i - 4); // свеча 4 назад
        var candle3 = stockDataList.get(i - 3); // свеча 3 назад
        var candle2 = stockDataList.get(i - 2); // свеча 2 назад
        var candle1 = stockDataList.get(i - 1); // свеча 1 назад

        var min5 = candle5.getLow(); // лой 5 свечей назад (25 мин)
        var min4 = candle4.getLow(); // лой 4 свечей назад (20 мин)
        var min3 = candle3.getLow(); // лой 3 свечей назад (15 мин)
        var min2 = candle2.getLow(); // лой 2 свечей назад (10 мин)
        var min1 = candle1.getLow(); // лой 1 свечей назад (5 мин)

        var max5 = candle5.getHigh(); // хай 5 свечей назад (25 мин)
        var max4 = candle4.getHigh(); // хай 4 свечей назад (20 мин)
        var max3 = candle3.getHigh(); // хай 3 свечей назад (15 мин)
        var max2 = candle2.getHigh(); // хай 2 свечей назад (10 мин)
        var max1 = candle1.getHigh(); // хай 1 свечей назад (5 мин)

        var volume5 = candle5.getVolume().doubleValue(); // объем 5 свечей назад (25 мин)
        var volume4 = candle4.getVolume().doubleValue(); // объем 4 свечей назад (20 мин)
        var volume3 = candle3.getVolume().doubleValue(); // объем 3 свечей назад (15 мин)
        var volume2 = candle2.getVolume().doubleValue(); // объем 2 свечей назад (10 мин)
        var volume1 = candle1.getVolume().doubleValue(); // объем 1 свечей назад (5 мин)

        var currentPrice = stockDataList.get(i).getClose(); // тек.цена

        var supportLevel = 0.0; // уровень снизу
        var resistanceLevel = 0.0; // уровень сверху
        for (Double level : levels) {
            if (level < currentPrice) {
                supportLevel = level;
            }
            if (level > currentPrice) {
                resistanceLevel = level;
                break;
            }
        }

        var potentialToSupportLevel = currentPrice - supportLevel; // потенциал до уровня снизу
        var potentialToResistanceLevel = resistanceLevel - currentPrice; // потенциал до уровня сверху

        return new double[]{
                startPrice,
                min5, min4, min3, min2, min1,
                max5, max4, max3, max2, max1,
                volume5, volume4, volume3, volume2, volume1,
                currentPrice,
                supportLevel, resistanceLevel,
                atr,
                potentialToSupportLevel, potentialToResistanceLevel
        };
    }

    private static Double calculateTrades(List<TickerCandle> candles,
                                          List<Integer> longTrades, List<Integer> shortTrades,
                                          Double balance, Double tpPercent) {
        if (longTrades.isEmpty() && shortTrades.isEmpty()) {
            return balance;
        }

        int count = 0;
        double cashOpen = 0.0;
        double prevClose = 0.0;
        for (int i = 0; i < candles.size(); i++) {
            var close = candles.get(i).getClose();
            if (longTrades.contains(i)) {
                count = (int) (balance / close);
                cashOpen = count * close;
                prevClose = close;

                var commission = Math.abs((cashOpen / 100) * COMISSION);
                balance = balance - commission;
            }
            if (shortTrades.contains(i)) {
                count = (-1) * ((int) (balance / close));
                cashOpen = count * close;
                prevClose = close;

                var commission = Math.abs((cashOpen / 100) * COMISSION);
                balance = balance - commission;
            }

            var longTP = (count > 0 && close >= (prevClose + ((prevClose / 100) * tpPercent)));
            var longSL = (count > 0 && close < (prevClose - ((prevClose / 100) * tpPercent / 3)));
            var shortTP = (count < 0 && close <= (prevClose - ((prevClose / 100) * tpPercent)));
            var shortSL = (count < 0 && close > (prevClose + ((prevClose / 100) * tpPercent / 3)));

            if (longTP || longSL || shortTP || shortSL) {
                var cashClose = count * close;
                var operationResult = (cashClose - cashOpen);
                out.println((count > 0 ? "BUY: " : "SELL: ") + operationResult);
                balance = balance + operationResult;
                cashOpen = 0.0;
                count = 0;

                var commission = Math.abs((cashClose / 100) * COMISSION);
                balance = balance - commission;
            }
        }
        return balance;
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

    public static List<TickerCandle> convertCandles(List<TickerCandle> candles, long newTimeFrame, ChronoUnit unit) {
        List<TickerCandle> newCandles = new ArrayList<>();
        if (candles.isEmpty()) return newCandles;

        LocalDateTime currentTimestamp = LocalDateTime.parse(candles.get(0).getDate(), formatter);

        double open = 0.0;
        double high = Double.MIN_VALUE;
        double low = Double.MAX_VALUE;
        int volume = 0;

        for (TickerCandle candle : candles) {
            open = 0.0 == open ? candle.getOpen() : open;
            LocalDateTime timestamp = LocalDateTime.parse(candle.getDate(), formatter);
            if (timestamp.isEqual(currentTimestamp.plus(newTimeFrame, unit))) {
                newCandles.add(new TickerCandle(
                        candle.getSymbol(),
                        formatter.format(currentTimestamp),
                        open,
                        high,
                        low,
                        candle.getClose(),
                        candle.getClose(),
                        volume
                ));
            } else if (timestamp.isAfter(currentTimestamp.plus(newTimeFrame, unit))) {
                currentTimestamp = timestamp;//.minusHours(1);
                open = candle.getOpen();
                high = candle.getHigh();
                low = candle.getLow();
            } else {
                high = Math.max(high, candle.getHigh());
                low = Math.min(low, candle.getLow());
                volume += candle.getVolume();
            }
        }
        return newCandles;
    }

    public static double calculateATR(List<TickerCandle> candles, int period) {
        if (candles.size() < period + 1) {
            return Double.MAX_VALUE;
        }

        double atr = 0.0;
        for (TickerCandle candle : candles.subList(candles.size() - period + 1, candles.size() - 1)) {
            atr += candle.getHigh() - candle.getLow();
        }
        return atr / period;
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

    public static List<Double> findSupportAndResistanceLevels(List<TickerCandle> candles, double threshold) {
        List<Double> lows = candles.stream().map(TickerCandle::getLow).collect(Collectors.toList());
        List<Double> highs = candles.stream().map(TickerCandle::getHigh).collect(Collectors.toList());

        Set<Double> supportLevels = new HashSet<>();
        Set<Double> resistanceLevels = new HashSet<>();

        for (int i = 1; i < highs.size() - 1; i++) {
            TickerCandle previousCandle = candles.get(i - 1);
            TickerCandle currentCandle = candles.get(i);
            TickerCandle nextCandle = candles.get(i + 1);

            if (currentCandle.getLow() < previousCandle.getLow() && currentCandle.getLow() < nextCandle.getLow()) {
                double support = currentCandle.getLow();
                int count = 1;

                for (int j = i + 1; j < lows.size(); j++) {
                    if (lows.get(j) < support * (1 + threshold)) {
                        count++;
                    } else {
                        break;
                    }
                }

                if (count >= 3) {
                    supportLevels.add(support);
                }
            }

            if (currentCandle.getHigh() > previousCandle.getHigh() && currentCandle.getHigh() > nextCandle.getHigh()) {
                double resistance = currentCandle.getHigh();
                int count = 1;

                for (int j = i + 1; j < highs.size(); j++) {
                    if (highs.get(j) > resistance * (1 - threshold)) {
                        count++;
                    } else {
                        break;
                    }
                }

                if (count >= 3) {
                    resistanceLevels.add(resistance);
                }
            }
        }

        List<Double> allLevels = new ArrayList<>();
        allLevels.addAll(supportLevels);
        allLevels.addAll(resistanceLevels);

        Set<Double> duplicate = new HashSet<>();
        return allLevels.stream()
                .filter(n -> !duplicate.add(n))
                .sorted()
                .distinct()
                .collect(toList());
    }
}
