import com.github.shk0da.GoldenDragon.model.TickerCandle;
import com.github.shk0da.GoldenDragon.model.TickerInfo;
import com.github.shk0da.GoldenDragon.model.TickerType;
import com.github.shk0da.GoldenDragon.repository.Repository;
import com.github.shk0da.GoldenDragon.repository.TickerRepository;
import com.github.shk0da.GoldenDragon.utils.IndicatorsUtil;
import com.google.gson.reflect.TypeToken;
import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartUtilities;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.ValueMarker;
import org.jfree.chart.plot.XYPlot;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;

import java.awt.*;
import java.io.FileOutputStream;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.Statement;
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
import java.util.stream.Stream;

import static com.github.shk0da.GoldenDragon.utils.SerializationUtils.loadDataFromDisk;
import static java.lang.System.out;
import static java.util.stream.Collectors.toList;

public class TestLevelTrader {

    public static void main(String[] args) throws Exception {
        Repository<TickerInfo.Key, TickerInfo> tickerRepository = TickerRepository.INSTANCE;
        Map<TickerInfo.Key, TickerInfo> dataFromDisk = loadDataFromDisk(TickerRepository.SERIALIZE_NAME, new TypeToken<>() {
        });
        tickerRepository.putAll(dataFromDisk);

        AtomicReference<Double> balance = new AtomicReference<>(100_000.00);
        List.of("ROSN", "LKOH", "NLMK", "SBER", "PIKK", "RTKM", "MGNT").forEach(tickerName -> {
            String name = tickerName.toLowerCase();
            String ticker = tickerRepository.getAll().values().stream()
                    .filter(it -> it.getType().equals(TickerType.STOCK))
                    .filter(it -> it.getName().toLowerCase().contains(name) || it.getTicker().toLowerCase().contains(name))
                    .map(TickerInfo::getFigi)
                    .findFirst()
                    .orElseThrow();
            balance.set(run(ticker, balance.get()));
            out.println("BALANCE: " + balance);
        });
        out.println("\nTOTAL BALANCE: " + balance);
    }

    public static Double run(String ticker, Double balance) {
        out.println("\nTICKER: " + ticker);
        Double tpPercent = 0.9;
        List<TickerCandle> M5 = getTickerCandles(ticker, "M5");
        List<TickerCandle> H1 = getTickerCandles(ticker, "H1");
        List<TickerCandle> D1 = convertCandles(H1, 24, ChronoUnit.HOURS);

        Boolean hasTrendUp = null;
        double atr = calculateATR(D1, 7);
        out.println("ATR: " + atr);

        List<Double> levelValues = findSupportAndResistanceLevels(H1.subList(0, (int) (H1.size() * 0.5)), atr);
        out.println("LEVELS: " + Arrays.toString(levelValues.toArray(new Double[]{})));

        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");

        M5 = M5.subList((int) (M5.size() - (M5.size() * 0.5)), M5.size());
        if (M5.isEmpty()) {
            return balance;
        }

        List<Integer> longTrades = new ArrayList<>();
        List<Integer> shortTrades = new ArrayList<>();
        TickerCandle startOfDay = M5.get(0);

        for (int i = 5, x = 0; i < M5.size(); i++, x++) {
            LocalDateTime currentDateTime = LocalDateTime.parse(M5.get(x).getDate(), formatter);
            LocalDateTime startOfDayDateTime = LocalDateTime.parse(startOfDay.getDate(), formatter);
            if (currentDateTime.getDayOfYear() > startOfDayDateTime.getDayOfYear()) {
                startOfDay = M5.get(x);
            }

            var candle1 = M5.get(i - 4);
            var candle2 = M5.get(i - 3);
            var candle3 = M5.get(i - 2);
            var candle4 = M5.get(i - 1);
            var candle5 = M5.get(i);

            if (x > 80) {
                var subList = M5.subList(x - 80, x);
                hasTrendUp = isHasTrendUp(subList);
            }

            var tp = (candle5.getClose() / 100) * tpPercent;
            var hasUpATR = (startOfDay.getLow() + (atr - (atr * 0.2))) > (candle5.getClose() + tp);
            var hasDownATR = (candle5.getClose() - tp) + (atr - (atr * 0.2)) < startOfDay.getHigh();

            if (Boolean.TRUE.equals(hasTrendUp) && hasUpATR) {
                var barSize1 = candle1.getHigh() - candle1.getLow();
                var barSize2 = candle2.getHigh() - candle2.getLow();
                var barSize3 = candle3.getHigh() - candle3.getLow();
                var barSize4 = candle4.getHigh() - candle4.getLow();

                var hasResistLevel = true;
                hasResistLevel &= candle1.getLow() <= candle2.getLow() && barSize2 <= barSize1;
                hasResistLevel &= candle2.getLow() <= candle3.getLow() && barSize3 <= barSize2;
                hasResistLevel &= candle3.getLow() <= candle4.getLow() && barSize4 <= barSize3;

                if (hasResistLevel) {
                    var resistLevel = Stream.of(
                                    candle1.getHigh(),
                                    candle2.getHigh(),
                                    candle3.getHigh(),
                                    candle4.getHigh()
                            )
                            .filter(it -> {
                                var str = it.toString();
                                return str.charAt(str.length() - 1) == '0';
                            })
                            .mapToDouble(Double::doubleValue)
                            .summaryStatistics()
                            .getMax();
                    var levelTest = candle5.getLow() == resistLevel;
                    if (levelTest) {
                        longTrades.add(i);
                    }
                }
            }

            if (Boolean.FALSE.equals(hasTrendUp) && hasDownATR) {
                var barSize1 = candle1.getHigh() - candle1.getLow();
                var barSize2 = candle2.getHigh() - candle2.getLow();
                var barSize3 = candle3.getHigh() - candle3.getLow();
                var barSize4 = candle4.getHigh() - candle4.getLow();

                var hasResistLevel = true;
                hasResistLevel &= candle1.getHigh() >= candle2.getHigh() && barSize2 <= barSize1;
                hasResistLevel &= candle2.getHigh() >= candle3.getHigh() && barSize3 <= barSize2;
                hasResistLevel &= candle3.getHigh() >= candle4.getHigh() && barSize4 <= barSize3;

                if (hasResistLevel) {
                    Set<Double> duplicate = new HashSet<>();
                    var resistLevel = Stream.of(
                                    candle1.getLow(),
                                    candle2.getLow(),
                                    candle3.getLow(),
                                    candle4.getLow()
                            )
                            .filter(it -> {
                                var str = it.toString();
                                return str.charAt(str.length() - 1) == '0';
                            })
                            .filter(n -> !duplicate.add(n))
                            .mapToDouble(Double::doubleValue)
                            .summaryStatistics()
                            .getMin();
                    var levelTest = candle5.getHigh() == resistLevel;
                    Double nextSupportLevel = null;
                    for (int y = levelValues.size() - 1; y >= 0; y--) {
                        if (levelValues.get(y) < resistLevel) {
                            nextSupportLevel = levelValues.get(y);
                            break;
                        }
                    }
                    var hasRangeATR = null != nextSupportLevel && (resistLevel - nextSupportLevel) > tp;
                    if (levelTest && hasRangeATR) {
                        shortTrades.add(i);
                    }
                }
            }
        }

        var allTrades = new ArrayList<Integer>();
        allTrades.addAll(longTrades);
        allTrades.addAll(shortTrades);
        out.println("TRADES: " + Arrays.toString(allTrades.toArray(new Integer[]{})));

        //List<TickerCandle> finalM = M5;
        //runAsync(() -> plotChart(ticker, finalM, levelValues, longTrades, shortTrades));

        return calculateTrades(M5, longTrades, shortTrades, balance, tpPercent);
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

                var commission = Math.abs((cashOpen / 100) * 0.05);
                balance = balance - commission;
            }
            if (shortTrades.contains(i)) {
                count = (-1) * ((int) (balance / close));
                cashOpen = count * close;
                prevClose = close;

                var commission = Math.abs((cashOpen / 100) * 0.05);
                balance = balance - commission;
            }

            var longTP = (count > 0 && close >= (prevClose + ((prevClose / 100) * tpPercent)));
            var longSL = (count > 0 && close < (prevClose - ((prevClose / 100) * tpPercent / 3)));
            var shortTP = (count < 0 && close <= (prevClose - ((prevClose / 100) * tpPercent)));
            var shortSL = (count < 0 && close > (prevClose + ((prevClose / 100) * tpPercent / 3)));

            if (longTP || longSL || shortTP || shortSL) {
                var cashClose = count * close;
                balance = balance + (cashClose - cashOpen);
                cashOpen = 0.0;
                count = 0;

                var commission = Math.abs((cashClose / 100) * 0.05);
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
            marker.setPaint(Color.BLACK);
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
            FileOutputStream out = new FileOutputStream("plots/" + ticker + ".png");
            ChartUtilities.writeChartAsPNG(out, chart, 1024, 600);
        } catch (Exception ex) {
            out.println(ex.getMessage());
        }
    }

    private static List<TickerCandle> getTickerCandles(String ticker, String period) {
        List<TickerCandle> candles = new ArrayList<>();
        try (Connection con = DriverManager.getConnection(
                "jdbc:postgresql://localhost:5432/postgres", "postgres", "postgres"
        )) {
            Statement stmt = con.createStatement();
            String selectSql = "SELECT * FROM candle WHERE tf = '" + period + "' AND name = '" + ticker + "' ORDER BY date_time";
            try (ResultSet resultSet = stmt.executeQuery(selectSql)) {
                while (resultSet.next()) {
                    TickerCandle candle = new TickerCandle(
                            resultSet.getString("name"),
                            resultSet.getString("date_time"),
                            resultSet.getDouble("open"),
                            resultSet.getDouble("high"),
                            resultSet.getDouble("low"),
                            resultSet.getDouble("close"),
                            resultSet.getDouble("close"),
                            resultSet.getInt("volume")
                    );
                    candles.add(candle);
                }
            }
        } catch (Exception ex) {
            out.println(ex.getMessage());
        }
        return candles;
    }

    public static List<TickerCandle> convertCandles(List<TickerCandle> candles, long newTimeFrame, ChronoUnit unit) {
        List<TickerCandle> newCandles = new ArrayList<>();
        if (candles.isEmpty()) return newCandles;

        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
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
                .filter(it -> {
                    var str = it.toString();
                    return str.charAt(str.length() - 1) == '0';
                })
                .filter(n -> !duplicate.add(n))
                .sorted()
                .distinct()
                .collect(toList());
    }
}
