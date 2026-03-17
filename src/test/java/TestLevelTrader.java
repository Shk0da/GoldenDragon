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
import java.nio.file.Paths;
import java.text.DecimalFormat;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;
import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartUtilities;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.ValueMarker;
import org.jfree.chart.plot.XYPlot;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;


import static com.github.shk0da.GoldenDragon.repository.TickerRepository.SERIALIZE_NAME;
import static com.github.shk0da.GoldenDragon.utils.IndicatorsUtil.INDICATORS_SHIFT;
import static com.github.shk0da.GoldenDragon.utils.IndicatorsUtil.calculateATR;
import static com.github.shk0da.GoldenDragon.utils.IndicatorsUtil.convertCandles;
import static com.github.shk0da.GoldenDragon.utils.SerializationUtils.loadDataFromDisk;
import static java.lang.System.out;
import static java.nio.file.Files.deleteIfExists;

public class TestLevelTrader {

    private static final double K2 = 0.1, COMISSION = 0.05, TP = 0.9, SL = 0.3, RISK = 30.0;
    private static final double INIT_BALANCE = 100_000.0, AVG_POS_COST = 10_000.0;
    private static final boolean CREATE_PLOT = false, DEBUG = false;
    private static final int MIN_PARAM = 1, MAX_PARAM = 20;
    private static final List<String> STOCKS = Collections.unmodifiableList(
            Arrays.asList("CNYRUBF"/*, "USDRUBF", "HEAD", "LKOH", "MTSS", "PLZL", "RTKM", "SBER"*/));

    private static final DecimalFormat DF = new DecimalFormat("#.##");
    private static final DateTimeFormatter FMT = DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm:ss");
    private static final Map<String, List<TickerCandle>> CANDLE_CACHE = new ConcurrentHashMap<>();
    private static final Map<String, List<Double>> LEVELS_CACHE = new ConcurrentHashMap<>();

    static final class Params {
        final int touches;
        final int candles;

        Params(int touches, int candles) {
            this.touches = touches;
            this.candles = candles;
        }

        @Override
        public String toString() {
            return "touches=" + touches + ", candles=" + candles;
        }
    }

    static final class Score implements Comparable<Score> {
        final Params params;
        final double winRate;
        final double profit;

        Score(Params params, double winRate, double profit) {
            this.params = params;
            this.winRate = winRate;
            this.profit = profit;
        }

        @Override
        public int compareTo(Score o) {
            int c = Double.compare(o.profit, this.profit);
            return c != 0 ? c : Double.compare(o.winRate, this.winRate);
        }
    }

    static final class Result {
        final double profit;
        final double winRate;
        final String message;

        Result(double profit, double winRate, String message) {
            this.profit = profit;
            this.winRate = winRate;
            this.message = message;
        }
    }

    static final class Pair<L, R> {
        final L left;
        final R right;

        Pair(L l, R r) {
            left = l;
            right = r;
        }

        L getLeft() {
            return left;
        }

        R getRight() {
            return right;
        }
    }

    public static void main(String[] args) {
        Repository<Key, TickerInfo> repo = TickerRepository.INSTANCE;
        Map<TickerInfo.Key, TickerInfo> dataFromDisk = loadDataFromDisk(
                SERIALIZE_NAME, new TypeToken<Map<TickerInfo.Key, TickerInfo>>() {
                });
        repo.putAll(dataFromDisk);

        int total = (MAX_PARAM - MIN_PARAM + 1) * (MAX_PARAM - MIN_PARAM + 1);
        out.printf("Полный перебор: %d комбинаций (%d..%d x %d..%d)%n%n",
                total, MIN_PARAM, MAX_PARAM, MIN_PARAM, MAX_PARAM);

        List<Params> allParams = new ArrayList<>(total);
        for (int t = MIN_PARAM; t <= MAX_PARAM; t++)
            for (int c = MIN_PARAM; c <= MAX_PARAM; c++)
                allParams.add(new Params(t, c));

        long startTime = System.currentTimeMillis();

        List<Score> results = allParams.parallelStream().map(p -> {
            GerchikUtils config = new GerchikUtils(p.touches, p.candles);
            Pair<Double, Double> r = evaluate(config);
            return new Score(p, r.getLeft(), r.getRight());
        }).sorted().collect(Collectors.toList());

        long elapsed = System.currentTimeMillis() - startTime;

        out.printf("%n========== РЕЗУЛЬТАТЫ (%d комбинаций за %.1f сек) ==========%n%n",
                total, elapsed / 1000.0);
        out.println("Top-10:");
        results.stream().limit(10).forEach(s ->
                out.printf("  %s → WinRate: %.2f%%, Profit: %.2f RUB%n",
                        s.params, s.winRate, s.profit));

        Score best = results.get(0);
        out.printf("%nЛучшая конфигурация: %s%n", best.params);
        out.printf("WinRate: %.2f%%%n", best.winRate);
        out.printf("Profit: %.2f RUB%n", best.profit);

        out.println("\n========== Подробный прогон лучшей конфигурации ==========\n");
        GerchikUtils bestConfig = new GerchikUtils(best.params.touches, best.params.candles);
        for (String name : STOCKS) {
            try {
                List<Double> levels = getLevels(name.toLowerCase());
                Result result = runBacktest(name, INIT_BALANCE, levels, bestConfig);
                out.printf("%-10s: Balance=%.2f, %s%n", name, result.profit, result.message);
            } catch (Exception e) {
                out.printf("%-10s: ERROR - %s%n", name, e.getMessage());
            }
        }
    }

    static Pair<Double, Double> evaluate(GerchikUtils config) {
        List<Double> winRates = new ArrayList<>();
        List<Double> profits = new ArrayList<>();
        for (String name : STOCKS) {
            try {
                List<Double> levels = getLevels(name.toLowerCase());
                Result result = runBacktest(name, INIT_BALANCE, levels, config);
                winRates.add(result.winRate);
                profits.add(result.profit);
            } catch (Exception ignored) {
            }
        }
        double wr = winRates.isEmpty() ? 0 : winRates.stream().mapToDouble(d -> d).average().orElse(0);
        double pr = profits.isEmpty() ? 0 : profits.stream().mapToDouble(d -> d).average().orElse(0);
        return new Pair<>(wr, pr);
    }

    static List<Double> getLevels(String name) {
        return LEVELS_CACHE.computeIfAbsent(name, n ->
                new LevelUtils().identifyKeyLevels(readCandles(n.toUpperCase(), "data", "candlesHOUR.txt"))
                        .stream().map(Level::getPrice).sorted().collect(Collectors.toList()));
    }

    static Result runBacktest(String name, double balance, List<Double> levels,
                              GerchikUtils config) throws Exception {
        List<TickerCandle> full = readCandles(name, "data", "candles5_MIN.txt");
        List<TickerCandle> M5 = full.subList((int) (full.size() - full.size() * K2), full.size());
        if (M5.isEmpty()) return new Result(balance, 0.0, "");

        Boolean hasTrendUp = null;
        List<Integer> longs = new ArrayList<>();
        List<Integer> shorts = new ArrayList<>();
        TickerCandle startOfDay = M5.get(0);
        int minLookback = INDICATORS_SHIFT + 2016;
        int h1Lookback = 80 * 12;

        for (int i = minLookback; i < M5.size(); i++) {
            LocalDateTime cur = LocalDateTime.parse(M5.get(i).getDate(), FMT);
            LocalDateTime sod = LocalDateTime.parse(startOfDay.getDate(), FMT);
            if (cur.getDayOfYear() > sod.getDayOfYear() || cur.getYear() > sod.getYear()) {
                startOfDay = M5.get(i);
            }

            if (i >= h1Lookback) {
                try {
                    List<TickerCandle> h1 = convertCandles(
                            M5.subList(i - h1Lookback, i), 1, ChronoUnit.HOURS);
                    if (!h1.isEmpty()) hasTrendUp = trendUp(h1);
                } catch (Exception ignored) {
                }
            }

            TickerCandle candle = M5.get(i);
            double tp = candle.getClose() / 100 * TP;
            List<TickerCandle> sub = M5.subList(i - minLookback, i);
            double atr = calculateATR(sub, 7);

            if (Boolean.TRUE.equals(hasTrendUp)
                    && (startOfDay.getLow() + atr * 0.8) > (candle.getClose() + tp)) {
                if (config.getLevelAction(sub, levels).isLong()) longs.add(i);
            }
            if (Boolean.FALSE.equals(hasTrendUp)
                    && (candle.getClose() - tp) + atr * 0.8 < startOfDay.getHigh()) {
                if (config.getLevelAction(sub, levels).isShort()) shorts.add(i);
            }
        }

        if (CREATE_PLOT) plotChart(name, M5, levels, longs, shorts);
        return calcTrades(M5, longs, shorts, balance);
    }

    static Result calcTrades(List<TickerCandle> candles, List<Integer> longs,
                             List<Integer> shorts, double balance) {
        if (longs.isEmpty() && shorts.isEmpty()) return new Result(balance, 0.0, "");
        Set<Integer> longSet = new HashSet<>(longs);
        Set<Integer> shortSet = new HashSet<>(shorts);
        double initBal = balance, minBal = balance, maxDD = 0;
        int count = 0, wins = 0, losses = 0;
        double cashOpen = 0, prevClose = 0;

        for (int i = 0; i < candles.size(); i++) {
            double cash = balance / 100 * RISK;
            if (cash > AVG_POS_COST) cash = AVG_POS_COST;
            else continue;
            double close = candles.get(i).getClose();

            if (count == 0 && longSet.contains(i)) {
                count = (int) (cash / close);
                cashOpen = count * close;
                prevClose = close;
                balance -= round(Math.abs(cashOpen / 100 * COMISSION), 4);
            } else if (count == 0 && shortSet.contains(i)) {
                count = -((int) (cash / close));
                cashOpen = count * close;
                prevClose = close;
                balance -= round(Math.abs(cashOpen / 100 * COMISSION), 4);
            }

            double lo = candles.get(i).getLow();
            double hi = candles.get(i).getHigh();
            boolean ltp = count > 0 && hi >= prevClose * (1 + TP / 100);
            boolean lsl = count > 0 && lo <= prevClose * (1 - SL / 100);
            boolean stp = count < 0 && lo <= prevClose * (1 - TP / 100);
            boolean ssl = count < 0 && hi >= prevClose * (1 + SL / 100);

            if (ltp || lsl || stp || ssl) {
                double price;
                if (lsl) price = prevClose * (1 - SL / 100);
                else if (ssl) price = prevClose * (1 + SL / 100);
                else if (ltp) price = prevClose * (1 + TP / 100);
                else price = prevClose * (1 - TP / 100);

                double result = round(count * price - cashOpen, 4);
                if (result > 0) wins++;
                else if (result < 0) losses++;
                double comm = round(Math.abs(count * price / 100 * COMISSION), 4);
                balance = round(balance + result - comm, 4);
                if (balance < initBal && balance - initBal < maxDD) maxDD = balance - initBal;
                if (balance < minBal) minBal = balance;
                cashOpen = 0;
                count = 0;
            }
        }

        int total = wins + losses;
        double wr = total > 0 ? (double) wins / total * 100 : 0;
        double ddPct = 100 - (initBal - Math.abs(maxDD)) / initBal * 100;
        String msg = String.format("L/S: %d/%d, W/L: %d/%d (%.1f%%), DD: %s (%.1f%%)",
                longs.size(), shorts.size(), wins, losses, wr, DF.format(minBal), ddPct);
        return new Result(balance, wr, msg);
    }

    static boolean trendUp(List<TickerCandle> candles) {
        double[] c = new double[candles.size()];
        for (int i = 0; i < candles.size(); i++) c[i] = candles.get(i).getClose();
        return IndicatorsUtil.movingAverageWhite(c) >= IndicatorsUtil.movingAverageBlack(c);
    }

    synchronized static List<TickerCandle> readCandles(String name, String dir, String file) {
        String key = name + dir + file;
        List<TickerCandle> cached = CANDLE_CACHE.get(key);
        if (cached != null) return new ArrayList<>(cached);
        out.println("Read: " + name + "/" + file);
        List<TickerCandle> list = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader(dir + "/" + name + "/" + file))) {
            String line;
            boolean first = true;
            while ((line = br.readLine()) != null) {
                if (line.trim().isEmpty() || first) {
                    first = false;
                    continue;
                }
                String[] v = line.split(",");
                if (v.length < 6) continue;
                try {
                    list.add(new TickerCandle(name, v[0],
                            Double.parseDouble(v[1]), Double.parseDouble(v[2]),
                            Double.parseDouble(v[3]), Double.parseDouble(v[4]),
                            Double.parseDouble(v[4]), Integer.parseInt(v[5])));
                } catch (NumberFormatException ignored) {
                }
            }
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
        CANDLE_CACHE.put(key, list);
        return new ArrayList<>(list);
    }

    static void plotChart(String ticker, List<TickerCandle> candles, List<Double> lvls,
                          List<Integer> longs, List<Integer> shorts) {
        if (longs.isEmpty() && shorts.isEmpty()) return;
        double mx = Double.MIN_VALUE, mn = Double.MAX_VALUE;
        XYSeries cl = new XYSeries("close");
        List<XYSeries> ls = new ArrayList<>();
        for (int x = 0; x < lvls.size(); x++) ls.add(new XYSeries("level_" + x));
        for (int i = 0; i < candles.size(); i++) {
            double c = candles.get(i).getClose();
            cl.add(i, c);
            for (int x = 0; x < lvls.size(); x++) ls.get(x).add(i, lvls.get(x));
            if (c > mx) mx = c;
            if (c < mn) mn = c;
        }
        XYSeriesCollection data = new XYSeriesCollection();
        data.addSeries(cl);
        ls.forEach(data::addSeries);
        JFreeChart chart = ChartFactory.createXYLineChart(
                ticker, "Ticks", "Price", data, PlotOrientation.VERTICAL, true, true, false);
        XYPlot plot = (XYPlot) chart.getPlot();
        for (int t : longs) {
            ValueMarker m = new ValueMarker(t);
            m.setPaint(Color.GREEN);
            plot.addDomainMarker(m);
        }
        for (int t : shorts) {
            ValueMarker m = new ValueMarker(t);
            m.setPaint(Color.RED);
            plot.addDomainMarker(m);
        }
        ((NumberAxis) plot.getRangeAxis()).setRange(mn * 0.9, mx * 1.1);
        try {
            deleteIfExists(Paths.get("data/" + ticker + "/chart.png"));
            ChartUtilities.writeChartAsPNG(
                    new FileOutputStream("data/" + ticker + "/chart.png"), chart, 8192, 4800);
        } catch (Exception e) {
            out.println(e.getMessage());
        }
    }

    static double round(double v, int p) {
        long f = (long) Math.pow(10, p);
        return (double) Math.round(v * f) / f;
    }
}