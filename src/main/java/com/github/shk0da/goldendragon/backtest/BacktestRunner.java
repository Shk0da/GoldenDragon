package com.github.shk0da.goldendragon.backtest;

import com.github.shk0da.goldendragon.config.UnifiedTraderConfig;
import com.github.shk0da.goldendragon.market.OrderExecutor.ExecutionResult;
import com.github.shk0da.goldendragon.model.Candle;
import com.github.shk0da.goldendragon.model.TickerType;
import com.github.shk0da.goldendragon.model.TradingDecision;
import com.github.shk0da.goldendragon.strategy.BaseStrategy;
import com.github.shk0da.goldendragon.strategy.StrategyRegistry;
import com.github.shk0da.goldendragon.utils.PropertiesUtils;
import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartUtilities;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.axis.DateAxis;
import org.jfree.chart.plot.XYPlot;
import org.jfree.data.time.Day;
import org.jfree.data.time.TimeSeries;
import org.jfree.data.time.TimeSeriesCollection;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.SimpleDateFormat;
import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

/**
 * Движок бэктестинга торговых стратегий на исторических данных.
 *
 * <p>Класс симулирует исполнение одной или нескольких стратегий (см. {@link
 * StrategyRegistry#backtestableNames()}) на массиве тикеров и временных периодов, имитируя реальную
 * торговлю с учётом комиссий, рабочих часов, EOD-закрытий и портфельного управления капиталом. По
 * завершении формирует сводную статистику и сравнительный рейтинг стратегий.
 *
 * <h2>Архитектура</h2>
 *
 * <ul>
 *   <li>{@link SimulatedBroker} — ЕДИНСТВЕННЫЙ источник истины по кэшу, позициям и PnL.
 *       Все денежные операции (buy/sell/close) выполняются через брокер.
 *   <li>{@link BaseStrategy} — общий движок для live и backtest режимов. В backtest получает
 *       обрезанную по currentTime историю (без look-ahead).
 *   <li>BacktestRunner — цикл симуляции по глобальному таймлайну, контроль событий.
 * </ul>
 *
 * <h2>Верификация достоверности</h2>
 *
 * <p>По завершении симуляции {@link #verifyBacktestTruth} проверяет:
 *
 * <ul>
 *   <li>PnL reconciliation: sum(trade.pnl) + tmonParkingPnl ≈ finalCash - initialBalance - totalDeposits.
 *   <li>Opens == Closes (все позиции закрыты).
 *   <li>concurrentPeak &le; {@link #MAX_CONCURRENT_POSITIONS}.
 *   <li>Final equity ≈ finalCash (после closeAll позиций нет), отдельный жёсткий допуск.
 * </ul>
 */
public class BacktestRunner {
    private static final DateTimeFormatter DATE_FMT = DateTimeFormatter.ofPattern("dd.MM.yyyy");
    private static final DateTimeFormatter DATE_TIME_FMT =
        DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm:ss");
    private static final int MIN_HOURS_REQUIRED = 60;
    private static final int MAX_CONCURRENT_POSITIONS = 8;
    private static final String TMON_TICKER = "TMON@";
    private static final LocalTime WORK_START_TIME = LocalTime.of(8, 30);
    private static final LocalTime EOD_CLOSE_TIME = LocalTime.of(21, 0);
    private static final String BACKTEST_MODE = System.getProperty("backtest.mode", "full");
    private static final int BACKTEST_THREADS =
        Math.max(
            1,
            Integer.getInteger(
                "backtest.threads",
                Math.max(1, Runtime.getRuntime().availableProcessors() - 1)));
    private static final List<String> ALL_STRATEGIES = StrategyRegistry.backtestableNames();
    /**
     * Default cooldown in 5-min candles if not specified in config.
     */
    private static final int DEFAULT_COOLDOWN_BARS = 3;

    public static class RawCandle {
        public final String time;
        public final double open;
        public final double high;
        public final double low;
        public final double close;
        public final long volume;
        public final LocalDateTime dateTime;

        public RawCandle(
            String time, double open, double high, double low, double close, long volume) {
            this.time = time;
            this.open = open;
            this.high = high;
            this.low = low;
            this.close = close;
            this.volume = volume;
            this.dateTime = LocalDateTime.parse(time, DATE_TIME_FMT);
        }
    }

    public static class TradeResult {
        public final String ticker;
        public final String dir;
        public final double entry;
        public final double exit;
        public final int qty;
        public final double pnl;
        public final String reason;
        public final String time;

        public TradeResult(
            String ticker,
            String dir,
            double entry,
            double exit,
            int qty,
            double pnl,
            String reason,
            String time) {
            this.ticker = ticker;
            this.dir = dir;
            this.entry = entry;
            this.exit = exit;
            this.qty = qty;
            this.pnl = pnl;
            this.reason = reason;
            this.time = time;
        }
    }

    public static class EquityPoint {
        public final String time;
        public final double equity;

        public EquityPoint(String time, double equity) {
            this.time = time;
            this.equity = equity;
        }
    }

    private static class TickerPeriodResult {
        final List<TradeResult> trades;
        final List<EquityPoint> equityCurve;
        final double pnl;
        final double dd;
        final double startBalance;
        final double winRate;

        TickerPeriodResult(
            List<TradeResult> trades,
            List<EquityPoint> equityCurve,
            double pnl,
            double dd,
            double startBalance,
            double winRate) {
            this.trades = trades;
            this.equityCurve = equityCurve;
            this.pnl = pnl;
            this.dd = dd;
            this.startBalance = startBalance;
            this.winRate = winRate;
        }
    }

    private static class MarketData {
        final List<Candle> hourCandles;
        final List<RawCandle> minuteCandlesRaw;
        final List<Candle> minuteCandles;
        final List<LocalDateTime> minuteTimes;
        final List<LocalDateTime> hourTimes;

        MarketData(
            List<Candle> hourCandles,
            List<RawCandle> minuteCandlesRaw,
            List<Candle> minuteCandles,
            List<LocalDateTime> minuteTimes,
            List<LocalDateTime> hourTimes) {
            this.hourCandles = hourCandles;
            this.minuteCandlesRaw = minuteCandlesRaw;
            this.minuteCandles = minuteCandles;
            this.minuteTimes = minuteTimes;
            this.hourTimes = hourTimes;
        }
    }

    public static class PortfolioPeriodResult {
        public final double pnl;
        public final double dd;
        public final List<EquityPoint> equityCurve;
        public final int totalTrades;
        public final double winRate;

        public PortfolioPeriodResult(
            double pnl,
            double dd,
            List<EquityPoint> equityCurve,
            int totalTrades,
            double winRate) {
            this.pnl = pnl;
            this.dd = dd;
            this.equityCurve = equityCurve;
            this.totalTrades = totalTrades;
            this.winRate = winRate;
        }
    }

    private static class BacktestExecutionResult {
        final Map<String, TickerPeriodResult> tickerResults;
        final PortfolioPeriodResult portfolioResult;

        BacktestExecutionResult(
            Map<String, TickerPeriodResult> tickerResults,
            PortfolioPeriodResult portfolioResult) {
            this.tickerResults = tickerResults;
            this.portfolioResult = portfolioResult;
        }
    }

    private static class PeriodDefinition {
        final String start;
        final String endExclusive;
        final String label;

        PeriodDefinition(String start, String endExclusive, String label) {
            this.start = start;
            this.endExclusive = endExclusive;
            this.label = label;
        }
    }

    private static class MarketDataLoadResult {
        final String ticker;
        final List<Candle> hourCandles;
        final MarketData marketData;

        MarketDataLoadResult(String ticker, List<Candle> hourCandles, MarketData marketData) {
            this.ticker = ticker;
            this.hourCandles = hourCandles;
            this.marketData = marketData;
        }
    }

    private final String dataDir;
    private final double initialBalance;
    private final double commission;
    private final double slippage;
    private final double monthlyRebalanceAmount;

    public BacktestRunner(
        String dataDir,
        double initialBalance,
        double commission,
        double monthlyRebalanceAmount) {
        this(dataDir, initialBalance, commission, 0.0005, monthlyRebalanceAmount);
    }

    public BacktestRunner(
        String dataDir,
        double initialBalance,
        double commission,
        double slippage,
        double monthlyRebalanceAmount) {
        this.dataDir = dataDir;
        this.initialBalance = initialBalance;
        this.commission = commission;
        this.slippage = slippage;
        this.monthlyRebalanceAmount = monthlyRebalanceAmount;
    }

    // Store results for comparison
    private static final Map<String, StrategyMetrics> strategyMetricsMap = new LinkedHashMap<>();

    private static double monthlyDeposit = Double.parseDouble(System.getProperty("backtest.monthlyDeposit", "100000"));

    public static void main(String[] args) throws IOException {
        double commission = Double.parseDouble(System.getProperty("backtest.commission", "0.0005"));
        BacktestRunner runner = new BacktestRunner("data", 100_000, commission, monthlyDeposit);
        boolean singleStrategyRun = args != null && args.length > 0;
        if (singleStrategyRun) {
            String strategyName = args[0];
            System.out.println("\n" + "=".repeat(100));
            System.out.println("RUNNING BACKTEST FOR STRATEGY: " + strategyName);
            System.out.println("=".repeat(100));
            runner.run(strategyName);
        } else {
            for (String strategyName : ALL_STRATEGIES) {
                System.out.println("\n" + "=".repeat(100));
                System.out.println("RUNNING BACKTEST FOR STRATEGY: " + strategyName);
                System.out.println("=".repeat(100));
                runner.run(strategyName);
            }
        }
        if (!singleStrategyRun) {
            System.out.println("\n" + "=".repeat(200));
            System.out.println("СРАВНИТЕЛЬНАЯ ТАБЛИЦА ЭФФЕКТИВНОСТИ СТРАТЕГИЙ");
            System.out.println("=".repeat(200));
            printStrategyComparison();
        }
    }

    public void run() throws IOException {
        run(ALL_STRATEGIES.isEmpty() ? "RegimeAwareStrategy" : ALL_STRATEGIES.get(0));
    }

    public void run(String strategyName) throws IOException {
        List<PeriodDefinition> tablePeriods = getPeriods();
        List<PeriodDefinition> chartPeriods = getFullYearlyPeriods();
        UnifiedTraderConfig config = new UnifiedTraderConfig();
        List<String> loadedTickers = loadTickers();
        List<String> activeTickers = filterEnabledTickers(loadedTickers, config);
        String fullStart = chartPeriods.get(0).start;
        String fullEnd = chartPeriods.get(chartPeriods.size() - 1).endExclusive;
        if (!activeTickers.isEmpty()) {
            int maxLeverage = config.getTickerParams(activeTickers.get(0)).leverage;
            if (config.isAdaptiveLeverageEnabled() && maxLeverage > 1) {
                System.out.println(
                    "Backtest leverage: adaptive (max "
                        + maxLeverage
                        + "x, min "
                        + config.getLeverageMin()
                        + "x)");
            } else {
                System.out.println("Backtest leverage: " + maxLeverage + "x");
            }
        }
        List<String> periodLabels = new ArrayList<>();
        Map<String, Map<String, TickerPeriodResult>> allData = new LinkedHashMap<>();
        Map<String, PortfolioPeriodResult> portfolioData = new LinkedHashMap<>();
        BacktestExecutionResult continuousResult =
            execute(strategyName, fullStart, fullEnd, activeTickers, config);
        for (PeriodDefinition period : tablePeriods) {
            periodLabels.add(period.label);
            BacktestExecutionResult periodResult = splitExecutionByPeriod(continuousResult, period);
            allData.put(period.label, periodResult.tickerResults);
            portfolioData.put(period.label, periodResult.portfolioResult);
        }
        printResults(strategyName, periodLabels, allData, portfolioData, activeTickers);
        collectStrategyMetrics(strategyName, allData, portfolioData);
        runBacktestExpertEvaluation(strategyName, allData, portfolioData);
        plotEquityCurveChart(strategyName, continuousResult.portfolioResult.equityCurve);
    }

    private BacktestExecutionResult splitExecutionByPeriod(BacktestExecutionResult full, PeriodDefinition period) {
        Map<String, TickerPeriodResult> tickerResults = new LinkedHashMap<>();
        int totalTrades = 0;
        int winningTrades = 0;
        for (Map.Entry<String, TickerPeriodResult> entry : full.tickerResults.entrySet()) {
            List<TradeResult> trades = filterTradesByPeriod(entry.getValue().trades, period);
            List<EquityPoint> equity = filterEquityByPeriod(entry.getValue().equityCurve, period);
            double pnl = trades.stream().mapToDouble(t -> t.pnl).sum();
            double dd = calcMaxDrawdownByEquity(equity);
            double winRate = calculateWinRate(trades);
            totalTrades += trades.size();
            winningTrades += (int) trades.stream().filter(t -> t.pnl > 0.0).count();
            tickerResults.put(
                entry.getKey(),
                new TickerPeriodResult(trades, equity, pnl, dd, initialBalance, winRate));
        }
        List<EquityPoint> portfolioEquity =
            filterEquityByPeriod(full.portfolioResult.equityCurve, period);
        double portfolioPnl = computePeriodPnlFromEquity(full.portfolioResult.equityCurve, period);
        double portfolioDd = calcMaxDrawdownByEquity(portfolioEquity);
        double portfolioWinRate = totalTrades > 0 ? (double) winningTrades / totalTrades : 0.0;
        PortfolioPeriodResult portfolioResult =
            new PortfolioPeriodResult(
                portfolioPnl, portfolioDd, portfolioEquity, totalTrades, portfolioWinRate);
        return new BacktestExecutionResult(tickerResults, portfolioResult);
    }

    private List<TradeResult> filterTradesByPeriod(
        List<TradeResult> trades, PeriodDefinition period) {
        if (trades == null || trades.isEmpty()) {
            return Collections.emptyList();
        }
        List<TradeResult> filtered = new ArrayList<>();
        for (TradeResult trade : trades) {
            if (isTimeInPeriod(trade.time, period)) {
                filtered.add(trade);
            }
        }
        return filtered;
    }

    private List<EquityPoint> filterEquityByPeriod(
        List<EquityPoint> equityCurve, PeriodDefinition period) {
        if (equityCurve == null || equityCurve.isEmpty()) {
            return Collections.emptyList();
        }
        List<EquityPoint> filtered = new ArrayList<>();
        for (EquityPoint point : equityCurve) {
            if (isTimeInPeriod(point.time, period)) {
                filtered.add(point);
            }
        }
        return filtered;
    }

    private boolean isTimeInPeriod(String time, PeriodDefinition period) {
        try {
            LocalDate date;
            try {
                date = LocalDateTime.parse(time, DATE_TIME_FMT).toLocalDate();
            } catch (Exception e) {
                String datePart = time.contains(" ") ? time.split(" ")[0] : time;
                date = LocalDate.parse(datePart, DATE_FMT);
            }
            LocalDate start = LocalDate.parse(period.start);
            LocalDate end = LocalDate.parse(period.endExclusive);
            return !date.isBefore(start) && !date.isAfter(end);
        } catch (Exception e) {
            return false;
        }
    }

    private double computePeriodPnlFromEquity(
        List<EquityPoint> equityCurve, PeriodDefinition period) {
        if (equityCurve == null || equityCurve.isEmpty()) {
            return 0.0;
        }
        LocalDate start = LocalDate.parse(period.start);
        LocalDate end = LocalDate.parse(period.endExclusive);
        Double equityBefore = null;
        Double lastInPeriod = null;
        for (EquityPoint point : equityCurve) {
            try {
                LocalDate date;
                try {
                    date = LocalDateTime.parse(point.time, DATE_TIME_FMT).toLocalDate();
                } catch (Exception e) {
                    String datePart =
                        point.time.contains(" ") ? point.time.split(" ")[0] : point.time;
                    date = LocalDate.parse(datePart, DATE_FMT);
                }
                if (date.isBefore(start)) {
                    equityBefore = point.equity;
                } else if (!date.isAfter(end)) {
                    lastInPeriod = point.equity;
                }
            } catch (Exception ignored) {
                // skip malformed timestamps
            }
        }
        if (lastInPeriod == null) {
            return 0.0;
        }
        double startEquity = equityBefore != null ? equityBefore : initialBalance;
        return lastInPeriod - startEquity;
    }

    /**
     * Generates and saves equity curve chart for the backtest.
     *
     * @param strategyName strategy name used for file naming
     * @param equityCurve  portfolio equity points for the full backtest range
     */
    private void plotEquityCurveChart(String strategyName, List<EquityPoint> equityCurve) {
        TimeSeries series = new TimeSeries("Capital");
        List<EquityPoint> sampled = downsampleEquityDaily(equityCurve);
        for (EquityPoint point : sampled) {
            try {
                LocalDateTime localDateTime = LocalDateTime.parse(point.time, DATE_TIME_FMT);
                Day day =
                    new Day(
                        java.util.Date.from(
                            localDateTime
                                .atZone(java.time.ZoneId.systemDefault())
                                .toInstant()));
                series.addOrUpdate(day, point.equity);
            } catch (Exception e) {
                // Skip invalid dates
            }
        }
        if (series.getItemCount() == 0) {
            System.out.println("No equity data available for chart generation");
            return;
        }
        TimeSeriesCollection dataset = new TimeSeriesCollection(series);
        String addItionalDesc = monthlyDeposit > 0 ? " +" + monthlyDeposit / 1000 + "K RUB/month" : "";
        JFreeChart chart =
            ChartFactory.createTimeSeriesChart(
                "Equity Curve - " + strategyName + addItionalDesc,
                "Date",
                "Capital (RUB)",
                dataset,
                true,
                true,
                false);
        XYPlot plot = (XYPlot) chart.getPlot();
        DateAxis dateAxis = (DateAxis) plot.getDomainAxis();
        dateAxis.setDateFormatOverride(new SimpleDateFormat("yyyy-MM"));
        org.jfree.chart.axis.NumberAxis rangeAxis =
            (org.jfree.chart.axis.NumberAxis) plot.getRangeAxis();
        if (series.getItemCount() > 0) {
            double minEquity = Double.MAX_VALUE;
            double maxEquity = Double.MIN_VALUE;
            for (int i = 0; i < series.getItemCount(); i++) {
                double value = series.getValue(i).doubleValue();
                minEquity = Math.min(minEquity, value);
                maxEquity = Math.max(maxEquity, value);
            }
            if (maxEquity - minEquity < 1.0) {
                double padding = Math.max(50_000.0, maxEquity * 0.05);
                rangeAxis.setRange(minEquity - padding, maxEquity + padding);
            }
        }
        try {
            Path imagesDir = Paths.get("images");
            Files.createDirectories(imagesDir);
            String fileName = strategyName + ".png";
            Path outputPath = imagesDir.resolve(fileName);
            try (FileOutputStream out = new FileOutputStream(outputPath.toFile())) {
                ChartUtilities.writeChartAsPNG(out, chart, 1200, 600);
            }
            System.out.println("Equity curve chart saved to: " + outputPath.toAbsolutePath());
        } catch (Exception e) {
            System.out.println("Failed to save equity curve chart: " + e.getMessage());
        }
    }

    /**
     * Keep last equity point per calendar day to avoid minute-level staircase rendering.
     */
    private List<EquityPoint> downsampleEquityDaily(List<EquityPoint> equityCurve) {
        if (equityCurve == null || equityCurve.isEmpty()) {
            return Collections.emptyList();
        }
        Map<java.time.LocalDate, EquityPoint> lastByDay = new LinkedHashMap<>();
        for (EquityPoint point : equityCurve) {
            try {
                java.time.LocalDate day =
                    LocalDateTime.parse(point.time, DATE_TIME_FMT).toLocalDate();
                lastByDay.put(day, point);
            } catch (Exception ignored) {
                // skip malformed timestamps
            }
        }
        return new ArrayList<>(lastByDay.values());
    }

    private List<PeriodDefinition> getPeriods() {
        java.time.LocalDate today = java.time.LocalDate.now();
        if ("fast".equalsIgnoreCase(BACKTEST_MODE)) {
            List<PeriodDefinition> periods = new ArrayList<>();
            java.time.LocalDate currentMonthStart = today.withDayOfMonth(1);
            for (int i = 5; i >= 0; i--) {
                java.time.LocalDate monthStart = currentMonthStart.minusMonths(i);
                java.time.LocalDate monthEnd = monthStart.plusMonths(1);
                String label =
                    String.format("%d.%02d", monthStart.getYear(), monthStart.getMonthValue());
                periods.add(
                    new PeriodDefinition(monthStart.toString(), monthEnd.toString(), label));
            }
            return periods;
        }
        return getFullYearlyPeriods();
    }

    /**
     * Full 5-year yearly periods used for equity curve chart regardless of backtest.mode.
     */
    private List<PeriodDefinition> getFullYearlyPeriods() {
        java.time.LocalDate today = java.time.LocalDate.now();
        List<PeriodDefinition> periods = new ArrayList<>();
        int currentYear = today.getYear();
        for (int i = 4; i >= 0; i--) {
            int year = currentYear - i;
            String start = year + "-01-01";
            String end = year + "-12-31";
            String label = String.valueOf(year);
            periods.add(new PeriodDefinition(start, end, label));
        }
        return periods;
    }

    private void printResults(
        String strategyName,
        List<String> periodLabels,
        Map<String, Map<String, TickerPeriodResult>> allData,
        Map<String, PortfolioPeriodResult> portfolioData,
        List<String> allTickers) {
        System.out.println("\n" + "=".repeat(130));
        System.out.println("РЕЗУЛЬТАТЫ ПО ПЕРИОДАМ ДЛЯ СТРАТЕГИИ: " + strategyName);
        System.out.println("=".repeat(130));
        StringBuilder header = new StringBuilder();
        header.append(String.format("%-10s", "Тикер"));
        for (String label : periodLabels) {
            header.append(String.format(" %22s", label));
        }
        System.out.println(header);
        StringBuilder subHeader = new StringBuilder();
        subHeader.append(String.format("%-10s", ""));
        for (String ignored : periodLabels) {
            // Баг форматирования (fix): PnL-поле расширено с %6s до %8s,
            // чтобы совпадать с formatCompactPnL (ширина 8) — иначе значения ≥100K сдвигали колонки.
            subHeader.append(String.format(" %8s %5s %4s %5s", "PnL", "DD%", "Trd", "WR%"));
        }
        System.out.println(subHeader);
        System.out.println("-".repeat(header.length()));
        for (String ticker : allTickers) {
            StringBuilder row = new StringBuilder();
            row.append(String.format("%-10s", ticker));
            boolean hasAny = false;
            for (String label : periodLabels) {
                Map<String, TickerPeriodResult> tickerData = allData.get(label);
                TickerPeriodResult result = tickerData != null ? tickerData.get(ticker) : null;
                if (result == null || result.trades.isEmpty()) {
                    row.append(String.format(" %22s", "—"));
                } else {
                    hasAny = true;
                    row.append(
                        String.format(
                            " %8s %5s %4d %5.1f",
                            formatCompactPnL(result.pnl),
                            formatCompactDD(result.dd * 100.0),
                            result.trades.size(),
                            result.winRate * 100.0));
                }
            }
            if (hasAny) {
                System.out.println(row);
            }
        }
        System.out.println("-".repeat(header.length()));
        StringBuilder portRow = new StringBuilder();
        portRow.append(String.format("%-10s", "ПОРТФЕЛЬ"));
        for (String label : periodLabels) {
            PortfolioPeriodResult result = portfolioData.get(label);
            if (result == null) {
                portRow.append(String.format(" %22s", "—"));
            } else {
                portRow.append(
                    String.format(
                        " %8s %5s %4d %5.1f",
                        formatCompactPnL(result.pnl),
                        formatCompactDD(result.dd * 100.0),
                        result.totalTrades,
                        result.winRate * 100.0));
            }
        }
        System.out.println(portRow);
        StringBuilder avgRow = new StringBuilder();
        avgRow.append(String.format("%-10s", "СРЕДНЕЕ"));
        double totalPnl = 0;
        int totalTrades = 0;
        double totalDd = 0;
        double totalWr = 0;
        int count = 0;
        for (String label : periodLabels) {
            PortfolioPeriodResult result = portfolioData.get(label);
            if (result != null) {
                totalPnl += result.pnl;
                totalTrades += result.totalTrades;
                totalDd += result.dd;
                totalWr += result.winRate;
                count++;
            }
        }
        if (count > 0) {
            avgRow.append(
                String.format(
                    " %8s %5s %4d %5.1f",
                    formatCompactPnL(totalPnl / count),
                    formatCompactDD((totalDd / count) * 100.0),
                    totalTrades,
                    (totalWr / count) * 100.0));
        }
        for (int i = 1; i < periodLabels.size(); i++) {
            avgRow.append(String.format(" %22s", ""));
        }
        System.out.println(avgRow);
        System.out.println("=".repeat(130));
    }

    private BacktestExecutionResult execute(
        String strategyName,
        String start,
        String endExclusive,
        List<String> tickers,
        UnifiedTraderConfig config)
        throws IOException {
        if (tickers.isEmpty()) {
            return new BacktestExecutionResult(
                Collections.emptyMap(),
                new PortfolioPeriodResult(0.0, 0.0, Collections.emptyList(), 0, 0.0));
        }
        Map<String, List<Candle>> allHourlyCandles = new LinkedHashMap<>();
        Map<String, MarketData> marketDataByTicker = new LinkedHashMap<>();
        Map<String, List<LocalDateTime>> peerTimesMap = new LinkedHashMap<>();
        List<MarketDataLoadResult> loadedMarketData =
            loadMarketDataParallel(tickers, config, start, endExclusive);
        for (MarketDataLoadResult loadResult : loadedMarketData) {
            allHourlyCandles.put(loadResult.ticker, loadResult.hourCandles);
            marketDataByTicker.put(loadResult.ticker, loadResult.marketData);
            peerTimesMap.put(loadResult.ticker, loadResult.marketData.hourTimes);
        }
        Map<String, List<String>> groupTickers = new LinkedHashMap<>();
        for (String ticker : allHourlyCandles.keySet()) {
            String allocGroup = config.getTickerParams(ticker).allocationGroup;
            if (allocGroup != null && !allocGroup.isEmpty()) {
                groupTickers.computeIfAbsent(allocGroup, k -> new ArrayList<>()).add(ticker);
            }
        }
        if (marketDataByTicker.isEmpty()) {
            return new BacktestExecutionResult(
                Collections.emptyMap(),
                new PortfolioPeriodResult(0.0, 0.0, Collections.emptyList(), 0, 0.0));
        }
        // Create broker - the SINGLE source of truth for cash/positions
        SimulatedBroker broker = new SimulatedBroker(initialBalance, commission, slippage);
        for (MarketDataLoadResult loadResult : loadedMarketData) {
            broker.loadCandles(loadResult.ticker, "HOUR", loadResult.hourCandles);
            broker.loadCandles(loadResult.ticker, "5_MIN", loadResult.marketData.minuteCandles);
        }
        BaseStrategy.setBacktestBroker(broker);
        BaseStrategy strategy = StrategyRegistry.createBacktest(strategyName, config);
        List<EquityPoint> portfolioEquity = new ArrayList<>();
        Map<String, List<TradeResult>> tradesByTicker = new LinkedHashMap<>();
        Map<String, Integer> minuteIndexByTicker = new LinkedHashMap<>();
        Map<String, Integer> lastSeenHourIdx = new LinkedHashMap<>();
        for (String ticker : marketDataByTicker.keySet()) {
            tradesByTicker.put(ticker, new ArrayList<>());
            minuteIndexByTicker.put(ticker, 0);
            lastSeenHourIdx.put(ticker, -1);
        }
        List<String> globalTimeline = buildGlobalTimeline(marketDataByTicker);
        int lastRebalanceMonth = -1;
        Map<String, Long> lastEodCloseDayByTicker = new HashMap<>();
        double totalDeposits = 0.0;
        int concurrentPeak = 0;
        int cooldownCandles = config.getCooldownCandles() > 0
            ? config.getCooldownCandles()
            : DEFAULT_COOLDOWN_BARS;
        for (String time : globalTimeline) {
            LocalDateTime currentTime = LocalDateTime.parse(time, DATE_TIME_FMT);
            long currentDay = currentTime.toLocalDate().toEpochDay();
            broker.setCurrentTime(currentTime);
            broker.tickCooldown();
            int currentMonth = currentTime.getMonthValue();
            if (currentMonth != lastRebalanceMonth) {
                broker.deposit(monthlyRebalanceAmount);
                totalDeposits += monthlyRebalanceAmount;
                lastRebalanceMonth = currentMonth;
                if (config.isTmonCashParkingEnabled()) {
                    double cash = broker.getSharedCash();
                    if (cash > 0) {
                        double tmonValue = broker.getTmonPositionValue();
                        double availableForPark = cash + tmonValue;
                        if (availableForPark > 0) {
                            SimulatedBroker.SimulatedPosition tmonPos = broker.getPositionState(TMON_TICKER);
                            if (!tmonPos.hasOpenPosition()) {
                                int tmonLotSize = 1;
                                int tmonShares = (int) Math.floor(availableForPark / tmonLotSize);
                                if (tmonShares > 0) {
                                    broker.buy(TMON_TICKER, tmonShares, null, null);
                                }
                            }
                        }
                    }
                }
            }
            for (String ticker : marketDataByTicker.keySet()) {
                MarketData marketData = marketDataByTicker.get(ticker);
                int idx = minuteIndexByTicker.get(ticker);
                if (idx >= marketData.minuteTimes.size()) {
                    continue;
                }
                LocalDateTime tickerTime = marketData.minuteTimes.get(idx);
                if (!tickerTime.equals(currentTime)) {
                    continue;
                }
                SimulatedBroker.SimulatedPosition brokerPos = broker.getPositionState(ticker);
                Candle current = marketData.minuteCandles.get(idx);
                if (brokerPos.hasOpenPosition() && !TMON_TICKER.equals(ticker)) {
                    ExecutionResult sltpResult = broker.checkStopLossTakeProfit(ticker, current);
                    if (sltpResult != null) {
                        brokerPos = broker.getPositionState(ticker);
                        brokerPos.cooldownRemaining = cooldownCandles;
                        strategy.getPositionStore().put(ticker, brokerPos.position);
                        portfolioEquity.add(new EquityPoint(time, broker.getTotalPortfolioValue()));
                        minuteIndexByTicker.put(ticker, idx + 1);
                        continue;
                    }
                }
                if (!isTradingDay(currentTime.toLocalDate())
                    || !isWithinWorkingHours(currentTime.toLocalTime())) {
                    if (brokerPos.hasOpenPosition()
                        && !currentTime.toLocalTime().isBefore(EOD_CLOSE_TIME)
                        && lastEodCloseDayByTicker.getOrDefault(ticker, -1L) != currentDay
                        && !TMON_TICKER.equals(ticker)) {
                        if (brokerPos.isLong()) {
                            broker.closeLong(ticker);
                        } else if (brokerPos.isShort()) {
                            broker.closeShort(ticker);
                        }
                        brokerPos = broker.getPositionState(ticker);
                        brokerPos.cooldownRemaining = cooldownCandles;
                        lastEodCloseDayByTicker.put(ticker, currentDay);
                    }
                    strategy.getPositionStore().put(ticker, brokerPos.position);
                    portfolioEquity.add(new EquityPoint(time, broker.getTotalPortfolioValue()));
                    minuteIndexByTicker.put(ticker, idx + 1);
                    continue;
                }
                int hourUpTo = upperBound(marketData.hourTimes, currentTime);
                if (hourUpTo < 0) {
                    minuteIndexByTicker.put(ticker, idx + 1);
                    continue;
                }
                List<Candle> hourHistory = marketData.hourCandles.subList(0, hourUpTo + 1);
                int prevSeen = lastSeenHourIdx.getOrDefault(ticker, -1);
                int seen = Math.max(prevSeen, hourUpTo);
                boolean hourChanged = brokerPos.hasOpenPosition() && seen != prevSeen;
                lastSeenHourIdx.put(ticker, seen);
                if (hourHistory.size() >= MIN_HOURS_REQUIRED) {
                    Map<String, List<Candle>> currentPeerCandles = buildCurrentPeerCandles(
                        ticker, currentTime, allHourlyCandles, groupTickers, peerTimesMap, hourHistory, config);
                    strategy.setPeerCandles(currentPeerCandles.isEmpty() ? Collections.emptyMap() : currentPeerCandles);
                    double effectiveBalance = broker.getSharedCash();
                    if (!TMON_TICKER.equals(ticker) && config.isTmonCashParkingEnabled()) {
                        double tmonValue = broker.getTmonPositionValue();
                        if (tmonValue > 0) {
                            effectiveBalance += tmonValue;
                        }
                    }
                    strategy.getPositionStore().put(ticker, brokerPos.position);
                    TradingDecision decision = strategy.decide(
                        ticker, hourHistory, marketData.minuteCandles.subList(0, idx + 1),
                        brokerPos.position, effectiveBalance, hourChanged);
                    executeDecisionViaBroker(ticker, strategy, decision, current, broker, cooldownCandles);
                    brokerPos = broker.getPositionState(ticker);
                    strategy.getPositionStore().put(ticker, brokerPos.position);
                }
                minuteIndexByTicker.put(ticker, idx + 1);
            }
            int open = broker.getOpenPositionCount();
            if (open > concurrentPeak) {
                concurrentPeak = open;
            }
            portfolioEquity.add(new EquityPoint(time, broker.getTotalPortfolioValue()));
        }
        broker.closeAll("period_end");
        broker.closeTmonParking("period_end");
        if (!globalTimeline.isEmpty()) {
            portfolioEquity.add(new EquityPoint(
                globalTimeline.get(globalTimeline.size() - 1),
                broker.getTotalPortfolioValue()));
        }
        for (SimulatedBroker.BacktestTrade bt : broker.getTradeHistory()) {
            if ("CLOSE".equals(bt.action)) {
                List<TradeResult> tickerTrades = tradesByTicker.get(bt.ticker);
                if (tickerTrades != null) {
                    tickerTrades.add(
                        new TradeResult(
                            bt.ticker, bt.direction, bt.entryPrice, bt.exitPrice,
                            bt.quantity, bt.pnl, bt.reason, bt.time));
                }
            }
        }
        double finalPortfolioValue = broker.getTotalPortfolioValue();
        double finalSharedCash = broker.getSharedCash();
        // Правка 7 (fix): прокидываем накопленный parking-PnL TMON@ в reconciliation.
        double tmonParkingPnl = broker.getTmonRealizedPnl();
        verifyBacktestTruth(
            broker.getTradeHistory(),
            finalSharedCash,
            initialBalance,
            totalDeposits,
            tmonParkingPnl,
            concurrentPeak,
            portfolioEquity);
        Map<String, TickerPeriodResult> tickerResults = new LinkedHashMap<>();
        int totalTrades = 0;
        int winningTrades = 0;
        for (String ticker : marketDataByTicker.keySet()) {
            List<TradeResult> tickerTrades = tradesByTicker.get(ticker);
            double tickerPnl = tickerTrades.stream().mapToDouble(t -> t.pnl).sum();
            double tickerWinRate = calculateWinRate(tickerTrades);
            totalTrades += tickerTrades.size();
            winningTrades += (int) tickerTrades.stream().filter(t -> t.pnl > 0.0).count();
            tickerResults.put(
                ticker,
                new TickerPeriodResult(
                    tickerTrades,
                    Collections.emptyList(),
                    tickerPnl,
                    0.0,
                    initialBalance,
                    tickerWinRate));
        }
        double portfolioPnl = finalPortfolioValue - initialBalance;
        double portfolioDd = calcMaxDrawdownByEquity(portfolioEquity);
        double portfolioWinRate = totalTrades > 0 ? (double) winningTrades / totalTrades : 0.0;
        PortfolioPeriodResult portfolioResult =
            new PortfolioPeriodResult(
                portfolioPnl, portfolioDd, portfolioEquity, totalTrades, portfolioWinRate);
        clearBacktestBroker();
        return new BacktestExecutionResult(tickerResults, portfolioResult);
    }

    /**
     * Clear backtest broker reference after simulation completes.
     * This ensures live mode runs without backtest broker interference.
     */
    public static void clearBacktestBroker() {
        BaseStrategy.setBacktestBroker(null);
    }

    /**
     * Execute a trading decision via the broker.
     * C2: Checks cooldown before OPEN.
     * E1: MAX_CONCURRENT checked inside broker.buy/sell.
     */
    private void executeDecisionViaBroker(
        String ticker, BaseStrategy strategy, TradingDecision decision,
        Candle current, SimulatedBroker broker, int cooldownCandles) {
        if (decision == null) {
            return;
        }
        SimulatedBroker.SimulatedPosition brokerPos = broker.getPositionState(ticker);
        switch (decision.action) {
            case "OPEN":
                if (brokerPos.cooldownRemaining > 0) {
                    return;
                }
                if (decision.updatedPosition == null || decision.quantity <= 0) {
                    return;
                }
                String openDir = decision.updatedPosition.direction;
                if (!"BUY".equals(openDir) && !"SELL".equals(openDir)) {
                    return;
                }
                if (!TMON_TICKER.equals(ticker) && decision.quantity > 0 && decision.entryPrice != null) {
                    double positionValue = decision.quantity * decision.entryPrice;
                    double availableCash = broker.getSharedCash();
                    double missing = positionValue - availableCash;
                    if (missing > 0) {
                        boolean sold = broker.sellByMarket(TMON_TICKER, TickerType.ETF, missing);
                        if (sold) {
                            brokerPos = broker.getPositionState(ticker);
                        }
                    }
                }
                ExecutionResult execResult;
                if ("BUY".equals(openDir)) {
                    execResult = broker.buy(ticker, decision.quantity,
                        decision.updatedPosition.stopLoss, decision.updatedPosition.takeProfit);
                } else {
                    execResult = broker.sell(ticker, decision.quantity,
                        decision.updatedPosition.stopLoss, decision.updatedPosition.takeProfit);
                }
                if (!execResult.isSuccess()) {
                    return;
                }
                brokerPos = broker.getPositionState(ticker);
                brokerPos.cooldownRemaining = cooldownCandles;
                return;
            case "CLOSE":
                if (brokerPos.isLong()) {
                    broker.closeLong(ticker);
                } else if (brokerPos.isShort()) {
                    broker.closeShort(ticker);
                }
                brokerPos = broker.getPositionState(ticker);
                brokerPos.cooldownRemaining = cooldownCandles;
                return;
            case "HOLD":
                if (decision.updatedPosition != null && brokerPos.hasOpenPosition()) {
                    broker.updateProtectiveLevels(
                        ticker,
                        decision.updatedPosition.stopLoss,
                        decision.updatedPosition.takeProfit);
                }
                return;
            default:
                return;
        }
    }

    private List<Candle> loadDailyCandles(String ticker) {
        File file = new File(dataDir, ticker + "/candlesDAY.txt");
        if (!file.exists()) {
            return Collections.emptyList();
        }
        try {
            List<String> lines = Files.readAllLines(file.toPath());
            List<Candle> candles = new ArrayList<>();
            for (int i = 1; i < lines.size(); i++) {
                String line = lines.get(i).trim();
                if (line.isEmpty()) {
                    continue;
                }
                try {
                    String[] parts = line.split(",");
                    if (parts.length < 6) {
                        continue;
                    }
                    candles.add(
                        new Candle(
                            parts[0].trim(),
                            Double.parseDouble(parts[1]),
                            Double.parseDouble(parts[2]),
                            Double.parseDouble(parts[3]),
                            Double.parseDouble(parts[4]),
                            Long.parseLong(parts[5])));
                } catch (Exception ignored) {
                }
            }
            candles.sort(
                Comparator.comparing(
                    c -> {
                        try {
                            return LocalDateTime.parse(c.time, DATE_TIME_FMT);
                        } catch (Exception e) {
                            return LocalDateTime.MIN;
                        }
                    }));
            return candles;
        } catch (IOException e) {
            return Collections.emptyList();
        }
    }

    private List<MarketDataLoadResult> loadMarketDataParallel(
        List<String> tickers, UnifiedTraderConfig config, String start, String endExclusive)
        throws IOException {
        ExecutorService executor =
            Executors.newFixedThreadPool(
                Math.min(BACKTEST_THREADS, Math.max(1, tickers.size())));
        try {
            List<Callable<MarketDataLoadResult>> tasks = new ArrayList<>();
            for (String ticker : tickers) {
                tasks.add(() -> loadMarketData(ticker, config, start, endExclusive));
            }
            List<Future<MarketDataLoadResult>> futures = executor.invokeAll(tasks);
            List<MarketDataLoadResult> results = new ArrayList<>();
            for (Future<MarketDataLoadResult> future : futures) {
                try {
                    MarketDataLoadResult loadResult = future.get();
                    if (loadResult != null) {
                        results.add(loadResult);
                    }
                } catch (ExecutionException e) {
                    Throwable cause = e.getCause();
                    if (cause instanceof IOException) {
                        throw (IOException) cause;
                    }
                    throw new IOException("Failed to load market data", cause);
                }
            }
            return results;
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new IOException("Market data loading interrupted", e);
        } finally {
            executor.shutdownNow();
        }
    }

    private MarketDataLoadResult loadMarketData(
        String ticker, UnifiedTraderConfig config, String start, String endExclusive) {
        UnifiedTraderConfig.TickerParams params = config.getTickerParams(ticker);
        if (!params.enabled) {
            return null;
        }
        List<RawCandle> raw = loadCandles(ticker, start, endExclusive);
        if (raw.size() < MIN_HOURS_REQUIRED) {
            return null;
        }
        List<Candle> wrapped = new ArrayList<>(raw.size());
        List<LocalDateTime> hourTimes = new ArrayList<>(raw.size());
        for (RawCandle c : raw) {
            wrapped.add(new Candle(c.time, c.open, c.high, c.low, c.close, c.volume));
            hourTimes.add(c.dateTime);
        }
        List<RawCandle> minuteCandlesRaw =
            params.useMinuteCandles ? loadCandles5Min(ticker, start, endExclusive) : raw;
        if (minuteCandlesRaw.isEmpty()) {
            return null;
        }
        List<Candle> wrappedMin = new ArrayList<>(minuteCandlesRaw.size());
        List<LocalDateTime> minTimes = new ArrayList<>(minuteCandlesRaw.size());
        for (RawCandle c : minuteCandlesRaw) {
            wrappedMin.add(new Candle(c.time, c.open, c.high, c.low, c.close, c.volume));
            minTimes.add(c.dateTime);
        }
        MarketData marketData =
            new MarketData(wrapped, minuteCandlesRaw, wrappedMin, minTimes, hourTimes);
        return new MarketDataLoadResult(ticker, wrapped, marketData);
    }

    private List<String> buildGlobalTimeline(Map<String, MarketData> marketDataByTicker) {
        Set<String> timeline = new TreeSet<>(this::compareTime);
        for (MarketData marketData : marketDataByTicker.values()) {
            for (Candle candle : marketData.minuteCandles) {
                timeline.add(candle.time);
            }
        }
        return new ArrayList<>(timeline);
    }

    private Map<String, List<Candle>> buildCurrentPeerCandles(
        String ticker,
        LocalDateTime minDt,
        Map<String, List<Candle>> allHourlyCandles,
        Map<String, List<String>> groupTickers,
        Map<String, List<LocalDateTime>> peerTimesMap,
        List<Candle> currentTickerHourHistory,
        UnifiedTraderConfig config) {
        Map<String, List<Candle>> result = new HashMap<>();
        result.put(ticker, currentTickerHourHistory);
        String allocGroup = config.getTickerParams(ticker).allocationGroup;
        if (allocGroup == null || allocGroup.isEmpty()) {
            return result;
        }
        List<String> members = groupTickers.getOrDefault(allocGroup, Collections.emptyList());
        for (String peer : members) {
            if (peer.equals(ticker)) {
                continue;
            }
            List<Candle> peerAll = allHourlyCandles.get(peer);
            List<LocalDateTime> peerTimes = peerTimesMap.get(peer);
            if (peerAll == null || peerTimes == null || peerTimes.isEmpty()) {
                continue;
            }
            int peerIdx = upperBound(peerTimes, minDt);
            if (peerIdx >= 0) {
                result.put(peer, peerAll.subList(0, peerIdx + 1));
            }
        }
        return result;
    }

    private int compareTime(String t1, String t2) {
        LocalDateTime d1 = LocalDateTime.parse(t1, DATE_TIME_FMT);
        LocalDateTime d2 = LocalDateTime.parse(t2, DATE_TIME_FMT);
        return d1.compareTo(d2);
    }

    private List<String> filterEnabledTickers(List<String> tickers, UnifiedTraderConfig config) {
        List<String> result = new ArrayList<>();
        for (String ticker : tickers) {
            try {
                UnifiedTraderConfig.TickerParams params = config.getTickerParams(ticker);
                if (params.enabled) {
                    result.add(ticker);
                }
            } catch (Exception ignored) {
            }
        }
        return result;
    }

    private int upperBound(List<LocalDateTime> times, LocalDateTime target) {
        int left = 0;
        int right = times.size() - 1;
        int ans = -1;
        while (left <= right) {
            int mid = (left + right) >>> 1;
            if (!times.get(mid).isAfter(target)) {
                ans = mid;
                left = mid + 1;
            } else {
                right = mid - 1;
            }
        }
        return ans;
    }

    private List<String> loadTickers() throws IOException {
        Properties props = PropertiesUtils.loadProperties();
        Set<String> tickers = new LinkedHashSet<>();
        for (String s : props.getProperty("datacollector.instruments", "").split(",")) {
            String t = s.trim();
            if (!t.isEmpty()) tickers.add(t);
        }
        for (String s : props.getProperty("datacollector.crypto", "").split(",")) {
            String t = s.trim();
            if (!t.isEmpty()) tickers.add(t);
        }
        for (String key : props.stringPropertyNames()) {
            if (key.startsWith("unifiedTrader.ticker.")) {
                String[] parts = key.split("\\.");
                if (parts.length > 2) {
                    tickers.add(parts[2]);
                }
            }
        }
        return new ArrayList<>(tickers);
    }

    private List<RawCandle> loadCandles(String ticker, String startDate, String endExclusiveDate) {
        File file = new File(dataDir, ticker + "/candlesHOUR.txt");
        if (!file.exists()) return Collections.emptyList();
        LocalDate start = LocalDate.parse(startDate);
        LocalDate endExclusive = LocalDate.parse(endExclusiveDate);
        try {
            List<String> lines = Files.readAllLines(file.toPath());
            List<RawCandle> result = new ArrayList<>();
            for (int i = 1; i < lines.size(); i++) {
                String line = lines.get(i).trim();
                if (line.isEmpty()) continue;
                try {
                    String[] parts = line.split(",");
                    if (parts.length < 6) continue;
                    int spaceIdx = parts[0].indexOf(' ');
                    String datePart = spaceIdx >= 0 ? parts[0].substring(0, spaceIdx) : parts[0];
                    LocalDate dt = LocalDate.parse(datePart, DATE_FMT);
                    if (dt.isBefore(start) || !dt.isBefore(endExclusive)) continue;
                    result.add(
                        new RawCandle(
                            parts[0],
                            Double.parseDouble(parts[1]),
                            Double.parseDouble(parts[2]),
                            Double.parseDouble(parts[3]),
                            Double.parseDouble(parts[4]),
                            Long.parseLong(parts[5])));
                } catch (Exception ignored) {
                }
            }
            result.sort(Comparator.comparing(c -> c.dateTime));
            return result;
        } catch (IOException e) {
            return Collections.emptyList();
        }
    }

    private List<RawCandle> loadCandles5Min(
        String ticker, String startDate, String endExclusiveDate) {
        File file = new File(dataDir, ticker + "/candles5_MIN.txt");
        if (!file.exists()) return Collections.emptyList();
        LocalDate start = LocalDate.parse(startDate);
        LocalDate endExclusive = LocalDate.parse(endExclusiveDate);
        try {
            List<String> lines = Files.readAllLines(file.toPath());
            List<RawCandle> result = new ArrayList<>();
            for (int i = 1; i < lines.size(); i++) {
                String line = lines.get(i).trim();
                if (line.isEmpty()) continue;
                try {
                    String[] parts = line.split(",");
                    if (parts.length < 6) continue;
                    int spaceIdx = parts[0].indexOf(' ');
                    String datePart = spaceIdx >= 0 ? parts[0].substring(0, spaceIdx) : parts[0];
                    LocalDate dt = LocalDate.parse(datePart, DATE_FMT);
                    if (dt.isBefore(start) || !dt.isBefore(endExclusive)) continue;
                    result.add(
                        new RawCandle(
                            parts[0],
                            Double.parseDouble(parts[1]),
                            Double.parseDouble(parts[2]),
                            Double.parseDouble(parts[3]),
                            Double.parseDouble(parts[4]),
                            Long.parseLong(parts[5])));
                } catch (Exception ignored) {
                }
            }
            result.sort(Comparator.comparing(c -> c.dateTime));
            return result;
        } catch (IOException e) {
            return Collections.emptyList();
        }
    }

    private boolean isTradingDay(LocalDate date) {
        DayOfWeek day = date.getDayOfWeek();
        return day != DayOfWeek.SATURDAY && day != DayOfWeek.SUNDAY;
    }

    private boolean isWithinWorkingHours(LocalTime time) {
        return !time.isBefore(WORK_START_TIME) && time.isBefore(EOD_CLOSE_TIME);
    }

    private double calcMaxDrawdownByEquity(List<EquityPoint> equityCurve) {
        if (equityCurve == null || equityCurve.isEmpty()) return 0.0;
        double peak = equityCurve.get(0).equity;
        double maxDd = 0.0;
        for (EquityPoint point : equityCurve) {
            if (point.equity > peak) peak = point.equity;
            double dd = peak > 0 ? (peak - point.equity) / peak : 0.0;
            if (dd > maxDd) maxDd = dd;
        }
        return maxDd;
    }

    private double calculateWinRate(List<TradeResult> trades) {
        if (trades == null || trades.isEmpty()) return 0.0;
        long winningTrades = trades.stream().filter(trade -> trade.pnl > 0).count();
        return (double) winningTrades / trades.size();
    }

    /**
     * Форматирует PnL в компактный вид с фиксированной шириной 8 символов.
     *
     * <p>Баг форматирования (fix): раньше поле было %6s, но "+123.4K" (7 симв.) и
     * "-1.23M" при знаке/дробях переполняли колонку, ломая выравнивание таблицы.
     * Теперь единая ширина 8 согласована с заголовками printResults (%8s).</p>
     */
    private static String formatCompactPnL(double pnl) {
        String sign = pnl >= 0 ? "+" : "-";
        double abs = Math.abs(pnl);
        String body;
        if (abs >= 1_000_000) {
            body = String.format("%.2f", abs / 1_000_000) + "M";
        } else if (abs >= 1_000) {
            body = String.format("%.1f", abs / 1_000) + "K";
        } else {
            body = String.format("%.0f", abs);
        }
        return String.format("%8s", sign + body);
    }

    private String formatCompactDD(double dd) {
        String risk = assessRisk(dd);
        String marker;
        if ("High".equals(risk)) marker = "*";
        else if ("Med".equals(risk)) marker = "!";
        else marker = " ";
        return String.format("%5s", marker + String.format("%.1f", dd));
    }

    private String assessRisk(double ddPct) {
        if (ddPct < 10.0) return "Low";
        if (ddPct < 25.0) return "Med";
        return "High";
    }

    /**
     * Strategy metrics for comparison.
     */
    private static class StrategyMetrics {
        final String strategyName;
        double totalPnL;
        double avgWinRate;
        double maxDrawdown;
        int totalTrades;
        double profitFactor;
        double averageMonthlyReturn;

        StrategyMetrics(String strategyName) {
            this.strategyName = strategyName;
        }
    }

    private void collectStrategyMetrics(
        String strategyName,
        Map<String, Map<String, TickerPeriodResult>> allData,
        Map<String, PortfolioPeriodResult> portfolioData) {
        StrategyMetrics metrics = new StrategyMetrics(strategyName);
        double totalPnL = 0.0;
        double totalWinRate = 0.0;
        double maxDD = 0.0;
        int periodCount = 0;
        List<Double> returns = new ArrayList<>();
        double grossProfit = 0.0;
        double grossLoss = 0.0;
        for (PortfolioPeriodResult portfolioResult : portfolioData.values()) {
            totalPnL += portfolioResult.pnl;
            if (portfolioResult.dd > maxDD) {
                maxDD = portfolioResult.dd;
            }
            totalWinRate += portfolioResult.winRate;
            periodCount++;
            if (portfolioResult.equityCurve != null && !portfolioResult.equityCurve.isEmpty()) {
                double startEquity = portfolioResult.equityCurve.get(0).equity;
                if (startEquity > 0.0) {
                    returns.add(portfolioResult.pnl / startEquity);
                }
            }
        }
        metrics.totalPnL = totalPnL;
        metrics.avgWinRate = periodCount > 0 ? totalWinRate / periodCount : 0.0;
        metrics.maxDrawdown = maxDD;
        metrics.totalTrades = portfolioData.values().stream().mapToInt(p -> p.totalTrades).sum();
        if (returns.size() > 1) {
            metrics.averageMonthlyReturn =
                returns.stream().mapToDouble(r -> r).average().orElse(0.0);
        }
        for (Map.Entry<String, Map<String, TickerPeriodResult>> entry : allData.entrySet()) {
            Map<String, TickerPeriodResult> tickerPeriodResults = entry.getValue();
            for (TickerPeriodResult result : tickerPeriodResults.values()) {
                for (TradeResult trade : result.trades) {
                    if (trade.pnl > 0.0) {
                        grossProfit += trade.pnl;
                    } else if (trade.pnl < 0.0) {
                        grossLoss += Math.abs(trade.pnl);
                    }
                }
            }
        }
        metrics.profitFactor = grossLoss > 0 ? grossProfit / grossLoss : 0.0;
        strategyMetricsMap.put(strategyName, metrics);
    }

    private static void printStrategyComparison() {
        if (strategyMetricsMap.isEmpty()) {
            System.out.println("No strategy metrics collected.");
            return;
        }
        System.out.println();
        String header =
            String.format(
                "%-20s %15s %10s %10s %10s %10s %10s",
                "Стратегия", "Total PnL", "WinRate%", "MaxDD%", "Trades", "PF", "Score");
        System.out.println(header);
        System.out.println("-".repeat(header.length()));
        List<StrategyMetrics> sortedMetrics = new ArrayList<>(strategyMetricsMap.values());
        sortedMetrics.sort((a, b) -> Double.compare(calculateScore(b), calculateScore(a)));
        for (StrategyMetrics m : sortedMetrics) {
            double score = calculateScore(m);
            System.out.println(
                String.format(
                    "%-20s %15s %10.1f %10.1f %10d %10.2f %10.1f",
                    m.strategyName,
                    formatCompactPnL(m.totalPnL),
                    m.avgWinRate * 100.0,
                    m.maxDrawdown * 100.0,
                    m.totalTrades,
                    m.profitFactor,
                    score));
        }
        System.out.println();
        System.out.println("Legend:");
        System.out.println("  Score = (WinRate% × 0.4) + ((20 - MaxDD%) × 0.3) + (PF × 0.3)");
        System.out.println("  Best strategy has highest score");
        System.out.println();
        if (!sortedMetrics.isEmpty()) {
            StrategyMetrics best = sortedMetrics.get(0);
            System.out.println(
                "🏆 BEST STRATEGY: "
                    + best.strategyName
                    + " (Score: "
                    + String.format("%.1f", calculateScore(best))
                    + ")");
        }
    }

    private static double calculateScore(StrategyMetrics m) {
        double winRateScore = m.avgWinRate * 100.0 * 0.4;
        double ddScore =
            Math.max(0, (20.0 - (m.maxDrawdown * 100.0))) * 0.3;
        double pfScore = Math.min(m.profitFactor, 3.0) * 10.0 * 0.3;
        return winRateScore + ddScore + pfScore;
    }

    private List<TradeResult> collectAllTrades(
        Map<String, Map<String, TickerPeriodResult>> allData) {
        List<TradeResult> allTrades = new ArrayList<>();
        for (Map<String, TickerPeriodResult> tickerResults : allData.values()) {
            for (TickerPeriodResult result : tickerResults.values()) {
                allTrades.addAll(result.trades);
            }
        }
        allTrades.sort(Comparator.comparing(t -> t.time));
        return allTrades;
    }

    private void runBacktestExpertEvaluation(
        String strategyName,
        Map<String, Map<String, TickerPeriodResult>> allData,
        Map<String, PortfolioPeriodResult> portfolioData) {
        List<TradeResult> allTrades = collectAllTrades(allData);
        allTrades.removeIf(t -> Math.abs(t.pnl) < 0.01);
        if (allTrades.isEmpty()) {
            System.out.println("\nBacktestExpert: No trades to evaluate for " + strategyName);
            return;
        }
        double commissionRate = 0.0005;
        BacktestExpertEvaluator.EvaluationResult result =
            BacktestExpertEvaluator.evaluate(
                allTrades, portfolioData, initialBalance, commissionRate);
        BacktestExpertEvaluator.printEvaluationReport(result, strategyName);
    }

    /**
     * Self-verification checks for backtest truth (todo.md Section D).
     *
     * <p>Правка 7 (fix): в reconciliation теперь учитывается parking-PnL TMON@
     * ({@code tmonParkingPnl}), поскольку сделки TMON@ не пишутся в tradeHistory.
     * reconciled = totalTradePnl + tmonParkingPnl должен совпадать с expectedPnl.</p>
     *
     * <p>Правка 4 (fix): для D4 (final equity vs cash) введён отдельный жёсткий
     * абсолютный допуск {@code PORTFOLIO_TOLERANCE = 1.0} вместо процентного —
     * после closeAll позиций нет, и equity обязана точно равняться кэшу.</p>
     * <p>
     * Validates:
     * - Sum of all trade PnL + tmonParkingPnl ≈ (finalCash - initialBalance - totalDeposits)
     * - Opens == Closes (all positions closed)
     * - concurrentPeak <= MAX_CONCURRENT_POSITIONS
     * - Final equity ≈ finalCash (after closeAll, no positions remain)
     */
    private void verifyBacktestTruth(
        List<SimulatedBroker.BacktestTrade> tradeHistory,
        double finalSharedCash,
        double initialBalance,
        double totalDeposits,
        double tmonParkingPnl,
        int concurrentPeak,
        List<EquityPoint> portfolioEquity) {
        double totalTradePnl = 0.0;
        int totalOpens = 0;
        int totalCloses = 0;
        for (SimulatedBroker.BacktestTrade trade : tradeHistory) {
            totalTradePnl += trade.pnl;
            if ("OPEN".equals(trade.action)) {
                totalOpens++;
            } else if ("CLOSE".equals(trade.action)) {
                totalCloses++;
            }
        }
        // Правка 7: полный realized-PnL = сделки из истории + parking-PnL TMON@.
        double reconciledPnl = totalTradePnl + tmonParkingPnl;
        double expectedPnl = finalSharedCash - initialBalance - totalDeposits;
        double pnlDiff = Math.abs(reconciledPnl - expectedPnl);
        double tolerance = Math.max(1.0, Math.abs(expectedPnl) * 0.01); // 1% tolerance
        if (pnlDiff > tolerance) {
            System.err.println("⚠️  BACKTEST VERIFICATION WARNING: PnL mismatch");
            System.err.println("   Expected: " + String.format("%.2f", expectedPnl));
            System.err.println("   Reconciled (trades+TMON@): " + String.format("%.2f", reconciledPnl));
            System.err.println("   From trades: " + String.format("%.2f", totalTradePnl));
            System.err.println("   TMON@ parking PnL: " + String.format("%.2f", tmonParkingPnl));
            System.err.println("   Difference: " + String.format("%.2f", pnlDiff));
            System.err.println("   Deposits: " + String.format("%.2f", totalDeposits));
        }
        if (totalOpens != totalCloses) {
            System.err.println("⚠️  BACKTEST VERIFICATION WARNING: Opens != Closes");
            System.err.println("   Opens: " + totalOpens + ", Closes: " + totalCloses);
        }
        if (concurrentPeak > MAX_CONCURRENT_POSITIONS) {
            System.err.println("⚠️  BACKTEST VERIFICATION WARNING: concurrentPeak exceeded MAX");
            System.err.println("   Peak: " + concurrentPeak + ", MAX: " + MAX_CONCURRENT_POSITIONS);
        }
        // Правка 4: жёсткий абсолютный допуск для final equity vs cash.
        final double PORTFOLIO_TOLERANCE = 1.0;
        if (!portfolioEquity.isEmpty()) {
            double lastEquity = portfolioEquity.get(portfolioEquity.size() - 1).equity;
            double equityDiff = Math.abs(lastEquity - finalSharedCash);
            if (equityDiff > PORTFOLIO_TOLERANCE) {
                System.err.println("⚠️  BACKTEST VERIFICATION WARNING: Final equity mismatch");
                System.err.println("   Portfolio equity: " + String.format("%.2f", lastEquity));
                System.err.println("   Shared cash: " + String.format("%.2f", finalSharedCash));
                System.err.println("   Difference: " + String.format("%.2f", equityDiff));
            }
        }
        System.out.println("✓ Backtest verification: Reconciled=" + String.format("%.2f", reconciledPnl) +
            " (Trades=" + String.format("%.2f", totalTradePnl) +
            ", TmonParkingPnl=" + String.format("%.2f", tmonParkingPnl) + ")" +
            ", Expected=" + String.format("%.2f", expectedPnl) +
            ", Deposits=" + String.format("%.2f", totalDeposits) +
            ", Opens=" + totalOpens +
            ", Closes=" + totalCloses +
            ", ConcurrentPeak=" + concurrentPeak +
            ", FinalCash=" + String.format("%.2f", finalSharedCash));
    }
}
