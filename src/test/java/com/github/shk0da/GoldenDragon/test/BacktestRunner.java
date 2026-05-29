package com.github.shk0da.GoldenDragon.test;

import com.github.shk0da.GoldenDragon.config.UnifiedTraderConfig;
import com.github.shk0da.GoldenDragon.model.Candle;
import com.github.shk0da.GoldenDragon.model.Position;
import com.github.shk0da.GoldenDragon.model.TradingDecision;
import com.github.shk0da.GoldenDragon.strategy.BaseStrategy;
import com.github.shk0da.GoldenDragon.strategy.HighWinRateStrategy;
import com.github.shk0da.GoldenDragon.strategy.IchimokuStrategy;
import com.github.shk0da.GoldenDragon.strategy.ScalpingStrategy;
import com.github.shk0da.GoldenDragon.strategy.UnifiedStrategy;
import com.github.shk0da.GoldenDragon.utils.PropertiesUtils;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
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

public class BacktestRunner {

    private static class StrategyFactory {
        public static BaseStrategy createStrategy(String strategyName, UnifiedTraderConfig config) {
            switch (strategyName) {
                case "UnifiedStrategy":
                    return new UnifiedStrategy(config, null);
                case "HighWinRateStrategy":
                    return new HighWinRateStrategy(config, null);
                case "ScalpingStrategy":
                    return new ScalpingStrategy(config, null);
                case "IchimokuStrategy":
                    return new IchimokuStrategy(config, null);
                default:
                    throw new IllegalArgumentException("Unknown strategy: " + strategyName);
            }
        }
    }

    private static final DateTimeFormatter DATE_FMT = DateTimeFormatter.ofPattern("dd.MM.yyyy");
    private static final DateTimeFormatter DATE_TIME_FMT = DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm:ss");

    private static final int MIN_HOURS_REQUIRED = 60;
    private static final LocalTime WORK_START_TIME = LocalTime.of(10, 0);
    private static final LocalTime EOD_CLOSE_TIME = LocalTime.of(21, 0);

    private static final String[] ALL_STRATEGIES = {
            "IchimokuStrategy"
    };

    public static class RawCandle {
        public final String time;
        public final double open;
        public final double high;
        public final double low;
        public final double close;
        public final long volume;
        public final LocalDateTime dateTime;

        public RawCandle(String time, double open, double high, double low, double close, long volume) {
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
        public final double pnl;
        public final String reason;
        public final String time;

        public TradeResult(String ticker, String dir, double entry, double exit, double pnl, String reason, String time) {
            this.ticker = ticker;
            this.dir = dir;
            this.entry = entry;
            this.exit = exit;
            this.pnl = pnl;
            this.reason = reason;
            this.time = time;
        }
    }

    private static class EquityPoint {
        final String time;
        final double equity;

        EquityPoint(String time, double equity) {
            this.time = time;
            this.equity = equity;
        }
    }

    private static class SimulateResult {
        final List<TradeResult> trades;
        final List<EquityPoint> equityCurve;
        final double finalBalance;

        SimulateResult(List<TradeResult> trades, List<EquityPoint> equityCurve, double finalBalance) {
            this.trades = trades;
            this.equityCurve = equityCurve;
            this.finalBalance = finalBalance;
        }
    }

    private static class TickerPeriodResult {
        final List<TradeResult> trades;
        final List<EquityPoint> equityCurve;
        final double pnl;
        final double dd;
        final double startBalance;
        final double winRate;

        TickerPeriodResult(List<TradeResult> trades,
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

    private static class PortfolioPeriodResult {
        final double pnl;
        final double dd;
        final List<EquityPoint> equityCurve;
        final int totalTrades;
        final double winRate;

        PortfolioPeriodResult(double pnl, double dd, List<EquityPoint> equityCurve, int totalTrades, double winRate) {
            this.pnl = pnl;
            this.dd = dd;
            this.equityCurve = equityCurve;
            this.totalTrades = totalTrades;
            this.winRate = winRate;
        }
    }

    private final String dataDir;
    private final double initialBalance;
    private final double commission;

    public BacktestRunner() {
        this("data", 1_000_000.0, 0.0005);
    }

    public BacktestRunner(String dataDir, double initialBalance, double commission) {
        this.dataDir = dataDir;
        this.initialBalance = initialBalance;
        this.commission = commission;
    }

    public static void main(String[] args) throws IOException {
        BacktestRunner runner = new BacktestRunner();
        
        // Run for all strategies
        for (String strategyName : ALL_STRATEGIES) {
            System.out.println("\n" + "=".repeat(100));
            System.out.println("RUNNING BACKTEST FOR STRATEGY: " + strategyName);
            System.out.println("=".repeat(100));
            runner.run(strategyName);
        }
    }

    public void run() throws IOException {
        run("UnifiedStrategy");
    }

    public void run(String strategyName) throws IOException {
        String[][] periods = {
                {"2024-01-01", "2024-12-31", "2024"},
                {"2025-01-01", "2025-12-31", "2025"},
                {"2026-01-01", "2026-02-01", "2026.01"},
                {"2026-02-01", "2026-03-01", "2026.02"},
                {"2026-03-01", "2026-04-01", "2026.03"},
                {"2026-04-01", "2026-05-01", "2026.04"},
                {"2026-05-01", "2026-06-01", "2026.05"},
        };

        UnifiedTraderConfig config = new UnifiedTraderConfig();
        List<String> loadedTickers = loadTickers();
        List<String> activeTickers = filterEnabledTickers(loadedTickers, config);

        List<String> periodLabels = new ArrayList<>();
        Map<String, Map<String, TickerPeriodResult>> allData = new LinkedHashMap<>();
        Map<String, PortfolioPeriodResult> portfolioData = new LinkedHashMap<>();

        for (String[] p : periods) {
            String start = p[0];
            String endExclusive = p[1];
            String label = p[2];

            periodLabels.add(label);

            Map<String, TickerPeriodResult> tickerResults = execute(strategyName, start, endExclusive, activeTickers, config);
            allData.put(label, tickerResults);

            PortfolioPeriodResult portfolioResult = buildPortfolioPeriodResult(tickerResults);
            portfolioData.put(label, portfolioResult);
        }

        printResults(strategyName, periodLabels, allData, portfolioData, activeTickers);
    }

    private void printResults(String strategyName,
                              List<String> periodLabels,
                              Map<String, Map<String, TickerPeriodResult>> allData,
                              Map<String, PortfolioPeriodResult> portfolioData,
                              List<String> allTickers) {
        System.out.println("\n" + "=".repeat(170));
        System.out.println("РЕЗУЛЬТАТЫ ПО ПЕРИОДАМ ДЛЯ СТРАТЕГИИ: " + strategyName);
        System.out.println("=".repeat(170));

        StringBuilder header = new StringBuilder();
        header.append(String.format("%-10s", "Тикер"));
        for (String label : periodLabels) {
            header.append(String.format(" %22s", label));
        }
        System.out.println(header);

        StringBuilder subHeader = new StringBuilder();
        subHeader.append(String.format("%-10s", ""));
        for (String ignored : periodLabels) {
            subHeader.append(String.format(" %6s %5s %4s %5s", "PnL", "DD%", "Trd", "WR%"));
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
                    row.append(String.format(" %6s %5s %4d %5.1f",
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
                portRow.append(String.format(" %6s %5s %4d %5.1f",
                        formatCompactPnL(result.pnl),
                        formatCompactDD(result.dd * 100.0),
                        result.totalTrades,
                        result.winRate * 100.0));
            }
        }

        System.out.println(portRow);
        System.out.println("=".repeat(170));
    }

    private Map<String, TickerPeriodResult> execute(String strategyName,
                                                    String start,
                                                    String endExclusive,
                                                    List<String> tickers,
                                                    UnifiedTraderConfig config) throws IOException {
        if (tickers.isEmpty()) {
            return Collections.emptyMap();
        }

        Map<String, Double> capitalAllocation = buildCapitalAllocation(tickers, config);
        Map<String, List<Candle>> allHourlyCandles = new LinkedHashMap<>();

        for (String ticker : tickers) {
            List<RawCandle> raw = loadCandles(ticker, start, endExclusive);
            if (raw.size() < MIN_HOURS_REQUIRED) continue;

            List<Candle> wrapped = new ArrayList<>(raw.size());
            for (RawCandle c : raw) {
                wrapped.add(new Candle(c.time, c.open, c.high, c.low, c.close, c.volume));
            }
            allHourlyCandles.put(ticker, wrapped);
        }

        Map<String, List<String>> groupTickers = new LinkedHashMap<>();
        for (String ticker : allHourlyCandles.keySet()) {
            String allocGroup = config.getTickerParams(ticker).allocationGroup;
            if (allocGroup != null && !allocGroup.isEmpty()) {
                groupTickers.computeIfAbsent(allocGroup, k -> new ArrayList<>()).add(ticker);
            }
        }

        Map<String, TickerPeriodResult> tickerResults = new LinkedHashMap<>();

        for (String ticker : allHourlyCandles.keySet()) {
            UnifiedTraderConfig.TickerParams params = config.getTickerParams(ticker);
            if (!params.enabled) continue;

            List<Candle> hourCandles = allHourlyCandles.get(ticker);
            BaseStrategy strategy = StrategyFactory.createStrategy(strategyName, config);

            boolean useMinCandles = params.useMinuteCandles;
            List<RawCandle> minuteCandlesRaw = useMinCandles
                    ? loadCandles5Min(ticker, start, endExclusive)
                    : loadCandles(ticker, start, endExclusive);

            if (minuteCandlesRaw.isEmpty()) continue;

            double startBalance = capitalAllocation.getOrDefault(ticker, 0.0);
            if (startBalance <= 0.0) continue;

            SimulateResult result = simulateUnifiedLongOnly(
                    strategy,
                    ticker,
                    hourCandles,
                    minuteCandlesRaw,
                    startBalance,
                    allHourlyCandles,
                    groupTickers,
                    config
            );

            double pnl = result.finalBalance - startBalance;
            double dd = calcMaxDrawdownByEquity(result.equityCurve);
            double winRate = calculateWinRate(result.trades);

            tickerResults.put(ticker, new TickerPeriodResult(
                    result.trades,
                    result.equityCurve,
                    pnl,
                    dd,
                    startBalance,
                    winRate
            ));
        }

        return tickerResults;
    }

    private SimulateResult simulateUnifiedLongOnly(BaseStrategy strategy,
                                                   String ticker,
                                                   List<Candle> wrappedHour,
                                                   List<RawCandle> minuteCandlesRaw,
                                                   double startBalance,
                                                   Map<String, List<Candle>> allHourlyCandles,
                                                   Map<String, List<String>> groupTickers,
                                                   UnifiedTraderConfig strategyConfig) {
        if (wrappedHour == null || wrappedHour.isEmpty() || minuteCandlesRaw == null || minuteCandlesRaw.isEmpty()) {
            return new SimulateResult(Collections.emptyList(), Collections.emptyList(), startBalance);
        }

        List<TradeResult> trades = new ArrayList<>();
        List<EquityPoint> equityCurve = new ArrayList<>();

        List<Candle> wrappedMin = new ArrayList<>(minuteCandlesRaw.size());
        List<LocalDateTime> minTimes = new ArrayList<>(minuteCandlesRaw.size());
        for (RawCandle c : minuteCandlesRaw) {
            wrappedMin.add(new Candle(c.time, c.open, c.high, c.low, c.close, c.volume));
            minTimes.add(c.dateTime);
        }

        List<LocalDateTime> hourTimes = new ArrayList<>(wrappedHour.size());
        for (Candle c : wrappedHour) {
            hourTimes.add(LocalDateTime.parse(c.time, DATE_TIME_FMT));
        }

        Map<String, List<LocalDateTime>> peerTimesMap = new HashMap<>();
        for (Map.Entry<String, List<Candle>> e : allHourlyCandles.entrySet()) {
            List<LocalDateTime> peerTimes = new ArrayList<>(e.getValue().size());
            for (Candle c : e.getValue()) {
                peerTimes.add(LocalDateTime.parse(c.time, DATE_TIME_FMT));
            }
            peerTimesMap.put(e.getKey(), peerTimes);
        }

        double cash = startBalance;
        Position pos = new Position();
        double entryPrice = 0.0;
        int hourIdx = -1;
        int lastSeenHourIdx = -1;
        LocalDate lastEodCloseDate = null;
        for (int i = 0; i < wrappedMin.size(); i++) {
            Candle current = wrappedMin.get(i);
            LocalDateTime minDt = minTimes.get(i);

            if (!isTradingDay(minDt.toLocalDate()) || !isWithinWorkingHours(minDt.toLocalTime())) {
                if (pos.quantity > 0
                        && !minDt.toLocalTime().isBefore(EOD_CLOSE_TIME)
                        && !minDt.toLocalDate().equals(lastEodCloseDate)) {

                    double exitPrice = current.close;
                    int q = pos.quantity;

                    double exitValue = q * exitPrice;
                    double entryValue = q * entryPrice;
                    double totalCommission = (entryValue + exitValue) * commission;
                    double pnl = exitValue - entryValue - totalCommission;

                    trades.add(new TradeResult(
                            ticker, "BUY", entryPrice, exitPrice, pnl, "eod_close", current.time
                    ));

                    double exitCommission = exitValue * commission;
                    cash += (exitValue - exitCommission);
                    pos = new Position();
                    entryPrice = 0.0;
                    lastEodCloseDate = minDt.toLocalDate();
                }

                double offHoursEquity = cash;
                if (pos.quantity > 0) {
                    offHoursEquity += pos.quantity * current.close;
                }
                equityCurve.add(new EquityPoint(current.time, offHoursEquity));
                continue;
            }

            while (hourIdx + 1 < hourTimes.size() && !hourTimes.get(hourIdx + 1).isAfter(minDt)) {
                hourIdx++;
            }

            boolean hourChanged = hourIdx != lastSeenHourIdx;

            double equity = cash;
            if (pos.quantity > 0) {
                equity += pos.quantity * current.close;
            }
            equityCurve.add(new EquityPoint(current.time, equity));

            if (hourIdx + 1 < MIN_HOURS_REQUIRED) {
                lastSeenHourIdx = hourIdx;
                continue;
            }

            List<Candle> hourHistory = wrappedHour.subList(0, hourIdx + 1);
            List<Candle> minHistory = wrappedMin.subList(0, i + 1);

            Map<String, List<Candle>> currentPeerCandles = buildCurrentPeerCandles(
                    ticker, minDt, allHourlyCandles, groupTickers, peerTimesMap, hourHistory, strategyConfig
            );
            if (!currentPeerCandles.isEmpty()) {
                strategy.setPeerCandles(currentPeerCandles);
            } else {
                strategy.setPeerCandles(Collections.emptyMap());
            }

            TradingDecision decision = strategy.decide(ticker, hourHistory, minHistory, pos, cash, hourChanged);

            switch (decision.action) {
                case "OPEN":
                    if (decision.updatedPosition == null || decision.quantity <= 0) break;
                    if (!"BUY".equals(decision.updatedPosition.direction)) break;
                    if (pos.quantity > 0) break;

                    double openEntry = decision.updatedPosition.entryPrice != null
                            ? decision.updatedPosition.entryPrice
                            : current.close;

                    int openQty = decision.quantity;
                    double positionValue = openQty * openEntry;
                    double entryCommission = positionValue * commission;

                    if (positionValue + entryCommission > cash) break;

                    cash -= (positionValue + entryCommission);
                    pos = decision.updatedPosition;
                    entryPrice = openEntry;
                    break;

                case "CLOSE":
                    if (pos.quantity <= 0) {
                        pos = decision.updatedPosition != null ? decision.updatedPosition : pos;
                        break;
                    }

                    double exitPrice = decision.entryPrice != null ? decision.entryPrice : current.close;
                    int q = pos.quantity;

                    double exitValue = q * exitPrice;
                    double entryValue = q * entryPrice;
                    double totalCommission = (entryValue + exitValue) * commission;
                    double pnl = exitValue - entryValue - totalCommission;

                    trades.add(new TradeResult(
                            ticker, "BUY", entryPrice, exitPrice, pnl, decision.reason, current.time
                    ));

                    double exitCommission = exitValue * commission;
                    cash += (exitValue - exitCommission);

                    pos = decision.updatedPosition != null ? decision.updatedPosition : new Position();
                    entryPrice = 0.0;
                    break;

                case "HOLD":
                    if (decision.updatedPosition != null) {
                        pos = decision.updatedPosition;
                    }
                    break;
            }

            lastSeenHourIdx = hourIdx;
        }

        double finalBalance = cash;
        if (pos.quantity > 0 && !wrappedMin.isEmpty()) {
            double lastPrice = wrappedMin.get(wrappedMin.size() - 1).close;
            int q = pos.quantity;
            double exitValue = q * lastPrice;
            double entryValue = q * entryPrice;
            double totalCommission = (entryValue + exitValue) * commission;
            double pnl = exitValue - entryValue - totalCommission;

            trades.add(new TradeResult(
                    ticker, "BUY", entryPrice, lastPrice, pnl,
                    "period_end", wrappedMin.get(wrappedMin.size() - 1).time
            ));

            double exitCommission = exitValue * commission;
            finalBalance += (exitValue - exitCommission);
        }

        return new SimulateResult(trades, equityCurve, finalBalance);
    }

    private Map<String, List<Candle>> buildCurrentPeerCandles(String ticker,
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

    private PortfolioPeriodResult buildPortfolioPeriodResult(Map<String, TickerPeriodResult> tickerResults) {
        if (tickerResults == null || tickerResults.isEmpty()) {
            return new PortfolioPeriodResult(0.0, 0.0, Collections.emptyList(), 0, 0.0);
        }

        double totalStartBalance = 0.0;
        double totalFinalBalance = 0.0;
        int totalTrades = 0;

        for (TickerPeriodResult result : tickerResults.values()) {
            totalStartBalance += result.startBalance;
            totalFinalBalance += result.startBalance + result.pnl;
            totalTrades += result.trades.size();
        }

        List<EquityPoint> portfolioEquity = mergePortfolioEquity(tickerResults);
        double dd = calcMaxDrawdownByEquity(portfolioEquity);
        double pnl = totalFinalBalance - totalStartBalance;
        double winRate = calculatePortfolioWinRate(tickerResults);

        return new PortfolioPeriodResult(pnl, dd, portfolioEquity, totalTrades, winRate);
    }

    private List<EquityPoint> mergePortfolioEquity(Map<String, TickerPeriodResult> tickerResults) {
        if (tickerResults == null || tickerResults.isEmpty()) {
            return Collections.emptyList();
        }

        Set<String> allTimes = new TreeSet<>((a, b) -> {
            LocalDateTime ta = LocalDateTime.parse(a, DATE_TIME_FMT);
            LocalDateTime tb = LocalDateTime.parse(b, DATE_TIME_FMT);
            return ta.compareTo(tb);
        });

        for (TickerPeriodResult result : tickerResults.values()) {
            for (EquityPoint point : result.equityCurve) {
                allTimes.add(point.time);
            }
        }

        Map<String, Double> lastEquityByTicker = new HashMap<>();
        Map<String, Integer> indexByTicker = new HashMap<>();
        List<EquityPoint> merged = new ArrayList<>();

        for (String ticker : tickerResults.keySet()) {
            TickerPeriodResult result = tickerResults.get(ticker);
            indexByTicker.put(ticker, 0);
            lastEquityByTicker.put(ticker, result.startBalance);
        }

        for (String time : allTimes) {
            double totalEquity = 0.0;

            for (Map.Entry<String, TickerPeriodResult> entry : tickerResults.entrySet()) {
                String ticker = entry.getKey();
                TickerPeriodResult result = entry.getValue();
                int idx = indexByTicker.get(ticker);

                while (idx < result.equityCurve.size()
                        && compareTime(result.equityCurve.get(idx).time, time) <= 0) {
                    lastEquityByTicker.put(ticker, result.equityCurve.get(idx).equity);
                    idx++;
                }

                indexByTicker.put(ticker, idx);
                totalEquity += lastEquityByTicker.get(ticker);
            }

            merged.add(new EquityPoint(time, totalEquity));
        }

        return merged;
    }

    private int compareTime(String t1, String t2) {
        LocalDateTime d1 = LocalDateTime.parse(t1, DATE_TIME_FMT);
        LocalDateTime d2 = LocalDateTime.parse(t2, DATE_TIME_FMT);
        return d1.compareTo(d2);
    }

    private Map<String, Double> buildCapitalAllocation(List<String> tickers, UnifiedTraderConfig config) {
        Map<String, Double> weights = new LinkedHashMap<>();
        double totalWeight = 0.0;

        for (String ticker : tickers) {
            UnifiedTraderConfig.TickerParams params = config.getTickerParams(ticker);
            if (!params.enabled) continue;

            double weight = params.allocationWeight > 0.0 ? params.allocationWeight : 1.0;
            weights.put(ticker, weight);
            totalWeight += weight;
        }

        if (weights.isEmpty() || totalWeight <= 0.0) {
            return Collections.emptyMap();
        }

        Map<String, Double> allocation = new LinkedHashMap<>();
        for (Map.Entry<String, Double> e : weights.entrySet()) {
            allocation.put(e.getKey(), initialBalance * (e.getValue() / totalWeight));
        }

        return allocation;
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

        for (String s : props.getProperty("datacollector.stocks", "").split(",")) {
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

                    result.add(new RawCandle(
                            parts[0],
                            Double.parseDouble(parts[1]),
                            Double.parseDouble(parts[2]),
                            Double.parseDouble(parts[3]),
                            Double.parseDouble(parts[4]),
                            Long.parseLong(parts[5])
                    ));
                } catch (Exception ignored) {
                }
            }

            result.sort(Comparator.comparing(c -> c.dateTime));
            return result;
        } catch (IOException e) {
            return Collections.emptyList();
        }
    }

    private List<RawCandle> loadCandles5Min(String ticker, String startDate, String endExclusiveDate) {
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

                    result.add(new RawCandle(
                            parts[0],
                            Double.parseDouble(parts[1]),
                            Double.parseDouble(parts[2]),
                            Double.parseDouble(parts[3]),
                            Double.parseDouble(parts[4]),
                            Long.parseLong(parts[5])
                    ));
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

    private double calculatePortfolioWinRate(Map<String, TickerPeriodResult> tickerResults) {
        if (tickerResults == null || tickerResults.isEmpty()) return 0.0;
        
        long totalTrades = 0;
        long winningTrades = 0;
        
        for (TickerPeriodResult result : tickerResults.values()) {
            for (TradeResult trade : result.trades) {
                totalTrades++;
                if (trade.pnl > 0) {
                    winningTrades++;
                }
            }
        }
        
        return totalTrades > 0 ? (double) winningTrades / totalTrades : 0.0;
    }

    private String formatCompactPnL(double pnl) {
        String sign = pnl >= 0 ? "+" : "-";
        double abs = Math.abs(pnl);

        if (abs >= 1_000_000) {
            return String.format("%6s", sign + String.format("%.2f", abs / 1_000_000) + "M");
        }
        if (abs >= 1_000) {
            return String.format("%6s", sign + String.format("%.1f", abs / 1_000) + "K");
        }
        return String.format("%6s", sign + String.format("%.0f", abs));
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
}
