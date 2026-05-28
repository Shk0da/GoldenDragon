package com.github.shk0da.GoldenDragon.test;

import com.github.shk0da.GoldenDragon.config.UnifiedTraderConfig;
import com.github.shk0da.GoldenDragon.model.Candle;
import com.github.shk0da.GoldenDragon.model.Position;
import com.github.shk0da.GoldenDragon.model.TradingDecision;
import com.github.shk0da.GoldenDragon.strategy.UnifiedStrategy;
import com.github.shk0da.GoldenDragon.utils.PropertiesUtils;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.stream.Collectors;

public class BacktestRunner {

    public static class RawCandle {
        public final String time;
        public final double open;
        public final double high;
        public final double low;
        public final double close;
        public final long volume;

        public RawCandle(String time, double open, double high, double low, double close, long volume) {
            this.time = time;
            this.open = open;
            this.high = high;
            this.low = low;
            this.close = close;
            this.volume = volume;
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

    private final String dataDir;
    private final String startDate;
    private final String endDate;
    private final double initialBalance;
    private final int sharesPerTrade;
    private final double commission;

    public BacktestRunner() {
        this("data", "2025-05-01", "2026-05-01", 1_000_000.0, 100, 0.0005);
    }

    public BacktestRunner(String dataDir, String startDate, String endDate, double initialBalance, int sharesPerTrade, double commission) {
        this.dataDir = dataDir;
        this.startDate = startDate;
        this.endDate = endDate;
        this.initialBalance = initialBalance;
        this.sharesPerTrade = sharesPerTrade;
        this.commission = commission;
    }

    public static void main(String[] args) throws IOException {
        new BacktestRunner().run();
    }

    public void run() throws IOException {
        String[][] periods = {
                {"2022-03-01", "2026-05-01", "Full"},
                {"2022-01-01", "2024-12-31", "2022"},
                {"2023-01-01", "2024-12-31", "2023"},
                {"2024-01-01", "2024-12-31", "2024"},
                {"2025-01-01", "2024-12-31", "2025"},
                {"2024-07-01", "2024-09-30", "Q3 2024"},
                {"2025-07-01", "2025-09-30", "Q3 2025"},
                {"2026-01-01", "2026-02-01", "Jan 2026"},
                {"2026-02-01", "2026-03-01", "Feb 2026"},
                {"2026-04-01", "2026-04-01", "Mar 2026"},
                {"2026-04-01", "2026-05-01", "Apr 2026"},
        };

        List<String> periodLabels = new ArrayList<>();
        Map<String, Map<String, List<TradeResult>>> allData = new LinkedHashMap<>();
        List<String> allTickers = new ArrayList<>();

        for (String[] p : periods) {
            String label = p[2];
            periodLabels.add(label);
            System.out.println("\n" + "=".repeat(90));
            System.out.println("ПЕРИОД: " + label + " (" + p[0] + " - " + p[1] + ")");
            System.out.println("=".repeat(90));
            Map<String, List<TradeResult>> tickerTrades = execute(p[0], p[1]);
            allData.put(label, tickerTrades);
            for (String t : tickerTrades.keySet()) {
                if (!allTickers.contains(t)) allTickers.add(t);
            }
        }

        System.out.println("\n" + "=".repeat(90));
        System.out.println("СРАВНЕНИЕ ПРОСАДОК ПО ПЕРИОДАМ");
        System.out.println("=".repeat(90));

        StringBuilder header = new StringBuilder();
        header.append(String.format("%-10s", "Тикер"));
        for (int i = 0; i < periodLabels.size(); i++) {
            if (periodLabels.get(i).equals("Full")) continue;
            header.append(String.format(" %12s", periodLabels.get(i)));
        }
        System.out.println(header);
        System.out.println("-".repeat(header.length()));

        for (String ticker : allTickers) {
            StringBuilder row = new StringBuilder();
            row.append(String.format("%-10s", ticker));
            for (String label : periodLabels) {
                if (label.equals("Full")) continue;
                Map<String, List<TradeResult>> tickerData = allData.get(label);
                List<TradeResult> trades = tickerData != null ? tickerData.get(ticker) : null;
                if (trades == null || trades.isEmpty()) {
                    row.append(String.format(" %12s", "—"));
                } else {
                    double dd = calcMaxDrawdown(trades) * 100;
                    row.append(String.format(" %8s", String.format("%.1f%%", dd)));
                    String risk = assessRisk(dd, 0);
                    if ("Med".equals(risk)) row.append("⚠");
                    else if ("High".equals(risk)) row.append("🔥");
                    else row.append("  ");
                }
            }
            System.out.println(row);
        }

        System.out.println("-".repeat(header.length()));
        StringBuilder portRow = new StringBuilder();
        portRow.append(String.format("%-10s", "ПОРТФЕЛЬ"));
        for (String label : periodLabels) {
            if (label.equals("Full")) continue;
            Map<String, List<TradeResult>> tickerData = allData.get(label);
            List<TradeResult> allPeriodTrades = tickerData != null
                    ? tickerData.values().stream().flatMap(List::stream).collect(Collectors.toList())
                    : Collections.emptyList();
            if (allPeriodTrades.isEmpty()) {
                portRow.append(String.format(" %12s", "—"));
            } else {
                List<TradeResult> sorted = sortByTime(allPeriodTrades);
                double dd = calcMaxDrawdown(sorted) * 100;
                portRow.append(String.format(" %8s", String.format("%.1f%%", dd)));
                String risk = assessRisk(dd, 0);
                if ("Med".equals(risk)) portRow.append("⚠");
                else if ("High".equals(risk)) portRow.append("🔥");
                else portRow.append("  ");
            }
        }
        System.out.println(portRow);
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
                String ticker = key.split("\\.")[2];
                tickers.add(ticker);
            }
        }
        return new ArrayList<>(tickers);
    }

    private Map<String, List<TradeResult>> execute(String start, String end) throws IOException {
        List<String> tickers = loadTickers();
        System.out.println("================================================================================");
        System.out.println("БЭКТЕСТ: UnifiedStrategy");
        System.out.println("Период: " + start + " - " + end);
        System.out.println("Начальный баланс: " + String.format("%,.0f", initialBalance));
        System.out.println("================================================================================");

        Map<String, List<TradeResult>> tickerResults = new LinkedHashMap<>();
        for (String ticker : tickers) {
            List<RawCandle> hourCandles = loadCandles(ticker, start, end);
            if (hourCandles.size() < 100) {
                System.out.println("\n" + ticker + ": нет данных");
                continue;
            }

            UnifiedStrategy unifiedStrategy = new UnifiedStrategy(new UnifiedTraderConfig(), null);
            boolean useMinCandles = unifiedStrategy.getUnifiedTraderConfig().getTickerParams(ticker).useMinuteCandles;

            List<RawCandle> minuteCandles;
            if (useMinCandles) {
                minuteCandles = loadCandles5Min(ticker, start, end);
                if (minuteCandles.isEmpty()) {
                    System.out.println("\n" + ticker + ": нет данных");
                    continue;
                }
            } else {
                minuteCandles = hourCandles;
            }

            List<TradeResult> uniTradesForTicker = simulateUnified(unifiedStrategy, ticker, hourCandles, minuteCandles);
            tickerResults.put(ticker, uniTradesForTicker);

            System.out.println("\n" + ticker + ":");
            printTickerDetails("UNI", uniTradesForTicker);
        }

        List<TradeResult> allTrades = tickerResults.values().stream().flatMap(List::stream).collect(Collectors.toList());
        List<TradeResult> portfolioTrades = sortByTime(allTrades);

        System.out.println("\n" + "=".repeat(90));
        System.out.println("СВОДНАЯ ТАБЛИЦА");
        System.out.println("=".repeat(90));
        System.out.println(String.format("%-10s %7s %9s %12s %8s %7s %4s", "Инструмент", "Сделок", "WinRate", "PnL", "Просадка", "Шарп", "Риск"));
        System.out.println("-".repeat(67));

        for (String ticker : tickers) {
            List<TradeResult> uni = tickerResults.getOrDefault(ticker, Collections.emptyList());
            if (uni.isEmpty()) continue;
            printTradeLine(ticker, "UNI", uni);
        }

        System.out.println("-".repeat(67));
        printTradeLine("ПОРТФЕЛЬ", "UNI", portfolioTrades);
        System.out.println("-".repeat(67));

        double uniPnl = allTrades.stream().mapToDouble(t -> t.pnl).sum();
        double uniWr = allTrades.isEmpty() ? 0.0 : (double) allTrades.stream().filter(t -> t.pnl > 0).count() / allTrades.size() * 100;

        System.out.println();
        System.out.println("UNI: PnL=" + String.format("%,.2f", uniPnl) + " WinRate=" + String.format("%.1f", uniWr) + "% Trades=" + portfolioTrades.size());

        double endBalanceUNI = initialBalance + uniPnl * sharesPerTrade;
        System.out.println();
        System.out.println("Конечный баланс UNI: " + String.format("%,.2f", endBalanceUNI) + " (" + String.format("%.2f", (endBalanceUNI - initialBalance) / initialBalance * 100) + "%)");

        return tickerResults;
    }

    private List<TradeResult> sortByTime(List<TradeResult> trades) {
        DateTimeFormatter fmt = DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm:ss");
        return trades.stream()
                .sorted((a, b) -> {
                    try {
                        return LocalDateTime.parse(a.time, fmt).compareTo(LocalDateTime.parse(b.time, fmt));
                    } catch (Exception e) {
                        return 0;
                    }
                })
                .collect(Collectors.toList());
    }

    private void printTradeLine(String label, String strategy, List<TradeResult> trades) {
        int total = trades.size();
        if (total == 0) return;
        long wins = trades.stream().filter(t -> t.pnl > 0).count();
        double wr = (double) wins / total * 100;
        double pnl = trades.stream().mapToDouble(t -> t.pnl).sum();
        double dd = calcMaxDrawdown(trades) * 100;
        double sharpe = calcSharpe(trades);
        String risk = assessRisk(dd, sharpe);
        String wrStr = wr >= 60.0 ? String.format("%.1f", wr) + "% ✅" : String.format("%.1f", wr) + "%";
        String pnlStr = pnl >= 0 ? "+" + String.format("%,.2f", pnl) : String.format("%,.2f", pnl);
        String ddStr = dd < 10.0 ? String.format("%.1f", dd) : String.format("%.1f", dd) + "⚠";
        String sharpeStr = sharpe >= 1.0 ? String.format("%.2f", sharpe) : String.format("%.2f", sharpe);
        System.out.println(String.format("%-10s %7d %9s %12s %8s %7s %4s", label, total, wrStr, pnlStr, ddStr + "%", sharpeStr, risk));
    }

    private void printTickerDetails(String strategy, List<TradeResult> trades) {
        int total = trades.size();
        long wins = trades.stream().filter(t -> t.pnl > 0).count();
        double wr = total > 0 ? (double) wins / total * 100 : 0.0;
        double totalPnl = trades.stream().mapToDouble(t -> t.pnl).sum();
        double avgPnl = total > 0 ? totalPnl / total : 0.0;

        long tp = trades.stream().filter(t -> "take_profit".equals(t.reason)).count();
        long sl = trades.stream().filter(t -> "stop_loss".equals(t.reason)).count();
        long expired = trades.stream().filter(t -> "expired".equals(t.reason)).count();
        long unknown = total - tp - sl - expired;

        System.out.print("  " + strategy + ": ");
        System.out.println(total + " сделок | " + String.format("%.1f", wr) + "% (" + wins + "/" + total + ") | ср." + String.format("%,+.2f", avgPnl) + " | PnL " + String.format("%,+.2f", totalPnl));
        System.out.println("    → TP:" + tp + "  SL:" + sl + "  Expired:" + expired + "  Unknown:" + unknown);

        if (total > 0) {
            double maxDd = calcMaxDrawdown(trades);
            double sharpe = calcSharpe(trades);
            double pf = calcProfitFactor(trades);
            String risk = assessRisk(maxDd * 100, sharpe);
            System.out.println("    → MaxDD: " + String.format("%.1f", maxDd * 100) + "% | Sharpe: " + String.format("%.2f", sharpe) + " | PF: " + String.format("%.2f", pf) + " | Risk: " + risk);
        }
    }

    private List<RawCandle> loadCandles(String ticker, String startDate, String endDate) {
        File file = new File(dataDir, ticker + "/candlesHOUR.txt");
        if (!file.exists()) return Collections.emptyList();

        DateTimeFormatter fmt = DateTimeFormatter.ofPattern("dd.MM.yyyy");
        LocalDate start = LocalDate.parse(startDate);
        LocalDate end = LocalDate.parse(endDate);

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
                    LocalDate dt = LocalDate.parse(datePart, fmt);
                    if (dt.isBefore(start) || dt.isAfter(end)) continue;
                    result.add(new RawCandle(
                            parts[0], Double.parseDouble(parts[1]), Double.parseDouble(parts[2]),
                            Double.parseDouble(parts[3]), Double.parseDouble(parts[4]), Long.parseLong(parts[5])
                    ));
                } catch (Exception ignored) {
                }
            }
            return result;
        } catch (IOException e) {
            return Collections.emptyList();
        }
    }
    private List<TradeResult> simulateUnified(UnifiedStrategy strategy, String ticker, List<RawCandle> hourCandles, List<RawCandle> minuteCandles) {
        List<TradeResult> trades = new ArrayList<>();
        List<Candle> wrappedHour = new ArrayList<>();
        for (RawCandle c : hourCandles) {
            wrappedHour.add(new Candle(c.time, c.open, c.high, c.low, c.close, c.volume));
        }
        List<Candle> wrappedMin = new ArrayList<>();
        for (RawCandle c : minuteCandles) {
            wrappedMin.add(new Candle(c.time, c.open, c.high, c.low, c.close, c.volume));
        }
        Position pos = new Position();
        double entryPrice = 0.0;
        String entryDir = "BUY";

        DateTimeFormatter dtf = DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm:ss");
        int hourIdx = 0;

        for (int i = 0; i < wrappedMin.size(); i++) {
            Candle current = wrappedMin.get(i);
            LocalDateTime minDt = LocalDateTime.parse(current.time, dtf);

            while (hourIdx < wrappedHour.size()) {
                LocalDateTime hourDt = LocalDateTime.parse(wrappedHour.get(hourIdx).time, dtf);
                if (!hourDt.isAfter(minDt)) {
                    hourIdx++;
                } else {
                    break;
                }
            }

            if (hourIdx < 60) continue;

            List<Candle> hourHistory = wrappedHour.subList(0, hourIdx);
            List<Candle> minHistory = wrappedMin.subList(0, i + 1);
            TradingDecision decision = strategy.decide(ticker, hourHistory, minHistory, pos, initialBalance);

            switch (decision.action) {
                case "OPEN":
                    if (decision.updatedPosition == null) continue;
                    pos = decision.updatedPosition;
                    entryPrice = pos.entryPrice != null ? pos.entryPrice : current.close;
                    entryDir = pos.direction != null ? pos.direction : "BUY";
                    break;
                case "CLOSE":
                    if (pos.quantity <= 0) {
                        pos = decision.updatedPosition != null ? decision.updatedPosition : pos;
                        continue;
                    }
                    double exitPrice;
                    switch (decision.reason) {
                        case "take_profit":
                            exitPrice = pos.takeProfit != null ? pos.takeProfit : current.close;
                            break;
                        case "stop_loss":
                            exitPrice = pos.stopLoss != null ? pos.stopLoss : current.close;
                            break;
                        default:
                            exitPrice = decision.entryPrice != null ? decision.entryPrice : current.close;
                            break;
                    }
                    String dir = entryDir;
                    int q = pos.quantity;
                    double pnl = UnifiedStrategy.calculatePnl(dir, entryPrice, exitPrice, q, commission);
                    trades.add(new TradeResult(ticker, dir, entryPrice, exitPrice, pnl, decision.reason, current.time));
                    pos = decision.updatedPosition != null ? decision.updatedPosition : new Position();
                    break;
                case "HOLD":
                    pos = decision.updatedPosition != null ? decision.updatedPosition : pos;
                    break;
            }
        }
        return trades;
    }

    private List<RawCandle> loadCandles5Min(String ticker, String startDate, String endDate) {
        File file = new File(dataDir, ticker + "/candles5_MIN.txt");
        if (!file.exists()) return Collections.emptyList();

        DateTimeFormatter fmt = DateTimeFormatter.ofPattern("dd.MM.yyyy");
        LocalDate start = LocalDate.parse(startDate);
        LocalDate end = LocalDate.parse(endDate);

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
                    LocalDate dt = LocalDate.parse(datePart, fmt);
                    if (dt.isBefore(start) || dt.isAfter(end)) continue;
                    result.add(new RawCandle(
                            parts[0], Double.parseDouble(parts[1]), Double.parseDouble(parts[2]),
                            Double.parseDouble(parts[3]), Double.parseDouble(parts[4]), Long.parseLong(parts[5])
                    ));
                } catch (Exception ignored) {
                }
            }
            return result;
        } catch (IOException e) {
            return Collections.emptyList();
        }
    }

    private double calcMaxDrawdown(List<TradeResult> trades) {
        double peak = initialBalance;
        double maxDd = 0;
        double equity = initialBalance;
        for (TradeResult t : trades) {
            equity += t.pnl;
            if (equity > peak) peak = equity;
            double dd = (peak - equity) / peak;
            if (dd > maxDd) maxDd = dd;
        }
        return maxDd;
    }

    private double calcSharpe(List<TradeResult> trades) {
        if (trades.size() < 2) return 0;
        double mean = trades.stream().mapToDouble(t -> t.pnl).average().orElse(0);
        double variance = trades.stream().mapToDouble(t -> Math.pow(t.pnl - mean, 2)).average().orElse(0);
        double std = Math.sqrt(variance);
        if (std == 0) return mean >= 0 ? 10.0 : -10.0;
        return mean / std;
    }

    private double calcProfitFactor(List<TradeResult> trades) {
        double grossProfit = trades.stream().filter(t -> t.pnl > 0).mapToDouble(t -> t.pnl).sum();
        double grossLoss = trades.stream().filter(t -> t.pnl < 0).mapToDouble(t -> Math.abs(t.pnl)).sum();
        if (grossLoss == 0) return Double.POSITIVE_INFINITY;
        return grossProfit / grossLoss;
    }

    private String assessRisk(double ddPct, double sharpe) {
        if (ddPct < 10.0) return "Low";
        if (ddPct < 25.0) return "Med";
        return "High";
    }

}
