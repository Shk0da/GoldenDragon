package com.github.shk0da.GoldenDragon.test;

import com.github.shk0da.GoldenDragon.config.UnifiedTraderConfig;
import com.github.shk0da.GoldenDragon.strategy.UnifiedStrategy;
import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
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

        public TradeResult(String ticker, String dir, double entry, double exit, double pnl, String reason) {
            this.ticker = ticker;
            this.dir = dir;
            this.entry = entry;
            this.exit = exit;
            this.pnl = pnl;
            this.reason = reason;
        }
    }

    private final String dataDir;
    private final String startDate;
    private final String endDate;
    private final double initialBalance;
    private final int sharesPerTrade;
    private final double commission;

    private final Gson gson = new Gson();

    public BacktestRunner() {
        this("data", "2024-10-01", "2025-06-01", 1_000_000.0, 100, 0.0005);
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
        System.out.println("================================================================================");
        System.out.println("БЭКТЕСТ: LevelTrader vs UnifiedStrategy");
        System.out.println("Период: " + startDate + " - " + endDate);
        System.out.println("Начальный баланс: " + String.format("%,.0f", initialBalance));
        System.out.println("================================================================================");

        List<String> tickers = Arrays.asList("CNYRUBF", "GAZPF", "GLDRUBF", "IMOEXF", "LKOH", "MTSS", "NVTK", "ROSN", "SBERF", "USDRUBF");
        List<TradeResult> ltTrades = new ArrayList<>();
        List<TradeResult> uniTrades = new ArrayList<>();

        for (String ticker : tickers) {
            List<RawCandle> candles = loadCandles(ticker);
            if (candles.size() < 100) {
                System.out.println("\n" + ticker + ": нет данных");
                continue;
            }

            List<TradeResult> ltTradesForTicker = simulateLevelTrader(ticker, candles);
            ltTrades.addAll(ltTradesForTicker);

            UnifiedStrategy unifiedStrategy = new UnifiedStrategy(new UnifiedTraderConfig(), null);
            List<TradeResult> uniTradesForTicker = simulateUnified(unifiedStrategy, ticker, candles);
            uniTrades.addAll(uniTradesForTicker);

            System.out.println("\n" + ticker + ":");
            printTickerDetails("LT", ltTradesForTicker);
            printTickerDetails("UNI", uniTradesForTicker);
        }

        System.out.println("\n" + "=".repeat(72));
        System.out.println("СВОДНАЯ ТАБЛИЦА");
        System.out.println("=".repeat(72));
        System.out.println(String.format("%-10s %7s %9s %10s %12s", "Инструмент", "Сделок", "WinRate", "Ср.PnL", "PnL"));
        System.out.println("-".repeat(50));

        for (String ticker : tickers) {
            List<TradeResult> lt = ltTrades.stream().filter(t -> t.ticker.equals(ticker)).collect(Collectors.toList());
            List<TradeResult> uni = uniTrades.stream().filter(t -> t.ticker.equals(ticker)).collect(Collectors.toList());
            if (lt.isEmpty() && uni.isEmpty()) continue;
            printTradeLine(ticker, "LT", lt);
            printTradeLine(ticker, "UNI", uni);
        }

        System.out.println("-".repeat(50));
        printTradeLine("LT", "LT", ltTrades);
        printTradeLine("UNI", "UNI", uniTrades);
        System.out.println("-".repeat(50));

        double ltPnl = ltTrades.stream().mapToDouble(t -> t.pnl).sum();
        double ltWr = ltTrades.isEmpty() ? 0.0 : (double) ltTrades.stream().filter(t -> t.pnl > 0).count() / ltTrades.size() * 100;
        double uniPnl = uniTrades.stream().mapToDouble(t -> t.pnl).sum();
        double uniWr = uniTrades.isEmpty() ? 0.0 : (double) uniTrades.stream().filter(t -> t.pnl > 0).count() / uniTrades.size() * 100;

        System.out.println();
        System.out.println("LT:  PnL=" + String.format("%,.2f", ltPnl) + " WinRate=" + String.format("%.1f", ltWr) + "% Trades=" + ltTrades.size());
        System.out.println("UNI: PnL=" + String.format("%,.2f", uniPnl) + " WinRate=" + String.format("%.1f", uniWr) + "% Trades=" + uniTrades.size());

        double endBalanceLT = initialBalance + ltPnl * sharesPerTrade;
        double endBalanceUNI = initialBalance + uniPnl * sharesPerTrade;
        System.out.println();
        System.out.println("Конечный баланс LT:  " + String.format("%,.2f", endBalanceLT) + " (" + String.format("%.2f", (endBalanceLT - initialBalance) / initialBalance * 100) + "%)");
        System.out.println("Конечный баланс UNI: " + String.format("%,.2f", endBalanceUNI) + " (" + String.format("%.2f", (endBalanceUNI - initialBalance) / initialBalance * 100) + "%)");
    }

    private void printTradeLine(String label, String strategy, List<TradeResult> trades) {
        int total = trades.size();
        if (total == 0) return;
        long wins = trades.stream().filter(t -> t.pnl > 0).count();
        double wr = (double) wins / total * 100;
        double avgPnl = trades.stream().mapToDouble(t -> t.pnl).sum() / total;
        double pnl = trades.stream().mapToDouble(t -> t.pnl).sum();
        String wrStr = wr >= 60.0 ? String.format("%.1f", wr) + "% ✅" : String.format("%.1f", wr) + "%";
        String avgStr = avgPnl >= 0 ? "+" + String.format("%,.2f", avgPnl) : String.format("%,.2f", avgPnl);
        String pnlStr = pnl >= 0 ? "+" + String.format("%,.2f", pnl) : String.format("%,.2f", pnl);
        System.out.println(String.format("%-10s %7d %9s %10s %12s", label, total, wrStr, avgStr, pnlStr));
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
    }

    private List<RawCandle> loadCandles(String ticker) {
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

    @SuppressWarnings("unchecked")
    private List<Double> loadLevels(String ticker) {
        try {
            File file = new File(dataDir, ticker + "/ticker.json");
            if (!file.exists()) return Collections.emptyList();
            java.lang.reflect.Type mapType = new TypeToken<Map<String, Object>>() {}.getType();
            Map<String, Object> map = gson.fromJson(Files.readString(file.toPath()), mapType);
            Object levelsObj = map.get("levels");
            if (levelsObj instanceof List) {
                return (List<Double>) levelsObj;
            }
            return Collections.emptyList();
        } catch (Exception e) {
            return Collections.emptyList();
        }
    }

    private List<TradeResult> simulateLevelTrader(String ticker, List<RawCandle> candles) {
        List<TradeResult> trades = new ArrayList<>();
        if (candles.size() < 100) return trades;
        List<Double> levels = loadLevels(ticker);
        double slMult = 0.8;
        double tpMult = 1.5;
        double riskP = 0.015;
        String posDir = "";
        double posEntry = 0.0;
        double posSl = 0.0;
        double posTp = 0.0;
        int posHeld = 0;
        int posQty = 0;
        boolean inPos = false;

        for (int i = 60; i < candles.size(); i++) {
            RawCandle c = candles.get(i);
            List<RawCandle> hist = candles.subList(0, i + 1);

            if (inPos) {
                posHeld++;
                boolean slHit = ("BUY".equals(posDir) && c.low <= posSl) || ("SELL".equals(posDir) && c.high >= posSl);
                boolean tpHit = ("BUY".equals(posDir) && c.high >= posTp) || ("SELL".equals(posDir) && c.low <= posTp);

                Double exitPrice = null;
                String reason = null;
                if (tpHit) {
                    exitPrice = posTp;
                    reason = "take_profit";
                } else if (slHit) {
                    exitPrice = posSl;
                    reason = "stop_loss";
                } else if (posHeld >= 48) {
                    exitPrice = c.close;
                    reason = "expired";
                }

                if (exitPrice != null) {
                    double rawPnl = "BUY".equals(posDir) ? exitPrice - posEntry : posEntry - exitPrice;
                    double pnl = rawPnl * posQty * (1 - commission);
                    trades.add(new TradeResult(ticker, posDir, posEntry, exitPrice, pnl, reason));
                    inPos = false;
                }
                continue;
            }

            double atr = calcAtr(hist, 14);
            double maFast = calcSma(hist, 7);
            double maSlow = calcSma(hist, 63);
            boolean trendUp = maFast > maSlow;
            List<RawCandle> avgVolWindow = hist.subList(Math.max(0, hist.size() - 20), hist.size());
            double avgVol = avgVolWindow.stream().mapToDouble(r -> (double) r.volume).average().orElse(0.0);

            RawCandle cForLevels = c;
            boolean nearLevel = levels.stream().anyMatch(lvl -> Math.abs(cForLevels.close - lvl) <= Math.abs(lvl) * 0.01);
            if (!nearLevel) continue;

            List<RawCandle> last5 = hist.subList(Math.max(0, hist.size() - 5), hist.size());
            long upCount = last5.stream().filter(r -> r.close > r.open).count();
            long downCount = last5.stream().filter(r -> r.close < r.open).count();
            boolean isUptrend = upCount >= 3;
            boolean isDowntrend = downCount >= 3;

            RawCandle cForEntry = c;
            if (trendUp && isUptrend
                    && levels.stream().anyMatch(lvl -> cForEntry.low <= lvl * 1.005 && cForEntry.close > lvl)
                    && c.volume > avgVol * 1.5) {
                double entry = c.close;
                double sl = entry - atr * slMult;
                double tp = entry + atr * tpMult;
                double risk = entry - sl;
                if (risk <= 0.0) continue;
                int qty = Math.max(1, (int) Math.floor(initialBalance * riskP / risk));
                posDir = "BUY";
                posEntry = entry;
                posSl = sl;
                posTp = tp;
                posHeld = 0;
                posQty = qty;
                inPos = true;
            } else if (!trendUp && isDowntrend
                    && levels.stream().anyMatch(lvl -> cForEntry.high >= lvl * 0.995 && cForEntry.close < lvl)
                    && c.volume > avgVol * 1.5) {
                double entry = c.close;
                double sl = entry + atr * slMult;
                double tp = entry - atr * tpMult;
                double risk = sl - entry;
                if (risk <= 0.0) continue;
                int qty = Math.max(1, (int) Math.floor(initialBalance * riskP / risk));
                posDir = "SELL";
                posEntry = entry;
                posSl = sl;
                posTp = tp;
                posHeld = 0;
                posQty = qty;
                inPos = true;
            }
        }
        return trades;
    }

    private List<TradeResult> simulateUnified(UnifiedStrategy strategy, String ticker, List<RawCandle> candles) {
        List<TradeResult> trades = new ArrayList<>();
        List<UnifiedStrategy.Candle> wrapped = new ArrayList<>();
        for (RawCandle c : candles) {
            wrapped.add(new UnifiedStrategy.Candle(c.time, c.open, c.high, c.low, c.close, c.volume));
        }
        UnifiedStrategy.Position pos = new UnifiedStrategy.Position();
        double entryPrice = 0.0;
        String entryDir = "BUY";

        for (int i = 60; i < wrapped.size(); i++) {
            List<UnifiedStrategy.Candle> history = wrapped.subList(0, i + 1);
            UnifiedStrategy.Candle current = wrapped.get(i);
            UnifiedStrategy.TradingDecision decision = strategy.decide(ticker, history, pos, initialBalance);

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
                    trades.add(new TradeResult(ticker, dir, entryPrice, exitPrice, pnl, decision.reason));
                    pos = decision.updatedPosition != null ? decision.updatedPosition : new UnifiedStrategy.Position();
                    break;
                case "HOLD":
                    pos = decision.updatedPosition != null ? decision.updatedPosition : pos;
                    break;
            }
        }
        return trades;
    }

    private double calcSma(List<RawCandle> candles, int period) {
        if (candles.size() < period) return candles.get(candles.size() - 1).close;
        return candles.subList(candles.size() - period, candles.size()).stream()
                .mapToDouble(c -> c.close).average().orElse(0.0);
    }

    private double calcAtr(List<RawCandle> candles, int period) {
        if (candles.size() < period + 1) return 0.0;
        double sum = 0.0;
        for (int i = candles.size() - period; i < candles.size(); i++) {
            RawCandle c = candles.get(i);
            RawCandle p = candles.get(i - 1);
            sum += Math.max(Math.max(c.high - c.low, Math.abs(c.high - p.close)), Math.abs(c.low - p.close));
        }
        return sum / period;
    }
}
