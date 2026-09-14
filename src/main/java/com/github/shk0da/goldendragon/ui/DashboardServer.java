package com.github.shk0da.goldendragon.ui;

import com.github.shk0da.goldendragon.model.PositionInfo;
import com.github.shk0da.goldendragon.model.TickerInfo;
import com.github.shk0da.goldendragon.model.TickerType;
import com.github.shk0da.goldendragon.service.TradingService;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.sun.net.httpserver.HttpServer;

import static com.github.shk0da.goldendragon.money.CashParkingManager.TINKOFF_PARKING_TICKER;

import java.io.IOException;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Executors;

/**
 * Simple HTTP dashboard server for trading statistics.
 * Runs on port 1040 and provides real-time portfolio view.
 */
public class DashboardServer {

    private static final int PORT = 1040;
    private static final long POLL_INTERVAL_SECONDS = 120;
    private static final Gson gson = new GsonBuilder().setPrettyPrinting().create();

    private final HttpServer server;
    private final TradingService tradingService;
    private final String currency;
    private int port;
    private final long pollIntervalSeconds;

    private static final LocalTime DEFAULT_END_OF_DAY = LocalTime.of(19, 0);

    private double totalPnl = 0.0;
    private int totalTrades = 0;
    private int winningTrades = 0;
    private double balance = 0.0;
    private final List<Map<String, Object>> tradeHistory = Collections.synchronizedList(new ArrayList<>());
    private volatile Instant appStartTime;
    private volatile java.util.concurrent.ScheduledExecutorService tradePoller;
    private final LocalTime endOfDay;

    public DashboardServer(TradingService tradingService) throws IOException {
        this(tradingService, PORT, POLL_INTERVAL_SECONDS, DEFAULT_END_OF_DAY);
    }

    public DashboardServer(TradingService tradingService, int port) throws IOException {
        this(tradingService, port, POLL_INTERVAL_SECONDS, DEFAULT_END_OF_DAY);
    }

    public DashboardServer(TradingService tradingService, int port, long pollIntervalSeconds) throws IOException {
        this(tradingService, port, pollIntervalSeconds, DEFAULT_END_OF_DAY);
    }

    public DashboardServer(TradingService tradingService, LocalTime endOfDay) throws IOException {
        this(tradingService, PORT, POLL_INTERVAL_SECONDS, endOfDay);
    }

    public DashboardServer(TradingService tradingService, int port, long pollIntervalSeconds, LocalTime endOfDay) throws IOException {
        this.tradingService = tradingService;
        this.currency = "RUB";
        this.port = port;
        this.pollIntervalSeconds = pollIntervalSeconds;
        this.endOfDay = endOfDay;
        this.server = createServer(port);
        this.server.createContext("/", this::handleRequest);
        this.server.setExecutor(Executors.newFixedThreadPool(4));
        this.appStartTime = Instant.now();
    }

    /**
     * Create the HTTP server on the requested port, falling back to the next port
     * when the requested one is already in use.
     */
    private HttpServer createServer(int requestedPort) throws IOException {
        try {
            return HttpServer.create(new InetSocketAddress(requestedPort), 0);
        } catch (IOException ex) {
            int fallbackPort = requestedPort + 1;
            System.err.println(
                    "Dashboard port " + requestedPort + " is busy, trying " + fallbackPort);
            this.port = fallbackPort;
            return HttpServer.create(new InetSocketAddress(fallbackPort), 0);
        }
    }

    private void loadTradeHistory() {
        try {
            Instant todayStart = Instant.now().atZone(ZoneId.systemDefault()).toLocalDate()
                    .atStartOfDay(ZoneId.systemDefault()).toInstant();
            List<Map<String, Object>> trades = tradingService.getTradeHistory(todayStart);
            synchronized (tradeHistory) {
                tradeHistory.clear();
                tradeHistory.addAll(trades);
                calculateStats();
            }
        } catch (Exception ex) {
            System.err.println("Failed to load trade history: " + ex.getMessage());
            ex.printStackTrace();
        }
    }

    private void calculateStats() {
        this.totalPnl = 0.0;
        this.winningTrades = 0;
        this.totalTrades = 0;

        for (Map<String, Object> trade : tradeHistory) {
            // Count only SELL operations (closed positions) for PnL and Win Rate,
            // excluding the cash parking ticker
            if (isCountableTrade(trade)) {
                this.totalTrades++;
                Object pnlObj = trade.get("pnl");
                if (pnlObj instanceof Number) {
                    double pnl = ((Number) pnlObj).doubleValue();
                    this.totalPnl += pnl;
                    if (pnl > 0) {
                        this.winningTrades++;
                    }
                }
            }
        }
    }

    static boolean isCountableTrade(Map<String, Object> trade) {
        String type = (String) trade.get("type");
        String ticker = (String) trade.get("ticker");
        return "SELL".equals(type) && !TINKOFF_PARKING_TICKER.equals(ticker);
    }

    public int getPort() {
        return port;
    }

    public void start() {
        server.start();
        // Don't poll trade history - load on-demand when /api/trades is requested
    }

    public void stop() {
        server.stop(0);
        if (tradePoller != null) {
            tradePoller.shutdown();
            try {
                if (!tradePoller.awaitTermination(5, java.util.concurrent.TimeUnit.SECONDS)) {
                    tradePoller.shutdownNow();
                }
            } catch (InterruptedException ex) {
                tradePoller.shutdownNow();
                Thread.currentThread().interrupt();
            }
        }
    }

    private void handleRequest(com.sun.net.httpserver.HttpExchange exchange) throws IOException {
        String path = exchange.getRequestURI().getPath();

        if ("/api/stats".equals(path)) {
            handleStats(exchange);
        } else if ("/api/positions/close".equals(path)) {
            handleClosePosition(exchange);
        } else if ("/api/positions".equals(path)) {
            handlePositions(exchange);
        } else if ("/api/trades".equals(path)) {
            handleTrades(exchange);
        } else {
            handleDashboard(exchange);
        }
    }

    private void handleDashboard(com.sun.net.httpserver.HttpExchange exchange) throws IOException {
        String html = getDashboardHtml();
        byte[] response = html.getBytes(StandardCharsets.UTF_8);
        exchange.getResponseHeaders().set("Content-Type", "text/html; charset=UTF-8");
        exchange.sendResponseHeaders(200, response.length);
        try (OutputStream os = exchange.getResponseBody()) {
            os.write(response);
        }
    }

    private void handleStats(com.sun.net.httpserver.HttpExchange exchange) throws IOException {
        Map<String, Object> stats = new HashMap<>();
        stats.put("balance", balance);
        stats.put("totalPnl", totalPnl);
        stats.put("totalTrades", totalTrades);
        stats.put("winningTrades", winningTrades);
        stats.put("winRate", totalTrades > 0 ? (double) winningTrades / totalTrades : 0.0);
        stats.put("currency", currency);
        // PnL as percentage of portfolio (balance + pnl)
        double portfolioValue = balance + totalPnl;
        stats.put("pnlPercent", portfolioValue > 0 ? (totalPnl / portfolioValue) * 100 : 0.0);

        String response = gson.toJson(stats);
        byte[] bytes = response.getBytes(StandardCharsets.UTF_8);
        exchange.getResponseHeaders().set("Content-Type", "application/json");
        exchange.getResponseHeaders().set("Access-Control-Allow-Origin", "*");
        exchange.sendResponseHeaders(200, bytes.length);
        try (OutputStream os = exchange.getResponseBody()) {
            os.write(bytes);
        }
    }

    private void handlePositions(com.sun.net.httpserver.HttpExchange exchange) throws IOException {
        List<Map<String, Object>> positions = new ArrayList<>();

        if (tradingService != null) {
            try {
                Map<TickerInfo.Key, PositionInfo> stockPositions = tradingService.getCurrentPositions(
                        com.github.shk0da.goldendragon.model.TickerType.STOCK);
                Map<TickerInfo.Key, PositionInfo> featurePositions = tradingService.getCurrentPositions(
                        com.github.shk0da.goldendragon.model.TickerType.FEATURE);
                Map<TickerInfo.Key, PositionInfo> cryptoPositions = tradingService.getCurrentPositions(
                        com.github.shk0da.goldendragon.model.TickerType.CRYPTO);

                Map<TickerInfo.Key, PositionInfo> allPositions = new HashMap<>();
                allPositions.putAll(stockPositions);
                allPositions.putAll(featurePositions);
                allPositions.putAll(cryptoPositions);

                for (Map.Entry<TickerInfo.Key, PositionInfo> entry : allPositions.entrySet()) {
                    PositionInfo pos = entry.getValue();
                    if (pos.getBalance() != null && pos.getBalance() != 0) {
                        Map<String, Object> positionData = new HashMap<>();
                        positionData.put("ticker", pos.getTicker());
                        positionData.put("balance", pos.getBalance());
                        positionData.put("expectedYield", pos.getExpectedYield());
                        positionData.put("averagePrice", pos.getAveragePositionPrice());
                        positions.add(positionData);
                    }
                }
            } catch (Exception e) {
                // Ignore
            }
        }

        String response = gson.toJson(positions);
        byte[] bytes = response.getBytes(StandardCharsets.UTF_8);
        exchange.getResponseHeaders().set("Content-Type", "application/json");
        exchange.getResponseHeaders().set("Access-Control-Allow-Origin", "*");
        exchange.sendResponseHeaders(200, bytes.length);
        try (OutputStream os = exchange.getResponseBody()) {
            os.write(bytes);
        }
    }

    private void handleTrades(com.sun.net.httpserver.HttpExchange exchange) throws IOException {
        // Load trade history on-demand when requested
        loadTradeHistory();

        List<Map<String, Object>> trades;
        synchronized (tradeHistory) {
            trades = new ArrayList<>(tradeHistory);
        }
        String response = gson.toJson(trades);
        byte[] bytes = response.getBytes(StandardCharsets.UTF_8);
        exchange.getResponseHeaders().set("Content-Type", "application/json");
        exchange.getResponseHeaders().set("Access-Control-Allow-Origin", "*");
        exchange.sendResponseHeaders(200, bytes.length);
        try (OutputStream os = exchange.getResponseBody()) {
            os.write(bytes);
        }
    }

    private void handleClosePosition(com.sun.net.httpserver.HttpExchange exchange) throws IOException {
        String body = new String(exchange.getRequestBody().readAllBytes(), StandardCharsets.UTF_8);
        Map<String, Object> request = gson.fromJson(body, Map.class);

        String ticker = (String) request.get("ticker");
        Double fractionObj = (Double) request.get("fraction");
        double fraction = fractionObj != null ? fractionObj : 1.0;

        Map<String, Object> result = new HashMap<>();

        if (ticker == null || fraction <= 0) {
            result.put("success", false);
            result.put("error", "Invalid parameters");
        } else {
            try {
                TickerInfo info = tradingService.searchTicker(new TickerInfo.Key(ticker, TickerType.STOCK));
                if (info == null) {
                    info = tradingService.searchTicker(new TickerInfo.Key(ticker, TickerType.FEATURE));
                }
                if (info == null) {
                    info = tradingService.searchTicker(new TickerInfo.Key(ticker, TickerType.CRYPTO));
                }

                if (info == null) {
                    result.put("success", false);
                    result.put("error", "Ticker not found: " + ticker);
                } else {
                    Map<TickerInfo.Key, PositionInfo> positions = tradingService.getCurrentPositions(info.getKey().getType());
                    PositionInfo pos = positions.get(info.getKey());
                    if (pos == null || pos.getBalance() == 0) {
                        result.put("success", false);
                        result.put("error", "No open position for " + ticker);
                    } else {
                        // Calculate quantity to close based on fraction
                        int balance = pos.getBalance();
                        int quantityToClose = (int) Math.floor(Math.abs(balance) * fraction);
                        if (quantityToClose <= 0) {
                            result.put("success", false);
                            result.put("error", "Calculated quantity is 0 (balance=" + balance + ", fraction=" + fraction + ")");
                        } else if (quantityToClose > Math.abs(balance)) {
                            quantityToClose = Math.abs(balance); // Don't close more than position size
                        }

                        // Close based on balance sign: positive = long, negative = short
                        boolean closed;
                        if (balance > 0) {
                            closed = tradingService.closeLongByMarketWithDetails(ticker, info.getKey().getType(), quantityToClose) != null;
                        } else {
                            closed = tradingService.closeShortByMarketWithDetails(ticker, info.getKey().getType(), quantityToClose) != null;
                        }
                        result.put("success", closed);
                        result.put("quantity", quantityToClose);
                        if (!closed) {
                            result.put("error", "Failed to close position");
                        } else {
                            result.put("message", "Closed " + quantityToClose + " of " + balance + " (" + (fraction * 100) + "%)");
                        }
                    }
                }
            } catch (Exception e) {
                result.put("success", false);
                result.put("error", e.getMessage());
                e.printStackTrace();
            }
        }

        String response = gson.toJson(result);
        byte[] bytes = response.getBytes(StandardCharsets.UTF_8);
        exchange.getResponseHeaders().set("Content-Type", "application/json");
        exchange.getResponseHeaders().set("Access-Control-Allow-Origin", "*");
        exchange.sendResponseHeaders(200, bytes.length);
        try (OutputStream os = exchange.getResponseBody()) {
            os.write(bytes);
        }
    }

    public void addTrade(String ticker, String type, int quantity, double price, double pnl) {
        Map<String, Object> trade = new LinkedHashMap<>();
        trade.put("time", new Date());
        trade.put("ticker", ticker);
        trade.put("type", type);
        trade.put("quantity", quantity);
        trade.put("price", price);
        trade.put("pnl", pnl);
        tradeHistory.add(trade);
        // Keep only last 100 trades
        synchronized (tradeHistory) {
            while (tradeHistory.size() > 100) {
                tradeHistory.remove(0);
            }
        }
    }

    public void updateStats(double pnl, boolean isWin) {
        this.totalPnl += pnl;
        this.totalTrades++;
        if (isWin) {
            this.winningTrades++;
        }
    }

    public void updateBalance(double balance) {
        this.balance = balance;
    }

    private String getDashboardHtml() {
        StringBuilder sb = new StringBuilder();
        sb.append("<!DOCTYPE html>\n");
        sb.append("<html lang=\"ru\">\n");
        sb.append("<head>\n");
        sb.append("    <meta charset=\"UTF-8\">\n");
        sb.append("    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n");
        sb.append("    <title>GoldenDragon</title>\n");
        sb.append("    <style>\n");
        sb.append("        :root {\n");
        sb.append("            --tinkoff-yellow: #ffdd2d;\n");
        sb.append("            --tinkoff-black: #333333;\n");
        sb.append("            --tinkoff-gray: #f5f5f5;\n");
        sb.append("            --tinkoff-green: #27ae60;\n");
        sb.append("            --tinkoff-red: #e74c3c;\n");
        sb.append("        }\n");
        sb.append("        * {\n");
        sb.append("            margin: 0;\n");
        sb.append("            padding: 0;\n");
        sb.append("            box-sizing: border-box;\n");
        sb.append("        }\n");
        sb.append("        body {\n");
        sb.append("            font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;\n");
        sb.append("            background: var(--tinkoff-gray);\n");
        sb.append("            color: var(--tinkoff-black);\n");
        sb.append("        }\n");
        sb.append("        .header {\n");
        sb.append("            background: var(--tinkoff-black);\n");
        sb.append("            color: white;\n");
        sb.append("            padding: 20px;\n");
        sb.append("            display: flex;\n");
        sb.append("            justify-content: space-between;\n");
        sb.append("            align-items: center;\n");
        sb.append("        }\n");
        sb.append("        .header h1 {\n");
        sb.append("            font-size: 24px;\n");
        sb.append("        }\n");
        sb.append("        .header .logo {\n");
        sb.append("            background: var(--tinkoff-yellow);\n");
        sb.append("            color: var(--tinkoff-black);\n");
        sb.append("            padding: 8px 16px;\n");
        sb.append("            border-radius: 4px;\n");
        sb.append("            font-weight: bold;\n");
        sb.append("        }\n");
        sb.append("        .container {\n");
        sb.append("            max-width: 1400px;\n");
        sb.append("            margin: 0 auto;\n");
        sb.append("            padding: 20px;\n");
        sb.append("        }\n");
        sb.append("        .stats-grid {\n");
        sb.append("            display: grid;\n");
        sb.append("            grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));\n");
        sb.append("            gap: 20px;\n");
        sb.append("            margin-bottom: 30px;\n");
        sb.append("        }\n");
        sb.append("        .stat-card {\n");
        sb.append("            background: white;\n");
        sb.append("            padding: 24px;\n");
        sb.append("            border-radius: 8px;\n");
        sb.append("            box-shadow: 0 2px 4px rgba(0,0,0,0.1);\n");
        sb.append("        }\n");
        sb.append("        .stat-card h3 {\n");
        sb.append("            font-size: 14px;\n");
        sb.append("            color: #666;\n");
        sb.append("            margin-bottom: 8px;\n");
        sb.append("            text-transform: uppercase;\n");
        sb.append("        }\n");
        sb.append("        .stat-card .value {\n");
        sb.append("            font-size: 32px;\n");
        sb.append("            font-weight: bold;\n");
        sb.append("        }\n");
        sb.append("        .stat-card .value.positive {\n");
        sb.append("            color: var(--tinkoff-green);\n");
        sb.append("        }\n");
        sb.append("        .stat-card .value.negative {\n");
        sb.append("            color: var(--tinkoff-red);\n");
        sb.append("        }\n");
        sb.append("        .stat-card .value .pnl-percent {\n");
        sb.append("            font-size: 16px;\n");
        sb.append("            color: #888;\n");
        sb.append("        }\n");
        sb.append("        .section {\n");
        sb.append("            background: white;\n");
        sb.append("            border-radius: 8px;\n");
        sb.append("            padding: 24px;\n");
        sb.append("            margin-bottom: 20px;\n");
        sb.append("            box-shadow: 0 2px 4px rgba(0,0,0,0.1);\n");
        sb.append("        }\n");
        sb.append("        .section h2 {\n");
        sb.append("            font-size: 20px;\n");
        sb.append("            margin-bottom: 16px;\n");
        sb.append("            padding-bottom: 12px;\n");
        sb.append("            border-bottom: 2px solid var(--tinkoff-yellow);\n");
        sb.append("        }\n");
        sb.append("        table {\n");
        sb.append("            width: 100%;\n");
        sb.append("            border-collapse: collapse;\n");
        sb.append("        }\n");
        sb.append("        th, td {\n");
        sb.append("            padding: 12px;\n");
        sb.append("            text-align: left;\n");
        sb.append("            border-bottom: 1px solid #eee;\n");
        sb.append("        }\n");
        sb.append("        th {\n");
        sb.append("            background: var(--tinkoff-gray);\n");
        sb.append("            font-weight: 600;\n");
        sb.append("            font-size: 13px;\n");
        sb.append("            text-transform: uppercase;\n");
        sb.append("            color: #666;\n");
        sb.append("        }\n");
        sb.append("        tr:hover {\n");
        sb.append("            background: #fafafa;\n");
        sb.append("        }\n");
        sb.append("        .ticker {\n");
        sb.append("            font-weight: 600;\n");
        sb.append("        }\n");
        sb.append("        .positive {\n");
        sb.append("            color: var(--tinkoff-green);\n");
        sb.append("        }\n");
        sb.append("        .negative {\n");
        sb.append("            color: var(--tinkoff-red);\n");
        sb.append("        }\n");
        sb.append("        .eod-countdown {\n");
        sb.append("            font-size: 20px;\n");
        sb.append("            font-weight: 600;\n");
        sb.append("            color: var(--tinkoff-yellow);\n");
        sb.append("            font-variant-numeric: tabular-nums;\n");
        sb.append("        }\n");
        sb.append("        .eod-label {\n");
        sb.append("            font-size: 13px;\n");
        sb.append("            color: #999;\n");
        sb.append("            margin-right: 8px;\n");
        sb.append("        }\n");
        sb.append("        .loading {\n");
        sb.append("            text-align: center;\n");
        sb.append("            padding: 40px;\n");
        sb.append("            color: #666;\n");
        sb.append("        }\n");
        sb.append("        .action-btn {\n");
        sb.append("            margin-right: 10px;\n");
        sb.append("        }\n");
        sb.append("    </style>\n");
        sb.append("</head>\n");
        sb.append("<body>\n");
        sb.append("    <div class=\"header\">\n");
        sb.append("        <h1>GoldenDragon</h1>\n");
        sb.append("        <div style=\"display: flex; align-items: center; gap: 16px;\">\n");
        sb.append("            <div class=\"eod-countdown\">\n");
        sb.append("                <span class=\"eod-label\">Until close</span>\n");
        sb.append("                <span id=\"eod-countdown\">--:--:--</span>\n");
        sb.append("            </div>\n");
        sb.append("        </div>\n");
        sb.append("    </div>\n");
        sb.append("    <div class=\"container\">\n");
        sb.append("        <div class=\"stats-grid\">\n");
        sb.append("            <div class=\"stat-card\">\n");
        sb.append("                <h3>Balance</h3>\n");
        sb.append("                <div class=\"value\" id=\"balance\">-</div>\n");
        sb.append("            </div>\n");
        sb.append("            <div class=\"stat-card\">\n");
        sb.append("                <h3>PnL</h3>\n");
        sb.append("                <div class=\"value\" id=\"pnl\">-</div>\n");
        sb.append("            </div>\n");
        sb.append("            <div class=\"stat-card\">\n");
        sb.append("                <h3>Total Trades</h3>\n");
        sb.append("                <div class=\"value\" id=\"trades\">-</div>\n");
        sb.append("            </div>\n");
        sb.append("            <div class=\"stat-card\">\n");
        sb.append("                <h3>Win Rate</h3>\n");
        sb.append("                <div class=\"value\" id=\"winrate\">-</div>\n");
        sb.append("            </div>\n");
        sb.append("        </div>\n");
        sb.append("        <div class=\"section\">\n");
        sb.append("            <h2>Positions</h2>\n");
        sb.append("            <table>\n");
        sb.append("                <thead>\n");
        sb.append("                    <tr>\n");
        sb.append("                        <th>Ticker</th>\n");
        sb.append("                        <th>Quantity</th>\n");
        sb.append("                        <th>Avg Price</th>\n");
        sb.append("                        <th>Expected PnL</th>\n");
        sb.append("                        <th>Actions</th>\n");
        sb.append("                    </tr>\n");
        sb.append("                </thead>\n");
        sb.append("                <tbody id=\"positions-body\">\n");
        sb.append("                    <tr><td colspan=\"5\" class=\"loading\">Loading...</td></tr>\n");
        sb.append("                </tbody>\n");
        sb.append("            </table>\n");
        sb.append("        </div>\n");
        sb.append("        <div class=\"section\">\n");
        sb.append("            <h2>Trade History</h2>\n");
        sb.append("            <table>\n");
        sb.append("                <thead>\n");
        sb.append("                    <tr>\n");
        sb.append("                        <th>Time</th>\n");
        sb.append("                        <th>Ticker</th>\n");
        sb.append("                        <th>Description</th>\n");
        sb.append("                        <th>Type</th>\n");
        sb.append("                        <th>Quantity</th>\n");
        sb.append("                        <th>Price</th>\n");
        sb.append("                        <th>PnL</th>\n");
        sb.append("                    </tr>\n");
        sb.append("                </thead>\n");
        sb.append("                <tbody id=\"trades-body\">\n");
        sb.append("                    <tr><td colspan=\"7\" class=\"loading\">Loading...</td></tr>\n");
        sb.append("                </tbody>\n");
        sb.append("            </table>\n");
        sb.append("        </div>\n");
        sb.append("    </div>\n");
        sb.append("    <script>\n");
        sb.append("        let currency = 'RUB';\n");
        // End-of-day countdown target: today's EOD in system timezone as epoch millis
        LocalDateTime eodToday = LocalDateTime.of(LocalDate.now(), endOfDay);
        long eodTargetMillis = eodToday.atZone(ZoneId.systemDefault()).toInstant().toEpochMilli();
        sb.append("        const eodTarget = " + eodTargetMillis + ";\n");
        sb.append("        function updateEodCountdown() {\n");
        sb.append("            const diff = eodTarget - Date.now();\n");
        sb.append("            const totalSeconds = Math.max(0, Math.floor(diff / 1000));\n");
        sb.append("            const hours = String(Math.floor(totalSeconds / 3600)).padStart(2, '0');\n");
        sb.append("            const minutes = String(Math.floor((totalSeconds % 3600) / 60)).padStart(2, '0');\n");
        sb.append("            const seconds = String(totalSeconds % 60).padStart(2, '0');\n");
        sb.append("            document.getElementById('eod-countdown').textContent = hours + ':' + minutes + ':' + seconds;\n");
        sb.append("        }\n");
        sb.append("        setInterval(updateEodCountdown, 1000);\n");
        sb.append("        updateEodCountdown();\n");
        sb.append("        function formatTimeMSK(isoTime) {\n");
        sb.append("            if (!isoTime) return '-';\n");
        sb.append("            try {\n");
        sb.append("                const date = new Date(isoTime);\n");
        sb.append("                return date.toLocaleString('ru-RU', { timeZone: 'Europe/Moscow', hour12: false, year: 'numeric', month: '2-digit', day: '2-digit', hour: '2-digit', minute: '2-digit', second: '2-digit' });\n");
        sb.append("            } catch (e) {\n");
        sb.append("                return isoTime;\n");
        sb.append("            }\n");
        sb.append("        }\n");
        sb.append("        async function loadData() {\n");
        sb.append("            try {\n");
        sb.append("                const statsRes = await fetch('/api/stats');\n");
        sb.append("                const stats = await statsRes.json();\n");
        sb.append("                currency = stats.currency || 'RUB';\n");
        sb.append("                document.getElementById('balance').textContent = formatMoney(stats.balance);\n");
        sb.append("                const pnlEl = document.getElementById('pnl');\n");
        sb.append("                const pnlPercent = stats.pnlPercent || 0;\n");
        sb.append("                pnlEl.innerHTML = formatMoney(stats.totalPnl) + ' <span class=\"pnl-percent\">(' + (pnlPercent >= 0 ? '+' : '') + pnlPercent.toFixed(2) + '%)</span>';\n");
        sb.append("                pnlEl.className = 'value ' + (stats.totalPnl >= 0 ? 'positive' : 'negative');\n");
        sb.append("                document.getElementById('trades').textContent = stats.totalTrades;\n");
        sb.append("                document.getElementById('winrate').textContent = (stats.winRate * 100).toFixed(1) + '%';\n");
        sb.append("                const positionsRes = await fetch('/api/positions');\n");
        sb.append("                const positions = await positionsRes.json();\n");
        sb.append("                const positionsBody = document.getElementById('positions-body');\n");
        sb.append("                if (positions.length === 0) {\n");
        sb.append("                    positionsBody.innerHTML = '<tr><td colspan=\"5\" class=\"loading\">No open positions</td></tr>';\n");
        sb.append("                } else {\n");
        sb.append("                    positionsBody.innerHTML = positions.map(pos => \n");
        sb.append("                        '<tr>' +\n");
        sb.append("                            '<td class=\"ticker\">' + pos.ticker + '</td>' +\n");
        sb.append("                            '<td>' + pos.balance + '</td>' +\n");
        sb.append("                            '<td>' + formatMoney(pos.averagePrice) + '</td>' +\n");
        sb.append("                            '<td class=\"' + (pos.expectedYield >= 0 ? 'positive' : 'negative') + '\">' +\n");
        sb.append("                                formatMoney(pos.expectedYield) +\n");
        sb.append("                            '</td>' +\n");
        sb.append("                            '<td>' +\n");
        sb.append("                                '<button class=\"action-btn close-half\" onclick=\"closePosition(\\\'' + pos.ticker + '\\', 0.5)\" title=\"Close 50%\">🔽 50%</button>' +\n");
        sb.append("                                '<button class=\"action-btn close-all\" onclick=\"closePosition(\\\'' + pos.ticker + '\\', 1.0)\" title=\"Close All\">❌ Close</button>' +\n");
        sb.append("                            '</td>' +\n");
        sb.append("                        '</tr>'\n");
        sb.append("                    ).join('');\n");
        sb.append("                }\n");
        sb.append("                const tradesRes = await fetch('/api/trades');\n");
        sb.append("                const trades = await tradesRes.json();\n");
        sb.append("                const tradesBody = document.getElementById('trades-body');\n");
        sb.append("                if (trades.length === 0) {\n");
        sb.append("                    tradesBody.innerHTML = '<tr><td colspan=\"7\" class=\"loading\">No trade history</td></tr>';\n");
        sb.append("                } else {\n");
        sb.append("                    tradesBody.innerHTML = trades.map(trade => \n");
        sb.append("                        '<tr>' +\n");
        sb.append("                            '<td>' + formatTimeMSK(trade.time) + '</td>' +\n");
        sb.append("                            '<td class=\"ticker\">' + trade.ticker + '</td>' +\n");
        sb.append("                            '<td>' + (trade.description || '-') + '</td>' +\n");
        sb.append("                            '<td>' + trade.type + '</td>' +\n");
        sb.append("                            '<td>' + trade.quantity + '</td>' +\n");
        sb.append("                            '<td>' + formatMoney(trade.price) + '</td>' +\n");
        sb.append("                            '<td class=\"' + (trade.pnl >= 0 ? 'positive' : 'negative') + '\">' +\n");
        sb.append("                                formatMoney(trade.pnl) +\n");
        sb.append("                            '</td>' +\n");
        sb.append("                        '</tr>'\n");
        sb.append("                    ).join('');\n");
        sb.append("                }\n");
        sb.append("            } catch (error) {\n");
        sb.append("                console.error('Error loading data:', error);\n");
        sb.append("            }\n");
        sb.append("        }\n");
        sb.append("        function formatMoney(value) {\n");
        sb.append("            if (value === null || value === undefined) return '-';\n");
        sb.append("            if (currency === 'USDT') {\n");
        sb.append("                return new Intl.NumberFormat('en-US', {\n");
        sb.append("                    minimumFractionDigits: 2,\n");
        sb.append("                    maximumFractionDigits: 2\n");
        sb.append("                }).format(value) + ' USDT';\n");
        sb.append("            }\n");
        sb.append("            return new Intl.NumberFormat('ru-RU', {\n");
        sb.append("                style: 'currency',\n");
        sb.append("                currency: 'RUB',\n");
        sb.append("                minimumFractionDigits: 2\n");
        sb.append("            }).format(value);\n");
        sb.append("        }\n");
        sb.append("        async function closePosition(ticker, fraction) {\n");
        sb.append("            if (!confirm('Close ' + (fraction * 100) + '% of ' + ticker + '?')) return;\n");
        sb.append("            try {\n");
        sb.append("                const res = await fetch('/api/positions/close', {\n");
        sb.append("                    method: 'POST',\n");
        sb.append("                    headers: {'Content-Type': 'application/json'},\n");
        sb.append("                    body: JSON.stringify({ticker, fraction})\n");
        sb.append("                });\n");
        sb.append("                const result = await res.json();\n");
        sb.append("                if (result.success) {\n");
        sb.append("                    alert(result.message || ('Closed ' + ticker));\n");
        sb.append("                    loadData();\n");
        sb.append("                } else {\n");
        sb.append("                    alert('Error: ' + result.error);\n");
        sb.append("                }\n");
        sb.append("            } catch (error) {\n");
        sb.append("                alert('Error: ' + error.message);\n");
        sb.append("            }\n");
        sb.append("        }\n");
        sb.append("        loadData();\n");
        sb.append("        setInterval(loadData, 120000); // Refresh every 2 minutes\n");
        sb.append("    </script>\n");
        sb.append("</body>\n");
        sb.append("</html>\n");
        return sb.toString();
    }
}
