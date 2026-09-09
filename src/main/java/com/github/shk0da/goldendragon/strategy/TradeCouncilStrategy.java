package com.github.shk0da.goldendragon.strategy;

import com.github.shk0da.goldendragon.config.TradeCouncilConfig;
import com.github.shk0da.goldendragon.config.UnifiedTraderConfig;
import com.github.shk0da.goldendragon.model.Candle;
import com.github.shk0da.goldendragon.model.PendingOrder;
import com.github.shk0da.goldendragon.model.Position;
import com.github.shk0da.goldendragon.model.TickerInfo;
import com.github.shk0da.goldendragon.model.TradingDecision;
import com.github.shk0da.goldendragon.service.TradingService;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

import java.io.IOException;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Semaphore;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.stream.Collectors;

import static java.net.http.HttpRequest.BodyPublishers;
import static java.net.http.HttpRequest.newBuilder;
import static java.net.http.HttpResponse.BodyHandlers;

/**
 * TradeCouncil Strategy - AI-powered trading strategy using LLM debate system.
 *
 * <p>Strategy workflow:
 * <ol>
 *   <li>At startup: build key levels (support/resistance) for each ticker from unifiedTrader config</li>
 *   <li>Monitor prices in real-time via BaseStrategy execution loop</li>
 *   <li>When price approaches a key level (within proximity %), trigger LLM debate</li>
 *   <li>Debate involves 3 agents: Analyst, Trader, Risk Manager (3 rounds)</li>
 *   <li>Arbiter produces final decision: LONG/SHORT/NO_TRADE with entry, stop, take-profit, position size</li>
 *   <li>Execute trade via TradingService if decision is actionable</li>
 * </ol>
 *
 * <p>Configuration via application.properties:
 * <ul>
 *   <li>tradecouncil.openai.baseUrl - LLM API endpoint</li>
 *   <li>tradecouncil.debater.model - model for debate agents</li>
 *   <li>tradecouncil.arbiter.model - model for final decision</li>
 *   <li>tradecouncil.proximity.percent - price distance to trigger debate (e.g. 2.0 = 2%)</li>
 *   <li>tradecouncil.risk.percent - risk per trade as % of capital</li>
 *   <li>tradecouncil.debate.rounds - number of debate rounds</li>
 * </ul>
 */
public class TradeCouncilStrategy extends BaseStrategy {

    // Configuration
    private final TradeCouncilConfig tcConfig;

    // Key levels storage: ticker -> {levelName -> price}
    private final Map<String, Map<String, Double>> keyLevels = new ConcurrentHashMap<>();

    // Last price monitoring: ticker -> lastPrice
    private final Map<String, Double> lastPrices = new ConcurrentHashMap<>();

    // Cooldown tracking: ticker -> lastDebateTime
    private final Map<String, Long> debateCooldowns = new ConcurrentHashMap<>();
    // Cooldown type: ticker -> true if no-trade cooldown (10 min), false if order cooldown (5 min)
    private final Map<String, Boolean> noTradeCooldown = new ConcurrentHashMap<>();

    private final Map<String, PendingOrder> pendingOrders = new ConcurrentHashMap<>();

    // Currently analyzing ticker - only one analysis at a time
    private volatile String currentlyAnalyzingTicker = null;
    private final ReentrantReadWriteLock analysisLock = new ReentrantReadWriteLock();
    private final ReentrantReadWriteLock.ReadLock analysisReadLock = analysisLock.readLock();
    private final ReentrantReadWriteLock.WriteLock analysisWriteLock = analysisLock.writeLock();

    // Semaphore for sequential LLM access
    private final Semaphore llmSemaphore = new Semaphore(1);

    // Rate limiter: 262000 tokens per minute
    private static final long MAX_TOKENS_PER_MIN = 262000;
    private static final long TOKENS_REFILL_PER_SEC = MAX_TOKENS_PER_MIN / 60;
    private final AtomicLong availableTokens = new AtomicLong(MAX_TOKENS_PER_MIN);
    private final AtomicLong lastRefillTime = new AtomicLong(System.currentTimeMillis());

    private static final long DEBATE_COOLDOWN_MS = 5 * 60 * 1000L; // 5 min after pending order created
    private static final long NO_TRADE_COOLDOWN_MS = 10 * 60 * 1000L; // 10 min after debate without trade
    private static final int H1_CANDLES = 72;
    private static final int M15_CANDLES = 64;

    public TradeCouncilStrategy(UnifiedTraderConfig unifiedTraderConfig, TradingService tradingService) {
        this(unifiedTraderConfig, tradingService, new com.github.shk0da.goldendragon.model.Config());
    }

    public TradeCouncilStrategy(
        UnifiedTraderConfig unifiedTraderConfig,
        TradingService tradingService,
        com.github.shk0da.goldendragon.model.Config config) {
        super(unifiedTraderConfig, tradingService, config);

        try {
            this.tcConfig = new TradeCouncilConfig();
        } catch (IOException e) {
            throw new RuntimeException("Failed to load TradeCouncilConfig", e);
        }

        log("TradeCouncilStrategy initialized with unifiedTrader tickers");
        log("Config: " + tcConfig);
        log("Proximity threshold: " + tcConfig.getProximityPercent() + "%, Risk per trade: " + tcConfig.getRiskPerTradePercent() + "%");
    }

    @Override
    protected String getStrategyName() {
        return "TradeCouncilStrategy";
    }

    @Override
    protected void onDailyReset() {
        log("TradeCouncilStrategy: Daily reset - clearing price history");
        lastPrices.clear();
    }

    @Override
    public TradingDecision decide(
        String ticker,
        List<Candle> hourCandles,
        List<Candle> minuteCandles,
        Position position,
        double balance,
        boolean incrementCandlesHeld) {

        if (ticker == null || ticker.isEmpty()) {
            return new TradingDecision("HOLD", "EMPTY_TICKER");
        }

        if (position != null && position.quantity != 0) {
            return new TradingDecision("HOLD", "HAS_ACTIVE_POSITION");
        }

        // Check if another ticker is currently being analyzed (with write lock)
        analysisWriteLock.lock();
        try {
            if (currentlyAnalyzingTicker != null && !currentlyAnalyzingTicker.equals(ticker)) {
                return new TradingDecision("HOLD", "ANALYSIS_IN_PROGRESS");
            }
            // Reserve this ticker for analysis
            currentlyAnalyzingTicker = ticker;
        } finally {
            analysisWriteLock.unlock();
        }

        Double currentPrice = getCurrentPrice(ticker, hourCandles, minuteCandles);
        if (currentPrice == null) {
            analysisWriteLock.lock();
            try {
                currentlyAnalyzingTicker = null;
            } finally {
                analysisWriteLock.unlock();
            }
            return new TradingDecision("HOLD", "NO_PRICE_DATA");
        }

        lastPrices.put(ticker, currentPrice);

        PendingOrder pending = pendingOrders.get(ticker);
        if (pending != null) {
            if (pending.isExpired()) {
                long ageMinutes = (System.currentTimeMillis() - pending.createdAt) / 60000L;
                double currentPriceForLog = currentPrice != null ? currentPrice : 0.0;
                log("⏰ PENDING ORDER EXPIRED: " + ticker + " " + pending.direction +
                    " @ " + pending.entryPrice + " (entry target: " + pending.entryPrice + 
                    ", current: " + String.format("%.2f", currentPriceForLog) +
                    ", age: " + ageMinutes + " min, TTL: " + pending.ttlMinutes + " min)");
                log("   Reasoning: " + pending.reasoning);
                pendingOrders.remove(ticker);
                analysisWriteLock.lock();
                try {
                    currentlyAnalyzingTicker = null;
                } finally {
                    analysisWriteLock.unlock();
                }
                return new TradingDecision("HOLD", "ORDER_EXPIRED");
            }

                        boolean isLong = "LONG".equalsIgnoreCase(pending.direction)
                || "BUY".equalsIgnoreCase(pending.direction);
            if (pending.shouldEnter(currentPrice, isLong)) {
                log("✅ ENTRY CONDITION MET for " + ticker + ": " + pending.direction +
                    " @ " + currentPrice + " (target: " + pending.entryPrice + ")");
                pendingOrders.remove(ticker);
                analysisWriteLock.lock();
                try {
                    currentlyAnalyzingTicker = null;
                } finally {
                    analysisWriteLock.unlock();
                }

                return new TradingDecision(
                    "OPEN",
                    pending.reasoning,
                    0.0,
                    pending.quantity,
                    pending.stopLoss,
                    pending.takeProfit,
                    pending.entryPrice,
                    new Position(
                        pending.direction,
                        pending.entryPrice,
                        pending.stopLoss,
                        pending.takeProfit,
                        pending.quantity,
                        0
                    )
                );
            }

            analysisWriteLock.lock();
            try {
                currentlyAnalyzingTicker = null;
            } finally {
                analysisWriteLock.unlock();
            }
            return new TradingDecision("HOLD", "WAITING_FOR_ENTRY");
        }

        Map<String, Double> levels = keyLevels.get(ticker);
        if (levels == null || levels.isEmpty()) {
            analysisWriteLock.lock();
            try {
                currentlyAnalyzingTicker = null;
            } finally {
                analysisWriteLock.unlock();
            }
            return new TradingDecision("HOLD", "NO_LEVELS");
        }

        String nearLevel = findNearLevel(ticker, currentPrice, levels);
        if (nearLevel == null) {
            analysisWriteLock.lock();
            try {
                currentlyAnalyzingTicker = null;
            } finally {
                analysisWriteLock.unlock();
            }
            return new TradingDecision("HOLD", "PRICE_NOT_NEAR_LEVEL");
        }

        // Skip if already in cooldown (5 min after order placed, 10 min after no-trade)
        Long lastDebate = debateCooldowns.get(ticker);
        if (lastDebate != null) {
            long elapsed = System.currentTimeMillis() - lastDebate;
            Boolean isNoTrade = noTradeCooldown.get(ticker);
            long cooldownMs = (isNoTrade != null && isNoTrade) ? NO_TRADE_COOLDOWN_MS : DEBATE_COOLDOWN_MS;

            if (elapsed < cooldownMs) {
                analysisWriteLock.lock();
                try {
                    currentlyAnalyzingTicker = null;
                } finally {
                    analysisWriteLock.unlock();
                }
                return new TradingDecision("HOLD", "DEBATE_COOLDOWN");
            }
        }

        log("=== CONSENSIUM START === " + ticker + ": Price " + currentPrice + " approached level " + nearLevel + " (" + levels.get(nearLevel) + ")");
        log("Debate in progress for " + ticker + " (rounds=" + tcConfig.getDebateRounds() + ")");

        try {
            TradingDecision debateResult = triggerDebate(ticker, hourCandles, minuteCandles, currentPrice, levels);

            if (debateResult != null && !"HOLD".equals(debateResult.action) && !"NO_TRADE".equals(debateResult.action)) {
                debateCooldowns.put(ticker, System.currentTimeMillis());
                noTradeCooldown.put(ticker, false); // 5 min cooldown for order

                if (debateResult.entryPrice != null) {
                    PendingOrder newOrder = new PendingOrder(
                        ticker,
                        debateResult.action,
                        debateResult.entryPrice,
                        debateResult.stopLoss,
                        debateResult.takeProfit,
                        debateResult.quantity,
                        debateResult.reason,
                        debateResult.ttlMinutes
                    );
                    pendingOrders.put(ticker, newOrder);
                    log("⏳ PENDING ORDER CREATED: " + ticker + " " + newOrder.direction +
                        " @ " + newOrder.entryPrice + " (SL: " + newOrder.stopLoss +
                        ", TP: " + newOrder.takeProfit + ", Qty: " + newOrder.quantity + ")");
                    log("   Reasoning: " + newOrder.reasoning);
                    log("   Expires in " + newOrder.ttlMinutes + " minutes");
                }

                return new TradingDecision("HOLD", "ORDER_PLACED");
            }

            log("=== CONSENSIUM RESULT === " + ticker + ": " + (debateResult != null ?
                "Action=" + debateResult.action + ", Reason=" + debateResult.reason : "NULL"));
        } catch (Exception e) {
            log("Debate failed for " + ticker + ": " + e.getMessage());
            analysisWriteLock.lock();
            try {
                currentlyAnalyzingTicker = null;
            } finally {
                analysisWriteLock.unlock();
            }
            return new TradingDecision("HOLD", "DEBATE_ERROR");
        }

        // Clear currently analyzing ticker and set 10 min cooldown for no-trade result
        log("=== CONSENSIUM RESULT === " + ticker + ": No trade decision - entering 10 min cooldown");
        debateCooldowns.put(ticker, System.currentTimeMillis());
        noTradeCooldown.put(ticker, true); // 10 min cooldown for no-trade
        analysisWriteLock.lock();
        try {
            currentlyAnalyzingTicker = null;
        } finally {
            analysisWriteLock.unlock();
        }
        return new TradingDecision("HOLD", "DEBATE_NO_TRADE");
    }

    /**
     * Build key levels for all tickers from unifiedTrader config at startup.
     * Called once when strategy starts.
     */
    public void buildKeyLevels() {
        log("Building key levels for all tickers from unifiedTrader config...");

        List<String> tickers = new ArrayList<>();
        if (unifiedTraderConfig != null) {
            tickers.addAll(unifiedTraderConfig.getStocks());
        }

        if (tickers.isEmpty()) {
            log("No tickers found in unifiedTrader config");
            return;
        }

        for (String ticker : tickers) {
            try {
                buildLevelsForTicker(ticker);
                Thread.sleep(100); // API rate limiting
            } catch (Exception e) {
                log("Failed to build levels for " + ticker + ": " + e.getMessage());
            }
        }

        int totalLevels = keyLevels.values().stream().mapToInt(Map::size).sum();
        log("Key levels built: " + keyLevels.size() + " tickers, " + totalLevels + " total levels");
    }

    private void buildLevelsForTicker(String ticker) throws Exception {
        TickerInfo info = findTickerInfo(ticker);
        if (info == null || info.getFigi() == null) {
            log("Ticker info not found for " + ticker);
            return;
        }

        List<Candle> h1Candles = loadCandles(info.getFigi(), "HOUR", Math.max(200, H1_CANDLES));
        if (h1Candles == null || h1Candles.isEmpty()) {
            log("No H1 candles for " + ticker);
            return;
        }

        Map<String, Double> levels = findSignificantLevels(ticker, h1Candles);

        if (levels.isEmpty()) {
            log(ticker + ": No significant levels found, using fallback");
            levels = buildFallbackLevels(h1Candles);
        }

        keyLevels.put(ticker, levels);
    }

    private Map<String, Double> findSignificantLevels(String ticker, List<Candle> candles) {
        Map<Double, Integer> supportHits = new HashMap<>();
        Map<Double, Integer> resistanceHits = new HashMap<>();

        for (int i = 1; i < candles.size() - 1; i++) {
            Candle prev = candles.get(i - 1);
            Candle curr = candles.get(i);
            Candle next = candles.get(i + 1);

            if (curr.high > prev.high && curr.high > next.high) {
                addLevel(resistanceHits, curr.high, 0.02);
            }

            if (curr.low < prev.low && curr.low < next.low) {
                addLevel(supportHits, curr.low, 0.02);
            }
        }

        Map<String, Double> levels = new LinkedHashMap<>();

        resistanceHits.entrySet().stream()
            .filter(e -> e.getValue() >= 2)
            .sorted((a, b) -> b.getValue().compareTo(a.getValue()))
            .limit(3)
            .forEach(e -> {
                String key = "R" + (levels.size() + 1);
                levels.put(key, e.getKey());
            });

        supportHits.entrySet().stream()
            .filter(e -> e.getValue() >= 2)
            .sorted((a, b) -> b.getValue().compareTo(a.getValue()))
            .limit(3)
            .forEach(e -> {
                String key = "S" + (levels.size() - 3 + 1);
                levels.put(key, e.getKey());
            });

        if (!levels.isEmpty()) {
            double avgPrice = candles.stream()
                .mapToDouble(c -> (c.high + c.low) / 2.0)
                .average()
                .orElse(0.0);
            levels.put("PIVOT", avgPrice);
        }

        log(ticker + ": Found " + (supportHits.size() + resistanceHits.size()) +
            " significant levels (2+ hits)");

        return levels;
    }

    private void addLevel(Map<Double, Integer> levelMap, double price, double tolerance) {
        for (Double existing : levelMap.keySet()) {
            if (Math.abs(existing - price) / existing <= tolerance) {
                levelMap.put(existing, levelMap.get(existing) + 1);
                return;
            }
        }
        levelMap.put(price, 1);
    }

    private Map<String, Double> buildFallbackLevels(List<Candle> candles) {
        Map<String, Double> levels = new LinkedHashMap<>();

        int lookback = Math.min(20, candles.size());
        double highestHigh = 0.0;
        double lowestLow = Double.MAX_VALUE;

        for (int i = candles.size() - lookback; i < candles.size(); i++) {
            Candle c = candles.get(i);
            if (c.high > highestHigh) highestHigh = c.high;
            if (c.low < lowestLow) lowestLow = c.low;
        }

        levels.put("R1", highestHigh);
        levels.put("R2", highestHigh * 1.02);
        levels.put("R3", highestHigh * 1.05);
        levels.put("S1", lowestLow);
        levels.put("S2", lowestLow * 0.98);
        levels.put("S3", lowestLow * 0.95);
        levels.put("PIVOT", (highestHigh + lowestLow) / 2.0);

        return levels;
    }

    private String findNearLevel(String ticker, double currentPrice, Map<String, Double> levels) {
        double threshold = currentPrice * (tcConfig.getProximityPercent() / 100.0);

        for (Map.Entry<String, Double> entry : levels.entrySet()) {
            double levelPrice = entry.getValue();
            if (Math.abs(currentPrice - levelPrice) <= threshold) {
                return entry.getKey();
            }
        }

        return null;
    }

    private Double getCurrentPrice(String ticker, List<Candle> hourCandles, List<Candle> minuteCandles) {
        if (ticker == null) return null;

        if (tradingService != null) {
            try {
                TickerInfo info = findTickerInfo(ticker);
                if (info != null && info.getFigi() != null) {
                    TickerInfo.Key key = info.getKey();
                    double livePrice = tradingService.getAvailablePrice(key);
                    if (livePrice > 0) {
                        return livePrice;
                    }
                }
            } catch (Exception e) {
                // Ignore and fallback to candle close
            }
        }

        if (minuteCandles != null && !minuteCandles.isEmpty()) {
            return minuteCandles.get(minuteCandles.size() - 1).close;
        }
        if (hourCandles != null && !hourCandles.isEmpty()) {
            return hourCandles.get(hourCandles.size() - 1).close;
        }
        return null;
    }

    private TradingDecision triggerDebate(
        String ticker,
        List<Candle> hourCandles,
        List<Candle> minuteCandles,
        double currentPrice,
        Map<String, Double> levels) throws Exception {

        TickerInfo info = findTickerInfo(ticker);
        String tickerName = info != null ? info.getName() : ticker;

        // Prepare market data context
        String marketData = formatMarketData(ticker, tickerName, currentPrice, hourCandles, minuteCandles, levels);

        // Run debate rounds
        log("Starting debate for " + ticker + " with " + tcConfig.getDebateRounds() + " rounds...");
        Map<String, String> history = new LinkedHashMap<>();
        Map<String, String> agents = new LinkedHashMap<>();
        agents.put("Analyst", tcConfig.getAnalystPrompt());
        agents.put("Trader", tcConfig.getTraderPrompt());
        agents.put("Risk Manager", tcConfig.getRiskManagerPrompt());

        for (int round = 0; round < tcConfig.getDebateRounds(); round++) {
            log(ticker + " | Round " + (round + 1) + "/" + tcConfig.getDebateRounds());
            for (Map.Entry<String, String> agent : agents.entrySet()) {
                StringBuilder prompt = new StringBuilder();
                prompt.append("Market data:\n").append(marketData).append("\n");
                if (!history.isEmpty()) {
                    prompt.append("Previous opinions:\n");
                    history.forEach((n, a) -> prompt.append("[").append(n).append("]: ").append(a).append("\n"));
                    prompt.append("Take previous opinions into account.\n");
                }

                String answer = callLLM(tcConfig.getDebaterModel(), agent.getValue(), prompt.toString(), 0.7);
                history.put(agent.getKey(), answer);
                log(ticker + " | R" + (round + 1) + " " + agent.getKey() + ": done");
                Thread.sleep(200);
            }
            Thread.sleep(500);

            if (round >= 1) {
                StringBuilder allOpinions = new StringBuilder();
                allOpinions.append("Market data:\n").append(marketData).append("\n\n");
                allOpinions.append("Debate history:\n");
                history.forEach((n, a) -> allOpinions.append("[").append(n).append("]:\n").append(a).append("\n\n"));

                String roundNum = "Round " + (round + 1);
                String consensusCheck = callLLM(
                    tcConfig.getDebaterModel(),
                    tcConfig.getConsensusPrompt().replace("{N}", String.valueOf(agents.size())) + "\n\nEvaluate consensus from: " + roundNum,
                    allOpinions.toString(),
                    0.0
                );

                if ("NO_RESPONSE".equals(consensusCheck)) {
                    log("Consensus check (" + roundNum + "): no response");
                } else if (consensusCheck.contains("CONSENSUS")) {
                    log("Consensus check (" + roundNum + "): CONSENSUS reached!");

                    if (consensusCheck.contains("decision")) {
                        return parseDecision(consensusCheck, ticker, currentPrice);
                    }

                    String lastOpinion = new ArrayList<>(history.values()).get(history.size() - 1);
                    return parseDecision(lastOpinion, ticker, currentPrice);
                } else {
                    log("Consensus check (" + roundNum + "): CONTINUE");
                }
            }
        }

        // Get final arbiter decision
        StringBuilder summary = new StringBuilder();
        history.forEach((n, a) -> summary.append("[").append(n).append("]:\n").append(a).append("\n"));

        String result = callLLM(tcConfig.getArbiterModel(), tcConfig.getArbiterPrompt(),
            "Market data:\n" + marketData + "\n\nExpert opinions:\n" + summary.toString() + "\n\nMake the final decision.",
            0.2);

        log(ticker + " | Arbiter decision: " + result);

        // Parse decision
        return parseDecision(result, ticker, currentPrice);
    }

    private String formatMarketData(
        String ticker, String name, double price,
        List<Candle> hourCandles, List<Candle> minuteCandles,
        Map<String, Double> levels) {

        StringBuilder sb = new StringBuilder();
        sb.append("Instrument: ").append(ticker).append(" (").append(name).append(")\n");
        sb.append("Current price: ").append(price).append("\n");
        sb.append("Levels: ");
        levels.forEach((k, v) -> sb.append(k).append("=").append(String.format("%.2f", v)).append(" "));
        sb.append("\n");

        if (hourCandles != null && !hourCandles.isEmpty()) {
            double rsi = calcRSI(hourCandles, 14);
            sb.append("RSI(14) H1: ").append(String.format("%.2f", rsi)).append("\n");

            // Recent candles
            sb.append("Last 5 H1 candles:\n");
            int start = Math.max(0, hourCandles.size() - 5);
            for (int i = start; i < hourCandles.size(); i++) {
                Candle c = hourCandles.get(i);
                sb.append(String.format("  O=%.2f H=%.2f L=%.2f C=%.2f V=%d\n",
                    c.open, c.high, c.low, c.close, c.volume));
            }
        }

        if (minuteCandles != null && !minuteCandles.isEmpty()) {
            sb.append("Last 3 M5 candles:\n");
            int start = Math.max(0, minuteCandles.size() - 3);
            for (int i = start; i < minuteCandles.size(); i++) {
                Candle c = minuteCandles.get(i);
                sb.append(String.format("  O=%.2f H=%.2f L=%.2f C=%.2f\n",
                    c.open, c.high, c.low, c.close));
            }
        }

        return sb.toString();
    }

    private TradingDecision parseDecision(String json, String ticker, double currentPrice) {
        try {
            // Simple JSON parsing
            String decision = extractJsonValue(json, "decision");
            String entryStr = extractJsonValue(json, "entry");
            String stopStr = extractJsonValue(json, "stop");
            String tpStr = extractJsonValue(json, "position_size");
            String confidenceStr = extractJsonValue(json, "confidence");
            String reasoning = extractJsonValue(json, "reasoning");
            String ttlMinutesStr = extractJsonValue(json, "ttlMinutes");

            double entry = entryStr != null ? Double.parseDouble(entryStr) : currentPrice;
            double stop = stopStr != null ? Double.parseDouble(stopStr) : 0.0;
            double confidence = confidenceStr != null ? Double.parseDouble(confidenceStr) : 50.0;
            int ttlMinutes = ttlMinutesStr != null ? Integer.parseInt(ttlMinutesStr) : 30; // Default 30 min

            String action = "HOLD";
            double positionMultiplier = 0.3;
            if ("LONG".equals(decision) || "BUY".equals(decision)) {
                action = "BUY";
            } else if ("SHORT".equals(decision) || "SELL".equals(decision)) {
                action = "SELL";
            }

            // Map position size to numeric multiplier (English values from LLM)
            if (tpStr != null) {
                switch (tpStr) {
                    case "FullCapital":
                        positionMultiplier = 1.0;
                        break;
                    case "HalfCapital":
                        positionMultiplier = 0.5;
                        break;
                    case "SmallPosition":
                        positionMultiplier = 0.3;
                        break;
                    default:
                        positionMultiplier = 0.3;
                }
            }

            // Calculate position size based on risk and multiplier
            double riskAmount = (tcConfig.getRiskPerTradePercent() / 100.0) * 1000000 * positionMultiplier;
            int quantity = 0;
            if (stop > 0 && action != null) {
                double riskPerShare = Math.abs(entry - stop);
                if (riskPerShare > 0) {
                    quantity = (int) (riskAmount / riskPerShare);
                }
            }

            log("Parsed decision: " + action + " " + ticker + " qty=" + quantity +
                " entry=" + entry + " stop=" + stop + " confidence=" + confidence + " ttl=" + ttlMinutes + "min");

            return new TradingDecision(
                action,
                reasoning != null ? reasoning : "LLM_DECISION",
                confidence,
                quantity,
                stop,
                entry * 1.03, // Default 3% take-profit
                entry,
                null,
                ttlMinutes
            );

        } catch (Exception e) {
            log("Failed to parse decision: " + e.getMessage());
            return new TradingDecision("HOLD", "PARSE_ERROR: " + e.getMessage());
        }
    }

    private String extractJsonValue(String json, String key) {
        if (json == null || key == null) return null;
        int keyIndex = json.indexOf("\"" + key + "\"");
        if (keyIndex == -1) return null;

        int colonIndex = json.indexOf(":", keyIndex);
        if (colonIndex == -1) return null;

        int startIndex = colonIndex + 1;
        while (startIndex < json.length() && Character.isWhitespace(json.charAt(startIndex))) {
            startIndex++;
        }

        if (startIndex >= json.length()) return null;

        char firstChar = json.charAt(startIndex);
        if (firstChar == '"') {
            // String value
            int endIndex = json.indexOf('"', startIndex + 1);
            return endIndex == -1 ? null : json.substring(startIndex + 1, endIndex);
        } else {
            // Numeric or other value
            int endIndex = startIndex;
            while (endIndex < json.length() && json.charAt(endIndex) != ',' && json.charAt(endIndex) != '}') {
                endIndex++;
            }
            return json.substring(startIndex, endIndex).trim();
        }
    }

    private long estimateTokens(String text) {
        // Approximate: 1 token ≈ 4 characters
        return text != null ? text.length() / 4 : 0;
    }

    private void refillTokens() {
        long now = System.currentTimeMillis();
        long elapsedSec = (now - lastRefillTime.get()) / 1000;
        if (elapsedSec > 0) {
            long refilled = elapsedSec * TOKENS_REFILL_PER_SEC;
            availableTokens.getAndUpdate(current -> Math.min(MAX_TOKENS_PER_MIN, current + refilled));
            lastRefillTime.set(now);
        }
    }

    private void waitForTokens(long needed) throws InterruptedException {
        refillTokens();
        while (availableTokens.get() < needed) {
            Thread.sleep(100);
            refillTokens();
        }
    }

    private String callLLM(String model, String systemPrompt, String userPrompt, double temperature) throws Exception {
        // Estimate tokens for this request
        long requestTokens = estimateTokens(systemPrompt) + estimateTokens(userPrompt);

        // Wait until we have enough tokens
        waitForTokens(requestTokens);

        llmSemaphore.acquire();
        try {
            // Deduct tokens
            availableTokens.getAndAdd(-requestTokens);

            String url = tcConfig.getOpenAiBaseUrl() + "/chat/completions";

            Gson gson = new GsonBuilder().create();
            JsonObject root = new JsonObject();
            root.addProperty("model", model);
            root.addProperty("temperature", temperature);

            JsonObject systemMsg = new JsonObject();
            systemMsg.addProperty("role", "system");
            systemMsg.addProperty("content", systemPrompt);

            JsonObject userMsg = new JsonObject();
            userMsg.addProperty("role", "user");
            userMsg.addProperty("content", userPrompt);

            JsonArray messages = new JsonArray();
            messages.add(systemMsg);
            messages.add(userMsg);

            root.add("messages", messages);

            String jsonBody = gson.toJson(root);

            HttpClient client = HttpClient.newBuilder()
                .connectTimeout(java.time.Duration.ofSeconds(30)).build();
            HttpRequest req = newBuilder()
                .uri(java.net.URI.create(url))
                .timeout(java.time.Duration.ofSeconds(300))
                .header("Content-Type", "application/json")
                .header("Authorization", "Bearer " + tcConfig.getOpenAiApiKey())
                .POST(BodyPublishers.ofString(jsonBody)).build();

            try {
                String response = client.send(req, BodyHandlers.ofString()).body();

                try {
                    JsonObject respObj = JsonParser.parseString(response).getAsJsonObject();
                    JsonArray choices = respObj.getAsJsonArray("choices");
                    if (choices != null && !choices.isEmpty()) {
                        JsonObject choice = choices.get(0).getAsJsonObject();
                        JsonObject message = choice.getAsJsonObject("message");
                        String content = message.get("content").getAsString();
                        availableTokens.getAndAdd(-estimateTokens(content));
                        return content;
                    } else {
                        log("LLM raw response (no choices): " + response);
                    }
                } catch (Exception e) {
                    log("LLM parse error: " + e.getMessage());
                    log("LLM raw response: " + response);
                }
            } catch (Exception e) {
                log("LLM call failed: " + e.getMessage());
            }

            return "NO_RESPONSE";
        } finally {
            llmSemaphore.release();
        }
    }

    private List<Candle> loadCandles(String figi, String interval, int limit) {
        if (tradingService != null) {
            return tradingService.getCandles(figi, interval, limit);
        }
        return new ArrayList<>();
    }

    private static double calcRSI(List<Candle> candles, int period) {
        if (candles.size() < period + 1) return Double.NaN;
        List<Double> closes = candles.stream().map(c -> c.close).collect(Collectors.toList());
        List<Double> changes = new ArrayList<>();
        for (int i = 1; i < closes.size(); i++) {
            changes.add(closes.get(i) - closes.get(i - 1));
        }
        double avgG = changes.stream()
            .skip(Math.max(0, changes.size() - period))
            .map(ch -> Math.max(0, ch))
            .collect(Collectors.averagingDouble(Double::doubleValue));
        double avgL = changes.stream()
            .skip(Math.max(0, changes.size() - period))
            .map(ch -> Math.max(0, -ch))
            .collect(Collectors.averagingDouble(Double::doubleValue));
        if (avgL == 0) return 100.0;
        double rs = avgG / avgL;
        return Math.round((100 - (100 / (1 + rs))) * 100.0) / 100.0;
    }
}
