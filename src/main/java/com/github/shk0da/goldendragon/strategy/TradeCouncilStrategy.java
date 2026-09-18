package com.github.shk0da.goldendragon.strategy;

import com.github.shk0da.goldendragon.config.MainConfig;
import com.github.shk0da.goldendragon.config.TradeCouncilConfig;
import com.github.shk0da.goldendragon.config.UnifiedTraderConfig;
import com.github.shk0da.goldendragon.model.Candle;
import com.github.shk0da.goldendragon.model.Config;
import com.github.shk0da.goldendragon.model.PendingOrder;
import com.github.shk0da.goldendragon.model.Position;
import com.github.shk0da.goldendragon.model.TickerInfo;
import com.github.shk0da.goldendragon.model.TickerType;
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
import java.net.http.HttpResponse;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;
import java.util.stream.Collectors;

import static com.github.shk0da.goldendragon.money.CashParkingManager.TINKOFF_PARKING_TICKER;

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

    // Currently analyzing tickers - multiple tickers can be analyzed in parallel
    private final Set<String> analyzingTickers = ConcurrentHashMap.newKeySet();

    // Rate limiter: 262000 tokens per minute (shared across all agents)
    private static final long MAX_TOKENS_PER_MIN = 262000;
    private static final long TOKENS_REFILL_PER_SEC = MAX_TOKENS_PER_MIN / 60;
    private final AtomicLong availableTokens = new AtomicLong(MAX_TOKENS_PER_MIN);
    private final AtomicLong lastRefillTime = new AtomicLong(System.currentTimeMillis());

    // Executor for parallel agent execution (3 agents per ticker)
    private final Executor debaterExecutor = Executors.newFixedThreadPool(6);

    private static final long DEBATE_COOLDOWN_MS = 5 * 60 * 1000L; // 5 min after pending order created
    private static final long NO_TRADE_COOLDOWN_MS = 10 * 60 * 1000L; // 10 min after debate without trade
    private static final int H1_CANDLES = 72;

    public TradeCouncilStrategy(UnifiedTraderConfig unifiedTraderConfig, TradingService tradingService) {
        this(unifiedTraderConfig, tradingService, new Config(unifiedTraderConfig), null);
    }

    public TradeCouncilStrategy(
        UnifiedTraderConfig unifiedTraderConfig,
        TradingService tradingService,
        Config config) {
        this(unifiedTraderConfig, tradingService, config, null);
    }

    public TradeCouncilStrategy(
        UnifiedTraderConfig unifiedTraderConfig,
        TradingService tradingService,
        Config config,
        MainConfig mainConfig) {
        super(unifiedTraderConfig, tradingService, config, null, mainConfig);

        try {
            this.tcConfig = new TradeCouncilConfig();
        } catch (IOException e) {
            throw new RuntimeException("Failed to load TradeCouncilConfig", e);
        }

        log("TradeCouncilStrategy initialized with unifiedTrader tickers");
        log("Config: " + tcConfig);
        log("Proximity threshold: " + tcConfig.getProximityPercent() + "%");
    }

    @Override
    protected String getStrategyName() {
        return "TradeCouncilStrategy";
    }

    @Override
    public void onDailyReset() {
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
        boolean incrementCandlesHeld
    ) {
        if (ticker == null || ticker.isEmpty()) {
            return new TradingDecision("HOLD", "EMPTY_TICKER");
        }

        if (TINKOFF_PARKING_TICKER.equals(ticker)) {
            return new TradingDecision("HOLD", "PARKING_TICKER");
        }

        if (position != null && position.quantity != 0) {
            return new TradingDecision("HOLD", "HAS_ACTIVE_POSITION");
        }

        PendingOrder pending = pendingOrders.get(ticker);
        if (null == pending) {
            // Check if this ticker is already being analyzed (per-ticker tracking)
            if (!analyzingTickers.add(ticker)) {
                return new TradingDecision("HOLD", "ANALYSIS_IN_PROGRESS");
            }
        }

        Double currentPrice = getCurrentPrice(ticker, hourCandles, minuteCandles);
        if (currentPrice == null) {
            return new TradingDecision("HOLD", "NO_PRICE_DATA");
        }
        lastPrices.put(ticker, currentPrice);

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
                return new TradingDecision("HOLD", "ORDER_EXPIRED");
            }

            if (pending.shouldEnter(currentPrice)) {
                logThrottled(ticker + "_entry_met",
                    "✅ ENTRY CONDITION MET for " + ticker + ": " + pending.direction +
                        " @ " + currentPrice + " (target: " + pending.entryPrice + ")", 5);

                TickerInfo info = findTickerInfo(ticker);
                String figi = info != null ? info.getFigi() : null;
                int quantity = calculateQuantityFromDepositPercent(ticker, pending.depositPercent, pending.entryPrice, balance, figi);
                logThrottled(ticker + "_qty_calc",
                    "   Calculated quantity: " + quantity + " (deposit%: " + pending.depositPercent + ", balance: " + balance + ")", 5);

                if (quantity <= 0) {
                    // keep pending order for retry when funds become available
                    return new TradingDecision("HOLD", "INSUFFICIENT_FUNDS");
                }

                pendingOrders.remove(ticker);

                return new TradingDecision(
                    "OPEN",
                    pending.reasoning,
                    0.0,
                    quantity,
                    pending.depositPercent,
                    pending.stopLoss,
                    pending.takeProfit,
                    pending.entryPrice,
                    new Position(
                        pending.direction,
                        pending.entryPrice,
                        pending.stopLoss,
                        pending.takeProfit,
                        quantity,
                        0
                    ),
                    pending.ttlMinutes
                );
            }
            return new TradingDecision("HOLD", "WAITING_FOR_ENTRY");
        }

        Map<String, Double> levels = keyLevels.get(ticker);
        if (levels == null || levels.isEmpty()) {
            return new TradingDecision("HOLD", "NO_LEVELS");
        }

        String nearLevel = findNearLevel(ticker, currentPrice, levels);
        if (nearLevel == null) {
            return new TradingDecision("HOLD", "PRICE_NOT_NEAR_LEVEL");
        }

        // Skip if already in cooldown (5 min after order placed, 10 min after no-trade)
        Long lastDebate = debateCooldowns.get(ticker);
        if (lastDebate != null) {
            long elapsed = System.currentTimeMillis() - lastDebate;
            Boolean isNoTrade = noTradeCooldown.get(ticker);
            long cooldownMs = (isNoTrade != null && isNoTrade) ? NO_TRADE_COOLDOWN_MS : DEBATE_COOLDOWN_MS;

            if (elapsed < cooldownMs) {
                analyzingTickers.remove(ticker);
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
                        debateResult.depositPercent,
                        debateResult.reason,
                        debateResult.ttlMinutes,
                        currentPrice
                    );
                    pendingOrders.put(ticker, newOrder);
                    log("⏳ PENDING ORDER CREATED: " + ticker + " " + newOrder.direction +
                        " @ " + newOrder.entryPrice + " (SL: " + newOrder.stopLoss +
                        ", TP: " + newOrder.takeProfit + ", Deposit%: " + newOrder.depositPercent +
                        ", mode: " + newOrder.describeEntryMode() + ")");
                    log("   Reasoning: " + newOrder.reasoning);
                    log("   Expires in " + newOrder.ttlMinutes + " minutes");
                }

                return new TradingDecision("HOLD", "ORDER_PLACED");
            }

            log("=== CONSENSIUM RESULT === " + ticker + ": " + (debateResult != null ?
                "Action=" + debateResult.action + ", Reason=" + debateResult.reason : "NULL"));
        } catch (Exception e) {
            log("Debate failed for " + ticker + ": " + e.getMessage());
            analyzingTickers.remove(ticker);
            return new TradingDecision("HOLD", "DEBATE_ERROR");
        }

        // Clear analyzing ticker and set 10 min cooldown for no-trade result
        log("=== CONSENSIUM RESULT === " + ticker + ": No trade decision - entering 10 min cooldown");
        debateCooldowns.put(ticker, System.currentTimeMillis());
        noTradeCooldown.put(ticker, true); // 10 min cooldown for no-trade
        analyzingTickers.remove(ticker);
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
            log("No H1 candles for " + ticker + " (sandbox mode or API unavailable - skipping key levels)");
            // Use simple fallback levels (will be refined when real candles are available)
            Map<String, Double> fallback = new HashMap<>();
            fallback.put("S1", 0.0);
            fallback.put("S2", 0.0);
            fallback.put("R1", 0.0);
            fallback.put("R2", 0.0);
            keyLevels.put(ticker, fallback);
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
            final int currentRound = round; // final variable for lambda
            log(ticker + " | Round " + (round + 1) + "/" + tcConfig.getDebateRounds());

            // Launch all agents in parallel within this round
            List<CompletableFuture<Void>> agentFutures = new ArrayList<>();
            Map<String, String> roundResults = new HashMap<>();

            for (Map.Entry<String, String> agent : agents.entrySet()) {
                String agentName = agent.getKey();
                String agentPrompt = agent.getValue();

                CompletableFuture<Void> future = CompletableFuture.runAsync(() -> {
                    try {
                        StringBuilder prompt = new StringBuilder();
                        prompt.append("Market data:\n").append(marketData).append("\n");
                        if (!history.isEmpty()) {
                            prompt.append("Previous opinions:\n");
                            history.forEach((n, a) -> prompt.append("[").append(n).append("]: ").append(a).append("\n"));
                            prompt.append("Take previous opinions into account.\n");
                        }

                        String answer = callLLM(tcConfig.getDebaterModel(), agentPrompt, prompt.toString(), 0.7);
                        roundResults.put(agentName, answer);
                        log(ticker + " | R" + (currentRound + 1) + " " + agentName + ": done");
                    } catch (Exception e) {
                        log(ticker + " | R" + (currentRound + 1) + " " + agentName + " failed: " + e.getMessage());
                        throw new RuntimeException(e);
                    }
                }, debaterExecutor);

                agentFutures.add(future);
            }

            // Wait for all agents in this round to complete (60s timeout per round)
            try {
                CompletableFuture.allOf(agentFutures.toArray(new CompletableFuture[0]))
                    .orTimeout(60, TimeUnit.SECONDS)
                    .join();
            } catch (Exception e) {
                log(ticker + " | Round " + (round + 1) + " timeout/error: " + e.getMessage());
                analyzingTickers.remove(ticker);
                return new TradingDecision("HOLD", "DEBATE_TIMEOUT");
            }

            // Copy results to history in original agent order
            for (String agentName : agents.keySet()) {
                history.put(agentName, roundResults.get(agentName));
            }

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
            "Market data:\n" + marketData + "\n\nExpert opinions:\n" + summary + "\n\nMake the final decision.",
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
            String decision = extractJsonValue(json, "decision");
            String entryStr = extractJsonValue(json, "entry");
            String stopStr = extractJsonValue(json, "stop");
            String positionSizeStr = extractJsonValue(json, "position_size");
            String confidenceStr = extractJsonValue(json, "confidence");
            String reasoning = extractJsonValue(json, "reasoning");
            String ttlMinutesStr = extractJsonValue(json, "ttlMinutes");

            Double takeProfit = extractJsonArrayFirstValue(json, "take_profits");

            double entry = entryStr != null ? Double.parseDouble(entryStr) : currentPrice;
            double stop = stopStr != null ? Double.parseDouble(stopStr) : 0.0;
            double confidence = confidenceStr != null ? Double.parseDouble(confidenceStr) : 50.0;
            int ttlMinutes = ttlMinutesStr != null ? Integer.parseInt(ttlMinutesStr) : 30;

            String action = "HOLD";
            if ("LONG".equals(decision) || "BUY".equals(decision)) {
                action = "BUY";
            } else if ("SHORT".equals(decision) || "SELL".equals(decision)) {
                action = "SELL";
            }

            double depositPercent = positionSizeToDepositPercent(positionSizeStr);

            double finalTakeProfit = takeProfit != null ? takeProfit : entry * 1.03;

            log("Parsed decision: " + action + " " + ticker + " depositPercent=" + depositPercent +
                " entry=" + entry + " stop=" + stop + " tp=" + finalTakeProfit +
                " confidence=" + confidence + " ttl=" + ttlMinutes + "min");

            return new TradingDecision(
                action,
                reasoning != null ? reasoning : "LLM_DECISION",
                confidence,
                0,
                depositPercent,
                stop,
                finalTakeProfit,
                entry,
                null,
                ttlMinutes
            );

        } catch (Exception e) {
            log("Failed to parse decision: " + e.getMessage());
            return new TradingDecision("HOLD", "PARSE_ERROR: " + e.getMessage());
        }
    }

    /**
     * Map LLM position size label to the fraction of deposit to deploy.
     * FullCapital deploys the whole available balance, HalfCapital half of it,
     * SmallPosition a third; unknown or missing labels default to SmallPosition.
     *
     * @param positionSize LLM position size label
     * @return percent of deposit to deploy (100.0 / 50.0 / 30.0)
     */
    double positionSizeToDepositPercent(String positionSize) {
        double positionMultiplier = 0.3;
        if (positionSize != null) {
            switch (positionSize) {
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
        return positionMultiplier * 100.0;
    }

    private Double extractJsonArrayFirstValue(String json, String key) {
        if (json == null || key == null) return null;
        int keyIndex = json.indexOf("\"" + key + "\"");
        if (keyIndex == -1) return null;

        int bracketIndex = json.indexOf("[", keyIndex);
        if (bracketIndex == -1 || bracketIndex - keyIndex > 50) return null;

        int closeBracket = json.indexOf("]", bracketIndex);
        if (closeBracket == -1) return null;

        String arrayContent = json.substring(bracketIndex + 1, closeBracket).trim();
        if (arrayContent.isEmpty()) return null;

        String firstValue = arrayContent.split(",")[0].trim();
        try {
            return Double.parseDouble(firstValue);
        } catch (NumberFormatException e) {
            return null;
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

        // Wait until we have enough tokens (rate limiter)
        waitForTokens(requestTokens);

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
            HttpRequest req = HttpRequest.newBuilder()
                .uri(java.net.URI.create(url))
                .timeout(java.time.Duration.ofSeconds(300))
                .header("Content-Type", "application/json")
                .header("Authorization", "Bearer " + tcConfig.getOpenAiApiKey())
                .POST(HttpRequest.BodyPublishers.ofString(jsonBody)).build();

            int maxRetries = 3;
            int retryDelayMs = 2000;
            HttpResponse<String> httpResponse = null;
            Exception lastException = null;

            for (int attempt = 1; attempt <= maxRetries; attempt++) {
                try {
                    httpResponse = client.send(req, HttpResponse.BodyHandlers.ofString());
                    if (httpResponse.statusCode() == 429) {
                        long jitter = java.util.concurrent.ThreadLocalRandom.current().nextLong(retryDelayMs / 2, retryDelayMs);
                        log("LLM API rate limited (429), retry " + attempt + "/" + maxRetries + " after " + (retryDelayMs + jitter) + "ms...");
                        Thread.sleep(retryDelayMs + jitter);
                        retryDelayMs = Math.min(retryDelayMs * 2, 10000);
                        continue;
                    }
                    break;
                } catch (Exception e) {
                    lastException = e;
                    if (attempt < maxRetries) {
                        long jitter = java.util.concurrent.ThreadLocalRandom.current().nextLong(retryDelayMs / 2, retryDelayMs);
                        log("LLM API call failed (attempt " + attempt + "/" + maxRetries + "): " + e.getMessage() + ", retrying in " + (retryDelayMs + jitter) + "ms...");
                        try {
                            Thread.sleep(retryDelayMs + jitter);
                            retryDelayMs = Math.min(retryDelayMs * 2, 10000);
                        } catch (InterruptedException ie) {
                            Thread.currentThread().interrupt();
                            throw new IOException("LLM API call interrupted", ie);
                        }
                    }
                }
            }

            if (httpResponse == null) {
                log("LLM API call failed after " + maxRetries + " attempts: " + (lastException != null ? lastException.getMessage() : "unknown error"));
                throw lastException != null ? lastException : new IOException("LLM API call failed after " + maxRetries + " attempts");
            }

            {
                String response = httpResponse.body();

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
            }

            return "NO_RESPONSE";
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

    /**
     * Calculate quantity from deposit percent, entry price and available balance.
     * Uses available balance to determine how many units can be bought.
     *
     * @param ticker ticker symbol
     * @param depositPercent percent of deposit to use (e.g., 1.0 for 1%)
     * @param entryPrice entry price
     * @param balance available balance
     * @return quantity in units (0 if calculation fails)
     */
    int calculateQuantityFromDepositPercent(String ticker, Double depositPercent, double entryPrice, double balance) {
        return calculateQuantityFromDepositPercent(ticker, depositPercent, entryPrice, balance, null);
    }

    /**
     * Calculate position quantity from deposit percent.
     * For futures: uses margin (ГО) from broker API instead of nominal price.
     *
     * @param ticker ticker symbol
     * @param depositPercent percent of deposit to use (e.g., 1.0 for 1%)
     * @param entryPrice entry price
     * @param balance available balance
     * @param figi instrument FIGI (used to fetch margin for futures)
     * @return quantity in units (0 if calculation fails)
     */
    int calculateQuantityFromDepositPercent(
            String ticker, Double depositPercent, double entryPrice, double balance, String figi) {
        if (depositPercent == null || depositPercent <= 0 || entryPrice <= 0 || balance <= 0) {
            log("⚠️ Invalid parameters for quantity calculation: depositPercent=" + depositPercent +
                ", entryPrice=" + entryPrice + ", balance=" + balance);
            return 0;
        }

        TickerInfo info = findTickerInfo(ticker);
        if (info == null) {
            log("⚠️ TickerInfo not found for " + ticker);
            return 0;
        }

        // Use margin-based calculation for futures
        if (info.getType() == TickerType.FEATURE && figi != null && tradingService != null) {
            return calculateFuturesQuantity(ticker, depositPercent, entryPrice, balance, figi);
        }

        double amountToUse = balance * (depositPercent / 100.0);
        double rawQuantity = amountToUse / entryPrice;

        Integer lot = info.getLot();
        int minLot = (lot != null && lot > 0) ? lot : 1;

        if (rawQuantity < minLot) {
            logThrottled(ticker + "_qty_min_lot",
                "⚠️ Calculated quantity " + rawQuantity + " below min lot " + minLot + " for " + ticker, 5);
            return 0;
        }

        int quantity = (int) (rawQuantity / minLot);
        log("💰 Quantity calculated: " + quantity + " lots (deposit%=" + depositPercent +
            ", balance=" + balance + ", amount=" + amountToUse + ", entry=" + entryPrice + ")");
        return quantity;
    }

    /**
     * Calculate futures quantity using margin (ГО) from broker API.
     * Uses max(marginOnBuy, marginOnSell) with 10% buffer for conservative risk management.
     */
    int calculateFuturesQuantity(
            String ticker, Double depositPercent, double entryPrice, double balance, String figi) {
        if (depositPercent == null || depositPercent <= 0 || balance <= 0) {
            log("⚠ Invalid parameters for futures quantity: depositPercent=" + depositPercent +
                ", balance=" + balance);
            return 0;
        }

        // 1. Get exact margin from broker API
        Double singleContractGo = null;
        if (tradingService != null) {
            singleContractGo = tradingService.getSingleContractGo(figi);
        }

        // Fallback: approx 25% of entry price if API unavailable
        if (singleContractGo == null || singleContractGo <= 0) {
            log("⚠ Could not fetch margin from broker for " + ticker + ". Using fallback (25% of price).");
            singleContractGo = entryPrice * 0.25;
        }

        // 2. Calculate allocated capital
        double allocatedCapital = balance * (depositPercent / 100.0);

        // 3. Apply 10% buffer for volatility/extension risk
        double effectiveGoWithBuffer = singleContractGo * 1.10;

        // 4. Final quantity calculation
        double rawQuantity = allocatedCapital / effectiveGoWithBuffer;
        int quantity = (int) Math.floor(rawQuantity);

        log(String.format(
            "💰 [FUTURES MARGIN] %s | Capital: %.2f | Margin: %.2f | w/ buffer: %.2f | Lots: %d",
            ticker, allocatedCapital, singleContractGo, effectiveGoWithBuffer, quantity));

        if (quantity < 1) {
            log("⚠ Futures quantity (" + rawQuantity + ") < 1 lot for " + ticker + ". Trade rejected.");
            return 0;
        }

        return quantity;
    }
}
