package com.github.shk0da.goldendragon.strategy.orderbook;

import static com.github.shk0da.goldendragon.service.TelegramNotifyService.telegramNotifyService;
import static com.github.shk0da.goldendragon.utils.TimeUtils.sleep;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toSet;

import com.github.shk0da.goldendragon.config.MainConfig;
import com.github.shk0da.goldendragon.config.OrderBookScalpConfig;
import com.github.shk0da.goldendragon.model.MarketDepthSnapshot;
import com.github.shk0da.goldendragon.model.MarketTickListener;
import com.github.shk0da.goldendragon.model.MarketTradeTick;
import com.github.shk0da.goldendragon.model.TickerInfo;
import com.github.shk0da.goldendragon.model.TickerType;
import com.github.shk0da.goldendragon.money.KillSwitch;
import com.github.shk0da.goldendragon.money.RiskManager;
import com.github.shk0da.goldendragon.service.TCSService;
import com.github.shk0da.goldendragon.strategy.OrderBookScalpScreener;
import com.github.shk0da.goldendragon.utils.LoggingUtils;
import com.github.shk0da.goldendragon.utils.TickerTypeResolver;
import java.time.Duration;
import java.time.Instant;
import java.time.LocalTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Shared order-book trading engine: subscriptions, screening, position management and execution.
 *
 * <p>Delegates entry/exit signals to pluggable {@link OrderBookSignal} implementations. One
 * position per ticker; first signal in priority order wins entry.
 */
public final class OrderBookTradingEngine implements MarketTickListener {

    private static final LocalTime WORK_START = LocalTime.of(10, 0);
    private static final LocalTime WORK_END = LocalTime.of(21, 0);
    private static final ZoneId MOSCOW = ZoneId.of("Europe/Moscow");
    private static final double TP_COMMISSION_SAFETY = 1.5;

    private final TCSService tcsService;
    private final MainConfig mainConfig;
    private final OrderBookScalpConfig config;
    private final List<OrderBookSignal> signals;
    private final Map<String, OrderBookSignal> signalsById;
    private final String strategyName;
    private final Map<String, TickerRuntime> runtimesByTicker = new ConcurrentHashMap<>();
    private final Map<String, TickerRuntime> runtimesByFigi = new ConcurrentHashMap<>();
    private final TradeStats tradeStats = new TradeStats();
    private final KillSwitch killSwitch;
    private final RiskManager riskManager;
    private volatile long lastStreamErrorLogMs;
    private volatile long nextRescreenMs;
    private volatile double initialEquity;

    public OrderBookTradingEngine(
            TCSService tcsService,
            MainConfig mainConfig,
            OrderBookScalpConfig config,
            List<OrderBookSignal> signals,
            String strategyName) {
        this.tcsService = tcsService;
        this.mainConfig = mainConfig;
        this.config = config;
        this.signals = List.copyOf(signals);
        this.signalsById = new HashMap<>();
        for (OrderBookSignal signal : this.signals) {
            this.signalsById.put(signal.id(), signal);
        }
        this.strategyName = strategyName;
        if (config.isRiskManagementEnabled()) {
            this.killSwitch = new KillSwitch(config.getCriticalDrawdownPercent());
            this.riskManager =
                    new RiskManager(
                            config.getRiskPerTradePercent(),
                            config.getMaxDailyLossPercent(),
                            config.getMaxConsecutiveLosses());
        } else {
            this.killSwitch = null;
            this.riskManager = null;
        }
    }

    public void run() {
        boolean paper = config.isPaperMode() || mainConfig.isTestMode();
        List<String> signalIds = signals.stream().map(OrderBookSignal::id).collect(toList());
        log(
                strategyName
                        + " start: instruments="
                        + config.getInstruments()
                        + ", depth="
                        + config.getDepth()
                        + ", paper="
                        + paper
                        + ", positionCash="
                        + config.getPositionCash()
                        + ", screeningTopN="
                        + config.getScreeningTopN()
                        + ", signals="
                        + signalIds);

        telegramNotifyService.sendMessage(strategyName + " started (paper=" + paper + ")");

        tcsService.logAccountTradingEligibility();

        List<TickerRuntime> subscribed = subscribeInstruments(resolveInstruments());
        if (subscribed.isEmpty()) {
            log(strategyName + ": no instruments subscribed, stopping");
            return;
        }

        nextRescreenMs = System.currentTimeMillis() + config.getRescreenMinutes() * 60_000L;

        try {
            while (isWithinWorkingHours()) {
                if (isAllFuturesMode(config.getInstruments())
                        && System.currentTimeMillis() >= nextRescreenMs) {
                    rescreenSubscriptions(subscribed, paper);
                    nextRescreenMs =
                            System.currentTimeMillis() + config.getRescreenMinutes() * 60_000L;
                }
                sleep(1_000);
            }
        } finally {
            for (TickerRuntime runtime : subscribed) {
                tcsService.unsubscribeMarketData(runtime.key, this);
                closeOpenPosition(runtime, "session_end", paper);
            }
            logStats("session_end");
            telegramNotifyService.sendMessage(strategyName + " stopped");
            log(strategyName + " stopped");
        }
    }

    @Override
    public void onOrderBook(MarketDepthSnapshot snapshot) {
        if (snapshot == null || !snapshot.isConsistent()) {
            return;
        }

        TickerRuntime runtime = runtimesByFigi.get(snapshot.getFigi());
        if (runtime == null) {
            return;
        }

        synchronized (runtime) {
            handleOrderBook(runtime, snapshot);
        }
    }

    @Override
    public void onTrade(MarketTradeTick trade) {
        // trade flow is read from TCSService recent trades buffer on each book update
    }

    @Override
    public void onError(Throwable throwable) {
        long now = System.currentTimeMillis();
        if (now - lastStreamErrorLogMs < 5_000L) {
            return;
        }
        lastStreamErrorLogMs = now;
        log(strategyName + " stream error: " + throwable.getMessage());
    }

    private void handleOrderBook(TickerRuntime runtime, MarketDepthSnapshot snapshot) {
        if (!isWithinWorkingHours()) {
            return;
        }

        Double bestBid = snapshot.getBestBid();
        Double bestAsk = snapshot.getBestAsk();
        if (bestBid == null || bestAsk == null || bestAsk <= bestBid) {
            return;
        }

        int bidQty0 = OrderBookMath.topQuantity(snapshot.getBids(), 0);
        int askQty0 = OrderBookMath.topQuantity(snapshot.getAsks(), 0);
        if (bidQty0 < config.getMinBestLevelQty() || askQty0 < config.getMinBestLevelQty()) {
            return;
        }

        double spread = bestAsk - bestBid;
        double mid = (bestBid + bestAsk) / 2.0;
        double spreadBps = mid > 0.0 ? spread / mid * 10_000.0 : Double.MAX_VALUE;
        if (spreadBps > config.getMaxSpreadBps()) {
            return;
        }

        boolean paper = config.isPaperMode() || mainConfig.isTestMode();
        double obi =
                OrderBookMath.calculateObi(
                        snapshot.getBids(), snapshot.getAsks(), config.getObiLevels());
        double microEdge = OrderBookMath.calculateMicroEdge(bestBid, bestAsk, bidQty0, askQty0);
        double tradeDelta = calculateTradeDelta(runtime.key);

        OrderBookMarketContext context =
                new OrderBookMarketContext(
                        snapshot,
                        runtime.key,
                        runtime.ticker,
                        bestBid,
                        bestAsk,
                        spread,
                        spreadBps,
                        bidQty0,
                        askQty0,
                        obi,
                        microEdge,
                        tradeDelta);

        if (runtime.openPosition != null) {
            manageOpenPosition(runtime, context, bestBid, spread, paper);
            return;
        }

        if (System.currentTimeMillis() < runtime.cooldownUntilMs) {
            return;
        }

        if (!isProfitableAfterCommission(bestAsk, spread)) {
            return;
        }

        if (riskManager != null && !riskManager.canTrade(initialEquity + tradeStats.netPnl)) {
            return;
        }

        if (killSwitch != null && !killSwitch.isTradingAllowed()) {
            return;
        }

        for (OrderBookSignal signal : signals) {
            OrderBookEntryDecision decision = signal.evaluateEntry(context, runtime.ticker);
            if (!decision.isEnter()) {
                continue;
            }
            openLong(
                    runtime,
                    bestBid,
                    bestAsk,
                    spread,
                    signal.id(),
                    decision.getDescription(),
                    paper);
            for (OrderBookSignal s : signals) {
                s.reset(runtime.ticker);
            }
            break;
        }

        if (config.isShortsEnabled() && runtime.openPosition == null) {
            for (OrderBookSignal signal : signals) {
                OrderBookEntryDecision decision =
                        signal.evaluateEntryShort(context, runtime.ticker);
                if (!decision.isEnter()) {
                    continue;
                }
                openShort(
                        runtime,
                        bestBid,
                        bestAsk,
                        spread,
                        signal.id(),
                        decision.getDescription(),
                        paper);
                for (OrderBookSignal s : signals) {
                    s.reset(runtime.ticker);
                }
                break;
            }
        }
    }

    private void manageOpenPosition(
            TickerRuntime runtime,
            OrderBookMarketContext context,
            double bestBid,
            double spread,
            boolean paper) {
        OpenPosition position = runtime.openPosition;
        if (position == null) {
            return;
        }

        boolean isLong = "LONG".equals(position.direction);
        double currentPrice = isLong ? bestBid : context.getBestAsk();

        long heldSeconds = Duration.between(position.entryTime, Instant.now()).getSeconds();
        if (heldSeconds >= config.getMaxHoldSeconds()) {
            closeOpenPosition(runtime, "time_stop", paper);
            return;
        }

        boolean inGracePeriod = heldSeconds < config.getEntryGraceSeconds();

        if (!inGracePeriod) {
            boolean tpHit =
                    isLong
                            ? currentPrice >= position.takeProfitPrice
                            : currentPrice <= position.takeProfitPrice;
            if (tpHit) {
                closeOpenPosition(runtime, "take_profit", paper);
                return;
            }
        }

        if (inGracePeriod) {
            return;
        }

        if (config.isTrailingEnabled() && position.spreadAtEntry > 0) {
            double profit =
                    isLong
                            ? currentPrice - position.entryPrice
                            : position.entryPrice - currentPrice;
            double activationThreshold =
                    config.getTrailingActivationSpreads() * position.spreadAtEntry;
            if (profit >= activationThreshold) {
                double newSl =
                        isLong
                                ? currentPrice
                                        - config.getTrailingStepSpreads() * position.spreadAtEntry
                                : currentPrice
                                        + config.getTrailingStepSpreads() * position.spreadAtEntry;
                boolean slImproved =
                        isLong ? newSl > position.stopLossPrice : newSl < position.stopLossPrice;
                if (slImproved) {
                    position.stopLossPrice = newSl;
                }
            }
        }

        boolean slHit =
                isLong
                        ? currentPrice <= position.stopLossPrice
                        : currentPrice >= position.stopLossPrice;
        if (slHit) {
            closeOpenPosition(runtime, "stop_loss", paper);
            return;
        }

        if (isLong && context.getMicroEdge() < 0) {
            closeOpenPosition(runtime, "microprice_reversal", paper);
            return;
        }

        if (!isLong && context.getMicroEdge() > 0) {
            closeOpenPosition(runtime, "microprice_reversal", paper);
            return;
        }

        if (isLong && context.getTradeDelta() < -config.getMinTradeFlow()) {
            closeOpenPosition(runtime, "flow_reversal", paper);
            return;
        }

        if (!isLong && context.getTradeDelta() > config.getMinTradeFlow()) {
            closeOpenPosition(runtime, "flow_reversal", paper);
            return;
        }

        if (context.getSpreadBps() > config.getMaxSpreadBps() * 1.5) {
            closeOpenPosition(runtime, "spread_widen", paper);
            return;
        }

        OrderBookSignal activeSignal = signalsById.get(position.signalId);
        if (activeSignal == null) {
            return;
        }
        String signalExit = activeSignal.evaluateExit(context, position, runtime.ticker);
        if (signalExit != null) {
            closeOpenPosition(runtime, signalExit, paper);
        }
    }

    private void openLong(
            TickerRuntime runtime,
            double entryBid,
            double entryAsk,
            double spread,
            String signalId,
            String signalDescription,
            boolean paper) {
        if (!tcsService.isTradableForAccount(runtime.tickerInfo)) {
            log("Skip OPEN " + runtime.ticker + ": not tradable for current account");
            return;
        }

        int units = tcsService.calculateTradeCount(runtime.key, config.getPositionCash(), entryAsk);
        if (units <= 0) {
            log("Skip OPEN " + runtime.ticker + ": insufficient cash for one lot");
            return;
        }

        int lot = Math.max(1, runtime.lot);
        BracketPrices bracket = buildBracketPrices(entryBid, entryAsk, spread);
        double tpPercent = entryAsk > 0.0 ? bracket.tpDistance / entryAsk * 100.0 : 0.0;
        double slPercent = entryAsk > 0.0 ? (entryAsk - bracket.slPrice) / entryAsk * 100.0 : 0.0;

        log(
                "OPEN signal ["
                        + signalId
                        + "] "
                        + runtime.ticker
                        + ": "
                        + signalDescription
                        + ", entryAsk="
                        + entryAsk);

        if (paper) {
            double entryValue = units * entryAsk * lot;
            runtime.openPosition =
                    new OpenPosition(
                            signalId,
                            "LONG",
                            entryAsk,
                            spread,
                            Instant.now(),
                            bracket.tpPrice,
                            bracket.slPrice,
                            units,
                            lot,
                            entryValue,
                            entryValue * config.getCommissionRate());
            runtime.cooldownUntilMs =
                    System.currentTimeMillis() + config.getCooldownSeconds() * 1000L;
            log(
                    "PAPER OPEN ["
                            + signalId
                            + "] "
                            + runtime.ticker
                            + " entry="
                            + entryAsk
                            + " units="
                            + units
                            + " tp="
                            + bracket.tpPrice
                            + " sl="
                            + bracket.slPrice);
            return;
        }

        TCSService.OrderExecutionResult result =
                tcsService.buyByMarketWithDetails(
                        runtime.ticker,
                        runtime.key.getType(),
                        config.getPositionCash(),
                        tpPercent,
                        slPercent);

        if (!result.isSuccess()) {
            log("OPEN failed for " + runtime.ticker);
            return;
        }

        double executedEntry =
                result.getExecutedPrice() != null ? result.getExecutedPrice() : entryAsk;
        int executedUnits = result.getExecutedCount() > 0 ? result.getExecutedCount() : units;
        BracketPrices executedBracket = buildBracketPrices(entryBid, executedEntry, spread);
        double entryValue = executedUnits * executedEntry * lot;
        runtime.openPosition =
                new OpenPosition(
                        signalId,
                        "LONG",
                        executedEntry,
                        spread,
                        Instant.now(),
                        executedBracket.tpPrice,
                        executedBracket.slPrice,
                        executedUnits,
                        lot,
                        entryValue,
                        result.getCommission());
        runtime.cooldownUntilMs = System.currentTimeMillis() + config.getCooldownSeconds() * 1000L;
        telegramNotifyService.sendMessage(
                strategyName
                        + " OPEN ["
                        + signalId
                        + "] "
                        + runtime.ticker
                        + " entry="
                        + executedEntry
                        + " tp="
                        + executedBracket.tpPrice
                        + " sl="
                        + executedBracket.slPrice);
    }

    private void openShort(
            TickerRuntime runtime,
            double entryBid,
            double entryAsk,
            double spread,
            String signalId,
            String signalDescription,
            boolean paper) {
        if (!tcsService.isTradableForAccount(runtime.tickerInfo)) {
            log("Skip SHORT " + runtime.ticker + ": not tradable for current account");
            return;
        }

        int units = tcsService.calculateTradeCount(runtime.key, config.getPositionCash(), entryBid);
        if (units <= 0) {
            log("Skip SHORT " + runtime.ticker + ": insufficient cash for one lot");
            return;
        }

        int lot = Math.max(1, runtime.lot);
        BracketPrices bracket = buildBracketPricesShort(entryBid, entryAsk, spread);

        log(
                "SHORT signal ["
                        + signalId
                        + "] "
                        + runtime.ticker
                        + ": "
                        + signalDescription
                        + ", entryBid="
                        + entryBid);

        if (paper) {
            double entryValue = units * entryBid * lot;
            runtime.openPosition =
                    new OpenPosition(
                            signalId,
                            "SHORT",
                            entryBid,
                            spread,
                            Instant.now(),
                            bracket.tpPrice,
                            bracket.slPrice,
                            units,
                            lot,
                            entryValue,
                            entryValue * config.getCommissionRate());
            runtime.cooldownUntilMs =
                    System.currentTimeMillis() + config.getCooldownSeconds() * 1000L;
            log(
                    "PAPER SHORT ["
                            + signalId
                            + "] "
                            + runtime.ticker
                            + " entry="
                            + entryBid
                            + " units="
                            + units
                            + " tp="
                            + bracket.tpPrice
                            + " sl="
                            + bracket.slPrice);
            return;
        }

        // For real trading, short selling requires margin account
        log("SHORT not supported in real mode yet: " + runtime.ticker);
    }

    private BracketPrices buildBracketPricesShort(double entryBid, double entryAsk, double spread) {
        double minTpDistance = entryBid * config.getCommissionRate() * 2.0 * TP_COMMISSION_SAFETY;
        double tpDistance = Math.max(spread * config.getTakeProfitSpreads(), minTpDistance);
        double slDistance = spread * config.getStopLossSpreads();
        double tpPrice = Math.max(0.0, entryBid - tpDistance);
        double slPrice = entryAsk + slDistance;
        return new BracketPrices(tpPrice, slPrice, tpDistance);
    }

    private void closeOpenPosition(TickerRuntime runtime, String reason, boolean paper) {
        OpenPosition position = runtime.openPosition;
        if (position == null) {
            return;
        }

        if (paper) {
            Map<String, Map<Double, Integer>> book =
                    tcsService.getCurrentPrices(runtime.key, false);
            boolean isLong = "LONG".equals(position.direction);
            double grossPnl;
            double exitPrice;
            if (isLong) {
                double fallback = position.entryPrice - position.spreadAtEntry;
                exitPrice = resolveBestBid(book, fallback);
                grossPnl = (exitPrice - position.entryPrice) * position.units * position.lot;
            } else {
                double fallback = position.entryPrice + position.spreadAtEntry;
                exitPrice = resolveBestAsk(book, fallback);
                grossPnl = (position.entryPrice - exitPrice) * position.units * position.lot;
            }
            double exitValue = position.units * exitPrice * position.lot;
            double commission = (position.entryValue + exitValue) * config.getCommissionRate();
            double netPnl = grossPnl - commission;
            tradeStats.record(netPnl);
            if (riskManager != null) {
                riskManager.registerTrade(netPnl);
            }
            log(
                    "PAPER CLOSE ["
                            + position.signalId
                            + "] "
                            + runtime.ticker
                            + " reason="
                            + reason
                            + " entry="
                            + position.entryPrice
                            + " exit="
                            + exitPrice
                            + " gross="
                            + String.format("%.2f", grossPnl)
                            + " commission="
                            + String.format("%.2f", commission)
                            + " net="
                            + String.format("%.2f", netPnl));
            runtime.openPosition = null;
            runtime.cooldownUntilMs =
                    System.currentTimeMillis() + config.getCooldownSeconds() * 1000L;
            logStatsIfNeeded();
            return;
        }

        TCSService.OrderExecutionResult result =
                tcsService.closeLongByMarketWithDetails(runtime.ticker, runtime.key.getType());
        if (!result.isSuccess()) {
            log("CLOSE failed for " + runtime.ticker + " reason=" + reason);
            return;
        }

        double exitPrice =
                result.getExecutedPrice() != null ? result.getExecutedPrice() : position.entryPrice;
        double exitValue = position.units * exitPrice * position.lot;
        double grossPnl = (exitPrice - position.entryPrice) * position.units * position.lot;
        double exitCommission = result.getCommission();
        double netPnl = grossPnl - position.entryCommission - exitCommission;
        tradeStats.record(netPnl);
        if (riskManager != null) {
            riskManager.registerTrade(netPnl);
        }
        runtime.openPosition = null;
        runtime.cooldownUntilMs = System.currentTimeMillis() + config.getCooldownSeconds() * 1000L;
        telegramNotifyService.sendMessage(
                strategyName
                        + " CLOSE ["
                        + position.signalId
                        + "] "
                        + runtime.ticker
                        + " ("
                        + reason
                        + ") net="
                        + String.format("%.2f", netPnl));
        log(
                "CLOSE ["
                        + position.signalId
                        + "] "
                        + runtime.ticker
                        + " reason="
                        + reason
                        + " net="
                        + String.format("%.2f", netPnl));
        logStatsIfNeeded();
    }

    private BracketPrices buildBracketPrices(double entryBid, double entryAsk, double spread) {
        double minTpDistance = entryAsk * config.getCommissionRate() * 2.0 * TP_COMMISSION_SAFETY;
        double tpDistance = Math.max(spread * config.getTakeProfitSpreads(), minTpDistance);
        double slDistance = spread * config.getStopLossSpreads();
        double tpPrice = entryAsk + tpDistance;
        double slPrice = Math.max(0.0, entryBid - slDistance);
        return new BracketPrices(tpPrice, slPrice, tpDistance);
    }

    private boolean isProfitableAfterCommission(double entryAsk, double spread) {
        double minTpDistance = entryAsk * config.getCommissionRate() * 2.0 * TP_COMMISSION_SAFETY;
        return spread * config.getTakeProfitSpreads() >= minTpDistance;
    }

    private List<TickerRuntime> subscribeInstruments(List<TickerInfo> instruments) {
        List<TickerRuntime> subscribed = new ArrayList<>();
        for (TickerInfo info : instruments) {
            TickerRuntime runtime = subscribeSingle(info);
            if (runtime != null) {
                subscribed.add(runtime);
            }
        }
        log(strategyName + " subscribed to " + subscribed.size() + " instruments");
        return subscribed;
    }

    private TickerRuntime subscribeSingle(TickerInfo info) {
        String ticker = info.getTicker();
        if (!tcsService.isTradableForAccount(info)) {
            log(
                    "Skip "
                            + ticker
                            + ": not tradable (qualOnly="
                            + info.isForQualInvestorFlag()
                            + ", apiTrade="
                            + info.isApiTradeAvailableFlag()
                            + ", normalTrading="
                            + info.isNormalTradingStatus()
                            + ")");
            return null;
        }
        TickerInfo.Key key = info.getKey();
        try {
            int lot = Math.max(1, info.getLot());
            TickerRuntime runtime = new TickerRuntime(ticker, key, info.getFigi(), lot, info);
            runtimesByTicker.put(ticker, runtime);
            runtimesByFigi.put(info.getFigi(), runtime);
            tcsService.subscribeMarketData(key, config.getDepth(), this);
            log("Subscribed to order book: " + ticker + " (" + key.getType() + ")");
            return runtime;
        } catch (Exception ex) {
            log("Failed to subscribe " + ticker + ": " + ex.getMessage());
            return null;
        }
    }

    private void rescreenSubscriptions(List<TickerRuntime> subscribed, boolean paper) {
        log("Rescreening order-book universe...");
        List<TickerInfo> refreshed = resolveInstruments();
        Set<String> targetTickers = refreshed.stream().map(TickerInfo::getTicker).collect(toSet());
        Set<String> currentTickers =
                subscribed.stream().map(runtime -> runtime.ticker).collect(toSet());

        for (TickerRuntime runtime : new ArrayList<>(subscribed)) {
            if (targetTickers.contains(runtime.ticker)) {
                continue;
            }
            closeOpenPosition(runtime, "rescreen_exit", paper);
            tcsService.unsubscribeMarketData(runtime.key, this);
            runtimesByTicker.remove(runtime.ticker);
            runtimesByFigi.remove(runtime.figi);
            subscribed.remove(runtime);
            log("Rescreen unsubscribe: " + runtime.ticker);
        }

        for (TickerInfo info : refreshed) {
            if (currentTickers.contains(info.getTicker())) {
                continue;
            }
            TickerRuntime runtime = subscribeSingle(info);
            if (runtime != null) {
                subscribed.add(runtime);
                log("Rescreen subscribe: " + info.getTicker());
            }
        }
        log("Rescreen complete: watching " + subscribed.size() + " instruments");
    }

    private double resolveBestBid(Map<String, Map<Double, Integer>> book, double fallback) {
        if (book == null || !book.containsKey("bids") || book.get("bids").isEmpty()) {
            return fallback;
        }
        return book.get("bids").keySet().stream()
                .mapToDouble(Double::doubleValue)
                .max()
                .orElse(fallback);
    }

    private double resolveBestAsk(Map<String, Map<Double, Integer>> book, double fallback) {
        if (book == null || !book.containsKey("asks") || book.get("asks").isEmpty()) {
            return fallback;
        }
        return book.get("asks").keySet().stream()
                .mapToDouble(Double::doubleValue)
                .min()
                .orElse(fallback);
    }

    private double calculateTradeDelta(TickerInfo.Key key) {
        List<MarketTradeTick> trades =
                tcsService.getRecentTrades(
                        key, Duration.ofSeconds(config.getTradeFlowWindowSeconds()));
        return OrderBookMath.calculateTradeDelta(trades);
    }

    private boolean isWithinWorkingHours() {
        LocalTime now = LocalTime.now(MOSCOW);
        return !now.isBefore(WORK_START) && now.isBefore(WORK_END);
    }

    private List<TickerInfo> resolveInstruments() {
        List<String> configured = config.getInstruments();
        if (isAllFuturesMode(configured)) {
            Map<TickerInfo.Key, TickerInfo> allFutures = tcsService.getFuturesList();
            List<TickerInfo> candidates =
                    allFutures.values().stream()
                            .filter(info -> "rub".equalsIgnoreCase(info.getCurrency()))
                            .filter(tcsService::isTradableForAccount)
                            .sorted(Comparator.comparing(TickerInfo::getTicker))
                            .collect(toList());
            log(
                    "Loaded "
                            + allFutures.size()
                            + " RUB futures, "
                            + candidates.size()
                            + " tradable for current account");
            return OrderBookScalpScreener.selectTop(tcsService, candidates, config);
        }

        List<TickerInfo> resolved = new ArrayList<>();
        for (String ticker : configured) {
            TickerType type = TickerTypeResolver.resolve(ticker);
            if (TickerType.UNKNOWN == type) {
                log("Skip " + ticker + ": unknown instrument type");
                continue;
            }
            try {
                TickerInfo info = tcsService.searchTicker(new TickerInfo.Key(ticker, type));
                if (!tcsService.isTradableForAccount(info)) {
                    log("Skip " + ticker + ": not tradable for current account");
                    continue;
                }
                resolved.add(info);
            } catch (Exception ex) {
                log("Failed to resolve " + ticker + ": " + ex.getMessage());
            }
        }
        return resolved;
    }

    private static boolean isAllFuturesMode(List<String> configured) {
        return configured.size() == 1 && "ALL".equalsIgnoreCase(configured.get(0).trim());
    }

    private void logStatsIfNeeded() {
        if (tradeStats.trades % 5 == 0) {
            logStats("progress");
        }
    }

    private void logStats(String label) {
        log(
                strategyName
                        + " stats ["
                        + label
                        + "]: trades="
                        + tradeStats.trades
                        + ", wins="
                        + tradeStats.wins
                        + ", winRate="
                        + String.format("%.1f%%", tradeStats.winRatePercent())
                        + ", netPnl="
                        + String.format("%.2f", tradeStats.netPnl));
    }

    private static void log(String message) {
        LoggingUtils.log(message);
    }

    private static final class BracketPrices {

        final double tpPrice;
        final double slPrice;
        final double tpDistance;

        private BracketPrices(double tpPrice, double slPrice, double tpDistance) {
            this.tpPrice = tpPrice;
            this.slPrice = slPrice;
            this.tpDistance = tpDistance;
        }
    }

    private static final class TradeStats {

        private int trades;
        private int wins;
        private double netPnl;

        private void record(double pnl) {
            trades++;
            if (pnl > 0.0) {
                wins++;
            }
            netPnl += pnl;
        }

        private double winRatePercent() {
            if (trades == 0) {
                return 0.0;
            }
            return wins * 100.0 / trades;
        }
    }

    private static final class OpenPosition implements OrderBookPositionView {

        final String signalId;
        final String direction;
        final double entryPrice;
        final double spreadAtEntry;
        final Instant entryTime;
        final double takeProfitPrice;
        volatile double stopLossPrice;
        final int units;
        final int lot;
        final double entryValue;
        final double entryCommission;

        OpenPosition(
                String signalId,
                String direction,
                double entryPrice,
                double spreadAtEntry,
                Instant entryTime,
                double takeProfitPrice,
                double stopLossPrice,
                int units,
                int lot,
                double entryValue,
                double entryCommission) {
            this.signalId = signalId;
            this.direction = direction;
            this.entryPrice = entryPrice;
            this.spreadAtEntry = spreadAtEntry;
            this.entryTime = entryTime;
            this.takeProfitPrice = takeProfitPrice;
            this.stopLossPrice = stopLossPrice;
            this.units = units;
            this.lot = lot;
            this.entryValue = entryValue;
            this.entryCommission = entryCommission;
        }

        @Override
        public String getSignalId() {
            return signalId;
        }

        public String getDirection() {
            return direction;
        }

        @Override
        public double getEntryPrice() {
            return entryPrice;
        }

        @Override
        public double getSpreadAtEntry() {
            return spreadAtEntry;
        }

        @Override
        public Instant getEntryTime() {
            return entryTime;
        }

        @Override
        public double getTakeProfitPrice() {
            return takeProfitPrice;
        }

        @Override
        public double getStopLossPrice() {
            return stopLossPrice;
        }

        @Override
        public long getHeldSeconds() {
            return Duration.between(entryTime, Instant.now()).getSeconds();
        }
    }

    private static class TickerRuntime {

        final String ticker;
        final TickerInfo.Key key;
        final String figi;
        final int lot;
        final TickerInfo tickerInfo;
        volatile long cooldownUntilMs;
        volatile OpenPosition openPosition;

        TickerRuntime(
                String ticker, TickerInfo.Key key, String figi, int lot, TickerInfo tickerInfo) {
            this.ticker = ticker;
            this.key = key;
            this.figi = figi;
            this.lot = lot;
            this.tickerInfo = tickerInfo;
        }
    }
}
