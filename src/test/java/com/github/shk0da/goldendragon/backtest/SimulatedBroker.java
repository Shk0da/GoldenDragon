package com.github.shk0da.goldendragon.backtest;

import com.github.shk0da.goldendragon.market.MarketDataProvider;
import com.github.shk0da.goldendragon.market.MarketPrices;
import com.github.shk0da.goldendragon.market.OrderExecutor;
import com.github.shk0da.goldendragon.model.Candle;
import com.github.shk0da.goldendragon.model.Position;
import com.github.shk0da.goldendragon.model.PositionInfo;
import com.github.shk0da.goldendragon.model.TickerInfo;
import com.github.shk0da.goldendragon.model.TickerType;
import com.github.shk0da.goldendragon.repository.TickerRepository;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Simulated broker - the SINGLE source of truth for cash, positions and PnL in backtests.
 *
 * <p>Implements both {@link MarketDataProvider} and {@link OrderExecutor} so that
 * {@link com.github.shk0da.goldendragon.strategy.BaseStrategy#processTicker} (and the backtest
 * engine itself) can execute through the same abstraction as live trading.</p>
 *
 * <p>Design rules:</p>
 * <ul>
 *   <li>All cash accounting happens here via {@code sharedCash}; no caller tracks money.</li>
 *   <li>All position state lives in {@link SimulatedPosition}; the engine only reads it.</li>
 *   <li>Every price read uses the CURRENT bar (set via {@link #setCurrentTime}), never the last
 *       element of the loaded history (avoids look-ahead).</li>
 *   <li>Commissions are symmetric: entry notional and exit notional both charged; for short a
 *       margin is posted at open and returned at close.</li>
 *   <li>Default SL/TP (2%/4%) applied when a strategy provides none, mirroring live
 *       {@code openPosition} behavior.</li>
 * </ul>
 */
public class SimulatedBroker implements MarketDataProvider, OrderExecutor {

    private static final DateTimeFormatter DATE_TIME_FMT =
            DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm:ss");

    /** Default SL % below/above entry for long/short when strategy provides none. Configurable via UnifiedTraderConfig. */
    private final double defaultSlPercent;

    /** Default TP % above/below entry for long/short when strategy provides none. Configurable via UnifiedTraderConfig. */
    private final double defaultTpPercent;

    /** Margin ratio posted for short positions (mirrors live 30%). Configurable via UnifiedTraderConfig. */
    private final double shortMarginRatio;

    private final int maxConcurrentPositions;

    /**
     * Get the cash parking ticker based on the instrument type.
     * For crypto (USDT pairs): returns "SPYUSDT"
     * For Tinkoff (stocks/ETFs): returns "TMON@"
     */
    private String getParkingTicker(String ticker) {
        if (ticker != null && ticker.endsWith("USDT")) {
            return "SPYUSDT";
        }
        return "TMON@";
    }

    /**
     * Check if this is TMON@ — the free cash parking ETF that is excluded from position count
     * and trade history. SPYUSDT is NOT special — it trades like any other instrument.
     */
    private boolean isTmonParking(String ticker) {
        return "TMON@".equals(ticker);
    }

    private final double initialBalance;
    private volatile double sharedCash;
    /** Diagnostic: totals of recorded trade pnl vs raw cash movements (to find reconciliation drift). */
    private double recordedTradePnl = 0.0;
    private double unrecordedCashDelta = 0.0;
    private double allCashMutations = 0.0;
    private double lastSharedCash = 0.0;
    private double tmonCashFlow = 0.0;
    private double totalCloseCashDelta = 0.0;
    private double totalCloseCashMinusPnl = 0.0;
    private int closeCount = 0;
    private int partialCloseCount = 0;
    private double totalOpenDeduction = 0.0;
    private int openCount = 0;
    private final Map<String, Double> closeCashMinusPnlByReason = new ConcurrentHashMap<>();
    private final Map<String, Integer> closeCountByReason = new ConcurrentHashMap<>();
    /** Per-ticker tracking for debugging reconciliation drift. */
    private final Map<String, Double> openDeductionByTicker = new ConcurrentHashMap<>();
    private final Map<String, Double> closeCashByTicker = new ConcurrentHashMap<>();
    private final Map<String, Double> recordedPnlByTicker = new ConcurrentHashMap<>();
    private final Map<String, SimulatedPosition> positions = new ConcurrentHashMap<>();
    private final Map<String, Map<String, List<Candle>>> candlesByTickerAndInterval =
            new ConcurrentHashMap<>();
    /** Cached parsed times for O(log N) lookup in getCandles. */
    private final Map<String, Map<String, List<LocalDateTime>>> timesByTickerAndInterval =
            new ConcurrentHashMap<>();
    private final double commissionRate;
    private final double slippage;

    /** Current simulation time; all price reads must reference the bar at this time. */
    private volatile LocalDateTime currentTime = null;

    private final List<BacktestTrade> tradeHistory =
            Collections.synchronizedList(new ArrayList<>());

    private int concurrentOpenPeak = 0;

    /**
     * Accumulator for realized PnL from TMON@ cash parking operations.
     * TMON@ entries/exits are not written to tradeHistory (to preserve Opens==Closes),
     * but the difference between entry value and exit value must be tracked for D1 reconciliation.
     * Populated by closeTmonParking() and sellByMarket() when TMON@ is sold.
     */
    private double tmonRealizedPnl = 0.0;

    /** Safety margin matching LiveOrderExecutor (1%) to prevent insufficient funds on market orders. */
    private static final double ORDER_QUANTITY_SAFETY_MARGIN = 1.01;

    /**
     * Kill Switch state: peak portfolio value for drawdown tracking.
     * Initialized in constructor.
     */
    private double portfolioPeak;

    /**
     * Kill Switch threshold: maximum allowed drawdown as decimal (e.g., 0.20 for 20%).
     */
    private final double maxDrawdownThreshold;

    /**
     * Kill Switch flag: if true, all new OPEN operations are blocked.
     */
    private boolean killSwitchTriggered = false;
    
    /**
     * Internal flag to prevent recursive Kill Switch checks during reset.
     */
    private boolean isResettingKillSwitch = false;

    public double getTmonRealizedPnl() {
        return tmonRealizedPnl;
    }

    public boolean isKillSwitchTriggered() {
        return killSwitchTriggered;
    }

    /**
     * Reset Kill Switch and allow trading again.
     * Also resets portfolio peak to current value to avoid immediate re-trigger.
     */
    public void resetKillSwitch() {
        killSwitchTriggered = false;
        portfolioPeak = getTotalPortfolioValue();
        System.out.println("Kill Switch RESET — trading resumed (peak reset to current portfolio value)");
    }

    public SimulatedBroker(double initialBalance, double commissionRate, double slippage) {
        this(initialBalance, commissionRate, slippage, 0.20); // default 20% max DD
    }

    public SimulatedBroker(double initialBalance, double commissionRate, double slippage, double maxDrawdownThreshold) {
        this(initialBalance, commissionRate, slippage, maxDrawdownThreshold,
             2.0, 4.0, 0.30, 8);
    }

    public SimulatedBroker(double initialBalance, double commissionRate, double slippage,
                           double maxDrawdownThreshold,
                           double defaultSlPercent, double defaultTpPercent,
                           double shortMarginRatio, int maxConcurrentPositions) {
        this.initialBalance = initialBalance;
        this.sharedCash = initialBalance;
        this.lastSharedCash = initialBalance;
        this.commissionRate = commissionRate;
        this.slippage = slippage;
        this.maxDrawdownThreshold = maxDrawdownThreshold;
        this.portfolioPeak = initialBalance;
        this.killSwitchTriggered = false;
        this.defaultSlPercent = defaultSlPercent;
        this.defaultTpPercent = defaultTpPercent;
        this.shortMarginRatio = shortMarginRatio;
        this.maxConcurrentPositions = maxConcurrentPositions;
    }

    /**
     * Simulated position with all metadata required for backtest parity.
     */
    public static class SimulatedPosition {
        public final String ticker;
        public Position position;      // Mutable for HOLD updates / trailing
        public double entryPrice;
        public int appliedLeverage;
        /** Margin posted at open (short only); returned to cash on close. */
        public double postedMargin;
        public int lotSize;
        public long entryBarIndex;
        /**
         * Source of truth for cooldown state in backtest.
         * Decrementing by tickCooldown() while position is closed; blocks new OPEN while > 0.
         * Note: Position.cooldownRemaining is not used by the backtest engine.
         */
        public int cooldownRemaining;

        public SimulatedPosition(String ticker) {
            this.ticker = ticker;
            this.position = new Position();
            this.entryPrice = 0.0;
            this.appliedLeverage = 1;
            this.postedMargin = 0.0;
            this.lotSize = 1;
            this.entryBarIndex = -1;
            this.cooldownRemaining = 0;
        }

        public boolean hasOpenPosition() {
            return position != null && position.quantity > 0;
        }

        public boolean isShort() {
            return hasOpenPosition() && "SELL".equals(position.direction);
        }

        public boolean isLong() {
            return hasOpenPosition() && "BUY".equals(position.direction);
        }

        public double getMarketValue(double currentPrice) {
            if (!hasOpenPosition()) {
                return 0.0;
            }
            long notional = (long) position.quantity * lotSize;
            return notional * currentPrice;
        }
    }

    /**
     * Immutable trade record for verification and reporting.
     */
    public static class BacktestTrade {
        public final String ticker;
        public final String direction;   // BUY or SELL
        public final String action;      // OPEN or CLOSE
        public final double entryPrice;
        public final double exitPrice;
        public final int quantity;
        public final double pnl;         // net of commissions
        public final double commission;
        public final String reason;
        public final String time;
        public final long barIndex;

        public BacktestTrade(
                String ticker, String direction, String action,
                double entryPrice, double exitPrice, int quantity,
                double pnl, double commission, String reason, String time, long barIndex) {
            this.ticker = ticker;
            this.direction = direction;
            this.action = action;
            this.entryPrice = entryPrice;
            this.exitPrice = exitPrice;
            this.quantity = quantity;
            this.pnl = pnl;
            this.commission = commission;
            this.reason = reason;
            this.time = time;
            this.barIndex = barIndex;
        }
    }

    /**
     * Load full historical candles for a ticker/interval. Prices are only accessible at or before
     * {@link #currentTime}.
     */
    public void loadCandles(String ticker, String interval, List<Candle> candles) {
        candlesByTickerAndInterval
                .computeIfAbsent(ticker, k -> new HashMap<>())
                .put(interval, new ArrayList<>(candles));
        // 4: cache parsed times for O(log N) lookup
        List<LocalDateTime> times = new ArrayList<>(candles.size());
        for (Candle c : candles) {
            times.add(LocalDateTime.parse(c.time, DATE_TIME_FMT));
        }
        timesByTickerAndInterval
                .computeIfAbsent(ticker, k -> new HashMap<>())
                .put(interval, times);
        positions.computeIfAbsent(ticker, SimulatedPosition::new);
    }

    /**
     * Advance simulated clock. All subsequent price reads resolve to the bar at/just before this
     * time (no look-ahead).
     */
    public void setCurrentTime(LocalDateTime time) {
        this.currentTime = time;
    }

    public LocalDateTime getCurrentTime() {
        return currentTime;
    }

    public SimulatedPosition getPositionState(String ticker) {
        return positions.computeIfAbsent(ticker, SimulatedPosition::new);
    }

    public double getSharedCash() {
        return sharedCash;
    }

    public int getConcurrentOpenPeak() {
        return concurrentOpenPeak;
    }

    /**
     * Add external funds (e.g. monthly rebalance deposit).
     */
    public void deposit(double amount) {
        if (amount != 0.0) {
            sharedCash += amount;
            recordUnrecordedCash(amount);  // deposit (external funds)
            noteMutation();
        }
    }

    /**
     * Total portfolio value = cash + all open positions marked at current bar price.
     * Also updates portfolio peak and checks Kill Switch threshold.
     */
    public double getTotalPortfolioValue() {
        double total = sharedCash;
        for (SimulatedPosition pos : positions.values()) {
            if (pos.hasOpenPosition()) {
                Candle bar = getCurrentCandle(pos.ticker, "5_MIN");
                if (bar != null) {
                    total += pos.getMarketValue(bar.close);
                } else {
                    total += pos.getMarketValue(pos.entryPrice);
                }
            }
        }
        // Update peak and check Kill Switch
        if (total > portfolioPeak) {
            portfolioPeak = total;
        }
        checkKillSwitch(total);
        return total;
    }

    /**
     * Check if portfolio drawdown exceeds threshold and trigger Kill Switch.
     */
    private void checkKillSwitch(double currentPortfolioValue) {
        if (killSwitchTriggered) {
            return; // already triggered
        }
        if (isResettingKillSwitch) {
            return; // prevent recursive check during reset
        }
        if (portfolioPeak <= 0) {
            return; // avoid division by zero
        }
        double drawdown = (portfolioPeak - currentPortfolioValue) / portfolioPeak;
        if (drawdown > maxDrawdownThreshold) {
            killSwitchTriggered = true;
            System.out.println(
                "KILL SWITCH TRIGGERED: Portfolio drawdown "
                + String.format("%.2f%%", drawdown * 100)
                + " exceeds threshold "
                + String.format("%.2f%%", maxDrawdownThreshold * 100)
                + " (peak=" + String.format("%.0f", portfolioPeak)
                + ", current=" + String.format("%.0f", currentPortfolioValue) + ")");
            closeAll("kill_switch");
            // Auto-reset Kill Switch after all positions are closed
            isResettingKillSwitch = true;
            killSwitchTriggered = false;
            portfolioPeak = getTotalPortfolioValue(); // reset peak to current value
            isResettingKillSwitch = false;
            System.out.println(
                "Kill Switch AUTO-RESET: trading resumed (peak reset to " 
                + String.format("%.0f", portfolioPeak) + ")");
        }
    }

    /**
     * Count open positions across all tickers, excluding TMON@ (it does not consume a slot).
     */
    public int getOpenPositionCount() {
        int count = 0;
        for (SimulatedPosition pos : positions.values()) {
            if (isTmonParking(pos.ticker)) {
                continue;
            }
            if (pos.hasOpenPosition()) {
                count++;
            }
        }
        return count;
    }

    public double getTmonPositionValue(String ticker) {
        String parkingTicker = getParkingTicker(ticker);
        SimulatedPosition tmonPos = positions.get(parkingTicker);
        if (tmonPos == null || !tmonPos.hasOpenPosition()) {
            return 0.0;
        }
        Candle bar = getCurrentCandle(parkingTicker, "5_MIN");
        double price = bar != null ? bar.close : tmonPos.entryPrice;
        return tmonPos.getMarketValue(price);
    }

    /**
     * Effective commission rate.
     * TMON@ (Tinkoff ETF parking) is free.
     * Regular instruments use the configured commission rate.
     */
    public double getEffectiveCommission(String ticker) {
        if ("TMON@".equals(ticker)) {
            return 0.0;
        }
        return commissionRate;
    }

    // ========================================================================
    // MarketDataProvider Implementation
    // ========================================================================

    /** Return candles only up to (and including) the current simulation time. O(log N) via binary search. */
    @Override
    public List<Candle> getCandles(String ticker, String interval) {
        List<Candle> all = candlesByTickerAndInterval
                .getOrDefault(ticker, Collections.emptyMap())
                .getOrDefault(interval, Collections.emptyList());
        if (all.isEmpty() || currentTime == null) {
            return all;
        }
        // 4: binary search on cached times (upper bound)
        List<LocalDateTime> times = timesByTickerAndInterval
                .getOrDefault(ticker, Collections.emptyMap())
                .getOrDefault(interval, Collections.emptyList());
        int idx = upperBound(times, currentTime);
        if (idx < 0) {
            return Collections.emptyList();
        }
        return all.subList(0, idx + 1);
    }

    /** Resolve the current bar (at or before currentTime) for a ticker/interval, or null. */
    public Candle getCurrentCandle(String ticker, String interval) {
        List<Candle> visible = getCandles(ticker, interval);
        return visible.isEmpty() ? null : visible.get(visible.size() - 1);
    }

    @Override
    public MarketPrices getLivePrices(String ticker) {
        Candle bar = getCurrentCandle(ticker, "5_MIN");
        if (bar == null) {
            bar = getCurrentCandle(ticker, "HOUR");
        }
        if (bar == null) {
            return new MarketPrices(null, null);
        }
        double spread = bar.close * 0.0002;
        return new MarketPrices(bar.close - spread, bar.close + spread);
    }

    @Override
    public PositionInfo getCurrentPositions(TickerType tickerType, String tickerName) {
        SimulatedPosition pos = positions.get(tickerName);
        if (pos == null || !pos.hasOpenPosition()) {
            return null;
        }
        return new PositionInfo(
                tickerName,        // figi (not used in backtest)
                tickerName,
                "",                // isin
                tickerType.name(),
                pos.position.quantity,
                0.0,
                pos.position.quantity,  // lots (lotSize handled separately)
                pos.entryPrice,
                tickerName);
    }

    // ========================================================================
    // OrderExecutor Implementation
    // ========================================================================

    @Override
    public ExecutionResult buy(String ticker, int quantity, Double stopLossPrice, Double takeProfitPrice) {
        // Kill Switch: block new OPENs if triggered
        if (killSwitchTriggered) {
            return ExecutionResult.failed("Kill Switch triggered — trading halted");
        }
        SimulatedPosition pos = getPositionState(ticker);
        if (pos.hasOpenPosition()) {
            return ExecutionResult.failed("Position already exists for " + ticker);
        }
        // E1: MAX_CONCURRENT checked here (single point) — TMON@ excluded from limit
        if (!isTmonParking(ticker) && getOpenPositionCount() >= maxConcurrentPositions) {
            return ExecutionResult.failed("Max concurrent positions (" + maxConcurrentPositions + ") reached");
        }

        Candle bar = getCurrentCandle(ticker, "5_MIN");
        if (bar == null) {
            return ExecutionResult.failed("No market data for " + ticker);
        }
        double rawPrice = bar.close;
        TickerInfo info = TickerRepository.INSTANCE.getByName(ticker);
        int lotSize = info != null && info.getLot() != null ? Math.max(1, info.getLot()) : 1;

        double slippedEntry = rawPrice * (1.0 + slippage);
        double entryNotional = notional(quantity, lotSize, slippedEntry) * ORDER_QUANTITY_SAFETY_MARGIN;
        double commission = entryNotional * getEffectiveCommission(ticker);
        if (entryNotional + commission > sharedCash) {
            return ExecutionResult.failed(
                    "Insufficient cash: needed " + (entryNotional + commission)
                            + ", available " + sharedCash);
        }

        // Adjust back to actual execution (without safety margin) for accurate PnL
        entryNotional = entryNotional / ORDER_QUANTITY_SAFETY_MARGIN;
        double openDeduction = entryNotional + commission;
        sharedCash -= openDeduction;
        if (!isTmonParking(ticker)) {
            totalOpenDeduction += openDeduction;
            openCount++;
            openDeductionByTicker.merge(ticker, openDeduction, Double::sum);
        }
        noteMutation();
        pos.lotSize = lotSize;

        // Use SL/TP prices directly (parity with live TCS stop-orders)
        // If not provided, use defaults (2% SL / 4% TP)
        double sl = stopLossPrice != null ? stopLossPrice : rawPrice * (1.0 - defaultSlPercent / 100.0);
        double tp = takeProfitPrice != null ? takeProfitPrice : rawPrice * (1.0 + defaultTpPercent / 100.0);

        pos.position = new Position(
                "BUY", slippedEntry, sl, tp, null, quantity, 0, 0, 1, false);
        pos.entryPrice = slippedEntry;
        pos.postedMargin = 0.0;  // longs don't post margin
        pos.entryBarIndex = barIndex(ticker, bar);
        trackConcurrentPeak();
        // 5.1: OPEN record with action="OPEN", entryPrice != 0
        // 5.4: TMON@ cash parking excluded from tradeHistory (not counted in Opens/Closes)
        if (!isTmonParking(ticker)) {
            tradeHistory.add(new BacktestTrade(
                    ticker, "BUY", "OPEN", slippedEntry, 0.0, quantity,
                    0.0, commission, "open", bar.time, pos.entryBarIndex));
        }
        return ExecutionResult.success(quantity, slippedEntry);
    }

    @Override
    public ExecutionResult sell(String ticker, int quantity, Double stopLossPrice, Double takeProfitPrice) {
        // Kill Switch: block new OPENs if triggered
        if (killSwitchTriggered) {
            return ExecutionResult.failed("Kill Switch triggered — trading halted");
        }
        SimulatedPosition pos = getPositionState(ticker);
        if (pos.hasOpenPosition()) {
            return ExecutionResult.failed("Position already exists for " + ticker);
        }
        // E1: MAX_CONCURRENT checked here (single point) — TMON@ excluded from limit
        if (!isTmonParking(ticker) && getOpenPositionCount() >= maxConcurrentPositions) {
            return ExecutionResult.failed("Max concurrent positions (" + maxConcurrentPositions + ") reached");
        }

        Candle bar = getCurrentCandle(ticker, "5_MIN");
        if (bar == null) {
            return ExecutionResult.failed("No market data for " + ticker);
        }
        double rawPrice = bar.close;
        TickerInfo info = TickerRepository.INSTANCE.getByName(ticker);
        int lotSize = info != null && info.getLot() != null ? Math.max(1, info.getLot()) : 1;

        double slippedEntry = rawPrice * (1.0 - slippage);
        double entryNotional = notional(quantity, lotSize, slippedEntry) * ORDER_QUANTITY_SAFETY_MARGIN;
        double marginRequired = entryNotional * shortMarginRatio;
        double commission = entryNotional * getEffectiveCommission(ticker);
        if (marginRequired + commission > sharedCash) {
            return ExecutionResult.failed(
                    "Insufficient margin: needed " + (marginRequired + commission)
                            + ", available " + sharedCash);
        }

        // Adjust back to actual execution (without safety margin) for accurate PnL
        entryNotional = entryNotional / ORDER_QUANTITY_SAFETY_MARGIN;
        marginRequired = entryNotional * shortMarginRatio;

        // Post margin at open, return it at close
        double openDeduction = marginRequired + commission;
        sharedCash -= openDeduction;
        totalOpenDeduction += openDeduction;
        openCount++;
        noteMutation();
        pos.lotSize = lotSize;

        // Use SL/TP prices directly (parity with live TCS stop-orders)
        // For short: SL above entry, TP below entry
        // If not provided, use defaults (2% SL / 4% TP)
        double sl = stopLossPrice != null ? stopLossPrice : rawPrice * (1.0 + defaultSlPercent / 100.0);
        double tp = takeProfitPrice != null ? takeProfitPrice : rawPrice * (1.0 - defaultTpPercent / 100.0);

        pos.position = new Position(
                "SELL", slippedEntry, sl, tp, null, quantity, 0, 0, 1, false);
        pos.entryPrice = slippedEntry;
        pos.postedMargin = marginRequired;
        pos.entryBarIndex = barIndex(ticker, bar);
        trackConcurrentPeak();
        // 5.1: OPEN record with action="OPEN", entryPrice != 0
        // 5.4: TMON@ cash parking excluded from tradeHistory (not counted in Opens/Closes)
        if (!isTmonParking(ticker)) {
            tradeHistory.add(new BacktestTrade(
                    ticker, "SELL", "OPEN", slippedEntry, 0.0, quantity,
                    0.0, commission, "open", bar.time, pos.entryBarIndex));
        }
        return ExecutionResult.success(quantity, slippedEntry);
    }

    @Override
    public ExecutionResult closeLong(String ticker) {
        SimulatedPosition pos = positions.get(ticker);
        if (pos == null || !pos.isLong()) {
            return ExecutionResult.failed("No long position for " + ticker);
        }
        Candle bar = getCurrentCandle(ticker, "5_MIN");
        if (bar == null) {
            return ExecutionResult.failed("No market data for " + ticker);
        }
        return closePosition(ticker, pos, bar.close, bar.time, "strategy_close");
    }

    @Override
    public ExecutionResult closeShort(String ticker) {
        SimulatedPosition pos = positions.get(ticker);
        if (pos == null || !pos.isShort()) {
            return ExecutionResult.failed("No short position for " + ticker);
        }
        Candle bar = getCurrentCandle(ticker, "5_MIN");
        if (bar == null) {
            return ExecutionResult.failed("No market data for " + ticker);
        }
        return closePosition(ticker, pos, bar.close, bar.time, "strategy_close");
    }

    /**
     * Close position with a custom reason (for strategy-decided closes).
     */
    public ExecutionResult closePositionByStrategy(String ticker, String reason) {
        SimulatedPosition pos = positions.get(ticker);
        if (pos == null || !pos.hasOpenPosition()) {
            return ExecutionResult.failed("No position for " + ticker);
        }
        Candle bar = getCurrentCandle(ticker, "5_MIN");
        if (bar == null) {
            return ExecutionResult.failed("No market data for " + ticker);
        }
        return closePosition(ticker, pos, bar.close, bar.time, reason);
    }

    @Override
    public ExecutionResult partialCloseLong(String ticker, int quantity) {
        SimulatedPosition pos = positions.get(ticker);
        if (pos == null || !pos.isLong()) {
            return ExecutionResult.failed("No long position for " + ticker);
        }
        if (quantity <= 0 || quantity >= pos.position.quantity) {
            return ExecutionResult.failed("Invalid partial close quantity: " + quantity);
        }
        Candle bar = getCurrentCandle(ticker, "5_MIN");
        if (bar == null) {
            return ExecutionResult.failed("No market data for " + ticker);
        }
        
        int remainingQty = pos.position.quantity - quantity;
        double exitPrice = bar.close;
        double commission = exitPrice * quantity * pos.lotSize * getEffectiveCommission(ticker);
        double proceeds = exitPrice * quantity * pos.lotSize - commission;
        double entryValue = pos.entryPrice * quantity * pos.lotSize;
        // Allocate entry commission proportionally (parity with partialCloseAtFirstTp)
        double entryCommissionTotal = notional(pos.position.quantity, pos.lotSize, pos.entryPrice) * getEffectiveCommission(ticker);
        double allocatedEntryCommission = entryCommissionTotal * ((double) quantity / pos.position.quantity);
        double pnl = proceeds - entryValue - allocatedEntryCommission;
        
        sharedCash += proceeds;
        totalCloseCashDelta += proceeds;
        partialCloseCount++;
        noteMutation();
        pos.position = new Position(
                pos.position.direction,
                pos.entryPrice,
                pos.position.stopLoss,
                pos.position.takeProfit,
                null,
                remainingQty,
                pos.position.candlesHeld,
                0,
                pos.position.appliedLeverage,
                true);
        
        tradeHistory.add(new BacktestTrade(
                ticker, "SELL", "PARTIAL_CLOSE", pos.entryPrice, exitPrice, quantity,
                pnl, commission, "partial_close", bar.time, barIndex(ticker, bar)));
        recordTradePnl(pnl);
        
        return ExecutionResult.success(quantity, exitPrice);
    }

    @Override
    public ExecutionResult partialCloseShort(String ticker, int quantity) {
        SimulatedPosition pos = positions.get(ticker);
        if (pos == null || !pos.isShort()) {
            return ExecutionResult.failed("No short position for " + ticker);
        }
        if (quantity <= 0 || quantity >= Math.abs(pos.position.quantity)) {
            return ExecutionResult.failed("Invalid partial close quantity: " + quantity);
        }
        Candle bar = getCurrentCandle(ticker, "5_MIN");
        if (bar == null) {
            return ExecutionResult.failed("No market data for " + ticker);
        }
        
        int remainingQty = Math.abs(pos.position.quantity) - quantity;
        double exitPrice = bar.close;
        double commission = exitPrice * quantity * pos.lotSize * getEffectiveCommission(ticker);
        double entryValue = pos.entryPrice * quantity * pos.lotSize;
        // Allocate entry commission proportionally (parity with partialCloseAtFirstTp)
        double entryCommissionTotal = notional(Math.abs(pos.position.quantity), pos.lotSize, pos.entryPrice) * getEffectiveCommission(ticker);
        double allocatedEntryCommission = entryCommissionTotal * ((double) quantity / Math.abs(pos.position.quantity));
        double grossPnlPartial = entryValue - notional(quantity, pos.lotSize, exitPrice);
        double pnl = grossPnlPartial - allocatedEntryCommission - commission;
        
        // Short partial close: return freed margin + realized gross PnL for the closed
        // portion, and scale the remaining margin down (parity with partialCloseAtFirstTp).
        // Exit commission is paid on the buy-to-cover; entry commission was paid at open.
        double freedMargin = pos.postedMargin * ((double) quantity / Math.abs(pos.position.quantity));
        double cashAdded = freedMargin + grossPnlPartial - commission;
        sharedCash += cashAdded;
        pos.postedMargin -= freedMargin;
        totalCloseCashDelta += cashAdded;
        partialCloseCount++;
        noteMutation();
        pos.position = new Position(
                pos.position.direction,
                pos.entryPrice,
                pos.position.stopLoss,
                pos.position.takeProfit,
                null,
                remainingQty,
                pos.position.candlesHeld,
                0,
                pos.position.appliedLeverage,
                true);
        
        tradeHistory.add(new BacktestTrade(
                ticker, "BUY", "PARTIAL_CLOSE", pos.entryPrice, exitPrice, quantity,
                pnl, commission, "partial_close", bar.time, barIndex(ticker, bar)));
        recordTradePnl(pnl);
        
        return ExecutionResult.success(quantity, exitPrice);
    }

    /**
     * Update only protective levels (trailing SL/TP). entry/quantity/direction are NOT changed.
     * TMON@ (cash parking) is skipped — no SL/TP.
     */
    public void updateProtectiveLevels(String ticker, Double stopLoss, Double takeProfit) {
        SimulatedPosition p = positions.get(ticker);
        if (p == null || !p.hasOpenPosition() || p.position == null) {
            return;
        }
        if (isTmonParking(ticker)) {
            return;  // cash parking без SL/TP
        }
        // Rebuild position keeping actual execution fields, only SL/TP change
        p.position = new Position(
                p.position.direction,
                p.position.entryPrice,
                stopLoss,
                takeProfit,
                null,
                p.position.quantity,
                p.position.candlesHeld,
                0,
                p.position.appliedLeverage,
                p.position.partialClosed);
    }

    /**
     * Fraction of the position closed at the first take-profit level (TP1). The remaining
     * fraction is left open and managed by the second take-profit level (TP2) / trailing stop.
     */
    private static final double TP1_CLOSE_FRACTION = 0.6;

    /**
     * Check SL/TP on the given bar and close (or partially close) the position if triggered.
     *
     * <p><b>Parity with live TCS:</b> In live trading, protective orders are posted as
     * {@code EXCHANGE_ORDER_TYPE_MARKET} stop-orders. When the trigger price is touched,
     * the order converts to a market order and executes at the CURRENT MARKET PRICE,
     * NOT exactly at the stop level. This method replicates that behavior by using
     * {@code currentBar.close} as the fill price (market price at trigger moment).</p>
     *
     * <p><b>Two-level take-profit lifecycle:</b> A fresh position carries {@code takeProfit}
     * (TP1) and {@code takeProfit2} (TP2). When TP1 is touched the first time, {@link
     * #TP1_CLOSE_FRACTION} (60%) of the position is closed and the remainder is rebuilt with a
     * breakeven stop and {@code takeProfit2} as its new take-profit ({@code partialClosed=true}).
     * On subsequent bars the remainder closes fully when TP2 (now {@code takeProfit}) or the stop
     * is touched.</p>
     *
     * <p><b>Invariant (Block 9.1):</b> Does NOT check cooldownRemaining — SL/TP triggers even
     * immediately after entry. This ensures positions are properly protected from the first bar.</p>
     *
     * <p><b>Risk Engineer priority:</b> When both stop-loss and take-profit on the same bar
     * (unknowable intrabar sequence), execution occurs at the stop-loss (pessimistic choice).
     * TP1 is only partially closed; the second level (TP2) is checked on later bars to avoid an
     * optimistic intrabar double fill.</p>
     *
     * @return execution result if the position was closed/partially closed, or null if none triggered
     */
    public ExecutionResult checkStopLossTakeProfit(String ticker, Candle currentBar) {
        SimulatedPosition pos = positions.get(ticker);
        if (pos == null || !pos.hasOpenPosition()) {
            return null;
        }
        if (isTmonParking(ticker)) {
            return null;  // cash parking — no SL/TP
        }
        boolean isLong = pos.isLong();
        Double sl = pos.position.stopLoss;
        Double tp = pos.position.takeProfit;
        Double tp2 = pos.position.takeProfit2;

        boolean slHit = sl != null && (isLong ? currentBar.low <= sl : currentBar.high >= sl);
        boolean tpHit = tp != null && (isLong ? currentBar.high >= tp : currentBar.low <= tp);

        // Pessimistic: stop-loss takes precedence over take-profit on the same bar.
        if (slHit) {
            return closePosition(ticker, pos, currentBar.close, currentBar.time, "sl_hit");
        }
        if (!tpHit) {
            return null;
        }

        // Position already partially closed: takeProfit now holds the original TP2.
        if (pos.position.partialClosed) {
            return closePosition(ticker, pos, currentBar.close, currentBar.time, "tp2_hit");
        }

        // Fresh position: TP2 hit before TP1 partial-close occurred — close the whole remainder
        // at TP2 in one shot (single-level TP behavior, no TP2 separation configured).
        boolean tpHit2 = tp2 != null && (isLong ? currentBar.high >= tp2 : currentBar.low <= tp2);
        if (tp2 != null && tpHit2) {
            return closePosition(ticker, pos, currentBar.close, currentBar.time, "tp2_hit_full");
        }

        // TP1 hit: partial close, rebuild the remainder with breakeven stop and TP2.
        return partialCloseAtFirstTp(ticker, pos, currentBar, isLong);
    }

    /**
     * Partial close of {@link #TP1_CLOSE_FRACTION} of the position when the first take-profit
     * level (TP1) is touched. The remainder keeps the same entry price but transitions to a
     * breakeven stop and the original second take-profit ({@code takeProfit2}) as its new
     * take-profit target ({@code partialClosed=true}).
     */
    private ExecutionResult partialCloseAtFirstTp(
            String ticker, SimulatedPosition pos, Candle bar, boolean isLong) {
        int totalQty = pos.position.quantity;
        int closeQty = Math.max(1, (int) Math.round(totalQty * TP1_CLOSE_FRACTION));
        int remainingQty = totalQty - closeQty;
        if (remainingQty <= 0) {
            return closePosition(ticker, pos, bar.close, bar.time, "tp1_full");
        }

        double exitPrice = bar.close;
        double exitCommission =
                exitPrice * closeQty * pos.lotSize * getEffectiveCommission(ticker);
        // Allocate entry commission proportionally to partial close (parity with closePosition pnl accounting)
        double entryPrice = pos.entryPrice;
        double entryCommissionTotal = notional(totalQty, pos.lotSize, entryPrice) * getEffectiveCommission(ticker);
        double allocatedEntryCommission = entryCommissionTotal * ((double) closeQty / totalQty);
        
        double proceeds = notional(closeQty, pos.lotSize, exitPrice) - exitCommission;
        double entryValue = notional(closeQty, pos.lotSize, entryPrice);
        // For longs: proceeds includes exit commission deduction. For shorts: entryValue - proceeds
        // would add exit commission (wrong), so we compute grossPnl directly and subtract commissions.
        double grossPnlPartial = isLong ? (notional(closeQty, pos.lotSize, exitPrice) - entryValue)
                                        : (entryValue - notional(closeQty, pos.lotSize, exitPrice));
        double pnl = grossPnlPartial - allocatedEntryCommission - exitCommission;
        String direction = pos.position.direction;

        if (isLong) {
            sharedCash += proceeds;
        } else {
            // Short: open posts only margin (30% of notional). A short partial close must
            // return freed margin + realized gross PnL for the closed portion, and scale the
            // remaining position's margin down to its remaining quantity. Exit commission
            // is paid on the buy-to-cover; entry commission was already paid at open.
            double freedMargin = pos.postedMargin * ((double) closeQty / totalQty);
            sharedCash += freedMargin + grossPnlPartial - exitCommission;
            pos.postedMargin -= freedMargin;
        }
        totalCloseCashDelta += proceeds;
        partialCloseCount++;
        noteMutation();
        // Rebuild the remainder: breakeven stop, TP2 as new take-profit, partialClosed=true.
        pos.position = new Position(pos.position, remainingQty, pos.entryPrice);

        tradeHistory.add(new BacktestTrade(
                ticker, direction, "PARTIAL_CLOSE",
                entryPrice, exitPrice, closeQty,
                pnl, exitCommission + allocatedEntryCommission, "tp1_partial", bar.time, barIndex(ticker, bar)));
        recordTradePnl(pnl);
        return ExecutionResult.success(closeQty, exitPrice);
    }

    /**
     * Close an open position at an explicit price applying slippage internally.
     */
    public ExecutionResult closeAtPrice(String ticker, double exitPrice, String time, String reason) {
        SimulatedPosition pos = positions.get(ticker);
        if (pos == null || !pos.hasOpenPosition()) {
            return ExecutionResult.failed("No position for " + ticker);
        }
        return closePosition(ticker, pos, exitPrice, time, reason);
    }

    /**
     * Close all open positions at current bar price with a given reason (EOD / period end).
     * Skips TMON@ (cash parking, not a trade).
     */
    public void closeAll(String reason) {
        for (SimulatedPosition pos : positions.values()) {
            if (isTmonParking(pos.ticker)) {
                continue;  // TMON@ is cash parking, not closed at period_end
            }
            if (!pos.hasOpenPosition()) {
                continue;
            }
            Candle bar = getCurrentCandle(pos.ticker, "5_MIN");
            double price = bar != null ? bar.close : pos.entryPrice;
            String time = bar != null ? bar.time : "";
            closePosition(pos.ticker, pos, price, time, reason);
        }
    }

    @Override
    public boolean sellByMarket(String name, TickerType type, double cashToSell) {
        // sellByMarket — путь частичной распарковки для TMON@ и SPYUSDT.
        // TMON@: комиссия 0, PnL идёт в tmonRealizedPnl
        // SPYUSDT: комиссия по тарифу, PnL как обычная позиция
        if (!"TMON@".equals(name) && !"SPYUSDT".equals(name)) {
            return false;
        }
        SimulatedPosition pos = positions.get(name);
        if (pos == null || !pos.hasOpenPosition()) {
            return false;
        }
        Candle bar = getCurrentCandle(name, "5_MIN");
        if (bar == null) {
            return false;
        }
        double price = bar.close;
        int sharesToSell = (int) Math.floor(cashToSell / price);
        if (sharesToSell <= 0 || sharesToSell > pos.position.quantity) {
            return false;
        }
        double proceeds = notional(sharesToSell, pos.lotSize, price);
        double commission = proceeds * getEffectiveCommission(name);
        double netProceeds = proceeds - commission;
        double entryValue = notional(sharesToSell, pos.lotSize, pos.entryPrice);
        
        if ("TMON@".equals(name)) {
            // TMON@: PnL идёт в tmonRealizedPnl (commission = 0)
            tmonRealizedPnl += (netProceeds - entryValue);
        }
        // SPYUSDT: комиссия уже вычтена из netProceeds, PnL не отслеживается отдельно
        
        sharedCash += netProceeds;
        noteMutation();
        pos.position = new Position(
            pos.position.direction,
            pos.entryPrice,
            pos.position.stopLoss,
            pos.position.takeProfit,
            null,
            pos.position.quantity - sharesToSell,
            pos.position.candlesHeld,
            0,
            pos.position.appliedLeverage,
            pos.position.partialClosed);
        return true;
    }

    @Override
    public boolean closeLongByMarket(String ticker, TickerType type) {
        SimulatedPosition pos = positions.get(ticker);
        if (pos == null || !pos.isLong()) {
            return false;
        }
        Candle bar = getCurrentCandle(ticker, "5_MIN");
        if (bar == null) {
            return false;
        }
        // 1.4 (fix): НЕ считаем parking-PnL здесь. closePosition() — единственная точка
        // учёта при полном закрытии (устранён двойной учёт TMON@).
        closePosition(ticker, pos, bar.close, bar.time, "cash_parking");
        return true;
    }

    public void closeTmonParking(String ticker, String reason) {
        // TMON@ only — SPYUSDT is closed via closeAll() as a normal position with commission.
        if (!"TMON@".equals(ticker)) {
            return;
        }
        SimulatedPosition p = positions.get(ticker);
        if (p == null || !p.hasOpenPosition()) {
            return;
        }
        Candle bar = getCurrentCandle(ticker, "5_MIN");
        double price = bar != null ? bar.close : p.entryPrice;
        String time = bar != null ? bar.time : "";
        closePosition(ticker, p, price, time, reason);
    }

    /**
     * Internal close logic with symmetric PnL/margin accounting.
     *
     * <p><b>Single point of TMON@ parking-PnL accounting:</b> для TMON@ realized-PnL
     * при полном закрытии накапливается здесь и только здесь (частичная продажа —
     * в sellByMarket). tradeHistory для TMON@ не пишется (Opens==Closes сохраняется).</p>
     */
    private ExecutionResult closePosition(String ticker, SimulatedPosition pos, double exitPrice,
                                          String time, String reason) {
        int quantity = pos.position.quantity;
        double entryPrice = pos.entryPrice;
        int lotSize = pos.lotSize;
        boolean isShort = pos.isShort();

        double entryNotional = notional(quantity, lotSize, entryPrice);
        double exitNotional = notional(quantity, lotSize, exitPrice);

        double eff = getEffectiveCommission(ticker);
        // Дефект A (fix): комиссия открытия была списана из кэша в buy/sell,
        // но не отражалась в trade.pnl. Реконструируем её из entryNotional и
        // вычитаем из pnl, чтобы trade.pnl == Δcash за полный цикл сделки.
        double entryCommission = entryNotional * eff;
        double exitCommission = exitNotional * eff;

        double grossPnl;
        if (isShort) {
            grossPnl = entryNotional - exitNotional;
        } else {
            grossPnl = exitNotional - entryNotional;
        }

        // pnl теперь net от ОБЕИХ комиссий — согласовано с движением кэша
        double pnl = grossPnl - entryCommission - exitCommission;

        if (isShort) {
            double cashDelta = pos.postedMargin + grossPnl - exitCommission;
            sharedCash += cashDelta;
            if (isTmonParking(ticker)) tmonCashFlow += cashDelta;
            totalCloseCashDelta += cashDelta;
            if (!isTmonParking(ticker)) {
                totalCloseCashMinusPnl += (cashDelta - pnl);
                String k = reason == null ? "NULL" : reason;
                closeCashMinusPnlByReason.merge(k, cashDelta - pnl, Double::sum);
                closeCountByReason.merge(k, 1, Integer::sum);
                closeCashByTicker.merge(ticker, cashDelta, Double::sum);
                recordedPnlByTicker.merge(ticker, pnl, Double::sum);
            }
            closeCount++;
            noteMutation();
        } else {
            double cashDelta = entryNotional + grossPnl - exitCommission;
            sharedCash += cashDelta;
            if (isTmonParking(ticker)) tmonCashFlow += cashDelta;
            totalCloseCashDelta += cashDelta;
            if (!isTmonParking(ticker)) {
                totalCloseCashMinusPnl += (cashDelta - pnl);
                String k = reason == null ? "NULL" : reason;
                closeCashMinusPnlByReason.merge(k, cashDelta - pnl, Double::sum);
                closeCountByReason.merge(k, 1, Integer::sum);
                closeCashByTicker.merge(ticker, cashDelta, Double::sum);
                recordedPnlByTicker.merge(ticker, pnl, Double::sum);
            }
            closeCount++;
            noteMutation();
        }

        if (isTmonParking(ticker)) {
            // TMON@: комиссия = 0, поэтому entryCommission == exitCommission == 0,
            // pnl == grossPnl == (marketValue - entryValue). Инвариант сохранён.
            tmonRealizedPnl += pnl;
        } else {
            tradeHistory.add(new BacktestTrade(
                ticker, pos.position.direction, "CLOSE",
                entryPrice, exitPrice, quantity,
                pnl, exitCommission + entryCommission, reason, time, pos.entryBarIndex));
            recordTradePnl(pnl);
        }

        pos.position = new Position();
        pos.entryPrice = 0.0;
        pos.postedMargin = 0.0;
        
        // Kill Switch check after position close (matches live onTradeClosed behavior)
        checkKillSwitch(getTotalPortfolioValue());
        
        return ExecutionResult.success(quantity, exitPrice);
    }

    @Override
    public double getAvailableCash() {
        return sharedCash;
    }

    @Override
    public double getInitialBalance() {
        return initialBalance;
    }

    /** Diagnostic: totals of recorded trade pnl vs raw cash movements (to find reconciliation drift). */
    public void recordTradePnl(double pnl) {
        recordedTradePnl += pnl;
    }

    public void recordUnrecordedCash(double delta) {
        unrecordedCashDelta += delta;
    }

    private void noteMutation() {
        allCashMutations += (sharedCash - lastSharedCash);
        lastSharedCash = sharedCash;
    }

    public void printReconciliationDrift() {
        double cashDelta = sharedCash - initialBalance;
        System.out.println("DBG-drift: sharedCashDelta=" + cashDelta
                + " allCashMutations=" + allCashMutations
                + " tmonCashFlow=" + tmonCashFlow
                + " totalOpenDeduction=" + totalOpenDeduction
                + " totalCloseCashDelta=" + totalCloseCashDelta
                + " totalCloseCashMinusPnl=" + totalCloseCashMinusPnl
                + " openCount=" + openCount
                + " closeCount=" + closeCount
                + " partialCloseCount=" + partialCloseCount
                + " recordedTradePnl=" + recordedTradePnl
                + " tmonRealizedPnl=" + tmonRealizedPnl
                + " unrecordedCashDelta=" + unrecordedCashDelta
                + " explained=" + (recordedTradePnl + tmonRealizedPnl + unrecordedCashDelta)
                + " unexplained=" + (cashDelta - recordedTradePnl - tmonRealizedPnl - unrecordedCashDelta));
        System.out.println("DBG-check: sharedCash=" + sharedCash + " initialBalance=" + initialBalance
                + " + recordedTradePnl=" + recordedTradePnl + " diff=" + (sharedCash - initialBalance - recordedTradePnl));
        System.out.println("DBG-reason size=" + closeCashMinusPnlByReason.size() + " closeCashByTicker=" + closeCashByTicker.size() + " recordedPnlByTicker=" + recordedPnlByTicker.size());
        closeCashMinusPnlByReason.forEach((reason, val) ->
            System.out.println("  " + reason + ": cashMinusPnl=" + val + " count=" + closeCountByReason.get(reason) + " avg=" + (val/closeCountByReason.get(reason))));
        // Top ticker discrepancies
        var tickerDisc = recordedPnlByTicker.keySet().stream()
            .map(t -> Map.entry(t,
                closeCashByTicker.getOrDefault(t, 0.0) - openDeductionByTicker.getOrDefault(t, 0.0)
                    - recordedPnlByTicker.getOrDefault(t, 0.0)))
            .sorted(Map.Entry.<String, Double>comparingByValue().reversed())
            .limit(10)
            .toList();
        System.out.println("DBG-ticker top discrepancies (cash - open - pnl):");
        tickerDisc.forEach(e -> System.out.println("  " + e.getKey() + ": " + e.getValue()));
    }

    public List<BacktestTrade> getTradeHistory() {
        return new ArrayList<>(tradeHistory);
    }

    public void tickCooldown() {
        for (SimulatedPosition pos : positions.values()) {
            if (!pos.hasOpenPosition() && pos.cooldownRemaining > 0) {
                pos.cooldownRemaining--;
            }
        }
    }

    // ------------------------------------------------------------------------
    // Helpers
    // ------------------------------------------------------------------------

    private double notional(int quantity, int lotSize, double price) {
        return (double) quantity * lotSize * price;
    }

    /** 4: binary search for rightmost element <= target. Returns -1 if none. */
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

    private long barIndex(String ticker, Candle bar) {
        List<LocalDateTime> times = timesByTickerAndInterval
                .getOrDefault(ticker, Collections.emptyMap())
                .getOrDefault("5_MIN", Collections.emptyList());
        // bar == текущий бар при OPEN, значит его время == currentTime (без повторного парсинга)
        LocalDateTime target = currentTime != null
                ? currentTime
                : LocalDateTime.parse(bar.time, DATE_TIME_FMT); // fallback на случай прямого вызова
        int idx = upperBound(times, target);
        return Math.max(0, idx);
    }

    private void trackConcurrentPeak() {
        int count = getOpenPositionCount();
        if (count > concurrentOpenPeak) {
            concurrentOpenPeak = count;
        }
    }
}
