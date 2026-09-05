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

    /** Default SL % below/above entry for long/short when strategy provides none. */
    private static final double DEFAULT_SL_PERCENT = 2.0;

    /** Default TP % above/below entry for long/short when strategy provides none. */
    private static final double DEFAULT_TP_PERCENT = 4.0;

    /** Margin ratio posted for short positions (mirrors live 30%). */
    private static final double SHORT_MARGIN_RATIO = 0.30;

    private static final int MAX_CONCURRENT_POSITIONS = 8;

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

    public double getTmonRealizedPnl() {
        return tmonRealizedPnl;
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

    public SimulatedBroker(double initialBalance, double commissionRate, double slippage) {
        this.initialBalance = initialBalance;
        this.sharedCash = initialBalance;
        this.commissionRate = commissionRate;
        this.slippage = slippage;
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
        }
    }

    /**
     * Total portfolio value = cash + all open positions marked at current bar price.
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
        return total;
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
     * SPYUSDT (ByBit crypto parking) incurs normal commission.
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
    public ExecutionResult buy(String ticker, int quantity, Double stopLossPercent, Double takeProfitPercent) {
        SimulatedPosition pos = getPositionState(ticker);
        if (pos.hasOpenPosition()) {
            return ExecutionResult.failed("Position already exists for " + ticker);
        }
        // E1: MAX_CONCURRENT checked here (single point) — TMON@ excluded from limit
        if (!isTmonParking(ticker) && getOpenPositionCount() >= MAX_CONCURRENT_POSITIONS) {
            return ExecutionResult.failed("Max concurrent positions (" + MAX_CONCURRENT_POSITIONS + ") reached");
        }

        Candle bar = getCurrentCandle(ticker, "5_MIN");
        if (bar == null) {
            return ExecutionResult.failed("No market data for " + ticker);
        }
        double rawPrice = bar.close;
        TickerInfo info = TickerRepository.INSTANCE.getByName(ticker);
        int lotSize = info != null && info.getLot() != null ? Math.max(1, info.getLot()) : 1;

        double slippedEntry = rawPrice * (1.0 + slippage);
        double entryNotional = notional(quantity, lotSize, slippedEntry);
        double commission = entryNotional * getEffectiveCommission(ticker);
        if (entryNotional + commission > sharedCash) {
            return ExecutionResult.failed(
                    "Insufficient cash: needed " + (entryNotional + commission)
                            + ", available " + sharedCash);
        }

        sharedCash -= (entryNotional + commission);
        pos.lotSize = lotSize;

        // Default SL/TP mirroring live openPosition (2% / 4%)
        double slPrice = stopLossPercent != null
                ? rawPrice * (1.0 - stopLossPercent / 100.0)
                : rawPrice * (1.0 - DEFAULT_SL_PERCENT / 100.0);
        double tpPrice = takeProfitPercent != null
                ? rawPrice * (1.0 + takeProfitPercent / 100.0)
                : rawPrice * (1.0 + DEFAULT_TP_PERCENT / 100.0);

        pos.position = new Position(
                "BUY", slippedEntry, slPrice, tpPrice, quantity, 0, 0, 1);
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
    public ExecutionResult sell(String ticker, int quantity, Double stopLossPercent, Double takeProfitPercent) {
        SimulatedPosition pos = getPositionState(ticker);
        if (pos.hasOpenPosition()) {
            return ExecutionResult.failed("Position already exists for " + ticker);
        }
        // E1: MAX_CONCURRENT checked here (single point) — TMON@ excluded from limit
        if (!isTmonParking(ticker) && getOpenPositionCount() >= MAX_CONCURRENT_POSITIONS) {
            return ExecutionResult.failed("Max concurrent positions (" + MAX_CONCURRENT_POSITIONS + ") reached");
        }

        Candle bar = getCurrentCandle(ticker, "5_MIN");
        if (bar == null) {
            return ExecutionResult.failed("No market data for " + ticker);
        }
        double rawPrice = bar.close;
        TickerInfo info = TickerRepository.INSTANCE.getByName(ticker);
        int lotSize = info != null && info.getLot() != null ? Math.max(1, info.getLot()) : 1;

        double slippedEntry = rawPrice * (1.0 - slippage);
        double entryNotional = notional(quantity, lotSize, slippedEntry);
        double marginRequired = entryNotional * SHORT_MARGIN_RATIO;
        double commission = entryNotional * getEffectiveCommission(ticker);
        if (marginRequired + commission > sharedCash) {
            return ExecutionResult.failed(
                    "Insufficient margin: needed " + (marginRequired + commission)
                            + ", available " + sharedCash);
        }

        // Post margin at open, return it at close
        sharedCash -= (marginRequired + commission);
        pos.lotSize = lotSize;

        double slPrice = stopLossPercent != null
                ? rawPrice * (1.0 + stopLossPercent / 100.0)
                : rawPrice * (1.0 + DEFAULT_SL_PERCENT / 100.0);
        double tpPrice = takeProfitPercent != null
                ? rawPrice * (1.0 - takeProfitPercent / 100.0)
                : rawPrice * (1.0 - DEFAULT_TP_PERCENT / 100.0);

        pos.position = new Position(
                "SELL", slippedEntry, slPrice, tpPrice, quantity, 0, 0, 1);
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
                p.position.quantity,
                p.position.candlesHeld,
                0,  // Position.cooldownRemaining не используется движком; источник истины — SimulatedPosition.cooldownRemaining
                p.position.appliedLeverage);
    }

    /**
     * Check SL/TP on the given bar and close position if triggered, at the SL/TP price.
     *
     * <p><b>Invariant (Block 9.1):</b> Does NOT check cooldownRemaining — SL/TP triggers even
     * immediately after entry. This ensures positions are properly protected from the first bar.</p>
     *
     * @return execution result if the position was closed, or null if none triggered
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
        boolean slHit = sl != null && (isLong ? currentBar.low <= sl : currentBar.high >= sl);
        boolean tpHit = tp != null && (isLong ? currentBar.high >= tp : currentBar.low <= tp);
        if (!slHit && !tpHit) {
            return null;
        }
        // Pessimistic: if both hit, close at SL first
        double exitPrice = slHit ? sl : tp;
        String reason = slHit ? "sl_hit" : "tp_hit";
        // closePosition applies slippage internally, so pass the raw SL/TP price
        return closePosition(ticker, pos, exitPrice, currentBar.time, reason);
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
        pos.position = new Position(
            pos.position.direction,
            pos.entryPrice,
            pos.position.stopLoss,
            pos.position.takeProfit,
            pos.position.quantity - sharesToSell,
            pos.position.candlesHeld,
            0,  // Position.cooldownRemaining не используется движком
            pos.position.appliedLeverage);
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
            sharedCash += (pos.postedMargin + grossPnl - exitCommission);
        } else {
            sharedCash += (entryNotional + grossPnl - exitCommission);
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
        }

        pos.position = new Position();
        pos.entryPrice = 0.0;
        pos.postedMargin = 0.0;
        return ExecutionResult.success(quantity, exitPrice);
    }

    @Override
    public double getAvailableCash() {
        return sharedCash;
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
