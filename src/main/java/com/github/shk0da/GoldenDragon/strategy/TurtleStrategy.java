package com.github.shk0da.GoldenDragon.strategy;

import com.github.shk0da.GoldenDragon.config.UnifiedTraderConfig;
import com.github.shk0da.GoldenDragon.model.Candle;
import com.github.shk0da.GoldenDragon.model.Config;
import com.github.shk0da.GoldenDragon.model.Position;
import com.github.shk0da.GoldenDragon.model.TradingDecision;
import com.github.shk0da.GoldenDragon.service.TCSService;
import com.github.shk0da.GoldenDragon.strategy.turtle.TurtleBreakoutSignal;
import com.github.shk0da.GoldenDragon.strategy.turtle.TurtleDrawdownManager;
import com.github.shk0da.GoldenDragon.strategy.turtle.TurtleExitManager;
import com.github.shk0da.GoldenDragon.strategy.turtle.TurtlePositionSizer;
import com.github.shk0da.GoldenDragon.strategy.turtle.TurtlePyramidingManager;
import com.github.shk0da.GoldenDragon.strategy.turtle.TurtleRiskManager;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

/**
 * Turtle Trading strategy implementation.
 * Based on classic Turtle Trading rules with modern adaptations.
 */
public class TurtleStrategy extends BaseStrategy {

    // Turtle components
    private final TurtleBreakoutSignal breakoutSignal;
    private final TurtlePositionSizer positionSizer;
    private final TurtleExitManager exitManager;
    private final TurtleRiskManager riskManager;
    private final TurtleDrawdownManager drawdownManager;
    private final TurtlePyramidingManager pyramidingManager;

    // Configuration
    private final boolean turtleEnabled;
    private final int entryLookback;
    private final double riskPercent;
    private final double atrStopMultiplier;

    // Track initial risk per position
    private final ConcurrentMap<String, Double> initialRiskPerTicker = new ConcurrentHashMap<>();

    public TurtleStrategy(UnifiedTraderConfig unifiedTraderConfig, TCSService tcsService) {
        this(unifiedTraderConfig, tcsService, new Config(), false);
    }

    public TurtleStrategy(UnifiedTraderConfig unifiedTraderConfig, TCSService tcsService, Config config) {
        this(unifiedTraderConfig, tcsService, config, false);
    }

    public TurtleStrategy(UnifiedTraderConfig unifiedTraderConfig, TCSService tcsService, Config config, boolean isBacktest) {
        super(unifiedTraderConfig, tcsService, config, isBacktest);

        // Turtle parameters - OPTIMIZED for RegimeAwareStrategy
        this.turtleEnabled = true;
        this.entryLookback = 20;
        this.riskPercent = 0.015; // 1.5% risk (increased for trending markets)
        this.atrStopMultiplier = 2.5; // Wider stop for trends
        
        // Initialize Turtle components - OPTIMIZED for RegimeAwareStrategy
        this.breakoutSignal = new TurtleBreakoutSignal(
                20,  // default lookback
                10,  // min lookback
                55,  // max lookback
                0.5, // breakout buffer
                0.5, // volatility low threshold
                2.0, // volatility high threshold
                25.0 // ADX filter - only trade when ADX > 25
        );

        this.positionSizer = new TurtlePositionSizer(
                riskPercent,
                atrStopMultiplier,
                1,   // min lot size
                1,   // lot step
                0.15 // maxPositionSize: 15% of capital
        );
        
        this.exitManager = new TurtleExitManager(
                2.0,  // trailing multiplier
                10,   // exit lookback
                true  // use Donchian exit
        );
        
        this.riskManager = new TurtleRiskManager(
                8,    // max positions
                0.20  // max capital at risk
        );
        
        this.drawdownManager = new TurtleDrawdownManager(
                0.05, // reduce risk at 5% DD
                0.10  // stop trading at 10% DD
        );
        
        this.pyramidingManager = new TurtlePyramidingManager(
                3,    // max units (reduced from 4 for RegimeAware)
                1.5   // add unit after 1.5 ATR (increased for quality)
        );
        
        logWithBacktest("TurtleStrategy: OPTIMIZED for RegimeAware (ADX>25, risk=1.5%, 3 units max)");

        logWithBacktest("TurtleStrategy initialized: lookback=20, risk=1%, stop=2ATR, ADX>25");
    }

    @Override
    protected String getStrategyName() {
        return "TurtleStrategy";
    }

    @Override
    public TradingDecision decide(String ticker,
                                  List<Candle> hourCandles,
                                  List<Candle> minuteCandles,
                                  Position position,
                                  double balance,
                                  boolean incrementCandlesHeld) {
        if (hourCandles == null || hourCandles.size() < 60 || minuteCandles == null || minuteCandles.isEmpty()) {
            return new TradingDecision("HOLD", "init");
        }

        // Check drawdown limits
        if (!drawdownManager.canTrade()) {
            return new TradingDecision("HOLD", "DD_LIMIT_" + (int) drawdownManager.getDrawdownPercent() + "%");
        }

        Candle cur = minuteCandles.get(minuteCandles.size() - 1);
        double currentPrice = cur.close;
        double atr = atrVal(hourCandles, config.atrPeriod);

        if (atr <= 0) {
            return new TradingDecision("HOLD", "ATR_ZERO");
        }

        // Manage existing position
        if (position.quantity > 0) {
            return manageExistingPosition(ticker, position, cur, hourCandles, atr);
        }

        // Check risk limits
        int openPositions = (int) positionStore.values().stream()
                .filter(p -> p.quantity > 0)
                .count();

        double positionRisk = balance * riskPercent;
        if (!riskManager.canOpenPosition(ticker, openPositions, balance, positionRisk)) {
            return new TradingDecision("HOLD", "RISK_LIMIT");
        }

        // Detect breakout signal
        String signal = breakoutSignal.detectSignal(hourCandles, atr, currentPrice);
        if (signal == null) {
            return new TradingDecision("HOLD", "NO_BREAKOUT");
        }

        // Calculate position size
        int qty = positionSizer.calculateSize(currentPrice, atr, balance);
        if (qty <= 0) {
            return new TradingDecision("HOLD", "QTY_ZERO");
        }

        // Calculate stop loss and take profit
        double stopLoss = positionSizer.calculateStopLoss(currentPrice, atr);
        double takeProfit = currentPrice + (atr * atrStopMultiplier * 2.5); // 2.5R target

        // Track initial risk
        initialRiskPerTicker.put(ticker, currentPrice - stopLoss);

        // Add initial unit
        pyramidingManager.addUnit(ticker, currentPrice, qty);

        logWithBacktest("Turtle OPEN " + ticker + ": qty=" + qty + ", entry=" + currentPrice +
                ", SL=" + String.format("%.4f", stopLoss) + ", signal=" + signal);

        return new TradingDecision(
                "OPEN",
                signal,
                1.0,  // confidence
                qty,
                stopLoss,
                takeProfit,
                currentPrice,
                new Position("BUY", currentPrice, stopLoss, takeProfit, qty, 0)
        );
    }

    /**
     * Manage existing position.
     */
    private TradingDecision manageExistingPosition(String ticker, Position position,
                                                    Candle cur, List<Candle> hourCandles, double atr) {
        double currentPrice = cur.close;

        // Check for exit signals
        if (exitManager.shouldExitDonchian(hourCandles, position, currentPrice)) {
            pyramidingManager.removePosition(ticker);
            initialRiskPerTicker.remove(ticker);
            return new TradingDecision(
                    "CLOSE", "DONCHIAN_EXIT", 0.0, position.quantity,
                    null, null, currentPrice, new Position(config.cooldownCandles)
            );
        }

        // Update trailing stop
        Double newStop = exitManager.updateTrailingStop(position, cur, atr);
        if (newStop != null && newStop > (position.stopLoss != null ? position.stopLoss : 0.0)) {
            Position updatedPosition = new Position(
                    position.direction,
                    position.entryPrice,
                    newStop,
                    position.takeProfit,
                    position.quantity,
                    position.candlesHeld + 1,
                    position.cooldownRemaining
            );
            return new TradingDecision("HOLD", "TRAIL_UPDATE", 0.0, 0,
                    null, null, null, updatedPosition);
        }

        // Check for pyramiding opportunity
        if (pyramidingManager.canAddUnit(ticker, position, currentPrice, atr)) {
            // Calculate additional unit size (same as initial)
            int unitSize = position.quantity / pyramidingManager.getUnitState(ticker).units;
            
            // Add unit
            TurtlePyramidingManager.UnitState state = pyramidingManager.addUnit(ticker, currentPrice, unitSize);
            
                logWithBacktest("Turtle ADD UNIT " + ticker + ": unit=" + state.units + ", avgEntry=" + 
                String.format("%.4f", state.avgEntry));
            
            // Return HOLD with updated position (pyramiding handled externally)
        }

        // Normal HOLD
        Position updatedPosition = new Position(
                position.direction,
                position.entryPrice,
                position.stopLoss,
                position.takeProfit,
                position.quantity,
                position.candlesHeld + 1,
                position.cooldownRemaining
        );

        return new TradingDecision("HOLD", "IN_POS", 0.0, 0,
                null, null, null, updatedPosition);
    }

    @Override
    protected void onTradeClosed(String ticker, double pnl, double entryPrice,
                                  double exitPrice, int quantity, String direction) {
        // Update drawdown manager
        long equityCents = (long) (tcsService.getTotalPortfolioCost() * 100);
        drawdownManager.updateEquity(equityCents);

        // Remove position from pyramiding manager
        pyramidingManager.removePosition(ticker);
        initialRiskPerTicker.remove(ticker);

        // Register position close with risk manager
        riskManager.registerPositionClose(ticker);

        logWithBacktest("Turtle CLOSED " + ticker + ": PnL=" + String.format("%.2f", pnl) +
                ", DD=" + String.format("%.2f", drawdownManager.getDrawdownPercent()) + "%");
    }

    @Override
    protected void onDailyReset() {
        drawdownManager.reset();
        initialRiskPerTicker.clear();
        logWithBacktest("TurtleStrategy: Daily reset completed");
    }
}
