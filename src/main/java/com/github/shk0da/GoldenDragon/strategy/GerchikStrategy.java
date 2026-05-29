package com.github.shk0da.GoldenDragon.strategy;

import com.github.shk0da.GoldenDragon.config.UnifiedTraderConfig;
import com.github.shk0da.GoldenDragon.model.Candle;
import com.github.shk0da.GoldenDragon.model.Config;
import com.github.shk0da.GoldenDragon.model.Position;
import com.github.shk0da.GoldenDragon.model.TradingDecision;
import com.github.shk0da.GoldenDragon.service.TCSService;
import com.github.shk0da.GoldenDragon.strategy.gerchik.GerchikExitManager;
import com.github.shk0da.GoldenDragon.strategy.gerchik.GerchikPositionSizer;
import com.github.shk0da.GoldenDragon.strategy.gerchik.GerchikRiskManager;
import com.github.shk0da.GoldenDragon.strategy.gerchik.OpeningRangeCalculator;
import com.github.shk0da.GoldenDragon.strategy.gerchik.RangeBreakoutDetector;
import com.github.shk0da.GoldenDragon.strategy.gerchik.SignalQualityFilter;
import com.github.shk0da.GoldenDragon.strategy.gerchik.TimeOfDayFilter;
import java.time.LocalTime;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

/**
 * Gerchik-style intraday strategy.
 * Time-based trading windows, opening range breakouts, strict risk control.
 */
public class GerchikStrategy extends BaseStrategy {

    // Gerchik components
    private final TimeOfDayFilter timeFilter;
    private final OpeningRangeCalculator orCalculator;
    private final RangeBreakoutDetector breakoutDetector;
    private final SignalQualityFilter qualityFilter;
    private final GerchikPositionSizer positionSizer;
    private final GerchikRiskManager riskManager;
    private final GerchikExitManager exitManager;

    // Configuration
    private final boolean gerchikEnabled;
    private final double riskPercent;
    private final double atrStopMultiplier;
    private final double tpRewardRatio;

    // State tracking
    private final ConcurrentMap<String, Candle> openingRanges = new ConcurrentHashMap<>();
    private final ConcurrentMap<String, Double> initialRiskPerTicker = new ConcurrentHashMap<>();
    private LocalTime lastSessionDate = null;

    public GerchikStrategy(UnifiedTraderConfig unifiedTraderConfig, TCSService tcsService) {
        this(unifiedTraderConfig, tcsService, new Config(), false);
    }

    public GerchikStrategy(UnifiedTraderConfig unifiedTraderConfig, TCSService tcsService, Config config) {
        this(unifiedTraderConfig, tcsService, config, false);
    }

    public GerchikStrategy(UnifiedTraderConfig unifiedTraderConfig, TCSService tcsService, Config config, boolean isBacktest) {
        super(unifiedTraderConfig, tcsService, config, isBacktest);

        // Gerchik parameters (hardcoded defaults)
        this.gerchikEnabled = true;
        this.riskPercent = 0.01; // 1% per trade
        this.atrStopMultiplier = 2.0; // 2 ATR stop
        this.tpRewardRatio = 2.0; // 2R target

        // Initialize components
        this.timeFilter = new TimeOfDayFilter(
                LocalTime.of(10, 0),   // session start
                LocalTime.of(21, 0),   // session end
                LocalTime.of(19, 0),   // no entry after
                LocalTime.of(20, 50)   // EOD close
        );

        this.orCalculator = new OpeningRangeCalculator(
                30,     // 30 minutes OR
                false,  // use time, not bar count
                6       // 6 bars (if using bar count)
        );

        this.breakoutDetector = new RangeBreakoutDetector(
                0.5,    // 0.5 ATR buffer
                20,     // 20 bar lookback
                true,   // use inside bar
                true    // use narrow range
        );

        this.qualityFilter = new SignalQualityFilter(
                0.4,    // min quality 40%
                0.8     // high quality 80%
        );

        this.positionSizer = new GerchikPositionSizer(
                riskPercent,
                atrStopMultiplier,
                tpRewardRatio
        );

        this.riskManager = new GerchikRiskManager(
                5,      // max 5 trades per day
                10000,  // max 10000 RUB daily loss
                3       // max 3 consecutive losses
        );

        this.exitManager = new GerchikExitManager(
                1.0,    // BE after 1 ATR
                1.5,    // Trail at 1.5 ATR
                true    // use trailing
        );

        logWithBacktest("GerchikStrategy initialized: risk=" + (riskPercent * 100) +
                "%, stop=" + atrStopMultiplier + "ATR, TP=" + tpRewardRatio + "R");
    }

    @Override
    protected String getStrategyName() {
        return "GerchikStrategy";
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

        LocalTime currentTime = LocalTime.now();

        // Check session reset
        checkSessionReset(currentTime);

        // Check EOD close
        if (timeFilter.shouldCloseAll(currentTime)) {
            if (position.quantity > 0) {
                return new TradingDecision("CLOSE", "EOD", 0.0, position.quantity,
                        null, null, minuteCandles.get(minuteCandles.size() - 1).close,
                        new Position(config.cooldownCandles));
            }
            return new TradingDecision("HOLD", "EOD_CLOSED");
        }

        // Check trading window
        if (!timeFilter.isWithinTradingWindow(currentTime)) {
            return new TradingDecision("HOLD", "OUTSIDE_WINDOW");
        }

        Candle cur = minuteCandles.get(minuteCandles.size() - 1);
        double currentPrice = cur.close;
        double atr = atrVal(hourCandles, config.atrPeriod);

        if (atr <= 0) {
            return new TradingDecision("HOLD", "ATR_ZERO");
        }

        // Manage existing position
        if (position.quantity > 0) {
            return manageExistingPosition(ticker, position, cur, hourCandles, minuteCandles, atr, currentTime);
        }

        // Check entry time
        if (!timeFilter.canEnterPosition(currentTime)) {
            return new TradingDecision("HOLD", "LATE_ENTRY");
        }

        // Check risk limits
        if (!riskManager.canTrade(ticker)) {
            return new TradingDecision("HOLD", "RISK_LIMIT_TRADES_" + riskManager.getTradesToday() +
                    "_LOSS_" + (int) riskManager.getDailyPnL());
        }

        // Calculate/update opening range
        if (!orCalculator.isOpeningRangeComplete(minuteCandles)) {
            return new TradingDecision("HOLD", "OR_INCOMPLETE");
        }

        Candle orCandle = orCalculator.calculateOpeningRange(minuteCandles);
        if (orCandle == null) {
            return new TradingDecision("HOLD", "OR_NULL");
        }

        openingRanges.put(ticker, orCandle);

        // Detect breakout
        String signal = breakoutDetector.detectBreakout(minuteCandles, orCandle, atr, currentPrice);
        if (signal == null) {
            return new TradingDecision("HOLD", "NO_BREAKOUT");
        }

        // Filter quality
        double quality = qualityFilter.calculateQuality(signal, cur, minuteCandles, atr);
        if (!qualityFilter.isQualitySufficient(quality)) {
            return new TradingDecision("HOLD", "LOW_QUALITY_" + (int) (quality * 100) + "%");
        }

        // Calculate position size
        double stopLoss = positionSizer.calculateStopLoss(currentPrice, orCandle.high, orCandle.low, atr, true);
        int qty = positionSizer.calculateSize(currentPrice, stopLoss, balance);

        if (qty <= 0) {
            return new TradingDecision("HOLD", "QTY_ZERO");
        }

        double takeProfit = positionSizer.calculateTakeProfit(currentPrice, stopLoss, true);
        double initialRisk = currentPrice - stopLoss;
        initialRiskPerTicker.put(ticker, initialRisk);

        logWithBacktest("Gerchik OPEN " + ticker + ": qty=" + qty + ", entry=" + currentPrice +
                ", SL=" + String.format("%.4f", stopLoss) + ", signal=" + signal + ", quality=" + (int) (quality * 100) + "%");

        return new TradingDecision(
                "OPEN",
                signal,
                quality,
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
                                                    Candle cur, List<Candle> hourCandles,
                                                    List<Candle> minuteCandles,
                                                    double atr, LocalTime currentTime) {
        double currentPrice = cur.close;
        Double initialRisk = initialRiskPerTicker.get(ticker);
        if (initialRisk == null) {
            initialRisk = position.entryPrice - (position.stopLoss != null ? position.stopLoss : position.entryPrice);
        }

        // Check for take profit
        if (exitManager.shouldTakeProfit(position, currentPrice, position.takeProfit)) {
            initialRiskPerTicker.remove(ticker);
            return new TradingDecision("CLOSE", "TAKE_PROFIT", 0.0, position.quantity,
                    null, null, currentPrice, new Position(config.cooldownCandles));
        }

        // Update stop loss
        Double newStop = exitManager.updateStop(position, cur, atr, initialRisk);
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
            return new TradingDecision("HOLD", "STOP_UPDATE", 0.0, 0,
                    null, null, null, updatedPosition);
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

    /**
     * Check if new session started and reset.
     */
    private void checkSessionReset(LocalTime currentTime) {
        LocalTime sessionStart = timeFilter.getSessionStart();
        
        // Detect new session (crossed session start time)
        if (lastSessionDate == null || 
            (currentTime.isAfter(sessionStart) && lastSessionDate.isAfter(sessionStart))) {
            // Same session, no reset
        } else if (currentTime.isBefore(sessionStart) && lastSessionDate != null && lastSessionDate.isAfter(sessionStart)) {
            // Crossed midnight, but not yet new session
        } else if (currentTime.isAfter(sessionStart) && (lastSessionDate == null || lastSessionDate.isBefore(sessionStart))) {
            // New session started
            riskManager.resetDaily();
            openingRanges.clear();
            initialRiskPerTicker.clear();
                logWithBacktest("GerchikStrategy: New session reset completed");
        }
        
        lastSessionDate = currentTime;
    }

    @Override
    protected void onTradeClosed(String ticker, double pnl, double entryPrice,
                                  double exitPrice, int quantity, String direction) {
        riskManager.registerTrade(pnl);
        initialRiskPerTicker.remove(ticker);
        logWithBacktest("Gerchik CLOSED " + ticker + ": PnL=" + String.format("%.2f", pnl) +
                ", tradesToday=" + riskManager.getTradesToday());
    }

    @Override
    protected void onDailyReset() {
        riskManager.resetDaily();
        openingRanges.clear();
        initialRiskPerTicker.clear();
        logWithBacktest("GerchikStrategy: Daily reset completed");
    }
}
