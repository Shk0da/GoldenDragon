package com.github.shk0da.goldendragon.strategy;

import com.github.shk0da.goldendragon.config.UnifiedTraderConfig;
import com.github.shk0da.goldendragon.model.Candle;
import com.github.shk0da.goldendragon.model.Config;
import com.github.shk0da.goldendragon.model.Position;
import com.github.shk0da.goldendragon.model.TradingDecision;
import com.github.shk0da.goldendragon.service.TCSService;
import java.util.List;

/**
 * TMON Averaging Strategy — aggressive dip buying.
 *
 * <p>Buys on ANY downward candle (close < previous close). Continues buying while price keeps
 * dropping. Exits when price recovers above average entry price.
 *
 * <p>Key idea: TMON@ steadily appreciates (~17%/year). Every dip is a buying opportunity. Scale out
 * when profitable.
 */
public class TmonAveragingStrategy extends BaseStrategy {

    private static final int MAX_ENTRY_STEPS = 5;
    private static final int MAX_EXIT_STEPS = 5;
    private static final double PROFIT_TARGET = 0.005;

    public enum Phase {
        WAITING,
        SCALING_IN,
        HOLDING,
        SCALING_OUT,
        EXITED
    }

    private Phase phase = Phase.WAITING;
    private double cashBalance;
    private int positionQuantity;
    private double avgEntryPrice;
    private int entryStep;
    private int exitStep;

    public TmonAveragingStrategy(UnifiedTraderConfig config, TCSService tcsService) {
        this(config, tcsService, new Config(), false, 1000000.0);
    }

    public TmonAveragingStrategy(
            UnifiedTraderConfig config,
            TCSService tcsService,
            Config backtestConfig,
            boolean isBacktest,
            double initialCash) {
        super(config, tcsService, backtestConfig, isBacktest);
        this.cashBalance = initialCash;
    }

    @Override
    protected String getStrategyName() {
        return "TmonAveragingStrategy";
    }

    public Phase getPhase() {
        return phase;
    }

    public double getCashBalance() {
        return cashBalance;
    }

    public int getPositionQuantity() {
        return positionQuantity;
    }

    public double getAvgEntryPrice() {
        return avgEntryPrice;
    }

    public void addMonthlyDeposit(double amount) {
        cashBalance += amount;
        log(
                "TmonAveragingStrategy monthly deposit: +"
                        + String.format("%.2f", amount)
                        + ", cash="
                        + String.format("%.2f", cashBalance));
    }

    @Override
    public TradingDecision decide(
            String ticker,
            List<Candle> hourCandles,
            List<Candle> minuteCandles,
            Position position,
            double balance,
            boolean incrementCandlesHeld) {
        if (hourCandles == null || hourCandles.size() < 3) {
            return new TradingDecision("HOLD", "insufficient_history");
        }

        Candle current = hourCandles.get(hourCandles.size() - 1);
        Candle previous = hourCandles.get(hourCandles.size() - 2);

        boolean isDip = current.close < previous.close;
        boolean hasProfit =
                positionQuantity > 0
                        && avgEntryPrice > 0
                        && current.close > avgEntryPrice * (1 + PROFIT_TARGET);

        switch (phase) {
            case WAITING:
                return handleWaiting(current, isDip);
            case SCALING_IN:
                return handleScalingIn(current, isDip);
            case HOLDING:
                return handleHolding(current, hasProfit);
            case SCALING_OUT:
                return handleScalingOut(current);
            case EXITED:
                return handleExited(current, isDip);
            default:
                return new TradingDecision("HOLD", "unknown_phase");
        }
    }

    private TradingDecision handleWaiting(Candle current, boolean isDip) {
        if (isDip && cashBalance > current.close) {
            phase = Phase.SCALING_IN;
            entryStep = 0;
            return executeBuy(current);
        }
        return new TradingDecision("HOLD", "waiting");
    }

    private TradingDecision handleScalingIn(Candle current, boolean isDip) {
        if (isDip && entryStep < MAX_ENTRY_STEPS && cashBalance > current.close) {
            return executeBuy(current);
        }

        if (entryStep >= MAX_ENTRY_STEPS || cashBalance <= current.close) {
            phase = Phase.HOLDING;
            return new TradingDecision("HOLD", "holding");
        }

        return new TradingDecision("HOLD", "scaling_in");
    }

    private TradingDecision handleHolding(Candle current, boolean hasProfit) {
        if (hasProfit) {
            phase = Phase.SCALING_OUT;
            exitStep = 0;
            return executeSell(current);
        }
        return new TradingDecision("HOLD", "holding");
    }

    private TradingDecision handleScalingOut(Candle current) {
        if (exitStep < MAX_EXIT_STEPS && positionQuantity > 0) {
            return executeSell(current);
        }

        phase = Phase.EXITED;
        exitStep = 0;
        return new TradingDecision("HOLD", "exit_complete");
    }

    private TradingDecision handleExited(Candle current, boolean isDip) {
        if (isDip && cashBalance > current.close) {
            phase = Phase.SCALING_IN;
            entryStep = 0;
            return executeBuy(current);
        }
        return new TradingDecision("HOLD", "exited");
    }

    private TradingDecision executeBuy(Candle current) {
        double allocation = cashBalance / (double) (MAX_ENTRY_STEPS - entryStep);
        int qty = Math.max(1, (int) Math.floor(allocation / current.close));
        double cost = qty * current.close;

        if (cost > cashBalance) {
            qty = Math.max(1, (int) Math.floor(cashBalance / current.close));
            cost = qty * current.close;
        }

        if (qty <= 0 || cost <= 0) {
            return new TradingDecision("HOLD", "insufficient_cash");
        }

        int prevQty = positionQuantity;
        cashBalance -= cost;
        positionQuantity += qty;
        avgEntryPrice = (avgEntryPrice * prevQty + current.close * qty) / (double) positionQuantity;
        entryStep++;

        logPosition("BUY", qty, current.close);
        return new TradingDecision(
                "OPEN",
                "dip_buy",
                1.0,
                qty,
                null,
                null,
                current.close,
                new Position("BUY", current.close, null, null, positionQuantity, 0));
    }

    private TradingDecision executeSell(Candle current) {
        int qty = Math.max(1, positionQuantity / (MAX_EXIT_STEPS - exitStep));
        qty = Math.min(qty, positionQuantity);
        double proceeds = qty * current.close;
        cashBalance += proceeds;
        positionQuantity -= qty;
        exitStep++;

        if (positionQuantity <= 0) {
            positionQuantity = 0;
            avgEntryPrice = 0;
        }

        logPosition("SELL", qty, current.close);
        return new TradingDecision(
                "CLOSE",
                "profit_take",
                1.0,
                qty,
                null,
                null,
                current.close,
                new Position("SELL", current.close, null, null, qty, 0));
    }

    private void logPosition(String action, int qty, double price) {
        log(
                getStrategyName()
                        + " "
                        + action
                        + ": qty="
                        + qty
                        + " @ "
                        + String.format("%.2f", price)
                        + ", avg="
                        + String.format("%.2f", avgEntryPrice)
                        + ", cash="
                        + String.format("%.2f", cashBalance)
                        + ", pos="
                        + positionQuantity);
    }
}
