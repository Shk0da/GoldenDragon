package com.github.shk0da.goldendragon.strategy;

import static com.github.shk0da.goldendragon.service.TelegramNotifyService.telegramNotifyService;

import com.github.shk0da.goldendragon.config.TmonAveragingConfig;
import com.github.shk0da.goldendragon.config.UnifiedTraderConfig;
import com.github.shk0da.goldendragon.model.Candle;
import com.github.shk0da.goldendragon.model.Config;
import com.github.shk0da.goldendragon.model.Position;
import com.github.shk0da.goldendragon.model.TickerInfo;
import com.github.shk0da.goldendragon.model.TradingDecision;
import com.github.shk0da.goldendragon.service.TCSService;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/**
 * TMON Averaging Strategy with dynamic ATR-based step sizing.
 *
 * <p>Buys on ANY downward candle (close < previous close). Uses ATR to determine allocation per
 * step: higher ATR = more aggressive buying (fewer remaining steps). Exits when price recovers
 * above average entry price.
 *
 * <p>Key idea: TMON@ steadily appreciates (~17%/year). Every dip is a buying opportunity. ATR helps
 * size positions proportionally to current volatility.
 *
 * <p>Uses its own {@link TmonAveragingConfig} — does not depend on {@code UnifiedTraderConfig} for
 * ticker selection or trading logic.
 */
public class TmonAveragingStrategy extends BaseStrategy {

    private final TmonAveragingConfig tmonConfig;

    private Phase phase = Phase.WAITING;
    private double cashBalance;
    private int positionQuantity;
    private double avgEntryPrice;
    private int entryStep;
    private int exitStep;
    private List<Candle> lastCandles;

    public enum Phase {
        WAITING,
        SCALING_IN,
        HOLDING,
        SCALING_OUT,
        EXITED
    }

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
        this.tmonConfig = new TmonAveragingConfig();
        this.cashBalance = initialCash;
    }

    public TmonAveragingStrategy(
            TmonAveragingConfig tmonConfig,
            TCSService tcsService,
            Config backtestConfig,
            boolean isBacktest,
            double initialCash) {
        super(null, tcsService, backtestConfig, isBacktest);
        this.tmonConfig = tmonConfig;
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
    public void run() {
        if (tcsService == null) {
            log(getStrategyName() + " stopped: tcsService is null.");
            return;
        }

        var initPortfolioCost = safeGetTotalPortfolioCost();
        var infoMessage =
                getStrategyName()
                        + " started. Ticker: "
                        + TmonAveragingConfig.TICKER
                        + ", Total Portfolio Cost: "
                        + initPortfolioCost;
        telegramNotifyService.sendMessage(infoMessage);
        log(infoMessage);

        if (!tmonConfig.isEnabled()) {
            log(getStrategyName() + " disabled in config.");
            return;
        }

        ExecutorService executor = Executors.newFixedThreadPool(2);
        try {
            CompletableFuture<Void> candleUpdater =
                    CompletableFuture.runAsync(
                            () -> {
                                while (isWorkingHours()) {
                                    com.github.shk0da.goldendragon.utils.TimeUtils.sleep(60_000);
                                }
                            },
                            executor);

            CompletableFuture<Void> trader =
                    CompletableFuture.runAsync(
                            () -> {
                                while (isWorkingHours()) {
                                    try {
                                        processTmonTicker();
                                    } catch (Exception ex) {
                                        log(getStrategyName() + " error: " + ex.getMessage());
                                    }
                                    com.github.shk0da.goldendragon.utils.TimeUtils.sleep(30_000);
                                }
                            },
                            executor);

            CompletableFuture.allOf(candleUpdater, trader).join();
        } finally {
            shutdownExecutor(executor);

            var endPortfolioCost = safeGetTotalPortfolioCost();
            var message = getStrategyName() + " stopped. Total Portfolio Cost: " + endPortfolioCost;
            telegramNotifyService.sendMessage(message);
            log(message);
        }
    }

    private void processTmonTicker() {
        TickerInfo tickerInfo = findTickerInfo(TmonAveragingConfig.TICKER);
        if (tickerInfo == null) {
            log("Ticker " + TmonAveragingConfig.TICKER + " not found in repository.");
            return;
        }

        String figi = tickerInfo.getFigi();
        var now = java.time.OffsetDateTime.now();
        String dataDir = tmonConfig.getDataDir();

        List<Candle> hourCandles =
                loadOrRefreshCandles(
                        TmonAveragingConfig.TICKER,
                        figi,
                        dataDir,
                        now,
                        ru.tinkoff.piapi.contract.v1.CandleInterval.CANDLE_INTERVAL_HOUR);
        if (hourCandles == null || hourCandles.isEmpty()) {
            log("No hourly candles for " + TmonAveragingConfig.TICKER + ", skipping.");
            return;
        }

        Position storedPosition =
                positionStore.getOrDefault(TmonAveragingConfig.TICKER, new Position());

        TradingDecision decision =
                decide(
                        TmonAveragingConfig.TICKER,
                        hourCandles,
                        null,
                        storedPosition,
                        cashBalance,
                        false);

        if ("OPEN".equals(decision.action)) {
            openPosition(TmonAveragingConfig.TICKER, tickerInfo, hourCandles, decision);
        } else if ("CLOSE".equals(decision.action)) {
            closePosition(TmonAveragingConfig.TICKER, tickerInfo, storedPosition, decision);
        }
    }

    @Override
    public TradingDecision decide(
            String ticker,
            List<Candle> hourCandles,
            List<Candle> minuteCandles,
            Position position,
            double balance,
            boolean incrementCandlesHeld) {
        int atrPeriod = tmonConfig.getAtrPeriod();
        if (hourCandles == null || hourCandles.size() < atrPeriod + 2) {
            return new TradingDecision("HOLD", "insufficient_history");
        }

        lastCandles = hourCandles;

        Candle current = hourCandles.get(hourCandles.size() - 1);
        Candle previous = hourCandles.get(hourCandles.size() - 2);

        boolean isDip = current.close < previous.close;
        boolean hasProfit =
                positionQuantity > 0
                        && avgEntryPrice > 0
                        && current.close > avgEntryPrice * (1 + tmonConfig.getProfitTarget());

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
        int maxEntrySteps = tmonConfig.getMaxEntrySteps();
        if (isDip && entryStep < maxEntrySteps && cashBalance > current.close) {
            return executeBuy(current);
        }

        if (entryStep >= maxEntrySteps || cashBalance <= current.close) {
            phase = Phase.HOLDING;
            return new TradingDecision("HOLD", "holding");
        }

        return new TradingDecision("HOLD", "scaling_in");
    }

    private TradingDecision handleHolding(Candle current, boolean hasProfit) {
        if (hasProfit && positionQuantity > 0) {
            phase = Phase.SCALING_OUT;
            exitStep = 0;
            return executeSell(current);
        }
        return new TradingDecision("HOLD", "holding");
    }

    private TradingDecision handleScalingOut(Candle current) {
        int maxExitSteps = tmonConfig.getMaxExitSteps();
        if (exitStep < maxExitSteps && positionQuantity > 0) {
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
        if (cashBalance < current.close) {
            return new TradingDecision("HOLD", "insufficient_cash");
        }

        int dynamicSteps = computeDynamicSteps(current);
        int remainingSteps = Math.max(1, dynamicSteps - entryStep);
        double allocation = cashBalance / (double) remainingSteps;
        int qty = Math.max(1, (int) Math.floor(allocation / current.close));
        double cost = qty * current.close;

        if (cost > cashBalance) {
            qty = Math.max(1, (int) Math.floor(cashBalance / current.close));
            cost = qty * current.close;
        }

        if (qty <= 0 || cost <= 0 || cost > cashBalance) {
            return new TradingDecision("HOLD", "insufficient_cash");
        }

        int prevQty = positionQuantity;
        cashBalance -= cost;
        positionQuantity += qty;
        avgEntryPrice = (avgEntryPrice * prevQty + current.close * qty) / (double) positionQuantity;
        entryStep++;

        logPosition("BUY", qty, current.close, dynamicSteps);
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
        if (positionQuantity <= 0) {
            return new TradingDecision("HOLD", "no_position");
        }

        int maxExitSteps = tmonConfig.getMaxExitSteps();
        int qty = Math.max(1, positionQuantity / (maxExitSteps - exitStep));
        qty = Math.min(qty, positionQuantity);
        double proceeds = qty * current.close;
        cashBalance += proceeds;
        positionQuantity -= qty;
        exitStep++;

        if (positionQuantity <= 0) {
            positionQuantity = 0;
            avgEntryPrice = 0;
        }

        logPosition("SELL", qty, current.close, maxExitSteps);
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

    private int computeDynamicSteps(Candle current) {
        double atr = computeAtr(current);
        double atrRatio = atr / current.close;
        int maxEntrySteps = tmonConfig.getMaxEntrySteps();

        if (atrRatio > 0.002) {
            return Math.max(2, maxEntrySteps - 2);
        }
        if (atrRatio > 0.001) {
            return Math.max(3, maxEntrySteps - 1);
        }
        return maxEntrySteps;
    }

    private double computeAtr(Candle current) {
        int atrPeriod = tmonConfig.getAtrPeriod();
        if (lastCandles == null || lastCandles.size() < 2) {
            return current.high - current.low;
        }

        double atr = 0.0;
        int size = Math.min(atrPeriod + 1, lastCandles.size());

        for (int i = lastCandles.size() - size; i < lastCandles.size(); i++) {
            Candle c = lastCandles.get(i);
            double tr =
                    Math.max(
                            c.high - c.low,
                            Math.max(Math.abs(c.high - c.close), Math.abs(c.low - c.close)));
            atr += tr;
        }
        return atr / (double) (size - 1);
    }

    private void logPosition(String action, int qty, double price, int dynamicSteps) {
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
                        + positionQuantity
                        + ", steps="
                        + dynamicSteps);
    }
}
