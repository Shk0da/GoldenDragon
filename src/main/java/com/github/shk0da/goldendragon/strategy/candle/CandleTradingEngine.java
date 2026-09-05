package com.github.shk0da.goldendragon.strategy.candle;

import com.github.shk0da.goldendragon.config.MainConfig;
import com.github.shk0da.goldendragon.config.OrderBookScalpConfig;
import com.github.shk0da.goldendragon.model.Candle;
import com.github.shk0da.goldendragon.model.TickerInfo;
import com.github.shk0da.goldendragon.service.TradingService;

import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

import static com.github.shk0da.goldendragon.utils.TimeUtils.sleep;

/**
 * Trading engine for candle-based scalping strategy.
 *
 * <p>Architecture:
 * <ol>
 *   <li>Poll recent 1-min candles from TradingService</li>
 *   <li>Evaluate candle-based signals (delta, volume, momentum)</li>
 *   <li>Execute trades based on signal decisions</li>
 *   <li>Monitor positions and apply stop loss / take profit</li>
 * </ol>
 *
 * <p>Polling interval: every 30-60 seconds (configurable).
 */
public class CandleTradingEngine {

    private static final long DEFAULT_POLL_INTERVAL_MS = 30_000L; // 30 seconds
    private static final int CANDLES_TO_KEEP = 50; // Keep last 50 candles per ticker

    private final TradingService tradingService;
    private final MainConfig mainConfig;
    private final OrderBookScalpConfig config;
    private final List<CandleScalpSignal> signals;
    private final String strategyName;
    private final List<String> instruments;

    // State tracking
    private volatile double initialEquity;
    private final Map<String, CandleHistory> candleHistoryByTicker = new ConcurrentHashMap<>();
    private final Map<String, PositionEntry> positionsByTicker = new ConcurrentHashMap<>();

    public CandleTradingEngine(
            TradingService tradingService,
            MainConfig mainConfig,
            OrderBookScalpConfig config,
            List<CandleScalpSignal> signals,
            List<String> instruments,
            String strategyName) {
        this.tradingService = tradingService;
        this.mainConfig = mainConfig;
        this.config = config;
        this.signals = signals;
        this.instruments = instruments;
        this.strategyName = strategyName;
    }

    /** Run the candle-based trading session. */
    public void run() {
        boolean paper = tradingService.isPaperTrading();
        log(strategyName + ": starting candle-based session, paper=" + paper + ", serviceType=" + tradingService.getServiceType());
        log(strategyName + ": instruments=" + instruments);
        log(strategyName + ": signals=" + signals.stream().map(CandleScalpSignal::id).collect(Collectors.joining(", ")));

        tradingService.logAccountTradingEligibility();

        initInitialEquity();

        try {
            while (true) {
                runCandleLoop(paper);
            }
        } catch (Exception ex) {
            log(strategyName + ": session error: " + ex.getMessage());
            ex.printStackTrace();
        }
    }

    private void initInitialEquity() {
        try {
            Double availableCash = tradingService.getAvailableCash();
            if (availableCash != null && availableCash > 0.0) {
                initialEquity = availableCash;
                log(strategyName + ": initialEquity (available cash)=" + String.format("%.2f", initialEquity));
            } else {
                initialEquity = 100_000.0; // Default synthetic equity
                log(strategyName + ": using synthetic initialEquity=" + String.format("%.2f", initialEquity));
            }
        } catch (Exception ex) {
            initialEquity = 100_000.0;
            log(strategyName + ": fallback to synthetic initialEquity=" + String.format("%.2f", initialEquity));
        }
    }

    /** Main polling loop: fetch candles, evaluate signals, execute trades. */
    private void runCandleLoop(boolean paper) {
        log(strategyName + ": starting candle poll cycle");

        for (String ticker : instruments) {
            try {
                processTicker(ticker, paper);
            } catch (Exception ex) {
                log(strategyName + ": error processing " + ticker + ": " + ex.getMessage());
            }
        }

        log(strategyName + ": completed candle poll cycle, sleeping " + DEFAULT_POLL_INTERVAL_MS + "ms");
        sleep(DEFAULT_POLL_INTERVAL_MS);
    }

    /** Process a single ticker: fetch candles, evaluate signals, check exits. */
    private void processTicker(String ticker, boolean paper) {
        // Fetch recent candles
        List<Candle> candles = fetchCandles(ticker, 50);
        if (candles == null || candles.isEmpty()) {
            log(strategyName + ": no candles for " + ticker + ", skipping");
            return;
        }

        // Update candle history
        candleHistoryByTicker.compute(ticker, (k, existing) -> {
            if (existing == null) {
                return new CandleHistory(ticker, candles);
            }
            existing.addCandles(candles);
            return existing;
        });

        CandleHistory history = candleHistoryByTicker.get(ticker);
        CandleMarketContext context = new CandleMarketContext(history.getRecentCandles(10));

        // Check exit conditions for existing positions
        PositionEntry position = positionsByTicker.get(ticker);
        if (position != null) {
            String exitReason = evaluateExit(ticker, position, context);
            if (exitReason != null) {
                log(strategyName + ": EXIT " + ticker + " " + position.direction + ": " + exitReason);
                closePosition(ticker, position, paper);
            }
        }

            // Evaluate entry signals
        if (position == null) {
            CandleEntryDecision decision = evaluateEntry(ticker, context);
            if (decision.shouldEnter()) {
                log(strategyName + ": ENTER " + ticker + " " + decision.reason() + " quality=" + String.format("%.2f", decision.quality()));
                executeTrade(ticker, decision, paper, context);
            }
        }
    }

    /** Fetch recent 1-min candles from TradingService. */
    private List<Candle> fetchCandles(String ticker, int count) {
        try {
            TickerInfo.Key key = new TickerInfo.Key(ticker, null);
            TickerInfo tickerInfo = tradingService.searchTicker(key);
            
            String figi = ticker;
            if (tickerInfo != null && tickerInfo.getFigi() != null) {
                figi = tickerInfo.getFigi();
            }

            List<Candle> candles = tradingService.getCandles(figi, "1_MIN", count);
            
            if (candles == null || candles.isEmpty()) {
                log(strategyName + ": empty candles for " + ticker);
                return new ArrayList<>();
            }

            return candles;
        } catch (Exception ex) {
            log(strategyName + ": error fetching candles for " + ticker + ": " + ex.getMessage());
            return new ArrayList<>();
        }
    }

    /** Evaluate entry signals for a ticker. */
    private CandleEntryDecision evaluateEntry(String ticker, CandleMarketContext context) {
        for (CandleScalpSignal signal : signals) {
            CandleEntryDecision longDecision = signal.evaluateEntry(context, ticker);
            if (longDecision.shouldEnter() && longDecision.quality() >= config.getEntryQualityThreshold()) {
                return longDecision;
            }

            CandleEntryDecision shortDecision = signal.evaluateEntryShort(context, ticker);
            if (shortDecision.shouldEnter() && shortDecision.quality() >= config.getEntryQualityThreshold()) {
                return shortDecision;
            }
        }
        return CandleEntryDecision.none();
    }

    /** Evaluate exit conditions for an existing position. */
    private String evaluateExit(String ticker, PositionEntry position, CandleMarketContext context) {
        for (CandleScalpSignal signal : signals) {
            String exitReason = signal.evaluateExit(context, new CandlePositionView(
                position.direction,
                position.entryPrice,
                position.units,
                signal.id()
            ), ticker);
            if (exitReason != null) {
                return exitReason;
            }
        }
        return null;
    }

    /** Execute a trade based on signal decision. */
    private void executeTrade(String ticker, CandleEntryDecision decision, boolean paper, CandleMarketContext context) {
        try {
            double positionCash = tradingService.getAvailableCash() != null 
                ? tradingService.getAvailableCash() * config.getPositionCashPercent() / 100.0 
                : 0.0;

            int units = executeMarketTrade(ticker, decision, paper, positionCash);
            if (units > 0) {
                // Store position
                double entryPrice = context.getLatestClose();
                positionsByTicker.put(ticker, new PositionEntry(
                    decision.reason().contains("LONG") ? "LONG" : "SHORT",
                    units,
                    System.currentTimeMillis(),
                    entryPrice
                ));
                log(strategyName + ": executed " + decision.reason() + " for " + ticker + " units=" + units);
            }
        } catch (Exception ex) {
            log(strategyName + ": error executing trade for " + ticker + ": " + ex.getMessage());
        }
    }

    /** Execute a market trade (simplified - in real implementation would handle lots, margin, etc.). */
    private int executeMarketTrade(String ticker, CandleEntryDecision decision, boolean paper, double cashToUse) {
        String direction = decision.reason().contains("LONG") ? "BUY" : "SELL";
        try {
            TickerInfo.Key key = new TickerInfo.Key(ticker, null);
            TickerInfo tickerInfo = tradingService.searchTicker(key);
            
            // Calculate units based on cash and price
            int units = 1; // Placeholder - would need proper price/lot calculation
            if (paper) {
                log(strategyName + ": [PAPER] " + direction + " " + units + " units of " + ticker);
            } else {
                // Real trade - would call tradingService.buyByMarket() or similar
                log(strategyName + ": [REAL] Would execute " + direction + " " + units + " units of " + ticker);
            }
            return units;
        } catch (Exception ex) {
            log(strategyName + ": error executing " + direction + " for " + ticker + ": " + ex.getMessage());
            return 0;
        }
    }

    /** Close an existing position. */
    private void closePosition(String ticker, PositionEntry position, boolean paper) {
        try {
            String direction = position.direction.equals("LONG") ? "SELL" : "BUY";
            TickerInfo.Key key = new TickerInfo.Key(ticker, null);
            
            if (paper) {
                log(strategyName + ": [PAPER] " + direction + " " + position.units + " units of " + ticker);
            } else {
                log(strategyName + ": [REAL] Would execute " + direction + " " + position.units + " units of " + ticker);
            }
            
            positionsByTicker.remove(ticker);
        } catch (Exception ex) {
            log(strategyName + ": error closing position for " + ticker + ": " + ex.getMessage());
        }
    }

    /** Helper to log messages. */
    private void log(String message) {
        System.out.println("[CandleEngine:" + strategyName + "] " + message);
    }

    /** Thread-safe candle history container. */
    private static class CandleHistory {
        private final String ticker;
        private final List<Candle> candles = new ArrayList<>();

        CandleHistory(String ticker, List<Candle> candles) {
            this.ticker = ticker;
            addCandles(candles);
        }

        void addCandles(List<Candle> newCandles) {
            synchronized (candles) {
                for (Candle candle : newCandles) {
                    candles.add(candle);
                }
                // Keep only last CANDLES_TO_KEEP
                while (candles.size() > CANDLES_TO_KEEP) {
                    candles.remove(0);
                }
            }
        }

        List<Candle> getRecentCandles(int count) {
            synchronized (candles) {
                if (candles.isEmpty()) {
                    return new ArrayList<>();
                }
                int start = Math.max(0, candles.size() - count);
                return new ArrayList<>(candles.subList(start, candles.size()));
            }
        }
    }

    /** Position entry record. */
    private static class PositionEntry {
        final String direction;
        final int units;
        final long entryTimeMs;
        final double entryPrice;

        PositionEntry(String direction, int units, long entryTimeMs, double entryPrice) {
            this.direction = direction;
            this.units = units;
            this.entryTimeMs = entryTimeMs;
            this.entryPrice = entryPrice;
        }
    }
}
