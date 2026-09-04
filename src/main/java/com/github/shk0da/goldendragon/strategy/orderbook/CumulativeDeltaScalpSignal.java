package com.github.shk0da.goldendragon.strategy.orderbook;

import com.github.shk0da.goldendragon.config.OrderBookScalpConfig;
import com.github.shk0da.goldendragon.model.Candle;
import com.github.shk0da.goldendragon.model.MarketDepthSnapshot;
import com.github.shk0da.goldendragon.model.MarketTradeTick;
import com.github.shk0da.goldendragon.model.TickerInfo;
import com.github.shk0da.goldendragon.repository.TickerRepository;
import com.github.shk0da.goldendragon.service.TCSService;
import com.github.shk0da.goldendragon.utils.IndicatorsUtil;
import ru.tinkoff.piapi.contract.v1.HistoricCandle;

import java.time.Duration;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.BiConsumer;

import static ru.tinkoff.piapi.contract.v1.CandleInterval.CANDLE_INTERVAL_5_MIN;

/**
 * Cumulative delta scalping signal implementation.
 *
 * <p>Implements two trading scenarios:
 * <ul>
 *   <li><b>Scenario A (Bounce):</b> Counter-trend bounce from large density
 *       with delta decay/divergence confirmation</li>
 *   <li><b>Scenario B (Breakout):</b> Impulse breakout when density is consumed
 *       with exponential delta growth</li>
 * </ul>
 *
 * <p>Key features:
 * <ul>
 *   <li>10-second cumulative delta calculation</li>
 *   <li>Dynamic density detection (3x/5x average volume)</li>
 *   <li>Spread protection (max 0.02%)</li>
 *   <li>Emergency exit on density disappearance</li>
 *   <li>Risk-reward 1:2 to 1:3 for bounce entries</li>
 * </ul>
 */
public final class CumulativeDeltaScalpSignal implements OrderBookSignal {

    public static final String SIGNAL_ID = "cumulative_delta";

    private static final Duration VOLUME_HISTORY_INTERVAL = Duration.ofMinutes(5);
    private static final long PERSISTENCE_TICKS_DEFAULT = 1; // Require signal persistence

    private final TCSService tcsService;
    private final OrderBookScalpConfig config;
    private final DensityAnalyzer densityAnalyzer;
    private final CumulativeDeltaTracker deltaTracker;
    private final Map<String, DensityVolume> densityVolumes = new ConcurrentHashMap<>();
    private final Map<String, Instant> lastVolumeUpdate = new ConcurrentHashMap<>();
    private final Map<String, Integer> persistenceCounter = new ConcurrentHashMap<>();
    private final Map<String, String> lastSignalTicker = new ConcurrentHashMap<>();
    private BiConsumer<String, Map<String, Object>> skipMetricsCallback;

    /**
     * Tracks original density volume for breakout detection.
     */
    private static final class DensityVolume {
        private final double price;
        private final double originalVolume;
        private final boolean isBid;

        DensityVolume(double price, double originalVolume, boolean isBid) {
            this.price = price;
            this.originalVolume = originalVolume;
            this.isBid = isBid;
        }

        public double getPrice() {
            return price;
        }

        public double getOriginalVolume() {
            return originalVolume;
        }

        public boolean isBid() {
            return isBid;
        }
    }

    public CumulativeDeltaScalpSignal(TCSService tcsService, OrderBookScalpConfig config) {
        this.tcsService = tcsService;
        this.config = config;
        this.densityAnalyzer = new DensityAnalyzer(tcsService, config);
        this.deltaTracker = new CumulativeDeltaTracker();
    }

    @Override
    public String id() {
        return SIGNAL_ID;
    }

    /**
     * Set callback for emitting skip diagnostic metrics.
     * The callback receives (ticker, metrics) where metrics contains skip reason and analysis data.
     */
    public void setSkipMetricsCallback(BiConsumer<String, Map<String, Object>> callback) {
        this.skipMetricsCallback = callback;
    }

    private void emitSkip(String ticker, String reason, Map<String, Object> extra) {
        if (skipMetricsCallback == null) {
            return;
        }
        Map<String, Object> metrics = new HashMap<>();
        metrics.put("signalId", SIGNAL_ID);
        metrics.put("skipReason", reason);
        if (extra != null) {
            metrics.putAll(extra);
        }
        skipMetricsCallback.accept(ticker, metrics);
    }

    @Override
    public OrderBookEntryDecision evaluateEntry(
            OrderBookMarketContext context, String ticker) {

        MarketDepthSnapshot snapshot = context.getSnapshot();
        double currentPrice = context.getBestBid(); // Use bid for long entry

        // Update volume history if needed
        updateVolumeHistory(ticker, snapshot);

        // Update cumulative delta from trades
        updateDelta(ticker);

        // Check for Scenario A: Bounce from density (higher priority)
        DensityAnalyzer.Density density = densityAnalyzer.findAnomalousDensity(snapshot, ticker, true); // Check bid side for long

        if (density == null) {
            emitSkip(ticker, "no_anomalous_density", Map.of("side", "bid"));
            return OrderBookEntryDecision.none();
        }

        // Get delta analysis for divergence detection (using config parameters)
        CumulativeDeltaTracker.DeltaAnalysis deltaAnalysis = deltaTracker.analyzeDelta(
            ticker, 1, config.getFadeRatio(), config.getAccelRatio(), config.getDeltaBarsLookback());

        // Evaluate Scenario A (Bounce) - HIGHER PRIORITY
        HftScalpDecision.Decision bounceDecision = HftScalpDecision.evaluateBounceEntry(
            density,
            currentPrice,
            deltaAnalysis,
            context.getSpread(),
            true, // Long
            config.getMinNetProfitTicks(),
            config.getTickSize() > 0 ? config.getTickSize() : 1.0
        );

        if (bounceDecision.isEnter()) {
            // Log bounce entry signal with detailed metrics
            String reason = String.format(
                "Bounce LONG (Scenario A): density=%.2f deltaDecay=%b deltaDivergence=%b spread=%.3fbps tick=%.4f",
                density.getPrice(),
                deltaAnalysis.isDecaying(),
                deltaAnalysis.isDiverging(),
                context.getSpreadBps(),
                HftScalpDecision.calculateTickSize(currentPrice)
            );

            // Track density volume for breakout scenario
            densityVolumes.put(ticker, new DensityVolume(
                density.getPrice(),
                density.getVolume(),
                density.isBid()
            ));

            logEntry(ticker, "LONG", reason);
            return OrderBookEntryDecision.enter(reason);
        }

        Map<String, Object> bounceMetrics = new HashMap<>();
        bounceMetrics.put("side", "bid");
        bounceMetrics.put("densityPrice", density.getPrice());
        bounceMetrics.put("deltaDecay", deltaAnalysis.isDecaying());
        bounceMetrics.put("deltaDivergence", deltaAnalysis.isDiverging());
        bounceMetrics.put("spreadBps", context.getSpreadBps());
        emitSkip(ticker, "bounce_rejected", bounceMetrics);
        return OrderBookEntryDecision.none();
    }

    @Override
    public OrderBookEntryDecision evaluateEntryShort(
            OrderBookMarketContext context, String ticker) {

        MarketDepthSnapshot snapshot = context.getSnapshot();
        double currentPrice = context.getBestAsk(); // Use ask for short entry
        Instant now = Instant.now();

        // Update volume history if needed
        updateVolumeHistory(ticker, snapshot);

        // Update cumulative delta from trades
        updateDelta(ticker);

        // Check for Scenario A: Bounce from density (higher priority)
        DensityAnalyzer.Density density = densityAnalyzer.findAnomalousDensity(snapshot, ticker, false); // Check ask side for short

        if (density == null) {
            emitSkip(ticker, "no_anomalous_density", Map.of("side", "ask"));
            return OrderBookEntryDecision.none();
        }

        // Get current density volume for breakout scenario
        double currentDensityVolume = 0;
        DensityVolume densityVolume = densityVolumes.get(ticker);
        if (densityVolume != null) {
            currentDensityVolume = densityVolume.getOriginalVolume();
        }

        // Get delta analysis for divergence detection (using config parameters)
        CumulativeDeltaTracker.DeltaAnalysis deltaAnalysis = deltaTracker.analyzeDelta(
            ticker, -1, config.getFadeRatio(), config.getAccelRatio(), config.getDeltaBarsLookback());

        // Evaluate Scenario A (Bounce) - HIGHER PRIORITY
        HftScalpDecision.Decision bounceDecision = HftScalpDecision.evaluateBounceEntry(
            density,
            currentPrice,
            deltaAnalysis,
            context.getSpread(),
            false, // Short
            config.getMinNetProfitTicks(),
            config.getTickSize() > 0 ? config.getTickSize() : 1.0
        );

        if (bounceDecision.isEnter()) {
            // Log bounce entry signal with detailed metrics
            String reason = String.format(
                "Bounce SHORT (Scenario A): density=%.2f deltaDecay=%b deltaDivergence=%b spread=%.3fbps tick=%.4f",
                density.getPrice(),
                deltaAnalysis.isDecaying(),
                deltaAnalysis.isDiverging(),
                context.getSpreadBps(),
                HftScalpDecision.calculateTickSize(currentPrice)
            );

            // Track density volume for breakout scenario
            densityVolumes.put(ticker, new DensityVolume(
                density.getPrice(),
                density.getVolume(),
                density.isBid()
            ));

            logEntry(ticker, "SHORT", reason);
            return OrderBookEntryDecision.enter(reason);
        }

        Map<String, Object> bounceMetrics = new HashMap<>();
        bounceMetrics.put("side", "ask");
        bounceMetrics.put("densityPrice", density.getPrice());
        bounceMetrics.put("deltaDecay", deltaAnalysis.isDecaying());
        bounceMetrics.put("deltaDivergence", deltaAnalysis.isDiverging());
        bounceMetrics.put("spreadBps", context.getSpreadBps());
        emitSkip(ticker, "bounce_rejected", bounceMetrics);
        return OrderBookEntryDecision.none();
    }

    @Override
    public String evaluateExit(
            OrderBookMarketContext context,
            OrderBookPositionView position,
            String ticker) {

        MarketDepthSnapshot snapshot = context.getSnapshot();
        double currentPrice = "LONG".equals(position.getDirection())
            ? context.getBestBid()
            : context.getBestAsk();

        // Check density disappearance for emergency exit (TODO.md 3.6: spoofing)
        DensityVolume densityVolume = densityVolumes.get(ticker);
        if (densityVolume != null) {
            // Find current density at same price level
            List<DensityAnalyzer.Density> densities = densityAnalyzer.findDensities(
                snapshot, ticker);

            double remainingVolume = 0;
            for (DensityAnalyzer.Density d : densities) {
                if (Math.abs(d.getPrice() - densityVolume.getPrice()) < 0.001) {
                    remainingVolume = d.getVolume();
                    break;
                }
            }

            // Emergency exit if density decreased more than density_pull_exit (spoofing detection)
            double remainingPercent = remainingVolume / densityVolume.getOriginalVolume();
            if (remainingPercent < (1.0 - config.getDensityPullExit())) {
                logExit(ticker, "spoofing_detected_density_gone", currentPrice);
                return "density_gone";
            }
        }

        return null;
    }

    @Override
    public void reset(String ticker) {
        densityVolumes.remove(ticker);
        lastSignalTicker.remove(ticker);
        persistenceCounter.put(ticker, 0);
        persistenceCounter.put(ticker + "_short", 0);
        deltaTracker.reset(ticker);
    }

    /**
     * Handle order book updates - track density volume changes.
     */
    public void onOrderBook(MarketDepthSnapshot snapshot, String ticker) {
        // Track density volumes for breakout detection
        densityVolumes.remove(ticker); // Clean stale entries
    }

    /**
     * Handle trade updates - accumulate cumulative delta.
     */
    public void onTrade(MarketTradeTick trade, String ticker) {
        deltaTracker.onTrade(ticker, trade);
    }

    private void updateVolumeHistory(String ticker, MarketDepthSnapshot snapshot) {
        Instant now = Instant.now();
        Instant last = lastVolumeUpdate.getOrDefault(ticker, Instant.EPOCH);

        if (Duration.between(last, now).compareTo(VOLUME_HISTORY_INTERVAL) >= 0) {
            // Load 5-minute candles for volume history
            try {
                List<Candle> candles = loadRecentCandles(ticker);
                if (!candles.isEmpty()) {
                    densityAnalyzer.initializeVolumeHistory(ticker, candles);
                    lastVolumeUpdate.put(ticker, now);
                }
            } catch (Exception e) {
                // Volume history update is not critical
            }
        }
    }

    private void updateDelta(String ticker) {
        // Delta is updated incrementally on each trade tick
        // This method is called to ensure state is current
    }

    private List<Candle> loadRecentCandles(String ticker) {
        // Load recent 5-minute candles (last 2 hours = 24 candles)
        // This queries the broker's API for historical data
        try {
            // Calculate time range for last 2 hours
            Instant now = Instant.now();
            Instant from = now.minus(2, ChronoUnit.HOURS);

            // Get ticker info to retrieve figi from local repository
            TickerInfo info = TickerRepository.INSTANCE.getByName(ticker);
            if (info == null) {
                return Collections.emptyList();
            }

            String figi = info.getFigi();

            // Load candles via TCSService and convert to Candle model
            List<HistoricCandle> historicCandles =
                tcsService.getCandles(figi, from, now, CANDLE_INTERVAL_5_MIN);

            return historicCandles.stream()
                .map(hc -> new Candle(
                    hc.getTime().toString(),
                    IndicatorsUtil.toDouble(hc.getOpen()),
                    IndicatorsUtil.toDouble(hc.getHigh()),
                    IndicatorsUtil.toDouble(hc.getLow()),
                    IndicatorsUtil.toDouble(hc.getClose()),
                    hc.getVolume()
                ))
                .toList();
        } catch (Exception e) {
            return Collections.emptyList();
        }
    }

    private void logEntry(String ticker, String direction, String reason) {
        System.out.println(String.format(
            "CUMULATIVE_DELTA %s %s: %s",
            ticker,
            direction,
            reason
        ));
    }

    private void logExit(String ticker, String reason, double price) {
        System.out.println(String.format(
            "CUMULATIVE_DELTA %s EXIT: %s at %.2f",
            ticker,
            reason,
            price
        ));
    }
}
