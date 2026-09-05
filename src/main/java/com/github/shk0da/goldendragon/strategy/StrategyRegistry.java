package com.github.shk0da.goldendragon.strategy;

import com.github.shk0da.goldendragon.config.DataCollectorConfig;
import com.github.shk0da.goldendragon.config.MainConfig;
import com.github.shk0da.goldendragon.config.UnifiedTraderConfig;
import com.github.shk0da.goldendragon.config.OrderBookScalpConfig;
import com.github.shk0da.goldendragon.service.TradingService;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import static java.lang.System.out;
import static java.util.List.*;

/**
 * Central registry of all runnable strategies. Each entry defines an optional live runner (used by
 * GoldenDragon).
 */
public final class StrategyRegistry {

    /** Runs a strategy live against the market. */
    @FunctionalInterface
    public interface LiveRunner {
        void run(
                MainConfig mainConfig,
                TradingService tradingService,
                String[] args)
                throws Exception;
    }

    @FunctionalInterface
    private interface StrategyAction {
        void execute(
                MainConfig mainConfig,
                TradingService tradingService,
                String[] args)
                throws Exception;
    }

    /** Registered strategy with its optional live runner. */
    public static final class Entry {
        private final String name;
        private final LiveRunner liveRunner;

        private Entry(String name, LiveRunner liveRunner) {
            this.name = name;
            this.liveRunner = liveRunner;
        }

        public String name() {
            return name;
        }

        public boolean hasLiveRunner() {
            return liveRunner != null;
        }

        public void runLive(
                MainConfig mainConfig,
                TradingService tradingService,
                String[] args)
                throws Exception {
            liveRunner.run(mainConfig, tradingService, args);
        }
    }

    private static final Map<String, Entry> ENTRIES = new LinkedHashMap<>();

    private StrategyRegistry() {
        // Utility class - prevent instantiation
    }

    private static void register(String name, LiveRunner liveRunner) {
        ENTRIES.put(name, new Entry(name, liveRunner));
    }

    /** Builds a live runner that logs errors. */
    private static LiveRunner runAndNotify(String name, String endMessage, StrategyAction action) {
        return (mainConfig, tradingService, args) -> {
            try {
                action.execute(mainConfig, tradingService, args);
            } catch (final Exception ex) {
                out.printf("%s error: %s%n", name, ex.getMessage());
                ex.printStackTrace();
            }
        };
    }

    static {
        register(
                "OrderBookScalpStrategy",
                runAndNotify(
                        "OrderBookScalpStrategy",
                        "Stop OrderBookScalpStrategy",
                        (mc, ts, args) ->
                                new OrderBookScalpStrategy(ts, mc, new OrderBookScalpConfig()).run()));
        register(
                "RegimeAwareStrategy",
                runAndNotify(
                        "RegimeAwareStrategy",
                        "Stop RegimeAwareStrategy",
                        (mc, ts, args) ->
                                new RegimeAwareStrategy(new UnifiedTraderConfig(), ts).run()));
        register(
                "DataCollector",
                runAndNotify(
                        "DataCollector",
                        "End DataCollector",
                        (mc, ts, args) ->
                                new DataCollector(new DataCollectorConfig(), ts).run()));
    }

    public static Entry get(String name) {
        return ENTRIES.get(name);
    }

    /**
     * Create a backtest strategy instance for a given name.
     *
     * <p>Used by BacktestRunner to instantiate strategies without live broker dependencies.
     * The backtest broker is already set via {@link BaseStrategy#setBacktestBroker}.</p>
     *
     * @param strategyName name of the strategy to create
     * @param config trader configuration
     * @return a BaseStrategy instance ready for backtest processing
     */
    public static BaseStrategy createBacktest(String strategyName, UnifiedTraderConfig config) {
        if ("RegimeAwareStrategy".equals(strategyName)) {
            return new RegimeAwareStrategy(config, null);
        } else {
            throw new IllegalArgumentException("Unknown strategy: " + strategyName);
        }
    }

    /**
     * Get list of strategy names that support backtesting.
     * @return list of strategy names
     */
    public static List<String> backtestableNames() {
        return of("RegimeAwareStrategy");
    }
}
