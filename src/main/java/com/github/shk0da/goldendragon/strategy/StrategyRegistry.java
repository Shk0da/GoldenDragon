package com.github.shk0da.goldendragon.strategy;

import com.github.shk0da.goldendragon.config.DataCollectorConfig;
import com.github.shk0da.goldendragon.config.MainConfig;
import com.github.shk0da.goldendragon.config.UnifiedTraderConfig;
import com.github.shk0da.goldendragon.model.Config;
import com.github.shk0da.goldendragon.service.TCSService;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import static java.lang.System.out;

/**
 * Central registry of all runnable strategies. Each entry defines an optional live runner (used by
 * GoldenDragon) and an optional backtest factory (used by BacktestRunner).
 */
public final class StrategyRegistry {

    /** Runs a strategy live against the market. */
    @FunctionalInterface
    public interface LiveRunner {
        void run(
                MainConfig mainConfig,
                TCSService tcsService,
                String[] args)
                throws Exception;
    }

    /** Creates a strategy instance for the backtest simulation engine. */
    @FunctionalInterface
    public interface BacktestFactory {
        BaseStrategy create(UnifiedTraderConfig config, TCSService tcsService);
    }

    @FunctionalInterface
    private interface StrategyAction {
        void execute(
                MainConfig mainConfig,
                TCSService tcsService,
                String[] args)
                throws Exception;
    }

    /** Registered strategy with its optional live runner and backtest factory. */
    public static final class Entry {
        private final String name;
        private final LiveRunner liveRunner;
        private final BacktestFactory backtestFactory;

        private Entry(String name, LiveRunner liveRunner, BacktestFactory backtestFactory) {
            this.name = name;
            this.liveRunner = liveRunner;
            this.backtestFactory = backtestFactory;
        }

        public String name() {
            return name;
        }

        public boolean hasLiveRunner() {
            return liveRunner != null;
        }

        public boolean hasBacktestFactory() {
            return backtestFactory != null;
        }

        public void runLive(
                MainConfig mainConfig,
                TCSService tcsService,
                String[] args)
                throws Exception {
            liveRunner.run(mainConfig, tcsService, args);
        }

        public BaseStrategy createBacktest(UnifiedTraderConfig config, TCSService tcsService) {
            return backtestFactory.create(config, tcsService);
        }
    }

    private static final Map<String, Entry> ENTRIES = new LinkedHashMap<>();

    private StrategyRegistry() {
        // Utility class - prevent instantiation
    }

    private static void register(
            String name, LiveRunner liveRunner, BacktestFactory backtestFactory) {
        ENTRIES.put(name, new Entry(name, liveRunner, backtestFactory));
    }

    /** Builds a live runner that logs errors. */
    private static LiveRunner runAndNotify(String name, String endMessage, StrategyAction action) {
        return (mainConfig, tcsService, args) -> {
            try {
                action.execute(mainConfig, tcsService, args);
            } catch (final Exception ex) {
                out.printf("%s error: %s%n", name, ex.getMessage());
                ex.printStackTrace();
            }
        };
    }

    static {
        register(
                "UnifiedStrategy",
                runAndNotify(
                        "UnifiedStrategy",
                        "Stop UnifiedStrategy",
                        (mc, tcs, args) ->
                                new UnifiedStrategy(new UnifiedTraderConfig(), tcs).run()),
                (config, tcsService) -> new UnifiedStrategy(config, tcsService, new Config(), true));
        register(
                "DataCollector",
                runAndNotify(
                        "DataCollector",
                        "End DataCollector",
                        (mc, tcs, args) ->
                                new DataCollector(new DataCollectorConfig(), tcs).run()),
                null);
    }

    public static Entry get(String name) {
        return ENTRIES.get(name);
    }

    public static BaseStrategy createBacktest(String name, UnifiedTraderConfig config) {
        return createBacktest(name, config, null);
    }

    public static BaseStrategy createBacktest(String name, UnifiedTraderConfig config, TCSService tcsService) {
        Entry entry = ENTRIES.get(name);
        if (entry == null || !entry.hasBacktestFactory()) {
            throw new IllegalArgumentException("Unknown backtest strategy: " + name);
        }
        return entry.createBacktest(config, tcsService);
    }

    /** Names of strategies supported by the backtest simulation engine, in registration order. */
    public static List<String> backtestableNames() {
        List<String> names = new ArrayList<>();
        for (Entry entry : ENTRIES.values()) {
            if (entry.hasBacktestFactory()) {
                names.add(entry.name());
            }
        }
        return names;
    }
}
