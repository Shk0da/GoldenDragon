package com.github.shk0da.goldendragon.strategy;

import com.github.shk0da.goldendragon.config.DataCollectorConfig;
import com.github.shk0da.goldendragon.config.MainConfig;
import com.github.shk0da.goldendragon.config.UnifiedTraderConfig;
import com.github.shk0da.goldendragon.service.TCSService;

import java.util.LinkedHashMap;
import java.util.Map;

import static java.lang.System.out;

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
                TCSService tcsService,
                String[] args)
                throws Exception;
    }

    @FunctionalInterface
    private interface StrategyAction {
        void execute(
                MainConfig mainConfig,
                TCSService tcsService,
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
                TCSService tcsService,
                String[] args)
                throws Exception {
            liveRunner.run(mainConfig, tcsService, args);
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
                "RegimeAwareStrategy",
                runAndNotify(
                        "RegimeAwareStrategy",
                        "Stop RegimeAwareStrategy",
                        (mc, tcs, args) ->
                                new RegimeAwareStrategy(new UnifiedTraderConfig(), tcs).run())
);
        register(
                "DataCollector",
                runAndNotify(
                        "DataCollector",
                        "End DataCollector",
                        (mc, tcs, args) ->
                                new DataCollector(new DataCollectorConfig(), tcs).run()));
    }

    public static Entry get(String name) {
        return ENTRIES.get(name);
    }
}
