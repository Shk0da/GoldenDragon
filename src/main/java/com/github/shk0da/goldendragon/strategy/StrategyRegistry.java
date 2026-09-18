package com.github.shk0da.goldendragon.strategy;

import com.github.shk0da.goldendragon.config.MainConfig;
import com.github.shk0da.goldendragon.config.UnifiedTraderConfig;
import com.github.shk0da.goldendragon.model.Config;
import com.github.shk0da.goldendragon.service.TradingService;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import static java.lang.System.out;
import static java.util.List.of;

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

        Entry(String name, LiveRunner liveRunner) {
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
                "UnifiedStrategy",
                runAndNotify(
                        "UnifiedStrategy",
                        "Stop UnifiedStrategy",
                        (mc, ts, args) ->
                                new UnifiedStrategy(new UnifiedTraderConfig(), ts, new Config(new UnifiedTraderConfig()), mc).run()));

        register(
                "TradeCouncilStrategy",
                runAndNotify(
                        "TradeCouncilStrategy",
                        "Stop TradeCouncilStrategy",
                        (mc, ts, args) -> {
                            try {
                                TradeCouncilStrategy strategy = new TradeCouncilStrategy(new UnifiedTraderConfig(), ts, new Config(new UnifiedTraderConfig()), mc);
                                strategy.buildKeyLevels();
                                strategy.run();
                            } catch (Exception e) {
                                out.printf("TradeCouncilStrategy initialization error: %s%n", e.getMessage());
                                e.printStackTrace();
                            }
                        }));
    }

    public static Entry get(String name) {
        return ENTRIES.get(name);
    }

}
