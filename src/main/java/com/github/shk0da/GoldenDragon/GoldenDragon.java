package com.github.shk0da.goldendragon;

import static com.github.shk0da.goldendragon.repository.TickerRepository.SERIALIZE_NAME;
import static com.github.shk0da.goldendragon.service.TelegramNotifyService.telegramNotifyService;
import static com.github.shk0da.goldendragon.utils.SerializationUtils.getDateOfContentOnDisk;
import static com.github.shk0da.goldendragon.utils.SerializationUtils.loadDataFromDisk;
import static com.github.shk0da.goldendragon.utils.SerializationUtils.saveDataToDisk;
import static com.github.shk0da.goldendragon.utils.TimeUtils.sleep;
import static java.lang.System.currentTimeMillis;
import static java.lang.System.out;
import static java.lang.System.setOut;
import static java.nio.charset.StandardCharsets.UTF_8;
import static java.util.TimeZone.setDefault;

import com.github.shk0da.goldendragon.config.DataCollectorConfig;
import com.github.shk0da.goldendragon.config.LevelTraderConfig;
import com.github.shk0da.goldendragon.config.MainConfig;
import com.github.shk0da.goldendragon.config.MarketConfig;
import com.github.shk0da.goldendragon.config.RSXConfig;
import com.github.shk0da.goldendragon.config.RebalanceConfig;
import com.github.shk0da.goldendragon.config.UnifiedTraderConfig;
import com.github.shk0da.goldendragon.model.Market;
import com.github.shk0da.goldendragon.model.TickerInfo;
import com.github.shk0da.goldendragon.repository.Repository;
import com.github.shk0da.goldendragon.repository.TickerRepository;
import com.github.shk0da.goldendragon.service.TCSService;
import com.github.shk0da.goldendragon.strategy.DataCollector;
import com.github.shk0da.goldendragon.strategy.DivFlow;
import com.github.shk0da.goldendragon.strategy.IndicatorTrader;
import com.github.shk0da.goldendragon.strategy.LevelTrader;
import com.github.shk0da.goldendragon.strategy.ModelGenerator;
import com.github.shk0da.goldendragon.strategy.RSX;
import com.github.shk0da.goldendragon.strategy.Rebalance;
import com.github.shk0da.goldendragon.strategy.RegimeAwareStrategyMl;
import com.github.shk0da.goldendragon.strategy.UnifiedStrategy;
import com.google.gson.reflect.TypeToken;
import java.io.FileDescriptor;
import java.io.FileOutputStream;
import java.io.PrintStream;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.TimeZone;
import java.util.concurrent.Callable;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

/**
 * Main application entry point for GoldenDragon trading system. Supports multiple trading
 * strategies: LevelTrader, UnifiedStrategy, RSX, DivFlow, Rebalance, etc. Initializes market data,
 * ticker repository, and runs selected strategy.
 */
public final class GoldenDragon {

    private static final Repository<TickerInfo.Key, TickerInfo> tickerRepository =
            TickerRepository.INSTANCE;

    private static final String STRATEGY_REBALANCE = "Rebalance";
    private static final String STRATEGY_RSX = "RSX";
    private static final String STRATEGY_DIV_FLOW = "DivFlow";
    private static final String STRATEGY_INDICATOR_TRADER = "IndicatorTrader";
    private static final String STRATEGY_DATA_COLLECTOR = "DataCollector";
    private static final String STRATEGY_LEVEL_TRADER = "LevelTrader";
    private static final String STRATEGY_UNIFIED = "UnifiedStrategy";
    private static final String STRATEGY_GENERATE_MODEL = "GenerateModel";
    private static final String STRATEGY_REGIME_AWARE_ML = "RegimeAwareStrategyMl";
    private static final int DEFAULT_ARG_INDEX = 0;
    private static final int MARKET_ARG_INDEX = 1;
    private static final int ACCOUNT_ARG_INDEX = 2;
    private static final String DEFAULT_STRATEGY = "LevelTrader";
    private static final Market DEFAULT_MARKET = Market.MOEX;
    private static final int SLEEP_MS = 5_000;

    private GoldenDragon() {
        // Utility class - prevent instantiation
    }

    public static void main(final String[] args) {
        setDefault(TimeZone.getTimeZone("Europe/Moscow"));
        setOut(new PrintStream(new FileOutputStream(FileDescriptor.out), true, UTF_8));
        out.printf("%s: Start GoldenDragon%n", new Date());

        try {
            final MainConfig mainConfig = new MainConfig();
            final String strategy = getStrategy(args);
            final Market market = getMarket(args);
            final String accountId = getAccountId(args, mainConfig);
            out.println("Run: " + strategy + " " + market.name() + " [" + accountId + "]");

            final MarketConfig marketConfig = MarketConfig.byMarket(market);
            final TCSService tcsService =
                    new TCSService(mainConfig.withAccountId(accountId), marketConfig);
            updateTickerRepository(tcsService);

            executeStrategy(strategy, mainConfig, marketConfig, tcsService, args);
        } catch (final Exception ex) {
            out.printf("Error: %s%n", ex.getMessage());
            ex.printStackTrace();
        }
        out.printf("%s: Finish GoldenDragon%n", new Date());
        sleep(SLEEP_MS);
        System.exit(0);
    }

    private static String getStrategy(final String[] args) {
        return args.length > DEFAULT_ARG_INDEX ? args[DEFAULT_ARG_INDEX] : DEFAULT_STRATEGY;
    }

    private static Market getMarket(final String[] args) {
        return args.length > MARKET_ARG_INDEX
                ? Market.valueOf(args[MARKET_ARG_INDEX])
                : DEFAULT_MARKET;
    }

    private static String getAccountId(final String[] args, final MainConfig mainConfig) {
        return args.length > ACCOUNT_ARG_INDEX
                ? args[ACCOUNT_ARG_INDEX]
                : mainConfig.getTcsAccountId();
    }

    private static void executeStrategy(
            final String strategy,
            final MainConfig mainConfig,
            final MarketConfig marketConfig,
            final TCSService tcsService,
            final String[] args) {

        switch (strategy) {
            case STRATEGY_REBALANCE:
                executeRebalance(marketConfig, tcsService);
                break;
            case STRATEGY_RSX:
                executeRsx(mainConfig, marketConfig, tcsService);
                break;
            case STRATEGY_DIV_FLOW:
                executeDivFlow(mainConfig, marketConfig, tcsService);
                break;
            case STRATEGY_INDICATOR_TRADER:
                executeIndicatorTrader(tcsService);
                break;
            case STRATEGY_DATA_COLLECTOR:
                executeDataCollector(tcsService);
                break;
            case STRATEGY_LEVEL_TRADER:
                executeLevelTrader(tcsService);
                break;
            case STRATEGY_UNIFIED:
                executeUnifiedStrategy(tcsService);
                break;
            case STRATEGY_GENERATE_MODEL:
                executeGenerateModel(args);
                break;
            case STRATEGY_REGIME_AWARE_ML:
                executeRegimeAwareStrategyMl(tcsService);
                break;
            default:
                out.println("Unknown strategy: " + strategy);
        }
    }

    private static void executeRebalance(
            final MarketConfig marketConfig, final TCSService tcsService) {
        telegramNotifyService.sendMessage("Run Rebalance");
        try {
            final RebalanceConfig rebalanceConfig = new RebalanceConfig();
            new Rebalance(marketConfig, rebalanceConfig, tcsService).run();
            telegramNotifyService.sendMessage("End Rebalance");
        } catch (final Exception ex) {
            out.printf("Rebalance error: %s%n", ex.getMessage());
            ex.printStackTrace();
        }
    }

    private static void executeRsx(
            final MainConfig mainConfig,
            final MarketConfig marketConfig,
            final TCSService tcsService) {
        telegramNotifyService.sendMessage("Run RSX");
        try {
            final RSXConfig rsxConfig = new RSXConfig();
            new RSX(mainConfig, marketConfig, rsxConfig, tcsService).run();
            telegramNotifyService.sendMessage("End RSX");
        } catch (final Exception ex) {
            out.printf("RSX error: %s%n", ex.getMessage());
            ex.printStackTrace();
        }
    }

    private static void executeDivFlow(
            final MainConfig mainConfig,
            final MarketConfig marketConfig,
            final TCSService tcsService) {
        telegramNotifyService.sendMessage("Run DivFlow");
        try {
            new DivFlow(mainConfig, marketConfig, tcsService).run();
            telegramNotifyService.sendMessage("End DivFlow");
        } catch (final Exception ex) {
            out.printf("DivFlow error: %s%n", ex.getMessage());
            ex.printStackTrace();
        }
    }

    private static void executeIndicatorTrader(final TCSService tcsService) {
        telegramNotifyService.sendMessage("Run IndicatorTrader");
        try {
            new IndicatorTrader(tcsService).run();
            telegramNotifyService.sendMessage("End IndicatorTrader");
        } catch (final Exception ex) {
            out.printf("IndicatorTrader error: %s%n", ex.getMessage());
            ex.printStackTrace();
        }
    }

    private static void executeDataCollector(final TCSService tcsService) {
        telegramNotifyService.sendMessage("Run DataCollector");
        try {
            final DataCollectorConfig dataCollectorConfig = new DataCollectorConfig();
            new DataCollector(dataCollectorConfig, tcsService).run();
            telegramNotifyService.sendMessage("End DataCollector");
        } catch (final Exception ex) {
            out.printf("DataCollector error: %s%n", ex.getMessage());
            ex.printStackTrace();
        }
    }

    private static void executeLevelTrader(final TCSService tcsService) {
        telegramNotifyService.sendMessage("Run LevelTrader");
        try {
            final LevelTraderConfig levelTraderConfig = new LevelTraderConfig();
            new LevelTrader(levelTraderConfig, tcsService).run();
            telegramNotifyService.sendMessage("Stop LevelTrader");
        } catch (final Exception ex) {
            out.printf("LevelTrader error: %s%n", ex.getMessage());
            ex.printStackTrace();
        }
    }

    private static void executeUnifiedStrategy(final TCSService tcsService) {
        telegramNotifyService.sendMessage("Run UnifiedStrategy");
        try {
            final UnifiedTraderConfig unifiedTraderConfig = new UnifiedTraderConfig();
            new UnifiedStrategy(unifiedTraderConfig, tcsService).run();
            telegramNotifyService.sendMessage("Stop UnifiedStrategy");
        } catch (final Exception ex) {
            out.printf("UnifiedStrategy error: %s%n", ex.getMessage());
            ex.printStackTrace();
        }
    }

    private static void executeGenerateModel(final String[] args) {
        telegramNotifyService.sendMessage("Run GenerateModel");
        try {
            new ModelGenerator().runGenerateModel(args);
        } catch (final Exception ex) {
            out.printf("GenerateModel error: %s%n", ex.getMessage());
            ex.printStackTrace();
        }
        telegramNotifyService.sendMessage("Stop GenerateModel");
    }

    private static void executeRegimeAwareStrategyMl(final TCSService tcsService) {
        telegramNotifyService.sendMessage("Run RegimeAwareStrategyMl");
        try {
            final UnifiedTraderConfig unifiedTraderConfig = new UnifiedTraderConfig();
            new RegimeAwareStrategyMl(unifiedTraderConfig, tcsService).run();
            telegramNotifyService.sendMessage("Stop RegimeAwareStrategyMl");
        } catch (final Exception ex) {
            out.printf("RegimeAwareStrategyMl error: %s%n", ex.getMessage());
            ex.printStackTrace();
        }
    }

    private static void updateTickerRepository(TCSService tcsService) throws Exception {
        AtomicReference<Map<TickerInfo.Key, TickerInfo>> tickerRegister =
                new AtomicReference<>(new HashMap<>());

        Callable<Boolean> isEmpty =
                () -> {
                    Map<TickerInfo.Key, TickerInfo> dataFromDisk =
                            loadDataFromDisk(SERIALIZE_NAME, new TypeToken<>() {});
                    if (null == dataFromDisk) {
                        return true;
                    }
                    tickerRegister.set(dataFromDisk);
                    return null == tickerRegister.get() || tickerRegister.get().isEmpty();
                };

        Callable<Boolean> isOld =
                () -> {
                    Date weekAgo = new Date(currentTimeMillis() - TimeUnit.DAYS.toMillis(7));
                    return getDateOfContentOnDisk(SERIALIZE_NAME).before(weekAgo);
                };

        if (isEmpty.call() || isOld.call()) {
            tickerRepository.putAll(tcsService.getCurrenciesList());
            tickerRepository.putAll(tcsService.getEtfList());
            tickerRepository.putAll(tcsService.getStockList());
            tickerRepository.putAll(tcsService.getBondList());
            tickerRepository.putAll(tcsService.getFuturesList());
            saveDataToDisk(SERIALIZE_NAME, tickerRepository.getAll());
        } else {
            tickerRepository.putAll(tickerRegister.get());
        }
    }
}
