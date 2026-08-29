package com.github.shk0da.goldendragon;

import com.github.shk0da.goldendragon.config.MainConfig;
import com.github.shk0da.goldendragon.model.TickerInfo;
import com.github.shk0da.goldendragon.repository.Repository;
import com.github.shk0da.goldendragon.repository.TickerRepository;
import com.github.shk0da.goldendragon.service.TCSService;
import com.github.shk0da.goldendragon.strategy.StrategyRegistry;
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

import static com.github.shk0da.goldendragon.repository.TickerRepository.SERIALIZE_NAME;
import static com.github.shk0da.goldendragon.utils.SerializationUtils.getDateOfContentOnDisk;
import static com.github.shk0da.goldendragon.utils.SerializationUtils.loadDataFromDisk;
import static com.github.shk0da.goldendragon.utils.SerializationUtils.saveDataToDisk;
import static com.github.shk0da.goldendragon.utils.TimeUtils.sleep;
import static java.lang.System.currentTimeMillis;
import static java.lang.System.out;
import static java.lang.System.setOut;
import static java.nio.charset.StandardCharsets.UTF_8;
import static java.util.TimeZone.setDefault;

/**
 * Main application entry point for GoldenDragon trading system. Supports multiple trading
 * strategies: LevelTrader, UnifiedStrategy, RSX, DivFlow, Rebalance, etc. Initializes market data,
 * ticker repository, and runs selected strategy.
 */
public final class GoldenDragon {

    private static final Repository<TickerInfo.Key, TickerInfo> tickerRepository = TickerRepository.INSTANCE;

    private static final int DEFAULT_ARG_INDEX = 0;
    private static final int ACCOUNT_ARG_INDEX = 1;
    private static final String DEFAULT_STRATEGY = "LevelTrader";
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
            final String accountId = getAccountId(args, mainConfig);
            out.println("Run: " + strategy + " [" + accountId + "]");

            final TCSService tcsService = new TCSService(mainConfig.withAccountId(accountId));
            updateTickerRepository(tcsService);

            executeStrategy(strategy, mainConfig, tcsService, args);
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

    private static String getAccountId(final String[] args, final MainConfig mainConfig) {
        return args.length > ACCOUNT_ARG_INDEX
                ? args[ACCOUNT_ARG_INDEX]
                : mainConfig.getTcsAccountId();
    }

    private static void executeStrategy(
            final String strategy,
            final MainConfig mainConfig,
            final TCSService tcsService,
            final String[] args)
            throws Exception {

        final StrategyRegistry.Entry entry = StrategyRegistry.get(strategy);
        if (entry == null) {
            out.println("Unknown strategy: " + strategy);
            return;
        }
        if (!entry.hasLiveRunner()) {
            out.println("Strategy has no live runner: " + strategy);
            return;
        }
        entry.runLive(mainConfig, tcsService, args);
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
