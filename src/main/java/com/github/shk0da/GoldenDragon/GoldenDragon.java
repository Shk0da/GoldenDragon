package com.github.shk0da.GoldenDragon;

import com.github.shk0da.GoldenDragon.config.DataCollectorConfig;
import com.github.shk0da.GoldenDragon.config.MainConfig;
import com.github.shk0da.GoldenDragon.config.MarketConfig;
import com.github.shk0da.GoldenDragon.config.UnifiedTraderConfig;
import com.github.shk0da.GoldenDragon.model.Market;
import com.github.shk0da.GoldenDragon.model.TickerInfo;
import com.github.shk0da.GoldenDragon.repository.Repository;
import com.github.shk0da.GoldenDragon.repository.TickerRepository;
import com.github.shk0da.GoldenDragon.service.TCSService;
import com.github.shk0da.GoldenDragon.strategy.DataCollector;
import com.github.shk0da.GoldenDragon.strategy.GerchikStrategy;
import com.github.shk0da.GoldenDragon.strategy.RegimeAwareStrategy;
import com.github.shk0da.GoldenDragon.strategy.RegimeAwareStrategyMl;
import com.github.shk0da.GoldenDragon.strategy.TurtleStrategy;
import com.github.shk0da.GoldenDragon.strategy.UnifiedStrategy;
import com.google.gson.reflect.TypeToken;
import java.io.FileDescriptor;
import java.io.FileOutputStream;
import java.io.PrintStream;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.Map;
import java.util.TimeZone;
import java.util.concurrent.Callable;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;


import static com.github.shk0da.GoldenDragon.config.MainConfig.CALENDAR_WORK_DAYS;
import static com.github.shk0da.GoldenDragon.repository.TickerRepository.SERIALIZE_NAME;
import static com.github.shk0da.GoldenDragon.service.TelegramNotifyService.telegramNotifyService;
import static com.github.shk0da.GoldenDragon.utils.SerializationUtils.getDateOfContentOnDisk;
import static com.github.shk0da.GoldenDragon.utils.SerializationUtils.loadDataFromDisk;
import static com.github.shk0da.GoldenDragon.utils.SerializationUtils.saveDataToDisk;
import static com.github.shk0da.GoldenDragon.utils.TimeUtils.sleep;
import static java.lang.System.currentTimeMillis;
import static java.lang.System.out;
import static java.lang.System.setOut;
import static java.nio.charset.StandardCharsets.UTF_8;
import static java.util.TimeZone.setDefault;

public class GoldenDragon {

    private static final Repository<TickerInfo.Key, TickerInfo> tickerRepository = TickerRepository.INSTANCE;

    public static void main(String[] args) {
        setDefault(TimeZone.getTimeZone("Europe/Moscow"));
        setOut(new PrintStream(new FileOutputStream(FileDescriptor.out), true, UTF_8));
        out.printf("%s: Start GoldenDragon%n", new Date());

        try {
            MainConfig mainConfig = new MainConfig();
            String strategy = (args.length >= 1) ? args[0] : "AITrader";
            Market market = (args.length >= 2) ? Market.valueOf(args[1]) : Market.MOEX;
            String accountId = (args.length >= 3) ? args[2] : mainConfig.getTcsAccountId();
            out.println("Run: " + strategy + " " + market.name() + " [" + accountId + "]");

            MarketConfig marketConfig = MarketConfig.byMarket(market);

            // Check Market hours
            GregorianCalendar currentCalendar = new GregorianCalendar();
            if (!mainConfig.isTestMode()) {
                if (!CALENDAR_WORK_DAYS.contains(currentCalendar.get(Calendar.DAY_OF_WEEK))) {
                    out.println("Not working day! Day of Week: " + currentCalendar.get(Calendar.DAY_OF_WEEK) + ".");
                }
                int currentHour = currentCalendar.get(Calendar.HOUR_OF_DAY);
                if (currentHour < marketConfig.getStartWorkHour() || currentHour >= marketConfig.getEndWorkHour()) {
                    out.println("Not working hours! Current Time: " + new Date() + ".");
                }
            }

            TCSService tcsService = new TCSService(mainConfig.withAccountId(accountId), marketConfig);
            updateTickerRepository(tcsService);

            // 1. Rebalance - DISABLED (class not found)
            if ("Rebalance".equals(strategy)) {
                // final RebalanceConfig rebalanceConfig = new RebalanceConfig();
                // new Rebalance(marketConfig, rebalanceConfig, tcsService).run();
                System.out.println("Rebalance strategy is disabled");
            }

            // 2. RSX - DISABLED (class not found)
            if ("RSX".equals(strategy)) {
                // final RSXConfig rsxConfig = new RSXConfig();
                // new RSX(mainConfig, marketConfig, rsxConfig, tcsService).run();
                System.out.println("RSX strategy is disabled");
            }

            // 3. DivFlow - DISABLED (class not found)
            if ("DivFlow".equals(strategy)) {
                // new DivFlow(mainConfig, marketConfig, tcsService).run();
                System.out.println("DivFlow strategy is disabled");
            }

            // 4. IndicatorTrader - DISABLED (class not found)
            if ("IndicatorTrader".equals(strategy)) {
                // new IndicatorTrader(tcsService).run();
                System.out.println("IndicatorTrader is disabled");
            }

            // 5. DataCollector
            if ("DataCollector".equals(strategy)) {
                telegramNotifyService.sendMessage("Run DataCollector");
                DataCollectorConfig dataCollectorConfig = new DataCollectorConfig();
                new DataCollector(dataCollectorConfig, tcsService).run();
                telegramNotifyService.sendMessage("End DataCollector");
            }

            // 6.LevelTrader - DISABLED (class not found)
            if ("LevelTrader".equals(strategy)) {
                // telegramNotifyService.sendMessage("Run LevelTrader");
                // LevelTraderConfig levelTraderConfig = new LevelTraderConfig();
                // new LevelTrader(levelTraderConfig, tcsService).run();
                // telegramNotifyService.sendMessage("Stop LevelTrader");
                System.out.println("LevelTrader is disabled");
            }

            // 7. UnifiedStrategy
            if ("UnifiedStrategy".equals(strategy)) {
                telegramNotifyService.sendMessage("Run UnifiedStrategy");
                UnifiedTraderConfig unifiedTraderConfig = new UnifiedTraderConfig();
                new UnifiedStrategy(unifiedTraderConfig, tcsService).run();
                telegramNotifyService.sendMessage("Stop UnifiedStrategy");
            }

            // 8. TurtleStrategy
            if ("TurtleStrategy".equals(strategy)) {
                telegramNotifyService.sendMessage("Run TurtleStrategy");
                UnifiedTraderConfig unifiedTraderConfig = new UnifiedTraderConfig();
                new TurtleStrategy(unifiedTraderConfig, tcsService).run();
                telegramNotifyService.sendMessage("Stop TurtleStrategy");
            }

            // 9. GerchikStrategy
            if ("GerchikStrategy".equals(strategy)) {
                telegramNotifyService.sendMessage("Run GerchikStrategy");
                UnifiedTraderConfig unifiedTraderConfig = new UnifiedTraderConfig();
                new GerchikStrategy(unifiedTraderConfig, tcsService).run();
                telegramNotifyService.sendMessage("Stop GerchikStrategy");
            }

            // 10. RegimeAwareStrategy
            if ("RegimeAwareStrategy".equals(strategy)) {
                telegramNotifyService.sendMessage("Run RegimeAwareStrategy (auto-switch by market regime)");
                UnifiedTraderConfig unifiedTraderConfig = new UnifiedTraderConfig();
                new RegimeAwareStrategy(unifiedTraderConfig, tcsService).run();
                telegramNotifyService.sendMessage("Stop RegimeAwareStrategy");
            }

            // 11. RegimeAwareStrategyMl
            if ("RegimeAwareStrategyMl".equals(strategy)) {
                telegramNotifyService.sendMessage("Run RegimeAwareStrategyMl");
                UnifiedTraderConfig unifiedTraderConfig = new UnifiedTraderConfig();
                new RegimeAwareStrategyMl(unifiedTraderConfig, tcsService).run();
                telegramNotifyService.sendMessage("Stop RegimeAwareStrategyMl");
            }
        } catch (Exception ex) {
            out.printf("Error: %s%n", ex.getMessage());
            ex.printStackTrace();
        }
        out.printf("%s: Finish GoldenDragon%n", new Date());
        sleep(5_000);
        System.exit(0);
    }

    private static void updateTickerRepository(TCSService tcsService) throws Exception {
        AtomicReference<Map<TickerInfo.Key, TickerInfo>> tickerRegister = new AtomicReference<>(new HashMap<>());

        Callable<Boolean> isEmpty = () -> {
            Map<TickerInfo.Key, TickerInfo> dataFromDisk = loadDataFromDisk(SERIALIZE_NAME, new TypeToken<>() {
            });
            if (null == dataFromDisk) {
                return true;
            }
            tickerRegister.set(dataFromDisk);
            return null == tickerRegister.get() || tickerRegister.get().isEmpty();
        };

        Callable<Boolean> isOld = () -> {
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
