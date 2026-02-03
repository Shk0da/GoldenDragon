package com.github.shk0da.GoldenDragon;

import com.github.shk0da.GoldenDragon.config.AILConfig;
import com.github.shk0da.GoldenDragon.config.DataCollectorConfig;
import com.github.shk0da.GoldenDragon.config.MainConfig;
import com.github.shk0da.GoldenDragon.config.MarketConfig;
import com.github.shk0da.GoldenDragon.config.RSXConfig;
import com.github.shk0da.GoldenDragon.config.RebalanceConfig;
import com.github.shk0da.GoldenDragon.config.TelegramAppConfig;
import com.github.shk0da.GoldenDragon.model.Market;
import com.github.shk0da.GoldenDragon.model.TickerInfo;
import com.github.shk0da.GoldenDragon.repository.Repository;
import com.github.shk0da.GoldenDragon.repository.TickerRepository;
import com.github.shk0da.GoldenDragon.service.TCSService;
import com.github.shk0da.GoldenDragon.strategy.AITrader;
import com.github.shk0da.GoldenDragon.strategy.DataCollector;
import com.github.shk0da.GoldenDragon.strategy.DataLearning;
import com.github.shk0da.GoldenDragon.strategy.DivFlow;
import com.github.shk0da.GoldenDragon.strategy.IndicatorTrader;
import com.github.shk0da.GoldenDragon.strategy.RSX;
import com.github.shk0da.GoldenDragon.strategy.Rebalance;
import com.github.shk0da.GoldenDragon.strategy.TelegramAuth;
import com.github.shk0da.GoldenDragon.strategy.TelegramSignal;
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

            // 1. Rebalance
            if ("Rebalance".equals(strategy)) {
                final RebalanceConfig rebalanceConfig = new RebalanceConfig();
                new Rebalance(mainConfig, marketConfig, rebalanceConfig, tcsService).run();
            }

            // 2. RSX
            if ("RSX".equals(strategy)) {
                final RSXConfig rsxConfig = new RSXConfig();
                new RSX(mainConfig, marketConfig, rsxConfig, tcsService).run();
            }

            // 3. DivFlow
            if ("DivFlow".equals(strategy)) {
                new DivFlow(mainConfig, marketConfig, tcsService).run();
            }

            // 4. IndicatorTrader
            if ("IndicatorTrader".equals(strategy)) {
                new IndicatorTrader(tcsService).run();
            }

            // 5. DataCollector
            if ("DataCollector".equals(strategy)) {
                telegramNotifyService.sendMessage("Run DataCollector");
                DataCollectorConfig dataCollectorConfig = new DataCollectorConfig();
                new DataCollector(dataCollectorConfig, tcsService).run();
                telegramNotifyService.sendMessage("End DataCollector");
            }

            // 6. AICollector
            if ("AICollector".equals(strategy)) {
                telegramNotifyService.sendMessage("Start AICollector");
                telegramNotifyService.sendMessage("Run DataCollector");
                DataCollectorConfig dataCollectorConfig = new DataCollectorConfig();
                new DataCollector(dataCollectorConfig, tcsService).run();
                telegramNotifyService.sendMessage("End DataCollector");
                telegramNotifyService.sendMessage("Run DataLearning");
                AILConfig ailConfig = new AILConfig();
                new DataLearning(ailConfig).run();
                telegramNotifyService.sendMessage("End DataLearning");
                telegramNotifyService.sendMessage("End AICollector");
            }

            // 7. AITrader
            if ("AITrader".equals(strategy)) {
                telegramNotifyService.sendMessage("Run AITrader");
                AILConfig ailConfig = new AILConfig();
                new AITrader(ailConfig, tcsService).run();
                telegramNotifyService.sendMessage("Stop AITrader");
            }

            // 8. TelegramAuth
            if ("TelegramAuth".equals(strategy)) {
                telegramNotifyService.sendMessage("Run TelegramAuth");
                TelegramAppConfig telegramAppConfig = new TelegramAppConfig();
                new TelegramAuth(telegramAppConfig).run();
                telegramNotifyService.sendMessage("Stop TelegramAuth");
            }

            // 9. TelegramSignal
            if ("TelegramSignal".equals(strategy)) {
                telegramNotifyService.sendMessage("Run TelegramSignal");
                TelegramAppConfig telegramAppConfig = new TelegramAppConfig();
                new TelegramSignal(telegramAppConfig, tcsService).run();
                telegramNotifyService.sendMessage("Stop TelegramSignal");
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
