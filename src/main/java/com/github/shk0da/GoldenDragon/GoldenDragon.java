package com.github.shk0da.GoldenDragon;

import com.github.shk0da.GoldenDragon.config.MainConfig;
import com.github.shk0da.GoldenDragon.config.MarketConfig;
import com.github.shk0da.GoldenDragon.config.RebalanceConfig;
import com.github.shk0da.GoldenDragon.model.Market;
import com.github.shk0da.GoldenDragon.model.TickerInfo;
import com.github.shk0da.GoldenDragon.repository.Repository;
import com.github.shk0da.GoldenDragon.repository.TickerRepository;
import com.github.shk0da.GoldenDragon.service.TCSService;
import com.github.shk0da.GoldenDragon.strategy.DivFlow;
import com.github.shk0da.GoldenDragon.strategy.RSX;
import com.github.shk0da.GoldenDragon.strategy.Rebalance;
import com.google.gson.reflect.TypeToken;

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
import static com.github.shk0da.GoldenDragon.utils.SerializationUtils.getDateOfContentOnDisk;
import static com.github.shk0da.GoldenDragon.utils.SerializationUtils.loadDataFromDisk;
import static com.github.shk0da.GoldenDragon.utils.SerializationUtils.saveDataToDisk;
import static java.lang.System.currentTimeMillis;
import static java.lang.System.out;

public class GoldenDragon {

    private static final Repository<TickerInfo.Key, TickerInfo> tickerRepository = TickerRepository.INSTANCE;

    public static void main(String[] args) {
        TimeZone.setDefault(TimeZone.getTimeZone("Europe/Moscow"));
        out.printf("%s: Start GoldenDragon%n", new Date().toString());

        try {
            String strategy = (args.length >= 1) ? args[0] : "Rebalance";
            Market market = (args.length >= 2) ? Market.valueOf(args[1]) : Market.MOEX;
            out.println("Run: " + strategy + " " + market.name());

            MainConfig mainConfig = new MainConfig();
            MarketConfig marketConfig = MarketConfig.byMarket(market);

            // Check Market hours
            GregorianCalendar currentCalendar = new GregorianCalendar();
            if (!mainConfig.isTestMode()) {
                if (!CALENDAR_WORK_DAYS.contains(currentCalendar.get(Calendar.DAY_OF_WEEK))) {
                    out.println("Not working day! Day of Week: " + currentCalendar.get(Calendar.DAY_OF_WEEK) + ". Exit...");
                    return;
                }
                int currentHour = currentCalendar.get(Calendar.HOUR_OF_DAY);
                if (currentHour < marketConfig.getStartWorkHour() || currentHour >= marketConfig.getEndWorkHour()) {
                    int currentMinute = currentCalendar.get(Calendar.MINUTE);
                    out.println("Not working hours! Current Time: " + currentHour + ":" + currentMinute + ". Exit...");
                    return;
                }
            }

            TCSService tcsService = new TCSService(mainConfig, marketConfig);
            updateTickerRepository(tcsService);

            // 1. Rebalance
            // CRON MOEX: Every Mon at 10:01 MSK
            if ("Rebalance".equals(strategy)) {
                final RebalanceConfig rebalanceConfig = new RebalanceConfig();
                new Rebalance(mainConfig, marketConfig, rebalanceConfig, tcsService).run();
            }

            // 2. RSX
            // CRON SPB: Every Mon at 17:00 MSK
            if ("RSX".equals(strategy)) {
                new RSX(mainConfig, marketConfig, tcsService).run();
            }

            // 3. DivFlow
            // CRON MOEX: Every Mon-Fri at 10:01 MSK
            // CRON SPB: Every Mon-Fri at 16:31 MSK
            if ("DivFlow".equals(strategy)) {
                new DivFlow(mainConfig, marketConfig, tcsService).run();
            }
        } catch (Exception ex) {
            out.printf("Error: %s%n", ex.getMessage());
            ex.printStackTrace();
        }
        out.printf("%s: Finish GoldenDragon%n", new Date().toString());
    }

    private static void updateTickerRepository(TCSService tcsService) throws Exception {
        AtomicReference<Map<TickerInfo.Key, TickerInfo>> tickerRegister = new AtomicReference<>(new HashMap<>());

        Callable<Boolean> isEmpty = () -> {
            Map<TickerInfo.Key, TickerInfo> dataFromDisk = loadDataFromDisk(TickerRepository.SERIALIZE_NAME, new TypeToken<>() {});
            if (null == dataFromDisk) {
                return true;
            }
            tickerRegister.set(dataFromDisk);
            return null == tickerRegister.get() || tickerRegister.get().isEmpty();
        };

        Callable<Boolean> isOld = () -> {
            Date weekAgo = new Date(currentTimeMillis() - TimeUnit.DAYS.toMillis(7));
            return getDateOfContentOnDisk(TickerRepository.SERIALIZE_NAME).before(weekAgo);
        };

        if (isEmpty.call() || isOld.call()) {
            tickerRepository.putAll(tcsService.getCurrenciesList());
            tickerRepository.putAll(tcsService.getEtfList());
            tickerRepository.putAll(tcsService.getStockList());
            tickerRepository.putAll(tcsService.getBondList());
            saveDataToDisk(TickerRepository.SERIALIZE_NAME, tickerRepository.getAll());
        } else {
            tickerRepository.putAll(tickerRegister.get());
        }
    }
}
