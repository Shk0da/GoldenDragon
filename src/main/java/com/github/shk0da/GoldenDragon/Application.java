package com.github.shk0da.GoldenDragon;

import com.github.shk0da.GoldenDragon.config.MainConfig;
import com.github.shk0da.GoldenDragon.model.Market;
import com.github.shk0da.GoldenDragon.repository.Repository;
import com.github.shk0da.GoldenDragon.repository.TickerRepository;
import com.github.shk0da.GoldenDragon.service.TCSService;
import com.github.shk0da.GoldenDragon.strategy.DivFlow;

import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Map;
import java.util.TimeZone;

import static com.github.shk0da.GoldenDragon.config.MainConfig.CALENDAR_WORK_DAYS;
import static java.lang.System.out;

public class Application {

    private static final Repository<String, Map<String, Object>> tickerRepository = TickerRepository.INSTANCE;

    public static void main(String[] args) {
        TimeZone.setDefault(TimeZone.getTimeZone("Europe/Moscow"));
        out.printf("%s: Start GoldenDragon%n", new Date().toString());

        try {
            Market market = Market.MOEX;
            if (args.length >= 1) {
                market = Market.valueOf(args[0]);
            }

            MainConfig mainConfig = new MainConfig(market);

            // Check Market hours
            GregorianCalendar currentCalendar = new GregorianCalendar();
            if (!mainConfig.isTestMode()) {
                if (!CALENDAR_WORK_DAYS.contains(currentCalendar.get(Calendar.DAY_OF_WEEK))) {
                    out.println("Not working day! Day of Week: " + currentCalendar.get(Calendar.DAY_OF_WEEK) + ". Exit...");
                    return;
                }
                int currentHour = currentCalendar.get(Calendar.HOUR_OF_DAY);
                if (currentHour < mainConfig.getMarketConfig().getStartWorkHour() || currentHour >=  mainConfig.getMarketConfig().getEndWorkHour()) {
                    int currentMinute = currentCalendar.get(Calendar.MINUTE);
                    out.println("Not working hours! Current Time: " + currentHour + ":" + currentMinute + ". Exit...");
                    return;
                }
            }

            TCSService tcsService = new TCSService(mainConfig);
            // Update Ticker Register tickerRegister
            tickerRepository.putAll(tcsService.getStockList());

            // 1. DivFlow
            new DivFlow(mainConfig, tcsService).run();

        } catch (Exception ex) {
            out.printf("Error: %s%n", ex.getMessage());
            ex.printStackTrace();
        }
        out.printf("%s: Finish GoldenDragon%n", new Date().toString());
    }
}
