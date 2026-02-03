package com.github.shk0da.GoldenDragon.strategy;

import com.github.shk0da.GoldenDragon.config.TelegramAppConfig;
import com.github.shk0da.GoldenDragon.model.OrderAction;
import com.github.shk0da.GoldenDragon.model.OrderAction.Direct;
import com.github.shk0da.GoldenDragon.service.TCSService;
import com.github.shk0da.GoldenDragon.service.TelegramAppService;
import java.text.DecimalFormat;
import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.function.Function;
import java.util.regex.Pattern;


import static com.github.shk0da.GoldenDragon.service.TelegramNotifyService.telegramNotifyService;
import static com.github.shk0da.GoldenDragon.utils.TimeUtils.sleep;
import static java.lang.System.out;
import static java.time.OffsetDateTime.now;
import static java.util.Calendar.HOUR_OF_DAY;
import static java.util.concurrent.CompletableFuture.allOf;
import static java.util.concurrent.CompletableFuture.runAsync;

public class TelegramSignal {

    private static final DecimalFormat decimalFormat = new DecimalFormat("#.##");

    private final Map<String, OffsetDateTime> lastHandle = new ConcurrentHashMap<>();
    private final Map<String, Function<String, OrderAction>> channels = new TreeMap<>() {{
        // MSKINVESTOR
        put("-1001328534558", TelegramSignal::parseMskInvestorSignal);
        // VIP MSKINVESTOR
        put("-1001713323733", TelegramSignal::parseMskInvestorSignal);
    }};

    private final TCSService tcsService;
    private final TelegramAppService telegramAppService;

    public TelegramSignal(TelegramAppConfig telegramAppConfig, TCSService tcsService) {
        this.tcsService = tcsService;
        this.telegramAppService = new TelegramAppService(telegramAppConfig);
    }

    public void run() throws Exception {
        var initPortfolioCost = tcsService.getTotalPortfolioCost();
        var infoMessage = "Total Portfolio Cost: " + initPortfolioCost;
        telegramNotifyService.sendMessage(infoMessage);
        out.println(infoMessage);

        List<CompletableFuture<Void>> tasks = new ArrayList<>();
        ExecutorService executor = Executors.newFixedThreadPool(channels.size());
        for (Entry<String, Function<String, OrderAction>> channel : channels.entrySet()) {
            tasks.add(
                    runAsync(() -> {
                        while (isWorkingHours()) {
                            handleMessages(channel.getKey(), channel.getValue());
                            sleep(30_000);
                        }
                    }, executor)
            );
            sleep(5_000);
        }
        allOf(tasks.toArray(new CompletableFuture[]{})).join();

        if (!isWorkingHours()) {
            var profit = tcsService.getTotalPortfolioCost() - initPortfolioCost;
            var profitInPercents = (tcsService.getTotalPortfolioCost() - initPortfolioCost) / initPortfolioCost * 100;
            var statsMessage = "Day profit: " + decimalFormat.format(profit) + "₽ (" + decimalFormat.format(profitInPercents) + "%)";
            telegramNotifyService.sendMessage(statsMessage);
        }
    }

    private boolean isWorkingHours() {
        var calendar = new GregorianCalendar();
        var hour = calendar.get(HOUR_OF_DAY);
        return !(hour >= 22);
    }

    private void handleMessages(String channelId, Function<String, OrderAction> function) {
        OffsetDateTime endTime = now();
        OffsetDateTime startTime = lastHandle.getOrDefault(channelId, endTime.minusMinutes(2));
        List<String> messages = telegramAppService.getMessages(channelId, startTime, endTime);
        messages.forEach(message -> {
            var signal = function.apply(message);
            if (null != signal) {
                out.println(signal);
                telegramNotifyService.sendMessage(signal.toString());
            }
        });
        lastHandle.put(channelId, endTime);
    }

    public static OrderAction parseMskInvestorSignal(String text) {
        if (text.contains("Закроем") || text.contains("закроем")) {
            var closePattern = Pattern.compile("([A-Z]{2,6})");
            var closeMatcher = closePattern.matcher(text);
            if (closeMatcher.find()) {
                return new OrderAction(closeMatcher.group(1), Direct.CLOSE);
            }
        }

        var buySellPattern = Pattern.compile("(Покупка|Продажа)\\s+([A-Z]{2,6})");
        var bsMatcher = buySellPattern.matcher(text);
        if (bsMatcher.find()) {
            String ticker = bsMatcher.group(2);
            Direct action = bsMatcher.group(1).equals("Покупка") ? Direct.BUY : Direct.SELL;

            var pricePattern = Pattern.compile("Цена[:\\s]*([\\d,]+)");
            var priceMatcher = pricePattern.matcher(text);
            Double entryPrice = null;
            if (priceMatcher.find()) {
                String priceStr = priceMatcher.group(1).replace(',', '.');
                entryPrice = Double.parseDouble(priceStr);
            }

            String sl = null;
            var stopPattern = Pattern.compile("Стоп[:\\s]*([\\d,]+)");
            var stopMatcher = stopPattern.matcher(text);
            if (stopMatcher.find()) {
                sl = stopMatcher.group(1).replace(',', '.');
            }

            String tp = null;
            var tpPattern = Pattern.compile("Выход[:\\s]*([\\d\\-%]+)");
            var tpMatcher = tpPattern.matcher(text);
            if (tpMatcher.find()) {
                tp = tpMatcher.group(1);
                Double avgTp = parseTakeProfit(tpMatcher.group(1));
                if (avgTp != null) {
                    if (entryPrice != null) {
                        double tpPrice = entryPrice * (1 + avgTp / 100.0);
                        if (action == Direct.SELL) {
                            tpPrice = entryPrice * (1 - avgTp / 100.0);
                        }
                        tp = String.format("%.4f", tpPrice);
                    } else {
                        tp = String.format("%.0f", avgTp) + "%";
                    }
                }
            }

            return new OrderAction(ticker, action, sl, tp);
        }

        return null;
    }

    private static Double parseTakeProfit(String input) {
        var tp = input.replaceAll("[^\\d,-]", "");
        String[] parts = tp.split("-");
        try {
            if (parts.length == 2) {
                double from = Double.parseDouble(parts[0].replace(',', '.'));
                double to = Double.parseDouble(parts[1].replace(',', '.'));
                return (from + to) / 2.0;
            } else if (parts.length == 1) {
                return Double.parseDouble(parts[0].replace(',', '.'));
            }
        } catch (NumberFormatException e) {
            System.err.println("Failed parse TP: " + tp);
        }
        return null;
    }
}
