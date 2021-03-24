package com.github.shk0da.GoldenDragon.strategy;

import com.github.shk0da.GoldenDragon.config.MainConfig;
import com.github.shk0da.GoldenDragon.config.MarketConfig;
import com.github.shk0da.GoldenDragon.config.RSXConfig;
import com.github.shk0da.GoldenDragon.model.DiviTicker;
import com.github.shk0da.GoldenDragon.model.PositionInfo;
import com.github.shk0da.GoldenDragon.model.TickerInfo;
import com.github.shk0da.GoldenDragon.model.TickerScan;
import com.github.shk0da.GoldenDragon.repository.Repository;
import com.github.shk0da.GoldenDragon.repository.TickerRepository;
import com.github.shk0da.GoldenDragon.service.TCSService;
import com.github.shk0da.GoldenDragon.service.TradingViewService;
import com.github.shk0da.GoldenDragon.service.YahooService;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import java.net.URI;
import java.net.URLEncoder;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import static com.github.shk0da.GoldenDragon.config.DivFlowConfig.DOHOD_DIV_CALENDAR;
import static com.github.shk0da.GoldenDragon.config.DivFlowConfig.INVESTING_DIV_CALENDAR;
import static com.github.shk0da.GoldenDragon.config.DivFlowConfig.SMART_LAB_DIV_CALENDAR;
import static com.github.shk0da.GoldenDragon.config.MainConfig.CALENDAR_WORK_DAYS;
import static com.github.shk0da.GoldenDragon.config.MainConfig.HEADER_USER_AGENT;
import static com.github.shk0da.GoldenDragon.config.MainConfig.USER_AGENT;
import static com.github.shk0da.GoldenDragon.config.MainConfig.dateFormat;
import static com.github.shk0da.GoldenDragon.config.MainConfig.dateFormatUs;
import static com.github.shk0da.GoldenDragon.config.MainConfig.httpClient;
import static com.github.shk0da.GoldenDragon.model.Market.DE;
import static com.github.shk0da.GoldenDragon.model.Market.MOEX;
import static com.github.shk0da.GoldenDragon.model.Market.US;
import static com.github.shk0da.GoldenDragon.model.TickerType.STOCK;
import static com.github.shk0da.GoldenDragon.utils.PredicateUtils.distinctByKey;
import static com.github.shk0da.GoldenDragon.utils.PrintUtils.printCalendarOfDividends;
import static com.github.shk0da.GoldenDragon.utils.PrintUtils.printCurrentPositions;
import static com.github.shk0da.GoldenDragon.utils.RequestUtils.requestWithRetry;
import static java.lang.Math.min;
import static java.lang.String.valueOf;
import static java.lang.System.currentTimeMillis;
import static java.lang.System.out;
import static java.nio.charset.StandardCharsets.UTF_8;
import static java.util.Map.entry;
import static java.util.concurrent.TimeUnit.DAYS;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;

/**
 * CRON MOEX: Every Mon-Fri at 10:01 MSK
 * CRON SPB: Every Mon-Fri at 16:31 MSK
 */
public class DivFlow {

    private final MainConfig mainConfig;
    private final MarketConfig marketConfig;

    private final TCSService tcsService;

    private final YahooService yahooService = YahooService.INSTANCE;
    private final TradingViewService tradingViewService = TradingViewService.INSTANCE;
    private final Repository<TickerInfo.Key, TickerInfo> tickerRepository = TickerRepository.INSTANCE;

    public DivFlow(MainConfig mainConfig, MarketConfig marketConfig, TCSService tcsService) {
        this.mainConfig = mainConfig;
        this.marketConfig = marketConfig;
        this.tcsService = tcsService;
    }

    public void run() {
        double cash = tcsService.getAvailableCash();
        Map<TickerInfo.Key, PositionInfo> currentPositions = tcsService.getCurrentPositions(STOCK);
        printCurrentPositions(marketConfig.getCurrency(), currentPositions, cash, tcsService);

        GregorianCalendar currentCalendar = new GregorianCalendar();
        // Download Dividends calendar
        List<String> tickersForScan = new ArrayList<>();
        Map<String, List<DiviTicker>> dividends = getCalendarDividends()
                .entrySet()
                .stream()
                .peek(it -> {
                    it.setValue(it.getValue().stream()
                            .filter(value -> tickerRepository.containsKey(new TickerInfo.Key(value.getTickerCode(), STOCK)))
                            .filter(value -> {
                                boolean isPast;
                                boolean isToDay;
                                boolean isFeature = false;
                                try {
                                    Date valueDate = dateFormat.parse(value.getCloseDate());
                                    isPast = valueDate.before(currentCalendar.getTime());
                                    isToDay = valueDate.equals(currentCalendar.getTime());
                                    isFeature = valueDate.after(currentCalendar.getTime());
                                } catch (Exception nothing) {
                                    isPast = true;
                                    isToDay = true;
                                }

                                boolean isHolding = currentPositions.containsKey(new TickerInfo.Key(value.getTickerCode(), STOCK));

                                if ((isToDay || isPast) && isHolding) return true;

                                return isFeature && (isHolding || (value.getPercent() >= 1.0 && value.getPercent() <= 10.0));
                            })
                            .filter(distinctByKey(DiviTicker::getTickerCode))
                            .distinct()
                            .collect(toList()));
                    try {
                        // is feature && not holding
                        if (dateFormat.parse(it.getKey()).after(currentCalendar.getTime())) {
                            tickersForScan.addAll(it.getValue()
                                    .stream()
                                    .map(DiviTicker::getTickerCode)
                                    .filter(ticker -> !currentPositions.containsKey(new TickerInfo.Key(ticker, STOCK)))
                                    .collect(toList())
                            );
                        }
                    } catch (Exception ex) {
                        out.println("Failed collect tickers to scan: " + ex.getMessage());
                    }
                })
                .filter(it -> !it.getValue().isEmpty())
                .collect(toMap(Map.Entry::getKey, Map.Entry::getValue, (o1, o2) -> o1, LinkedHashMap::new));

        if (!tickersForScan.isEmpty()) {
            List<String> afterScan = scanSymbols(tickersForScan);
            dividends = dividends
                    .entrySet()
                    .stream()
                    .peek(it -> {
                        try {
                            // is feature
                            if (dateFormat.parse(it.getKey()).after(currentCalendar.getTime())) {
                                it.setValue(it.getValue().stream()
                                        .filter(value -> afterScan.contains(value.getTickerCode())
                                                || currentPositions.containsKey(new TickerInfo.Key(value.getTickerCode(), STOCK)))
                                        .collect(toList()));
                            }
                        } catch (Exception ex) {
                            out.println("Failed scan tickers: " + ex.getMessage());
                        }
                    })
                    .collect(toMap(Map.Entry::getKey, Map.Entry::getValue, (o1, o2) -> o1, LinkedHashMap::new));
        }

        printCalendarOfDividends(dividends);

        GregorianCalendar sellCalendar = new GregorianCalendar();
        sellCalendar.add(Calendar.DATE, -3);

        GregorianCalendar buyCalendar = new GregorianCalendar();
        buyCalendar.setTime(new Date(currentCalendar.getTime().getTime() + DAYS.toMillis(10)));
        while (!CALENDAR_WORK_DAYS.contains(buyCalendar.get(Calendar.DAY_OF_WEEK))) {
            buyCalendar.add(Calendar.DATE, 1);
        }

        boolean isPortfolioCostChanged = false;
        String currentDate = dateFormat.format(currentCalendar.getTime());

        // sell
        List<DiviTicker> tickersToSell = new ArrayList<>(10);
        for (int i = 0; i < 3; i++) {
            String sellDate = dateFormat.format(sellCalendar.getTime());
            List<DiviTicker> tickers = dividends.get(sellDate);
            if (null != tickers) {
                tickersToSell.addAll(tickers);
            }
            sellCalendar.add(Calendar.DATE, 1);
        }
        if (!tickersToSell.isEmpty()) {
            for (DiviTicker diviTicker : tickersToSell) {
                if (!currentPositions.containsKey(new TickerInfo.Key(diviTicker.getTickerCode(), STOCK))) {
                    continue;
                }
                try {
                    preparingSale(currentDate, diviTicker, currentPositions.get(new TickerInfo.Key(diviTicker.getTickerCode(), STOCK)));
                    isPortfolioCostChanged = true;
                    out.println();
                } catch (Exception ex) {
                    out.println("Failed preparing sale: " + ex.getMessage());
                }
            }
        }

        // buy
        List<DiviTicker> tickersToBuy = new ArrayList<>(10);
        for (int i = 0; i < 7; i++) {
            String buyDate = dateFormat.format(buyCalendar.getTime());
            List<DiviTicker> tickers = dividends.get(buyDate);
            if (null != tickers) {
                for (DiviTicker ticker : tickers) {
                    if (currentPositions.containsKey(new TickerInfo.Key(ticker.getTickerCode(), STOCK))) {
                        continue;
                    }
                    tickersToBuy.add(ticker);
                }
            }
            buyCalendar.add(Calendar.DATE, 1);
        }
        if (isPortfolioCostChanged && !tickersToBuy.isEmpty()) {
            cash = tcsService.getAvailableCash();
        }
        if (!tickersToBuy.isEmpty() && cash > 0.0) {
            int sizeTickersToBuy = tickersToBuy.size();
            for (int i = tickersToBuy.size() - 1; i >= 0; i--) {
                DiviTicker diviTicker = tickersToBuy.get(i);
                try {
                    double availableCashToBuy = min(marketConfig.getMaxPositionCostToBuy(), (cash * .8) / sizeTickersToBuy);
                    double cost = preparingPurchase(currentDate, diviTicker, availableCashToBuy);
                    if (cost > 0.0) {
                        cash = cash - cost;
                        isPortfolioCostChanged = true;
                    }
                    sizeTickersToBuy = sizeTickersToBuy - 1;
                    out.println();
                } catch (Exception ex) {
                    out.println("Failed preparing purchase: " + ex.getMessage());
                }
            }
        }

        if (isPortfolioCostChanged) {
            printCurrentPositions(marketConfig.getCurrency(), tcsService.getCurrentPositions(STOCK), tcsService.getAvailableCash(), tcsService);
        }
    }

    private double preparingSale(String currentDate, DiviTicker diviTicker, PositionInfo currentPosition) {
        var key = new TickerInfo.Key(diviTicker.getTickerCode(), STOCK);

        int lot = tcsService.searchTicker(key).getLot();
        int count = currentPosition.getBalance();
        while (count % lot != 0 && count > 0) {
            count = count - 1;
        }
        if (count == 0) {
            out.println("Warn: sale will be skipped - " + diviTicker.getTickerCode() + " with count " + count);
            return 0.0;
        }

        double tickerPrice = tcsService.getAvailablePrice(key, count, true);
        if (0.0 == tickerPrice) {
            out.println("Warn: sale will be used Market Price - " + diviTicker.getTickerCode());
        }

        double cost = count * tickerPrice;
        if (1 == sell(currentDate, key, tickerPrice, count, cost)) {
            return cost;
        } else {
            return 0.0;
        }
    }

    private double preparingPurchase(String currentDate, DiviTicker diviTicker, double availableCashToBuy) {
        int value = 0;
        double tickerPrice = 0.0;
        var key = new TickerInfo.Key(diviTicker.getTickerCode(), STOCK);
        for (Map.Entry<Double, Integer> ask : tcsService.getCurrentPrices(key).get("asks").entrySet()) {
            tickerPrice = ask.getKey();
            value = value + ask.getValue();
            if (value >= (availableCashToBuy / tickerPrice)) break;
        }

        if (0.0 == tickerPrice) {
            out.println("Warn: purchase will be skipped - " + diviTicker.getTickerCode() + " by price " + tickerPrice);
            return 0.0;
        }

        int lot = tcsService.searchTicker(key).getLot();
        int count = (int) (availableCashToBuy / tickerPrice);
        while (count % lot != 0 && count > 0) {
            count = count - 1;
        }
        if (count == 0) {
            out.println("Warn: purchase will be skipped - " + diviTicker.getTickerCode() + " with count " + count);
            return 0.0;
        }
        double cost = count * tickerPrice;
        if (1 == buy(currentDate, key, tickerPrice, count, cost)) {
            return cost;
        } else {
            return 0.0;
        }
    }

    private int sell(String currentDate, TickerInfo.Key key, double tickerPrice, int count, double cost) {
        out.println("[" + currentDate + "] Sell: " + count + " " + key.getTicker() + " by " + tickerPrice + " (" + cost + ")");
        if (mainConfig.isTestMode()) {
            return 1;
        }
        return tcsService.createOrder(key, tickerPrice, count, "Sell");
    }

    private int buy(String currentDate, TickerInfo.Key key, double tickerPrice, int count, double cost) {
        out.println("[" + currentDate + "] Buy: " + count + " " + key.getTicker() + " by " + tickerPrice + " (" + cost + ")");
        if (mainConfig.isTestMode()) {
            return 1;
        }
        return tcsService.createOrder(key, tickerPrice, count, "Buy");
    }

    private Map<String, List<DiviTicker>> getCalendarDividends() {
        Map<String, List<DiviTicker>> result = new LinkedHashMap<>();

        if (MOEX == marketConfig.getMarket()) {
            try {
                mergeCalendars(result, getSmartLabDividends());
            } catch (Exception ex) {
                out.println("Failed getSmartLabDividends: " + ex.getMessage());
            }
            try {
                mergeCalendars(result, getDohodDividends());
            } catch (Exception ex) {
                out.println("Failed getDohodDividends: " + ex.getMessage());
            }
        }

        try {
            mergeCalendars(result, getInvestingDividends());
        } catch (Exception ex) {
            out.println("Failed getInvestingDividends: " + ex.getMessage());
        }

        return result.entrySet().stream()
                .sorted((o1, o2) -> {
                    try {
                        var date1 = dateFormat.parse(o1.getKey());
                        var date2 = dateFormat.parse(o2.getKey());
                        return date2.compareTo(date1);
                    } catch (ParseException ex) {
                        out.println("failed sort CalendarDividends: " + ex.getMessage());
                        return 0;
                    }
                })
                .collect(toMap(Map.Entry::getKey, Map.Entry::getValue, (o1, o2) -> o1, LinkedHashMap::new));
    }

    private Map<String, List<DiviTicker>> getSmartLabDividends() {
        out.println("Loading dividends from SmartLab...");

        HttpResponse<String> response = requestWithRetry(() -> {
            HttpRequest request = HttpRequest.newBuilder()
                    .GET()
                    .uri(URI.create(SMART_LAB_DIV_CALENDAR))
                    .setHeader(HEADER_USER_AGENT, USER_AGENT)
                    .build();
            try {
                return MainConfig.httpClient.send(request, HttpResponse.BodyHandlers.ofString());
            } catch (Exception ex) {
                out.println("Error: " + ex.getMessage());
                return null;
            }
        });

        Document html = Jsoup.parse(response.body());
        Elements dividendsApproved = html
                .select("table.trades-table")
                .first()
                .select("tr.dividend_approved");

        Map<String, List<DiviTicker>> result = new LinkedHashMap<>();
        for (Element item : dividendsApproved) {
            try {
                var closeDate = item.select("td:eq(9)").text();
                if (null == closeDate || closeDate.isBlank()) continue;

                var code = item.select("td:eq(1)").text();
                if (null == code || code.isBlank()) continue;
                code = code.toUpperCase();

                var dividend = item.select("td:eq(5)").text();
                if (null == dividend || dividend.isBlank()) continue;

                var profit = item.select("td:eq(7)").text();
                if (null == profit || profit.isBlank()) continue;

                var name = item.select("td:eq(0) > a").text();
                var lastDate = item.select("td:eq(8)").text();
                var price = item.select("td:eq(6)").text();
                DiviTicker ticker = new DiviTicker(name, code, lastDate, closeDate, dividend, profit, price);

                List<DiviTicker> byDate = result.getOrDefault(ticker.getCloseDate(), new ArrayList<>(4));
                byDate.add(ticker);
                byDate.sort((c1, c2) -> c2.getPercent().compareTo(c1.getPercent()));
                result.put(ticker.getCloseDate(), byDate);
            } catch (Exception ex) {
                out.println("Error: Failed parse DiviTicker: " + ex.getMessage());
            }
        }
        return result;
    }

    private Map<String, List<DiviTicker>> getDohodDividends() {
        out.println("Loading dividends from Dohod...");

        HttpResponse<String> response = requestWithRetry(() -> {
            HttpRequest request = HttpRequest.newBuilder()
                    .GET()
                    .uri(URI.create(DOHOD_DIV_CALENDAR))
                    .setHeader(HEADER_USER_AGENT, USER_AGENT)
                    .build();
            try {
                return httpClient.send(request, HttpResponse.BodyHandlers.ofString());
            } catch (Exception ex) {
                out.println("Error: " + ex.getMessage());
                return null;
            }
        });

        Document html = Jsoup.parse(response.body());
        Elements dividends = html
                .getElementById("table-dividend")
                .select("tbody")
                .first()
                .select("tr");

        Map<String, List<DiviTicker>> result = new LinkedHashMap<>();
        for (Element item : dividends) {
            try {
                var closeDate = item.select("td:eq(8)").text();
                if (null == closeDate || closeDate.isBlank() || "n/a".equals(closeDate)) continue;
                var closeByDate = dateFormat.parse(closeDate);

                var dividend = item.select("td:eq(3)").text();
                if (null == dividend || dividend.isBlank()) continue;

                var profit = item.select("td:eq(6)").text();
                if (null == profit || profit.isBlank()) continue;

                var isRecommended = !item.select("td:eq(4) > span").isEmpty();
                if (!isRecommended && closeByDate.getTime() > (currentTimeMillis() + DAYS.toMillis(1))) continue;

                var itemUrl = item.select("td:eq(0) > a").attr("href").split("/");
                if (itemUrl.length == 0) continue;
                var code = itemUrl[itemUrl.length - 1];
                if (null == code || code.isBlank()) continue;
                code = code.toUpperCase();

                var name = item.select("td:eq(0) > a").text();
                var lastDate = dateFormat.format(new Date(closeByDate.getTime() - DAYS.toMillis(2)));
                var price = item.select("td:eq(7)").text();
                DiviTicker ticker = new DiviTicker(name, code, lastDate, closeDate, dividend, profit, price);

                List<DiviTicker> byDate = result.getOrDefault(ticker.getCloseDate(), new ArrayList<>(4));
                byDate.add(ticker);
                byDate.sort((c1, c2) -> c2.getPercent().compareTo(c1.getPercent()));
                result.put(ticker.getCloseDate(), byDate);
            } catch (Exception ex) {
                out.println("Error: Failed parse DiviTicker: " + ex.getMessage());
            }
        }
        return result;
    }

    private Map<String, List<DiviTicker>> getInvestingDividends() {
        out.println("Loading dividends from Investing...");

        Calendar calendar = new GregorianCalendar();
        if (US == marketConfig.getMarket()) {
            calendar.add(Calendar.DATE, -7);
        }
        String dateFrom = dateFormatUs.format(calendar.getTime());
        calendar.add(Calendar.DATE, 21);
        String dateTo = dateFormatUs.format(calendar.getTime());

        List<Element> dividends = new ArrayList<>();
        long lastTimeScope = currentTimeMillis() / 1000L;
        for (int i = 0; i < 10; i++) {
            List<Map.Entry<String, String>> parameters = new ArrayList<>(9);
            switch (marketConfig.getMarket()) {
                case US:
                    parameters.add(entry("country[]", "5"));
                    break;
                case DE:
                    parameters.add(entry("country[]", "17"));
                    break;
                case MOEX:
                default:
                    parameters.add(entry("country[]", "56"));
                    break;
            }
            parameters.add(entry("dateFrom", dateFrom));
            parameters.add(entry("dateTo", dateTo));
            parameters.add(entry("currentTab", "custom"));
            parameters.add(entry("limit_from", valueOf(i)));
            if (i >= 1) {
                parameters.add(entry("submitFilters", "0"));
                parameters.add(entry("last_time_scope", valueOf(lastTimeScope)));
                parameters.add(entry("byHandler", "true"));
            }

            String form = parameters.stream()
                    .map(entry -> entry.getKey() + "=" + URLEncoder.encode(entry.getValue(), UTF_8))
                    .collect(Collectors.joining("&"));

            HttpResponse<String> response = requestWithRetry(() -> {
                HttpRequest request = HttpRequest.newBuilder()
                        .POST(HttpRequest.BodyPublishers.ofString(form))
                        .uri(URI.create(INVESTING_DIV_CALENDAR))
                        .setHeader(HEADER_USER_AGENT, USER_AGENT)
                        .setHeader("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8")
                        .setHeader("Origin", "https://ru.investing.com")
                        .setHeader("Referer", "https://ru.investing.com/dividends-calendar/")
                        .setHeader("X-Requested-With", "XMLHttpRequest")
                        .setHeader("dnt", "1")
                        .setHeader("sec-gpc", "1")
                        .setHeader("Sec-Fetch-Mode", "cors")
                        .build();
                try {
                    return httpClient.send(request, HttpResponse.BodyHandlers.ofString());
                } catch (Exception ex) {
                    out.println("Error: " + ex.getMessage());
                    return null;
                }
            });

            JsonObject jsonResponse = JsonParser.parseString(response.body()).getAsJsonObject();
            String data = jsonResponse.get("data").getAsString();
            Document html = Jsoup.parseBodyFragment("<table><tbody>" + data + "</tbody></table>");
            List<Element> toAdd = html.select("tr").stream()
                    .filter(it -> !it.hasAttr("tablesorterdivider"))
                    .collect(toList());
            dividends.addAll(toAdd);

            if (!jsonResponse.get("bind_scroll_handler").getAsBoolean()) break;
            lastTimeScope = jsonResponse.get("last_time_scope").getAsLong();
            try {
                TimeUnit.MILLISECONDS.sleep(550);
            } catch (InterruptedException ex) {
                out.println("Error: " + ex.getMessage());
            }
        }

        Map<String, List<DiviTicker>> result = new LinkedHashMap<>();
        for (Element item : dividends) {
            try {
                var closeDate = item.select("td:eq(2)").text();
                if (null == closeDate || closeDate.isBlank()) continue;
                var closeByDate = dateFormat.parse(closeDate);

                var dividend = item.select("td:eq(3)").text();
                if (null == dividend || dividend.isBlank()) continue;

                var profit = item.select("td:eq(6)").text();
                if (null == profit || profit.isBlank()) continue;

                var code = item.select("td:eq(1) > a").text();
                if (null == code || code.isBlank()) continue;
                if (!tickerRepository.containsKey(new TickerInfo.Key(code, STOCK))) continue;

                var lastCandle = yahooService.getLastCandle(code);
                var price = valueOf(null != lastCandle ? lastCandle.getClose() : 0.0);

                code = code.toUpperCase();
                if (DE == marketConfig.getMarket()) {
                    code = code + "@DE";
                }

                var name = item.select("td:eq(1) > span").text();

                Calendar nextWorkDay = Calendar.getInstance();
                nextWorkDay.setTime(closeByDate);
                do {
                    nextWorkDay.add(Calendar.DATE, 1);
                } while (!CALENDAR_WORK_DAYS.contains(nextWorkDay.get(Calendar.DAY_OF_WEEK)));
                var realCloseByDate = dateFormat.format(nextWorkDay.getTime());

                var lastDate = dateFormat.format(new Date(closeByDate.getTime() - DAYS.toMillis(2)));
                DiviTicker ticker = new DiviTicker(name, code, lastDate, realCloseByDate, dividend, profit, price);

                List<DiviTicker> byDate = result.getOrDefault(ticker.getCloseDate(), new ArrayList<>(4));
                byDate.add(ticker);
                byDate.sort((c1, c2) -> c2.getPercent().compareTo(c1.getPercent()));
                result.put(ticker.getCloseDate(), byDate);
            } catch (Exception ex) {
                out.println("Error: Failed parse DiviTicker: " + ex.getMessage());
            }
        }

        return result;
    }

    private List<String> scanSymbols(Collection<String> names) {
        out.println("Scanning symbols from TradingView...");
        List<TickerScan> scan = tradingViewService.scanMarket(marketConfig.getMarket(), names);
        if (scan.isEmpty()) {
            return List.of();
        }
        if (MOEX != marketConfig.getMarket()) {
            try {
                RSXConfig rsxConfig = new RSXConfig();
                RSX rsx = new RSX(mainConfig, marketConfig, rsxConfig, tcsService);
                if (rsx.isTrendUp()) {
                    return rsx.topSymbols(scan);
                }
                return List.of();
            } catch (Exception ex) {
                out.println("Failed use RSX to filter symbols: " + ex.getMessage());
            }
        }
        return scan.stream().map(TickerScan::getName).collect(Collectors.toUnmodifiableList());
    }

    private void mergeCalendars(Map<String, List<DiviTicker>> result, Map<String, List<DiviTicker>> dividends) {
        for (Map.Entry<String, List<DiviTicker>> entry : dividends.entrySet()) {
            if (result.containsKey(entry.getKey())) {
                var items = result.get(entry.getKey());
                items.addAll(entry.getValue().stream().filter(it -> !items.contains(it)).collect(toList()));
                result.put(entry.getKey(), items);
            } else {
                result.put(entry.getKey(), entry.getValue());
            }
        }
    }
}