package com.github.shk0da.GoldenDragon.strategy;

import com.github.shk0da.GoldenDragon.config.MainConfig;
import com.github.shk0da.GoldenDragon.config.MarketConfig;
import com.github.shk0da.GoldenDragon.model.DiviTicker;
import com.github.shk0da.GoldenDragon.model.Market;
import com.github.shk0da.GoldenDragon.repository.Repository;
import com.github.shk0da.GoldenDragon.repository.TickerRepository;
import com.github.shk0da.GoldenDragon.service.TCSService;
import com.google.gson.Gson;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
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
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import static com.github.shk0da.GoldenDragon.config.MainConfig.CALENDAR_WORK_DAYS;
import static com.github.shk0da.GoldenDragon.config.MainConfig.DOHOD_DIV_CALENDAR;
import static com.github.shk0da.GoldenDragon.config.MainConfig.HEADER_USER_AGENT;
import static com.github.shk0da.GoldenDragon.config.MainConfig.INVESTING_DIV_CALENDAR;
import static com.github.shk0da.GoldenDragon.config.MainConfig.SCAN_MOEX;
import static com.github.shk0da.GoldenDragon.config.MainConfig.SCAN_US;
import static com.github.shk0da.GoldenDragon.config.MainConfig.USER_AGENT;
import static com.github.shk0da.GoldenDragon.config.MainConfig.dateFormat;
import static com.github.shk0da.GoldenDragon.config.MainConfig.dateFormatUs;
import static com.github.shk0da.GoldenDragon.config.MainConfig.httpClient;
import static com.github.shk0da.GoldenDragon.utils.PredicateUtils.distinctByKey;
import static com.github.shk0da.GoldenDragon.utils.PrintUtils.printCalendarOfDividends;
import static com.github.shk0da.GoldenDragon.utils.PrintUtils.printCurrentPositions;
import static com.github.shk0da.GoldenDragon.utils.RequestUtils.requestWithRetry;
import static java.lang.Math.min;
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

    private final MainConfig config;
    private final MarketConfig marketConfig;

    private final TCSService tcsService;

    private final Repository<String, Map<String, Object>> tickerRepository = TickerRepository.INSTANCE;

    public DivFlow(MainConfig config, TCSService tcsService) {
        out.printf("%s: Start DivFlow%n", new Date().toString());
        this.config = config;
        this.marketConfig = config.getMarketConfig();
        this.tcsService = tcsService;
    }

    public void run() {
        double cash = tcsService.getAvailableCash();
        Map<String, Map<String, Object>> currentPositions = tcsService.getCurrentPositions();
        printCurrentPositions(marketConfig.getCurrency(), currentPositions, cash, tcsService);

        GregorianCalendar currentCalendar = new GregorianCalendar();
        // Download Dividends calendar
        List<String> tickersForScan = new ArrayList<>();
        Map<String, List<DiviTicker>> dividends = getCalendarDividends()
                .entrySet()
                .stream()
                .peek(it -> {
                    it.setValue(it.getValue().stream()
                            .filter(value -> tickerRepository.containsKey(value.getTickerCode()))
                            .filter(value -> {
                                boolean isPast;
                                boolean isToDay;
                                boolean isFeature = false;
                                try {
                                    Date valueDate = dateFormat.parse(value.getCloseDate());
                                    isPast = valueDate.before(currentCalendar.getTime());
                                    isToDay =  valueDate.equals(currentCalendar.getTime());
                                    isFeature = valueDate.after(currentCalendar.getTime());
                                } catch (Exception nothing) {
                                    isPast = true;
                                    isToDay = true;
                                }

                                boolean isHolding = currentPositions.containsKey(value.getTickerCode());

                                if ((isToDay || isPast) && isHolding) return true;

                                return isFeature && (isHolding || (value.getPercent() >= 1.0 && value.getPercent() <= 10.0));
                            })
                            .filter(distinctByKey(DiviTicker::getTickerCode))
                            .distinct()
                            .collect(toList()));
                    try {
                        // is feature
                        if (dateFormat.parse(it.getKey()).after(currentCalendar.getTime())) {
                            tickersForScan.addAll(it.getValue()
                                    .stream()
                                    .map(DiviTicker::getTickerCode)
                                    .filter(ticker -> !currentPositions.containsKey(ticker))
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
                                        .filter(value -> afterScan.contains(value.getTickerCode()) || currentPositions.containsKey(value.getTickerCode()))
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

        boolean isPrintPortfolioCost = false;
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
                if (!currentPositions.containsKey(diviTicker.getTickerCode())) {
                    continue;
                }
                try {
                    double cost = preparingSale(currentDate, diviTicker, currentPositions.get(diviTicker.getTickerCode()));
                    if (cost > 0.0) {
                        isPrintPortfolioCost = true;
                    }
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
                tickersToBuy.addAll(tickers);
            }
            buyCalendar.add(Calendar.DATE, 1);
        }
        if (!tickersToBuy.isEmpty() && cash > 0.0) {
            for (DiviTicker diviTicker : tickersToBuy) {
                if (currentPositions.containsKey(diviTicker.getTickerCode())) {
                    continue;
                }
                try {
                    double availableCashToBuy = min(marketConfig.getMaxPositionCostToBuy(), (cash * .8) / tickersToBuy.size());
                    double cost = preparingPurchase(currentDate, diviTicker, availableCashToBuy);
                    if (cost > 0.0) {
                        cash = cash - cost;
                        isPrintPortfolioCost = true;
                    }
                    out.println();
                } catch (Exception ex) {
                    out.println("Failed preparing purchase: " + ex.getMessage());
                }
            }
        }

        if (isPrintPortfolioCost) {
            printCurrentPositions(marketConfig.getCurrency(), tcsService.getCurrentPositions(), tcsService.getAvailableCash(), tcsService);
        }
    }

    private double preparingSale(String currentDate, DiviTicker diviTicker, Map<String, Object> currentPosition) {
        int count = (Integer) currentPosition.get("balance");
        if (count == 0) {
            out.println("Warn: sale will be skipped - " + diviTicker.getTickerCode() + " with count " + count);
            return 0.0;
        }

        double tickerPrice = tcsService.getAvailablePrice(diviTicker.getTickerCode(), count, true);
        if (0.0 == tickerPrice) {
            out.println("Warn: sale will be used Market Price - " + diviTicker.getTickerCode());
        }

        double cost = count * tickerPrice;
        if (1 == sell(currentDate, diviTicker, tickerPrice, count, cost)) {
            return cost;
        } else {
            return 0.0;
        }
    }

    private double preparingPurchase(String currentDate, DiviTicker diviTicker, double availableCashToBuy) {
        int value = 0;
        double tickerPrice = 0.0;
        for (Map.Entry<Double, Integer> ask : tcsService.getCurrentPrices(diviTicker.getTickerCode()).get("asks").entrySet()) {
            tickerPrice = ask.getKey();
            value = value + ask.getValue();
            if (value >= (availableCashToBuy / tickerPrice)) break;
        }

        if (0.0 == tickerPrice) {
            out.println("Warn: purchase will be skipped - " + diviTicker.getTickerCode() + " by price " + tickerPrice);
            return 0.0;
        }

        int lot = (int) tcsService.searchTicker(diviTicker.getTickerCode()).get("lot");
        int count = (int) (availableCashToBuy / tickerPrice);
        while (count % lot != 0 && count > 0) {
            count = count - 1;
        }
        if (count == 0) {
            out.println("Warn: purchase will be skipped - " + diviTicker.getTickerCode() + " with count " + count);
            return 0.0;
        }
        double cost = count * tickerPrice;
        if (1 == buy(currentDate, diviTicker, tickerPrice, count, cost)) {
            return cost;
        } else {
            return 0.0;
        }
    }

    private int sell(String currentDate, DiviTicker diviTicker, double tickerPrice, int count, double cost) {
        out.println("[" + currentDate + "] Sell: " + count + " " + diviTicker.getTickerCode() + " by " + tickerPrice + " (" + cost + ")");
        if (config.isTestMode()) {
            return 1;
        }
        return tcsService.createOrder(diviTicker.getTickerCode(), tickerPrice, count, "Sell");
    }

    private int buy(String currentDate, DiviTicker diviTicker, double tickerPrice, int count, double cost) {
        out.println("[" + currentDate + "] Buy: " + count + " " + diviTicker.getTickerCode() + " by " + tickerPrice + " (" + cost + ")");
        if (config.isTestMode()) {
            return 1;
        }
        return tcsService.createOrder(diviTicker.getTickerCode(), tickerPrice, count, "Buy");
    }

    private Map<String, List<DiviTicker>> getCalendarDividends() {
        Map<String, List<DiviTicker>> result = new LinkedHashMap<>();

        if (Market.MOEX == marketConfig.getMarket()) {
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
                    .uri(URI.create(MainConfig.SMART_LAB_DIV_CALENDAR))
                    .setHeader(MainConfig.HEADER_USER_AGENT, MainConfig.USER_AGENT)
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
        if (Market.US == marketConfig.getMarket()) {
            calendar.add(Calendar.DATE, -4);
        }
        String dateFrom = dateFormatUs.format(calendar.getTime());
        calendar.add(Calendar.DATE, 18);
        String dateTo = dateFormatUs.format(calendar.getTime());

        List<Element> dividends = new ArrayList<>();
        long lastTimeScope = currentTimeMillis() / 1000L;
        for (int i = 0; i < 10; i++) {
            List<Map.Entry<String, String>> parameters = new ArrayList<>(9);
            parameters.add(entry("country[]", Market.US == marketConfig.getMarket() ? "5" : "56"));

            parameters.add(entry("dateFrom", dateFrom));
            parameters.add(entry("dateTo", dateTo));
            parameters.add(entry("currentTab", "custom"));
            parameters.add(entry("limit_from", String.valueOf(i)));
            if (i >= 1) {
                parameters.add(entry("submitFilters", "0"));
                parameters.add(entry("last_time_scope", String.valueOf(lastTimeScope)));
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
                code = code.toUpperCase();

                var name = item.select("td:eq(1) > span").text();
                var lastDate = dateFormat.format(new Date(closeByDate.getTime() - DAYS.toMillis(2)));
                var price = "0.0";
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

    private List<String> scanSymbols(Collection<String> names) {
        out.println("Scanning symbols from TradingView...");

        Map<String, Object> jsonData = new HashMap<>();

        Map<String, Object> symbols = new HashMap<>();
        List<String> tickers = new ArrayList<>(names.size() * 2);
        if (Market.US == marketConfig.getMarket()) {
            tickers.addAll(names.stream().map(name -> "NASDAQ:" + name).collect(toList()));
            tickers.addAll(names.stream().map(name -> "NYSE:" + name).collect(toList()));
        }
        if (Market.MOEX == marketConfig.getMarket()) {
            tickers.addAll(names.stream().map(name -> "MOEX:" + name).collect(toList()));
        }
        symbols.put("tickers", tickers);
        jsonData.put("symbols", symbols);

        List<Map<String, Object>> filter = new ArrayList<>(5);
        if (Market.US == marketConfig.getMarket()) {
            Map<String, Object> marketCapBasic = new LinkedHashMap<>();
            marketCapBasic.put("left", "market_cap_basic");
            marketCapBasic.put("operation", "egreater");
            marketCapBasic.put("right", 50000000);
            filter.add(marketCapBasic);

            Map<String, Object> debtToEquity = new LinkedHashMap<>();
            debtToEquity.put("left", "debt_to_equity");
            debtToEquity.put("operation", "in_range");
            debtToEquity.put("right", new int[]{-50, 3});
            filter.add(debtToEquity);

            Map<String, Object> recommend1M = new LinkedHashMap<>();
            recommend1M.put("left", "Recommend.All|1M");
            recommend1M.put("operation", "egreater");
            recommend1M.put("right", 0.5);
            filter.add(recommend1M);

            Map<String, Object> numberOfEmployees = new LinkedHashMap<>();
            numberOfEmployees.put("left", "number_of_employees");
            numberOfEmployees.put("operation", "egreater");
            numberOfEmployees.put("right", 1000);
            filter.add(numberOfEmployees);
        }

        if (Market.MOEX == marketConfig.getMarket()) {
            Map<String, Object> recommend1W = new LinkedHashMap<>();
            recommend1W.put("left", "Recommend.All|1W");
            recommend1W.put("operation", "egreater");
            recommend1W.put("right", 0.4);
            filter.add(recommend1W);
        }

        Map<String, Object> totalRevenue = new LinkedHashMap<>();
        totalRevenue.put("left", "total_revenue");
        totalRevenue.put("operation", "egreater");
        totalRevenue.put("right", 0);
        filter.add(totalRevenue);

        jsonData.put("filter", filter);

        jsonData.put("columns", new String[]{
                "name",
                "Recommend.MA|1W",
                "Recommend.All|1W"
        });
        String json = new Gson().toJson(jsonData);

        HttpResponse<String> response = requestWithRetry(() -> {
            HttpRequest request = HttpRequest.newBuilder()
                    .POST(HttpRequest.BodyPublishers.ofString(json))
                    .uri(URI.create(marketConfig.getMarket() == Market.US ? SCAN_US : SCAN_MOEX))
                    .setHeader(HEADER_USER_AGENT, USER_AGENT)
                    .build();
            try {
                return httpClient.send(request, HttpResponse.BodyHandlers.ofString());
            } catch (Exception ex) {
                out.println("Error: " + ex.getMessage());
                return null;
            }
        });

        List<String> result = new ArrayList<>(names.size());
        JsonArray data = JsonParser.parseString(response.body()).getAsJsonObject().get("data").getAsJsonArray();
        for (JsonElement item : data) {
            var params = item.getAsJsonObject().get("d").getAsJsonArray();
            var ma = params.get(1).getAsDouble();
            var all = params.get(2).getAsDouble();
            if (ma >= 0.8 && all >= 0.4) {
                var name = params.get(0).getAsString();
                result.add(name);
            }
        }
        return result;
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