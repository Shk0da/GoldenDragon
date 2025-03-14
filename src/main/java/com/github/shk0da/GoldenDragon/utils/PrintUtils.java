package com.github.shk0da.GoldenDragon.utils;

import com.github.shk0da.GoldenDragon.model.DiviTicker;
import com.github.shk0da.GoldenDragon.model.PortfolioPosition;
import com.github.shk0da.GoldenDragon.model.PositionInfo;
import com.github.shk0da.GoldenDragon.model.TickerInfo;
import com.github.shk0da.GoldenDragon.service.TCSService;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import static java.lang.Math.min;
import static java.lang.Math.round;
import static java.lang.System.out;
import static java.util.Comparator.comparing;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;

public final class PrintUtils {

    public static void printGlassOfPrices(String tickerName, Map<String, Map<Double, Integer>> currentPrices) {
        out.println(tickerName + ": ");
        out.printf("    %-8s %-8s \n", "price", "value");
        out.println("    asks: ");
        for (Map.Entry<Double, Integer> ask : currentPrices.get("asks")
                .entrySet()
                .stream()
                .sorted((o1, o2) -> o2.getKey().compareTo(o1.getKey()))
                .collect(toList())) {
            out.printf("    %-8s %-8s \n", ask.getKey(), ask.getValue());
        }
        out.println("    bids: ");
        for (Map.Entry<Double, Integer> bid : currentPrices.get("bids").entrySet()) {
            out.printf("    %-8s %-8s \n", bid.getKey(), bid.getValue());
        }
        out.printf("    %-8s %-8s \n", "price", "value");
    }

    public static void printCurrentPositions(String currency,
                                             Map<TickerInfo.Key, PositionInfo> currentPositions,
                                             double cash,
                                             TCSService tcsService) {
        double totalPortfolioCost = 0.0D;
        out.println("========================== Current Positions =============================");
        out.printf("%-8s %-8s %-15s %-15s %-9s %-5s %-5s \n",
                "Ticker",
                "Type",
                "FIGI",
                "ISIN",
                "Qty",
                "Lots",
                "Yield"
        );
        out.println("--------------------------------------------------------------------------");

        Map<TickerInfo.Key, PositionInfo> sortedPositions = currentPositions.entrySet().stream()
                .sorted(comparing(o -> o.getValue().getTicker()))
                .sorted(comparing(o -> o.getValue().getInstrumentType().name()))
                .collect(toMap(Map.Entry::getKey, Map.Entry::getValue, (o1, o2) -> o1, LinkedHashMap::new));
        for (Map.Entry<TickerInfo.Key, PositionInfo> entry : sortedPositions.entrySet()) {
            PositionInfo currentPosition = entry.getValue();

            String ticker = currentPosition.getTicker();
            String shortTicker = ticker.substring(0, min(ticker.length(), 7));
            int balance = currentPosition.getBalance();
            out.printf("%-8s %-8s %-15s %-15s %-9s %-5s %-5s \n",
                    shortTicker,
                    currentPosition.getInstrumentType(),
                    currentPosition.getFigi(),
                    currentPosition.getIsin(),
                    currentPosition.getBalance(),
                    currentPosition.getLots(),
                    currentPosition.getExpectedYield()
            );
            TickerInfo tickerInfo = tcsService.searchTicker(entry.getKey());
            if (currency.equals(tickerInfo.getCurrency())) {
                totalPortfolioCost += tcsService.getAvailablePrice(tickerInfo.getKey(), balance, false) * balance;
            }
        }
        out.println("==========================================================================\n");
        out.printf("Total portfolio cost: %.2f %s", totalPortfolioCost + cash, currency);
        out.println("\n");
    }

    public static void printCalendarOfDividends(Map<String, List<DiviTicker>> dividends) {
        out.println("========================== Dividends Calendar ============================");
        out.printf("%-10s %-8s %-12s %-8s %-12s %-8s %-10s \n",
                "Close Date",
                "Ticker",
                "Description",
                "Price",
                "Dividend",
                "Percent",
                "Close Date"
        );
        out.println("--------------------------------------------------------------------------");
        for (Map.Entry<String, List<DiviTicker>> entry : dividends.entrySet()) {
            for (DiviTicker diviTicker : entry.getValue()) {
                out.printf("%-10s %-8s %-12s %-8s %-12s %-8s %-10s \n",
                        entry.getKey(),
                        diviTicker.getTickerCode(),
                        diviTicker.getDescription(),
                        diviTicker.getPrice(),
                        diviTicker.getDividend(),
                        diviTicker.getPercent() + "%",
                        diviTicker.getCloseDate()
                );
            }
        }
        out.println("==========================================================================\n");
    }

    public static void printCurrentPositions(Map<TickerInfo.Key, PortfolioPosition> currentPositions,
                                             Map<TickerInfo.Key, PositionInfo> portfolioPositions,
                                             Map<TickerInfo.Key, Double> prices,
                                             double totalPortfolioCost,
                                             String currency) {
        out.println("================================= Current Positions ================================");
        out.printf("%-8s %-8s %-15s %-15s %-9s %-5s %-12s %-5s \n",
                "Ticker",
                "Type",
                "FIGI",
                "ISIN",
                "Qty",
                "Lots",
                "Price",
                "  %"
        );
        out.println("------------------------------------------------------------------------------------");

        Map<TickerInfo.Key, PortfolioPosition> sortedPositions = currentPositions.entrySet().stream()
                .sorted(comparing(o -> o.getValue().getName()))
                .sorted(comparing(o -> o.getValue().getType().name()))
                .collect(toMap(Map.Entry::getKey, Map.Entry::getValue, (o1, o2) -> o1, LinkedHashMap::new));
        for (Map.Entry<TickerInfo.Key, PortfolioPosition> entry : sortedPositions.entrySet()) {
            PortfolioPosition currentPosition = entry.getValue();
            PositionInfo positionInfo = portfolioPositions.get(entry.getKey());
            if (null == positionInfo) continue;

            String ticker = currentPosition.getName();
            String shortTicker = ticker.substring(0, min(ticker.length(), 7));
            out.printf("%-8s %-8s %-15s %-15s %-9s %-5s %-12s %2.2f\n",
                    shortTicker,
                    positionInfo.getInstrumentType(),
                    positionInfo.getFigi(),
                    positionInfo.getIsin(),
                    positionInfo.getBalance(),
                    positionInfo.getLots(),
                    prices.getOrDefault(entry.getKey(), 0.0),
                    currentPosition.getPercent()
            );
        }
        out.println("====================================================================================\n");
        out.printf("Total portfolio cost: %.2f %s", totalPortfolioCost, currency);
        out.println("\n");
    }

    public static String formatFloat(Double value) {
        return String.format("%.2f", round(value * 100) / 100.0);
    }
}
