package com.github.shk0da.goldendragon.utils;

import java.util.Map;

import static java.lang.System.out;
import static java.util.stream.Collectors.toList;

public final class PrintUtils {

    public static void printGlassOfPrices(
            String tickerName, Map<String, Map<Double, Integer>> currentPrices) {
        out.println(tickerName + ": ");
        out.printf("    %-8s %-8s \n", "price", "value");
        out.println("    asks: ");
        for (Map.Entry<Double, Integer> ask :
                currentPrices.get("asks").entrySet().stream()
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
}
