package com.github.shk0da.goldendragon.strategy.orderbook;

import com.github.shk0da.goldendragon.model.MarketDepthLevel;
import com.github.shk0da.goldendragon.model.MarketTradeTick;
import java.util.List;

/** Shared order-book metric helpers for signals and the trading engine. */
public final class OrderBookMath {

    private OrderBookMath() {}

    public static double calculateObi(
            List<MarketDepthLevel> bids, List<MarketDepthLevel> asks, int levels) {
        double bidVol = sumQuantity(bids, levels);
        double askVol = sumQuantity(asks, levels);
        double total = bidVol + askVol;
        if (total <= 0.0) {
            return 0.0;
        }
        return (bidVol - askVol) / total;
    }

    public static double calculateMicroEdge(
            double bestBid, double bestAsk, int bidQty0, int askQty0) {
        double mid = (bestBid + bestAsk) / 2.0;
        int totalTop = bidQty0 + askQty0;
        if (totalTop <= 0 || mid <= 0.0) {
            return 0.0;
        }
        double micro = (bestBid * askQty0 + bestAsk * bidQty0) / totalTop;
        return micro - mid;
    }

    public static double calculateTradeDelta(List<MarketTradeTick> trades) {
        long buyVolume = 0;
        long sellVolume = 0;
        for (MarketTradeTick trade : trades) {
            if (isBuyDirection(trade.getDirection())) {
                buyVolume += trade.getQuantity();
            } else {
                sellVolume += trade.getQuantity();
            }
        }
        return buyVolume - sellVolume;
    }

    public static int topQuantity(List<MarketDepthLevel> levels, int index) {
        if (levels == null || levels.size() <= index) {
            return 0;
        }
        return levels.get(index).getQuantity();
    }

    private static double sumQuantity(List<MarketDepthLevel> levels, int maxLevels) {
        if (levels == null || levels.isEmpty()) {
            return 0.0;
        }
        double sum = 0.0;
        int limit = Math.min(maxLevels, levels.size());
        for (int i = 0; i < limit; i++) {
            sum += levels.get(i).getQuantity();
        }
        return sum;
    }

    private static boolean isBuyDirection(String direction) {
        return direction != null && direction.toUpperCase().contains("BUY");
    }
}
