package com.github.shk0da.GoldenDragon.strategy;

import com.github.shk0da.GoldenDragon.config.MainConfig;
import com.github.shk0da.GoldenDragon.config.MarketConfig;
import com.github.shk0da.GoldenDragon.model.PortfolioPosition;
import com.github.shk0da.GoldenDragon.model.PositionInfo;
import com.github.shk0da.GoldenDragon.model.TickerInfo;
import com.github.shk0da.GoldenDragon.model.TickerType;
import com.github.shk0da.GoldenDragon.service.TCSService;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

import static com.github.shk0da.GoldenDragon.config.MainConfig.dateFormat;
import static com.github.shk0da.GoldenDragon.utils.PrintUtils.printCurrentPositions;
import static java.lang.Math.round;
import static java.lang.System.out;
import static java.util.Comparator.comparing;

public abstract class Rebalancing {

    private final MainConfig mainConfig;
    private final MarketConfig marketConfig;
    private final TCSService tcsService;

    public Rebalancing(MainConfig mainConfig, MarketConfig marketConfig, TCSService tcsService) {
        this.mainConfig = mainConfig;
        this.marketConfig = marketConfig;
        this.tcsService = tcsService;
    }

    public Map<TickerInfo.Key, PortfolioPosition> doRebalance(double totalPortfolioCost,
                                                              Map<TickerInfo.Key, PortfolioPosition> previousPositions,
                                                              Map<TickerInfo.Key, PortfolioPosition> targetPositions,
                                                              double positionPercentToDo) {
        Map<TickerInfo.Key, PortfolioPosition> currentPositions = new HashMap<>();
        if (null != previousPositions && !previousPositions.isEmpty()) {
            Map<PortfolioPosition, Double> portfolioPositionToCost = new HashMap<>();
            out.println();
            Map<TickerInfo.Key, PositionInfo> positions = tcsService.getCurrentPositions(TickerType.ALL);
            Map<TickerInfo.Key, Double> prices = new HashMap<>();
            for (Map.Entry<TickerInfo.Key, PositionInfo> position : positions.entrySet()) {
                if (previousPositions.containsKey(position.getKey())) {
                    int balance = position.getValue().getBalance();
                    double price = tcsService.getPriceInCurrentCurrency(position.getKey(), balance, marketConfig.getCurrency());
                    double cost = balance * price;
                    totalPortfolioCost += cost;
                    prices.put(position.getKey(), price);
                    portfolioPositionToCost.put(previousPositions.get(position.getKey()), cost);
                }
            }

            for (Map.Entry<PortfolioPosition, Double> position : portfolioPositionToCost.entrySet()) {
                PortfolioPosition portfolioPosition = position.getKey();
                double percent = round((100 * ((position.getValue()) / totalPortfolioCost)) * 100) / 100.0;
                currentPositions.put(
                        new TickerInfo.Key(portfolioPosition.getName(), portfolioPosition.getType()),
                        new PortfolioPosition(
                                portfolioPosition.getName(),
                                portfolioPosition.getType(),
                                percent
                        )
                );
            }
            printCurrentPositions(currentPositions, positions, prices, totalPortfolioCost, marketConfig.getCurrency());
        }

        Map<TickerInfo.Key, PortfolioPosition> corrections = new HashMap<>();
        if (null != targetPositions && !targetPositions.isEmpty()) {
            for (Map.Entry<TickerInfo.Key, PortfolioPosition> targetPosition : targetPositions.entrySet()) {
                double targetPercent = targetPosition.getValue().getPercent();
                if (currentPositions.containsKey(targetPosition.getKey())) {
                    var currentPercent = currentPositions.get(targetPosition.getKey()).getPercent();
                    double diff = Math.abs(currentPercent - targetPercent);
                    if (diff >= positionPercentToDo) {
                        targetPercent = currentPercent > targetPercent ? -1 * diff : diff;
                    } else {
                        targetPercent = currentPercent;
                    }
                }
                corrections.put(
                        targetPosition.getKey(),
                        new PortfolioPosition(
                                targetPosition.getValue().getName(),
                                targetPosition.getValue().getType(),
                                targetPercent
                        )
                );
            }
            for (Map.Entry<TickerInfo.Key, PortfolioPosition> currentPosition : currentPositions.entrySet()) {
                if (!targetPositions.containsKey(currentPosition.getKey())) {
                    corrections.put(
                            currentPosition.getKey(),
                            new PortfolioPosition(
                                    currentPosition.getValue().getName(),
                                    currentPosition.getValue().getType(),
                                    (-1) * currentPosition.getValue().getPercent()
                            )
                    );
                }
            }
        }

        boolean isPrintResultTable = false;
        Map<TickerInfo.Key, PortfolioPosition> positionsToSave = new HashMap<>();
        for (PortfolioPosition correction : corrections.values()
                .stream()
                .sorted(comparing(PortfolioPosition::getPercent))
                .collect(Collectors.toList())) {
            double cost = Math.abs(totalPortfolioCost / 100 * correction.getPercent());
            if (correction.getPercent() < 0) {
                if (!sell(correction.getName(), correction.getType(), cost)) {
                    positionsToSave.put(new TickerInfo.Key(correction.getName(), correction.getType()), correction);
                } else {
                    isPrintResultTable = true;
                }
            } else {
                if (buy(correction.getName(), correction.getType(), cost)) {
                    positionsToSave.put(new TickerInfo.Key(correction.getName(), correction.getType()), correction);
                    isPrintResultTable = true;
                }
            }
        }

        if (isPrintResultTable) {
            out.println();
            double portfolioCost = tcsService.getAvailableCash();
            Map<TickerInfo.Key, Double> prices = new HashMap<>();
            Map<TickerInfo.Key, PositionInfo> positions = tcsService.getCurrentPositions(TickerType.ALL);

            for (Map.Entry<TickerInfo.Key, PortfolioPosition> position : positionsToSave.entrySet()) {
                var currentPosition = positions.get(position.getKey());
                if (null == currentPosition) continue;

                double price = tcsService.getAvailablePrice(position.getKey(), currentPosition.getBalance(), false);

                String basicCurrency = marketConfig.getCurrency();
                String currency = tcsService.searchTicker(position.getKey()).getCurrency();
                if (!basicCurrency.equals(currency)) {
                    price = tcsService.convertCurrencies(currency, basicCurrency, price);
                }
                prices.put(position.getKey(), price);

                portfolioCost += price * currentPosition.getBalance();
            }

            printCurrentPositions(positionsToSave, positions, prices, portfolioCost, marketConfig.getCurrency());
        }

        return positionsToSave;
    }

    protected boolean sell(String name, TickerType type, double cost) {
        if (cost == 0) {
            out.println("Warn: sale will be skipped - " + name + " with cost " + cost);
            return false;
        }

        var key = new TickerInfo.Key(name, type);

        String basicCurrency = marketConfig.getCurrency();
        String currency = tcsService.searchTicker(key).getCurrency();
        if (!basicCurrency.equals(currency)) {
            cost = tcsService.convertCurrencies(currency, basicCurrency, cost);
        }

        int lot = tcsService.searchTicker(key).getLot();
        int count = (int) round(cost / tcsService.getAvailablePrice(key));
        while (count % lot != 0 && count > 0) {
            count = count - 1;
        }
        if (count == 0) {
            out.println("Warn: sale will be skipped - " + name + " with count " + count);
            return false;
        }
        double tickerPrice = tcsService.getAvailablePrice(key, count, true);
        if (0.0 == tickerPrice) {
            out.println("Warn: sale will be used Market Price - " + name);
        }

        String currentDate = dateFormat.format(new Date());
        out.println("[" + currentDate + "] Sell: " + count + " " + key.getTicker() + " by " + tickerPrice + " (" + cost + " " + currency + ")");
        if (mainConfig.isTestMode()) {
            return true;
        }
        return 1 == tcsService.createOrder(key, tickerPrice, count, "Sell");
    }

    protected boolean buy(String name, TickerType type, double cashToBuy) {
        var key = new TickerInfo.Key(name, type);

        String basicCurrency = marketConfig.getCurrency();
        String currency = tcsService.searchTicker(key).getCurrency();
        if (!basicCurrency.equals(currency)) {
            cashToBuy = tcsService.convertCurrencies(currency, basicCurrency, cashToBuy);
        }

        int value = 0;
        double tickerPrice = 0.0;
        for (Map.Entry<Double, Integer> ask : tcsService.getCurrentPrices(key).get("asks").entrySet()) {
            tickerPrice = ask.getKey();
            value = value + ask.getValue();
            if (value >= (cashToBuy / tickerPrice)) break;
        }

        if (0.0 == tickerPrice) {
            out.println("Warn: purchase will be skipped - " + name + " by price " + tickerPrice);
            return false;
        }

        int lot = tcsService.searchTicker(key).getLot();
        int count = (int) (cashToBuy / tickerPrice);
        while (count % lot != 0 && count > 0) {
            count = count - 1;
        }
        if (count == 0) {
            out.println("Warn: purchase will be skipped - " + name + " with count " + count);
            return false;
        }
        double cost = count * tickerPrice;

        String currentDate = dateFormat.format(new Date());
        out.println("[" + currentDate + "] Buy: " + count + " " + key.getTicker() + " by " + tickerPrice + " (" + cost + " " + currency + ")");
        if (mainConfig.isTestMode()) {
            return true;
        }
        return 1 == tcsService.createOrder(key, tickerPrice, count, "Buy");
    }
}
