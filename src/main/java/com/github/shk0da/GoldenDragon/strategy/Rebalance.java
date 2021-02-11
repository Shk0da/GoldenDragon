package com.github.shk0da.GoldenDragon.strategy;

import com.github.shk0da.GoldenDragon.config.MainConfig;
import com.github.shk0da.GoldenDragon.config.MarketConfig;
import com.github.shk0da.GoldenDragon.config.RebalanceConfig;
import com.github.shk0da.GoldenDragon.model.PortfolioPosition;
import com.github.shk0da.GoldenDragon.model.PositionInfo;
import com.github.shk0da.GoldenDragon.model.TickerInfo;
import com.github.shk0da.GoldenDragon.model.TickerType;
import com.github.shk0da.GoldenDragon.service.TCSService;
import com.google.gson.reflect.TypeToken;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

import static com.github.shk0da.GoldenDragon.config.MainConfig.dateFormat;
import static com.github.shk0da.GoldenDragon.utils.PrintUtils.printCurrentPositions;
import static com.github.shk0da.GoldenDragon.utils.SerializationUtils.loadDataFromDisk;
import static com.github.shk0da.GoldenDragon.utils.SerializationUtils.saveDataToDisk;
import static java.lang.Math.round;
import static java.lang.System.out;
import static java.util.Comparator.comparing;

/**
 * CRON MOEX: Every Mon at 10:01 MSK
 */
public class Rebalance {

    private final MainConfig mainConfig;
    private final MarketConfig marketConfig;
    private final RebalanceConfig rebalanceConfig;

    private final TCSService tcsService;

    public Rebalance(MainConfig mainConfig, MarketConfig marketConfig, RebalanceConfig rebalanceConfig, TCSService tcsService) {
        this.mainConfig = mainConfig;
        this.marketConfig = marketConfig;
        this.rebalanceConfig = rebalanceConfig;
        this.tcsService = tcsService;
    }

    public void run() throws Exception {
        double totalPortfolioCost = tcsService.getAvailableCash();
        Map<TickerInfo.Key, PortfolioPosition> currentPositions = new HashMap<>();
        Map<TickerInfo.Key, PortfolioPosition> previousPositions = loadDataFromDisk(RebalanceConfig.SERIALIZE_NAME, new TypeToken<>() {});
        if (null != previousPositions && !previousPositions.isEmpty()) {
            Map<PortfolioPosition, Double> portfolioPositionToCost = new HashMap<>();
            Map<TickerInfo.Key, PositionInfo> positions = tcsService.getCurrentPositions(TickerType.ALL);
            Map<TickerInfo.Key, Double> prices = new HashMap<>();
            for (Map.Entry<TickerInfo.Key, PositionInfo> position : positions.entrySet()) {
                if (previousPositions.containsKey(position.getKey())) {
                    int balance = position.getValue().getBalance();
                    double price = tcsService.getAvailablePrice(position.getKey(), balance, false);

                    String basicCurrency = marketConfig.getCurrency();
                    String currency = tcsService.searchTicker(position.getKey()).getCurrency();
                    if (!basicCurrency.equals(currency)) {
                        price = tcsService.convertCurrencies(currency, basicCurrency, price);
                    }
                    prices.put(position.getKey(), price);

                    double cost = balance * price;
                    totalPortfolioCost += cost;
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
        Map<TickerInfo.Key, PortfolioPosition> targetPositions = rebalanceConfig.getPortfolioPositions();
        if (null != targetPositions && !targetPositions.isEmpty()) {
            for (Map.Entry<TickerInfo.Key, PortfolioPosition> entry : targetPositions.entrySet()) {
                double percent = entry.getValue().getPercent();
                if (currentPositions.containsKey(entry.getKey())) {
                    var currentPosition = currentPositions.get(entry.getKey());
                    percent = currentPosition.getPercent() > percent
                            ? -1 * (currentPosition.getPercent() - percent)
                            : percent - currentPosition.getPercent();
                }
                corrections.put(entry.getKey(), new PortfolioPosition(entry.getValue().getName(), entry.getValue().getType(), percent));
            }
            for (Map.Entry<TickerInfo.Key, PortfolioPosition> entry : currentPositions.entrySet()) {
                if (!targetPositions.containsKey(entry.getKey())) {
                    corrections.put(
                            entry.getKey(),
                            new PortfolioPosition(entry.getValue().getName(), entry.getValue().getType(), (-1) * entry.getValue().getPercent())
                    );
                }
            }
        }

        Map<TickerInfo.Key, PortfolioPosition> positionsToSave = new HashMap<>();
        for (PortfolioPosition correction : corrections.values()
                .stream()
                .sorted(comparing(PortfolioPosition::getPercent))
                .collect(Collectors.toList())) {
            double cost = Math.abs(totalPortfolioCost / 100 * correction.getPercent());
            if (correction.getPercent() < 0) {
                if (!sell(correction.getName(), correction.getType(), cost)) {
                    positionsToSave.put(new TickerInfo.Key(correction.getName(), correction.getType()), correction);
                }
            } else {
                if (buy(correction.getName(), correction.getType(), cost)) {
                    positionsToSave.put(new TickerInfo.Key(correction.getName(), correction.getType()), correction);
                }
            }
        }

        saveDataToDisk(RebalanceConfig.SERIALIZE_NAME, positionsToSave);
    }

    private boolean sell(String name, TickerType type, double cost) {
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

        int count = (int) round(cost / tcsService.getAvailablePrice(key));
        double tickerPrice = tcsService.getAvailablePrice(key, count, true);
        if (0.0 == tickerPrice) {
            out.println("Warn: sale will be used Market Price - " + name);
        }

        String currentDate = dateFormat.format(new Date());
        out.println("[" + currentDate + "] Sell: " + count + " " + key.getTicker() + " by " + tickerPrice + " (" + cost + ")");
        if (mainConfig.isTestMode()) {
            return true;
        }
        return 1 == tcsService.createOrder(key, tickerPrice, count, "Sell");
    }

    private boolean buy(String name, TickerType type, double availableCashToBuy) {
        var key = new TickerInfo.Key(name, type);

        String basicCurrency = marketConfig.getCurrency();
        String currency = tcsService.searchTicker(key).getCurrency();
        if (!basicCurrency.equals(currency)) {
            availableCashToBuy = tcsService.convertCurrencies(currency, basicCurrency, availableCashToBuy);
        }

        int value = 0;
        double tickerPrice = 0.0;
        for (Map.Entry<Double, Integer> ask : tcsService.getCurrentPrices(key).get("asks").entrySet()) {
            tickerPrice = ask.getKey();
            value = value + ask.getValue();
            if (value >= (availableCashToBuy / tickerPrice)) break;
        }

        if (0.0 == tickerPrice) {
            out.println("Warn: purchase will be skipped - " + name + " by price " + tickerPrice);
            return false;
        }

        int lot = tcsService.searchTicker(key).getLot();
        int count = (int) (availableCashToBuy / tickerPrice);
        while (count % lot != 0 && count > 0) {
            count = count - 1;
        }
        if (count == 0) {
            out.println("Warn: purchase will be skipped - " + name + " with count " + count);
            return false;
        }
        double cost = count * tickerPrice;

        String currentDate = dateFormat.format(new Date());
        out.println("[" + currentDate + "] Buy: " + count + " " + key.getTicker() + " by " + tickerPrice + " (" + cost + ")");
        if (mainConfig.isTestMode()) {
            return true;
        }
        return 1 == tcsService.createOrder(key, tickerPrice, count, "Buy");
    }
}
