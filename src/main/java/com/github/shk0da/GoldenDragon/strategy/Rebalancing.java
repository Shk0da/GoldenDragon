package com.github.shk0da.GoldenDragon.strategy;

import com.github.shk0da.GoldenDragon.config.MarketConfig;
import com.github.shk0da.GoldenDragon.model.PortfolioPosition;
import com.github.shk0da.GoldenDragon.model.PositionInfo;
import com.github.shk0da.GoldenDragon.model.TickerInfo;
import com.github.shk0da.GoldenDragon.model.TickerType;
import com.github.shk0da.GoldenDragon.service.TCSService;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static com.github.shk0da.GoldenDragon.utils.PrintUtils.printCurrentPositions;
import static java.lang.Math.round;
import static java.lang.System.out;
import static java.util.Comparator.comparing;

public abstract class Rebalancing {

    private final MarketConfig marketConfig;
    private final TCSService tcsService;

    public Rebalancing(MarketConfig marketConfig, TCSService tcsService) {
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
        List<PortfolioPosition> sortedCorrections = corrections.values()
                .stream()
                .sorted(comparing(PortfolioPosition::getPercent))
                .collect(Collectors.toList());
        for (PortfolioPosition correction : sortedCorrections) {
            out.printf("Correction: %s [%.2f] \n", correction.getName(), correction.getPercent());
            double cost = Math.abs(totalPortfolioCost / 100 * correction.getPercent());
            if (correction.getPercent() > 0) {
                TickerInfo.Key tickerInfoKey = new TickerInfo.Key(correction.getName(), correction.getType());
                boolean isSuccessfulBuy = tcsService.buy(correction.getName(), correction.getType(), cost);
                boolean isCurrentPosition = currentPositions.containsKey(tickerInfoKey);
                if (isSuccessfulBuy || isCurrentPosition) {
                    if (isCurrentPosition) {
                        double currentPercent = currentPositions.get(tickerInfoKey).getPercent();
                        double totalPercent = isSuccessfulBuy  ? correction.getPercent() + currentPercent : currentPercent;
                        correction = new PortfolioPosition(correction.getName(), correction.getType(), totalPercent);
                    }
                    positionsToSave.put(tickerInfoKey, correction);
                    isPrintResultTable = true;
                }
            } else {
                boolean isSuccessfulSell =  tcsService.sell(correction.getName(), correction.getType(), cost);
                if (isSuccessfulSell) {
                    isPrintResultTable = true;
                } else {
                    positionsToSave.put(new TickerInfo.Key(correction.getName(), correction.getType()), correction);
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
}
