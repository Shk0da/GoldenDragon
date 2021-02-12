package com.github.shk0da.GoldenDragon.strategy;

import com.github.shk0da.GoldenDragon.config.MainConfig;
import com.github.shk0da.GoldenDragon.config.MarketConfig;
import com.github.shk0da.GoldenDragon.config.RebalanceConfig;
import com.github.shk0da.GoldenDragon.model.PortfolioPosition;
import com.github.shk0da.GoldenDragon.model.TickerInfo;
import com.github.shk0da.GoldenDragon.service.TCSService;
import com.google.gson.reflect.TypeToken;

import java.util.Map;

import static com.github.shk0da.GoldenDragon.utils.SerializationUtils.loadDataFromDisk;
import static com.github.shk0da.GoldenDragon.utils.SerializationUtils.saveDataToDisk;

/**
 * CRON MOEX: Every Mon at 10:01 MSK
 */
public class Rebalance extends Rebalancing {

    private final TCSService tcsService;

    private final RebalanceConfig rebalanceConfig;

    public Rebalance(MainConfig mainConfig, MarketConfig marketConfig, RebalanceConfig rebalanceConfig, TCSService tcsService) {
        super(mainConfig, marketConfig, tcsService);
        this.rebalanceConfig = rebalanceConfig;
        this.tcsService = tcsService;
    }

    public void run() throws Exception {
        double totalPortfolioCost = tcsService.getAvailableCash();
        Map<TickerInfo.Key, PortfolioPosition> previousPositions = loadDataFromDisk(RebalanceConfig.SERIALIZE_NAME, new TypeToken<>() {});
        Map<TickerInfo.Key, PortfolioPosition> targetPositions = rebalanceConfig.getPortfolioPositions();
        Map<TickerInfo.Key, PortfolioPosition> positionsToSave = doRebalance(totalPortfolioCost, previousPositions, targetPositions, rebalanceConfig.getPositionPercent());
        if (!positionsToSave.isEmpty()) {
            saveDataToDisk(RebalanceConfig.SERIALIZE_NAME, positionsToSave);
        }
    }
}
