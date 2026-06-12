package com.github.shk0da.goldendragon.strategy;

import com.github.shk0da.goldendragon.config.MarketConfig;
import com.github.shk0da.goldendragon.config.RebalanceConfig;
import com.github.shk0da.goldendragon.model.PortfolioPosition;
import com.github.shk0da.goldendragon.model.TickerInfo;
import com.github.shk0da.goldendragon.service.TCSService;
import java.util.Map;

/** CRON MOEX: Every Mon at 10:01 MSK */
public class Rebalance extends Rebalancing {

    private final TCSService tcsService;
    private final RebalanceConfig rebalanceConfig;

    public Rebalance(
            MarketConfig marketConfig, RebalanceConfig rebalanceConfig, TCSService tcsService) {
        super(marketConfig, tcsService);
        this.rebalanceConfig = rebalanceConfig;
        this.tcsService = tcsService;
    }

    public void run() {
        double totalPortfolioCost = tcsService.getAvailableCash();
        Map<TickerInfo.Key, PortfolioPosition> targetPositions =
                rebalanceConfig.getPortfolioPositions();
        doRebalance(totalPortfolioCost, targetPositions, rebalanceConfig.getPositionPercent());
    }
}
