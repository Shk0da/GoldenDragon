package com.github.shk0da.GoldenDragon.strategy;

import com.github.shk0da.GoldenDragon.config.MainConfig;
import com.github.shk0da.GoldenDragon.config.MarketConfig;
import com.github.shk0da.GoldenDragon.model.TickerInfo;
import com.github.shk0da.GoldenDragon.repository.Repository;
import com.github.shk0da.GoldenDragon.repository.TickerRepository;
import com.github.shk0da.GoldenDragon.service.TCSService;

/**
 * CRON SPB: Every Mon at 17:00 MSK
 */
public class RSX {

    private final MainConfig config;
    private final MarketConfig marketConfig;

    private final TCSService tcsService;

    private final Repository<TickerInfo.Key, TickerInfo> tickerRepository = TickerRepository.INSTANCE;

    public RSX(MainConfig config, MarketConfig marketConfig, TCSService tcsService) {
        this.config = config;
        this.marketConfig = marketConfig;
        this.tcsService = tcsService;
    }

    public void run() {
        // -
    }
}
