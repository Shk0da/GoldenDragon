package com.github.shk0da.GoldenDragon.strategy;

import com.github.shk0da.GoldenDragon.config.MainConfig;
import com.github.shk0da.GoldenDragon.config.MarketConfig;
import com.github.shk0da.GoldenDragon.repository.Repository;
import com.github.shk0da.GoldenDragon.repository.TickerRepository;
import com.github.shk0da.GoldenDragon.service.TCSService;

import java.util.Map;

/**
 * CRON SPB: Every Mon at 17:00 MSK
 */
public class RSX {

    private final MainConfig config;
    private final MarketConfig marketConfig;

    private final TCSService tcsService;

    private final Repository<String, Map<String, Object>> tickerRepository = TickerRepository.INSTANCE;

    public RSX(MainConfig config, TCSService tcsService) {
        this.config = config;
        this.marketConfig = config.getMarketConfig();
        this.tcsService = tcsService;
    }
}
