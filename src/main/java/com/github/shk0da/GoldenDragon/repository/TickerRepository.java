package com.github.shk0da.GoldenDragon.repository;

import com.github.shk0da.GoldenDragon.model.TickerInfo;

public class TickerRepository extends AbstractRepository<TickerInfo.Key, TickerInfo> {

    public static final String SERIALIZE_NAME = "tickers.json";
    public static final TickerRepository INSTANCE = new TickerRepository();

}
