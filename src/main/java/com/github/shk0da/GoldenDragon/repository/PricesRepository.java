package com.github.shk0da.GoldenDragon.repository;

import com.github.shk0da.GoldenDragon.model.TickerInfo;

import java.util.Map;

public class PricesRepository extends AbstractRepository<TickerInfo.Key, Map<String, Map<Double, Integer>>> {
    public static final PricesRepository INSTANCE = new PricesRepository();
}
