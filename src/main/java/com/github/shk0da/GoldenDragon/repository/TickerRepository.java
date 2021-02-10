package com.github.shk0da.GoldenDragon.repository;

import java.util.Map;

public class TickerRepository extends AbstractRepository<String, Map<String, Object>> {
    public static final TickerRepository INSTANCE = new TickerRepository();
}
