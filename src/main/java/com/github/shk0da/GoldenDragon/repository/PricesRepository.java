package com.github.shk0da.GoldenDragon.repository;

import java.util.Map;

public class PricesRepository extends AbstractRepository<String, Map<String, Map<Double, Integer>>> {
    public static final PricesRepository INSTANCE = new PricesRepository();
}
