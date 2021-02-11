package com.github.shk0da.GoldenDragon.repository;

import com.github.shk0da.GoldenDragon.model.TickerInfo;

public class FigiRepository extends AbstractRepository<TickerInfo.Key, String> {
    public static final FigiRepository INSTANCE = new FigiRepository();
}
