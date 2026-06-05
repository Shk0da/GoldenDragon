package com.github.shk0da.GoldenDragon.repository;

import com.github.shk0da.GoldenDragon.model.TickerInfo;
import com.google.gson.reflect.TypeToken;
import java.util.Map;


import static com.github.shk0da.GoldenDragon.utils.SerializationUtils.loadDataFromDisk;

public class TickerRepository extends AbstractRepository<TickerInfo.Key, TickerInfo> {

    public static final String SERIALIZE_NAME = "tickers.json";
    public static final TickerRepository INSTANCE = new TickerRepository();

    public TickerRepository() {
        Map<TickerInfo.Key, TickerInfo> dataFromDisk = loadDataFromDisk(SERIALIZE_NAME, new TypeToken<>() {});
        if (null != dataFromDisk) {
            this.putAll(dataFromDisk);
        }
    }
}
