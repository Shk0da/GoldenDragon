package com.github.shk0da.goldendragon.repository;

import com.github.shk0da.goldendragon.model.TickerInfo;
import java.util.Map;

/**
 * Repository for storing current market prices (bids/asks). Singleton instance for caching order
 * book data by ticker.
 */
public class PricesRepository
        extends AbstractRepository<TickerInfo.Key, Map<String, Map<Double, Integer>>> {
    public static final PricesRepository INSTANCE = new PricesRepository();
}
