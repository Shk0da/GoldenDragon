package com.github.shk0da.goldendragon.repository;

import com.github.shk0da.goldendragon.model.TickerInfo;

/**
 * Repository for storing FIGI (Financial Instrument Global Identifier) mappings. Singleton instance
 * for ticker-to-FIGI conversion and caching.
 */
public class FigiRepository extends AbstractRepository<TickerInfo.Key, String> {
    public static final FigiRepository INSTANCE = new FigiRepository();
}
