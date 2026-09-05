package com.github.shk0da.goldendragon.service;

import com.github.shk0da.goldendragon.config.ByBitConfig;
import com.github.shk0da.goldendragon.config.MainConfig;
import com.github.shk0da.goldendragon.utils.PropertiesUtils;

import java.util.Properties;

/**
 * Factory for creating trading service instances based on configuration.
 * Supports switching between Tinkoff (TCS) and ByBit APIs.
 */
public class TradingServiceFactory {

    public enum TradingServiceType {
        TINKOFF,
        BYBIT
    }

    /**
     * Creates a trading service instance based on the {@code trading.service} property.
     *
     * @param mainConfig main application configuration (used for Tinkoff)
     * @return {@link TradingService} implementation (TCSService or ByBitService)
     * @throws Exception if service initialization fails
     */
    public static TradingService createTradingService(MainConfig mainConfig) throws Exception {
        Properties properties = PropertiesUtils.loadProperties();
        String serviceType = properties.getProperty("trading.service", "TINKOFF").toUpperCase();
        
        return createTradingService(TradingServiceType.valueOf(serviceType), mainConfig);
    }

    /**
     * Creates a trading service instance of the specified type.
     *
     * @param type the type of trading service to create
     * @param mainConfig main application configuration
     * @return {@link TradingService} implementation (TCSService or ByBitService)
     * @throws Exception if service initialization fails
     */
    public static TradingService createTradingService(TradingServiceType type, MainConfig mainConfig) throws Exception {
        switch (type) {
            case BYBIT:
                ByBitConfig byBitConfig = new ByBitConfig();
                return new ByBitService(byBitConfig);
            case TINKOFF:
            default:
                return new TCSService(mainConfig);
        }
    }

    /**
     * Returns the configured trading service type.
     *
     * @return the configured {@link TradingServiceType}
     */
    public static TradingServiceType getConfiguredServiceType() throws Exception {
        Properties properties = PropertiesUtils.loadProperties();
        String serviceType = properties.getProperty("trading.service", "TINKOFF").toUpperCase();
        return TradingServiceType.valueOf(serviceType);
    }
}
