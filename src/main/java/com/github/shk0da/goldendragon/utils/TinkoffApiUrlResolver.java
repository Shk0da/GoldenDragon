package com.github.shk0da.goldendragon.utils;

import com.github.shk0da.goldendragon.config.MainConfig;

/**
 * Resolves Tinkoff Invest API URLs based on sandbox mode.
 */
public final class TinkoffApiUrlResolver {

    private TinkoffApiUrlResolver() {}

    /**
     * Base URL for sandbox environment.
     */
    private static final String SANDBOX_BASE_URL = "https://sandbox-invest-public-api.tbank.ru";

    /**
     * Base URL for production environment.
     */
    private static final String PRODUCTION_BASE_URL = "https://invest-public-api.tbank.ru";

    /**
     * Gets the base URL for Tinkoff Invest API based on sandbox mode.
     *
     * @param config the main configuration with sandbox setting
     * @return the appropriate base URL for the current environment
     */
    public static String resolveBaseUrl(MainConfig config) {
        return config.isSandbox() ? SANDBOX_BASE_URL : PRODUCTION_BASE_URL;
    }

    /**
     * Builds a full REST API endpoint URL.
     *
     * @param config the main configuration
     * @param endpoint the endpoint path (e.g., "InstrumentsService/GetFuturesMargin")
     * @return the complete REST URL
     */
    public static String buildRestUrl(MainConfig config, String endpoint) {
        return resolveBaseUrl(config) + "/rest/tinkoff.public.invest.api.contract.v1." + endpoint;
    }
}
