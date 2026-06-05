package com.github.shk0da.GoldenDragon.utils;

import com.github.shk0da.GoldenDragon.model.TickerInfo;
import com.github.shk0da.GoldenDragon.model.TickerType;
import com.github.shk0da.GoldenDragon.repository.TickerRepository;

import java.util.Map;

/**
 * Utility class for resolving TickerType by ticker name.
 * Uses ticker repository to determine the instrument type automatically.
 */
public class TickerTypeResolver {

    private static final TickerRepository tickerRepository = TickerRepository.INSTANCE;

    /**
     * Resolves TickerType by ticker name using ticker repository.
     * Searches through all available instruments to find the matching type.
     *
     * @param ticker ticker symbol to resolve
     * @return resolved TickerType, or TickerType.UNKNOWN if not found
     */
    public static TickerType resolve(String ticker) {
        if (ticker == null || ticker.trim().isEmpty()) {
            return TickerType.UNKNOWN;
        }

        String normalizedTicker = ticker.trim();

        try {
            Map<TickerInfo.Key, TickerInfo> allTickers = tickerRepository.getAll();

            TickerInfo tickerInfo = allTickers.values().stream()
                    .filter(it -> it.getName().equalsIgnoreCase(normalizedTicker) ||
                                  it.getTicker().equalsIgnoreCase(normalizedTicker))
                    .findFirst()
                    .orElse(null);

            if (tickerInfo != null) {
                return tickerInfo.getType();
            }

            // Fallback heuristics for common patterns
            if (normalizedTicker.endsWith("F")) {
                return TickerType.FEATURE;
            }

            // Default to STOCK for unknown tickers
            return TickerType.STOCK;
        } catch (Exception e) {
            // Fallback heuristics in case of repository access errors
            if (normalizedTicker.endsWith("F")) {
                return TickerType.FEATURE;
            }
            return TickerType.STOCK;
        }
    }

    /**
     * Resolves TickerType by ticker name with a default fallback.
     *
     * @param ticker ticker symbol to resolve
     * @param defaultType default type to return if resolution fails
     * @return resolved TickerType, or defaultType if not found
     */
    public static TickerType resolve(String ticker, TickerType defaultType) {
        TickerType resolved = resolve(ticker);
        return resolved != TickerType.UNKNOWN ? resolved : defaultType;
    }

    /**
     * Checks if the ticker is a stock.
     *
     * @param ticker ticker symbol to check
     * @return true if the ticker is a stock
     */
    public static boolean isStock(String ticker) {
        return resolve(ticker) == TickerType.STOCK;
    }

    /**
     * Checks if the ticker is a future.
     *
     * @param ticker ticker symbol to check
     * @return true if the ticker is a future
     */
    public static boolean isFuture(String ticker) {
        return resolve(ticker) == TickerType.FEATURE;
    }

    /**
     * Checks if the ticker is an ETF.
     *
     * @param ticker ticker symbol to check
     * @return true if the ticker is an ETF
     */
    public static boolean isEtf(String ticker) {
        return resolve(ticker) == TickerType.ETF;
    }

    /**
     * Checks if the ticker is a bond.
     *
     * @param ticker ticker symbol to check
     * @return true if the ticker is a bond
     */
    public static boolean isBond(String ticker) {
        return resolve(ticker) == TickerType.BOND;
    }

    /**
     * Checks if the ticker is a currency.
     *
     * @param ticker ticker symbol to check
     * @return true if the ticker is a currency
     */
    public static boolean isCurrency(String ticker) {
        return resolve(ticker) == TickerType.CURRENCY;
    }
}
