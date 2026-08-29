package com.github.shk0da.goldendragon.utils;

import com.github.shk0da.goldendragon.model.TickerInfo;
import com.github.shk0da.goldendragon.model.TickerType;
import com.github.shk0da.goldendragon.repository.TickerRepository;
import java.util.Map;

/**
 * Utility class for resolving TickerType by ticker name. Uses ticker repository to determine the
 * instrument type automatically.
 */
public class TickerTypeResolver {

    private static final TickerRepository tickerRepository = TickerRepository.INSTANCE;

    /**
     * Resolves TickerType by ticker name using ticker repository. Searches through all available
     * instruments to find the matching type.
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

            TickerInfo tickerInfo =
                    allTickers.values().stream()
                            .filter(
                                    it ->
                                            it.getName().equalsIgnoreCase(normalizedTicker)
                                                    || it.getTicker()
                                                            .equalsIgnoreCase(normalizedTicker))
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
}
