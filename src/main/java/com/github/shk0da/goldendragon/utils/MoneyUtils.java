package com.github.shk0da.goldendragon.utils;

/**
 * Utility class for Tinkoff Money API parsing.
 * Tinkoff represents money amounts as {units: long, nano: int}.
 */
public final class MoneyUtils {

    private MoneyUtils() {
        // Utility class
    }

    /**
     * Parse Tinkoff money object: units + nano / 1_000_000_000.0.
     */
    public static double parseUnitsNano(long units, int nano) {
        return units + (nano / 1_000_000_000.0);
    }
}
