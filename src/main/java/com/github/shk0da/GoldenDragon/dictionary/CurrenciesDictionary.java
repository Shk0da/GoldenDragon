package com.github.shk0da.GoldenDragon.dictionary;

import java.util.HashMap;
import java.util.Map;

public class CurrenciesDictionary {

    private static final Map<String, String> dictionary = new HashMap<>(){{
        put("USD", "USD000UTSTOM");
        put("EUR", "EUR_RUB__TOM");
    }};

    public static String getTickerName(String currency) {
        if (dictionary.containsKey(currency)) {
            return dictionary.get(currency);
        }
        return currency;
    }
}
