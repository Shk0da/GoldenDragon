package com.github.shk0da.GoldenDragon.filters;

import com.github.shk0da.GoldenDragon.model.Candle;
import java.util.List;
import java.util.Map;

/**
 * Group confirmation filter for peer validation.
 * Checks if correlated instruments confirm the trading signal.
 * Requires minimum number of peers moving in the same direction.
 */
public class GroupConfirmationFilter {

    private static final int LOOKBACK_BARS = 3;
    private static final int MIN_PEER_CONFIRMATIONS = 2;

    public static boolean isConfirmed(String ticker, boolean isBuy,
                                       Map<String, List<Candle>> peerCandles) {
        if (peerCandles == null || peerCandles.isEmpty()) return true;

        int confirmations = 0;
        for (Map.Entry<String, List<Candle>> entry : peerCandles.entrySet()) {
            if (entry.getKey().equals(ticker)) continue;
            List<Candle> candles = entry.getValue();
            if (candles == null || candles.size() < LOOKBACK_BARS + 1) continue;

            boolean peerUp = candles.get(candles.size() - 1).close
                    > candles.get(candles.size() - 1 - LOOKBACK_BARS).close;
            if (isBuy == peerUp) confirmations++;
        }

        return confirmations >= MIN_PEER_CONFIRMATIONS;
    }
}
