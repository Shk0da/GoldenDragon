package com.github.shk0da.goldendragon.strategy.candle;

import com.github.shk0da.goldendragon.config.OrderBookScalpConfig;
import com.github.shk0da.goldendragon.service.TradingService;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * Builds enabled {@link CandleScalpSignal} instances from configuration.
 */
public final class CandleSignalFactory {

    private CandleSignalFactory() {}

    public static List<CandleScalpSignal> createEnabledSignals(
            TradingService tradingService, OrderBookScalpConfig config) {
        // Primary candle signal for scalping
        List<CandleScalpSignal> allSignals = new ArrayList<>();
        allSignals.add(new CandleDeltaSignal(tradingService, config));
        
        Map<String, CandleScalpSignal> available = new LinkedHashMap<>();
        for (CandleScalpSignal signal : allSignals) {
            available.put(signal.id().toLowerCase(Locale.ROOT), signal);
        }

        List<CandleScalpSignal> enabled = new ArrayList<>();
        for (String signalId : config.getEnabledSignals()) {
            String normalized = signalId.trim().toLowerCase(Locale.ROOT);
            CandleScalpSignal signal = available.get(normalized);
            if (signal != null) {
                enabled.add(signal);
            }
        }
        if (enabled.isEmpty()) {
            enabled.add(new CandleDeltaSignal(tradingService, config));
        }
        return enabled;
    }
}
