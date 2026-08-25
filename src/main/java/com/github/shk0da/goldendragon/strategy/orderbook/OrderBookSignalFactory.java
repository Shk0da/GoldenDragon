package com.github.shk0da.goldendragon.strategy.orderbook;

import com.github.shk0da.goldendragon.config.OrderBookScalpConfig;
import com.github.shk0da.goldendragon.service.TCSService;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/** Builds enabled {@link OrderBookSignal} instances from configuration. */
public final class OrderBookSignalFactory {

    private OrderBookSignalFactory() {}

    public static List<OrderBookSignal> createEnabledSignals(
            TCSService tcsService, OrderBookScalpConfig config) {
        // CumulativeDeltaScalpSignal is the primary signal for HFT scalping
        // It implements both bounce and breakout scenarios from the spec
        List<OrderBookSignal> allSignals =
                List.of(
                        new CumulativeDeltaScalpSignal(tcsService, config),
                        new TradeFlowScalpSignal(config),
                        new MicropriceDriftSignal(config),
                        new DensityImbalanceSignal(config));
        Map<String, OrderBookSignal> available = new LinkedHashMap<>();
        for (OrderBookSignal signal : allSignals) {
            available.put(signal.id().toLowerCase(Locale.ROOT), signal);
        }

        List<OrderBookSignal> enabled = new ArrayList<>();
        for (String signalId : config.getEnabledSignals()) {
            String normalized = signalId.trim().toLowerCase(Locale.ROOT);
            OrderBookSignal signal = available.get(normalized);
            if (signal != null) {
                enabled.add(signal);
            }
        }
        if (enabled.isEmpty()) {
            // Default to CumulativeDeltaScalpSignal if no signals configured
            enabled.add(new CumulativeDeltaScalpSignal(tcsService, config));
        }
        return enabled;
    }
}
