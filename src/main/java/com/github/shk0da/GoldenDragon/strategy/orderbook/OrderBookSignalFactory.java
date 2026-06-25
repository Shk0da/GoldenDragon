package com.github.shk0da.goldendragon.strategy.orderbook;

import com.github.shk0da.goldendragon.config.OrderBookScalpConfig;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/** Builds enabled {@link OrderBookSignal} instances from configuration. */
public final class OrderBookSignalFactory {

    private OrderBookSignalFactory() {}

    public static List<OrderBookSignal> createEnabledSignals(OrderBookScalpConfig config) {
        List<OrderBookSignal> allSignals =
                List.of(
                        new ObiScalpSignal(config),
                        new TradeFlowScalpSignal(config),
                        new MicropriceDriftSignal(config));
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
            enabled.add(new ObiScalpSignal(config));
        }
        return enabled;
    }
}
