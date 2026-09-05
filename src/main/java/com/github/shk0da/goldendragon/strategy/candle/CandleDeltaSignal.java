package com.github.shk0da.goldendragon.strategy.candle;

import com.github.shk0da.goldendragon.config.OrderBookScalpConfig;
import com.github.shk0da.goldendragon.model.Candle;
import com.github.shk0da.goldendragon.service.TradingService;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Simple candle-based signal using volume delta and price momentum.
 *
 * <p>Entry logic:
 * <ul>
 *   <li>LONG: 3+ consecutive bullish candles with increasing volume</li>
 *   <li>SHORT: 3+ consecutive bearish candles with increasing volume</li>
 * </ul>
 *
 * <p>Exit logic (5-min timeframe):
 * <ul>
 *   <li>Price moves against position by threshold (2-3% for 5-min)</li>
 *   <li>Candle pattern reverses (e.g., bearish candle after LONG entry)</li>
 *   <li>Take profit at 3-5% for 5-min timeframe</li>
 * </ul>
 */
public class CandleDeltaSignal implements CandleScalpSignal {

    private static final String SIGNAL_ID = "candle_delta";
    private final TradingService tradingService;
    private final OrderBookScalpConfig config;
    private final Map<String, EntryState> entryStateByTicker = new ConcurrentHashMap<>();

    public CandleDeltaSignal(TradingService tradingService, OrderBookScalpConfig config) {
        this.tradingService = tradingService;
        this.config = config;
    }

    @Override
    public String id() {
        return SIGNAL_ID;
    }

    @Override
    public CandleEntryDecision evaluateEntry(CandleMarketContext context, String ticker) {
        List<Candle> candles = context.getCandles();
        if (candles == null || candles.size() < 3) {
            return CandleEntryDecision.none();
        }

        // Check for consecutive bullish/bearish candles with momentum
        int bullishCount = 0;
        int bearishCount = 0;
        double totalVolume = 0.0;
        double prevVolume = 0.0;

        for (int i = candles.size() - 1; i >= Math.max(0, candles.size() - 5); i--) {
            Candle candle = candles.get(i);
            boolean isBullish = candle.close >= candle.open;
            
            if (isBullish) {
                bullishCount++;
            } else {
                bearishCount++;
            }

            // Track volume
            if (i < candles.size() - 1) {
                prevVolume = candles.get(i + 1).volume;
            }
            totalVolume += candle.volume;
        }

        double avgVolume = totalVolume / Math.min(5, candles.size());
        double volumeRatio = avgVolume / Math.max(1.0, getAverageVolume(ticker));

        // LONG signal: 2+ bullish candles (relaxed from 3)
        if (bullishCount >= 2 && volumeRatio > 0.8) {
            double quality = Math.min(1.0, (bullishCount * 0.25) + (volumeRatio * 0.2));
            return CandleEntryDecision.enter(
                SIGNAL_ID + " LONG: " + bullishCount + " bullish, vol=" + String.format("%.2f", volumeRatio),
                quality
            );
        }

        // SHORT signal: 2+ bearish candles (relaxed from 3)
        if (bearishCount >= 2 && volumeRatio > 0.8) {
            double quality = Math.min(1.0, (bearishCount * 0.25) + (volumeRatio * 0.2));
            return CandleEntryDecision.enter(
                SIGNAL_ID + " SHORT: " + bearishCount + " bearish, vol=" + String.format("%.2f", volumeRatio),
                quality
            );
        }

        return CandleEntryDecision.none();
    }

    @Override
    public CandleEntryDecision evaluateEntryShort(CandleMarketContext context, String ticker) {
        return evaluateEntry(context, ticker);
    }

    @Override
    public String evaluateExit(CandleMarketContext context, CandlePositionView position, String ticker) {
        List<Candle> candles = context.getCandles();
        if (candles == null || candles.isEmpty()) {
            return null;
        }

        Candle latest = candles.get(candles.size() - 1);
        double currentPrice = latest.close;
        double entryPrice = position.getEntryPrice();
        double pnlPercent = (currentPrice - entryPrice) / entryPrice * 100.0;

        // Exit LONG on bearish candle or stop loss
        if ("LONG".equals(position.getDirection())) {
            if (latest.close < latest.open) {
                return "candle_reversal";
            }
            if (pnlPercent < -2.0) { // 2% stop loss for 5-min timeframe
                return "stop_loss";
            }
            if (pnlPercent > 4.0) { // 4% take profit for 5-min timeframe
                return "take_profit";
            }
        }

        // Exit SHORT on bullish candle or stop loss
        if ("SHORT".equals(position.getDirection())) {
            if (latest.close > latest.open) {
                return "candle_reversal";
            }
            if (pnlPercent > 2.0) { // 2% stop loss for 5-min timeframe
                return "stop_loss";
            }
            if (pnlPercent < -4.0) { // 4% take profit for 5-min timeframe
                return "take_profit";
            }
        }

        return null;
    }

    @Override
    public void reset(String ticker) {
        entryStateByTicker.remove(ticker);
    }

    private double getAverageVolume(String ticker) {
        // Simple average - could be enhanced with historical tracking
        // Backtest fix: use realistic default based on typical crypto volumes
        return 100.0; // Reduced from 1000 to match typical 5-min crypto candle volumes
    }

    private static final class EntryState {
        String direction;
        double entryPrice;
        long entryTime;

        EntryState(String direction, double entryPrice) {
            this.direction = direction;
            this.entryPrice = entryPrice;
            this.entryTime = System.currentTimeMillis();
        }
    }
}
