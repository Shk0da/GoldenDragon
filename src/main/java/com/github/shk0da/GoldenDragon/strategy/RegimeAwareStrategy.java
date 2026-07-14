package com.github.shk0da.goldendragon.strategy;

import com.github.shk0da.goldendragon.config.UnifiedTraderConfig;
import com.github.shk0da.goldendragon.model.Candle;
import com.github.shk0da.goldendragon.model.Config;
import com.github.shk0da.goldendragon.model.Position;
import com.github.shk0da.goldendragon.model.TradingDecision;
import com.github.shk0da.goldendragon.service.TCSService;
import java.util.List;

/**
 * Regime-aware strategy - filters trades based on market regime.
 *
 * <p>Detects market regime via ADX:
 *
 * <ul>
 *   <li>TREND (ADX > 26): Allow trend signals
 *   <li>NORMAL (ADX 18-26): Allow only strong signals, skip FX
 *   <li>RANGE (ADX < 16): Skip all entries
 * </ul>
 *
 * <p>UnifiedStrategy handles all signal generation and position sizing.
 */
public class RegimeAwareStrategy extends BaseStrategy {

    // Market regime thresholds
    private static final double ADX_TREND_THRESHOLD = 26.0;
    private static final double ADX_HOT_THRESHOLD = 35.0;
    private static final double ADX_RANGE_THRESHOLD = 16.0;
    private static final double NORMAL_MIN_ADX = 18.0;

    // UnifiedStrategy instance
    private final UnifiedStrategy unifiedStrategy;

    // Regime tracking
    private int trendBars = 0;
    private int rangeBars = 0;
    private int normalBars = 0;

    private enum MarketRegime {
        TREND,
        RANGE,
        NORMAL,
        UNKNOWN
    }

    public RegimeAwareStrategy(
            UnifiedTraderConfig unifiedTraderConfig,
            TCSService tcsService,
            Config config,
            boolean isBacktest) {
        super(unifiedTraderConfig, tcsService, config, isBacktest);

        // Single UnifiedStrategy instance
        this.unifiedStrategy =
                new UnifiedStrategy(unifiedTraderConfig, tcsService, config, isBacktest);

        logWithBacktest(
                "RegimeAwareStrategy: Regime filter (TREND:ADX>"
                        + ADX_TREND_THRESHOLD
                        + ", RANGE:ADX<"
                        + ADX_RANGE_THRESHOLD
                        + ")");
    }

    @Override
    protected String getStrategyName() {
        return "RegimeAwareStrategy";
    }

    @Override
    public TradingDecision decide(
            String ticker,
            List<Candle> hourCandles,
            List<Candle> minuteCandles,
            Position position,
            double balance,
            boolean incrementCandlesHeld) {

        // Detect market regime
        MarketRegime regime = MarketRegime.UNKNOWN;
        double adx = 0.0;

        if (hourCandles != null && hourCandles.size() >= 60) {
            adx = calculateAdx(hourCandles, 14);

            if (adx >= ADX_TREND_THRESHOLD) {
                regime = MarketRegime.TREND;
                trendBars++;
            } else if (adx <= ADX_RANGE_THRESHOLD) {
                regime = MarketRegime.RANGE;
                rangeBars++;
            } else {
                regime = MarketRegime.NORMAL;
                normalBars++;
            }
        }

        if (MarketRegime.RANGE == regime) {
            return new TradingDecision("HOLD", "RANGE_SKIP_ADX" + (int) adx);
        }

        if (MarketRegime.NORMAL == regime && adx < NORMAL_MIN_ADX) {
            return new TradingDecision("HOLD", "NORMAL_WEAK_ADX" + (int) adx);
        }

        // Call UnifiedStrategy with actual balance (no artificial size multiplier)
        TradingDecision decision =
                unifiedStrategy.decide(
                        ticker,
                        hourCandles,
                        minuteCandles,
                        position,
                        balance,
                        incrementCandlesHeld);

        if ("OPEN".equals(decision.action) && decision.reason != null) {
            if (MarketRegime.NORMAL == regime && decision.reason.startsWith("FX")) {
                return new TradingDecision("HOLD", "NORMAL_SKIP_FX_" + decision.reason);
            }
            if (adx < ADX_TREND_THRESHOLD && decision.reason.startsWith("TB_4")) {
                return new TradingDecision("HOLD", "WEAK_TREND_SKIP_" + decision.reason);
            }
        }

        // Wrap decision with regime info
        return new TradingDecision(
                decision.action,
                regime + "_ADX" + (int) adx + "_" + decision.reason,
                decision.confidence,
                decision.quantity,
                decision.stopLoss,
                decision.takeProfit,
                decision.entryPrice,
                decision.updatedPosition);
    }

    /** Calculate ADX(14) for regime detection. */
    private double calculateAdx(List<Candle> candles, int period) {
        if (candles.size() < period * 2 + 10) {
            return 0.0;
        }

        int start = candles.size() - period;
        double trSum = 0.0, pdSum = 0.0, mdSum = 0.0;

        for (int i = start; i < candles.size(); i++) {
            Candle c = candles.get(i);
            Candle p = candles.get(i - 1);

            double tr =
                    Math.max(
                            Math.max(c.high - c.low, Math.abs(c.high - p.close)),
                            Math.abs(c.low - p.close));
            trSum += tr;

            double up = c.high - p.high;
            double dn = p.low - c.low;

            pdSum += (up > dn && up > 0) ? up : 0.0;
            mdSum += (dn > up && dn > 0) ? dn : 0.0;
        }

        double atr = trSum / period;
        double diPlus = atr > 0 ? (pdSum / period) / atr * 100 : 0.0;
        double diMinus = atr > 0 ? (mdSum / period) / atr * 100 : 0.0;
        double adx =
                (diPlus + diMinus) > 0
                        ? Math.abs(diPlus - diMinus) / (diPlus + diMinus) * 100
                        : 0.0;

        return adx;
    }

    @Override
    protected void onDailyReset() {
        unifiedStrategy.onDailyReset();

        int total = trendBars + rangeBars + normalBars;
        if (total > 0) {
            logWithBacktest(
                    "RegimeAwareStrategy v4: Daily - Trend:"
                            + trendBars
                            + "("
                            + (trendBars * 100 / total)
                            + "%), Range:"
                            + rangeBars
                            + "("
                            + (rangeBars * 100 / total)
                            + "%), Normal:"
                            + normalBars
                            + "("
                            + (normalBars * 100 / total)
                            + "%)");
        }
    }

    @Override
    protected void onTradeClosed(
            String ticker,
            double pnl,
            double entryPrice,
            double exitPrice,
            int quantity,
            String direction) {
        unifiedStrategy.onTradeClosed(ticker, pnl, entryPrice, exitPrice, quantity, direction);
    }

    @Override
    protected void closeAllPositions(TCSService tcsService, UnifiedTraderConfig config) {
        unifiedStrategy.closeAllPositions(tcsService, config);
    }
}
