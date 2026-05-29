package com.github.shk0da.GoldenDragon.strategy;

import com.github.shk0da.GoldenDragon.config.UnifiedTraderConfig;
import com.github.shk0da.GoldenDragon.filters.MarketRegimeFilter;
import com.github.shk0da.GoldenDragon.model.Candle;
import com.github.shk0da.GoldenDragon.model.Config;
import com.github.shk0da.GoldenDragon.model.Position;
import com.github.shk0da.GoldenDragon.model.TradingDecision;
import com.github.shk0da.GoldenDragon.service.TCSService;
import java.util.List;

/**
 * Regime-aware strategy that switches between different strategies based on market conditions.
 * 
 * Market Regimes:
 * - TREND (ADX > 25): Use TurtleStrategy for trend following
 * - RANGE (ADX < 20): Use GerchikStrategy for range breakouts
 * - TRANSITION (ADX 20-25): Use UnifiedStrategy (balanced)
 */
public class RegimeAwareStrategy extends BaseStrategy {

    // Market regime thresholds
    private static final double ADX_TREND_THRESHOLD = 25.0;
    private static final double ADX_RANGE_THRESHOLD = 20.0;
    
    // Current market regime
    private enum MarketRegime {
        TREND,      // Strong trend - use Turtle
        RANGE,      // Sideways/range - use Gerchik
        TRANSITION, // Unclear - use Unified
        UNKNOWN     // Not enough data
    }

    // Sub-strategies
    private final UnifiedStrategy unifiedStrategy;
    private final TurtleStrategy turtleStrategy;
    private final GerchikStrategy gerchikStrategy;
    
    // Regime detection
    private final MarketRegimeFilter regimeFilter;
    private MarketRegime currentRegime = MarketRegime.UNKNOWN;
    private String lastRegimeChangeReason = "INIT";
    private int regimeChangeBar = 0;
    
    // Statistics
    private int trendBars = 0;
    private int rangeBars = 0;
    private int transitionBars = 0;

    public RegimeAwareStrategy(UnifiedTraderConfig unifiedTraderConfig, TCSService tcsService) {
        this(unifiedTraderConfig, tcsService, new Config(), false);
    }

    public RegimeAwareStrategy(UnifiedTraderConfig unifiedTraderConfig, TCSService tcsService, Config config) {
        this(unifiedTraderConfig, tcsService, config, false);
    }

    public RegimeAwareStrategy(UnifiedTraderConfig unifiedTraderConfig, TCSService tcsService, Config config, boolean isBacktest) {
        super(unifiedTraderConfig, tcsService, config, isBacktest);
        
        // Initialize regime filter
        this.regimeFilter = new MarketRegimeFilter(true, 14, 14, 50);
        
        // Initialize sub-strategies
        this.unifiedStrategy = new UnifiedStrategy(unifiedTraderConfig, tcsService, config, isBacktest);
        this.turtleStrategy = new TurtleStrategy(unifiedTraderConfig, tcsService, config, isBacktest);
        this.gerchikStrategy = new GerchikStrategy(unifiedTraderConfig, tcsService, config, isBacktest);
        
        logWithBacktest("RegimeAwareStrategy initialized: TREND(ADX>" + ADX_TREND_THRESHOLD + 
                        "), RANGE(ADX<" + ADX_RANGE_THRESHOLD + "), TRANSITION(between)");
    }

    @Override
    protected String getStrategyName() {
        return "RegimeAwareStrategy";
    }

    @Override
    public TradingDecision decide(String ticker,
                                   List<Candle> hourCandles,
                                   List<Candle> minuteCandles,
                                   Position position,
                                   double balance,
                                   boolean incrementCandlesHeld) {
        
        // Detect current market regime
        MarketRegime regime = detectMarketRegime(hourCandles);
        
        // Update statistics
        updateRegimeStats(regime);
        
        // Check if regime changed
        if (regime != currentRegime) {
            lastRegimeChangeReason = "REGIME_" + currentRegime + "_TO_" + regime;
            regimeChangeBar = hourCandles != null ? hourCandles.size() : 0;
            currentRegime = regime;
            
            logWithBacktest("Regime changed to: " + regime + " for " + ticker);
        }
        
        // Select and execute appropriate strategy
        TradingDecision baseDecision;
        switch (regime) {
            case TREND:
                baseDecision = turtleStrategy.decide(ticker, hourCandles, minuteCandles, position, balance, incrementCandlesHeld);
                break;
                
            case RANGE:
                baseDecision = gerchikStrategy.decide(ticker, hourCandles, minuteCandles, position, balance, incrementCandlesHeld);
                break;
                
            case TRANSITION:
            case UNKNOWN:
            default:
                baseDecision = unifiedStrategy.decide(ticker, hourCandles, minuteCandles, position, balance, incrementCandlesHeld);
                break;
        }
        
        // Wrap decision with regime prefix
        return new TradingDecision(
                baseDecision.action,
                regime + "_" + baseDecision.reason,
                baseDecision.confidence,
                baseDecision.quantity,
                baseDecision.stopLoss,
                baseDecision.takeProfit,
                baseDecision.entryPrice,
                baseDecision.updatedPosition
        );
    }

    /**
     * Detect market regime based on ADX and other indicators.
     */
    private MarketRegime detectMarketRegime(List<Candle> hourCandles) {
        if (hourCandles == null || hourCandles.size() < 60) {
            return MarketRegime.UNKNOWN;
        }
        
        // Use MarketRegimeFilter to evaluate market state
        MarketRegimeFilter.FilterResult result = regimeFilter.evaluate(hourCandles);
        
        // Extract ADX from result reason (format: "TREND_ATR_OK_V100_C80")
        // For more precise control, calculate ADX directly
        double adx = calculateAdx(hourCandles, 14);
        
        if (adx >= ADX_TREND_THRESHOLD) {
            return MarketRegime.TREND;
        } else if (adx <= ADX_RANGE_THRESHOLD) {
            return MarketRegime.RANGE;
        } else {
            return MarketRegime.TRANSITION;
        }
    }

    /**
     * Calculate ADX(14) for regime detection.
     */
    private double calculateAdx(List<Candle> candles, int period) {
        if (candles.size() < period * 2 + 10) {
            return 0.0;
        }
        
        int start = candles.size() - period;
        double trSum = 0.0, pdSum = 0.0, mdSum = 0.0;
        
        for (int i = start; i < candles.size(); i++) {
            Candle c = candles.get(i);
            Candle p = candles.get(i - 1);
            
            double tr = Math.max(Math.max(c.high - c.low, Math.abs(c.high - p.close)), Math.abs(c.low - p.close));
            trSum += tr;
            
            double up = c.high - p.high;
            double dn = p.low - c.low;
            
            pdSum += (up > dn && up > 0) ? up : 0.0;
            mdSum += (dn > up && dn > 0) ? dn : 0.0;
        }
        
        double atr = trSum / period;
        double diPlus = atr > 0 ? (pdSum / period) / atr * 100 : 0.0;
        double diMinus = atr > 0 ? (mdSum / period) / atr * 100 : 0.0;
        double adx = (diPlus + diMinus) > 0 ? Math.abs(diPlus - diMinus) / (diPlus + diMinus) * 100 : 0.0;
        
        return adx;
    }

    /**
     * Update regime statistics.
     */
    private void updateRegimeStats(MarketRegime regime) {
        switch (regime) {
            case TREND:
                trendBars++;
                break;
            case RANGE:
                rangeBars++;
                break;
            case TRANSITION:
                transitionBars++;
                break;
        }
    }

    protected void onDailyReset() {
        unifiedStrategy.onDailyReset();
        turtleStrategy.onDailyReset();
        gerchikStrategy.onDailyReset();
        
        logWithBacktest("RegimeAwareStrategy: Daily reset - Regime=" + currentRegime + 
                        " (Trend:" + trendBars + ", Range:" + rangeBars + ", Transition:" + transitionBars + ")");
    }

    @Override
    protected void onTradeClosed(String ticker, double pnl, double entryPrice,
                                  double exitPrice, int quantity, String direction) {
        super.onTradeClosed(ticker, pnl, entryPrice, exitPrice, quantity, direction);
        
        // Notify active strategy
        switch (currentRegime) {
            case TREND:
                turtleStrategy.onTradeClosed(ticker, pnl, entryPrice, exitPrice, quantity, direction);
                break;
            case RANGE:
                gerchikStrategy.onTradeClosed(ticker, pnl, entryPrice, exitPrice, quantity, direction);
                break;
            default:
                unifiedStrategy.onTradeClosed(ticker, pnl, entryPrice, exitPrice, quantity, direction);
                break;
        }
    }

    protected void closeAllPositions(TCSService tcsService, UnifiedTraderConfig config) {
        super.closeAllPositions(tcsService, config);
        unifiedStrategy.closeAllPositions(tcsService, config);
        turtleStrategy.closeAllPositions(tcsService, config);
        gerchikStrategy.closeAllPositions(tcsService, config);
    }

    /**
     * Get current market regime.
     */
    public MarketRegime getCurrentRegime() {
        return currentRegime;
    }

    /**
     * Get regime distribution statistics.
     */
    public String getRegimeStats() {
        int total = trendBars + rangeBars + transitionBars;
        if (total == 0) return "No data";
        
        return String.format("Trend: %.1f%%, Range: %.1f%%, Transition: %.1f%%",
                trendBars * 100.0 / total,
                rangeBars * 100.0 / total,
                transitionBars * 100.0 / total);
    }
}
