package com.github.shk0da.goldendragon.config;

import com.github.shk0da.goldendragon.filters.BadWeatherFilter;
import com.github.shk0da.goldendragon.utils.PropertiesUtils;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import static java.util.Arrays.stream;
import static java.util.stream.Collectors.toList;

public class UnifiedTraderConfig {

    public static class TickerParams {

        public final String group;
        public final double slMult;
        public final double tpMult;
        public final double riskP;
        public final boolean useMinuteCandles;
        public final String allocationGroup;
        public final double marketRegimeAdxRangeThreshold;
        public final double marketRegimeAdxUnclearThreshold;
        public final double marketRegimeVolumeRatioMin;
        public final double marketRegimeConfidenceMin;
        public final int marketRegimeAtrBars;
        public final BadWeatherFilter.Params badWeatherParams;
        public final boolean enabled;
        public final double allocationWeight;

        // Money Management parameters (per-ticker overrides)
        public final double mmRiskPercent;
        public final double mmAtrStopMultiplier;
        public final double mmTrailingMultiplier;
        public final boolean mmEnabled;
        public final int leverage;

        public TickerParams(
            String group,
            double slMult,
            double tpMult,
            double riskP,
            boolean useMinuteCandles) {
            this(
                group,
                slMult,
                tpMult,
                riskP,
                useMinuteCandles,
                "",
                20.0,
                25.0,
                30.0,
                50.0,
                4,
                new BadWeatherFilter.Params(),
                true,
                1.5,
                0.01,
                2.0,
                1.0,
                true,
                1);
        }

        public TickerParams(
            String group,
            double slMult,
            double tpMult,
            double riskP,
            boolean useMinuteCandles,
            String allocationGroup,
            double marketRegimeAdxRangeThreshold,
            double marketRegimeAdxUnclearThreshold,
            double marketRegimeVolumeRatioMin,
            double marketRegimeConfidenceMin,
            int marketRegimeAtrBars,
            BadWeatherFilter.Params badWeatherParams,
            boolean enabled,
            double allocationWeight) {
            this(
                group,
                slMult,
                tpMult,
                riskP,
                useMinuteCandles,
                allocationGroup,
                marketRegimeAdxRangeThreshold,
                marketRegimeAdxUnclearThreshold,
                marketRegimeVolumeRatioMin,
                marketRegimeConfidenceMin,
                marketRegimeAtrBars,
                badWeatherParams,
                enabled,
                allocationWeight,
                0.01,
                2.0,
                1.0,
                true,
                1);
        }

        public TickerParams(
            String group,
            double slMult,
            double tpMult,
            double riskP,
            boolean useMinuteCandles,
            String allocationGroup,
            double marketRegimeAdxRangeThreshold,
            double marketRegimeAdxUnclearThreshold,
            double marketRegimeVolumeRatioMin,
            double marketRegimeConfidenceMin,
            int marketRegimeAtrBars,
            BadWeatherFilter.Params badWeatherParams,
            boolean enabled,
            double allocationWeight,
            double mmRiskPercent,
            double mmAtrStopMultiplier,
            double mmTrailingMultiplier,
            boolean mmEnabled,
            int leverage) {
            this.group = group;
            this.slMult = slMult;
            this.tpMult = tpMult;
            this.riskP = riskP;
            this.useMinuteCandles = useMinuteCandles;
            this.allocationGroup = allocationGroup;
            this.marketRegimeAdxRangeThreshold = marketRegimeAdxRangeThreshold;
            this.marketRegimeAdxUnclearThreshold = marketRegimeAdxUnclearThreshold;
            this.marketRegimeVolumeRatioMin = marketRegimeVolumeRatioMin;
            this.marketRegimeConfidenceMin = marketRegimeConfidenceMin;
            this.marketRegimeAtrBars = marketRegimeAtrBars;
            this.badWeatherParams = badWeatherParams;
            this.enabled = enabled;
            this.allocationWeight = allocationWeight;
            this.mmRiskPercent = mmRiskPercent;
            this.mmAtrStopMultiplier = mmAtrStopMultiplier;
            this.mmTrailingMultiplier = mmTrailingMultiplier;
            this.mmEnabled = mmEnabled;
            this.leverage = leverage;
        }
    }

    private List<String> stocks;
    private boolean badWeatherFilterEnabled;
    private final int leverageMin;
    private final boolean adaptiveLeverageEnabled;
    private final boolean tmonCashParkingEnabled;
    private final Map<String, TickerParams> tickerParams;
    private final Properties properties;

    public UnifiedTraderConfig() throws IOException {
        final Properties properties = PropertiesUtils.loadProperties();
        this.properties = properties;
        stocks =
            stream(
                properties
                    .getProperty(
                        "levelTrader.instruments",
                        properties.getProperty("datacollector.instruments"))
                    .split(","))
                .collect(toList());
        badWeatherFilterEnabled =
            Boolean.parseBoolean(
                properties.getProperty("unifiedTrader.badWeatherFilter.enabled", "true"));
        leverageMin = Integer.parseInt(properties.getProperty("unifiedTrader.leverage.min", "1"));
        adaptiveLeverageEnabled =
            Boolean.parseBoolean(
                properties.getProperty("unifiedTrader.adaptiveLeverage.enabled", "true"));
        tmonCashParkingEnabled =
            Boolean.parseBoolean(
                properties.getProperty("unifiedTrader.tmonCashParking.enabled", "false"));
        this.tickerParams = loadTickerParams(properties);
    }

    private Map<String, TickerParams> loadTickerParams(Properties properties) {
        Map<String, TickerParams> result = new HashMap<>();
        for (String stock : stocks) {
            result.put(stock, loadSingleTickerParams(properties, stock));
        }
        return result;
    }

    private TickerParams loadSingleTickerParams(Properties properties, String ticker) {
        String prefix = "unifiedTrader.ticker." + ticker + ".";
        String group = properties.getProperty(prefix + "group", "TREND");
        double slMult =
            Double.parseDouble(
                properties.getProperty(
                    prefix + "slMult",
                    getGroupDefault(properties, group, "slMult", "1.2")));
        double tpMult =
            Double.parseDouble(
                properties.getProperty(
                    prefix + "tpMult",
                    getGroupDefault(properties, group, "tpMult", "2.5")));
        double riskP =
            Double.parseDouble(
                properties.getProperty(
                    prefix + "riskP",
                    getGroupDefault(properties, group, "riskP", "0.01")));
        boolean useMinuteCandles =
            Boolean.parseBoolean(properties.getProperty(prefix + "useMinuteCandles", "true"));
        String allocationGroup = properties.getProperty(prefix + "allocationGroup", "");

        double adxRangeThreshold =
            Double.parseDouble(
                properties.getProperty(prefix + "marketRegimeAdxRangeThreshold", "20.0"));
        double adxUnclearThreshold =
            Double.parseDouble(
                properties.getProperty(prefix + "marketRegimeAdxUnclearThreshold", "25.0"));
        double volumeRatioMin =
            Double.parseDouble(
                properties.getProperty(prefix + "marketRegimeVolumeRatioMin", "30.0"));
        double confidenceMin =
            Double.parseDouble(
                properties.getProperty(prefix + "marketRegimeConfidenceMin", "50.0"));
        int atrBars = Integer.parseInt(properties.getProperty(prefix + "marketRegimeAtrBars", "4"));

        double lowVolumeThreshold =
            Double.parseDouble(
                properties.getProperty(
                    prefix + "badWeatherLowVolumeThreshold",
                    properties.getProperty(
                        "unifiedTrader.badWeatherFilter.lowVolumeThreshold",
                        "0.5")));
        double lowAtrThreshold =
            Double.parseDouble(
                properties.getProperty(
                    prefix + "badWeatherLowAtrThreshold",
                    properties.getProperty(
                        "unifiedTrader.badWeatherFilter.lowAtrThreshold", "0.7")));
        double minRangePercent =
            Double.parseDouble(
                properties.getProperty(
                    prefix + "badWeatherMinRangePercent",
                    properties.getProperty(
                        "unifiedTrader.badWeatherFilter.minRangePercent",
                        "0.005")));
        double highAtrThreshold =
            Double.parseDouble(
                properties.getProperty(
                    prefix + "badWeatherHighAtrThreshold",
                    properties.getProperty(
                        "unifiedTrader.badWeatherFilter.highAtrThreshold", "2.0")));
        double maxSpreadPercent =
            Double.parseDouble(
                properties.getProperty(
                    prefix + "badWeatherMaxSpreadPercent",
                    properties.getProperty(
                        "unifiedTrader.badWeatherFilter.maxSpreadPercent",
                        "0.01")));
        double maxWickRatio =
            Double.parseDouble(
                properties.getProperty(
                    prefix + "badWeatherMaxWickRatio",
                    properties.getProperty(
                        "unifiedTrader.badWeatherFilter.maxWickRatio", "0.4")));
        double panicVolumeThreshold =
            Double.parseDouble(
                properties.getProperty(
                    prefix + "badWeatherPanicVolumeThreshold",
                    properties.getProperty(
                        "unifiedTrader.badWeatherFilter.panicVolumeThreshold",
                        "3.0")));
        double minAvgDailyVolume =
            Double.parseDouble(
                properties.getProperty(
                    prefix + "badWeatherMinAvgDailyVolume",
                    properties.getProperty(
                        "unifiedTrader.badWeatherFilter.minAvgDailyVolume",
                        "100000")));
        double atrSpikeThreshold =
            Double.parseDouble(
                properties.getProperty(
                    prefix + "badWeatherAtrSpikeThreshold",
                    properties.getProperty(
                        "unifiedTrader.badWeatherFilter.atrSpikeThreshold",
                        "2.5")));

        BadWeatherFilter.Params badWeatherParams =
            new BadWeatherFilter.Params(
                lowVolumeThreshold,
                lowAtrThreshold,
                minRangePercent,
                highAtrThreshold,
                maxSpreadPercent,
                maxWickRatio,
                panicVolumeThreshold,
                minAvgDailyVolume,
                atrSpikeThreshold);

        boolean enabled = Boolean.parseBoolean(properties.getProperty(prefix + "enabled", "true"));
        double allocationWeight =
            Double.parseDouble(properties.getProperty(prefix + "allocationWeight", "1.0"));

        // Money Management parameters (per-ticker overrides)
        double tickerMmRiskPercent =
            Double.parseDouble(
                properties.getProperty(prefix + "mmRiskPercent", String.valueOf(riskP)));
        double tickerMmAtrStopMultiplier =
            Double.parseDouble(properties.getProperty(prefix + "mmAtrStopMultiplier", "2.0"));
        double tickerMmTrailingMultiplier =
            Double.parseDouble(properties.getProperty(prefix + "mmTrailingMultiplier", "1.0"));
        boolean tickerMmEnabled =
            Boolean.parseBoolean(properties.getProperty(prefix + "mmEnabled", "true"));
        String globalLeverage =
            System.getProperty(
                "unifiedTrader.leverage",
                properties.getProperty("unifiedTrader.leverage", "1"));
        int leverage =
            Integer.parseInt(properties.getProperty(prefix + "leverage", globalLeverage));

        return new TickerParams(
            group,
            slMult,
            tpMult,
            riskP,
            useMinuteCandles,
            allocationGroup,
            adxRangeThreshold,
            adxUnclearThreshold,
            volumeRatioMin,
            confidenceMin,
            atrBars,
            badWeatherParams,
            enabled,
            allocationWeight,
            tickerMmRiskPercent,
            tickerMmAtrStopMultiplier,
            tickerMmTrailingMultiplier,
            tickerMmEnabled,
            leverage);
    }

    private String getGroupDefault(
        Properties properties, String group, String field, String defaultValue) {
        String prefix = "unifiedTrader.group." + group + ".";
        String value = properties.getProperty(prefix + field);
        return value != null ? value : defaultValue;
    }

    public TickerParams getTickerParams(String ticker) {
        TickerParams params = tickerParams.get(ticker);
        if (params != null) return params;
        return loadTickerParamsFor(ticker);
    }

    public String getTickerGroup(String ticker) {
        return getTickerParams(ticker).group;
    }

    private TickerParams loadTickerParamsFor(String ticker) {
        TickerParams tp = loadSingleTickerParams(properties, ticker);
        tickerParams.put(ticker, tp);
        return tp;
    }

    public List<String> getStocks() {
        return stocks;
    }

    public boolean isBadWeatherFilterEnabled() {
        return badWeatherFilterEnabled;
    }

    public int getLeverageMin() {
        return leverageMin;
    }

    public boolean isAdaptiveLeverageEnabled() {
        return adaptiveLeverageEnabled;
    }

    public boolean isTmonCashParkingEnabled() {
        return tmonCashParkingEnabled;
    }

    /**
     * Cooldown in candles after closing a position. Mirrors live {@code Config.cooldownCandles}.
     * Reads the same property key as live.
     */
    public int getCooldownCandles() {
        return Integer.parseInt(
            properties.getProperty("unifiedTrader.cooldownCandles", "3"));
    }

    /**
     * Controls verbose diagnostic logging (HOLD/DECISION summaries and per-cycle
     * parking/sizing details). Trade executions and errors are always logged.
     */
    public boolean isVerboseLoggingEnabled() {
        return Boolean.parseBoolean(
            properties.getProperty("unifiedTrader.verboseLogging.enabled", "true"));
    }

    /**
     * Controls logging of HOLD reasons. By default disabled to reduce log verbosity.
     * Trade executions and errors are always logged.
     */
    public boolean isLogHoldReasons() {
        return Boolean.parseBoolean(
            properties.getProperty("unifiedTrader.logHoldReasons", "false"));
    }

    /**
     * Controls logging of effective balance calculations. By default disabled to reduce log verbosity.
     */
    public boolean isLogEffectiveBalance() {
        return Boolean.parseBoolean(
            properties.getProperty("unifiedTrader.logEffectiveBalance", "false"));
    }

    /**
     * Hour candle history window in days fetched in live. Backtest trims history
     * to the same window so simulation sees the same data depth as live.
     */
    public int getLiveHourLookbackDays() {
        return Integer.parseInt(
            properties.getProperty("unifiedTrader.live.hourLookbackDays", "60"));
    }

    /**
     * Minute candle history window in hours fetched in live. Backtest trims history
     * to the same window so simulation sees the same data depth as live.
     */
    public int getLiveMinuteLookbackHours() {
        return Integer.parseInt(
            properties.getProperty("unifiedTrader.live.minuteLookbackHours", "72"));
    }

    // =====================================================
    // Backtest Configuration
    // =====================================================

    /**
     * Default Stop Loss percentage for backtest (e.g., 2.0 = 2%).
     * Mirrors SimulatedBroker.DEFAULT_SL_PERCENT
     */
    public double getBacktestDefaultSlPercent() {
        return Double.parseDouble(
            properties.getProperty("unifiedTrader.backtest.defaultSlPercent", "2.0"));
    }

    /**
     * Default Take Profit percentage for backtest (e.g., 4.0 = 4%).
     * Mirrors SimulatedBroker.DEFAULT_TP_PERCENT
     */
    public double getBacktestDefaultTpPercent() {
        return Double.parseDouble(
            properties.getProperty("unifiedTrader.backtest.defaultTpPercent", "4.0"));
    }

    /**
     * Short position margin ratio for backtest (e.g., 0.30 = 30%).
     * Mirrors SimulatedBroker.SHORT_MARGIN_RATIO
     */
    public double getBacktestShortMarginRatio() {
        return Double.parseDouble(
            properties.getProperty("unifiedTrader.backtest.shortMarginRatio", "0.30"));
    }

    /**
     * Maximum concurrent positions for backtest.
     * Mirrors SimulatedBroker.MAX_CONCURRENT_POSITIONS
     */
    public int getBacktestMaxConcurrentPositions() {
        return Integer.parseInt(
            properties.getProperty("unifiedTrader.backtest.maxConcurrentPositions", "8"));
    }

    /**
     * Regime filter configuration for UnifiedStrategy.
     */
    public RegimeFilterParams getRegimeFilterConfig() {
        return RegimeFilterParams.load(properties, "unifiedTrader.");
    }

    /**
     * Per-ticker regime filter configuration.
     */
    public RegimeFilterParams getTickerRegimeFilterConfig(String ticker) {
        String prefix = "unifiedTrader.ticker." + ticker + ".";
        return RegimeFilterParams.load(properties, prefix);
    }

    @Override
    public String toString() {
        return "UnifiedTraderConfig{stocks=" + stocks + '}';
    }

    // =====================================================
    // Regime Filter Configuration
    // =====================================================

    /**
     * Parameters for market regime filtering.
     *
     * <p>Controls whether trades are filtered based on ADX-derived market regime:
     *
     * <ul>
     *   <li>RANGE (ADX < rangeAdxMax) → skip trades
     *   <li>NORMAL (ADX rangeAdxMax to trendAdxMin) → filter weak signals
     *   <li>TREND (ADX > trendAdxMin) → allow all signals
     * </ul>
     */
    public static class RegimeFilterParams {
        public final boolean enabled;
        public final String mode;  // "RANGE_SKIP" or "FULL"
        public final double rangeAdxMax;
        public final double trendAdxMin;
        public final double normalMinAdx;

        public RegimeFilterParams(
                boolean enabled, String mode,
                double rangeAdxMax, double trendAdxMin, double normalMinAdx) {
            this.enabled = enabled;
            this.mode = mode;
            this.rangeAdxMax = rangeAdxMax;
            this.trendAdxMin = trendAdxMin;
            this.normalMinAdx = normalMinAdx;
        }

        /**
         * Load regime filter configuration from properties.
         *
         * @param properties the properties object
         * @param prefix property prefix (e.g., "unifiedTrader." or "unifiedTrader.ticker.X.")
         * @return loaded RegimeFilterParams instance
         */
        public static RegimeFilterParams load(Properties properties, String prefix) {
            boolean enabled = Boolean.parseBoolean(
                properties.getProperty(prefix + "regimeFilter.enabled", "false"));
            String mode = properties.getProperty(prefix + "regimeFilter.mode", "FULL");
            double rangeAdxMax = Double.parseDouble(
                properties.getProperty(prefix + "regimeFilter.rangeAdxMax", "16.0"));
            double trendAdxMin = Double.parseDouble(
                properties.getProperty(prefix + "regimeFilter.trendAdxMin", "26.0"));
            double normalMinAdx = Double.parseDouble(
                properties.getProperty(prefix + "regimeFilter.normalMinAdx", "18.0"));

            return new RegimeFilterParams(enabled, mode, rangeAdxMax, trendAdxMin, normalMinAdx);
        }
    }
}
