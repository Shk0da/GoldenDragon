/**
 * Trading signal filters for GoldenDragon application.
 *
 * <h2>Package Purpose</h2>
 * <p>The {@code filters} package contains classes for filtering trading signals before execution.
 * Filters check market conditions, confirmation from correlated instruments,
 * extreme market situations. Used by strategies to reduce false entries.</p>
 *
 * <h2>Key Filters</h2>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.filters.MarketRegimeFilter} — market regime filter.
 *       Detects trend/range by ADX (Average Directional Index), volatility (ATR), volume.
 *       Returns trading permission with confidence score and position multiplier.
 *       <br><b>Parameters</b>: ADX period (14), ATR period (14), volume period (50).</li>
 *
 *   <li>{@link com.github.shk0da.GoldenDragon.filters.GroupConfirmationFilter} — group
 *       confirmation filter. Checks if correlated instruments (peer instruments)
 *       move in the same direction. Requires minimum confirmations (min 2 of 3).
 *       <br><b>Usage</b>: {@code UnifiedStrategy} for signal validation.</li>
 *
 *   <li>{@link com.github.shk0da.GoldenDragon.filters.BadWeatherFilter} — "bad weather" filter.
 *       Prohibits trading under unfavorable conditions:
 *       <ul>
 *         <li>Low volume (below average * threshold).</li>
 *         <li>Low volatility (ATR below average).</li>
 *         <li>Abnormally high spread.</li>
 *         <li>Large candle with long wicks (panic candle).</li>
 *         <li>Volatility spike (ATR spike).</li>
 *       </ul>
 *       <br><b>Parameters</b>: configurable thresholds for each condition.</li>
 * </ul>
 *
 * <h2>Strategy Integration</h2>
 * <p>Filters are called in strategy {@code decide()} method before signal generation:</p>
 * <pre>{@code
 * // Example usage in BaseStrategy
 * MarketRegimeFilter.FilterResult regimeResult = marketRegimeFilter.checkRegime(ticker, candles);
 * if (!regimeResult.canTrade) return TradingDecision.HOLD(regimeResult.reason);
 *
 * boolean groupConfirmed = GroupConfirmationFilter.isConfirmed(ticker, isBuy, peerCandles);
 * if (!groupConfirmed) return TradingDecision.HOLD("noGroupConf");
 *
 * BadWeatherFilter.WeatherCondition weather = badWeatherFilter.checkWeather(candles);
 * if (!weather.isGood) return TradingDecision.HOLD("badWeather");
 * }</pre>
 *
 * <h2>Filter Results</h2>
 * <p>Filters return:</p>
 * <ul>
 *   <li>Trading permission (boolean).</li>
 *   <li>Confidence score (0.0–1.0).</li>
 *   <li>Position multiplier for size reduction during uncertainty.</li>
 *   <li>Rejection reason for logging.</li>
 * </ul>
 *
 * <h2>Thread Safety</h2>
 * <p>Filters are stateless and thread-safe. Parameters are set in constructor and immutable.
 * Methods can be called from different strategy threads.</p>
 *
 * @see com.github.shk0da.GoldenDragon.strategy.BaseStrategy
 * @see com.github.shk0da.GoldenDragon.strategy.UnifiedStrategy
 */
package com.github.shk0da.GoldenDragon.filters;
