/**
 * Model classes and data structures for GoldenDragon application.
 *
 * <h2>Package Purpose</h2>
 * <p>The {@code model} package contains data transfer objects (DTOs), interfaces, and enums
 * for representing market data, positions, instruments, and trading decisions. These classes
 * are used by all system components: strategies, services, repositories.</p>
 *
 * <h2>Market Data</h2>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.model.Candle} — candlestick (OHLCV) for backtests and analysis.</li>
 *   <li>{@link com.github.shk0da.GoldenDragon.model.MarketDepthSnapshot} — order book snapshot (bids/asks).</li>
 *   <li>{@link com.github.shk0da.GoldenDragon.model.MarketDepthLevel} — order book level (price/quantity).</li>
 *   <li>{@link com.github.shk0da.GoldenDragon.model.MarketTradeTick} — last trade tick.</li>
 * </ul>
 *
 * <h2>Instruments and Tickers</h2>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.model.TickerInfo} — instrument information:
 *       FIGI, ISIN, currency, type, lot.</li>
 *   <li>{@link com.github.shk0da.GoldenDragon.model.TickerType} — instrument type enum:
 *       STOCK, BOND, ETF, CURRENCY, FEATURE.</li>
 *   <li>{@link com.github.shk0da.GoldenDragon.model.Market} — market enum: MOEX.</li>
 *   <li>{@link com.github.shk0da.GoldenDragon.model.Group} — instrument group enum:
 *       TREND, FX, MIXED.</li>
 * </ul>
 *
 * <h2>Positions and Portfolio</h2>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.model.Position} — trading position:
 *       direction, quantity, entry price.</li>
 *   <li>{@link com.github.shk0da.GoldenDragon.model.PositionInfo} — position info from API:
 *       balance, lots, expected yield.</li>
 *   <li>{@link com.github.shk0da.GoldenDragon.model.PortfolioPosition} — target portfolio position:
 *       name, type, portfolio percentage.</li>
 * </ul>
 *
 * <h2>Trading Decisions</h2>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.model.TradingDecision} — strategy decision:
 *       action (BUY/SELL/HOLD/CLOSE), signal, quantity, prices.</li>
 * </ul>
 *
 * <h2>Specialized Models</h2>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.model.DiviTicker} — dividend information:
 *       ticker, cut-off date, dividend, yield.</li>
 *   <li>{@link com.github.shk0da.GoldenDragon.model.TickerScan} — TradingView scan result:
 *       analyst recommendation, debt/equity.</li>
 *   <li>{@link com.github.shk0da.GoldenDragon.model.TickerJson} — serializable data for LevelTrader:
 *       ticker + support/resistance levels.</li>
 *   <li>{@link com.github.shk0da.GoldenDragon.model.Config} — configuration for ML strategies
 *       (RegimeAwareStrategyMl).</li>
 * </ul>
 *
 * <h2>Interfaces</h2>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.model.MarketTickListener} — interface for receiving
 *       real-time market data (order book, trades). Implemented by strategies.</li>
 * </ul>
 *
 * <h2>Thread Safety</h2>
 * <p>Most data classes are immutable or used in single-threaded context.
 * For thread safety, strategies use {@code ConcurrentHashMap} and explicit synchronization.</p>
 *
 * @see com.github.shk0da.GoldenDragon.strategy
 * @see com.github.shk0da.GoldenDragon.service
 * @see com.github.shk0da.GoldenDragon.repository
 */
package com.github.shk0da.GoldenDragon.model;
