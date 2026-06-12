/**
 * Model classes and data structures for GoldenDragon application.
 *
 * <h2>Package Purpose</h2>
 *
 * <p>The {@code model} package contains data transfer objects (DTOs), interfaces, and enums for
 * representing market data, positions, instruments, and trading decisions. These classes are used
 * by all system components: strategies, services, repositories.
 *
 * <h2>Market Data</h2>
 *
 * <ul>
 *   <li>{@link com.github.shk0da.goldendragon.model.Candle} — candlestick (OHLCV) for backtests and
 *       analysis.
 *   <li>{@link com.github.shk0da.goldendragon.model.MarketDepthSnapshot} — order book snapshot
 *       (bids/asks).
 *   <li>{@link com.github.shk0da.goldendragon.model.MarketDepthLevel} — order book level
 *       (price/quantity).
 *   <li>{@link com.github.shk0da.goldendragon.model.MarketTradeTick} — last trade tick.
 * </ul>
 *
 * <h2>Instruments and Tickers</h2>
 *
 * <ul>
 *   <li>{@link com.github.shk0da.goldendragon.model.TickerInfo} — instrument information: FIGI,
 *       ISIN, currency, type, lot.
 *   <li>{@link com.github.shk0da.goldendragon.model.TickerType} — instrument type enum: STOCK,
 *       BOND, ETF, CURRENCY, FEATURE.
 *   <li>{@link com.github.shk0da.goldendragon.model.Market} — market enum: MOEX.
 *   <li>{@link com.github.shk0da.goldendragon.model.Group} — instrument group enum: TREND, FX,
 *       MIXED.
 * </ul>
 *
 * <h2>Positions and Portfolio</h2>
 *
 * <ul>
 *   <li>{@link com.github.shk0da.goldendragon.model.Position} — trading position: direction,
 *       quantity, entry price.
 *   <li>{@link com.github.shk0da.goldendragon.model.PositionInfo} — position info from API:
 *       balance, lots, expected yield.
 *   <li>{@link com.github.shk0da.goldendragon.model.PortfolioPosition} — target portfolio position:
 *       name, type, portfolio percentage.
 * </ul>
 *
 * <h2>Trading Decisions</h2>
 *
 * <ul>
 *   <li>{@link com.github.shk0da.goldendragon.model.TradingDecision} — strategy decision: action
 *       (BUY/SELL/HOLD/CLOSE), signal, quantity, prices.
 * </ul>
 *
 * <h2>Specialized Models</h2>
 *
 * <ul>
 *   <li>{@link com.github.shk0da.goldendragon.model.DiviTicker} — dividend information: ticker,
 *       cut-off date, dividend, yield.
 *   <li>{@link com.github.shk0da.goldendragon.model.TickerScan} — TradingView scan result: analyst
 *       recommendation, debt/equity.
 *   <li>{@link com.github.shk0da.goldendragon.model.TickerJson} — serializable data for
 *       LevelTrader: ticker + support/resistance levels.
 *   <li>{@link com.github.shk0da.goldendragon.model.Config} — configuration for ML strategies
 *       (RegimeAwareStrategyMl).
 * </ul>
 *
 * <h2>Interfaces</h2>
 *
 * <ul>
 *   <li>{@link com.github.shk0da.goldendragon.model.MarketTickListener} — interface for receiving
 *       real-time market data (order book, trades). Implemented by strategies.
 * </ul>
 *
 * <h2>Thread Safety</h2>
 *
 * <p>Most data classes are immutable or used in single-threaded context. For thread safety,
 * strategies use {@code ConcurrentHashMap} and explicit synchronization.
 *
 * @see com.github.shk0da.goldendragon.strategy
 * @see com.github.shk0da.goldendragon.service
 * @see com.github.shk0da.goldendragon.repository
 */
package com.github.shk0da.goldendragon.model;
