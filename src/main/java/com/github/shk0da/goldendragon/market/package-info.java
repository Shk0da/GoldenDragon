/**
 * Market data and order execution infrastructure for GoldenDragon application.
 *
 * <h2>Package Purpose</h2>
 *
 * <p>The {@code market} package contains classes responsible for market data access and order
 * execution. These classes provide abstractions for:
 *
 * <ul>
 *   <li>Reading historical and live market data (candles, prices)
 *   <li>Executing market orders (buy/sell)
 *   <li>Simulating market behavior in backtest mode
 * </ul>
 *
 * <h2>Key Components</h2>
 *
 * <ul>
 *   <li>{@link com.github.shk0da.goldendragon.market.MarketDataProvider} — interface for market
 *       data access. Provides methods for:
 *       <ul>
 *         <li>Getting historical candles (hourly, 5-minute)
 *         <li>Getting live prices (bid/ask)
 *         <li>Checking data freshness and availability
 *       </ul>
 *   <li>{@link com.github.shk0da.goldendragon.market.LiveMarketDataProvider} — live market data
 *       provider backed by TCSService. Fetches real-time data from broker API.
 *   <li>{@link com.github.shk0da.goldendragon.market.OrderExecutor} — interface for order
 *       execution. Provides methods for:
 *       <ul>
 *         <li>Market buy/sell orders
 *         <li>Position closing (long/short)
 *         <li>Available cash and position queries
 *       </ul>
 *   <li>{@link com.github.shk0da.goldendragon.market.LiveOrderExecutor} — live order executor
 *       backed by TCSService. Executes real orders against broker API.
 *   <li>{@link com.github.shk0da.goldendragon.market.MarketPrices} — snapshot of current market
 *       prices (bid/ask).
 * </ul>
 *
 * <h2>Architecture</h2>
 *
 * <p>Market package follows interface-segregation pattern:
 *
 * <ul>
 *   <li>Interfaces ({@code MarketDataProvider}, {@code OrderExecutor}) define contracts
 *   <li>Implementations ({@code LiveMarketDataProvider}, {@code LiveOrderExecutor}) provide live
 *       functionality
 *   <li>Backtest implementations live in the test source set
 * </ul>
 *
 * <h2>Usage</h2>
 *
 * <p>Strategies use market components through {@code MarketDataProvider} and {@code OrderExecutor}
 * interfaces:
 *
 * <pre>{@code
 * MarketDataProvider dataProvider = new LiveMarketDataProvider(tcsService);
 * OrderExecutor executor = new LiveOrderExecutor(tcsService);
 *
 * List<Candle> candles = dataProvider.getCandles("SBER", "HOUR");
 * MarketPrices prices = dataProvider.getLivePrices("SBER");
 * }</pre>
 *
 * <h2>Backtest Mode</h2>
 *
 * <p>In backtest mode, strategies use backtest-specific implementations of {@code
 * MarketDataProvider} and {@code OrderExecutor} (see {@code
 * com.github.shk0da.goldendragon.test} package). A simulated broker delegates to these to ensure
 * parity with live trading.
 *
 * <h2>Thread Safety</h2>
 *
 * <ul>
 *   <li>{@code LiveMarketDataProvider} — thread-safe (delegates to thread-safe TCSService)
 *   <li>{@code LiveOrderExecutor} — not thread-safe (order execution must be synchronized at
 *       strategy level)
 * </ul>
 *
 * @see com.github.shk0da.goldendragon.service.TCSService
 * @see com.github.shk0da.goldendragon.strategy
 * @see com.github.shk0da.goldendragon.model
 */
package com.github.shk0da.goldendragon.market;
