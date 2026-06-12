/**
 * Testing, backtesting, and data collection for GoldenDragon application.
 *
 * <h2>Package Purpose</h2>
 *
 * <p>The {@code test} package contains tools for testing trading strategies on historical data
 * (backtesting), market event simulation, and machine learning data collection. Classes in this
 * package are not used in production trading but are critical for: strategy validation,
 * hyperparameter tuning, risk analysis, and ML model training.
 *
 * <h2>Key Components</h2>
 *
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.test.BacktestRunner} — backtest engine for
 *       candle-based strategies ({@code BaseStrategy} and subclasses). Features:
 *       <ul>
 *         <li><b>Supported Strategies</b>: {@link
 *             com.github.shk0da.GoldenDragon.strategy.UnifiedStrategy}, {@link
 *             com.github.shk0da.GoldenDragon.strategy.RegimeAwareStrategy}, {@link
 *             com.github.shk0da.GoldenDragon.strategy.RegimeAwareStrategyMl}.
 *         <li><b>Run Modes</b>: {@code "fast"} (6 short periods) or {@code "full"} (6 yearly
 *             periods).
 *         <li><b>Parallel Data Loading</b>: thread pool ({@code backtest.threads}, default {@code
 *             availableProcessors - 1}) for reading candle CSV files.
 *         <li><b>Portfolio Simulation</b>: shared cash across all tickers, capital distribution,
 *             commission tracking (0.05% round-trip), trading hours (10:00–21:00, Mon–Fri).
 *         <li><b>Global Timeline</b>: merge all ticker minute timestamps into {@link
 *             java.util.TreeSet} for synchronized processing.
 *         <li><b>Statistics</b>: per strategy — PnL, trade count, win %, max drawdown, Sharpe
 *             Ratio, final balance. Strategy comparison table.
 *       </ul>
 *       <p>Backtest simulates real trading: checks trading hours, closes positions at EOD, applies
 *       commission, respects position and portfolio limits.
 * </ul>
 *
 * <h2>Backtest Data Structure</h2>
 *
 * <ul>
 *   <li><b>Candles</b>: CSV files {@code candlesHOUR.txt} (hourly) and {@code candles5_MIN.txt}
 *       (5-minute). Line format:
 *       <pre>{@code dd.MM.yyyy HH:mm:ss,open,high,low,close,volume}</pre>
 *       Files stored in {@code data/<TICKER>/}.
 *   <li><b>Order Books and Trades</b>: CSV files {@code ticks.txt} for order flow. Format:
 *       <pre>{@code time,best_bid,best_ask,mid_price,bids,asks}</pre>
 *       where {@code bids/asks} are level chains {@code price:quantity|price:quantity|...}.
 *   <li><b>Periods</b>: specified in properties ({@code backtest.periods}) or via system properties
 *       ({@code backtest.mode}).
 * </ul>
 *
 * <h2>Backtesting Process</h2>
 *
 * <ol>
 *   <li><b>Load Tickers</b>: from properties ({@code datacollector.instruments}, {@code
 *       unifiedTrader.ticker.<NAME>.*}). Filter by {@code params.enabled} flag.
 *   <li><b>Load Data</b>: parallel CSV reading per ticker via {@link
 *       java.util.concurrent.ExecutorService}. Sort by time, filter by date range.
 *   <li><b>Create Strategy Instances</b>: one instance per ticker (for state isolation).
 *   <li><b>Build Timeline</b>: merge all timestamps into {@code TreeSet} for synchronized
 *       processing.
 *   <li><b>Main Loop</b>: for each timestamp:
 *       <ul>
 *         <li>Update candles/order books for tickers with data.
 *         <li>Call strategy method ({@code onTick()}, {@code onOrderBook()}, {@code onTrade()}).
 *         <li>Check signals, execute orders via {@code TradingGateway}.
 *         <li>Update positions, PnL, cash.
 *       </ul>
 *   <li><b>Finalization</b>: close positions, calculate statistics, output report.
 * </ol>
 *
 * <h2>ML Data Collection</h2>
 *
 * <p>Classes for collecting labeled training data:
 *
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.ml.TradeDataCollector} — collect trade data:
 *       entry/exit, PnL, indicators at entry, market regime. Saves to CSV for model training
 *       ({@code XGBoost}, {@code Random Forest}).
 *   <li>{@link com.github.shk0da.GoldenDragon.strategy.DataCollector} — collect candles, positions,
 *       strategy decisions in real-time. Used for dataset accumulation.
 * </ul>
 *
 * <p>ML data format:
 *
 * <pre>{@code
 * entry_time,ticker,strategy,entry_price,exit_price,pnl,commission,
 * rsi,macd,atr,regime,label
 * }</pre>
 *
 * <h2>Run Backtest</h2>
 *
 * <ul>
 *   <li><b>Via Gradle</b>:
 *       <pre>{@code ./gradlew clean runBacktest}</pre>
 *       Runs {@code BacktestRunner.main()} for all strategies in {@code ALL_STRATEGIES}.
 *   <li><b>Single Strategy</b>:
 *       <pre>{@code ./gradlew runStrategy -Pstrategy=UnifiedStrategy}</pre>
 *       Runs specific strategy in backtest mode.
 *   <li><b>Full Build</b>:
 *       <pre>{@code ./gradlew clean uberJar}</pre>
 *       Creates fat-jar for server deployment.
 * </ul>
 *
 * <h2>Backtest Configuration</h2>
 *
 * <ul>
 *   <li>{@code backtest.mode}: {@code "fast"} or {@code "full"} (mode_periods).
 *   <li>{@code backtest.threads}: thread count for data loading.
 *   <li>{@code backtest.startBalance}: starting balance (default 1,000,000).
 *   <li>{@code backtest.commissionPercent}: commission (default 0.05%).
 *   <li>{@code datacollector.instruments}: ticker list for testing.
 * </ul>
 *
 * <h2>Reporting</h2>
 *
 * <p>After backtest outputs:
 *
 * <ul>
 *   <li>Strategy summary table: PnL, trades, win %, drawdown, Sharpe, final balance.
 *   <li>Detailed trade log: time, ticker, direction, entry/exit price, PnL.
 *   <li>Charts (optional): equity curve, drawdown, PnL distribution (via external tools).
 * </ul>
 *
 * <h2>Thread Safety</h2>
 *
 * <ul>
 *   <li>{@code BacktestRunner} — uses {@code ExecutorService} for parallel data loading, but main
 *       simulation loop is single-threaded (sequential timeline processing).
 *   <li>Data collections ({@code List}, {@code Map}) are not synchronized, as access is from single
 *       thread only.
 * </ul>
 *
 * <h2>Extension</h2>
 *
 * <p>To add new strategy to backtest:
 *
 * <ol>
 *   <li>Create strategy class (extend {@code BaseStrategy} or implement {@code
 *       MarketTickListener}).
 *   <li>Add strategy to {@code ALL_STRATEGIES} in {@code BacktestRunner}.
 *   <li>Create factory ({@code StrategyFactory}) for parameterized instances if needed.
 *   <li>Run backtest via {@code ./gradlew clean runBacktest}.
 * </ol>
 *
 * <h2>Package Interaction</h2>
 *
 * <ul>
 *   <li>{@code strategy}: backtest engine creates strategy instances and simulates execution.
 *   <li>{@code service}: backtest uses {@code TradingGateway} instead of direct {@code TCSService}
 *       call (execution simulation).
 *   <li>{@code model}: data classes ({@code Candle}, {@code Position}, {@code TradingDecision})
 *       used for state storage.
 *   <li>{@code config}: load ticker parameters, periods, general backtest settings.
 * </ul>
 *
 * <h2>Limitations</h2>
 *
 * <ul>
 *   <li>Backtest doesn't fully account for slippage — uses candle close price.
 *   <li>Liquidity not modeled — orders executed at market price without order book depth.
 *   <li>Network delays and order execution delays not simulated.
 * </ul>
 *
 * @see com.github.shk0da.GoldenDragon.strategy
 * @see com.github.shk0da.GoldenDragon.service
 * @see com.github.shk0da.GoldenDragon.ml
 */
package com.github.shk0da.GoldenDragon.test;
