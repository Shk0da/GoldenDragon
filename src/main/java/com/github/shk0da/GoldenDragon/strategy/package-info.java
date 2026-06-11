/**
 * Trading strategies and decision-making algorithms for GoldenDragon application.
 *
 * <h2>Package Purpose</h2>
 * <p>The {@code strategy} package contains trading strategy implementations, base classes for
 * strategy lifecycle management, indicators, market regime filters, and helper components
 * for generating trading signals. Strategies work in real-time (live trading) and can
 * be run through the backtest engine from the {@code test} package.</p>
 *
 * <h2>Strategy Architecture</h2>
 * <p>All strategies follow a common architecture:</p>
 * <ol>
 *   <li><b>Initialization</b>: load configuration, subscribe to market data, initialize
 *       indicators and filters.</li>
 *   <li><b>Main Loop</b>: periodic market data polling (candles, order books, trades),
 *       indicator calculation, signal generation.</li>
 *   <li><b>Decision Making</b>: signal validation through filters (market regime, risk management),
 *       position size calculation, stop-loss and take-profit setup.</li>
 *   <li><b>Execution</b>: order submission via {@link com.github.shk0da.GoldenDragon.service.TCSService},
 *       execution monitoring, open position management.</li>
 *   <li><b>Completion</b>: position close on signal, timeout, or trading session end.</li>
 * </ol>
 *
 * <h2>Key Classes</h2>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.strategy.BaseStrategy} — abstract base class
 *       for candle-based trading strategies. Implements:
 *       <ul>
 *         <li>Lifecycle: trading hours, trading day checks, EOD position close.</li>
 *         <li>Historical candle loading and caching (hourly, 5-minute).</li>
 *         <li>Parallel ticker processing via {@link java.util.concurrent.ExecutorService}.</li>
 *         <li>Capital management: cash distribution across tickers, error cooldowns.</li>
 *         <li>Technical indicators: RSI, MACD, ATR, moving averages (via {@code IndicatorsUtil}).</li>
 *       </ul>
 *       <p>Concrete strategies extend {@code BaseStrategy} and implement
 *       {@code decide()} method returning {@link com.github.shk0da.GoldenDragon.model.TradingDecision}.</p>
 *   </li>
 *
 *   <li>{@link com.github.shk0da.GoldenDragon.strategy.UnifiedStrategy} — universal
 *       medium-term trading strategy, extends {@code BaseStrategy}. Combines:
 *       <ul>
 *         <li>Technical indicators (RSI, MACD, ATR) for entry and exit.</li>
 *         <li>Market regime filters (volatility, trend).</li>
 *         <li>Portfolio capital management (ticker distribution).</li>
 *         <li>Group confirmation — correlated instrument validation.</li>
 *       </ul>
 *   </li>
 *
 *   <li>{@link com.github.shk0da.GoldenDragon.strategy.RegimeAwareStrategy} — strategy with
 *       market regime adaptation. Detects regime by volatility and volume, switches entry
 *       logic and risk parameters based on regime.</li>
 *
 *   <li>{@link com.github.shk0da.GoldenDragon.strategy.RegimeAwareStrategyMl} — extension
 *       of {@code RegimeAwareStrategy} using ML model (XGBoost) for regime classification
 *       and price movement prediction.</li>
 *
 *   <li>{@link com.github.shk0da.GoldenDragon.strategy.DivFlow} — dividend trading strategy.
 *       Analyzes dividend yield, cut-off dates, fundamental indicators.</li>
 *
 *   <li>{@link com.github.shk0da.GoldenDragon.strategy.LevelTrader} — key level trading
 *       (support/resistance). Uses historical extremes, volume profiles.</li>
 *
 *   <li>{@link com.github.shk0da.GoldenDragon.strategy.IndicatorTrader} — indicator strategy,
 *       focuses on technical indicators without pattern dependency.</li>
 *
 *   <li>{@link com.github.shk0da.GoldenDragon.strategy.Rebalance} — strategy for
 *       periodic portfolio rebalancing by target weights.</li>
 *
 *   <li>{@link com.github.shk0da.GoldenDragon.strategy.ModelGenerator} — trading model
 *       generation and validation (hyperparameters, cross-validation).</li>
 *
 *   <li>{@link com.github.shk0da.GoldenDragon.strategy.RSX} — RSI with exponential
 *       smoothing (RSX = RSI Smoothed).</li>
 *
 *   <li>{@link com.github.shk0da.GoldenDragon.strategy.DataCollector} — ML model data collection:
 *       save candles, positions, decisions in training format.</li>
 * </ul>
 *
 * <h2>Interfaces and Abstractions</h2>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.model.MarketTickListener} — interface for receiving
 *       real-time market data updates (order books, trades). Implemented by strategies
 *       for stream subscription.</li>
 * </ul>
 *
 * <h2>State Management</h2>
 * <p>Strategies store state in class fields:</p>
 * <ul>
 *   <li>Positions: {@link com.github.shk0da.GoldenDragon.model.Position},
 *       {@link com.github.shk0da.GoldenDragon.model.PositionInfo}.</li>
 *   <li>Market data: {@link com.github.shk0da.GoldenDragon.model.Candle},
 *       {@link com.github.shk0da.GoldenDragon.model.MarketDepthSnapshot},
 *       {@link com.github.shk0da.GoldenDragon.model.MarketTradeTick}.</li>
 *   <li>Signals: {@link com.github.shk0da.GoldenDragon.model.TradingDecision}.</li>
 * </ul>
 * <p>For thread safety, uses {@link java.util.concurrent.ConcurrentHashMap},
 * {@link java.util.concurrent.CopyOnWriteArrayList} and explicit state object synchronization.</p>
 *
 * <h2>Thread Safety</h2>
 * <ul>
 *   <li>{@code BaseStrategy} — uses {@code ExecutorService} with thread pool, each ticker
 *       processed in separate thread. State shared across tickers via {@code ConcurrentHashMap}.</li>
 *   <li>Other strategies — single-threaded per ticker, multi-threaded at ticker pool level.</li>
 * </ul>
 *
 * <h2>Configuration</h2>
 * <p>Strategies use configuration from:</p>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.config.UnifiedTraderConfig} — parameters for
 *       {@code BaseStrategy} and subclasses (tickers, limits, indicators).</li>
 *   <li>{@link com.github.shk0da.GoldenDragon.config.MainConfig} — general settings (API keys,
 *       sandbox mode, test trading flag).</li>
 * </ul>
 *
 * <h2>Logging and Notifications</h2>
 * <p>Strategies log to {@link java.lang.System#out} with timestamps
 * {@code dd.MM.yyyy HH:mm:ss}. Critical events (position open/close, errors)
 * are duplicated to Telegram via {@link com.github.shk0da.GoldenDragon.service.TelegramNotifyService}.</p>
 *
 * <h2>Backtesting</h2>
 * <p>Strategies can be run in backtest mode via
 * {@link com.github.shk0da.GoldenDragon.test.BacktestRunner}
 * order execution on historical data with commissions, trading hours, and portfolio management.</p>
 *
 * <h2>Extension</h2>
 * <p>To create a new strategy:</p>
 * <ol>
 *   <li>Extend {@code BaseStrategy} (for candle trading) or implement
 *       {@code MarketTickListener} (for order flow).</li>
 *   <li>Implement {@code decide()} method (for {@code BaseStrategy}) or
 *       {@code onOrderBook()}/{@code onTrade()} (for {@code MarketTickListener}).</li>
 *   <li>Add configuration class (if parameters needed).</li>
 *   <li>Register strategy in {@link com.github.shk0da.GoldenDragon.GoldenDragon#main}
 *       or backtest engine.</li>
 * </ol>
 *
 * @see com.github.shk0da.GoldenDragon.service
 * @see com.github.shk0da.GoldenDragon.test
 * @see com.github.shk0da.GoldenDragon.model
 * @see com.github.shk0da.GoldenDragon.config
 */
package com.github.shk0da.GoldenDragon.strategy;
