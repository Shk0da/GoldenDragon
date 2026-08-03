/**
 * Capital management and risk management for GoldenDragon application.
 *
 * <h2>Package Purpose</h2>
 *
 * <p>The {@code money} package contains components for position sizing, risk control, performance
 * tracking, and emergency trading halt. These classes are used by strategies for money management
 * (MM).
 *
 * <h2>Position Sizing</h2>
 *
 * <ul>
 *   <li>{@link com.github.shk0da.goldendragon.money.SizingStrategy} — interface for position sizing
 *       algorithms. Implementations calculate optimal size based on risk.
 *   <li>{@link com.github.shk0da.goldendragon.money.FixedRiskSizing} — fixed risk per trade (e.g.,
 *       1% of capital). Classic position sizing approach.
 *   <li>{@link com.github.shk0da.goldendragon.money.VolatilityAdjustedSizing} — position size
 *       adjusted by volatility (ATR). Smaller size during high volatility.
 *   <li>{@link com.github.shk0da.goldendragon.money.PositionSizer} — SizingStrategy wrapper.
 *       Applies minimum lot size and lot step to calculation.
 * </ul>
 *
 * <h2>Risk Control</h2>
 *
 * <ul>
 *   <li>{@link com.github.shk0da.goldendragon.money.RiskManager} — risk manager: daily loss limits,
 *       consecutive losses, risk per trade. {@code canTrade()} method checks if trading is allowed.
 *   <li>{@link com.github.shk0da.goldendragon.money.KillSwitch} — emergency trading halt. Triggers
 *       on critical drawdown, connection loss, abnormal spread. Blocks trading until manual reset.
 *   <li>{@link com.github.shk0da.goldendragon.money.StopLossManager} — stop-loss management.
 *       Dynamic stop update on profit (trailing stop), ATR-based calculation.
 * </ul>
 *
 * <h2>Adaptive Capital</h2>
 *
 * <ul>
 *   <li>{@link com.github.shk0da.goldendragon.money.AdaptiveCapital} — anti-martingale system.
 *       Reduces risk after losing streaks, restores after winning trades. Never uses martingale (no
 *       doubling down on losses).
 * </ul>
 *
 * <h2>Performance Tracking</h2>
 *
 * <ul>
 *   <li>{@link com.github.shk0da.goldendragon.money.PerformanceTracker} — trading statistics: PnL,
 *       trade count, win rate, maximum drawdown, largest win/loss. Used for adaptive capital
 *       management.
 * </ul>
 *
 * <h2>Strategy Integration</h2>
 *
 * <p>Money management components are used in {@code UnifiedStrategy} and other strategies:
 *
 * <ol>
 *   <li>Before entry: check {@code RiskManager.canTrade()}, {@code KillSwitch.isTradingAllowed()}.
 *   <li>Size calculation: {@code PositionSizer.calculateSize()} with volatility adjustment.
 *   <li>After exit: {@code PerformanceTracker.registerTrade()}, {@code
 *       RiskManager.registerTrade()}.
 *   <li>Stop update: {@code StopLossManager.updateStopLoss()}.
 * </ol>
 *
 * <h2>Thread Safety</h2>
 *
 * <p>All classes use {@code AtomicReference}, {@code AtomicInteger}, {@code volatile} for thread
 * safety. Methods can be called from different strategy threads.
 *
 * @see com.github.shk0da.goldendragon.strategy.UnifiedStrategy
 * @see com.github.shk0da.goldendragon.strategy.BaseStrategy
 */
package com.github.shk0da.goldendragon.money;
