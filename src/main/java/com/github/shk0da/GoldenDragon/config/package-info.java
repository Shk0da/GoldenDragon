/**
 * Configuration classes for GoldenDragon application.
 *
 * <h2>Package Purpose</h2>
 *
 * <p>The {@code config} package contains configuration classes for various system components:
 * trading strategies, services, market settings. Configuration is loaded from properties files and
 * provides typed access to parameters.
 *
 * <h2>Key Classes</h2>
 *
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.config.MainConfig} — main application configuration:
 *       Tinkoff Invest API keys, sandbox settings, test mode, HTTP client.
 *   <li>{@link com.github.shk0da.GoldenDragon.config.MarketConfig} — market settings (MOEX):
 *       currency, position limits, market parameters.
 *   <li>{@link com.github.shk0da.GoldenDragon.config.LevelTraderConfig} — LevelTrader parameters:
 *       stop-loss, take-profit, level confirmation, instrument list.
 *   <li>{@link com.github.shk0da.GoldenDragon.config.UnifiedTraderConfig} — UnifiedStrategy
 *       configuration: indicator parameters, limits, money management settings.
 *   <li>{@link com.github.shk0da.GoldenDragon.config.RSXConfig} — RSX strategy settings: trend
 *       ticker, maximum portfolio size.
 *   <li>{@link com.github.shk0da.GoldenDragon.config.DivFlowConfig} — DivFlow constants: dividend
 *       calendar URLs (Smart-Lab, Dohod, Investing.com).
 *   <li>{@link com.github.shk0da.GoldenDragon.config.TelegramNotifyConfig} — Telegram
 *       notifications: bot token, chat_id, extended notification mode.
 *   <li>{@link com.github.shk0da.GoldenDragon.config.DataCollectorConfig} — data collection: data
 *       directory, instrument list, replace mode.
 *   <li>{@link com.github.shk0da.GoldenDragon.config.RebalanceConfig} — portfolio rebalancing:
 *       target position weights, rebalancing percentages.
 * </ul>
 *
 * <h2>Configuration Loading</h2>
 *
 * <p>All configuration classes use {@link com.github.shk0da.GoldenDragon.utils.PropertiesUtils} to
 * load settings from {@code application.properties}. Default values are specified in constructors.
 *
 * <h2>Thread Safety</h2>
 *
 * <p>Configuration classes are immutable after creation (except MainConfig with mutable accountId).
 * Configuration is loaded once during instance creation.
 *
 * @see com.github.shk0da.GoldenDragon.strategy
 * @see com.github.shk0da.GoldenDragon.service
 * @see com.github.shk0da.GoldenDragon.utils.PropertiesUtils
 */
package com.github.shk0da.GoldenDragon.config;
