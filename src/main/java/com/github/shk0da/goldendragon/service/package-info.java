/**
 * Service layer for GoldenDragon application — external system integration and infrastructure
 * components.
 *
 * <h2>Package Purpose</h2>
 *
 * <p>The {@code service} package contains classes responsible for interaction with external APIs
 * and services: broker API (Tinkoff Investments), TradingView for market scanning. These classes
 * contain no trading logic — they provide low-level abstractions for working with exchange data,
 * order execution, and message sending.
 *
 * <h2>Key Components</h2>
 *
 * <ul>
 *   <li>{@link com.github.shk0da.goldendragon.service.TCSService} — main service for working with
 *       broker API (Tinkoff Investments). Provides methods for:
 *       <ul>
 *         <li>Market data retrieval: order books, last trades, historical candles.
 *         <li>Order execution: market and limit orders for buy/sell.
 *         <li>Position management: close positions, set stop-losses and take-profits.
 *         <li>Portfolio: get available cash, current positions, total portfolio value.
 *         <li>Instrument search: ticker-to-FIGI conversion, instrument metadata caching.
 *       </ul>
 *       <p>Class encapsulates {@link ru.tinkoff.piapi.core.InvestApi} and provides convenient
 *       interface for strategies. Supports sandbox mode and timestamped logging.
 * </ul>
 *
 * <h2>Strategy Interaction</h2>
 *
 * <p>Strategies from {@code strategy} package use services as follows:
 *
 * <ul>
 *   <li>{@code TCSService} — direct call via {@link
 *       com.github.shk0da.goldendragon.service.TCSService} or via {@code TradingGateway} interface
 *       (adapter for easier testing).
 * </ul>
 *
 * <h2>Thread Safety</h2>
 *
 * <ul>
 *   <li>{@code TCSService} — not fully thread-safe. Internal collections ({@code
 *       ConcurrentHashMap}, {@code CopyOnWriteArrayList}) are protected from concurrent access, but
 *       order execution calls must be synchronized at strategy level.
 *   <li>{@code TelegramNotifyService} — thread-safe thanks to {@code ExecutorService} with single
 *       thread (message queue).
 *   <li>{@code TradingViewService} — stateless, thread-safe.
 * </ul>
 *
 * <h2>Configuration</h2>
 *
 * <p>Services use configuration from:
 *
 * <ul>
 *   <li>{@link com.github.shk0da.goldendragon.config.MainConfig} — API keys, account, sandbox mode.
 * </ul>
 *
 * <h2>Logging</h2>
 *
 * <p>Services log to {@link java.lang.System#out} with timestamps in format {@code dd.MM.yyyy
 * HH:mm:ss}. Formatting via {@link java.time.format.DateTimeFormatter} or {@link
 * java.text.SimpleDateFormat}.
 *
 * @see com.github.shk0da.goldendragon.strategy
 * @see com.github.shk0da.goldendragon.config
 * @see com.github.shk0da.goldendragon.model
 */
package com.github.shk0da.goldendragon.service;
