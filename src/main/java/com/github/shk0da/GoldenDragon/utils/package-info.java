/**
 * Utility classes for GoldenDragon application.
 *
 * <h2>Package Purpose</h2>
 *
 * <p>The {@code utils} package contains helper classes for working with properties, JSON, time,
 * indicators, HTTP requests. These classes contain no business logic and are used by all system
 * components.
 *
 * <h2>Configuration</h2>
 *
 * <ul>
 *   <li>{@link com.github.shk0da.goldendragon.utils.PropertiesUtils} — load properties files from
 *       classpath and file system. Used by all configuration classes.
 * </ul>
 *
 * <h2>Serialization</h2>
 *
 * <ul>
 *   <li>{@link com.github.shk0da.goldendragon.utils.SerializationUtils} — JSON
 *       serialization/deserialization via Gson. Supports disk load/save, {@code TypeToken}
 *       handling, custom deserializers (e.g., for {@code TickerInfo.Key}).
 * </ul>
 *
 * <h2>Time</h2>
 *
 * <ul>
 *   <li>{@link com.github.shk0da.goldendragon.utils.TimeUtils} — time utilities: {@code sleep(long
 *       millis)} with {@code InterruptedException} handling.
 * </ul>
 *
 * <h2>Predicates</h2>
 *
 * <ul>
 *   <li>{@link com.github.shk0da.goldendragon.utils.PredicateUtils} — predicate utilities: {@code
 *       distinctByKey()} for duplicate filtering in streams.
 * </ul>
 *
 * <h2>Technical Indicators</h2>
 *
 * <ul>
 *   <li>{@link com.github.shk0da.goldendragon.utils.IndicatorsUtil} — technical indicator
 *       calculation: RSI, MACD, ATR, moving averages (SMA, EMA). Uses TA-Lib library.
 * </ul>
 *
 * <h2>HTTP Requests</h2>
 *
 * <ul>
 *   <li>{@link com.github.shk0da.goldendragon.utils.RequestUtils} — HTTP requests with retries:
 *       {@code requestWithRetry()} for stable external API work (TradingView, Telegram).
 * </ul>
 *
 * <h2>Levels (LevelUtils)</h2>
 *
 * <ul>
 *   <li>{@link com.github.shk0da.goldendragon.utils.LevelUtils} — support/resistance level
 *       handling: {@code Level} classes, level breakout detection methods.
 * </ul>
 *
 * <h2>GerchikUtils</h2>
 *
 * <ul>
 *   <li>{@link com.github.shk0da.goldendragon.utils.GerchikUtils} — Gerchik system methods: level
 *       calculation, entry patterns, position management.
 * </ul>
 *
 * <h2>TickerTypeResolver</h2>
 *
 * <ul>
 *   <li>{@link com.github.shk0da.goldendragon.utils.TickerTypeResolver} — ticker type detection
 *       (STOCK, BOND, ETF, CURRENCY) by name. Uses heuristics and prefixes.
 * </ul>
 *
 * <h2>Thread Safety</h2>
 *
 * <p>Most utility classes are stateless and thread-safe. Exceptions:
 *
 * <ul>
 *   <li>{@code SerializationUtils} — synchronized methods for load/save.
 *   <li>{@code IndicatorsUtil} — requires external synchronization for parallel calculations.
 * </ul>
 *
 * @see com.github.shk0da.goldendragon.config
 * @see com.github.shk0da.goldendragon.strategy
 * @see com.github.shk0da.goldendragon.service
 */
package com.github.shk0da.goldendragon.utils;
