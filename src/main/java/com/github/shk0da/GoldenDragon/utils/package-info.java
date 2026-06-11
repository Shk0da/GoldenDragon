/**
 * Utility classes for GoldenDragon application.
 *
 * <h2>Package Purpose</h2>
 * <p>The {@code utils} package contains helper classes for working with properties, JSON, time,
 * indicators, HTTP requests. These classes contain no business logic and are used
 * by all system components.</p>
 *
 * <h2>Configuration</h2>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.utils.PropertiesUtils} — load properties files
 *       from classpath and file system. Used by all configuration classes.</li>
 * </ul>
 *
 * <h2>Serialization</h2>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.utils.SerializationUtils} — JSON serialization/deserialization
 *       via Gson. Supports disk load/save, {@code TypeToken} handling,
 *       custom deserializers (e.g., for {@code TickerInfo.Key}).</li>
 * </ul>
 *
 * <h2>Time</h2>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.utils.TimeUtils} — time utilities:
 *       {@code sleep(long millis)} with {@code InterruptedException} handling.</li>
 * </ul>
 *
 * <h2>Predicates</h2>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.utils.PredicateUtils} — predicate utilities:
 *       {@code distinctByKey()} for duplicate filtering in streams.</li>
 * </ul>
 *
 * <h2>Technical Indicators</h2>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.utils.IndicatorsUtil} — technical indicator calculation:
 *       RSI, MACD, ATR, moving averages (SMA, EMA). Uses TA-Lib library.</li>
 * </ul>
 *
 * <h2>HTTP Requests</h2>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.utils.RequestUtils} — HTTP requests with retries:
 *       {@code requestWithRetry()} for stable external API work (TradingView, Telegram).</li>
 * </ul>
 *
 * <h2>Levels (LevelUtils)</h2>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.utils.LevelUtils} — support/resistance level handling:
 *       {@code Level} classes, level breakout detection methods.</li>
 * </ul>
 *
 * <h2>GerchikUtils</h2>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.utils.GerchikUtils} — Gerchik system methods:
 *       level calculation, entry patterns, position management.</li>
 * </ul>
 *
 * <h2>TickerTypeResolver</h2>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.utils.TickerTypeResolver} — ticker type detection
 *       (STOCK, BOND, ETF, CURRENCY) by name. Uses heuristics and prefixes.</li>
 * </ul>
 *
 * <h2>Thread Safety</h2>
 * <p>Most utility classes are stateless and thread-safe. Exceptions:</p>
 * <ul>
 *   <li>{@code SerializationUtils} — synchronized methods for load/save.</li>
 *   <li>{@code IndicatorsUtil} — requires external synchronization for parallel calculations.</li>
 * </ul>
 *
 * @see com.github.shk0da.GoldenDragon.config
 * @see com.github.shk0da.GoldenDragon.strategy
 * @see com.github.shk0da.GoldenDragon.service
 */
package com.github.shk0da.GoldenDragon.utils;
