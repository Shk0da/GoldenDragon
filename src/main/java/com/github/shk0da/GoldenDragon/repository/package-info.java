/**
 * Repositories for data storage in GoldenDragon application.
 *
 * <h2>Package Purpose</h2>
 * <p>The {@code repository} package contains Repository pattern implementations for in-memory
 * data storage: tickers, FIGI, prices. Repositories are used for caching reference data
 * and fast data access during trading.</p>
 *
 * <h2>Architecture</h2>
 * <p>All repositories extend {@link com.github.shk0da.GoldenDragon.repository.AbstractRepository},
 * which implements {@link com.github.shk0da.GoldenDragon.repository.Repository} interface.
 * AbstractRepository uses {@code ConcurrentHashMap} for thread-safe storage.</p>
 *
 * <h2>Key Repositories</h2>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.repository.TickerRepository} — ticker information
 *       storage ({@code TickerInfo}): FIGI, ISIN, currency, type, lot.
 *       <br><b>Singleton</b>: loads data from {@code tickers.json} on initialization.
 *       <br>Used by: {@code TCSService}, {@code GoldenDragon}, strategies.</li>
 *
 *   <li>{@link com.github.shk0da.GoldenDragon.repository.FigiRepository} — FIGI
 *       (Financial Instrument Global Identifier) storage. Ticker to FIGI mapping.
 *       <br><b>Singleton</b>.
 *       <br>Used by: {@code TCSService} for ticker-to-FIGI conversion.</li>
 *
 *   <li>{@link com.github.shk0da.GoldenDragon.repository.PricesRepository} — current
 *       market price storage (order book: bids/asks).
 *       <br><b>Singleton</b>.
 *       <br>Used by: {@code TCSService} for caching latest prices.</li>
 * </ul>
 *
 * <h2>Repository Interface</h2>
 * <ul>
 *   <li>{@code getById(ID id)} — get element by identifier.</li>
 *   <li>{@code getById(ID id, T defaultValue)} — get with default value.</li>
 *   <li>{@code insert(ID id, T value)} — insert/update element.</li>
 *   <li>{@code putAll(Map<ID, T> values)} — bulk insert.</li>
 *   <li>{@code containsKey(ID key)} — check key existence.</li>
 *   <li>{@code getAll()} — get all elements.</li>
 * </ul>
 *
 * <h2>Serialization</h2>
 * <p>{@code TickerRepository} supports disk load/save via
 * {@link com.github.shk0da.GoldenDragon.utils.SerializationUtils}. Data is stored in JSON format
 * ({@code tickers.json}) and loaded on application startup for faster initialization.</p>
 *
 * <h2>Thread Safety</h2>
 * <p>All repositories are thread-safe using {@code ConcurrentHashMap}.
 * Methods can be called from different threads without additional synchronization.</p>
 *
 * <h2>Usage Example</h2>
 * <pre>{@code
 * // Get ticker from repository
 * TickerInfo ticker = TickerRepository.INSTANCE.getById(new TickerInfo.Key("SBER", TickerType.STOCK));
 *
 * // Insert new ticker
 * TickerRepository.INSTANCE.insert(key, tickerInfo);
 *
 * // Bulk insert from API
 * tickerRepository.putAll(tcsService.getStockList());
 * }</pre>
 *
 * @see com.github.shk0da.GoldenDragon.service.TCSService
 * @see com.github.shk0da.GoldenDragon.model.TickerInfo
 * @see com.github.shk0da.GoldenDragon.utils.SerializationUtils
 */
package com.github.shk0da.GoldenDragon.repository;
