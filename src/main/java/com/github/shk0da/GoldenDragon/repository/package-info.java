/**
 * Repositories for data storage in GoldenDragon application.
 *
 * <h2>Package Purpose</h2>
 *
 * <p>The {@code repository} package contains Repository pattern implementations for in-memory data
 * storage: tickers, FIGI, prices. Repositories are used for caching reference data and fast data
 * access during trading.
 *
 * <h2>Architecture</h2>
 *
 * <p>All repositories extend {@link com.github.shk0da.goldendragon.repository.AbstractRepository},
 * which implements {@link com.github.shk0da.goldendragon.repository.Repository} interface.
 * AbstractRepository uses {@code ConcurrentHashMap} for thread-safe storage.
 *
 * <h2>Key Repositories</h2>
 *
 * <ul>
 *   <li>{@link com.github.shk0da.goldendragon.repository.TickerRepository} — ticker information
 *       storage ({@code TickerInfo}): FIGI, ISIN, currency, type, lot. <br>
 *       <b>Singleton</b>: loads data from {@code tickers.json} on initialization. <br>
 *       Used by: {@code TCSService}, {@code GoldenDragon}, strategies.
 *   <li>{@link com.github.shk0da.goldendragon.repository.FigiRepository} — FIGI (Financial
 *       Instrument Global Identifier) storage. Ticker to FIGI mapping. <br>
 *       <b>Singleton</b>. <br>
 *       Used by: {@code TCSService} for ticker-to-FIGI conversion.
 *   <li>{@link com.github.shk0da.goldendragon.repository.PricesRepository} — current market price
 *       storage (order book: bids/asks). <br>
 *       <b>Singleton</b>. <br>
 *       Used by: {@code TCSService} for caching latest prices.
 * </ul>
 *
 * <h2>Repository Interface</h2>
 *
 * <ul>
 *   <li>{@code getById(ID id)} — get element by identifier.
 *   <li>{@code getById(ID id, T defaultValue)} — get with default value.
 *   <li>{@code insert(ID id, T value)} — insert/update element.
 *   <li>{@code putAll(Map<ID, T> values)} — bulk insert.
 *   <li>{@code containsKey(ID key)} — check key existence.
 *   <li>{@code getAll()} — get all elements.
 * </ul>
 *
 * <h2>Serialization</h2>
 *
 * <p>{@code TickerRepository} supports disk load/save via {@link
 * com.github.shk0da.goldendragon.utils.SerializationUtils}. Data is stored in JSON format ({@code
 * tickers.json}) and loaded on application startup for faster initialization.
 *
 * <h2>Thread Safety</h2>
 *
 * <p>All repositories are thread-safe using {@code ConcurrentHashMap}. Methods can be called from
 * different threads without additional synchronization.
 *
 * <h2>Usage Example</h2>
 *
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
 * @see com.github.shk0da.goldendragon.service.TCSService
 * @see com.github.shk0da.goldendragon.model.TickerInfo
 * @see com.github.shk0da.goldendragon.utils.SerializationUtils
 */
package com.github.shk0da.goldendragon.repository;
