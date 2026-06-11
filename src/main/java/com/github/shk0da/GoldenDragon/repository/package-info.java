/**
 * Репозитории для хранения данных приложения GoldenDragon.
 *
 * <h2>Назначение пакета</h2>
 * <p>Пакет {@code repository} содержит реализации паттерна Repository для ин-мемори хранения
 * данных: тикеры, FIGI, цены. Репозитории используются для кеширования справочной информации
 * и быстрого доступа к данным во время торговли.</p>
 *
 * <h2>Архитектура</h2>
 * <p>Все репозитории наследуются от {@link com.github.shk0da.GoldenDragon.repository.AbstractRepository},
 * который реализует интерфейс {@link com.github.shk0da.GoldenDragon.repository.Repository}.
 * AbstractRepository использует {@code ConcurrentHashMap} для потокобезопасного хранения.</p>
 *
 * <h2>Ключевые репозитории</h2>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.repository.TickerRepository} — хранилище информации
 *       о тикерах ({@code TickerInfo}): FIGI, ISIN, валюта, тип, лот.
 *       <br><b>Синглтон</b>: загружает данные из {@code tickers.json} при инициализации.
 *       <br>Используется: {@code TCSService}, {@code GoldenDragon}, стратегии.</li>
 *
 *   <li>{@link com.github.shk0da.GoldenDragon.repository.FigiRepository} — хранилище FIGI
 *       (Financial Instrument Global Identifier). Маппинг тикер → FIGI.
 *       <br><b>Синглтон</b>.
 *       <br>Используется: {@code TCSService} для конвертации тикеров в FIGI.</li>
 *
 *   <li>{@link com.github.shk0da.GoldenDragon.repository.PricesRepository} — хранилище текущих
 *       рыночных цен (стакан: bids/asks).
 *       <br><b>Синглтон</b>.
 *       <br>Используется: {@code TCSService} для кеширования последних цен.</li>
 * </ul>
 *
 * <h2>Интерфейс Repository</h2>
 * <ul>
 *   <li>{@code getById(ID id)} — получение элемента по идентификатору.</li>
 *   <li>{@code getById(ID id, T defaultValue)} — получение с значением по умолчанию.</li>
 *   <li>{@code insert(ID id, T value)} — вставка/обновление элемента.</li>
 *   <li>{@code putAll(Map<ID, T> values)} — массовая вставка.</li>
 *   <li>{@code containsKey(ID key)} — проверка наличия ключа.</li>
 *   <li>{@code getAll()} — получение всех элементов.</li>
 * </ul>
 *
 * <h2>Сериализация</h2>
 * <p>{@code TickerRepository} поддерживает загрузку/сохранение данных на диск через
 * {@link com.github.shk0da.GoldenDragon.utils.SerializationUtils}. Данные хранятся в JSON формате
 * ({@code tickers.json}) и загружаются при старте приложения для ускорения инициализации.</p>
 *
 * <h2>Потокобезопасность</h2>
 * <p>Все репозитории потокобезопасны благодаря использованию {@code ConcurrentHashMap}.
 * Методы могут вызываться из разных потоков без дополнительной синхронизации.</p>
 *
 * <h2>Пример использования</h2>
 * <pre>{@code
 * // Получение тикера из репозитория
 * TickerInfo ticker = TickerRepository.INSTANCE.getById(new TickerInfo.Key("SBER", TickerType.STOCK));
 *
 * // Вставка нового тикера
 * TickerRepository.INSTANCE.insert(key, tickerInfo);
 *
 * // Массовая вставка из API
 * tickerRepository.putAll(tcsService.getStockList());
 * }</pre>
 *
 * @see com.github.shk0da.GoldenDragon.service.TCSService
 * @see com.github.shk0da.GoldenDragon.model.TickerInfo
 * @see com.github.shk0da.GoldenDragon.utils.SerializationUtils
 */
package com.github.shk0da.GoldenDragon.repository;
