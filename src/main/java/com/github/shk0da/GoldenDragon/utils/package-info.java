/**
 * Утилитные классы приложения GoldenDragon.
 *
 * <h2>Назначение пакета</h2>
 * <p>Пакет {@code utils} содержит вспомогательные классы для работы с properties, JSON, временем,
 * индикаторами, HTTP-запросами. Эти классы не содержат бизнес-логики и используются
 * всеми компонентами системы.</p>
 *
 * <h2>Работа с конфигурацией</h2>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.utils.PropertiesUtils} — загрузка properties-файлов
 *       из classpath и файловой системы. Используется всеми конфигурационными классами.</li>
 * </ul>
 *
 * <h2>Сериализация</h2>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.utils.SerializationUtils} — сериализация/десериализация
 *       JSON через Gson. Поддерживает загрузку/сохранение данных на диск, работу с {@code TypeToken},
 *       кастомные десериализаторы (например, для {@code TickerInfo.Key}).</li>
 * </ul>
 *
 * <h2>Время</h2>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.utils.TimeUtils} — утилиты для работы со временем:
 *       {@code sleep(long millis)} с обработкой {@code InterruptedException}.</li>
 * </ul>
 *
 * <h2>Предикаты</h2>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.utils.PredicateUtils} — утилиты для предикатов:
 *       {@code distinctByKey()} для фильтрации дубликатов в стримах.</li>
 * </ul>
 *
 * <h2>Технические индикаторы</h2>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.utils.IndicatorsUtil} — расчет технических индикаторов:
 *       RSI, MACD, ATR, скользящие средние (SMA, EMA). Использует TA-Lib библиотеку.</li>
 * </ul>
 *
 * <h2>HTTP запросы</h2>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.utils.RequestUtils} — HTTP запросы с повторными попытками:
 *       {@code requestWithRetry()} для устойчивой работы с внешними API (TradingView, Telegram).</li>
 * </ul>
 *
 * <h2>Уровни (LevelUtils)</h2>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.utils.LevelUtils} — работа с уровнями поддержки/сопротивления:
 *       классы {@code Level}, методы для определения пробоев уровней.</li>
 * </ul>
 *
 * <h2>GerchikUtils</h2>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.utils.GerchikUtils} — методы по системе Герчика:
 *       расчет уровней, паттерны входа, управление позицией.</li>
 * </ul>
 *
 * <h2>TickerTypeResolver</h2>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.utils.TickerTypeResolver} — определение типа тикера
 *       (STOCK, BOND, ETF, CURRENCY) по названию. Использует эвристики и префиксы.</li>
 * </ul>
 *
 * <h2>Потокобезопасность</h2>
 * <p>Большинство утилитных классов stateless и потокобезопасны. Исключения:</p>
 * <ul>
 *   <li>{@code SerializationUtils} — синхронизированные методы для загрузки/сохранения.</li>
 *   <li>{@code IndicatorsUtil} — требует внешнюю синхронизацию при параллельных вычислениях.</li>
 * </ul>
 *
 * @see com.github.shk0da.GoldenDragon.config
 * @see com.github.shk0da.GoldenDragon.strategy
 * @see com.github.shk0da.GoldenDragon.service
 */
package com.github.shk0da.GoldenDragon.utils;
