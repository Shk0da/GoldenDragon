/**
 * Конфигурационные классы приложения GoldenDragon.
 *
 * <h2>Назначение пакета</h2>
 * <p>Пакет {@code config} содержит классы конфигурации для различных компонентов системы:
 * торговых стратегий, сервисов, рыночных настроек. Конфигурация загружается из properties-файлов
 * и предоставляет типизированный доступ к параметрам.</p>
 *
 * <h2>Ключевые классы</h2>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.config.MainConfig} — основная конфигурация приложения:
 *       API-ключи Tinkoff Invest, настройки песочницы, тестовый режим, HTTP клиент.</li>
 *
 *   <li>{@link com.github.shk0da.GoldenDragon.config.MarketConfig} — настройки рынка (MOEX):
 *       валюта, лимиты на позицию, параметры рынка.</li>
 *
 *   <li>{@link com.github.shk0da.GoldenDragon.config.LevelTraderConfig} — параметры для LevelTrader:
 *       стоп-лосс, тейк-профит, подтверждение уровней, список инструментов.</li>
 *
 *   <li>{@link com.github.shk0da.GoldenDragon.config.UnifiedTraderConfig} — конфигурация для UnifiedStrategy:
 *       параметры индикаторов, лимиты, настройки money management.</li>
 *
 *   <li>{@link com.github.shk0da.GoldenDragon.config.RSXConfig} — настройки для RSX стратегии:
 *       трендовый тикер, максимальный размер портфеля.</li>
 *
 *   <li>{@link com.github.shk0da.GoldenDragon.config.DivFlowConfig} — константы для DivFlow:
 *       URL календарей дивидендов (Smart-Lab, Dohod, Investing.com).</li>
 *
 *   <li>{@link com.github.shk0da.GoldenDragon.config.TelegramNotifyConfig} — Telegram уведомления:
 *       токен бота, chat_id, режим расширенных уведомлений.</li>
 *
 *   <li>{@link com.github.shk0da.GoldenDragon.config.DataCollectorConfig} — сбор данных:
 *       директория для данных, список инструментов, режим замены.</li>
 *
 *   <li>{@link com.github.shk0da.GoldenDragon.config.RebalanceConfig} — ребалансировка портфеля:
 *       целевые веса позиций, проценты для ребалансировки.</li>
 * </ul>
 *
 * <h2>Загрузка конфигурации</h2>
 * <p>Все классы конфигурации используют {@link com.github.shk0da.GoldenDragon.utils.PropertiesUtils}
 * для загрузки настроек из {@code application.properties}. Значения по умолчанию указываются в конструкторах.</p>
 *
 * <h2>Потокобезопасность</h2>
 * <p>Классы конфигурации иммутабельны после создания (кроме MainConfig с изменяемым accountId).
 * Конфигурация загружается один раз при создании экземпляра.</p>
 *
 * @see com.github.shk0da.GoldenDragon.strategy
 * @see com.github.shk0da.GoldenDragon.service
 * @see com.github.shk0da.GoldenDragon.utils.PropertiesUtils
 */
package com.github.shk0da.GoldenDragon.config;
