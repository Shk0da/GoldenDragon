/**
 * Сервисный слой приложения GoldenDragon — интеграция с внешними системами и инфраструктурные компоненты.
 *
 * <h2>Назначение пакета</h2>
 * <p>Пакет {@code service} содержит классы, отвечающие за взаимодействие с внешними API и сервисами:
 * брокерское API (Тинькофф Инвестиции), Telegram-бот для уведомлений, TradingView для сканирования рынка.
 * Эти классы не содержат торговой логики — они предоставляют низкоуровневые абстракции для работы
 * с биржевыми данными, исполнения ордеров и отправки сообщений.</p>
 *
 * <h2>Ключевые компоненты</h2>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.service.TCSService} — основной сервис для работы
 *       с брокерским API (Тинькофф Инвестиции). Предоставляет методы для:
 *       <ul>
 *         <li>Получения рыночных данных: стаканы (order book), последние сделки, исторические свечи.</li>
 *         <li>Исполнения ордеров: рыночные и лимитные заявки на покупку/продажу.</li>
 *         <li>Управления позициями: закрытие позиций, установка стоп-лоссов и тейк-профитов.</li>
 *         <li>Портфеля: получение доступного кэша, текущих позиций, общей стоимости портфеля.</li>
 *         <li>Поиска инструментов: конвертация тикеров в FIGI, кеширование метаданных инструментов.</li>
 *       </ul>
 *       <p>Класс инкапсулирует {@link ru.tinkoff.piapi.core.InvestApi} и предоставляет удобный интерфейс
 *       для стратегий. Поддерживает работу с песочницей (sandbox mode) и ведение логов с временными метками.</p>
 *   </li>
 *
 *   <li>{@link com.github.shk0da.GoldenDragon.service.TelegramNotifyService} — синглтон для отправки
 *       уведомлений в Telegram. Используется для:
 *       <ul>
 *         <li>Оповещений об открытии/закрытии позиций.</li>
 *         <li>Сообщений о достижении лимитов (дневной убыток, ошибки).</li>
 *         <li>Стартовых/финальных сообщений стратегии.</li>
 *       </ul>
 *       <p>Отправка выполняется асинхронно через {@link java.util.concurrent.ExecutorService}, чтобы не
 *       блокировать основной поток торговли. Поддерживает режим extended-уведомлений (только важные события).</p>
 *   </li>
 *
 *   <li>{@link com.github.shk0da.GoldenDragon.service.TradingViewService} — сканер рынка через API
 *       TradingView. Позволяет:
 *       <ul>
 *         <li>Фильтровать акции по фундаментальным показателям (долг/капитал, рекомендация аналитиков,
 *             рыночная капитализация, выручка).</li>
 *         <li>Получать список тикеров MOEX для дальнейшего анализа.</li>
 *         <li>Сканировать рынок по произвольным фильтрам и спискам тикеров.</li>
 *       </ul>
 *       <p>Используется на этапе предторгового анализа для отбора ликвидных инструментов с приемлемым
 *           финансовым состоянием.</p>
 *   </li>
 * </ul>
 *
 * <h2>Взаимодействие со стратегиями</h2>
 * <p>Стратегии из пакета {@code strategy} используют сервисы следующим образом:</p>
 * <ul>
 *   <li>{@code TCSService} — прямой вызов через {@link com.github.shk0da.GoldenDragon.service.TCSService}
 *       или через интерфейс {@code TradingGateway} (адаптер для упрощения тестирования).</li>
 *   <li>{@code TelegramNotifyService} — через статический экземпляр
 *       {@link com.github.shk0da.GoldenDragon.service.TelegramNotifyService#telegramNotifyService}.</li>
 *   <li>{@code TradingViewService} — через {@link com.github.shk0da.GoldenDragon.service.TradingViewService#INSTANCE}
 *       для сканирования рынка перед запуском стратегии.</li>
 * </ul>
 *
 * <h2>Потокобезопасность</h2>
 * <ul>
 *   <li>{@code TCSService} — не является полностью потокобезопасным. Внутренние коллекции
 *       ({@code ConcurrentHashMap}, {@code CopyOnWriteArrayList}) защищены от конкурентного доступа,
 *       но вызовы методов исполнения ордеров должны синхронизироваться на уровне стратегии.</li>
 *   <li>{@code TelegramNotifyService} — потокобезопасен благодаря использованию {@code ExecutorService}
 *       с одиночным потоком (очередь сообщений).</li>
 *   <li>{@code TradingViewService} — состояние отсутствует (stateless), потокобезопасен.</li>
 * </ul>
 *
 * <h2>Конфигурация</h2>
 * <p>Сервисы используют конфигурацию из:</p>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.config.MainConfig} — API-ключи, аккаунт, режим песочницы.</li>
 *   <li>{@link com.github.shk0da.GoldenDragon.config.MarketConfig} — базовая валюта, параметры рынка.</li>
 *   <li>{@link com.github.shk0da.GoldenDragon.config.TelegramNotifyConfig} — токен бота, chat_id, режим уведомлений.</li>
 * </ul>
 *
 * <h2>Логирование</h2>
 * <p>Сервисы пишут логи в {@link java.lang.System#out} с временными метками в формате
 * {@code dd.MM.yyyy HH:mm:ss}. Форматирование выполняется через
 * {@link java.time.format.DateTimeFormatter} или {@link java.text.SimpleDateFormat}.</p>
 *
 * @see com.github.shk0da.GoldenDragon.strategy
 * @see com.github.shk0da.GoldenDragon.config
 * @see com.github.shk0da.GoldenDragon.model
 */
package com.github.shk0da.GoldenDragon.service;
