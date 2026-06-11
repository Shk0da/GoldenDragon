/**
 * Торговые стратегии и алгоритмы принятия решений приложения GoldenDragon.
 *
 * <h2>Назначение пакета</h2>
 * <p>Пакет {@code strategy} содержит реализацию торговых стратегий, базовые классы для управления
 * жизненным циклом стратегии, индикаторы, фильтры рыночных режимов и вспомогательные компоненты
 * для генерации торговых сигналов. Стратегии работают в реальном времени (live trading) и могут
 * запускаться через бэктест-движок из пакета {@code test}.</p>
 *
 * <h2>Архитектура стратегий</h2>
 * <p>Все стратегии следуют общей архитектуре:</p>
 * <ol>
 *   <li><b>Инициализация</b>: загрузка конфигурации, подписка на рыночные данные, инициализация
 *       индикаторов и фильтров.</li>
 *   <li><b>Основной цикл</b>: периодический опрос рыночных данных (свечи, стаканы, сделки),
 *       вычисление индикаторов, генерация сигналов.</li>
 *   <li><b>Принятие решений</b>: проверка сигналов через фильтры (рыночный режим, риск-менеджмент),
 *       расчёт размера позиции, установка стоп-лосса и тейк-профита.</li>
 *   <li><b>Исполнение</b>: отправка ордеров через {@link com.github.shk0da.GoldenDragon.service.TCSService},
 *       мониторинг исполнения, управление открытыми позициями.</li>
 *   <li><b>Завершение</b>: закрытие позиций по сигналу, таймауту или окончанию торговой сессии.</li>
 * </ol>
 *
 * <h2>Ключевые классы</h2>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.strategy.BaseStrategy} — абстрактный базовый класс
 *       для стратегий, торгующих по свечам. Реализует:
 *       <ul>
 *         <li>Жизненный цикл: рабочие часы, проверка торговых дней, EOD-закрытие позиций.</li>
 *         <li>Загрузку и кеширование исторических свечей (часовые, 5-минутные).</li>
 *         <li>Параллельную обработку нескольких тикеров через {@link java.util.concurrent.ExecutorService}.</li>
 *         <li>Управление капиталом: распределение кэша по тикерам, cooldown'ы после ошибок.</li>
 *         <li>Технические индикаторы: RSI, MACD, ATR, скользящие средние (через {@code IndicatorsUtil}).</li>
 *       </ul>
 *       <p>Конкретные стратегии наследуются от {@code BaseStrategy} и реализуют метод
 *       {@code decide()}, который возвращает {@link com.github.shk0da.GoldenDragon.model.TradingDecision}.</p>
 *   </li>
 *
 *   <li>{@link com.github.shk0da.GoldenDragon.strategy.UnifiedStrategy} — универсальная стратегия
 *       среднесрочной торговли, наследуется от {@code BaseStrategy}. Комбинирует:
 *       <ul>
 *         <li>Технические индикаторы (RSI, MACD, ATR) для входа и выхода.</li>
 *         <li>Фильтры рыночного режима (волатильность, тренд).</li>
 *         <li>Портфельное управление капиталом (распределение по тикерам).</li>
 *         <li>Групповые подтверждения (peer confirmation) — проверка коррелированных инструментов.</li>
 *       </ul>
 *   </li>
 *
 *   <li>{@link com.github.shk0da.GoldenDragon.strategy.RegimeAwareStrategy} — стратегия с адаптацией
 *       к рыночному режиму. Определяет режим по волатильности и объёмам, переключает логику входа
 *       и параметры риска в зависимости от режима.</li>
 *
 *   <li>{@link com.github.shk0da.GoldenDragon.strategy.RegimeAwareStrategyMl} — расширение
 *       {@code RegimeAwareStrategy} с использованием ML-модели (XGBoost) для классификации режима
 *       и прогнозирования направления движения цены.</li>
 *
 *   <li>{@link com.github.shk0da.GoldenDragon.strategy.DivFlow} — стратегия дивидендной торговли.
 *       Анализирует дивидендную доходность, даты отсечек, фундаментальные показатели.</li>
 *
 *   <li>{@link com.github.shk0da.GoldenDragon.strategy.LevelTrader} — торговля от ключевых уровней
 *       поддержки/сопротивления. Использует исторические экстремумы, объёмные профили.</li>
 *
 *   <li>{@link com.github.shk0da.GoldenDragon.strategy.IndicatorTrader} — индикаторная стратегия,
 *       фокусируется на технических индикаторах без привязки к паттернам.</li>
 *
 *   <li>{@link com.github.shk0da.GoldenDragon.strategy.Rebalance} — стратегия
 *       периодической ребалансировки портфеля по целевым весам.</li>
 *
 *   <li>{@link com.github.shk0da.GoldenDragon.strategy.ModelGenerator} — генерация и валидация
 *       торговых моделей (гиперпараметры, кросс-валидация).</li>
 *
 *   <li>{@link com.github.shk0da.GoldenDragon.strategy.RSX} — индикатор RSI с экспоненциальным
 *       сглаживанием (RSX = RSI Smoothed).</li>
 *
 *   <li>{@link com.github.shk0da.GoldenDragon.strategy.DataCollector} — сбор данных для ML-модели:
 *       сохранение свечей, позиций, решений в формате для обучения.</li>
 * </ul>
 *
 * <h2>Интерфейсы и абстракции</h2>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.model.MarketTickListener} — интерфейс для получения
 *       обновлений рыночных данных в реальном времени (стаканы, сделки). Реализуется стратегиями
 *       для подписки на стримы.</li>
 *
 * </ul>
 *
 * <h2>Управление состоянием</h2>
 * <p>Стратегии хранят состояние в полях классов:</p>
 * <ul>
 *   <li>Позиции: {@link com.github.shk0da.GoldenDragon.model.Position},
 *       {@link com.github.shk0da.GoldenDragon.model.PositionInfo}.</li>
 *   <li>Рыночные данные: {@link com.github.shk0da.GoldenDragon.model.Candle},
 *       {@link com.github.shk0da.GoldenDragon.model.MarketDepthSnapshot},
 *       {@link com.github.shk0da.GoldenDragon.model.MarketTradeTick}.</li>
 *   <li>Сигналы: {@link com.github.shk0da.GoldenDragon.model.TradingDecision}.</li>
 * </ul>
 * <p>Для потокобезопасности используются {@link java.util.concurrent.ConcurrentHashMap},
 * {@link java.util.concurrent.CopyOnWriteArrayList} и явная синхронизация на объектах состояния.</p>
 *
 * <h2>Потокобезопасность</h2>
 * <ul>
 *   <li>{@code BaseStrategy} — использует {@code ExecutorService} с пулом потоков, каждый тикер
 *       обрабатывается в отдельном потоке. Состояние по тикерам разделяется через {@code ConcurrentHashMap}.</li>
 *   <li>Остальные стратегии — однопоточные в рамках одного тикера, многопоточные на уровне пула тикеров.</li>
 * </ul>
 *
 * <h2>Конфигурация</h2>
 * <p>Стратегии используют конфигурацию из:</p>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.config.UnifiedTraderConfig} — параметры для
 *       {@code BaseStrategy} и наследников (тикеры, лимиты, индикаторы).</li>
 *   <li>{@link com.github.shk0da.GoldenDragon.config.MainConfig} — общие настройки (API-ключи,
 *       режим песочницы, флаг тестовой торговли).</li>
 * </ul>
 *
 * <h2>Логирование и уведомления</h2>
 * <p>Стратегии пишут логи в {@link java.lang.System#out} с временными метками
 * {@code dd.MM.yyyy HH:mm:ss}. Критические события (открытие/закрытие позиций, ошибки)
 * дублируются в Telegram через {@link com.github.shk0da.GoldenDragon.service.TelegramNotifyService}.</p>
 *
 * <h2>Бэктестинг</h2>
 * <p>Стратегии могут запускаться в режиме бэктеста через
 * {@link com.github.shk0da.GoldenDragon.test.BacktestRunner}
 * исполнение ордеров на исторических данных с учётом комиссий, рабочих часов и портфельного управления.</p>
 *
 * <h2>Расширение</h2>
 * <p>Для создания новой стратегии:</p>
 * <ol>
 *   <li>Наследоваться от {@code BaseStrategy} (для свечной торговли) или реализовать
 *       {@code MarketTickListener} (для order flow).</li>
 *   <li>Реализовать метод {@code decide()} (для {@code BaseStrategy}) или
 *       {@code onOrderBook()}/{@code onTrade()} (для {@code MarketTickListener}).</li>
 *   <li>Добавить конфигурационный класс (если нужны параметры).</li>
 *   <li>Зарегистрировать стратегию в {@link com.github.shk0da.GoldenDragon.GoldenDragon#main}
 *       или в бэктест-движке.</li>
 * </ol>
 *
 * @see com.github.shk0da.GoldenDragon.service
 * @see com.github.shk0da.GoldenDragon.test
 * @see com.github.shk0da.GoldenDragon.model
 * @see com.github.shk0da.GoldenDragon.config
 */
package com.github.shk0da.GoldenDragon.strategy;
