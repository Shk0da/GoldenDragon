/**
 * Тестирование, бэктестинг и сбор данных приложения GoldenDragon.
 *
 * <h2>Назначение пакета</h2>
 * <p>Пакет {@code test} содержит инструменты для тестирования торговых стратегий на исторических
 * данных (бэктестинг), симуляции рыночных событий и сбора данных для машинного обучения.
 * Классы этого пакета не используются в production-торговле, но критически важны для:
 * валидации стратегий, подбора гиперпараметров, анализа рисков и обучения ML-моделей.</p>
 *
 * <h2>Ключевые компоненты</h2>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.test.BacktestRunner} — движок бэктестинга для
 *       стратегий, работающих со свечами ({@code BaseStrategy} и наследники). Возможности:
 *       <ul>
 *         <li><b>Поддерживаемые стратегии</b>: {@link com.github.shk0da.GoldenDragon.strategy.UnifiedStrategy},
 *             {@link com.github.shk0da.GoldenDragon.strategy.RegimeAwareStrategy},
 *             {@link com.github.shk0da.GoldenDragon.strategy.RegimeAwareStrategyMl}.</li>
 *         <li><b>Режимы запуска</b>: {@code "fast"} (6 коротких периодов, декабрь 2025 – май 2026)
 *             или {@code "full"} (4 годовых периода, 2023–2026).</li>
 *         <li><b>Параллельная загрузка данных</b>: пул потоков ({@code backtest.threads}, по умолчанию
 *             {@code availableProcessors - 1}) для чтения CSV-файлов свечей.</li>
 *         <li><b>Портфельная симуляция</b>: общий кэш на все тикеры, распределение капитала,
 *             учёт комиссий (0.05% двусторонняя), рабочие часы (10:00–21:00, пн–пт).</li>
 *         <li><b>Глобальная временная шкала</b>: объединение минутных меток всех тикеров в
 *             {@link java.util.TreeSet} для синхронной обработки.</li>
 *         <li><b>Статистика</b>: по каждой стратегии — PnL, количество сделок, % прибыльных,
 *             максимальная просадка, Sharpe Ratio, итоговый баланс. Сравнительная таблица стратегий.</li>
 *       </ul>
 *       <p>Бэктест имитирует реальную торговлю: проверяет рабочие часы, закрывает позиции в EOD,
 *       применяет комиссию, соблюдает лимиты на позицию и портфель.</p>
 *   </li>
 * </ul>
 *
 * <h2>Структура данных для бэктеста</h2>
 * <ul>
 *   <li><b>Свечи</b>: CSV-файлы {@code candlesHOUR.txt} (часовые) и {@code candles5_MIN.txt}
 *       (5-минутные). Формат строки:
 *       <pre>{@code dd.MM.yyyy HH:mm:ss,open,high,low,close,volume}</pre>
 *       Файлы хранятся в {@code data/<TICKER>/}.</li>
 *   <li><b>Стаканы и сделки</b>: CSV-файлы {@code ticks.txt} для order flow. Формат:
 *       <pre>{@code time,best_bid,best_ask,mid_price,bids,asks}</pre>
 *       где {@code bids/asks} — цепочки уровней {@code price:quantity|price:quantity|...}.</li>
 *   <li><b>Периоды</b>: задаются в properties ({@code backtest.periods}) или через system properties
 *       ({@code backtest.mode}).</li>
 * </ul>
 *
 * <h2>Процесс бэктестинга</h2>
 * <ol>
 *   <li><b>Загрузка тикеров</b>: из properties ({@code datacollector.instruments},
 *       {@code unifiedTrader.ticker.<NAME>.*}). Фильтрация по флагу {@code params.enabled}.</li>
 *   <li><b>Загрузка данных</b>: параллельное чтение CSV для каждого тикера через
 *       {@link java.util.concurrent.ExecutorService}. Сортировка по времени, фильтрация по диапазону дат.</li>
 *   <li><b>Создание экземпляров стратегий</b>: по одному экземпляру на тикер (для изоляции состояния).</li>
 *   <li><b>Построение временной шкалы</b>: объединение всех меток времени в {@code TreeSet}
 *       для синхронной обработки.</li>
 *   <li><b>Основной цикл</b>: для каждой временной метки:
 *       <ul>
 *         <li>Обновление свечей/стаканов для тикеров, у которых есть данные.</li>
 *         <li>Вызов метода стратегии ({@code onTick()}, {@code onOrderBook()}, {@code onTrade()}).</li>
 *         <li>Проверка сигналов, исполнение ордеров через {@code TradingGateway}.</li>
 *         <li>Обновление позиций, PnL, кэша.</li>
 *       </ul>
 *   </li>
 *   <li><b>Финализация</b>: закрытие позиций, подсчёт статистики, вывод отчёта.</li>
 * </ol>
 *
 * <h2>Сбор данных для ML</h2>
 * <p>Классы для сбора размеченных данных обучения:</p>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.ml.TradeDataCollector} — сбор данных о сделках:
 *       вход/выход, PnL, индикаторы на момент входа, рыночный режим. Сохраняет в CSV для
 *       обучения моделей ({@code XGBoost}, {@code Random Forest}).</li>
 *   <li>{@link com.github.shk0da.GoldenDragon.strategy.DataCollector} — сбор свечей, позиций,
 *       решений стратегии в реальном времени. Используется для накопления датасета.</li>
 * </ul>
 * <p>Формат данных для ML:</p>
 * <pre>
 * {@code
 * entry_time,ticker,strategy,entry_price,exit_price,pnl,commission,
 * rsi,macd,atr,regime,label
 * }
 * </pre>
 *
 * <h2>Запуск бэктеста</h2>
 * <ul>
 *   <li><b>Через Gradle</b>:
 *       <pre>{@code ./gradlew clean runBacktest}</pre>
 *       Запускает {@code BacktestRunner.main()} для всех стратегий из {@code ALL_STRATEGIES}.</li>
 *   <li><b>Одна стратегия</b>:
 *       <pre>{@code ./gradlew runStrategy -Pstrategy=UnifiedStrategy}</pre>
 *       Запускает конкретную стратегию в режиме бэктеста.</li>
 *   <li><b>Полная сборка</b>:
 *       <pre>{@code ./gradlew clean uberJar}</pre>
 *       Создаёт fat-jar для запуска на сервере.</li>
 * </ul>
 *
 * <h2>Конфигурация бэктеста</h2>
 * <ul>
 *   <li>{@code backtest.mode}: {@code "fast"} или {@code "full"} (режим_periods).</li>
 *   <li>{@code backtest.threads}: число потоков для загрузки данных.</li>
 *   <li>{@code backtest.startBalance}: начальный баланс (по умолчанию 1 000 000).</li>
 *   <li>{@code backtest.commissionPercent}: комиссия (по умолчанию 0.05%).</li>
 *   <li>{@code datacollector.instruments}: список тикеров для тестирования.</li>
 * </ul>
 *
 * <h2>Отчётность</h2>
 * <p>После бэктеста выводится:</p>
 * <ul>
 *   <li>Сводная таблица стратегий: PnL, сделки, % побед, просадка, Sharpe, итоговый баланс.</li>
 *   <li>Детальный лог по каждой сделке: время, тикер, направление, цена входа/выхода, PnL.</li>
 *   <li>Графики (опционально): equity curve, просадка, распределение PnL (через внешние инструменты).</li>
 * </ul>
 *
 * <h2>Потокобезопасность</h2>
 * <ul>
 *   <li>{@code BacktestRunner} — использует {@code ExecutorService} для параллельной загрузки
 *       данных, но основной цикл симуляции однопоточный (последовательная обработка таймлайна).</li>
 *   <li>{@code MarketTickBacktestRunner} — однопоточный, симулирует последовательную подачу тиков.</li>
 *   <li>Коллекции для хранения данных ({@code List}, {@code Map}) не синхронизированы,
 *       так как доступ только из одного потока.</li>
 * </ul>
 *
 * <h2>Расширение</h2>
 * <p>Для добавления новой стратегии в бэктест:</p>
 * <ol>
 *   <li>Создать класс стратегии (наследовать от {@code BaseStrategy} или реализовать {@code MarketTickListener}).</li>
 *   <li>Добавить стратегию в {@code ALL_STRATEGIES} в {@code BacktestRunner}.</li>
 *   <li>При необходимости создать фабрику ({@code StrategyFactory}) для создания экземпляров с параметрами.</li>
 *   <li>Запустить бэктест через {@code ./gradlew clean runBacktest}.</li>
 * </ol>
 *
 * <h2>Взаимодействие с другими пакетами</h2>
 * <ul>
 *   <li>{@code strategy}: бэктест-движок создаёт экземпляры стратегий и симулирует их исполнение.</li>
 *   <li>{@code service}: в бэктесте используется {@code TradingGateway} вместо прямого вызова
 *       {@code TCSService} (симуляция исполнения).</li>
 *   <li>{@code model}: классы данных ({@code Candle}, {@code Position}, {@code TradingDecision})
 *       используются для хранения состояния.</li>
 *   <li>{@code config}: загрузка параметров тикеров, периодов, общих настроек бэктеста.</li>
 * </ul>
 *
 * <h2>Ограничения</h2>
 * <ul>
 *   <li>Бэктест не учитывает проскальзывание (slippage) в полной мере — используется цена закрытия свечи.</li>
 *   <li>Ликвидность не моделируется — ордера исполняются по цене рынка без учёта глубины стакана
 *       (кроме {@code MarketTickBacktestRunner}).</li>
 *   <li>Задержки сети и задержки исполнения ордеров не симулируются.</li>
 * </ul>
 *
 * @see com.github.shk0da.GoldenDragon.strategy
 * @see com.github.shk0da.GoldenDragon.service
 * @see com.github.shk0da.GoldenDragon.ml
 */
package com.github.shk0da.GoldenDragon.test;
