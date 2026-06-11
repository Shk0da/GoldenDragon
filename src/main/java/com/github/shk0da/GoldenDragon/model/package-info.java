/**
 * Модельные классы и структуры данных приложения GoldenDragon.
 *
 * <h2>Назначение пакета</h2>
 * <p>Пакет {@code model} содержит классы данных (DTO), интерфейсы и enum для представления
 * рыночных данных, позиций, инструментов и торговых решений. Эти классы используются
 * всеми компонентами системы: стратегиями, сервисами, репозиториями.</p>
 *
 * <h2>Рыночные данные</h2>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.model.Candle} — свеча (OHLCV) для бэктестов и анализа.</li>
 *   <li>{@link com.github.shk0da.GoldenDragon.model.MarketDepthSnapshot} — снимок стакана (bids/asks).</li>
 *   <li>{@link com.github.shk0da.GoldenDragon.model.MarketDepthLevel} — уровень стакана (цена/объем).</li>
 *   <li>{@link com.github.shk0da.GoldenDragon.model.MarketTradeTick} — тик последней сделки.</li>
 * </ul>
 *
 * <h2>Инструменты и тикеры</h2>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.model.TickerInfo} — информация об инструменте:
 *       FIGI, ISIN, валюта, тип, лот.</li>
 *   <li>{@link com.github.shk0da.GoldenDragon.model.TickerType} — enum типов инструментов:
 *       STOCK, BOND, ETF, CURRENCY, FEATURE.</li>
 *   <li>{@link com.github.shk0da.GoldenDragon.model.Market} — enum рынков: MOEX.</li>
 *   <li>{@link com.github.shk0da.GoldenDragon.model.Group} — enum групп инструментов:
 *       TREND, FX, MIXED.</li>
 * </ul>
 *
 * <h2>Позиции и портфель</h2>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.model.Position} — торговая позиция:
 *       направление, количество, цена входа.</li>
 *   <li>{@link com.github.shk0da.GoldenDragon.model.PositionInfo} — информация о позиции из API:
 *       баланс, лоты, ожидаемая доходность.</li>
 *   <li>{@link com.github.shk0da.GoldenDragon.model.PortfolioPosition} — целевая позиция портфеля:
 *       имя, тип, процент в портфеле.</li>
 * </ul>
 *
 * <h2>Торговые решения</h2>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.model.TradingDecision} — решение стратегии:
 *       действие (BUY/SELL/HOLD/CLOSE), сигнал, количество, цены.</li>
 * </ul>
 *
 * <h2>Специализированные модели</h2>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.model.DiviTicker} — дивидендная информация:
 *       тикер, дата отсечки, дивиденд, доходность.</li>
 *   <li>{@link com.github.shk0da.GoldenDragon.model.TickerScan} — результат сканирования TradingView:
 *       рекомендация аналитиков, долг/капитал.</li>
 *   <li>{@link com.github.shk0da.GoldenDragon.model.TickerJson} — сериализуемые данные для LevelTrader:
 *       тикер + уровни поддержки/сопротивления.</li>
 *   <li>{@link com.github.shk0da.GoldenDragon.model.Config} — конфигурация для ML-стратегий
 *       (RegimeAwareStrategyMl).</li>
 * </ul>
 *
 * <h2>Интерфейсы</h2>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.model.MarketTickListener} — интерфейс для получения
 *       рыночных данных в реальном времени (стаканы, сделки). Реализуется стратегиями.</li>
 * </ul>
 *
 * <h2>Потокобезопасность</h2>
 * <p>Большинство классов данных иммутабельны или используются в однопоточном контексте.
 * Для потокобезопасности в стратегиях используются {@code ConcurrentHashMap} и явная синхронизация.</p>
 *
 * @see com.github.shk0da.GoldenDragon.strategy
 * @see com.github.shk0da.GoldenDragon.service
 * @see com.github.shk0da.GoldenDragon.repository
 */
package com.github.shk0da.GoldenDragon.model;
