/**
 * Управление капиталом и риск-менеджмент приложения GoldenDragon.
 *
 * <h2>Назначение пакета</h2>
 * <p>Пакет {@code money} содержит компоненты для управления размером позиции, контроля рисков,
 * отслеживания производительности и аварийной остановки торговли. Эти классы используются
 * стратегиями для money management (MM).</p>
 *
 * <h2>Расчет размера позиции</h2>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.money.SizingStrategy} — интерфейс для алгоритмов
 *       расчета размера позиции. Реализации вычисляют оптимальный размер на основе риска.</li>
 *
 *   <li>{@link com.github.shk0da.GoldenDragon.money.FixedRiskSizing} — фиксированный риск на сделку
 *       (например, 1% от капитала). Классический подход к позиционированию.</li>
 *
 *   <li>{@link com.github.shk0da.GoldenDragon.money.VolatilityAdjustedSizing} — размер позиции,
 *       корректируемый по волатильности (ATR). Меньший размер при высокой волатильности.</li>
 *
 *   <li>{@link com.github.shk0da.GoldenDragon.money.PositionSizer} — обертка над SizingStrategy.
 *       Применяет минимальный размер лота и шаг лота к расчету.</li>
 * </ul>
 *
 * <h2>Контроль рисков</h2>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.money.RiskManager} — менеджер рисков:
 *       лимиты на дневной убыток, серия проигрышей, риск на сделку.
 *       Метод {@code canTrade()} проверяет, разрешена ли торговля.</li>
 *
 *   <li>{@link com.github.shk0da.GoldenDragon.money.KillSwitch} — аварийная остановка торговли.
 *       Срабатывает при критической просадке, потере соединения, аномальном спреде.
 *       Блокирует торговлю до ручного сброса.</li>
 *
 *   <li>{@link com.github.shk0da.GoldenDragon.money.StopLossManager} — управление стоп-лоссами.
 *       Динамическое обновление стопа по прибыли (trailing stop), расчет по ATR.</li>
 * </ul>
 *
 * <h2>Адаптивный капитал</h2>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.money.AdaptiveCapital} — анти-мартингейл система.
 *       Уменьшает риск после серии убытков, восстанавливает после прибыльных сделок.
 *       Never uses martingale (no doubling down on losses).</li>
 * </ul>
 *
 * <h2>Отслеживание производительности</h2>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.money.PerformanceTracker} — статистика торговли:
 *       PnL, количество сделок, % побед, максимальная просадка, largest win/loss.
 *       Используется для адаптивного управления капиталом.</li>
 * </ul>
 *
 * <h2>Интеграция со стратегиями</h2>
 * <p>Компоненты money management используются в {@code UnifiedStrategy} и других стратегиях:</p>
 * <ol>
 *   <li>Перед входом: проверка {@code RiskManager.canTrade()}, {@code KillSwitch.isTradingAllowed()}.</li>
 *   <li>Расчет размера: {@code PositionSizer.calculateSize()} с учетом volatility.</li>
 *   <li>После выхода: {@code PerformanceTracker.registerTrade()}, {@code RiskManager.registerTrade()}.</li>
 *   <li>Обновление стопов: {@code StopLossManager.updateStopLoss()}.</li>
 * </ol>
 *
 * <h2>Потокобезопасность</h2>
 * <p>Все классы используют {@code AtomicReference}, {@code AtomicInteger}, {@code volatile}
 * для потокобезопасности. Методы могут вызываться из разных потоков стратегии.</p>
 *
 * @see com.github.shk0da.GoldenDragon.strategy.UnifiedStrategy
 * @see com.github.shk0da.GoldenDragon.strategy.BaseStrategy
 */
package com.github.shk0da.GoldenDragon.money;
