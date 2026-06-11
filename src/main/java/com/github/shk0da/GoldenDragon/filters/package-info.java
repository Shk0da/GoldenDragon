/**
 * Фильтры торговых сигналов приложения GoldenDragon.
 *
 * <h2>Назначение пакета</h2>
 * <p>Пакет {@code filters} содержит классы для фильтрации торговых сигналов перед исполнением.
 * Фильтры проверяют рыночные условия, подтверждение от коррелированных инструментов,
 * экстремальные рыночные ситуации. Используются стратегиями для уменьшения ложных входов.</p>
 *
 * <h2>Ключевые фильтры</h2>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.filters.MarketRegimeFilter} — фильтр рыночного режима.
 *       Определяет тренд/флэт по ADX (Average Directional Index), волатильности (ATR), объёмам.
 *       Возвращает разрешение на торговлю с коэффициентом доверия (confidence) и множителем позиции.
 *       <br><b>Параметры</b>: ADX период (14), ATR период (14), volume период (50).</li>
 *
 *   <li>{@link com.github.shk0da.GoldenDragon.filters.GroupConfirmationFilter} — фильтр группового
 *       подтверждения. Проверяет, движутся ли коррелированные инструменты (peer instruments)
 *       в том же направлении. Требует минимальное количество подтверждений (min 2 из 3).
 *       <br><b>Использование</b>: {@code UnifiedStrategy} для проверки сигналов.</li>
 *
 *   <li>{@link com.github.shk0da.GoldenDragon.filters.BadWeatherFilter} — фильтр "плохой погоды".
 *       Запрещает торговлю при неблагоприятных условиях:
 *       <ul>
 *         <li>Низкий объём (ниже среднего * порог).</li>
 *         <li>Низкая волатильность (ATR ниже среднего).</li>
 *         <li>Аномально высокий спред.</li>
 *         <li>Большая свеча с длинными тенями (panic candle).</li>
 *         <li>Скачок волатильности (ATR spike).</li>
 *       </ul>
 *       <br><b>Параметры</b>: настраиваемые пороги для каждого условия.</li>
 * </ul>
 *
 * <h2>Интеграция со стратегиями</h2>
 * <p>Фильтры вызываются в методе {@code decide()} стратегий перед генерацией сигнала:</p>
 * <pre>{@code
 * // Пример использования в BaseStrategy
 * MarketRegimeFilter.FilterResult regimeResult = marketRegimeFilter.checkRegime(ticker, candles);
 * if (!regimeResult.canTrade) return TradingDecision.HOLD(regimeResult.reason);
 *
 * boolean groupConfirmed = GroupConfirmationFilter.isConfirmed(ticker, isBuy, peerCandles);
 * if (!groupConfirmed) return TradingDecision.HOLD("noGroupConf");
 *
 * BadWeatherFilter.WeatherCondition weather = badWeatherFilter.checkWeather(candles);
 * if (!weather.isGood) return TradingDecision.HOLD("badWeather");
 * }</pre>
 *
 * <h2>Результаты фильтрации</h2>
 * <p>Фильтры возвращают:</p>
 * <ul>
 *   <li>Разрешение на торговлю (boolean).</li>
 *   <li>Коэффициент доверия (confidence 0.0–1.0).</li>
 *   <li>Множитель позиции (positionMultiplier) для уменьшения размера при неопределенности.</li>
 *   <li>Причину отказа (reason) для логирования.</li>
 * </ul>
 *
 * <h2>Потокобезопасность</h2>
 * <p>Фильтры stateless и потокобезопасны. Параметры задаются в конструкторе и не изменяются.
 * Методы могут вызываться из разных потоков стратегии.</p>
 *
 * @see com.github.shk0da.GoldenDragon.strategy.BaseStrategy
 * @see com.github.shk0da.GoldenDragon.strategy.UnifiedStrategy
 */
package com.github.shk0da.GoldenDragon.filters;
