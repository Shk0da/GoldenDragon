/**
 * Машинное обучение и сбор данных для ML приложения GoldenDragon.
 *
 * <h2>Назначение пакета</h2>
 * <p>Пакет {@code ml} содержит компоненты для сбора размеченных данных, обучения моделей
 * классификации (XGBoost), прогнозирования рыночных режимов и торговых сигналов.
 * ML-модели используются в {@code RegimeAwareStrategyMl} для адаптации к рыночным условиям.</p>
 *
 * <h2>Сбор данных</h2>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.ml.TradeDataCollector} — сбор размеченных данных
 *       о сделках для обучения. Сохраняет в CSV:
 *       <ul>
 *         <li>Время входа/выхода, тикер, стратегия.</li>
 *         <li>Цена входа/выхода, PnL, комиссия.</li>
 *         <li>Индикаторы на момент входа (RSI, MACD, ATR, ADX).</li>
 *         <li>Рыночный режим (тренд/флэт).</li>
 *         <li>Метка (label): прибыльная/убыточная сделка.</li>
 *       </ul>
 *       <br><b>Формат</b>: {@code trades.csv} для обучения XGBoost.</li>
 *
 *   <li>{@link com.github.shk0da.GoldenDragon.ml.TradeFeatures} — признаки для ML-модели:
 *       технические индикаторы, рыночный режим, волатильность, объёмы.
 *       Используется как входной вектор для предсказания.</li>
 * </ul>
 *
 * <h2>Обучение моделей</h2>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.ml.MlModelTrainer} — обучение ML-модели (XGBoost)
 *       на исторических данных. Поддерживает:
 *       <ul>
 *         <li>Обучение на всех данных (общая модель).</li>
 *         <li>Обучение по тикерам (персональная модель для каждого инструмента).</li>
 *         <li>Кросс-валидацию, подбор гиперпараметров.</li>
 *         <li>Сохранение модели в текстовый формат ({@code .txt}).</li>
 *       </ul>
 *       <br><b>Запуск</b>: {@code ./gradlew runMlTraining} или {@code ./gradlew trainAllTickers}.</li>
 *
 *   <li>{@link com.github.shk0da.GoldenDragon.ml.MlAutoTrainingService} — автоматическое
 *       переобучение модели. Проверяет накопленные данные, запускает обучение при достижении
 *       минимального количества новых сделок. Используется в {@code RegimeAwareStrategyMl}.</li>
 * </ul>
 *
 * <h2>Прогнозирование</h2>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.ml.MlPredictionService} — сервис для предсказаний
 *       на основе обученной модели. Предоставляет:
 *       <ul>
 *         <li>{@code predictProbability(features)} — вероятность прибыльной сделки.</li>
 *         <li>{@code shouldTakeTrade(features)} — бинарное решение (брать сделку или нет).</li>
 *         <li>{@code getPositionSizeMultiplier(features)} — множитель размера позиции
 *             (увеличение при высокой уверенности, уменьшение при низкой).</li>
 *         <li>{@code getStopLossMultiplier(features)} — корректировка стоп-лосса.</li>
 *       </ul>
 *       <br><b>Модели</b>: поддерживает общую модель и персональные модели по тикерам.</li>
 * </ul>
 *
 * <h2>Интеграция со стратегиями</h2>
 * <p>ML-компоненты используются в {@code RegimeAwareStrategyMl}:</p>
 * <ol>
 *   <li>Перед входом: создание {@code TradeFeatures} с текущими индикаторами.</li>
 *   <li>Прогноз: {@code MlPredictionService.shouldTakeTrade(features)}.</li>
 *   <li>Размер позиции: {@code getPositionSizeMultiplier(features)} * базовый размер.</li>
 *   <li>После выхода: {@code TradeDataCollector.registerTrade()} для сбора данных.</li>
 *   <li>Переобучение: {@code MlAutoTrainingService.tryRetrain()} периодически.</li>
 * </ol>
 *
 * <h2>Gradle команды</h2>
 * <ul>
 *   <li>{@code ./gradlew runMlTraining} — обучить модель по умолчанию.</li>
 *   <li>{@code ./gradlew runMlTraining -Pdata=... -Poutput=...} — кастомные пути.</li>
 *   <li>{@code ./gradlew runMlTraining -Pticker=SBER} — модель для конкретного тикера.</li>
 *   <li>{@code ./gradlew trainAllTickers} — модели для всех тикеров.</li>
 *   <li>{@code ./gradlew generateModel} — полный pipeline (бэктест + обучение).</li>
 * </ul>
 *
 * <h2>Потокобезопасность</h2>
 * <p>{@code TradeDataCollector} и {@code MlPredictionService} потокобезопасны
 * (синхронизированные методы, concurrent коллекции).
 * {@code MlModelTrainer} запускается в отдельном потоке при переобучении.</p>
 *
 * @see com.github.shk0da.GoldenDragon.strategy.RegimeAwareStrategyMl
 * @see com.github.shk0da.GoldenDragon.strategy.BaseStrategy
 */
package com.github.shk0da.GoldenDragon.ml;
