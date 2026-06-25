# PLAN: новая стратегия `PrecisionStrategy`

> Документ-промт для агента. Цель — создать новую торговую стратегию в проекте
> GoldenDragon, опираясь **только** на уже существующий код, инфраструктуру данных
> (`data/`) и бэктестер (`BacktestRunner`). Промт самодостаточен: его можно отдать
> агенту целиком, и он содержит контекст, целевые метрики, дизайн, точки интеграции,
> пошаговый план и критерии приёмки.

---

## 1. Целевые метрики (acceptance criteria)

Стратегия `PrecisionStrategy` должна на бэктесте (`backtest.mode=full`, 5 годовых
периодов, портфель из enabled-тикеров) показать:

- **Max drawdown ≤ 2–5%** (по equity-кривой портфеля).
- **Win rate ≥ 70%** (доля закрытых сделок с положительным PnL).
- **Доходность 5–10% в месяц** (среднемесячная по equity-кривой).

> ⚠️ **Реалистичность.** Одновременно «DD ≤ 5% + winrate ≥ 70% + 5–10%/мес» — крайне
> агрессивная комбинация. Высокий winrate обычно достигается малым take-profit
> относительно stop-loss (низкий R:R), что увеличивает риск «хвостовых» убытков и
> мешает удерживать просадку. Поэтому задача решается **итеративно и эмпирически**:
> мы строим стратегию с правильным каркасом риск-менеджмента и затем подбираем
> параметры через бэктест, фиксируя приёмочные ворота. Если все три цели
> недостижимы вместе — приоритет согласно `AGENTS.md`: сначала корректность и
> безопасность (DD), затем стабильность (winrate), затем доходность. В отчёте честно
> зафиксировать достигнутый фронт.

---

## 2. Выбор алгоритмического подхода (что и почему)

Вместо «одного индикатора» используем **ансамбль/конфлюэнс** проверенных подходов,
каждый из которых уже частично реализован в репозитории. Высокий winrate и низкая
просадка достигаются за счёт **редких входов только при совпадении нескольких
независимых условий** и жёсткого портфельного риск-оверлея.

Каркас `PrecisionStrategy` = комбинация четырёх «топовых» элементов:

1. **Trend-following pullback (вход на откате по тренду).**
   Торгуем только в направлении установившегося восходящего тренда (только long —
   шорты в проекте отключены). Вход не «по пробою», а на откате к динамической
   поддержке (EMA / ATR-канал). Это статистически даёт более высокий winrate и
   лучший entry, чем вход по импульсу.

2. **Mean-reversion подтверждение (RSI/откат).**
   Дополнительный фильтр перекупленности/перепроданности: входим, когда краткосрочный
   RSI указывает на завершение отката (а не на разворот тренда). Использует уже
   готовые `rsiVal(...)` / `IndicatorsUtil.rsi(...)`.

3. **ML-гейт вероятности успеха сделки.**
   Переиспользуем существующий ML-пайплайн (`MlPredictionService`,
   `trade_classifier_<TICKER>.txt`): вход разрешён только если предсказанная
   вероятность выигрыша ≥ порога (например 0.6–0.7). Это главный рычаг повышения
   winrate. Модели обучаются на сделках, сгенерированных бэктестом самой стратегии
   (см. цикл в разделе 7).

4. **Строгий риск-оверлей (контроль просадки).**
   Портфельный `KillSwitch` (стоп торговли при достижении лимита просадки), дневной
   лимит убытка `RiskManager`, волатильностный сайзинг `VolatilityAdjustedSizing` +
   `PositionSizer`, адаптивное снижение риска после серии убытков `AdaptiveCapital`,
   трейлинг/безубыток через `StopLossManager`. Это инструменты, которые жёстко
   ограничивают max drawdown.

**Confluence-правило входа (OPEN):** все условия должны выполняться одновременно:
тренд вверх **И** откат завершается (RSI) **И** рыночный режим благоприятен
(`MarketRegimeFilter`, ADX) **И** нет «плохой погоды» (`BadWeatherFilter`) **И**
peer-подтверждение (`GroupConfirmationFilter`) **И** ML-вероятность ≥ порога **И**
KillSwitch/RiskManager разрешают торговлю. Иначе — `HOLD`.

> Такой «И-фильтр» резко снижает частоту сделок, но повышает их качество — это
> сознательный размен ради winrate и просадки.

---

## 3. Точки интеграции в существующем коде

Все пути относительно корня репозитория. Пакет в коде: `com.github.shk0da.goldendragon.*`.

### 3.1 Базовый контракт

Наследуемся от `BaseStrategy`
(`src/main/java/com/github/shk0da/GoldenDragon/strategy/BaseStrategy.java`) — получаем
бесплатно: загрузку свечей, lifecycle, исполнение ордеров, MM, фильтры, peer-candles.

Обязательный к реализации абстрактный метод:

```java
public abstract TradingDecision decide(
    String ticker,
    List<Candle> hourCandles,
    List<Candle> minuteCandles,
    Position position,
    double balance,
    boolean incrementCandlesHeld);

protected abstract String getStrategyName();
```

Хуки для опционального переопределения: `onTradeClosed(...)`, `onDailyReset()`.

Эталон для подражания — `RegimeAwareStrategy.java` (тонкая обёртка над
`UnifiedStrategy`, регулирующая поведение через `decide`). Новая стратегия строится
по тому же паттерну: композиция поверх `UnifiedStrategy` + собственные фильтры
конфлюэнса, либо самостоятельная реализация `decide` с использованием утилит
`BaseStrategy`.

### 3.2 Доступные индикаторы (из `BaseStrategy`)

```java
protected double ema(List<Candle>, int period)
protected double atrVal(List<Candle>, int period)
protected double rsiVal(List<Candle>, int period)
protected double adxVal(List<Candle>, int period)   // либо calculateAdx(...)
```

Плюс `IndicatorsUtil` (`utils/IndicatorsUtil.java`) для TA-Lib-индикаторов при
необходимости (MACD, OBV, SMA).

### 3.3 Контракты решений

- `model/TradingDecision.java` — `action` ∈ {`HOLD`,`OPEN`,`CLOSE`}, плюс
  `confidence`, `quantity`, `stopLoss`, `takeProfit`, `entryPrice`, `updatedPosition`.
- `model/Position.java` — `direction`, `entryPrice`, `stopLoss`, `takeProfit`,
  `quantity`, `candlesHeld`, `cooldownRemaining`.

### 3.4 Money management / риск (пакет `money/`)

- `SizingStrategy` / `VolatilityAdjustedSizing` / `FixedRiskSizing` →
  `int calculateSize(ticker, entry, stopLoss, balance, atr)`
- `PositionSizer` — округление по лоту.
- `RiskManager` — `canTrade(equity)`, `registerTrade(pnl)`, дневной/серийный лимит.
- `StopLossManager` — `calculateInitialStop()`, `updateStopLoss()` (безубыток + ATR-трейлинг по R).
- `AdaptiveCapital` — `getRiskMultiplier()`, `registerWin/Loss()`.
- `KillSwitch` — `isTradingAllowed()`, `checkDrawdown()`, `trigger(reason)`.
- `PerformanceTracker` — статистика PnL/просадки.

### 3.5 Фильтры (пакет `filters/`)

- `MarketRegimeFilter` → `FilterResult(canTrade, confidence, positionMultiplier, reason)`.
- `BadWeatherFilter` — объём/ATR/спред/wick/panic.
- `GroupConfirmationFilter.isConfirmed(ticker, isBuy, peerCandles)`.

### 3.6 ML (пакет `ml/`)

- `MlPredictionService.loadModelForTicker(ticker, "ml_strategy/models/trade_classifier_" + ticker + ".txt")`
  и инференс вероятности выигрыша сделки.
- `TradeDataCollector` — запись фич/исхода сделок в `ml_strategy/data_pipeline/trades.csv`
  (бэктест уже умеет это через `recordBacktestTradeEntry/Outcome`).
- Обучение: `MlModelTrainer` (логистическая регрессия), задачи `runMlTraining` /
  `trainAllTickers`.
- Эталон ML-интеграции в стратегии — `RegimeAwareStrategyMl.java`.

### 3.7 Регистрация стратегии (ОБЯЗАТЕЛЬНО)

1. `test/BacktestRunner.java` → `StrategyFactory.createStrategy(...)`: добавить
   `case "PrecisionStrategy": return new PrecisionStrategy(config, null, new Config(), true);`
   и при необходимости добавить имя в массив `ALL_STRATEGIES`.
2. `GoldenDragon.java` (live entry point) → добавить ветку запуска `PrecisionStrategy`
   по аналогии с существующими стратегиями.

### 3.8 Конфигурация

- `src/main/resources/application.properties` — переиспользуем существующие
  `unifiedTrader.ticker.<SYMBOL>.*` (enabled, group, riskP, slMult, tpMult,
  allocationGroup, allocationWeight, параметры BadWeather/MarketRegime).
- Новые параметры стратегии (порог ML, период RSI отката, глубина отката, минимальный
  ADX, R:R, лимит просадки KillSwitch) добавить как поля с дефолтами в `model/Config.java`
  и/или как `unifiedTrader.precision.*`, читая через `PropertiesUtils`. Дефолты — в
  коде, чтобы стратегия работала «из коробки».

---

## 4. Формат данных (для справки, менять не требуется)

- `data/<TICKER>/candlesHOUR.txt`, `candles5_MIN.txt`: CSV
  `Datetime,Open,High,Low,Close,Volume`, время `dd.MM.yyyy HH:mm:ss`.
- `data/<TICKER>/ticker.json`: метаданные инструмента + `levels`.
- `data/<TICKER>/levels.txt`: уровни (одна цена на строку).
- Бэктест: warm-up `MIN_HOURS_REQUIRED = 60`, часы 10:00–21:00 МСК, пн–пт,
  `initialBalance = 1_000_000`, комиссия `0.0005`, EOD-закрытие в 21:00.

---

## 5. Пошаговый план реализации

> Делать **минимальными изменениями** (см. `AGENTS.md`): новый класс + регистрация в
> двух точках + дефолтные параметры. Без рефакторинга чужого кода и новых библиотек.

1. **Скелет класса.** Создать
   `src/main/java/com/github/shk0da/GoldenDragon/strategy/PrecisionStrategy.java`,
   `extends BaseStrategy`, конструктор как у `RegimeAwareStrategy`
   `(UnifiedTraderConfig, TCSService, Config, boolean isBacktest)`, реализовать
   `getStrategyName()` → `"PrecisionStrategy"` и заглушку `decide(...)` → `HOLD`.
   Пустая строка после объявления класса; полные импорты; стиль `CONST == value`.

2. **Регистрация и компиляция.** Добавить `case` в `BacktestRunner.StrategyFactory`
   и ветку в `GoldenDragon`. Проверить:
   `./gradlew clean compileJava`.

3. **Базовая логика входа (без ML).** В `decide(...)`:
   - guard'ы в начале: проверка размера `hourCandles` (≥ 60), наличие свечей;
   - детект тренда: `close > emaFast > emaSlow` (например EMA 20/50 по часам) и
     `adxVal(hourCandles, 14) ≥ ADX_MIN`;
   - детект завершённого отката: краткосрочный RSI вышел вверх из зоны
     отката (например пересёк снизу вверх ~40–45 после касания ~30–35), либо цена
     откатилась к EMA-fast и отскочила;
   - пройдены `MarketRegimeFilter`, `BadWeatherFilter`, `GroupConfirmationFilter`;
   - расчёт SL по ATR (`entry − ATR*slMult`), TP по R:R (`entry + ATR*tpMult`),
     `quantity` через `VolatilityAdjustedSizing`/`PositionSizer`;
   - вернуть `OPEN` с заполненными `entryPrice/stopLoss/takeProfit/quantity` и
     осмысленным `reason` (например `"PRX_PULLBACK"`).

4. **Логика выхода/удержания (CLOSE/HOLD).** При открытой позиции:
   - обновлять стоп через `StopLossManager` (безубыток на 1R, трейлинг далее);
   - закрывать по SL/TP (high/low свечи), по тайм-ауту удержания (`maxCandlesHold`),
     либо по сигналу разрушения тренда (`close < emaSlow`).

5. **Риск-оверлей.** Перед `OPEN` спросить `KillSwitch.isTradingAllowed()` и
   `RiskManager.canTrade(...)`; применять `AdaptiveCapital.getRiskMultiplier()` к
   размеру. На `onTradeClosed(...)` обновлять `RiskManager.registerTrade(pnl)`,
   `AdaptiveCapital`, `PerformanceTracker`, `KillSwitch.checkDrawdown(...)`. На
   `onDailyReset()` сбрасывать дневные лимиты.

6. **ML-гейт.** По аналогии с `RegimeAwareStrategyMl`: загрузить per-ticker модель,
   собрать `TradeFeatures` на момент входа, получить вероятность; разрешать `OPEN`
   только если `prob ≥ ML_THRESHOLD`. Если модели нет — деградировать gracefully
   (торговать без ML-гейта, логировать). Запись фич сделок включить, чтобы бэктест
   наполнял `trades.csv`.

7. **Параметры в Config.** Вынести магические числа (EMA-периоды, ADX_MIN, RSI-зоны,
   slMult/tpMult по умолчанию, ML_THRESHOLD, лимит просадки KillSwitch, maxCandlesHold)
   в именованные константы/поля `Config` с разумными дефолтами.

---

## 6. Методология валидации на бэктесте

1. Скомпилировать: `./gradlew clean compileJava`.
2. Быстрый прогон для отладки логики: `./gradlew runBacktest -Pstrategy=PrecisionStrategy`
   с `-Dbacktest.mode=fast` (6 месяцев).
3. Полный прогон для приёмки: `backtest.mode=full` (5 годовых периодов).
4. Снять метрики из отчёта `BacktestRunner`: PnL, **max drawdown**, **win rate**,
   Sharpe, Profit Factor, composite score, equity-кривые (JFreeChart).
5. Сверить с воротами раздела 1. Зафиксировать в отчёте по каждому периоду отдельно,
   чтобы убедиться в устойчивости (не переобучение под один период).

**Анти-оверфит:** параметры подбирать на части периодов, проверять на отложенных;
избегать подгонки под единичный «удачный» год. ML-модели тренировать
out-of-sample (time-series CV уже в `MlModelTrainer`).

---

## 7. Итерационный цикл подбора (главный рычаг достижения целей)

```
(a) baseline без ML  → runBacktest fast → метрики
(b) ужесточить confluence-фильтры (ADX_MIN↑, RSI-зоны, peer-confirm) → winrate↑, частота↓
(c) настроить SL/TP/R:R и трейлинг → баланс winrate vs drawdown
(d) включить риск-оверлей (KillSwitch DD-лимит = целевой максимум) → жёстко режем просадку
(e) сгенерировать trades.csv бэктестом → ./gradlew trainAllTickers → модели
(f) включить ML-гейт, подобрать ML_THRESHOLD (ml.probability.threshold) → winrate↑
(g) повторять (b)-(f), пока не выполнены ворота раздела 1 или не зафиксирован
    наилучший достижимый фронт
```

Полный ML-пайплайн одной командой: `./gradlew generateModel`
(backtest → trainAllTickers).

---

## 8. Определение готовности (Definition of Done)

- [ ] `PrecisionStrategy.java` создан, наследует `BaseStrategy`, реализует `decide` и
      `getStrategyName`.
- [ ] Зарегистрирован в `BacktestRunner.StrategyFactory` и `GoldenDragon`.
- [ ] Параметры вынесены в `Config`/properties с дефолтами; магических чисел в логике нет.
- [ ] Используются существующие `money/`, `filters/`, `ml/` компоненты (без новых либ).
- [ ] `./gradlew clean compileJava` — успешно.
- [ ] `./gradlew spotlessCheck` и `./gradlew pmdMain` — без нарушений.
- [ ] `./gradlew check` — тесты проходят.
- [ ] `./gradlew runBacktest -Pstrategy=PrecisionStrategy` (full) — метрики сняты и
      сопоставлены с целевыми; результат (достигнутый фронт) задокументирован.
- [ ] Нет TODO, закомментированного/мёртвого кода, временных заглушек.

---

## 9. Команды (шпаргалка)

```bash
# компиляция
./gradlew clean compileJava

# быстрый бэктест (отладка)
./gradlew runBacktest -Pstrategy=PrecisionStrategy -Dbacktest.mode=fast

# полный бэктест (приёмка)
./gradlew runBacktest -Pstrategy=PrecisionStrategy -Dbacktest.mode=full

# обучение ML по всем тикерам из trades.csv
./gradlew trainAllTickers

# полный ML-пайплайн (backtest + обучение)
./gradlew generateModel

# проверки качества
./gradlew check
./gradlew spotlessCheck
./gradlew pmdMain

# live-запуск (после валидации)
./gradlew runStrategy -Pstrategy=PrecisionStrategy
```

---

## 10. Ограничения и принципы (из `AGENTS.md`)

- Только long (шорты в проекте отключены).
- Минимальные изменения, без расширения скоупа и рефакторинга чужого кода.
- Никаких новых библиотек/подходов без необходимости.
- Java 11; код, комментарии и коммиты — на английском; именование осмысленное.
- После изменений запускать `./gradlew check`, перед коммитом — `clean compileJava`,
  `spotlessCheck`, `pmdMain`.
- Честно фиксировать достигнутые метрики: приоритет «корректность/безопасность →
  стабильность → доходность».
