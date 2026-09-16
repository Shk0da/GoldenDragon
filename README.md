# GoldenDragon

Алгоритмическая торговая система для **Тинькофф Инвестиции** (MOEX). Реализована на Java 11 и использует gRPC API.

## Возможности

- Многопоточный движок стратегий с пулом на каждый тикер
- Money management с адаптивным сайзингом, risk manager и kill switch
- Cash parking: TMON@ (Tinkoff ETF)
- Сбор исторических данных с Tinkoff
- Данные на диске (`data/`) только для бэктестов
- Бэктестинг с поддержкой Tinkoff

## Стратегии

| Стратегия | Описание |
|---|---|
| `UnifiedStrategy` | Основная стратегия с настраиваемым режим-фильтром (бывшие RegimeAwareStrategy + UnifiedStrategy) |

### Live-only стратегии (не участвуют в бэктесте)

| Стратегия | Описание |
|---|---|
| `TradeCouncilStrategy` | AI-стратегия с LLM-дебатами: 3 агента (Analyst, Trader, Risk Manager) обсуждают сделку, арбитр принимает финальное решение. Уровни поддержки/сопротивления на основе значимых разворотов цены (2+ касания). Работает только в реальном времени, не тестируется через бэктест |

## Архитектура

```
src/main/java/com/github/shk0da/goldendragon/
├── GoldenDragon.java          # точка входа, диспетчер стратегий
├── config/                   # конфигурация (MainConfig, UnifiedTraderConfig, ...)
├── model/                    # DTO (Candle, Position, TickerInfo, Config, ...)
├── money/                    # управление капиталом
│   ├── CashParkingManager    # парковка кеша в TMON@ (Tinkoff ETF)
│   ├── SizingStrategy        # интерфейс: FixedRiskSizing, VolatilityAdjustedSizing
│   ├── PositionSizer         # расчёт размера позиции с учётом лота и шага
│   ├── RiskManager           # дневные лимиты, серия проигрышей
│   ├── StopLossManager       # breakeven и трейлинг стопов
│   ├── KillSwitch            # аварийная остановка при критической просадке
│   ├── AdaptiveCapital       # anti-martingale адаптация риска
│   └── PerformanceTracker    # win rate, PnL, drawdown
├── filters/                  # фильтры входа
│   ├── BadWeatherFilter      # низкая активность, хаос, турбулентность
│   ├── GroupConfirmationFilter  # подтверждение по peer-инструментам группы
│   └── MarketRegimeFilter    # фильтрация по режиму рынка (ADX, volume, confidence)
├── market/                   # рыночные данные и исполнение ордеров
│   ├── MarketDataProvider    # интерфейс: получение свечей и цен
│   ├── LiveMarketDataProvider # live-данные от брокера
│   ├── OrderExecutor         # интерфейс: исполнение ордеров
│   └── LiveOrderExecutor     # live-исполнение через брокера
├── repository/               # кеширование FIGI и цен
├── service/                  # внешние сервисы
│   ├── TradingService        # общий интерфейс торговли (getAvailableCash, createOrder, ...)
│   └── TCSService            # Tinkoff Invest API (ордера, стакан, свечи, портфель)
└── strategy/                 # торговые стратегии
    ├── BaseStrategy          # базовый класс (жизненный цикл, индикаторы)
    ├── UnifiedStrategy       # основная стратегия с режимом фильтрации рынка
    ├── TradeCouncilStrategy  # AI-стратегия с LLM-дебатами (live-only, не бэктестируется)
    └── DataCollector         # сбор исторических данных (5_MIN, HOUR) с Tinkoff
```

## Конфигурация

Основные параметры в `src/main/resources/application.properties`:

```properties
# ============================================
# TCS Client Config
# ============================================
tcs.testMode=false
tcs.isSandbox=true
tcs.accountId=
tcs.apiKey=

# ============================================
# DataCollector Config
# ============================================
datacollector.dataDir=data
datacollector.instruments=GMKN,T,VTBR,SNGS,GLDRUBF,IMOEXF,MGNT,PLZL,YDEX,MTSS,GAZPF,SNGSP,SIBN,TATN,OZON,X5,AKRN,NLMK,RUAL,ALRS,LENT,RTKM,HYDR,VKCO,FESH,UPRO,UWGN,TMON@
datacollector.historyDays=1900

# ============================================
# UnifiedTrader Config
# ============================================
unifiedTrader.leverage=3
unifiedTrader.adaptiveLeverage.enabled=true
unifiedTrader.tmonCashParking.enabled=true

# ============================================
# UnifiedStrategy Config
# ============================================
unifiedTrader.ticker.T.marketRegimeAdxRangeThreshold=25.0
unifiedTrader.ticker.T.marketRegimeConfidenceMin=60.0

# ============================================
# TradeCouncilStrategy Config (AI/LLM)
# ============================================
tradecouncil.openai.baseUrl=http://localhost:4000/v1
tradecouncil.openai.apiKey=your-api-key
tradecouncil.debater.model=shcoder
tradecouncil.arbiter.model=shcoder
tradecouncil.prompt.consensus=You are a consensus judge. Compare all {N} debater outputs...
tradecouncil.debate.rounds=3
tradecouncil.proximity.percent=2.0
tradecouncil.risk.percent=1.0
```

## Быстрый старт

### Требования

- Java 11+
- Gradle
- API-ключ Тинькофф Инвестиции (песочница или боевой)

### Установка

```bash
git clone https://github.com/shk0da/GoldenDragon.git
cd GoldenDragon
./gradlew build
```

### Запуск стратегии

```bash
# Запуск UnifiedStrategy (основная)
./gradlew runStrategy -Pstrategy=UnifiedStrategy

# Запуск TradeCouncilStrategy (AI с LLM-дебатами)
./gradlew runStrategyAI
```

### Запуск бэктеста

```bash
./gradlew runBacktest
```

### Сбор данных

```bash
# Сбор данных для всех инструментов
./gradlew dataCollect
```

## Бэктестинг

BacktestRunner использует инструменты из `datacollector.instruments` (акции, ETF, фьючерсы MOEX).

Cash parking в бэктесте:
- **TMON@**: комиссия 0%, не учитывается в tradeHistory

### Инструменты анализа бэктестов

Проект включает набор инструментов для всесторонней валидации торговых стратегий:

#### 1. BacktestRunner — движок исполнения бэктестов

**Назначение:** Запуск бэктестов стратегий на исторических данных с учётом комиссий, проскальзываний и реалистичного исполнения ордеров.

**Ключевые возможности:**
- Поддержка всех стратегий, реализующих интерфейс `BaseStrategy`
- Реалистичное моделирование исполнения ордеров (комиссии, проскальзывание)
- Cash parking в TMON@ (комиссия 0%)
- Расчёт портфельной доходности и метрик по каждому тикеру
- Генерация equity curve для визуализации

**Пример использования:**
```java
BacktestRunner runner = new BacktestRunner("data", 100_000, 0.0005, 0.0);
BacktestExecutionResult result = runner.execute(
    "UnifiedStrategy",
    "2022-01-01",
    "2026-12-31",
    tickers,
    config
);
PortfolioPeriodResult portfolio = result.portfolioResult;
System.out.println("PnL: " + portfolio.pnl);
System.out.println("Trades: " + portfolio.totalTrades);
```

**Возвращаемые метрики:**
- `pnl` — общая доходность за период (в валюте)
- `dd` — максимальная просадка (в валюте)
- `winRate` — процент прибыльных сделок
- `totalTrades` — количество сделок
- `equityCurve` — кривая капитала для построения графиков

---

#### 2. WalkForwardAnalyzer — валидация на скользящих окнах

**Назначение:** Проверка устойчивости стратегии на непересекающихся периодах (in-sample / out-of-sample).

**Принцип работы:**
1. Разбивает исторический период на скользящие окна
2. Для каждого окна: обучение (train) → тестирование (test)
3. Сравнивает метрики IS/OOS для выявления переобучения

**Конфигурация:**
- `trainMonths` — длительность окна обучения (3–6 месяцев)
- `testMonths` — длительность тестового окна (1–2 месяца)
- `stepMonths` — шаг сдвига (1 месяц)

**Критерии устойчивости:**
- PnL Ratio (Test/Train): 0.7–1.3 — стратегия устойчива
- Win Rate Ratio: близкий к 1.0 — стабильность качества сигналов
- DD Ratio: тестовая просадка не превышает обучающую значительно

**Пример использования:**
```java
WalkForwardAnalyzer analyzer = new WalkForwardAnalyzer(
    "UnifiedStrategy",
    "GMKN",
    config,
    6,  // train months
    2,  // test months
    1   // step months
);
WalkForwardResult result = analyzer.analyze("2022-01-01", "2026-12-31");
SummaryMetrics summary = result.summary;
System.out.println("Robust: " + summary.isRobust); // true если PnL ratio в норме
```

**Интерпретация результатов:**
- `isRobust = true` — стратегия показывает сопоставимые результаты на IS/OOS
- `pnlRatio < 0.7` — переобучение (резко хуже на unseen данных)
- `pnlRatio > 1.3` — недообучение или случайная удача на тесте

---

#### 3. SensitivityAnalyzer — анализ чувствительности параметров

**Назначение:** Поиск оптимальных параметров и оценка устойчивости к их изменению.

**Типы анализа:**
- **ADX Sensitivity** — пороги ADX для фильтрации режимов рынка
- **SL/TP Sensitivity** — мультипликаторы стоп-лосса и тейк-профита

**Ключевые метрики:**
- `optimalSet` — лучшая комбинация параметров по Sharpe ratio
- `parameterSensitivity` — диапазон изменения PnL при варьировании параметра
- **Низкая чувствительность** — широкий «плато» устойчивости (хорошо)
- **Высокая чувствительность** — узкий «пик» (риск переобучения)

**Пример использования:**
```java
SensitivityAnalyzer analyzer = new SensitivityAnalyzer(
    "UnifiedStrategy",
    "GMKN",
    config,
    "2022-01-01",
    "2026-12-31"
);
SensitivityResult adxResult = analyzer.analyzeAdxSensitivity();
ParameterSet optimal = adxResult.optimalSet;
System.out.println("Optimal ADX: " + optimal.parameters);
```

**Рекомендации по интерпретации:**
- Ищите широкие плато (параметры работают в диапазоне ±20–30%)
- Избегайте узких пиков (работает только при точных значениях)
- Сравнивайте оптимальные параметры с эвристическими ожиданиями

---

#### 4. MetricsCalculator — расчёт метрик качества

**Назначение:** Расчёт стандартных метрик риск-скорректированной доходности.

**Поддерживаемые метрики:**

| Метрика | Формула | Интерпретация |
|---|---|---|
| **Sharpe Ratio** | `(avgReturn - riskFree) / stdDev` | Доходность на единицу общей волатильности |
| **Sortino Ratio** | `(avgReturn - riskFree) / downsideDev` | Доходность на единицу downside-риска |
| **Calmar Ratio** | `CAGR / MaxDrawdown` | Доходность на единицу максимальной просадки |
| **Recovery Factor** | `GrossProfit / MaxDrawdown` | Способность восстанавливаться после просадок |
| **R-Multiple** | `PnL / InitialRisk` | Нормализованная доходность на единицу риска |

**Пример использования:**
```java
List<Double> monthlyReturns = Arrays.asList(0.05, -0.02, 0.08, ...);
double sharpe = MetricsCalculator.calculateSharpeRatio(monthlyReturns, 0.05, 12);
double sortino = MetricsCalculator.calculateSortinoRatio(monthlyReturns, 0.05, 12);
double calmar = MetricsCalculator.calculateCalmarRatio(equityCurve, 0.05);
```

**Пороговые значения:**
- Sharpe > 1.0 — приемлемо, > 2.0 — отлично
- Sortino > 1.5 — хорошо (учитывает только downside-риск)
- Calmar > 3.0 — сильная стратегия

---

#### 5. BacktestExpertEvaluator — комплексная оценка качества

**Назначение:** Оценка бэктеста по 5 измерениям с выдачей вердикта (DEPLOY/REFINE/ABANDON).

**5 измерений (0–20 баллов каждое):**

1. **Sample Size (0–20)** — статистическая значимость
   - ≥100 сделок: 20 баллов
   - 50–99 сделок: 15 баллов
   - <30 сделок: 5 баллов (недостаточно данных)

2. **Expectancy (0–20)** — математическое ожидание
   - Средняя прибыль на сделку vs средний убыток
   - Процент прибыльных сделок (win rate)

3. **Risk Management (0–20)** — управление рисками
   - Максимальная просадка (чем меньше, тем лучше)
   - Наличие stop-loss, trailing stop
   - Позиционирование (risk per trade)

4. **Robustness (0–20)** — устойчивость
   - Результаты walk-forward анализа (IS/OOS consistency)
   - Чувствительность параметров (широкие плато vs узкие пики)

5. **Execution Realism (0–20)** — реалистичность исполнения
   - Учёт комиссий, проскальзывания, спреда
   - Реалистичное моделирование ликвидности

**Вердикты:**
- **DEPLOY (≥80)** — стратегия готова к реальной торговле
- **REFINE (50–79)** — перспективная, но требует доработки
- **ABANDON (<50)** — фундаментальные проблемы, лучше переработать

**Пример использования:**
```java
BacktestExpertEvaluator evaluator = new BacktestExpertEvaluator();
EvaluationResult eval = evaluator.evaluate(result, config);
System.out.println("Total Score: " + eval.totalScore + "/100");
System.out.println("Verdict: " + eval.verdict);
```

---

#### 6. LiveParityVerifier — проверка соответствия live-торговле

**Назначение:** Валидация того, что бэктест-моделирование воспроизводит поведение реальной торговли.

**Проверяемые аспекты:**
- Цены исполнения ордеров (в пределах tolerance)
- Расчёт размера позиции (идентичен live)
- Изменения баланса (с учётом комиссий)
- Обработка ошибок (совпадает поведение)

**Критерии прохождения:**
- Pass Rate ≥ 95% — бэктест достоверен
- Pass Rate < 95% — требуется калибровка модели

**Пример использования:**
```java
LiveParityVerifier verifier = new LiveParityVerifier("GMKN", 0.01); // 1% tolerance
ParityResult parity = verifier.verify(backtestResult, liveResult);
System.out.println("Pass Rate: " + parity.passRate * 100 + "%");
System.out.println("Valid: " + parity.passed);
```

---

#### 7. SimulatedBroker — симулятор брокера

**Назначение:** Реалистичное моделирование исполнения ордеров в бэктесте.

**Возможности:**
- Покупка/продажа по количеству (`buyByQuantity`, `sellByQuantity`)
- Расчёт полной стоимости сделки (цена × количество × комиссия)
- Учёт проскальзывания (slippage)
- Отслеживание открытых позиций
- Проверка доступности средств

**Отличия от live-брокера:**
- Использует исторические цены из файлов свечей
- Не требует подключения к API брокера
- Мгновенное исполнение (без задержек сети)

---

#### 8. BacktestTradingService — mock торгового сервиса

**Назначение:** Замена реального `TradingService` для бэктестов.

**Реализуемые методы:**
- `getAvailableCash()` — доступные средства (с учётом позиции)
- `buy()` / `sell()` — исполнение ордеров через `SimulatedBroker`
- `getPosition()` — текущая позиция по тикеру
- `closePosition()` — закрытие позиции

**Интеграция:**
Используется внутри `BacktestRunner` для изоляции бэктеста от внешней системы.

---

### Запуск бэктеста

```bash
# Запуск стандартного бэктеста (2022–2026)
./gradlew runBacktest

# Запуск с кастомными параметрами (через Java-системные свойства)
./gradlew runBacktest -Dbacktest.start=2023-01-01 -Dbacktest.end=2025-12-31
```

**Результаты бэктеста:**
- Вывод в консоль: общая доходность, количество сделок, win rate, максимальная просадка
- Equity curve для построения графиков (доступна через `result.portfolioResult.equityCurve`)
- Детализация по каждому тикеру (через `result.tickerResults`)

---

### Рекомендации по валидации стратегий

1. **Запустите базовый бэктест** на полном периоде (2022–2026)
2. **Проведите walk-forward анализ** для проверки на переобучение
3. **Выполните sensitivity analysis** ключевых параметров (ADX, SL/TP)
4. **Рассчитайте метрики** (Sharpe, Sortino, Calmar) для сравнения с бенчмарками
5. **Оцените качество** через `BacktestExpertEvaluator` (цель: ≥80 баллов, вердикт DEPLOY)
6. **Сверьте с live-торговлей** через `LiveParityVerifier` (если есть live-данные)

Только после прохождения всех этапов валидации стратегия считается готовой к развёртыванию.

## TradeCouncilStrategy (AI/LLM)

> **Live-only**: стратегия работает только в реальном времени и не участвует в бэктесте (зависит от LLM API).

AI-стратегия, использующая дебаты между LLM-агентами для принятия торговых решений.

### Архитектура

```
┌─────────────┐
│   Price     │
│ approaches  │
│   level     │
└──────┬──────┘
       │
       v
┌─────────────────────────────────────────┐
│         DEBATE (3 rounds)               │
│  ┌──────────┬──────────┬──────────────┐ │
│  │ Analyst  │  Trader  │ Risk Manager │ │
│  │          │          │              │ │
│  │ trend    │  entry   │   R:R check  │ │
│  │ RSI      │  stop    │   sizing     │ │
│  │ volume   │  target  │   NO_TRADE   │ │
│  └──────────┴──────────┴──────────────┘ │
└─────────────────────────────────────────┘
       │
       v (consensus check after round 2+)
┌──────────────┐
│   Arbiter    │
│ final decision│
└──────┬───────┘
       │
       v
┌──────────────┐
│  Execute     │
│  LONG/SHORT  │
└──────────────┘
```

### Ключевые особенности

- **Уровни**: значимые уровни поддержки/сопротивления (цена разворачивалась 2+ раза)
- **Агенты**: Analyst (тренд, RSI, объём), Trader (вход, стоп, цель), Risk Manager (R:R, размер)
- **Консенсус**: проверка согласия после 2-го раунда (температура 0.0)
- **Арбитр**: финальное решение с температурой 0.2
- **Rate limiter**: 262000 токенов/минуту (token bucket)
- **Семафор**: последовательные вызовы к LLM (1 запрос за раз)
- **Таймаут**: 5 минут на один LLM-вызов

### Логирование

```
=== CONSENSIUM START === TATN: Price 595.4 approached level S1 (590.1)
Debate in progress for TATN (rounds=3)
TATN | Round 1/3
TATN | R1 Analyst: done
TATN | R1 Trader: done
TATN | R1 Risk Manager: done
TATN | R2 Consensus check: CONTINUE
=== CONSENSIUM RESULT === TATN: Action=LONG, Reason=Strong uptrend confirmed
```

### Конфигурация

См. секцию `# TradeCouncilStrategy Config (AI/LLM)` в `application.properties`.

## Структура проекта

```
GoldenDragon/
├── src/
│   ├── main/
│   │   ├── java/
│   │   └── resources/
│   └── test/
├── data/                 # исторические свечи
├── images/               # графики equity
├── scripts/              # скрипты дополнительные
├── build.gradle
└── README.md
```

## Зависимости

| Библиотека | Назначение |
|---|---|
| `ru.tinkoff.piapi:java-sdk-core` | Tinkoff Invest API gRPC клиент |
| `com.fasterxml.jackson` | JSON сериализация |
| `org.jfree:jfreechart` | построение графиков equity |

## Лицензия

MIT
