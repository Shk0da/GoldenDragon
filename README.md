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
| `RegimeAwareStrategy` | Основная стратегия с тремя типами сигналов (trend, fx, mixed), свечными паттернами, голосованием и режимом фильтрации рынка (Regime-Aware Filter) |

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
# Запуск RegimeAwareStrategy (основная)
./gradlew runStrategy -Pstrategy=RegimeAwareStrategy

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
