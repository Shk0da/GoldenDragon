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
./gradlew runStrategy -Pstrategy=RegimeAwareStrategy
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
