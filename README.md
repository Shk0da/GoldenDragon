# GoldenDragon

Алгоритмическая торговая система для биржи Тинькофф Инвестиции с поддержкой рынка MOEX. Реализована на Java 11 и использует Tinkoff Invest API (gRPC) для исполнения ордеров, сбора рыночных данных и управления портфелем.

## Возможности

- Многопоточный движок стратегий с пулом на каждый тикер
- Money management с адаптивным сайзингом, risk manager и kill switch
- Полноценный backtest-движок с метриками качества
- Сбор исторических данных с Tinkoff, Yahoo Finance, MOEX ISS
- Кеширование свечей и уровней на диске

## Стратегии

| Стратегия | Описание |
|---|---|
| `UnifiedStrategy` | Основная стратегия с тремя типами сигналов (trend, fx, mixed), свечными паттернами, голосованием и режимом фильтрации рынка (Regime-Aware Filter) |
| `Rebalance` | Простая ребалансировка по фиксированному портфелю |
| `OrderBookScalpStrategy` | Скальпинг на основе анализа стакана и дельты |

## Архитектура

```
src/main/java/com/github/shk0da/goldendragon/
├── GoldenDragon.java          # точка входа, диспетчер стратегий
├── config/                   # конфигурация (MainConfig, UnifiedTraderConfig, ...)
├── model/                    # DTO (Candle, Position, TickerInfo, Config, ...)
├── money/                    # управление капиталом
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
│   ├── BacktestMarketDataProvider # исторические данные из CSV
│   ├── OrderExecutor         # интерфейс: исполнение ордеров
│   └── LiveOrderExecutor     # live-исполнение через брокера
├── repository/               # кеширование FIGI и цен
├── service/                  # внешние сервисы
│   └── TCSService            # Tinkoff Invest API (ордера, стакан, свечи, портфель)
├── strategy/                 # торговые стратегии
│   ├── BaseStrategy          # базовый класс (жизненный цикл, индикаторы, кэш)
│   ├── UnifiedStrategy       # основная стратегия с режимом фильтрации рынка
│   └── OrderBookScalpStrategy # скальпинг по стакану
└── test/                     # backtest и утилиты
    ├── BacktestRunner        # движок backtest
    └── VirtualTCSService     # виртуальный брокер для backtest
```

## Быстрый старт

### Требования

- Java 11+
- Maven или Gradle
- API-ключ Тинькофф Инвестиции (песочница или боевой)

### Установка

```bash
git clone https://github.com/shk0da/GoldenDragon.git
cd GoldenDragon
./gradlew build
```

### Запуск стратегии

```bash
./gradlew runStrategy -Pstrategy=UnifiedStrategy
```

Доступные стратегии: `UnifiedStrategy`, `Rebalance`, `OrderBookScalpStrategy`.

### Backtest

```bash
./gradlew runBacktest
```

Поддерживается: `UnifiedStrategy`.

## Конфигурация

Основные параметры в `src/main/resources/application.properties`:

```properties
# TCS Client Config
tcs.testMode=false
tcs.isSandbox=true
tcs.accountId=
tcs.apiKey=

# Market Config
market.moex.maxPositionCostToBuy=10000
market.moex.currency=RUB

# Rebalance Config
rebalance.position.percent=5.0
rebalance.portfolio.ratio=AIV:10;BXP:10;EXR:10;IRM:10;KIM:10;PLD:10;O:10;SLG:10;UDR:10;VTR:10;

# UnifiedTrader Config
unifiedTrader.averagePositionCost=500000
unifiedTrader.leverage=3
unifiedTrader.adaptiveLeverage.enabled=true
```

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
├── docs/                 # документация
├── build.gradle
└── README.md
```

## Зависимости

| Библиотека | Назначение |
|---|---|
| `ru.tinkoff.piapi:tinkoff-invest-api` | Tinkoff Invest API gRPC клиент |
| `com.fasterxml.jackson` | JSON сериализация |
| `org.jfree:jfreechart` | построение графиков equity |

## Лицензия

MIT
