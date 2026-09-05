# GoldenDragon

Алгоритмическая торговая система с поддержкой нескольких бирж: **Тинькофф Инвестиции** (MOEX) и **ByBit** (криптовалюты). Реализована на Java 11 и использует gRPC для Tinkoff и REST API для ByBit.

## Возможности

- Многопоточный движок стратегий с пулом на каждый тикер
- Поддержка двух торговых сервисов: Tinkoff (TCS) и ByBit через единый интерфейс `TradingService`
- Crypto support: торговля USDT-перпетуалами на ByBit
- Money management с адаптивным сайзингом, risk manager и kill switch
- Cash parking: TMON@ (Tinkoff ETF); для ByBit парковка отключена
- Сбор исторических данных с Tinkoff и ByBit
- Данные на диске (`data/`) только для бэктестов
- Бэктестинг с поддержкой обоих брокеров

## Стратегии

| Стратегия | Описание |
|---|---|
| `RegimeAwareStrategy` | Основная стратегия с тремя типами сигналов (trend, fx, mixed), свечными паттернами, голосованием и режимом фильтрации рынка (Regime-Aware Filter) |
| `OrderBookScalpStrategy` | Скальпинг на основе анализа стакана и дельты (Tinkoff и ByBit) |

## Архитектура

```
src/main/java/com/github/shk0da/goldendragon/
├── GoldenDragon.java          # точка входа, диспетчер стратегий
├── config/                   # конфигурация (MainConfig, UnifiedTraderConfig, ByBitConfig, ...)
├── model/                    # DTO (Candle, Position, TickerInfo, Config, ...)
├── money/                    # управление капиталом
│   ├── CashParkingManager    # абстракция парковки: TMON@ (Tinkoff); для ByBit отключена
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
│   ├── TradingServiceType    # enum: TINKOFF, BYBIT
│   ├── TradingServiceFactory # фабрика создания сервиса по конфигу
│   ├── TCSService            # Tinkoff Invest API (ордера, стакан, свечи, портфель)
│   └── ByBitService          # ByBit API (список фьючерсов, свечи, ордера, позиции, цены)
└── strategy/                 # торговые стратегии
    ├── BaseStrategy          # базовый класс (жизненный цикл, индикаторы)
    ├── UnifiedStrategy       # основная стратегия с режимом фильтрации рынка
    ├── DataCollector         # сбор данных с Tinkoff и ByBit
    └── OrderBookScalpStrategy # скальпинг по стакану (Tinkoff и ByBit)
```

## Конфигурация

Основные параметры в `src/main/resources/application.properties`:

```properties
# ============================================
# Trading Service Selector: TINKOFF | BYBIT
# ============================================
trading.service=TINKOFF

# ============================================
# TCS Client Config (только для TINKOFF)
# ============================================
tcs.testMode=false
tcs.isSandbox=true
tcs.accountId=
tcs.apiKey=

# ============================================
# ByBit Client Config (только для BYBIT)
# ============================================
bybit.testMode=false
bybit.apiKey=
bybit.apiSecret=

# ============================================
# DataCollector Config
# ============================================
datacollector.dataDir=data
datacollector.instruments=GMKN,T,VTBR,SNGS,GLDRUBF,IMOEXF,MGNT,PLZL,YDEX,MTSS,GAZPF,SNGSP,SIBN,TATN,OZON,X5,AKRN,NLMK,RUAL,ALRS,LENT,RTKM,HYDR,VKCO,FESH,UPRO,UWGN,TMON@
datacollector.crypto=ADAUSDT,APTUSDT,ARBUSDT,ATOMUSDT,AVAXUSDT,BNBUSDT,BTCUSDT,DOGEUSDT,DOTUSDT,ETCUSDT,ETHUSDT,FILUSDT,LINKUSDT,LTCUSDT,POLUSDT,NEARUSDT,SOLUSDT,UNIUSDT,XLMUSDT,XRPUSDT
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
- Или API-ключ ByBit (для крипто-торговли)

### Установка

```bash
git clone https://github.com/shk0da/GoldenDragon.git
cd GoldenDragon
./gradlew build
```

### Запуск стратегии

```bash
# Tinkoff (по умолчанию)
./gradlew runStrategy -Pstrategy=RegimeAwareStrategy

# ByBit
./gradlew runStrategy -Pstrategy=RegimeAwareStrategy -Dtrading.service=BYBIT

# OrderBook-скальпинг
./gradlew runOrderBookStrategy
```

### Запуск бэктеста

```bash
# Тинькофф (по умолчанию)
./gradlew runBacktest

# ByBit — автоматически фильтрует только crypto-тикеры
./gradlew runBacktest -Dtrading.service=BYBIT
```

### Сбор данных

```bash
# Сбор данных для всех инструментов
./gradlew dataCollect
```

## Бэктестинг

BacktestRunner автоматически адаптируется к выбранному trading service:
- **TINKOFF**: использует только `datacollector.instruments` (акции, ETF, фьючерсы MOEX)
- **BYBIT**: использует только `datacollector.crypto` (USDT-перпетуалы)

Cash parking в бэктесте:
- **TMON@** (Tinkoff): комиссия 0%, не учитывается в tradeHistory
- **ByBit**: парковка отключена

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
| `io.github.wuhewuhe:bybit-java-api` | ByBit REST API клиент |
| `com.fasterxml.jackson` | JSON сериализация |
| `org.jfree:jfreechart` | построение графиков equity |

## Лицензия

MIT
