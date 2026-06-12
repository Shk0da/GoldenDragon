# GoldenDragon

Алгоритмическая торговая система для биржи Тинькофф Инвестиции с поддержкой рынка MOEX. Реализована на Java 11 и использует Tinkoff Invest API (gRPC) для исполнения ордеров, сбора рыночных данных и управления портфелем.

## Возможности

- Многопоточный движок стратегий с пулом на каждый тикер
- 9 торговых стратегий — от простой ребалансировки до ML-усиленных систем
- Money management с адаптивным сайзингом, risk manager и kill switch
- ML-модель логистической регрессии с автодобучением и фильтрацией сигналов
- Полноценный backtest-движок с метриками качества и composite scoring
- Сбор исторических данных с Tinkoff, Yahoo Finance, MOEX ISS
- Сканирование рынка через TradingView Scanner API
- Уведомления в Telegram
- Кеширование свечей и уровней на диске

## Стратегии

| Стратегия | Описание |
|---|---|
| `UnifiedStrategy` | Основная стратегия с тремя типами сигналов (trend, fx, mixed), свечными паттернами и голосованием |
| `RegimeAwareStrategy` | Обёртка над UnifiedStrategy, адаптирует баланс по рыночному режиму (ADX) |
| `RegimeAwareStrategyMl` | Расширенная версия с ML-фильтрацией, ML-сайзингом и автодобучением |
| `IndicatorTrader` | Мультииндикаторная стратегия (MA, MACD, RSI, OBV, ADX) с двухуровневой проверкой (M5 + H1) |
| `LevelTrader` | Торговля от уровней поддержки/сопротивления с анализом стакана |
| `RSX` | Ребалансирование портфеля с фильтрацией по momentum, debt-to-equity и TradingView рекомендациям |
| `Rebalance` | Простая ребалансировка по фиксированному портфелю |
| `DivFlow` | Торговля вокруг дат закрытия дивидендного реестра (SmartLab, Dohod, Investing.com) |
| `DataCollector` | Утилита сбора исторических свечей и расчёта ценовых уровней |

## Архитектура

```
src/main/java/com/github/shk0da/GoldenDragon/
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
├── ml/                       # ML-подсистема
│   ├── MlPredictionService    # логистическая регрессия (predict, shouldTakeTrade)
│   ├── MlModelTrainer        # обучение (SGD + L2, 1500 эпох, time-series CV)
│   ├── MlAutoTrainingService # автоматическое переобучение (каждые 6ч, от 500 сделок)
│   ├── TradeDataCollector    # сбор 26+ признаков при открытии/закрытии
│   └── TradeFeatures         # DTO признаков сделки
├── repository/               # кеширование FIGI и цен
├── service/                  # внешние сервисы
│   ├── TCSService            # Tinkoff Invest API (ордера, стакан, свечи, портфель)
│   ├── TradingViewService    # TradingView Scanner (скрининг)
│   └── TelegramNotifyService # уведомления через Telegram Bot API
├── strategy/                 # торговые стратегии
└── utils/                    # утилиты (индикаторы, уровни, время, HTTP)
```

## Индикаторы и сигналы

**UnifiedStrategy** использует три типа сигналов с голосованием:

- **Trend** — EMA-пересечения, ADX >= 18, RSI 45-68, свечные паттерны (порог: 4/10 баллов)
- **FX (counter-trend)** — RSI перепроданность (<= 25) / перекупленность (>= 75) + разворотные паттерны
- **Mixed** — гибрид трендовых признаков и свечных паттернов

Свечные паттерны: DOJI, PIN_BAR, ENGULFING, MORNING/EVENING_STAR, THREE_WHITE/BLACK.

Режимы рынка определяются по ADX: RANGE (< 15), NORMAL (15-28), TREND (>= 28), HOT_TREND (>= 38).

## Money Management

Система управления капиталом включает:

- **FixedRiskSizing** — фиксированный процент риска на сделку (по умолчанию 1%)
- **VolatilityAdjustedSizing** — адаптивный размер по ATR (baseVolatility / currentATR)
- **StopLossManager** — breakeven при >= 0.5R, трейлинг при >= 1.0R
- **KillSwitch** — остановка при просадке >= 10%
- **AdaptiveCapital** — снижение риска на 50% после 3 лоссов подряд, восстановление после 5 побед
- **RiskManager** — дневной лимит убытков (3%), ограничение серии проигрышей (3)
- **Leverage** — настраиваемое плечо (1x, 2x, 3x, 5x) для каждого тикера

## ML-подсистема

- Логистическая регрессия с 26+ признаками (ADX, DI+/DI-, ATR, RSI, EMA, volume, regime, time)
- Обучение: SGD + L2-регуляризация, 1500 эпох, early stopping (patience=100), class weights
- Оценка: accuracy, precision, recall, AUC, Sharpe ratio, profit factor, max drawdown
- Time-series cross-validation (до 5 фолдов)
- Автоматическое переобучение каждые 6 часов при накоплении 500+ сделок
- Фильтрация сигналов с адаптивным порогом по рыночному режиму

## Сборка и запуск

### Требования

- Java 11+
- Gradle 8.10 (используется wrapper)

### Сборка

```bash
./gradlew clean uberJar
```

Создаётся `GoldenDragon-1.0.jar` в `build/libs/`.

### Конфигурация

Скопируйте `src/main/resources/application.properties` в корень проекта и заполните:

```properties
# Tinkoff Invest API
sandbox=true
accountId=<account_id>
apiKey=<api_key>

# Telegram Bot (опционально)
telegram.bot.token=<bot_token>
telegram.chat.id=<chat_id>

# Leverage (опционально, по умолчанию 1)
unifiedTrader.ticker.BTCUSDT.leverage=3
unifiedTrader.ticker.ETHUSDT.leverage=2
```

### Запуск стратегий

```bash
# сборка JAR
./gradlew clean uberJar

# запуск через JAR
java -Dapplication.properties=./application.properties -jar build/libs/GoldenDragon-1.0.jar <strategy> [market] [account]
```

Доступные стратегии: `UnifiedStrategy`, `RegimeAwareStrategyMl`, `IndicatorTrader`, `LevelTrader`, `RSX`, `DivFlow`, `Rebalance`, `DataCollector`.

### Backtest

```bash
# через Gradle
./gradlew clean runBacktest
./gradlew clean runBacktest -Pstrategy=UnifiedStrategy

# через JAR
java -Dapplication.properties=./application.properties -jar build/libs/GoldenDragon-1.0.jar GenerateModel
```

Поддерживаются: `UnifiedStrategy`, `RegimeAwareStrategy`, `RegimeAwareStrategyMl`.

### ML-обучение

```bash
# Gradle: backtest + обучение (генерация модели)
./gradlew clean generateModel

# Gradle: только обучение
./gradlew runMlTraining --data=ml_strategy/data_pipeline/trades.csv --output=models/trade_classifier_v2.txt

# JAR: backtest + обучение
java -Dapplication.properties=./application.properties -jar build/libs/GoldenDragon-1.0.jar GenerateModel

# JAR: только обучение (без backtest)
java -Dapplication.properties=./application.properties -jar build/libs/GoldenDragon-1.0.jar GenerateModel --skip-backtest

# JAR: с параметрами
java -Dapplication.properties=./application.properties -jar build/libs/GoldenDragon-1.0.jar GenerateModel --data=ml_strategy/data_pipeline/trades.csv --output=models/trade_classifier_v2.txt --report-dir=ml_strategy
```

## Зависимости

| Библиотека | Назначение |
|---|---|
| `ru.tinkoff.piapi:java-sdk-core:1.5` | Tinkoff Invest API (gRPC) |
| `com.google.code.gson:gson:2.8.9` | JSON-сериализация |
| `org.jsoup:jsoup:1.15.3` | HTML-парсинг (дивиденды, TradingView) |
| `com.tictactec:ta-lib:0.4.0` | Технические индикаторы (EMA, RSI, ADX, ATR, MACD) |
| `org.jfree:jfreechart:1.0.19` | Построение графиков |

## Структура данных

```
data/              # кеш свечей и уровней (CSV + JSON)
logs/              # логи работы стратегий
models/            # ML-модели
ml_strategy/       # ML-конвейер
├── backtest/      # результаты бэктестов
├── data_pipeline/ # данные сделок для обучения (trades.csv)
├── features/      # описание признаков
└── strategy/      # артефакты обучения
tickers.json       # кеш тикеров с FIGI, ISIN, параметрами
application.properties # конфигурация (не коммитится)
```

## Рынки и инструменты

- **MOEX** — акции, облигации, ETF, валюты, фьючерсы

Рабочие часы: пн-пт, расписание определяется конфигурацией рынка (MOEX 10:00-18:00 MSK).
