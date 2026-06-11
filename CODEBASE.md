# Codebase Structure

## Package Overview

```
src/main/java/com/github/shk0da/GoldenDragon/
├── GoldenDragon.java              # Main application entry point
├── config/                        # Configuration classes
├── strategy/                      # Trading strategies
├── service/                       # External service integrations
├── model/                         # Data transfer objects
├── money/                         # Money management
├── ml/                            # Machine learning
├── repository/                    # Data repositories
├── filters/                       # Signal filters
├── utils/                         # Utility classes
├── dictionary/                    # Reference data
└── test/                          # Backtesting and testing
```

## Package Details

### `config/` — Configuration Classes
Configuration loaded from `application.properties`.

| Class | Purpose |
|-------|---------|
| `MainConfig` | API keys, sandbox mode, test mode, HTTP client |
| `MarketConfig` | Market settings (MOEX, currency, limits) |
| `LevelTraderConfig` | LevelTrader parameters (SL/TP, levels) |
| `UnifiedTraderConfig` | UnifiedStrategy parameters (tickers, MM) |
| `RSXConfig` | RSX strategy settings (trend ticker, size) |
| `DivFlowConfig` | Dividend calendar URLs (constants) |
| `TelegramNotifyConfig` | Telegram bot token, chat_id |
| `DataCollectorConfig` | Data collection settings |
| `RebalanceConfig` | Portfolio rebalancing targets |

### `strategy/` — Trading Strategies
All strategies extend `BaseStrategy`.

| Class | Purpose |
|-------|---------|
| `BaseStrategy` | Abstract engine: lifecycle, data loading, filters |
| `UnifiedStrategy` | Medium-term trading with MM integration |
| `LevelTrader` | Support/resistance level trading |
| `RSX` | RSI Smoothed strategy |
| `DivFlow` | Dividend-based trading |
| `Rebalance` | Portfolio rebalancing |
| `RegimeAwareStrategy` | Market regime adaptive strategy |
| `RegimeAwareStrategyMl` | ML-powered regime strategy |
| `IndicatorTrader` | Technical indicator strategy |
| `DataCollector` | Real-time data collection |
| `ModelGenerator` | ML model generation |

### `service/` — External Integrations
External API wrappers.

| Class | Purpose |
|-------|---------|
| `TCSService` | Tinkoff Invest API (market data, orders) |
| `TelegramNotifyService` | Telegram notifications (singleton) |
| `TradingViewService` | Market scanner (singleton) |

### `model/` — Data Classes
DTOs and domain models.

| Class | Purpose |
|-------|---------|
| `Candle` | OHLCV candlestick |
| `Position` | Trading position (direction, quantity, entry) |
| `PositionInfo` | Position info from API |
| `PortfolioPosition` | Target portfolio allocation |
| `TradingDecision` | Strategy decision (BUY/SELL/HOLD/CLOSE) |
| `TickerInfo` | Instrument metadata (FIGI, ISIN, currency) |
| `TickerType` | Instrument type enum (STOCK, BOND, ETF, etc.) |
| `Market` | Market enum (MOEX) |
| `Group` | Instrument group enum (TREND, FX, MIXED) |
| `MarketDepthSnapshot` | Order book snapshot |
| `MarketDepthLevel` | Order book level (price/quantity) |
| `MarketTradeTick` | Last trade tick |
| `MarketTickListener` | Interface for real-time data |
| `DiviTicker` | Dividend info (date, amount, yield) |
| `TickerScan` | TradingView scan result |
| `TickerJson` | LevelTrader serializable data |
| `Config` | ML strategy configuration |

### `money/` — Money Management
Risk control and position sizing.

| Class | Purpose |
|-------|---------|
| `RiskManager` | Daily limits, consecutive losses |
| `KillSwitch` | Emergency trading halt |
| `PositionSizer` | Position size calculation |
| `StopLossManager` | Trailing stop management |
| `AdaptiveCapital` | Anti-martingale sizing |
| `PerformanceTracker` | Trade statistics |
| `SizingStrategy` | Interface for sizing algorithms |
| `FixedRiskSizing` | Fixed risk per trade |
| `VolatilityAdjustedSizing` | ATR-based sizing |

### `ml/` — Machine Learning
XGBoost integration.

| Class | Purpose |
|-------|---------|
| `TradeDataCollector` | Collect labeled trade data |
| `TradeFeatures` | ML features (indicators, regime) |
| `MlModelTrainer` | Train XGBoost models |
| `MlPredictionService` | Predict trade probability |
| `MlAutoTrainingService` | Automatic model retraining |

### `repository/` — Data Repositories
In-memory storage with `ConcurrentHashMap`.

| Class | Purpose |
|-------|---------|
| `Repository` | Generic key-value interface |
| `AbstractRepository` | Base implementation |
| `TickerRepository` | Ticker metadata (singleton, loads from JSON) |
| `FigiRepository` | Ticker-to-FIGI mapping (singleton) |
| `PricesRepository` | Order book cache (singleton) |

### `filters/` — Signal Filters
Stateless, thread-safe filters.

| Class | Purpose |
|-------|---------|
| `MarketRegimeFilter` | Trend/range detection (ADX, ATR, volume) |
| `GroupConfirmationFilter` | Peer instrument validation |
| `BadWeatherFilter` | Market condition checks |

### `utils/` — Utility Classes
Helper functions.

| Class | Purpose |
|-------|---------|
| `PropertiesUtils` | Load properties files |
| `SerializationUtils` | JSON serialization (Gson) |
| `TimeUtils` | Sleep with interrupt handling |
| `PredicateUtils` | Stream predicates |
| `IndicatorsUtil` | Technical indicators (TA-Lib) |
| `RequestUtils` | HTTP requests with retries |
| `LevelUtils` | Support/resistance levels |
| `GerchikUtils` | Gerchik system methods |
| `TickerTypeResolver` | Ticker type detection |
| `PrintUtils` | Console output formatting |

### `dictionary/` — Reference Data
Static dictionaries.

| Class | Purpose |
|-------|---------|
| `CurrenciesDictionary` | Currency name conversion |

### `test/` — Backtesting and Testing
Testing infrastructure.

| Class | Purpose |
|-------|---------|
| `BacktestRunner` | Backtest engine (historical simulation) |
| `TCSServiceSmokeTest` | TCSService integration test |
| `UnifiedStrategySmokeTest` | UnifiedStrategy integration test |

## Key Entry Points

### Main Application
```java
// src/main/java/com/github/shk0da/GoldenDragon/GoldenDragon.java
public static void main(String[] args)
```

**Usage:**
```bash
./gradlew runStrategy -Pstrategy=UnifiedStrategy
```

### Backtest Engine
```java
// src/main/java/com/github/shk0da/GoldenDragon/test/BacktestRunner.java
public static void main(String[] args)
```

**Usage:**
```bash
./gradlew clean runBacktest
```

### ML Model Trainer
```java
// src/main/java/com/github/shk0da/GoldenDragon/ml/MlModelTrainer.java
public static void main(String[] args)
```

**Usage:**
```bash
./gradlew runMlTraining
```

## Common Patterns

### Strategy Pattern
```java
public class MyStrategy extends BaseStrategy {
    @Override
    public TradingDecision decide(String ticker, ...) {
        // Implement signal logic
        return TradingDecision.BUY(...);
    }
}
```

### Filter Pattern
```java
public class MyFilter {
    public boolean canTrade(List<Candle> candles, ...) {
        // Check conditions
        return true; // or false
    }
}
```

### Repository Pattern (Singleton)
```java
public class MyRepository extends AbstractRepository<Key, Value> {
    public static final MyRepository INSTANCE = new MyRepository();
}
```

### Money Management Integration
```java
// In strategy
if (!riskManager.canTrade(equity)) {
    return TradingDecision.HOLD("RISK_LIMIT");
}
if (!killSwitch.isTradingAllowed()) {
    return TradingDecision.HOLD("KILL_SWITCH");
}
int size = positionSizer.calculateSize(...);
```

## File Naming Conventions

- **Config classes**: `*Config.java` (e.g., `LevelTraderConfig.java`)
- **Strategy classes**: `*Strategy.java` or `*Trader.java` (e.g., `UnifiedStrategy.java`)
- **Service classes**: `*Service.java` (e.g., `TCSService.java`)
- **Model classes**: Noun (e.g., `Candle.java`, `Position.java`)
- **Filter classes**: `*Filter.java` (e.g., `MarketRegimeFilter.java`)
- **Repository classes**: `*Repository.java` (e.g., `TickerRepository.java`)
- **Utility classes**: `*Utils.java` (e.g., `IndicatorsUtil.java`)

## Code Style

- **Java 11**
- **No wildcards** in imports (except where required)
- **Full imports** preferred
- **Blank line** after class declaration
- **Comma** after each argument in multi-line signatures
- **Early returns** to minimize nesting
- **No `Pair`** in arguments or return types
- **DTO** for functions with >5 parameters

## Testing

### Smoke Tests
- `TCSServiceSmokeTest` — TCSService integration
- `UnifiedStrategySmokeTest` — UnifiedStrategy integration

### Backtest
- `BacktestRunner` — Historical simulation
- Modes: `fast` (6 periods), `full` (6 yearly periods)

## Configuration Files

- `application.properties` — Main configuration
- `tickers.json` — Cached ticker metadata
- `rsx.json` — RSX strategy state
- `data/<TICKER>/candlesHOUR.txt` — Hourly candles
- `data/<TICKER>/candles5_MIN.txt` — 5-minute candles
- `ml_strategy/data_pipeline/trades.csv` — ML training data
