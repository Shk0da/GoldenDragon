# AI Assistant Context

## Project Overview

**GoldenDragon** is an automated trading bot for the Russian stock market (MOEX) with multiple trading strategies, money management, and ML-powered regime detection.

### Quick Facts
- **Language**: Java 11
- **Build Tool**: Gradle
- **Main Broker API**: Tinkoff Invest API
- **ML Framework**: XGBoost
- **Key Patterns**: Strategy, Repository, Filter, Money Management

## Project Goals

1. **Automated Trading**: Execute trades on MOEX based on algorithmic strategies
2. **Risk Management**: Protect capital with position sizing, stop-losses, and daily limits
3. **ML Integration**: Use machine learning for regime detection and trade prediction
4. **Backtesting**: Validate strategies on historical data before live trading
5. **Multi-Strategy**: Support multiple concurrent strategies (Unified, LevelTrader, RSX, DivFlow)

## Key Design Decisions

### 1. Strategy Pattern
All strategies extend `BaseStrategy` which provides:
- Lifecycle management (trading hours, EOD close)
- Candle loading and caching
- Parallel ticker processing
- Filter integration
- Position management

**Why**: Code reuse, consistent lifecycle, easy to add new strategies.

### 2. Money Management Layer
Separate MM components (`RiskManager`, `KillSwitch`, `PositionSizer`, etc.) used by strategies.

**Why**: Decouple risk logic from signal generation, reusable across strategies.

### 3. Stateless Filters
Filters (`MarketRegimeFilter`, `BadWeatherFilter`, `GroupConfirmationFilter`) are stateless and thread-safe.

**Why**: Easy to test, no shared state, can be used in parallel processing.

### 4. Repository Pattern
In-memory repositories (`TickerRepository`, `FigiRepository`, `PricesRepository`) using `ConcurrentHashMap`.

**Why**: Fast lookups, thread-safe, simple API.

### 5. Singleton Services
Services (`TCSService`, `TelegramNotifyService`, `TradingViewService`) are singletons.

**Why**: Single connection to external APIs, shared state, resource efficiency.

### 6. ML Data Collection
`TradeDataCollector` collects labeled data during live trading for offline model training.

**Why**: Continuous improvement, real market data, no need for manual labeling.

## Common Patterns

### Strategy Implementation
```java
public class MyStrategy extends BaseStrategy {
    @Override
    public TradingDecision decide(String ticker, List<Candle> hourCandles, ...) {
        // 1. Apply filters
        if (!marketRegimeFilter.canTrade(...)) {
            return TradingDecision.HOLD("BAD_REGIME");
        }
        
        // 2. Generate signal
        if (buySignal) {
            return TradingDecision.BUY("MY_SIGNAL", quantity, entry, stopLoss, takeProfit);
        }
        
        return TradingDecision.HOLD("NO_SIGNAL");
    }
}
```

### Filter Implementation
```java
public class MyFilter {
    private final boolean enabled;
    
    public boolean canTrade(List<Candle> candles, ...) {
        if (!enabled) return true;
        
        // Check conditions
        if (badCondition) {
            return false;
        }
        
        return true;
    }
}
```

### Money Management Usage
```java
// In strategy
if (!killSwitch.isTradingAllowed()) {
    return TradingDecision.HOLD("KILL_SWITCH_ACTIVE");
}

if (!riskManager.canTrade(equity)) {
    return TradingDecision.HOLD("RISK_LIMIT_REACHED");
}

int size = positionSizer.calculateSize(ticker, entry, stopLoss, balance, atr);
```

### Repository Usage
```java
// Get ticker info
TickerInfo ticker = TickerRepository.INSTANCE.getById(new TickerInfo.Key("SBER", TickerType.STOCK));

// Insert new data
FigiRepository.INSTANCE.insert(key, figi);
```

### ML Integration
```java
// Collect data
TradeDataCollector.INSTANCE.registerTrade(...);

// Get prediction
double probability = mlPredictionService.predictProbability(features);
if (probability > threshold) {
    // Take trade
}

// Adjust position size
double multiplier = mlPredictionService.getPositionSizeMultiplier(features);
int size = (int)(baseSize * multiplier);
```

## Important Constraints

### Technical Constraints
- **Java 11** — Do not use Java 12+ features
- **No external dependencies** — Only what's in `build.gradle`
- **No wildcards** in imports (except where required)
- **No `Pair`** in arguments or return types
- **Max 5 parameters** — Extract to DTO if more

### Code Style
- **Full imports** preferred
- **Blank line** after class declaration
- **Comma** after each argument in multi-line signatures
- **Early returns** to minimize nesting
- **English comments** and documentation

### Business Constraints
- **No short selling** — Only BUY positions (in current implementation)
- **Trading hours** — 10:00-21:00 Moscow time, Mon-Fri
- **Position limits** — Max 8 concurrent positions
- **Daily loss limit** — Configurable (default 3%)
- **Risk per trade** — Configurable (default 1%)

## File Locations

### Entry Points
- `src/main/java/.../GoldenDragon.java` — Main application
- `src/main/java/.../test/BacktestRunner.java` — Backtest engine
- `src/main/java/.../ml/MlModelTrainer.java` — ML training

### Configuration
- `src/main/java/.../config/` — Configuration classes
- `application.properties` — Main config file

### Strategies
- `src/main/java/.../strategy/` — All strategies

### Money Management
- `src/main/java/.../money/` — Risk management components

### ML
- `src/main/java/.../ml/` — ML components
- `ml_strategy/data_pipeline/trades.csv` — Training data
- `ml_strategy/models/` — Trained models

### Data
- `data/<TICKER>/candlesHOUR.txt` — Hourly candles
- `data/<TICKER>/candles5_MIN.txt` — 5-minute candles
- `tickers.json` — Cached ticker metadata

## Build Commands

```bash
# Full build
./gradlew clean uberJar

# Run backtest
./gradlew clean runBacktest

# Run specific strategy
./gradlew runStrategy -Pstrategy=UnifiedStrategy

# Train ML model (default params)
./gradlew runMlTraining

# Train ML model (custom params)
./gradlew runMlTraining -Pdata=... -Poutput=... -Pticker=SBER

# Train models for all tickers
./gradlew trainAllTickers

# Full ML pipeline (backtest + train)
./gradlew generateModel

# Run smoke tests
./gradlew runTCSServiceSmokeTest
./gradlew runUnifiedStrategySmokeTest
```

## Testing Guidelines

### Unit Tests
- Use JUnit 5
- Mock external services (TCSService, Telegram)
- Test business logic in isolation

### Integration Tests
- Use smoke tests (`*SmokeTest.java`)
- Test with real API (sandbox mode)
- Verify end-to-end flow

### Backtest
- Use `BacktestRunner` for historical simulation
- Test on multiple periods (`fast` mode for quick tests, `full` for thorough)
- Compare strategies side-by-side

## Common Tasks

### Add New Strategy
1. Create class extending `BaseStrategy`
2. Implement `decide()` method
3. Add config class if needed
4. Register in `GoldenDragon.main()`
5. Add to `BacktestRunner.ALL_STRATEGIES`

### Add New Filter
1. Create stateless class in `filters/` package
2. Implement `canTrade()` method
3. Use in strategy's `decide()` method

### Add New Indicator
1. Add method to `IndicatorsUtil`
2. Use TA-Lib or custom calculation
3. Use in strategy's `decide()` method

### Modify ML Features
1. Update `TradeFeatures` class
2. Update `TradeDataCollector` to collect new feature
3. Retrain models with `runMlTraining`

### Change Configuration
1. Add property to `application.properties`
2. Add getter to config class
3. Use in strategy/service

## Troubleshooting

### Compilation Errors
```bash
./gradlew clean compileJava
```

### Runtime Errors
- Check `application.properties` for required properties
- Verify API keys are correct
- Check logs for detailed error messages

### ML Training Fails
- Ensure `ml_strategy/data_pipeline/trades.csv` exists
- Run backtest first to generate data: `./gradlew runBacktest`
- Check file permissions

### Backtest Slow
- Use `fast` mode: set `backtest.mode=fast` in properties
- Reduce `backtest.threads`
- Reduce number of tickers in `datacollector.instruments`

## External Resources

- **Tinkoff Invest API**: https://tinkoff.github.io/investAPI/
- **TA-Lib**: https://github.com/mrdoan/talib
- **XGBoost**: https://xgboost.readthedocs.io/
- **Gradle**: https://gradle.org/
