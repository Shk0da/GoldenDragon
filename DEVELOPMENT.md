# Development Guide

## Build System

### Gradle Tasks

| Task | Description |
|------|-------------|
| `clean` | Clean build directory |
| `compileJava` | Compile source code |
| `build` | Full build (compile + test) |
| `uberJar` | Create fat JAR for deployment |
| `runBacktest` | Run backtest engine |
| `runStrategy` | Run trading strategy |
| `runMlTraining` | Train ML model |
| `trainAllTickers` | Train models for all tickers |
| `generateModel` | Full ML pipeline (backtest + train) |
| `runTCSServiceSmokeTest` | Run TCSService smoke test |
| `runUnifiedStrategySmokeTest` | Run UnifiedStrategy smoke test |

### Build Commands

```bash
# Clean build
./gradlew clean build

# Create deployment JAR
./gradlew clean uberJar

# Compile only
./gradlew compileJava

# Run tests
./gradlew test
```

## Testing

### Smoke Tests

Smoke tests verify integration with external services (Tinkoff API, etc.).

**Run TCSService smoke test:**
```bash
./gradlew runTCSServiceSmokeTest
```

**Run UnifiedStrategy smoke test:**
```bash
./gradlew runUnifiedStrategySmokeTest
```

**Requirements:**
- Valid API keys in `application.properties`
- Network access to Tinkoff API
- Sandbox mode recommended for testing

### Backtest

Backtest engine simulates trading on historical data.

**Run backtest:**
```bash
./gradlew clean runBacktest
```

**Backtest modes:**
- `fast` — 6 short periods (quick validation)
- `full` — 6 yearly periods (thorough testing)

**Configure in `application.properties`:**
```properties
backtest.mode=fast
backtest.threads=4
backtest.startBalance=1000000
backtest.commissionPercent=0.05
```

**Output:**
- PnL per strategy
- Trade count and win rate
- Maximum drawdown
- Sharpe ratio
- Final balance

### Unit Tests

Currently no unit tests. Consider adding:
- Filter logic tests
- Money management tests
- Indicator calculation tests
- ML feature tests

## Configuration

### application.properties

Main configuration file. Key sections:

```properties
# Tinkoff Invest API
tcs.apiKey=...
tcs.accountId=...
tcs.isSandbox=true
tcs.testMode=false

# Market settings
market.moex.currency=RUB
market.moex.maxPositionCostToBuy=100000

# Strategy parameters
unifiedTrader.ticker.SBER.enabled=true
unifiedTrader.ticker.SBER.slMult=1.2
unifiedTrader.ticker.SBER.tpMult=2.5
unifiedTrader.ticker.SBER.riskP=0.01

# Money management
unifiedTrader.mmEnabled=true
unifiedTrader.mmRiskPercent=0.01
unifiedTrader.mmMaxDailyLossPercent=0.03
unifiedTrader.mmCriticalDrawdownPercent=0.10

# Telegram notifications
telegram.notify.enable=true
telegram.notify.botToken=...
telegram.notify.chatId=...
telegram.notify.extended=true

# Backtest settings
backtest.mode=fast
backtest.threads=4
backtest.startBalance=1000000

# ML settings
ml.tradesPath=ml_strategy/data_pipeline/trades.csv
ml.modelPath=ml_strategy/models/trade_classifier_v2.txt
ml.reportDir=ml_strategy
```

### System Properties

Override properties via command line:

```bash
# Run with custom strategy
./gradlew runStrategy -Pstrategy=UnifiedStrategy

# Run ML training with custom data
./gradlew runMlTraining -Pdata=custom/data.csv -Poutput=custom/model.txt

# Run with custom ticker
./gradlew runMlTraining -Pticker=SBER
```

### Environment Variables

Not currently used. All configuration via `application.properties`.

## Code Style

### Import Rules
- **No wildcards** (except where required)
- **Full imports** preferred
- Group imports by package

```java
// Good
import com.github.shk0da.GoldenDragon.model.Candle;
import com.github.shk0da.GoldenDragon.model.Position;

// Bad
import com.github.shk0da.GoldenDragon.model.*;
```

### Class Structure
- Blank line after class declaration
- Fields before methods
- One blank line between fields and methods

```java
public class MyClass {
    
    private final String field1;
    private final int field2;
    
    public MyClass(String field1, int field2) {
        this.field1 = field1;
        this.field2 = field2;
    }
    
    public void method1() {
        // ...
    }
    
    public void method2() {
        // ...
    }
}
```

### Method Signatures
- Comma after each argument in multi-line signatures
- Max 5 parameters (extract to DTO if more)

```java
// Good
public TradingDecision decide(
    String ticker,
    List<Candle> hourCandles,
    List<Candle> minCandles,
    Position position,
    double balance
) {
    // ...
}

// Bad (too many parameters)
public TradingDecision decide(String ticker, List<Candle> hourCandles, 
    List<Candle> minCandles, Position position, double balance, 
    double atr, double rsi, double macd, boolean hourChanged) {
    // ...
}
```

### Control Flow
- Early returns to minimize nesting
- Validate input at beginning

```java
// Good
public boolean canTrade(List<Candle> candles) {
    if (candles == null || candles.isEmpty()) {
        return false;
    }
    
    if (!enabled) {
        return true;
    }
    
    // Main logic
    return true;
}

// Bad (deep nesting)
public boolean canTrade(List<Candle> candles) {
    if (candles != null && !candles.isEmpty()) {
        if (enabled) {
            // Main logic
            return true;
        }
    }
    return false;
}
```

### Comments and Documentation
- English only
- Javadoc for public classes and methods
- Inline comments explain "why", not "what"

```java
/**
 * Calculate position size based on entry price, stop loss and available balance.
 *
 * @param ticker ticker symbol
 * @param entry entry price
 * @param stopLoss stop loss price
 * @param balance available balance
 * @param atr current ATR value (optional)
 * @return position size in units (0 if calculation fails)
 */
public int calculateSize(String ticker, double entry, double stopLoss, 
                         double balance, double atr) {
    // Reduce size during high volatility
    if (atr > threshold) {
        return baseSize / 2;
    }
    return baseSize;
}
```

## Debugging

### Logging

Current logging via `System.out`:
```java
out.println("Position opened: " + ticker);
out.printf("PnL: %.2f%n", pnl);
```

Consider adding structured logging (SLF4J + Logback) for:
- Trade execution
- Error messages
- Performance metrics

### IntelliJ IDEA

**Run configurations:**
- `GoldenDragon` — Main application
- `BacktestRunner` — Backtest engine
- `MlModelTrainer` — ML training

**Debug mode:**
1. Set breakpoints in strategy code
2. Run in debug mode
3. Inspect variables and call stack

## Common Development Tasks

### Add New Strategy

1. **Create strategy class:**
```java
public class MyStrategy extends BaseStrategy {
    @Override
    public TradingDecision decide(String ticker, ...) {
        // Implement logic
        return TradingDecision.HOLD("NO_SIGNAL");
    }
}
```

2. **Create config class (if needed):**
```java
public class MyStrategyConfig {
    private final double param1;
    
    public MyStrategyConfig() throws IOException {
        Properties props = PropertiesUtils.loadProperties();
        param1 = Double.parseDouble(props.getProperty("myStrategy.param1", "1.0"));
    }
}
```

3. **Register in GoldenDragon:**
```java
if ("MyStrategy".equals(strategy)) {
    new MyStrategy(config, tcsService).run();
}
```

4. **Add to backtest:**
```java
// In BacktestRunner
ALL_STRATEGIES.add("MyStrategy");
```

### Add New Filter

1. **Create filter class:**
```java
public class MyFilter {
    private final boolean enabled;
    
    public MyFilter(boolean enabled) {
        this.enabled = enabled;
    }
    
    public boolean canTrade(List<Candle> candles, ...) {
        if (!enabled) return true;
        
        // Check conditions
        return true;
    }
}
```

2. **Use in strategy:**
```java
if (!myFilter.canTrade(candles, ...)) {
    return TradingDecision.HOLD("MY_FILTER_BLOCKED");
}
```

### Add New Indicator

1. **Add to IndicatorsUtil:**
```java
public static double calculateMyIndicator(List<Candle> candles, int period) {
    // Implementation
    return value;
}
```

2. **Use in strategy:**
```java
double value = IndicatorsUtil.calculateMyIndicator(candles, 14);
if (value > threshold) {
    // Signal
}
```

### Modify ML Features

1. **Update TradeFeatures:**
```java
public class TradeFeatures {
    public final double rsi;
    public final double macd;
    public final double newFeature; // Add field
    
    public TradeFeatures(..., double newFeature) {
        this.newFeature = newFeature;
    }
}
```

2. **Update TradeDataCollector:**
```java
new TradeFeatures(..., calculateNewFeature(candles));
```

3. **Retrain models:**
```bash
./gradlew runMlTraining
```

### Change Configuration

1. **Add to application.properties:**
```properties
my.new.property=value
```

2. **Add getter to config class:**
```java
public String getMyProperty() {
    return myProperty;
}
```

3. **Use in code:**
```java
String value = config.getMyProperty();
```

## Performance Optimization

### Backtest Performance

1. **Reduce threads** if CPU-bound:
```properties
backtest.threads=2
```

2. **Use fast mode** for quick iterations:
```properties
backtest.mode=fast
```

3. **Reduce ticker list** for testing:
```properties
datacollector.instruments=SBER,GAZP,LKOH
```

### Runtime Performance

1. **Cache expensive calculations** (indicators, features)
2. **Use parallel streams** for independent operations
3. **Minimize API calls** (use repository cache)
4. **Batch database operations** (if using database)

### ML Training Performance

1. **Use per-ticker models** for better accuracy:
```bash
./gradlew trainAllTickers
```

2. **Reduce feature set** if training is slow
3. **Use GPU** if available (XGBoost supports GPU)

## Deployment

### Build JAR

```bash
./gradlew clean uberJar
```

Output: `build/libs/GoldenDragon-1.0.jar`

### Run JAR

```bash
java -jar build/libs/GoldenDragon-1.0.jar UnifiedStrategy
```

### Server Deployment

1. Copy JAR to server
2. Copy `application.properties` to server
3. Run with systemd or similar:
```bash
java -jar GoldenDragon-1.0.jar UnifiedStrategy
```

## Version Control

### Git Workflow

```bash
# Feature branch
git checkout -b feature/my-feature

# Commit changes
git add .
git commit -m "Add my feature"

# Push and create PR
git push origin feature/my-feature
```

### Commit Message Format

```
<type>: <description>

[optional body]

[optional footer]
```

**Types:**
- `feat` — New feature
- `fix` — Bug fix
- `docs` — Documentation
- `style` — Code style (formatting)
- `refactor` — Code refactoring
- `test` — Tests
- `chore` — Build/config changes

**Example:**
```
feat: add new market regime filter

- Implement ADX-based regime detection
- Add filter to UnifiedStrategy
- Update documentation

Closes #123
```

## Continuous Integration

Currently no CI/CD. Consider adding:
- GitHub Actions for builds
- Automated tests on PR
- Code quality checks (SpotBugs, PMD)
- Automated deployment
