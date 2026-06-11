# Quick Start

## First Time Setup

### 1. Clone Repository

```bash
git clone <repository-url>
cd GoldenDragon
```

### 2. Install Java 11

```bash
# Ubuntu/Debian
sudo apt install openjdk-11-jdk

# macOS
brew install openjdk@11

# Verify
java -version
```

### 3. Configure Application

Copy example properties file:

```bash
cp application.properties.example application.properties
```

Edit `application.properties`:

```properties
# Tinkoff Invest API (get from https://tinkoff.ru/invest/settings/)
tcs.apiKey=YOUR_API_KEY
tcs.accountId=YOUR_ACCOUNT_ID
tcs.isSandbox=true

# Telegram notifications (optional)
telegram.notify.enable=false
telegram.notify.botToken=YOUR_BOT_TOKEN
telegram.notify.chatId=YOUR_CHAT_ID

# Strategy parameters
unifiedTrader.ticker.SBER.enabled=true
unifiedTrader.ticker.GAZP.enabled=true
```

### 4. Build Project

```bash
./gradlew clean build
```

### 5. Run Backtest (Recommended First Step)

```bash
./gradlew clean runBacktest
```

This will:
- Download historical data
- Run all strategies
- Show performance metrics

**Expected output:**
```
==========================================
Strategy: UnifiedStrategy
PnL: 15000.00
Trades: 42
Win Rate: 0.62
Max Drawdown: 0.05
Sharpe Ratio: 1.8
Final Balance: 1015000.00
==========================================
```

## Common Tasks

### Run Trading Strategy

```bash
# Run UnifiedStrategy (default)
./gradlew runStrategy -Pstrategy=UnifiedStrategy

# Run LevelTrader
./gradlew runStrategy -Pstrategy=LevelTrader

# Run RSX
./gradlew runStrategy -Pstrategy=RSX
```

### Train ML Model

```bash
# Train with default parameters
./gradlew runMlTraining

# Train for specific ticker
./gradlew runMlTraining -Pticker=SBER

# Train with custom data/output
./gradlew runMlTraining -Pdata=my_data.csv -Poutput=my_model.txt

# Train models for all tickers
./gradlew trainAllTickers

# Full pipeline (backtest + train)
./gradlew generateModel
```

### Run Tests

```bash
# TCSService smoke test
./gradlew runTCSServiceSmokeTest

# UnifiedStrategy smoke test
./gradlew runUnifiedStrategySmokeTest

# All tests
./gradlew test
```

### Build for Deployment

```bash
./gradlew clean uberJar
```

Output: `build/libs/GoldenDragon-1.0.jar`

## Directory Structure

After first run, you'll see:

```
GoldenDragon/
├── data/                    # Historical data
│   ├── SBER/
│   │   ├── candlesHOUR.txt
│   │   └── candles5_MIN.txt
│   └── GAZP/
│       └── ...
├── ml_strategy/             # ML models and data
│   ├── data_pipeline/
│   │   └── trades.csv
│   └── models/
│       └── trade_classifier_v2.txt
├── tickers.json             # Cached ticker metadata
└── application.properties   # Configuration
```

## Next Steps

1. **Review Architecture**: Read `ARCHITECTURE.md` to understand system design
2. **Explore Codebase**: See `CODEBASE.md` for package structure
3. **Development Guide**: Check `DEVELOPMENT.md` for coding guidelines
4. **AI Context**: Read `AI_CONTEXT.md` for common patterns and constraints

## Troubleshooting

### "Java 11 not found"

```bash
# Check Java version
java -version

# Install Java 11
# See section 2 above
```

### "API key invalid"

1. Get API key from https://tinkoff.ru/invest/settings/
2. Update `tcs.apiKey` in `application.properties`
3. Ensure account ID is correct

### "No trades.csv found"

Run backtest first to generate data:

```bash
./gradlew clean runBacktest
```

### "Out of memory"

Increase heap size:

```bash
export GRADLE_OPTS="-Xmx2g"
./gradlew clean runBacktest
```

## Get Help

- **Architecture**: `ARCHITECTURE.md`
- **Codebase**: `CODEBASE.md`
- **Development**: `DEVELOPMENT.md`
- **AI Context**: `AI_CONTEXT.md`
- **Troubleshooting**: `TROUBLESHOOTING.md`
