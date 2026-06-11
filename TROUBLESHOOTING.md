# Troubleshooting

## Common Issues

### Compilation Errors

**Problem:** Build fails with compilation errors

**Solution:**
```bash
# Clean and rebuild
./gradlew clean compileJava

# Check Java version
java -version  # Should be Java 11

# If using wrong Java version
export JAVA_HOME=/path/to/java11
./gradlew clean compileJava
```

### Runtime Errors

**Problem:** Application crashes at startup

**Check:**
1. `application.properties` exists and is valid
2. API keys are correct
3. Network connection is available

**Solution:**
```bash
# Run with verbose output
./gradlew runStrategy --info

# Check logs for specific error
```

### API Connection Errors

**Problem:** Cannot connect to Tinkoff API

**Possible causes:**
- Invalid API key
- Account ID incorrect
- Network issues
- API rate limits

**Solution:**
```bash
# Verify API key in application.properties
tcs.apiKey=YOUR_API_KEY
tcs.accountId=YOUR_ACCOUNT_ID

# Test connection (sandbox mode)
tcs.isSandbox=true

# Check network
ping invest-public-api.tinkoff.ru
```

### Backtest Issues

**Problem:** Backtest is very slow

**Solution:**
```bash
# Use fast mode
backtest.mode=fast

# Reduce threads
backtest.threads=2

# Reduce ticker list
datacollector.instruments=SBER,GAZP
```

**Problem:** No data files found

**Solution:**
```bash
# Check data directory exists
ls -la data/

# Re-download data
rm -rf data/
./gradlew clean runBacktest
```

### ML Training Issues

**Problem:** trades.csv not found

**Solution:**
```bash
# Run backtest first to generate data
./gradlew clean runBacktest

# Verify file exists
ls -la ml_strategy/data_pipeline/trades.csv
```

**Problem:** Model training fails

**Solution:**
```bash
# Check data file has content
wc -l ml_strategy/data_pipeline/trades.csv

# Should have > 100 lines for training
# If too few lines, run more backtests

# Try with different parameters
./gradlew runMlTraining -Pdata=ml_strategy/data_pipeline/trades.csv
```

**Problem:** Out of memory during training

**Solution:**
```bash
# Increase heap size
export GRADLE_OPTS="-Xmx4g"
./gradlew runMlTraining
```

### Telegram Notifications

**Problem:** Notifications not sent

**Solution:**
```bash
# Enable notifications
telegram.notify.enable=true

# Verify bot token
telegram.notify.botToken=YOUR_BOT_TOKEN

# Verify chat ID
telegram.notify.chatId=YOUR_CHAT_ID

# Test bot manually via Telegram
```

### Position Management

**Problem:** Positions not closing

**Possible causes:**
- Trading hours ended
- Strategy logic issue
- API connection lost

**Solution:**
```bash
# Check trading hours (10:00-21:00 Moscow time)
# Check logs for close signals
# Verify API connection
```

**Problem:** Too many concurrent positions

**Solution:**
```bash
# Reduce max concurrent positions
# In strategy config:
unifiedTrader.maxConcurrentPositions=5
```

### Performance Issues

**Problem:** High CPU usage

**Solution:**
```bash
# Reduce backtest threads
backtest.threads=2

# Reduce ticker count
datacollector.instruments=SBER,GAZP,LKOH

# Use larger candle intervals
# (hourly instead of 5-minute)
```

**Problem:** High memory usage

**Solution:**
```bash
# Increase heap size
export GRADLE_OPTS="-Xmx2g"

# Reduce data retention
# Clear old candle files
rm -rf data/*/candles5_MIN.txt
```

### Data Issues

**Problem:** Stale candle data

**Solution:**
```bash
# Clear cache
rm data/*/candles*.txt

# Re-download
./gradlew clean runBacktest
```

**Problem:** Missing tickers

**Solution:**
```bash
# Add ticker to config
unifiedTrader.ticker.NEW.enabled=true

# Download data
./gradlew clean runBacktest
```

## Debug Mode

### Enable Debug Logging

Currently logging via `System.out`. To add debug logging:

1. Add SLF4J + Logback to `build.gradle`
2. Configure `logback.xml`
3. Replace `out.println` with `logger.debug`

### Inspect State

**During backtest:**
```bash
# Add println statements in strategy
# Run backtest
./gradlew clean runBacktest

# Check output for state
```

**During live trading:**
```bash
# Enable extended Telegram notifications
telegram.notify.extended=true

# Check logs regularly
```

## Get Help

### Check Documentation

- **Architecture**: `ARCHITECTURE.md`
- **Codebase**: `CODEBASE.md`
- **Quick Start**: `QUICKSTART.md`
- **Development**: `DEVELOPMENT.md`
- **AI Context**: `AI_CONTEXT.md`

### Check Logs

```bash
# Console output
./gradlew runStrategy 2>&1 | tee run.log

# Search for errors
grep -i error run.log
```

### Check Configuration

```bash
# Verify properties file
cat application.properties

# Check for typos
# Verify API keys
# Verify ticker symbols
```

### Contact Support

- **GitHub Issues**: Create issue with error details
- **Documentation**: Review all MD files
- **Code**: Check relevant source files

## Error Codes

### Tinkoff API Errors

- `UNAUTHENTICATED` — Invalid API key
- `PERMISSION_DENIED` — Account access denied
- `NOT_FOUND` — Instrument not found
- `INVALID_ARGUMENT` — Invalid request parameters

### Application Errors

- `NO_DATA` — Historical data not found
- `API_ERROR` — External API failure
- `CONFIG_ERROR` — Configuration issue
- `MEMORY_ERROR` — Out of memory

## Recovery Procedures

### Reset Everything

```bash
# Clean all generated files
./gradlew clean
rm -rf data/
rm -rf ml_strategy/
rm tickers.json

# Rebuild
./gradlew build

# Run backtest
./gradlew clean runBacktest
```

### Recover from Bad Trade

```bash
# Stop application
# Manually close position via Tinkoff app
# Restart application
# Check logs for cause
```

### Recover from Data Corruption

```bash
# Remove corrupted files
rm data/<TICKER>/candles*.txt

# Re-download
./gradlew clean runBacktest
```
