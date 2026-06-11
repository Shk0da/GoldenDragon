# ADR 003: ML Integration via Data Collection

**Date:** 2024-01-01  
**Status:** Accepted  
**Context:** Machine Learning Integration

## Context

We want to use ML to improve trading decisions:
- Predict trade success probability
- Adjust position sizes based on confidence
- Detect market regimes

Challenge: Need labeled training data from real trading.

## Decision

Use **continuous data collection** approach:

1. **Collect during live trading:**
```java
public class TradeDataCollector {
    public void registerTrade(
        String ticker,
        double entryPrice,
        double exitPrice,
        double pnl,
        Map<String, Double> indicators
    ) {
        // Save to trades.csv
    }
}
```

2. **Train offline:**
```bash
./gradlew runMlTraining
```

3. **Use predictions in strategy:**
```java
double probability = mlPredictionService.predictProbability(features);
if (probability > threshold) {
    // Take trade
}

double multiplier = mlPredictionService.getPositionSizeMultiplier(features);
int size = baseSize * multiplier;
```

## Consequences

### Positive
- **Real data**: Models trained on actual market data
- **Continuous improvement**: More data = better models
- **No manual labeling**: PnL provides natural labels
- **Flexible**: Can retrain as market conditions change

### Negative
- **Cold start**: Need initial data before ML works
- **Data quality**: Bad trades pollute dataset
- **Complexity**: Additional ML infrastructure

### Trade-offs
- Chose offline training over online learning for stability
- Could add online learning later if needed

## Compliance

ML integration requires:
1. Call `TradeDataCollector.registerTrade()` after every exit
2. Retrain models periodically (`trainAllTickers`)
3. Use `MlPredictionService` for predictions
4. Log ML decisions for debugging

## Data Format

`trades.csv` columns:
```
entry_time,ticker,strategy,entry_price,exit_price,pnl,commission,
rsi,macd,atr,adx,regime,label
```

Where `label = 1` if profitable, `0` otherwise.
