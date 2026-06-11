# ADR 001: Strategy Pattern for Trading Logic

**Date:** 2024-01-01  
**Status:** Accepted  
**Context:** Trading System Design

## Context

We need a flexible way to implement multiple trading strategies (Unified, LevelTrader, RSX, DivFlow, etc.) while sharing common infrastructure:
- Lifecycle management (trading hours, EOD close)
- Data loading and caching
- Position management
- Filter integration
- Money management

## Decision

Use **Strategy Pattern** with abstract base class `BaseStrategy`:

```java
public abstract class BaseStrategy {
    // Common lifecycle
    public void run() { ... }
    
    // Common data loading
    protected List<Candle> loadCandles(...) { ... }
    
    // Common position management
    protected void openPosition(...) { ... }
    
    // Abstract method for subclasses
    public abstract TradingDecision decide(...);
}
```

Concrete strategies extend `BaseStrategy` and implement `decide()`:

```java
public class UnifiedStrategy extends BaseStrategy {
    @Override
    public TradingDecision decide(...) {
        // Strategy-specific logic
    }
}
```

## Consequences

### Positive
- **Code reuse**: Common logic in one place
- **Consistency**: All strategies follow same lifecycle
- **Easy extension**: Add new strategy by extending one class
- **Testability**: Can test base class and strategies separately

### Negative
- **Inheritance coupling**: Changes to base class affect all strategies
- **Rigid hierarchy**: Hard to mix behaviors from multiple strategies

### Trade-offs
- Chose inheritance over composition for simplicity
- Could refactor to composition if needed later

## Compliance

All strategies must:
1. Extend `BaseStrategy`
2. Implement `decide()` method
3. Use common filters (`MarketRegimeFilter`, `BadWeatherFilter`)
4. Integrate with money management components
