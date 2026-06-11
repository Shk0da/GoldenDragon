# ADR 002: Money Management Layer

**Date:** 2024-01-01  
**Status:** Accepted  
**Context:** Risk Control System

## Context

Trading strategies need robust risk management to protect capital:
- Limit position sizes
- Control daily losses
- Emergency shutdown on critical drawdown
- Adaptive position sizing based on performance

## Decision

Create **separate Money Management layer** with independent components:

```
money/
├── RiskManager          # Daily limits, consecutive losses
├── KillSwitch           # Emergency halt
├── PositionSizer        # Size calculation
├── StopLossManager      # Trailing stops
├── AdaptiveCapital      # Anti-martingale sizing
└── PerformanceTracker   # Statistics
```

Strategies use MM components:

```java
public class UnifiedStrategy extends BaseStrategy {
    private final RiskManager riskManager;
    private final KillSwitch killSwitch;
    private final PositionSizer positionSizer;
    
    public TradingDecision decide(...) {
        if (!riskManager.canTrade(equity)) {
            return TradingDecision.HOLD("RISK_LIMIT");
        }
        
        if (!killSwitch.isTradingAllowed()) {
            return TradingDecision.HOLD("KILL_SWITCH");
        }
        
        int size = positionSizer.calculateSize(...);
        // ...
    }
}
```

## Consequences

### Positive
- **Separation of concerns**: Risk logic separate from signal logic
- **Reusability**: MM components used by all strategies
- **Testability**: MM can be tested independently
- **Flexibility**: Easy to adjust risk parameters

### Negative
- **Complexity**: Additional layer to understand
- **Performance**: Extra method calls on each decision

### Trade-offs
- Chose separate layer over embedding in strategies
- Could simplify for very basic strategies

## Compliance

All production strategies must:
1. Check `RiskManager.canTrade()` before entry
2. Check `KillSwitch.isTradingAllowed()` before entry
3. Use `PositionSizer` for position calculation
4. Update `PerformanceTracker` after exit
5. Call `RiskManager.registerTrade()` after exit
