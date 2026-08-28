# Strategy Consolidation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Consolidate RegimeAwareStrategy + UnifiedStrategy into single RegimeAwareStrategy, remove duplicate strategies (RegimeAwareStrategyMl, TmonAveragingStrategy, PrecisionStrategy), update BasicTMON with monthly 100k deposit.

**Architecture:** 
- RegimeAwareStrategy becomes the primary strategy combining regime detection + unified trading logic
- Remove wrapper strategies that delegate to RegimeAwareStrategyMl (PrecisionStrategy) or create unnecessary complexity
- TmonAveragingStrategy logic simplified to basic monthly deposit + TMON@ holding
- StrategyRegistry cleaned up to expose only working strategies

**Tech Stack:** Java 11, Spring Boot, existing strategy framework

---

### Task 01: Consolidate RegimeAwareStrategy + UnifiedStrategy

**Files:**
- Modify: `src/main/java/com/github/shk0da/goldendragon/strategy/RegimeAwareStrategy.java`
- Delete: `src/main/java/com/github/shk0da/goldendragon/strategy/UnifiedStrategy.java` (after migration)

**Goal:** Merge UnifiedStrategy logic into RegimeAwareStrategy, remove delegation pattern.

- [ ] **Step 1: Read UnifiedStrategy.decide() method completely**

Read lines 256-800 of UnifiedStrategy.java to understand full decide() logic including:
- Money Management (KillSwitch, RiskManager, PositionSizer, StopLossManager)
- TMON@ Cash Parking logic
- Entry/exit logic with MM sizing
- Trailing stop logic

- [ ] **Step 2: Read RegimeAwareStrategy.decide() method**

Read lines 80-200 of RegimeAwareStrategy.java to understand current regime detection:
- ADX calculation for regime detection (TREND/RANGE/NORMAL)
- Balance adjustment based on regime (1.5x for TREND, 0.5x for RANGE)
- Delegation to unifiedStrategy.decide()

- [ ] **Step 3: Merge UnifiedStrategy.decide() into RegimeAwareStrategy.decide()**

Replace delegation pattern with inline logic. New RegimeAwareStrategy.decide() should:
1. Detect market regime using ADX (existing logic)
2. Adjust balance based on regime (existing logic)
3. Execute UnifiedStrategy logic inline (copy from UnifiedStrategy.decide())
4. Return TradingDecision

Code structure:
```java
@Override
public TradingDecision decide(String ticker, List<Candle> hourCandles, 
                              List<Candle> minuteCandles, Position position, 
                              double balance, boolean incrementCandlesHeld) {
    // 1. Detect regime (existing RegimeAwareStrategy logic)
    MarketRegime regime = detectRegime(hourCandles);
    
    // 2. Adjust balance based on regime
    double adjustedBalance = balance;
    if (regime == MarketRegime.TREND) {
        adjustedBalance = balance * 1.5;
    } else if (regime == MarketRegime.RANGE) {
        adjustedBalance = balance * 0.5;
    }
    
    // 3. Execute UnifiedStrategy logic inline (copy from UnifiedStrategy.decide())
    // ... entire UnifiedStrategy.decide() body ...
    
    return decision;
}
```

- [ ] **Step 4: Move MM initialization from UnifiedStrategy to RegimeAwareStrategy**

Copy constructor initialization from UnifiedStrategy to RegimeAwareStrategy:
```java
public RegimeAwareStrategy(...) {
    super(...);
    
    // Copy MM initialization from UnifiedStrategy constructor
    this.mmEnabled = config.mmEnabled;
    if (mmEnabled) {
        this.positionSizer = new PositionSizer(sizingStrategy);
        this.riskManager = new RiskManager(...);
        this.stopLossManager = new StopLossManager(...);
        this.adaptiveCapital = new AdaptiveCapital(...);
        this.killSwitch = new KillSwitch(...);
        this.performanceTracker = new PerformanceTracker();
    }
}
```

- [ ] **Step 5: Move MM fields from UnifiedStrategy to RegimeAwareStrategy**

Copy these fields to RegimeAwareStrategy:
```java
protected final boolean mmEnabled;
protected PositionSizer positionSizer;
protected RiskManager riskManager;
protected StopLossManager stopLossManager;
protected AdaptiveCapital adaptiveCapital;
protected KillSwitch killSwitch;
protected PerformanceTracker performanceTracker;
protected Map<String, Double> initialRiskPerTicker = new ConcurrentHashMap<>();
protected Integer fixedEntryLeverage = null;
```

- [ ] **Step 6: Update RegimeAwareStrategy to handle TMON@ Cash Parking**

Copy decideTmonCashParking() method from UnifiedStrategy to RegimeAwareStrategy.

- [ ] **Step 7: Compile and verify no errors**

Run: `./gradlew compileJava`
Expected: BUILD SUCCESSFUL

- [ ] **Step 8: Commit**

```bash
git add src/main/java/com/github/shk0da/goldendragon/strategy/RegimeAwareStrategy.java
git rm src/main/java/com/github/shk0da/goldendragon/strategy/UnifiedStrategy.java
git commit -m "refactor: consolidate RegimeAwareStrategy + UnifiedStrategy

- Merge UnifiedStrategy.decide() logic into RegimeAwareStrategy
- Remove delegation pattern, inline all trading logic
- Move MM initialization, fields, TMON@ cash parking to RegimeAwareStrategy
- Delete UnifiedStrategy.java (no longer needed)"
```

---

### Task 02: Delete RegimeAwareStrategyMl, PrecisionStrategy

**Files:**
- Delete: `src/main/java/com/github/shk0da/goldendragon/strategy/RegimeAwareStrategyMl.java`
- Delete: `src/main/java/com/github/shk0da/goldendragon/strategy/PrecisionStrategy.java`
- Modify: `src/main/java/com/github/shk0da/goldendragon/strategy/StrategyRegistry.java`

- [ ] **Step 1: Verify no references to RegimeAwareStrategyMl**

Run: `grep -r "RegimeAwareStrategyMl" src/main/java --include="*.java" | grep -v "RegimeAwareStrategyMl.java"`
Expected: Only StrategyRegistry.java references

- [ ] **Step 2: Verify no references to PrecisionStrategy**

Run: `grep -r "PrecisionStrategy" src/main/java --include="*.java" | grep -v "PrecisionStrategy.java"`
Expected: Only StrategyRegistry.java references

- [ ] **Step 3: Remove RegimeAwareStrategyMl from StrategyRegistry**

Delete these lines from StrategyRegistry.java:
```java
register(
        "RegimeAwareStrategyMl",
        runAndNotify(
                "RegimeAwareStrategyMl",
                "Stop RegimeAwareStrategyMl",
                (mc, mkt, tcs, args) ->
                        new RegimeAwareStrategyMl(new UnifiedTraderConfig(), tcs).run()),
        (config, tcsService) -> new RegimeAwareStrategyMl(config, tcsService, new Config(), true, true, true));
```

- [ ] **Step 4: Remove PrecisionStrategy from StrategyRegistry**

Delete these lines from StrategyRegistry.java:
```java
register(
        "PrecisionStrategy",
        runAndNotify(
                "PrecisionStrategy",
                "Stop PrecisionStrategy",
                (mc, mkt, tcs, args) ->
                        new PrecisionStrategy(new UnifiedTraderConfig(), tcs).run()),
        (config, tcsService) -> new PrecisionStrategy(config, tcsService, new Config(), true));
```

- [ ] **Step 5: Delete RegimeAwareStrategyMl.java**

Run: `git rm src/main/java/com/github/shk0da/goldendragon/strategy/RegimeAwareStrategyMl.java`

- [ ] **Step 6: Delete PrecisionStrategy.java**

Run: `git rm src/main/java/com/github/shk0da/goldendragon/strategy/PrecisionStrategy.java`

- [ ] **Step 7: Compile and verify no errors**

Run: `./gradlew compileJava`
Expected: BUILD SUCCESSFUL

- [ ] **Step 8: Commit**

```bash
git add src/main/java/com/github/shk0da/goldendragon/strategy/StrategyRegistry.java
git commit -m "refactor: remove RegimeAwareStrategyMl and PrecisionStrategy

- Delete RegimeAwareStrategyMl.java (ML filtering merged into RegimeAwareStrategy if needed)
- Delete PrecisionStrategy.java (was thin wrapper over RegimeAwareStrategyMl)
- Remove entries from StrategyRegistry"
```

---

### Task 03: Simplify TmonAveragingStrategy to BasicTMON with monthly deposit

**Files:**
- Rename: `src/main/java/com/github/shk0da/goldendragon/strategy/TmonAveragingStrategy.java` → `BasicTMON.java`
- Modify: `src/main/java/com/github/shk0da/goldendragon/strategy/BasicTMON.java`

**Goal:** Simplify to basic monthly deposit strategy, remove complex scaling logic.

- [ ] **Step 1: Rename TmonAveragingStrategy.java to BasicTMON.java**

Run: `git mv src/main/java/com/github/shk0da/goldendragon/strategy/TmonAveragingStrategy.java src/main/java/com/github/shk0da/goldendragon/strategy/BasicTMON.java`

- [ ] **Step 2: Update class name and package**

In BasicTMON.java replace:
```java
public class TmonAveragingStrategy extends BaseStrategy {
```
With:
```java
public class BasicTMON extends BaseStrategy {
```

- [ ] **Step 3: Simplify decide() to HOLD-only logic**

Replace entire decide() method with simple HOLD logic:
```java
@Override
public TradingDecision decide(String ticker, List<Candle> hourCandles,
                              List<Candle> minuteCandles, Position position,
                              double balance, boolean incrementCandlesHeld) {
    // BasicTMON: just hold TMON@ and accumulate monthly deposits
    // No active trading, no scaling in/out
    return new TradingDecision("HOLD", "monthly_accumulation");
}
```

- [ ] **Step 4: Add monthly deposit tracking**

Add fields:
```java
private double monthlyDepositAmount = 100_000.0;
private OffsetDateTime lastDepositDate = null;
```

- [ ] **Step 5: Add monthly deposit check in run() method**

Add to processTmonTicker() before decide() call:
```java
checkAndApplyMonthlyDeposit();
```

Implement checkAndApplyMonthlyDeposit():
```java
private void checkAndApplyMonthlyDeposit() {
    OffsetDateTime now = OffsetDateTime.now();
    if (lastDepositDate == null || 
        (now.getYear() > lastDepositDate.getYear() || 
         now.getMonthValue() > lastDepositDate.getMonthValue())) {
        cashBalance += monthlyDepositAmount;
        lastDepositDate = now;
        log("BasicTMON monthly deposit: +" + String.format("%.2f", monthlyDepositAmount) + 
            ", cash=" + String.format("%.2f", cashBalance));
    }
}
```

- [ ] **Step 6: Update constructors**

Replace constructors with:
```java
public BasicTMON(UnifiedTraderConfig config, TCSService tcsService) {
    this(config, tcsService, new Config(), false, 1_000_000.0);
}

public BasicTMON(UnifiedTraderConfig config, TCSService tcsService,
                 Config backtestConfig, boolean isBacktest, double initialCash) {
    super(config, tcsService, backtestConfig, isBacktest);
    this.cashBalance = initialCash;
    this.monthlyDepositAmount = 100_000.0;
}
```

- [ ] **Step 7: Remove unused fields and methods**

Delete:
- phase, entryStep, exitStep, avgEntryPrice, positionQuantity fields
- Phase enum
- handleWaiting(), handleScalingIn(), handleHolding(), handleScalingOut(), handleExited()
- executeBuy(), executeSell(), computeDynamicSteps(), computeAtr()
- getPhase(), getPositionQuantity(), getAvgEntryPrice()

Keep:
- cashBalance, lastCandles
- addMonthlyDeposit() (for external deposits)
- processTmonTicker(), run()

- [ ] **Step 8: Update log messages**

Replace all "TmonAveragingStrategy" with "BasicTMON" in log messages.

- [ ] **Step 9: Compile and verify no errors**

Run: `./gradlew compileJava`
Expected: BUILD SUCCESSFUL

- [ ] **Step 10: Commit**

```bash
git add src/main/java/com/github/shk0da/goldendragon/strategy/BasicTMON.java
git commit -m "feat: simplify TmonAveragingStrategy to BasicTMON

- Rename TmonAveragingStrategy → BasicTMON
- Remove complex scaling in/out logic
- Add monthly 100k deposit tracking
- Simplify to HOLD-only strategy for passive TMON@ accumulation"
```

---

### Task 04: Update StrategyRegistry

**Files:**
- Modify: `src/main/java/com/github/shk0da/goldendragon/strategy/StrategyRegistry.java`

- [ ] **Step 1: Remove TmonAveragingStrategy entry**

Delete from StrategyRegistry:
```java
register(
        "TmonAveragingStrategy",
        runAndNotify(
                "TmonAveragingStrategy",
                "Stop TmonAveragingStrategy",
                (mc, mkt, tcs, args) ->
                        new TmonAveragingStrategy(
                                        new TmonAveragingConfig(),
                                        tcs,
                                        new Config(),
                                        false,
                                        0)
                                .run()),
        (config, tcsService) ->
                new TmonAveragingStrategy(
                        new TmonAveragingConfig(), tcsService, new Config(), true, 1_000_000.0));
```

- [ ] **Step 2: Add BasicTMON entry**

Add to StrategyRegistry after RegimeAwareStrategy:
```java
register(
        "BasicTMON",
        runAndNotify(
                "BasicTMON",
                "Stop BasicTMON",
                (mc, mkt, tcs, args) ->
                        new BasicTMON(new UnifiedTraderConfig(), tcs).run()),
        (config, tcsService) ->
                new BasicTMON(config, tcsService, new Config(), true, 1_000_000.0));
```

- [ ] **Step 3: Verify remaining strategies**

StrategyRegistry should now have:
- UnifiedStrategy (if not deleted)
- RegimeAwareStrategy
- BasicTMON
- Rebalance, RSX, DivFlow, IndicatorTrader, DataCollector, LevelTrader, OrderBookScalpStrategy, GenerateModel

- [ ] **Step 4: Compile and verify no errors**

Run: `./gradlew compileJava`
Expected: BUILD SUCCESSFUL

- [ ] **Step 5: Run backtest to verify strategies work**

Run: `./gradlew runBacktest -Pstrategy=RegimeAwareStrategy`
Expected: Backtest completes without errors

- [ ] **Step 6: Commit**

```bash
git add src/main/java/com/github/shk0da/goldendragon/strategy/StrategyRegistry.java
git commit -m "refactor: update StrategyRegistry after consolidation

- Remove TmonAveragingStrategy entry (replaced by BasicTMON)
- Add BasicTMON entry with monthly 100k deposit
- Remove RegimeAwareStrategyMl, PrecisionStrategy entries"
```

---

### Task 05: Update BacktestRunner references

**Files:**
- Modify: `src/main/java/com/github/shk0da/goldendragon/test/BacktestRunner.java`

- [ ] **Step 1: Search for TmonAveragingStrategy references**

Run: `grep -n "TmonAveragingStrategy" src/main/java/com/github/shk0da/goldendragon/test/BacktestRunner.java`
Expected: No references (or update to BasicTMON)

- [ ] **Step 2: Search for RegimeAwareStrategyMl references**

Run: `grep -n "RegimeAwareStrategyMl" src/main/java/com/github/shk0da/goldendragon/test/BacktestRunner.java`
Expected: No references

- [ ] **Step 3: Search for PrecisionStrategy references**

Run: `grep -n "PrecisionStrategy" src/main/java/com/github/shk0da/goldendragon/test/BacktestRunner.java`
Expected: No references

- [ ] **Step 4: Update strategy name in backtest config if needed**

If BacktestRunner references old strategy names, update to new names.

- [ ] **Step 5: Compile and verify no errors**

Run: `./gradlew compileJava`
Expected: BUILD SUCCESSFUL

- [ ] **Step 6: Commit**

```bash
git add src/main/java/com/github/shk0da/goldendragon/test/BacktestRunner.java
git commit -m "refactor: update BacktestRunner for strategy consolidation

- Update strategy name references (TmonAveragingStrategy → BasicTMON)
- Remove RegimeAwareStrategyMl, PrecisionStrategy references"
```

---

### Task 06: Final verification and cleanup

**Files:**
- All modified strategy files

- [ ] **Step 1: Full compilation**

Run: `./gradlew clean compileJava`
Expected: BUILD SUCCESSFUL

- [ ] **Step 2: Run all backtests**

Run: `./gradlew runBacktest` for each strategy:
- RegimeAwareStrategy
- BasicTMON
- UnifiedStrategy (if kept)

Expected: All backtests complete without errors

- [ ] **Step 3: Check for orphaned imports**

Run: `grep -r "import.*UnifiedStrategy" src/main/java --include="*.java"`
Expected: No results (or only in RegimeAwareStrategy if it extends UnifiedStrategy)

- [ ] **Step 4: Verify StrategyRegistry.backtestableNames()**

Check that backtestableNames() returns correct list:
- UnifiedStrategy (if kept)
- RegimeAwareStrategy
- BasicTMON

- [ ] **Step 5: Commit final cleanup**

```bash
git commit --allow-empty -m "chore: complete strategy consolidation

- Verified all strategies compile and run
- Removed orphaned imports and references
- Updated documentation if needed"
```

---

## Summary of Changes

**Deleted Files:**
- `src/main/java/com/github/shk0da/goldendragon/strategy/UnifiedStrategy.java`
- `src/main/java/com/github/shk0da/goldendragon/strategy/RegimeAwareStrategyMl.java`
- `src/main/java/com/github/shk0da/goldendragon/strategy/PrecisionStrategy.java`
- `src/main/java/com/github/shk0da/goldendragon/strategy/TmonAveragingStrategy.java`

**Created Files:**
- `src/main/java/com/github/shk0da/goldendragon/strategy/BasicTMON.java` (renamed from TmonAveragingStrategy)

**Modified Files:**
- `src/main/java/com/github/shk0da/goldendragon/strategy/RegimeAwareStrategy.java` (consolidated logic)
- `src/main/java/com/github/shk0da/goldendragon/strategy/StrategyRegistry.java` (updated entries)
- `src/main/java/com/github/shk0da/goldendragon/test/BacktestRunner.java` (updated references)

**Final Strategy List:**
1. **RegimeAwareStrategy** - Primary strategy with regime detection + unified trading
2. **BasicTMON** - Passive TMON@ accumulation with monthly 100k deposits
3. **UnifiedStrategy** - (Optional, if not deleted) Basic unified trading without regime detection
