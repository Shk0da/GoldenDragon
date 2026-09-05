# Agent Instructions

- Respond only in Russian unless specified otherwise.
- Write code comments, commit messages, and documentation in English.
- Follow existing code, package structure, and project patterns.
- Make only the minimum necessary changes.
- Do not expand the task scope. Avoid unnecessary refactoring or architecture changes unless explicitly requested.
- Do not add new libraries or approaches unless necessary.
- Do not leave TODO, commented code, temporary stubs, or unused code.

## Project Context
- Java 11

## Priorities
1. Correctness and safety
2. Consistency with existing project code
3. Passing tests and static analysis (e.g., detekt)
4. Simplicity and readability
5. Minimal changes

## Code Verification
- Always run `./gradlew check` after making code changes to ensure tests pass
- Run `./gradlew clean compileJava` to verify compilation before committing
- Fix any compilation errors or test failures before considering work complete

## Code Guidelines
- Use precise and meaningful names.
- Write small functions with a single responsibility.
- Minimize nesting; prefer early returns.
- Validate input at the beginning.
- Avoid duplication.
- Do not use `Pair` in arguments or return types.
- If a function has more than 5 parameters, extract them into a DTO.
- Do not use `enum` in public contracts.
- Use named arguments where applicable.
- Use `CONST == value` style for comparisons.
- Use only full imports (no wildcards unless required).
- Leave a blank line after class declarations.
- Use at most one blank line between fields and functions.
- In multi-line signatures, place a comma after each argument.

## Logging
- Write clear log messages: what happened and with what data.
- Avoid meaningless messages like `Error` or `Failed` without context.

### Structure
- Base package: `com.github.shk0da.goldendragon.*`
- Place new files near logically related code.

## Clarify the Task If
- It is unclear where the logic should reside.
- You need to change a public contract.
- There are multiple possible business behaviors.
- There is insufficient data for a correct implementation.

## Text Formatting
- Headings: capitalize first letter, no period at the end.
- Sentences: capitalize first letter, end with a period.
- Lists: start with lowercase, no period at the end.
- Ordinary comments in code: start with lowercase, no period at the end.

## Log Analysis
- When asked to analyze logs, diagnostics, or metrics — always use `python scripts/analyze_strategy.py` instead of reading raw log files.
- The script parses `orderbook-metrics.csv` and `orderbook-diagnostics-replay.log` into a structured report.
- For custom paths: `python scripts/analyze_strategy.py --csv path/to/metrics.csv --log path/to/diagnostics.log`

## Commands
- Verify code changes: `./gradlew check`
- Full build: `./gradlew clean uberJar`
- Run Strategy (Tinkoff): `./gradlew runStrategy -Pstrategy=RegimeAwareStrategy`
- Run Strategy (ByBit): `./gradlew runStrategy -Pstrategy=RegimeAwareStrategy -Dtrading.service=BYBIT`
- Run Backtest (Tinkoff): `./gradlew runBacktest`
- Run Backtest (ByBit): `./gradlew runBacktest -Dtrading.service=BYBIT`
- Data collection: `./gradlew dataCollect`
- Analyze strategy logs: `python scripts/analyze_strategy.py`

## Multi-Service Architecture

The project supports two trading services via the `TradingService` interface:

### Tinkoff (TCS)
- Uses `TCSService` with gRPC API
- Trades MOEX instruments (stocks, ETFs, futures)
- Cash parking: TMON@ ETF (commission-free in backtest)
- Service type: `TradingServiceType.TINKOFF`

### ByBit (Crypto)
- Uses `ByBitService` with REST API
- Trades USDT perpetual contracts
- Cash parking: SPYUSDT (commission applied in backtest)
- Service type: `TradingServiceType.BYBIT`

Switch service via:
- System property: `-Dtrading.service=BYBIT`
- Config property: `trading.service=BYBIT` in `application.properties`

## Backtest Behavior

BacktestRunner automatically filters tickers based on `trading.service`:
- **TINKOFF**: only tickers from `datacollector.instruments`
- **BYBIT**: only tickers from `datacollector.crypto` (USDT pairs)

SPYUSDT is automatically selected as parking ticker when crypto is enabled.

## ByBit Strategy Differences

Different strategies use different ticker selection approaches for ByBit:

- **RegimeAwareStrategy / UnifiedStrategy**: Uses tickers from `datacollector.crypto` (whitelist)
- **OrderBookScalpStrategy**: Screens all available USDT perpetuals and selects the most liquid ones based on spread, depth, and trade flow

## ByBit 24/7 Trading

For ByBit crypto trading, you can enable 24/7 trading (no working hours restriction):
- Config property: `unifiedTrader.bybit24h=true` in `application.properties`
- When enabled: trading allowed on weekends and outside MOEX hours (08:30-21:00)
- Default: `false` (follows Tinkoff working hours)

## Logging
- Write clear log messages: what happened and with what data.
- Avoid meaningless messages like `Error` or `Failed` without context.

### Structure
- Base package: `com.github.shk0da.goldendragon.*`
- Place new files near logically related code.

## Clarify the Task If
- It is unclear where the logic should reside.
- You need to change a public contract.
- There are multiple possible business behaviors.
- There is insufficient data for a correct implementation.

## Text Formatting
- Headings: capitalize first letter, no period at the end.
- Sentences: capitalize first letter, end with a period.
- Lists: start with lowercase, no period at the end.
- Ordinary comments in code: start with lowercase, no period at the end.

## Log Analysis
- When asked to analyze logs, diagnostics, or metrics — always use `python scripts/analyze_strategy.py` instead of reading raw log files.
- The script parses `orderbook-metrics.csv` and `orderbook-diagnostics-replay.log` into a structured report.
- For custom paths: `python scripts/analyze_strategy.py --csv path/to/metrics.csv --log path/to/diagnostics.log`
