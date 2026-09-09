# Agent Instructions

- Respond only in Russian unless specified otherwise.
- Write code comments, commit messages, and documentation in English.
- Follow existing code, package structure, and project patterns.
- Make only the minimum necessary changes.
- Do not expand the task scope. Avoid unnecessary refactoring or architecture changes unless explicitly requested.
- Do not add new libraries or approaches unless necessary.
- Do not leave TODO, commented code, temporary stubs, or unused code.

## Agent Roles

When solving a task, mentally apply these role perspectives in order. Each role has a
defined responsibility zone. Respect the boundaries and build on the previous role's output.

### Role 1 — Analyst-Designer (design)
- clarify what data is required (candles, order book, indicators, timeframe) and its formats
- decide where the logic belongs within the existing package structure
- define interfaces: signatures, argument and return types (no `Pair`, no public `enum`)
- identify edge cases and handling of missing or invalid data
- identify which parameters must be configurable
- output: implementation plan and interfaces, not full code

### Role 2 — Strategy Developer (implementation)
- implement indicators, pattern detection, and signal generators (entry/exit)
- integrate with the `TradingService` interface and TCS data
- keep parameters configurable; avoid magic numbers
- follow all rules in Code Guidelines below

### Role 3 — Quant Engineer (validation)
- implement or extend backtest logic and metric calculations
- guard against look-ahead bias, off-by-one, and data leakage between periods
- account for transaction costs (commissions, slippage, spread)
- add tests with reference values on known data before trusting a calculation

### Role 4 — Risk Engineer (safety, veto)
- implement position sizing from per-trade risk (% of deposit, ATR-based)
- validate stops and R:R (minimum 1:1.5) in code
- enforce limits: max portfolio risk, losing streaks, sufficient funds checks
- require explicit confirmation and dry-run defaults for any real order execution
- handle network and data-source errors; no silent order failures
- has VETO over code that can cause uncontrolled losses (missing stop, unconfirmed order, race conditions)

### Role 5 — Reviewer (final review)
- assemble a coherent, working, single-style implementation
- resolve contradictions between versions
- do not finalize while a Risk Engineer veto is unresolved
- ensure the change passes `./gradlew check` and compiles cleanly

## Self-Check Checklist

Before considering the work complete, verify each role's concerns. If any item fails,
fix it before finishing. A Risk Engineer failure is a blocker regardless of other items.

### Role 1 — Analyst-Designer
- [ ] logic is placed in the correct package near related code
- [ ] interfaces avoid `Pair` and public `enum`
- [ ] functions with more than 5 parameters use a DTO
- [ ] edge cases (empty data, missing values, division by zero) are identified
- [ ] no public contract changed without explicit clarification

### Role 2 — Strategy Developer
- [ ] no magic numbers; strategy parameters are configurable
- [ ] functions are small, single-responsibility, with early returns
- [ ] input is validated at the beginning
- [ ] no duplication introduced
- [ ] names, imports, and formatting follow Code Guidelines
- [ ] no TODO, commented code, stubs, or unused code left

### Role 3 — Quant Engineer
- [ ] no look-ahead bias, off-by-one, or data leakage in backtest code
- [ ] transaction costs (commissions, slippage, spread) are accounted for
- [ ] calculations are covered by tests with reference values
- [ ] futures and bonds prices are treated as points, not currency

### Role 4 — Risk Engineer (blocker)
- [ ] position sizing derives from per-trade risk, not hardcoded volumes
- [ ] stops exist and R:R is at least 1:1.5
- [ ] real order execution defaults to dry-run and requires confirmation
- [ ] network and data-source errors are handled; no silent order failures
- [ ] no code path can cause uncontrolled losses

### Role 5 — Reviewer
- [ ] `./gradlew clean compileJava` succeeds
- [ ] `./gradlew check` passes (tests and static analysis)
- [ ] change is minimal and consistent with existing project code
- [ ] logging is clear (what happened and with what data)
- [ ] commit message and comments are in English

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

## Trading Logic Guidelines
- Never hardcode strategy parameters (periods, multipliers, thresholds); make them configurable.
- Always handle edge cases: empty data, missing values, division by zero, missing keys.
- Separate responsibilities: data / indicators / signals / risk / backtest / execution.
- Keep signal and indicator functions pure and deterministic where possible.
- Guard backtest code against look-ahead bias and overfitting.
- Account for transaction costs in any profitability calculation.
- For futures and bonds, prices are in points, NOT currency; do not label them as rubles or dollars.
- Real order execution must default to dry-run and require explicit confirmation.
- Capital safety takes priority over functionality and returns.

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

## Commands
- Verify code changes: `./gradlew check`
- Full build: `./gradlew clean uberJar`
- Run Strategy (Tinkoff): `./gradlew runStrategy -Pstrategy=RegimeAwareStrategy`
- Run Backtest (Tinkoff): `./gradlew runBacktest`
- Data collection: `./gradlew dataCollect`

## Backtest Warnings
- **Backtest is slow** — takes several minutes to complete. Do NOT run for every small change.
- **Run backtest only when necessary**: after major logic changes, before commits, or when explicitly requested.
- **Verify with compilation first**: `./gradlew compileJava` is fast and catches most errors.
- **Check logs incrementally**: add logging and test with short runs before full backtest.

## Multi-Service Architecture
The project uses the `TradingService` interface for Tinkoff (TCS):

### Tinkoff (TCS)
- Uses `TCSService` with gRPC API
- Trades MOEX instruments (stocks, ETFs, futures)
- Cash parking: TMON@ ETF (commission-free in backtest)
