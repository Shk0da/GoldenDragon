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
- Run `./gradlew spotlessCheck` to verify code formatting matches IDEA style
- Run `./gradlew pmdMain` to verify code quality rules
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
- Base package: `com.github.shk0da.GoldenDragon.*`
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
- Run backtest strategies: `./gradlew clean runBacktest`
- Full build: `./gradlew clean uberJar`
- Run Strategy: `./gradlew runStrategy -Pstrategy=UnifiedStrategy`

### ML Training Commands
- Train ML model (default params): `./gradlew runMlTraining`
- Train with custom data: `./gradlew runMlTraining -Pdata=ml_strategy/data_pipeline/trades.csv`
- Train with custom output: `./gradlew runMlTraining -Poutput=ml_strategy/models/model.txt`
- Train for specific ticker: `./gradlew runMlTraining -Pticker=SBER`
- Train models for all tickers: `./gradlew trainAllTickers`
- Full ML pipeline (backtest + train): `./gradlew generateModel`