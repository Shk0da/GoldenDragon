# Регрессионное тестирование

Этот документ описывает систему регрессионного тестирования проекта GoldenDragon.

## Обзор

Система регрессионного тестирования гарантирует, что новые изменения не ломают существующую функциональность.

## Уровни тестирования

### 1. Unit Tests (Модульные тесты)

**Цель:** Проверка отдельных компонентов изолированно.

**Запуск:**
```bash
./gradlew test
```

**Покрытие:**
- Money Management (SizingStrategy, RiskManager, StopLossManager)
- Filters (MarketRegimeFilter, BadWeatherFilter, GroupConfirmationFilter)
- Services (TradingService, TCSService, TradingServiceCache)
- Strategies (BaseStrategy, UnifiedStrategy, TradeCouncilStrategy)
- Utilities (TinkoffApiUrlResolver, SerializationUtils)

**Требования к покрытию:**
- Минимум 60% overall coverage
- Минимум 70% для критических пакетов (money, strategy, service)

### 2. Integration Tests (Интеграционные тесты)

**Цель:** Проверка взаимодействия между компонентами.

**Запуск:**
```bash
./gradlew test --tests "*Integration*"
```

**Тесты:**
- `TradeCouncilStrategyFuturesIntegrationTest` — futures margin calculation
- `TradingServiceCacheTest` — кэширование API вызовов
- `BaseStrategyOpenPositionTest` — открытие позиций

### 3. Regression Tests (Регрессионные тесты)

**Цель:** Проверка что бэктест работает и показывает ожидаемые метрики.

**Запуск:**
```bash
./gradlew runBacktest
```

**Проверки:**
- Бэктест завершается без ошибок
- Генерируется equity curve chart
- Метрики в допустимых пределах (PnL > 0, Win Rate > 40%)

### 4. Performance Tests (Тесты производительности)

**Цель:** Проверка что производительность не деградировала.

**Запуск:**
```bash
./gradlew runBacktest -Pverbose=false
```

**Метрики:**
- Время выполнения бэктеста < 10 минут
- Потребление памяти < 2GB

## CI/CD Pipeline

### GitHub Actions Workflow

Файл: `.github/workflows/ci.yml`

**Джобы:**

1. **unit-tests** — запуск unit тестов с coverage
   - JDK 11
   - JaCoCo coverage report
   - Артефакты: coverage report, test results

2. **regression** — запуск бэктестов
   - Зависит от unit-tests
   - Таймаут: 30 минут
   - Проверка: equity chart сгенерирован

3. **security** — security scan
   - OWASP dependency check
   - Проверка на hardcoded secrets

### Запуск локально

```bash
# Все тесты
./gradlew check

# Только unit тесты
./gradlew test

# Coverage report
./gradlew jacocoTestReport

# Проверка coverage thresholds
./gradlew jacocoTestCoverageVerification

# Бэктест
./gradlew runBacktest

# Security scan
./gradlew dependencyCheckAnalyze
```

## Новые тесты

### Добавление нового теста

1. Создать тест в `src/test/java/com/github/shk0da/goldendragon/<package>/`
2. Использовать JUnit 5 + AssertJ
3. Следовать naming convention: `<Component>Test.java`

### Пример теста

```java
@DisplayName("ComponentName")
class ComponentNameTest {

    @Nested
    @DisplayName("methodName")
    class MethodName {

        @Test
        @DisplayName("Should do something when condition")
        void shouldDoSomething() {
            // Given
            // When
            // Then
        }
    }
}
```

## Regression Checklist

Перед каждым коммитом:

- [ ] `./gradlew check` — все тесты проходят
- [ ] `./gradlew jacocoTestCoverageVerification` — coverage в норме
- [ ] `./gradlew runBacktest` — бэктест работает
- [ ] Новые тесты для новой функциональности добавлены

## Troubleshooting

### Тесты падают

1. Проверить логи: `build/reports/tests/test/index.html`
2. Запустить конкретный тест: `./gradlew test --tests TestClassName`
3. Проверить что тесты изолированы (нет shared state)

### Coverage ниже порога

1. Запустить report: `./gradlew jacocoTestReport`
2. Открыть `build/reports/jacoco/test/html/index.html`
3. Добавить тесты для непокрытых строк

### Бэктест не проходит

1. Проверить данные: `data/` директория
2. Проверить конфигурацию: `application.properties`
3. Запустить с verbose: `./gradlew runBacktest -Pverbose=true`

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0 | 2026-09-16 | Initial regression testing setup |
