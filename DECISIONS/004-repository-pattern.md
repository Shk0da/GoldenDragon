# ADR 004: Repository Pattern for In-Memory Storage

**Date:** 2024-01-01  
**Status:** Accepted  
**Context:** Data Storage and Caching

## Context

We need fast, thread-safe access to:
- Ticker metadata (FIGI, ISIN, currency, type)
- FIGI mappings (ticker → FIGI)
- Current prices (order book data)

Requirements:
- Fast lookups during trading
- Thread-safe (parallel ticker processing)
- Simple API
- Persistence across restarts (optional)

## Decision

Use **Repository Pattern** with `ConcurrentHashMap`:

```java
public interface Repository<ID, T> {
    T getById(ID id);
    T insert(ID id, T value);
    void putAll(Map<ID, T> values);
    boolean containsKey(ID key);
    Map<ID, T> getAll();
}

public abstract class AbstractRepository<ID, T> implements Repository<ID, T> {
    private final Map<ID, T> register = new ConcurrentHashMap<>();
    
    // Implementation
}
```

Concrete repositories:

```java
public class TickerRepository extends AbstractRepository<TickerInfo.Key, TickerInfo> {
    public static final TickerRepository INSTANCE = new TickerRepository();
    
    public TickerRepository() {
        // Load from disk on startup
        Map<Key, TickerInfo> data = loadDataFromDisk("tickers.json");
        if (data != null) {
            putAll(data);
        }
    }
}
```

Singleton pattern for global access:

```java
TickerInfo ticker = TickerRepository.INSTANCE.getById(key);
```

## Consequences

### Positive
- **Fast lookups**: O(1) HashMap access
- **Thread-safe**: ConcurrentHashMap handles concurrency
- **Simple API**: Easy to use and understand
- **Persistence**: Can save/load to disk

### Negative
- **Memory**: All data in RAM (not suitable for large datasets)
- **No queries**: Simple key-value only
- **Singleton**: Global state can be hard to test

### Trade-offs
- Chose in-memory over database for speed
- Could add database backend if data grows large
- Singleton for simplicity, could use DI if needed

## Compliance

All repositories must:
1. Extend `AbstractRepository`
2. Be singleton (`INSTANCE`)
3. Use `ConcurrentHashMap` for storage
4. Support serialization to disk (optional)

## Usage Examples

```java
// Get ticker
TickerInfo ticker = TickerRepository.INSTANCE.getById(key);

// Insert new data
FigiRepository.INSTANCE.insert(key, figi);

// Bulk insert
tickerRepository.putAll(tcsService.getStockList());

// Check existence
if (tickerRepository.containsKey(key)) {
    // ...
}
```
