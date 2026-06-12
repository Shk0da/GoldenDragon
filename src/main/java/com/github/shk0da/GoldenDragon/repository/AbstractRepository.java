package com.github.shk0da.goldendragon.repository;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Abstract thread-safe repository implementation using ConcurrentHashMap. Provides default
 * implementation for all Repository interface methods.
 *
 * @param <ID> the type of the identifier
 * @param <T> the type of the stored element
 */
public abstract class AbstractRepository<ID, T> implements Repository<ID, T> {

  private final Map<ID, T> register = new ConcurrentHashMap<>();

  public T getById(ID id) {
    return register.get(id);
  }

  public T getById(ID id, T defaultValue) {
    return register.getOrDefault(id, defaultValue);
  }

  public T insert(ID id, T value) {
    return register.put(id, value);
  }

  @Override
  public void putAll(Map<ID, T> values) {
    register.putAll(values);
  }

  @Override
  public boolean containsKey(ID key) {
    return register.containsKey(key);
  }

  @Override
  public Map<ID, T> getAll() {
    return Map.copyOf(register);
  }
}
