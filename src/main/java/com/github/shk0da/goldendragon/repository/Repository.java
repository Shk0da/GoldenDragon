package com.github.shk0da.goldendragon.repository;

import java.util.Map;

/**
 * Generic repository interface for key-value storage. Provides basic CRUD operations for in-memory
 * data storage.
 *
 * @param <ID> the type of the identifier
 * @param <T> the type of the stored element
 */
public interface Repository<ID, T> {
    T getById(ID id);

    T getById(ID id, T defaultValue);

    T insert(ID id, T value);

    void putAll(Map<ID, T> values);

    boolean containsKey(ID key);

    Map<ID, T> getAll();
}
