package com.github.shk0da.GoldenDragon.repository;

import java.util.Map;

public interface Repository<ID, T> {
    T getById(ID id);

    T getById(ID id, T defaultValue);

    T insert(ID id, T value);

    void putAll(Map<ID, T> values);

    boolean containsKey(ID key);
}
