package com.github.shk0da.goldendragon.strategy.orderbook;

import com.github.shk0da.goldendragon.utils.SerializationUtils;
import com.google.gson.reflect.TypeToken;
import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

/**
 * Persistent store of tracked open positions keyed by ticker.
 *
 * <p>Persists the tracking state to disk so that a strategy restart keeps the positions that are
 * still open on the exchange side instead of losing them. Persistence is best-effort: a failure to
 * read or write the file is logged and does not stop trading.
 */
public final class OrderBookPositionStore {

  private static final TypeToken<Map<String, OrderBookTradingEngine.PositionState>>
      POSITION_STATES_TOKEN = new TypeToken<Map<String, OrderBookTradingEngine.PositionState>>() {};

  private final String filePath;

  public OrderBookPositionStore(String filePath) {
    this.filePath = filePath;
  }

  public Map<String, OrderBookTradingEngine.PositionState> load() {
    try {
      Map<String, OrderBookTradingEngine.PositionState> states =
          SerializationUtils.loadDataFromDisk(filePath, POSITION_STATES_TOKEN);
      return states != null ? states : new HashMap<>();
    } catch (Exception ex) {
      System.out.println(
          "WARN: cannot load tracked positions from " + filePath + ": " + ex.getMessage());
      return new HashMap<>();
    }
  }

  public void save(String ticker, OrderBookTradingEngine.PositionState state) {
    Map<String, OrderBookTradingEngine.PositionState> states = load();
    states.put(ticker, state);
    write(states);
  }

  public void remove(String ticker) {
    Map<String, OrderBookTradingEngine.PositionState> states = load();
    if (states.remove(ticker) != null) {
      write(states);
    }
  }

  private void write(Map<String, OrderBookTradingEngine.PositionState> states) {
    try {
      File file = new File(filePath);
      File parent = file.getParentFile();
      if (parent != null && !parent.exists() && !parent.mkdirs()) {
        System.out.println("WARN: cannot create directory " + parent);
        return;
      }
      SerializationUtils.saveDataToDisk(filePath, states);
    } catch (IOException ex) {
      System.out.println("WARN: cannot persist positions to " + filePath + ": " + ex.getMessage());
    }
  }
}
