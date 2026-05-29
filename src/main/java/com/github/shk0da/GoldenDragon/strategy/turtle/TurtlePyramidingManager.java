package com.github.shk0da.GoldenDragon.strategy.turtle;

import com.github.shk0da.GoldenDragon.model.Position;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

/**
 * Turtle pyramiding manager.
 * Manages adding units to profitable positions.
 */
public class TurtlePyramidingManager {

    private final int maxUnits;
    private final double addUnitAtr;
    private final ConcurrentMap<String, UnitState> unitStates = new ConcurrentHashMap<>();

    /**
     * Unit state for tracking pyramiding.
     */
    public static class UnitState {
        public final int units;
        public final double avgEntry;
        public final double lastAdditionPrice;

        public UnitState(int units, double avgEntry, double lastAdditionPrice) {
            this.units = units;
            this.avgEntry = avgEntry;
            this.lastAdditionPrice = lastAdditionPrice;
        }
    }

    /**
     * Create Turtle pyramiding manager.
     *
     * @param maxUnits maximum units per position (e.g., 4)
     * @param addUnitAtr add unit after price moves this many ATR in profit (e.g., 1.0)
     */
    public TurtlePyramidingManager(int maxUnits, double addUnitAtr) {
        this.maxUnits = maxUnits;
        this.addUnitAtr = addUnitAtr;
    }

    /**
     * Check if can add unit to position.
     *
     * @param ticker ticker symbol
     * @param position current position
     * @param currentPrice current price
     * @param atr current ATR value
     * @return true if can add unit
     */
    public boolean canAddUnit(String ticker, Position position, double currentPrice, double atr) {
        if (position == null || position.quantity <= 0) {
            return false;
        }

        UnitState state = unitStates.get(ticker);
        if (state == null) {
            return false; // No state tracked, treat as initial position
        }

        // Check max units
        if (state.units >= maxUnits) {
            return false;
        }

        // Check if price moved enough in profit
        if (atr <= 0) {
            return false;
        }

        double profitPerUnit = currentPrice - state.lastAdditionPrice;
        return profitPerUnit >= (atr * addUnitAtr);
    }

    /**
     * Add unit to position.
     *
     * @param ticker ticker symbol
     * @param currentPrice entry price for new unit
     * @param unitSize size of new unit
     * @return updated unit state
     */
    public UnitState addUnit(String ticker, double currentPrice, int unitSize) {
        UnitState currentState = unitStates.get(ticker);

        if (currentState == null) {
            // Initial position
            UnitState newState = new UnitState(1, currentPrice, currentPrice);
            unitStates.put(ticker, newState);
            return newState;
        }

        // Calculate new average entry
        int newUnits = currentState.units + 1;
        double totalValue = (currentState.avgEntry * currentState.units) + (currentPrice * unitSize);
        double newAvgEntry = totalValue / newUnits;

        UnitState newState = new UnitState(newUnits, newAvgEntry, currentPrice);
        unitStates.put(ticker, newState);
        return newState;
    }

    /**
     * Remove position state (on close).
     *
     * @param ticker ticker symbol
     */
    public void removePosition(String ticker) {
        unitStates.remove(ticker);
    }

    /**
     * Get current unit state for ticker.
     */
    public UnitState getUnitState(String ticker) {
        return unitStates.get(ticker);
    }

    /**
     * Get max units per position.
     */
    public int getMaxUnits() {
        return maxUnits;
    }

    /**
     * Get ATR units for adding position.
     */
    public double getAddUnitAtr() {
        return addUnitAtr;
    }
}
