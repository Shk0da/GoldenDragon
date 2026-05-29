package com.github.shk0da.GoldenDragon.strategy.turtle;

import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

/**
 * Turtle risk manager.
 * Controls position limits and capital allocation.
 */
public class TurtleRiskManager {

    private final int maxPositions;
    private final double maxCapitalAtRisk;
    private final ConcurrentMap<String, Double> positionRisk = new ConcurrentHashMap<>();

    /**
     * Create Turtle risk manager.
     *
     * @param maxPositions maximum concurrent positions (e.g., 8)
     * @param maxCapitalAtRisk maximum total capital at risk as decimal (e.g., 0.20 for 20%)
     */
    public TurtleRiskManager(int maxPositions, double maxCapitalAtRisk) {
        this.maxPositions = maxPositions;
        this.maxCapitalAtRisk = maxCapitalAtRisk;
    }

    /**
     * Check if can open new position.
     *
     * @param ticker ticker symbol
     * @param currentPositions current number of open positions
     * @param totalCapital total portfolio capital
     * @param positionRiskAmount risk amount for this position
     * @return true if can open position
     */
    public boolean canOpenPosition(String ticker, int currentPositions, double totalCapital, double positionRiskAmount) {
        // Check if already have position in this ticker
        if (positionRisk.containsKey(ticker)) {
            return false; // Already have position
        }

        // Check max positions limit
        if (currentPositions >= maxPositions) {
            return false;
        }

        // Check total capital at risk
        double totalRisk = getTotalRisk() + positionRiskAmount;
        if (totalRisk > totalCapital * maxCapitalAtRisk) {
            return false;
        }

        return true;
    }

    /**
     * Register position open.
     *
     * @param ticker ticker symbol
     * @param riskAmount risk amount for this position
     */
    public void registerPositionOpen(String ticker, double riskAmount) {
        positionRisk.put(ticker, riskAmount);
    }

    /**
     * Register position close.
     *
     * @param ticker ticker symbol
     */
    public void registerPositionClose(String ticker) {
        positionRisk.remove(ticker);
    }

    /**
     * Get total risk across all positions.
     */
    private double getTotalRisk() {
        double total = 0.0;
        for (double risk : positionRisk.values()) {
            total += risk;
        }
        return total;
    }

    /**
     * Get current position count.
     */
    public int getCurrentPositionCount() {
        return positionRisk.size();
    }

    /**
     * Get max positions limit.
     */
    public int getMaxPositions() {
        return maxPositions;
    }

    /**
     * Get max capital at risk.
     */
    public double getMaxCapitalAtRisk() {
        return maxCapitalAtRisk;
    }
}
