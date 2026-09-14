package com.github.shk0da.goldendragon.money;

import com.github.shk0da.goldendragon.market.MarketDataProvider;
import com.github.shk0da.goldendragon.model.Position;
import com.github.shk0da.goldendragon.model.TickerInfo;
import com.github.shk0da.goldendragon.service.TradingService;
import com.github.shk0da.goldendragon.utils.LoggingUtils;

import java.util.Map;

/**
 * Monitors free cash and automatically buys TMON@ ETF when idle cash is available.
 * Runs in a separate thread with configurable monitoring interval.
 * This monitor works independently from the main trading loop and focuses solely on
 * parking idle cash into TMON@ ETF to earn returns on uninvested capital.
 */
public class TmonCashParkingMonitor implements Runnable {

    private static final long MONITORING_INTERVAL_MS = 5 * 60 * 1000; // 5 minutes

    private final TradingService tradingService;
    private final MarketDataProvider marketDataProvider;
    private final CashParkingManager cashParkingManager;
    private final Map<String, Position> positionStore;
    private volatile boolean running = true;
    private volatile boolean tradingInProgress = false;

    public TmonCashParkingMonitor(
            TradingService tradingService,
            MarketDataProvider marketDataProvider,
            CashParkingManager cashParkingManager,
            Map<String, Position> positionStore) {
        this.tradingService = tradingService;
        this.marketDataProvider = marketDataProvider;
        this.cashParkingManager = cashParkingManager;
        this.positionStore = positionStore;
    }

    /**
     * Set trading in progress flag to prevent monitor from buying during active trading.
     */
    public void setTradingInProgress(boolean tradingInProgress) {
        this.tradingInProgress = tradingInProgress;
    }

    @Override
    public void run() {
        LoggingUtils.log("TMON_MONITOR: Started with interval " + (MONITORING_INTERVAL_MS / 1000) + "s");

        // Initial delay to allow positions to be restored before first check
        try {
            Thread.sleep(10_000);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            return;
        }

        while (running) {
            try {
                monitorAndBuyTmon();
                Thread.sleep(MONITORING_INTERVAL_MS);
            } catch (InterruptedException e) {
                LoggingUtils.log("TMON_MONITOR: Interrupted, stopping");
                Thread.currentThread().interrupt();
                break;
            } catch (Exception e) {
                LoggingUtils.log("TMON_MONITOR: Error - " + e.getMessage());
                try {
                    Thread.sleep(MONITORING_INTERVAL_MS);
                } catch (InterruptedException ie) {
                    Thread.currentThread().interrupt();
                    break;
                }
            }
        }

        LoggingUtils.log("TMON_MONITOR: Stopped");
    }

    /**
     * Main monitoring logic: check for free cash and buy TMON@ if available.
     */
    void monitorAndBuyTmon() {
        if (!cashParkingManager.isParkingEnabled()) {
            return;
        }

        String parkingTicker = cashParkingManager.getParkingTicker();
        if (parkingTicker == null) {
            return;
        }

        // Skip if strategy is actively trading
        if (tradingInProgress) {
            return;
        }

        try {
            // Skip parking if there are active non-parking positions (cash needed for trading)
            if (hasActiveNonParkingPositions()) {
                return;
            }

            // Get available cash from trading service
            double availableCash = getAvailableCash();
            if (availableCash <= 0) {
                return;
            }

            // Get TMON ticker info
            TickerInfo.Key parkingKey = new TickerInfo.Key(parkingTicker, cashParkingManager.getParkingTickerType());
            TickerInfo parkingTickerInfo = tradingService != null
                    ? tradingService.searchTicker(parkingKey)
                    : null;

            if (parkingTickerInfo == null) {
                LoggingUtils.log("TMON_MONITOR: Ticker info not found for " + parkingTicker);
                return;
            }

            int lot = parkingTickerInfo.getLot() != null ? parkingTickerInfo.getLot() : 1;
            Double currentPrice = getCurrentPrice(parkingTicker);

            if (currentPrice == null || currentPrice <= 0) {
                LoggingUtils.log("TMON_MONITOR: Cannot get current price for " + parkingTicker);
                return;
            }

            // Calculate how many lots we can buy (use 95% of available cash to avoid insufficient funds error)
            double usableCash = availableCash * 0.95;
            double effectivePrice = currentPrice * 1.01;
            double effectiveCostPerLot = effectivePrice * lot;

            if (usableCash < effectiveCostPerLot) {
                // Not enough cash to buy even one lot
                return;
            }

            int buyLots = (int) Math.floor(usableCash / effectiveCostPerLot);
            if (buyLots <= 0) {
                return;
            }

            // Execute purchase
            double totalCost = buyLots * currentPrice * lot;

            if (tradingService != null) {
                // Re-check capital to guard against race with strategy parking path
                double freshCash = getAvailableCash();
                if (freshCash < totalCost) {
                    LoggingUtils.log(
                            "TMON_MONITOR: Insufficient capital for " + parkingTicker
                                    + " buy (need=" + String.format("%.2f", totalCost)
                                    + ", have=" + String.format("%.2f", freshCash) + "), skipping");
                    return;
                }

                tradingService.buyByMarketWithDetails(
                        parkingTicker,
                        cashParkingManager.getParkingTickerType(),
                        totalCost,
                        0.0,
                        0.0);
                LoggingUtils.log(
                        "TMON_MONITOR: Bought " + parkingTicker + " qty=" + (buyLots * lot)
                                + " (lots=" + buyLots + ") value=" + String.format("%.2f", totalCost)
                                + " (available cash was " + String.format("%.2f", availableCash) + ")");
            }

        } catch (Exception e) {
            LoggingUtils.log("TMON_MONITOR: Failed to buy TMON - " + e.getMessage());
        }
    }

    /**
     * Get available cash from trading service.
     */
    private double getAvailableCash() {
        if (tradingService != null) {
            try {
                return tradingService.getAvailableCash();
            } catch (Exception e) {
                LoggingUtils.log("TMON_MONITOR: Failed to get available cash - " + e.getMessage());
                return 0.0;
            }
        }
        return 0.0;
    }

    /**
     * Get current ask price for parking ticker from the order book.
     * Uses only the ask side (unlike getLivePrices which requires both ask and bid).
     */
    private Double getCurrentPrice(String ticker) {
        if (tradingService != null) {
            try {
                TickerInfo.Key key = new TickerInfo.Key(
                        ticker, cashParkingManager.getParkingTickerType());
                double price = tradingService.getAvailablePrice(key, 1, "asks", false);
                if (price > 0) {
                    return price;
                }
            } catch (Exception e) {
                // Ignore, will return null
            }
        }
        return null;
    }

    /**
     * Stop the monitoring thread.
     */
    public void stop() {
        running = false;
    }

    /**
     * Check if monitor is running.
     */
    public boolean isRunning() {
        return running;
    }

    /**
     * Check if there are active positions (excluding parking ticker).
     * Used to prevent parking when cash is needed for trading.
     */
    private boolean hasActiveNonParkingPositions() {
        for (Map.Entry<String, Position> entry : positionStore.entrySet()) {
            if (cashParkingManager.isParkingTicker(entry.getKey())) {
                continue;
            }
            if (entry.getValue().quantity > 0) {
                return true;
            }
        }
        return false;
    }
}
