package com.github.shk0da.goldendragon.money;

import com.github.shk0da.goldendragon.market.MarketDataProvider;
import com.github.shk0da.goldendragon.model.Position;
import com.github.shk0da.goldendragon.model.TickerInfo;
import com.github.shk0da.goldendragon.model.TickerType;
import com.github.shk0da.goldendragon.service.TradingService;
import com.github.shk0da.goldendragon.utils.LoggingUtils;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Manages cash parking operations across different trading services.
 * <p>
 * For TINKOFF: uses TMON@ (ETF) for cash parking.
 * For BYBIT: cash parking is disabled (no parking ticker).
 * </p>
 */
public class CashParkingManager {

    private static final String TINKOFF_PARKING_TICKER = "TMON@";
    private static final TickerType TINKOFF_PARKING_TYPE = TickerType.ETF;

    private final TradingService tradingService;
    private final MarketDataProvider marketDataProvider;
    private final Map<String, Position> positionStore;

    public CashParkingManager(
            TradingService tradingService,
            MarketDataProvider marketDataProvider,
            Map<String, Position> positionStore) {
        this.tradingService = tradingService;
        this.marketDataProvider = marketDataProvider;
        this.positionStore = positionStore != null ? positionStore : new ConcurrentHashMap<>();
    }

    /**
     * Get the parking ticker based on TradingService type.
     * @return "TMON@" for TINKOFF, null for BYBIT (cash parking disabled)
     */
    public String getParkingTicker() {
        if (tradingService != null && tradingService.getServiceType() == TradingService.TradingServiceType.BYBIT) {
            return null;
        }
        return TINKOFF_PARKING_TICKER;
    }

    /**
     * Get the parking ticker type based on TradingService type.
     * @return ETF for TINKOFF, null for BYBIT (cash parking disabled)
     */
    public TickerType getParkingTickerType() {
        if (tradingService != null && tradingService.getServiceType() == TradingService.TradingServiceType.BYBIT) {
            return null;
        }
        return TINKOFF_PARKING_TYPE;
    }

    /**
     * Check if cash parking is enabled for the current trading service.
     * @return true for TINKOFF, false for BYBIT
     */
    public boolean isParkingEnabled() {
        return getParkingTicker() != null;
    }

    /**
     * Check if the given ticker is the parking ticker.
     */
    public boolean isParkingTicker(String ticker) {
        String parkingTicker = getParkingTicker();
        return parkingTicker != null && parkingTicker.equals(ticker);
    }

    /**
     * Get the current parking position from the broker.
     * @return PositionInfo or null if not found
     */
    public com.github.shk0da.goldendragon.model.PositionInfo getParkingPosition() {
        if (tradingService == null && marketDataProvider == null) {
            return null;
        }

        try {
            String parkingTicker = getParkingTicker();
            if (parkingTicker == null) {
                return null;
            }
            TickerType parkingType = getParkingTickerType();

            if (tradingService != null) {
                return tradingService.getCurrentPositions(parkingType, parkingTicker);
            } else {
                return marketDataProvider.getCurrentPositions(parkingType, parkingTicker);
            }
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * Get the parking position value (qty * price).
     * @return value or 0.0 if not found
     */
    public double getParkingValue() {
        com.github.shk0da.goldendragon.model.PositionInfo parkingInfo = getParkingPosition();
        if (parkingInfo == null || parkingInfo.getBalance() <= 0) {
            return 0.0;
        }

        double parkingQty = Math.abs(parkingInfo.getBalance());
        Double parkingPrice = parkingInfo.getAveragePositionPrice();
        if (parkingPrice == null || parkingPrice <= 0) {
            return 0.0;
        }

        return parkingQty * parkingPrice;
    }

    /**
     * Sell parking position to free cash for a trade.
     * @param cashToFree amount of cash to free
     * @param tradeTicker ticker for logging purposes
     */
    public void sellParkingToFreeCash(double cashToFree, String tradeTicker) {
        String parkingTicker = getParkingTicker();
        if (parkingTicker == null) {
            return;
        }
        TickerType parkingType = getParkingTickerType();

        try {
            com.github.shk0da.goldendragon.model.PositionInfo parkingInfo = getParkingPosition();
            if (parkingInfo == null || parkingInfo.getBalance() <= 0) {
                return;
            }

            int parkingQty = parkingInfo.getBalance();
            Double parkingPriceDouble = parkingInfo.getAveragePositionPrice();
            double parkingPrice = (parkingPriceDouble != null && parkingPriceDouble > 0)
                    ? parkingPriceDouble
                    : cashToFree / parkingQty;

            int parkingLots = parkingInfo.getLots() > 0 ? parkingInfo.getLots() : 1;
            double parkingLotCost = parkingPrice * parkingLots;
            int neededLots = (int) Math.ceil(cashToFree / parkingLotCost);
            int parkingLotsToSell = Math.min(neededLots, parkingQty);

            if (parkingLotsToSell > 0) {
                double actualCashToFree = parkingLotsToSell * parkingLotCost;

                if (tradingService != null) {
                    tradingService.sellByMarketWithDetails(parkingTicker, parkingType, actualCashToFree, 0.0, 0.0);
                } else if (marketDataProvider != null) {
                    marketDataProvider.sellByMarket(parkingTicker, parkingType, actualCashToFree);
                }

                LoggingUtils.log(
                        "PARTIALFREE " + tradeTicker + ": sold " + parkingTicker + " value="
                                + String.format("%.2f", actualCashToFree) + " (" + parkingLotsToSell
                                + " lots) to free cash");
            }
        } catch (Exception e) {
            LoggingUtils.log("PARTIALFREE_FAIL: Failed to sell " + getParkingTicker() + ": " + e.getMessage());
        }
    }

    /**
     * Close the parking position completely.
     */
    public void closeParkingPosition() {
        String parkingTicker = getParkingTicker();
        if (parkingTicker == null) {
            return;
        }
        TickerType parkingType = getParkingTickerType();

        try {
            if (tradingService != null) {
                tradingService.closeLongByMarket(parkingTicker, parkingType);
            } else if (marketDataProvider != null) {
                marketDataProvider.closeLongByMarket(parkingTicker, parkingType);
            }

            positionStore.remove(parkingTicker);
            LoggingUtils.log(parkingTicker + " sold, positionStore cleared");
        } catch (Exception e) {
            LoggingUtils.log("CASH_FREEFAIL: Failed to sell " + parkingTicker + ": " + e.getMessage());
        }
    }

    /**
     * Get the parking position from the local store.
     */
    public Position getStoredParkingPosition() {
        String parkingTicker = getParkingTicker();
        return parkingTicker != null ? positionStore.get(parkingTicker) : null;
    }

    /**
     * Store the parking position locally.
     */
    public void storeParkingPosition(Position position) {
        String parkingTicker = getParkingTicker();
        if (parkingTicker != null) {
            positionStore.put(parkingTicker, position);
        }
    }

    /**
     * Remove the parking position from the local store.
     */
    public void removeStoredParkingPosition() {
        String parkingTicker = getParkingTicker();
        if (parkingTicker != null) {
            positionStore.remove(parkingTicker);
        }
    }

    /**
     * Check if parking position exists in the store.
     */
    public boolean hasStoredParkingPosition() {
        Position pos = getStoredParkingPosition();
        return pos != null && pos.quantity > 0;
    }

    /**
     * Find ticker info for the parking ticker.
     */
    public TickerInfo findParkingTickerInfo(Map<TickerInfo.Key, TickerInfo> allTickers) {
        String parkingTicker = getParkingTicker();
        if (parkingTicker == null) {
            return null;
        }
        for (TickerInfo info : allTickers.values()) {
            if (info.getName().equalsIgnoreCase(parkingTicker) || info.getTicker().equalsIgnoreCase(parkingTicker)) {
                return info;
            }
        }
        return null;
    }
}
