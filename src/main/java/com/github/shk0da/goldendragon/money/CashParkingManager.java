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
 * For BYBIT: uses SPYUSDT (crypto perpetual) for cash parking.
 * </p>
 */
public class CashParkingManager {

    private static final String TINKOFF_PARKING_TICKER = "TMON@";
    private static final String BYBIT_PARKING_TICKER = "SPYUSDT";
    private static final TickerType TINKOFF_PARKING_TYPE = TickerType.ETF;
    private static final TickerType BYBIT_PARKING_TYPE = TickerType.CRYPTO;

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
     * @return "SPYUSDT" for BYBIT, "TMON@" for TINKOFF
     */
    public String getParkingTicker() {
        if (tradingService != null && tradingService.getServiceType() == TradingService.TradingServiceType.BYBIT) {
            return BYBIT_PARKING_TICKER;
        }
        return TINKOFF_PARKING_TICKER;
    }

    /**
     * Get the parking ticker type based on TradingService type.
     * @return CRYPTO for BYBIT, ETF for TINKOFF
     */
    public TickerType getParkingTickerType() {
        if (tradingService != null && tradingService.getServiceType() == TradingService.TradingServiceType.BYBIT) {
            return BYBIT_PARKING_TYPE;
        }
        return TINKOFF_PARKING_TYPE;
    }

    /**
     * Check if the given ticker is the parking ticker.
     */
    public boolean isParkingTicker(String ticker) {
        return getParkingTicker().equals(ticker);
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
        return positionStore.get(getParkingTicker());
    }

    /**
     * Store the parking position locally.
     */
    public void storeParkingPosition(Position position) {
        positionStore.put(getParkingTicker(), position);
    }

    /**
     * Remove the parking position from the local store.
     */
    public void removeStoredParkingPosition() {
        positionStore.remove(getParkingTicker());
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
        for (TickerInfo info : allTickers.values()) {
            if (info.getName().equalsIgnoreCase(parkingTicker) || info.getTicker().equalsIgnoreCase(parkingTicker)) {
                return info;
            }
        }
        return null;
    }
}
