package com.github.shk0da.goldendragon.strategy.orderbook;

import java.time.Instant;

/** Read-only view of an open position for signal exit evaluation. */
public interface OrderBookPositionView {

    String getSignalId();

    String getDirection();

    double getEntryPrice();

    double getSpreadAtEntry();

    Instant getEntryTime();

    double getTakeProfitPrice();

    double getStopLossPrice();

    long getHeldSeconds();
}
