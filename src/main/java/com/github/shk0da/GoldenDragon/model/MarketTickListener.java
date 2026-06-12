package com.github.shk0da.GoldenDragon.model;

public interface MarketTickListener {

  default void onOrderBook(MarketDepthSnapshot snapshot) {}

  default void onTrade(MarketTradeTick trade) {}

  default void onError(Throwable throwable) {}
}
