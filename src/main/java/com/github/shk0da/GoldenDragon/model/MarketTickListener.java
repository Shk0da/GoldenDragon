package com.github.shk0da.goldendragon.model;

public interface MarketTickListener {

  default void onOrderBook(MarketDepthSnapshot snapshot) {}

  default void onTrade(MarketTradeTick trade) {}

  default void onError(Throwable throwable) {}
}
