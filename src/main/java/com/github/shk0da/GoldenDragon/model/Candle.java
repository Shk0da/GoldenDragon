package com.github.shk0da.GoldenDragon.model;

public class Candle {

  public final String time;
  public final double open;
  public final double high;
  public final double low;
  public final double close;
  public final long volume;

  public Candle(String time, double open, double high, double low, double close, long volume) {
    this.time = time;
    this.open = open;
    this.high = high;
    this.low = low;
    this.close = close;
    this.volume = volume;
  }
}
