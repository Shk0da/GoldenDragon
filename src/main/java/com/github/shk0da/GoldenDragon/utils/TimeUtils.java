package com.github.shk0da.goldendragon.utils;

import java.util.concurrent.TimeUnit;

/**
 * Utility class for time operations. Provides safe sleep method that handles InterruptedException.
 */
public final class TimeUtils {

  public static void sleep(long time) {
    try {
      TimeUnit.MILLISECONDS.sleep(time);
    } catch (InterruptedException skip) {
      // nothing
    }
  }
}
