package com.github.shk0da.GoldenDragon.utils;

import java.util.concurrent.TimeUnit;

public final class TimeUtils {

    public static void sleep(long time) {
        try {
            TimeUnit.MILLISECONDS.sleep(time);
        } catch (InterruptedException skip) {
            // nothing
        }
    }
}
