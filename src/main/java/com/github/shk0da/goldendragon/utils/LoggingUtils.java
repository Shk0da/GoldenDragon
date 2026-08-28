package com.github.shk0da.goldendragon.utils;

import java.text.SimpleDateFormat;
import java.util.Date;

import static java.lang.System.out;

/**
 * Utility class for logging with timestamps. Provides consistent log format across the application.
 */
public class LoggingUtils {

    private static final ThreadLocal<SimpleDateFormat> LOG_TIME_FORMAT =
            ThreadLocal.withInitial(() -> new SimpleDateFormat("dd.MM.yyyy HH:mm:ss"));

    /**
     * Logs a message with timestamp. Format: [dd.MM.yyyy HH:mm:ss] message
     *
     * @param message message to log
     */
    public static void log(String message) {
        out.println("[" + LOG_TIME_FORMAT.get().format(new Date()) + "] " + message);
    }
}
