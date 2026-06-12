package com.github.shk0da.GoldenDragon.utils;

import java.text.SimpleDateFormat;
import java.util.Date;

import static java.lang.System.out;

/**
 * Utility class for logging with timestamps.
 * Provides consistent log format across the application.
 */
public class LoggingUtils {

    private static final ThreadLocal<SimpleDateFormat> LOG_TIME_FORMAT =
            ThreadLocal.withInitial(() -> new SimpleDateFormat("dd.MM.yyyy HH:mm:ss"));

    /**
     * Logs a message with timestamp.
     * Format: [dd.MM.yyyy HH:mm:ss] message
     *
     * @param message message to log
     */
    public static void log(String message) {
        out.println("[" + LOG_TIME_FORMAT.get().format(new Date()) + "] " + message);
    }

    /**
     * Logs an error message with timestamp.
     * Format: [dd.MM.yyyy HH:mm:ss] ERROR: message
     *
     * @param message error message
     * @param t optional throwable (can be null)
     */
    public static void logError(String message, Throwable t) {
        out.println("[" + LOG_TIME_FORMAT.get().format(new Date()) + "] ERROR: " + message);
        if (t != null) {
            t.printStackTrace(out);
        }
    }

    /**
     * Logs a message with timestamp and custom format.
     * Format: [dd.MM.yyyy HH:mm:ss.SSS] message
     *
     * @param message message to log
     */
    public static void logDetailed(String message) {
        ThreadLocal<SimpleDateFormat> detailedFormat = ThreadLocal.withInitial(
                () -> new SimpleDateFormat("dd.MM.yyyy HH:mm:ss.SSS")
        );
        out.println("[" + detailedFormat.get().format(new Date()) + "] " + message);
    }
}
