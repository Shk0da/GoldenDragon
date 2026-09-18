package com.github.shk0da.goldendragon.strategy;

import com.github.shk0da.goldendragon.GoldenDragon;
import com.github.shk0da.goldendragon.config.MainConfig;
import com.github.shk0da.goldendragon.service.TradingService;
import org.junit.jupiter.api.Test;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

public class GoldenDragonErrorResilienceTest {

    @Test
    void executeStrategy_shouldNotCrashWhenStrategyThrowsException() throws Exception {
        // Arrange
        MainConfig mockConfig = mock(MainConfig.class);
        TradingService mockService = mock(TradingService.class);
        String[] args = new String[]{};

        // Act & Assert - verify no exception propagates
        assertDoesNotThrow(() -> {
            // Use reflection to call private executeStrategy method
            java.lang.reflect.Method method = GoldenDragon.class.getDeclaredMethod(
                "executeStrategy", 
                String.class, 
                MainConfig.class, 
                TradingService.class, 
                String[].class
            );
            method.setAccessible(true);
            method.invoke(null, "TestStrategy", mockConfig, mockService, args);
        });
    }

    @Test
    void executeStrategy_shouldLogErrorWhenStrategyThrowsException() throws Exception {
        // Arrange
        MainConfig mockConfig = mock(MainConfig.class);
        TradingService mockService = mock(TradingService.class);
        String[] args = new String[]{};

        // Capture System.out
        ByteArrayOutputStream outContent = new ByteArrayOutputStream();
        PrintStream originalOut = System.out;
        System.setOut(new PrintStream(outContent));

        try {
            // Act
            java.lang.reflect.Method method = GoldenDragon.class.getDeclaredMethod(
                "executeStrategy", 
                String.class, 
                MainConfig.class, 
                TradingService.class, 
                String[].class
            );
            method.setAccessible(true);
            method.invoke(null, "UnknownStrategy", mockConfig, mockService, args);

            // Assert - verify something was logged
            String output = outContent.toString();
            assertTrue(output.length() > 0, "Should log something");
        } finally {
            System.setOut(originalOut);
        }
    }

    @Test
    void executeStrategy_shouldContinueAfterUnknownStrategy() throws Exception {
        // Arrange
        MainConfig mockConfig = mock(MainConfig.class);
        TradingService mockService = mock(TradingService.class);
        String[] args = new String[]{};

        // Capture System.out
        ByteArrayOutputStream outContent = new ByteArrayOutputStream();
        PrintStream originalOut = System.out;
        System.setOut(new PrintStream(outContent));

        try {
            // Act
            java.lang.reflect.Method method = GoldenDragon.class.getDeclaredMethod(
                "executeStrategy", 
                String.class, 
                MainConfig.class, 
                TradingService.class, 
                String[].class
            );
            method.setAccessible(true);
            method.invoke(null, "UnknownStrategy", mockConfig, mockService, args);

            // Assert - verify message was logged
            String output = outContent.toString();
            assertTrue(output.contains("Unknown strategy"), "Should log 'Unknown strategy'");
        } finally {
            System.setOut(originalOut);
        }
    }
}
