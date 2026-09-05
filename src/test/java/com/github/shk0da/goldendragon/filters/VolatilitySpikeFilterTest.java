package com.github.shk0da.goldendragon.filters;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.BDDAssertions.then;

/**
 * BDD-style tests for {@link VolatilitySpikeFilter}.
 */
@DisplayName("Volatility Spike Filter - Cooldown Management")
class VolatilitySpikeFilterTest {

    private static final int COOLDOWN_MS = 5000; // 5 seconds

    @Nested
    @DisplayName("When filter is disabled")
    class FilterDisabled {

        @Test
        @DisplayName("Should not be in cooldown when disabled")
        void shouldNotBeInCooldown_WhenDisabled() {
            // Given
            VolatilitySpikeFilter filter = new VolatilitySpikeFilter(false, COOLDOWN_MS);

            // When/Then
            then(filter.isInCooldown()).isFalse()
                    .as("Disabled filter should never be in cooldown");
        }

        @Test
        @DisplayName("Should report as disabled")
        void shouldBeDisabled() {
            // Given
            VolatilitySpikeFilter filter = new VolatilitySpikeFilter(false, COOLDOWN_MS);

            // When/Then
            then(filter.isEnabled()).isFalse();
        }
    }

    @Nested
    @DisplayName("When filter is enabled")
    class FilterEnabled {

        @Test
        @DisplayName("Should not be in cooldown initially")
        void shouldNotBeInCooldown_Initially() {
            // Given
            VolatilitySpikeFilter filter = new VolatilitySpikeFilter(true, COOLDOWN_MS);

            // When/Then
            then(filter.isInCooldown()).isFalse()
                    .as("Filter should not be in cooldown initially");
        }

        @Test
        @DisplayName("Should report as enabled")
        void shouldBeEnabled() {
            // Given
            VolatilitySpikeFilter filter = new VolatilitySpikeFilter(true, COOLDOWN_MS);

            // When/Then
            then(filter.isEnabled()).isTrue();
        }
    }

    @Nested
    @DisplayName("When cooldown is active")
    class CooldownActive {

        @Test
        @DisplayName("Should be in cooldown after spike detected")
        void shouldBeInCooldown_AfterSpikeDetected() throws InterruptedException {
            // Given
            VolatilitySpikeFilter filter = new VolatilitySpikeFilter(true, COOLDOWN_MS);
            
            // Simulate spike detection by setting lastSpikeDetectedAt
            simulateSpikeDetection(filter);

            // When
            boolean inCooldown = filter.isInCooldown();

            // Then
            then(inCooldown).isTrue()
                    .as("Should be in cooldown immediately after spike");
        }

        @Test
        @DisplayName("Should exit cooldown after timeout")
        void shouldExitCooldown_AfterTimeout() throws InterruptedException {
            // Given
            VolatilitySpikeFilter filter = new VolatilitySpikeFilter(true, 100); // 100ms cooldown
            
            simulateSpikeDetection(filter);
            then(filter.isInCooldown()).isTrue();

            // When - wait for cooldown to expire
            Thread.sleep(150);

            // Then
            then(filter.isInCooldown()).isFalse()
                    .as("Should exit cooldown after timeout expires");
        }

        @Test
        @DisplayName("Should remain in cooldown during timeout period")
        void shouldRemainInCooldown_DuringTimeout() throws InterruptedException {
            // Given
            VolatilitySpikeFilter filter = new VolatilitySpikeFilter(true, 1000); // 1 second
            
            simulateSpikeDetection(filter);

            // When - check at various points
            Thread.sleep(100);
            boolean stillInCooldown1 = filter.isInCooldown();
            
            Thread.sleep(400);
            boolean stillInCooldown2 = filter.isInCooldown();

            // Then
            then(stillInCooldown1).isTrue();
            then(stillInCooldown2).isTrue()
                    .as("Should remain in cooldown until timeout expires");
        }
    }

    @Nested
    @DisplayName("When reset is called")
    class Reset {

        @Test
        @DisplayName("Should clear cooldown state")
        void shouldClearCooldown() throws InterruptedException {
            // Given
            VolatilitySpikeFilter filter = new VolatilitySpikeFilter(true, COOLDOWN_MS);
            simulateSpikeDetection(filter);
            then(filter.isInCooldown()).isTrue();

            // When
            filter.reset();

            // Then
            then(filter.isInCooldown()).isFalse()
                    .as("Reset should clear cooldown state");
        }

        @Test
        @DisplayName("Should handle multiple resets gracefully")
        void shouldHandleMultipleResets() {
            // Given
            VolatilitySpikeFilter filter = new VolatilitySpikeFilter(true, COOLDOWN_MS);

            // When/Then
            then(filter.isInCooldown()).isFalse();
            
            filter.reset();
            then(filter.isInCooldown()).isFalse();
            
            filter.reset();
            then(filter.isInCooldown()).isFalse()
                    .as("Multiple resets should not cause issues");
        }
    }

    @Nested
    @DisplayName("When cooldown duration varies")
    class CooldownDuration {

        @Test
        @DisplayName("Should respect short cooldown")
        void shouldRespectShortCooldown() throws InterruptedException {
            // Given
            VolatilitySpikeFilter filter = new VolatilitySpikeFilter(true, 50); // 50ms
            
            simulateSpikeDetection(filter);

            // When
            Thread.sleep(100);

            // Then
            then(filter.isInCooldown()).isFalse()
                    .as("Short cooldown should expire quickly");
        }

        @Test
        @DisplayName("Should respect long cooldown")
        void shouldRespectLongCooldown() throws InterruptedException {
            // Given
            VolatilitySpikeFilter filter = new VolatilitySpikeFilter(true, 2000); // 2 seconds
            
            simulateSpikeDetection(filter);

            // When
            Thread.sleep(500);

            // Then
            then(filter.isInCooldown()).isTrue()
                    .as("Long cooldown should persist");
        }

        @Test
        @DisplayName("Should handle zero cooldown")
        void shouldHandleZeroCooldown() {
            // Given
            VolatilitySpikeFilter filter = new VolatilitySpikeFilter(true, 0);
            
            simulateSpikeDetection(filter);

            // When/Then
            then(filter.isInCooldown()).isFalse()
                    .as("Zero cooldown should mean no cooldown");
        }
    }

    @Nested
    @DisplayName("When checking cooldown state")
    class CooldownState {

        @Test
        @DisplayName("Should return correct cooldown status")
        void shouldReturnCorrectCooldownStatus() throws InterruptedException {
            // Given
            VolatilitySpikeFilter filter = new VolatilitySpikeFilter(true, 500);
            
            simulateSpikeDetection(filter);

            // When/Then - immediate check
            then(filter.isInCooldown()).isTrue();

            // When/Then - after partial cooldown
            Thread.sleep(250);
            then(filter.isInCooldown()).isTrue();

            // When/Then - after full cooldown
            Thread.sleep(300);
            then(filter.isInCooldown()).isFalse();
        }
    }

    // ===== Helper method =====
    
    /**
     * Simulates spike detection by using reflection to set the private field.
     * This is necessary because VolatilitySpikeFilter doesn't expose a method
     * to trigger spike detection externally.
     */
    private void simulateSpikeDetection(VolatilitySpikeFilter filter) {
        // Set lastSpikeDetectedAtMs to current time using reflection
        try {
            java.lang.reflect.Field field = VolatilitySpikeFilter.class
                    .getDeclaredField("lastSpikeDetectedAtMs");
            field.setAccessible(true);
            field.set(filter, System.currentTimeMillis());
        } catch (Exception e) {
            throw new RuntimeException("Failed to simulate spike detection", e);
        }
    }
}
