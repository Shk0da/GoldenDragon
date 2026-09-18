package com.github.shk0da.goldendragon;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Integration tests for Gradle tasks: runStrategy and runStrategyAI.
 *
 * These tests verify that the Gradle tasks are properly configured and can be launched.
 * They verify task configuration WITHOUT executing the strategies themselves (which
 * would require API keys and real trading). Instead, they check the build.gradle file
 * for correct task configuration.
 *
 * Note: These tests are lightweight and run as part of the default test suite.
 */
class GradleTasksIntegrationTest {

    private static final String BUILD_GRADLE = "build.gradle";

    @Test
    @DisplayName("build.gradle should define runStrategy task")
    void buildGradleShouldDefineRunStrategyTask() throws IOException {
        // Arrange
        String buildGradle = readBuildGradle();

        // Act & Assert
        assertThat(buildGradle)
                .contains("task runStrategy")
                .contains("mainClass = 'com.github.shk0da.goldendragon.GoldenDragon'")
                .contains("sourceSets.test.runtimeClasspath");
    }

    @Test
    @DisplayName("build.gradle should define runStrategyAI task")
    void buildGradleShouldDefineRunStrategyAITask() throws IOException {
        // Arrange
        String buildGradle = readBuildGradle();

        // Act & Assert
        assertThat(buildGradle)
                .contains("task runStrategyAI")
                .contains("mainClass = 'com.github.shk0da.goldendragon.GoldenDragon'")
                .contains("args 'TradeCouncilStrategy'");
    }

    @Test
    @DisplayName("build.gradle should configure default strategy for runStrategy")
    void buildGradleShouldConfigureDefaultStrategy() throws IOException {
        // Arrange
        String buildGradle = readBuildGradle();

        // Act & Assert
        assertThat(buildGradle)
                .contains("findProperty('strategy')")
                .contains("?: 'UnifiedStrategy'");
    }

    @Test
    @DisplayName("build.gradle should keep Java 11 compatibility settings")
    void buildGradleShouldKeepJava11Compatibility() throws IOException {
        // Arrange
        String buildGradle = readBuildGradle();

        // Act & Assert
        assertThat(buildGradle)
                .contains("sourceCompatibility = JavaVersion.VERSION_11");
    }

    private String readBuildGradle() throws IOException {
        Path buildGradlePath = Path.of(BUILD_GRADLE);
        if (!Files.exists(buildGradlePath)) {
            throw new AssertionError("build.gradle not found in " + System.getProperty("user.dir"));
        }
        return Files.readString(buildGradlePath, StandardCharsets.UTF_8);
    }
}
