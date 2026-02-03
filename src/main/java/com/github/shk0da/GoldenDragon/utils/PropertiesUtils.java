package com.github.shk0da.GoldenDragon.utils;

import java.io.FileInputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Objects;
import java.util.Properties;

public final class PropertiesUtils {

    private static final String PROPERTIES_FILE = "application.properties";

    public static Properties loadProperties() throws IOException {
        final Properties properties = new Properties();

        ClassLoader classLoader = PropertiesUtils.class.getClassLoader();
        try (var internalProperties = classLoader.getResourceAsStream(PROPERTIES_FILE)) {
            properties.load(Objects.requireNonNull(internalProperties));
        }

        if (Files.exists(Path.of(PROPERTIES_FILE))) {
            try (FileInputStream input = new FileInputStream(PROPERTIES_FILE)) {
                properties.load(input);
            } catch (Exception skip) {
                // nothing
            }
        }

        String externalPathProperties = System.getProperty(PROPERTIES_FILE);
        if (null != externalPathProperties && Files.exists(Path.of(externalPathProperties))) {
            try (var externalProperties = new FileInputStream(externalPathProperties)) {
                properties.load(Objects.requireNonNull(externalProperties));
            }
        }
        return properties;
    }
}
