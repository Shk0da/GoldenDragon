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
        return loadProperties(PROPERTIES_FILE);
    }

    public static Properties loadProperties(String fileName) throws IOException {
        final Properties properties = new Properties();

        ClassLoader classLoader = PropertiesUtils.class.getClassLoader();
        try (var internalProperties = classLoader.getResourceAsStream(fileName)) {
            if (null != internalProperties) {
                properties.load(internalProperties);
            }
        }

        if (Files.exists(Path.of(fileName))) {
            try (FileInputStream input = new FileInputStream(fileName)) {
                properties.load(input);
            } catch (Exception skip) {
                // nothing
            }
        }

        String externalPathProperties = System.getProperty(fileName);
        if (null != externalPathProperties && Files.exists(Path.of(externalPathProperties))) {
            try (var externalProperties = new FileInputStream(externalPathProperties)) {
                properties.load(Objects.requireNonNull(externalProperties));
            }
        }
        return properties;
    }
}
