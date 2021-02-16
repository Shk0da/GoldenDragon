package com.github.shk0da.GoldenDragon.utils;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Objects;
import java.util.Properties;

public final class PropertiesUtils {

    public static Properties loadProperties() throws IOException {
        final Properties properties = new Properties();

        InputStream internalProperties = PropertiesUtils.class.getClassLoader().getResourceAsStream("application.properties");
        properties.load(Objects.requireNonNull(internalProperties));

        String externalPathProperties = System.getProperty("application.properties");
        if (null != externalPathProperties && Files.exists(Path.of(externalPathProperties))) {
            InputStream externalProperties = new FileInputStream(new File(externalPathProperties));
            properties.load(Objects.requireNonNull(externalProperties));
        }

        return properties;
    }
}
