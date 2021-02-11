package com.github.shk0da.GoldenDragon.utils;

import java.io.IOException;
import java.io.InputStream;
import java.util.Objects;
import java.util.Properties;

public final class PropertiesUtils {

    public static Properties loadProperties() throws IOException {
        InputStream inputStream = PropertiesUtils.class.getClassLoader().getResourceAsStream("application.properties");
        final Properties properties = new Properties();
        properties.load(Objects.requireNonNull(inputStream));
        return properties;
    }
}
