package com.github.shk0da.GoldenDragon.config;

import java.util.Properties;

import com.github.shk0da.GoldenDragon.utils.PropertiesUtils;

public class DatabaseConfig {

    private final String jdbcUrl;
    private final String jdbcUser;
    private final String jdbcPassword;

    public DatabaseConfig() throws Exception {
        final Properties properties = PropertiesUtils.loadProperties();
        this.jdbcUrl = properties.getProperty("database.url");
        this.jdbcUser = properties.getProperty("database.user");
        this.jdbcPassword = properties.getProperty("database.password");
    }

    public String getJdbcUrl() {
        return jdbcUrl;
    }

    public String getJdbcUser() {
        return jdbcUser;
    }

    public String getJdbcPassword() {
        return jdbcPassword;
    }
}
