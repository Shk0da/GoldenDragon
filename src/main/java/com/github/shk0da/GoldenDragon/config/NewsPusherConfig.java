package com.github.shk0da.GoldenDragon.config;

import com.github.shk0da.GoldenDragon.utils.PropertiesUtils;

import java.io.IOException;
import java.util.Properties;

public class NewsPusherConfig {

    private final String newsSource;

    public NewsPusherConfig() throws IOException {
        final Properties properties = PropertiesUtils.loadProperties();
        this.newsSource = properties.getProperty("news.pusher.source");
    }

    public String getNewsSource() {
        return newsSource;
    }
}
