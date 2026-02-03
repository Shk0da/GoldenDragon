package com.github.shk0da.GoldenDragon.config;

import com.github.shk0da.GoldenDragon.utils.PropertiesUtils;
import java.io.IOException;
import java.util.Properties;

public class TelegramAppConfig {

    private final String appId;
    private final String apiHash;
    private final String encryptionKey;
    private final String sessionDir;

    public TelegramAppConfig() throws IOException {
        final Properties properties = PropertiesUtils.loadProperties();
        this.appId = properties.getProperty("telegram.app.appId");
        this.apiHash = properties.getProperty("telegram.app.apiHash");
        this.encryptionKey = properties.getProperty("telegram.app.encryptionKey");
        this.sessionDir = properties.getProperty("telegram.app.sessionDir", "telegram_session");
    }

    public String getAppId() {
        return appId;
    }

    public String getApiHash() {
        return apiHash;
    }

    public String getEncryptionKey() {
        return encryptionKey;
    }

    public String getSessionDir() {
        return sessionDir;
    }
}
