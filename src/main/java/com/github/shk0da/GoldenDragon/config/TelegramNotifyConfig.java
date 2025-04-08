package com.github.shk0da.GoldenDragon.config;

import com.github.shk0da.GoldenDragon.utils.PropertiesUtils;

import java.io.IOException;
import java.util.Properties;

public class TelegramNotifyConfig {

    private final Boolean enable;
    private final String botToken;
    private final String chatId;
    private final Boolean extended;

    public TelegramNotifyConfig() throws IOException {
        final Properties properties = PropertiesUtils.loadProperties();
        this.enable = Boolean.valueOf(properties.getProperty("telegram.notify.enable", "false"));
        this.botToken = properties.getProperty("telegram.notify.botToken");
        this.chatId = properties.getProperty("telegram.notify.chatId");
        this.extended = Boolean.valueOf(properties.getProperty("telegram.notify.extended", "true"));
    }

    public Boolean isEnable() {
        return enable;
    }

    public String getBotToken() {
        return botToken;
    }

    public String getChatId() {
        return chatId;
    }

    public Boolean isExtended() {
        return extended;
    }
}
