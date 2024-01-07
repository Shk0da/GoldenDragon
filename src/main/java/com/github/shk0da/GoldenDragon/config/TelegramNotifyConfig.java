package com.github.shk0da.GoldenDragon.config;

import com.github.shk0da.GoldenDragon.utils.PropertiesUtils;

import java.io.IOException;
import java.util.Properties;

public class TelegramNotifyConfig {

    private final String botToken;
    private final String chatId;

    public TelegramNotifyConfig() throws IOException {
        final Properties properties = PropertiesUtils.loadProperties();
        this.botToken = properties.getProperty("telegram.notify.botToken");
        this.chatId = properties.getProperty("telegram.notify.chatId");
    }

    public String getBotToken() {
        return botToken;
    }

    public String getChatId() {
        return chatId;
    }
}
