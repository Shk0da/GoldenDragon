package com.github.shk0da.GoldenDragon.strategy;

import com.github.shk0da.GoldenDragon.config.TelegramAppConfig;
import com.github.shk0da.GoldenDragon.service.TelegramAppService;

public class TelegramAuth {

    private final TelegramAppService telegramAppService;

    public TelegramAuth(TelegramAppConfig telegramAppConfig) {
        this.telegramAppService = new TelegramAppService(telegramAppConfig);
    }

    public void run() throws Exception {
        telegramAppService.startSession();
    }
}
