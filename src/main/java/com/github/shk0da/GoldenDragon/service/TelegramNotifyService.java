package com.github.shk0da.GoldenDragon.service;

import com.github.shk0da.GoldenDragon.config.TelegramNotifyConfig;

import java.net.URI;
import java.net.URLEncoder;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.time.temporal.ChronoUnit;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import static com.github.shk0da.GoldenDragon.config.MainConfig.HEADER_USER_AGENT;
import static com.github.shk0da.GoldenDragon.config.MainConfig.USER_AGENT;
import static com.github.shk0da.GoldenDragon.config.MainConfig.httpClient;
import static com.github.shk0da.GoldenDragon.utils.RequestUtils.requestWithRetry;
import static java.lang.System.out;

public class TelegramNotifyService {

    public static final TelegramNotifyService telegramNotifyService = new TelegramNotifyService();

    private static final String TELEGRAM_SEND_MESSAGE_URL = "https://api.telegram.org/bot%s/sendMessage?chat_id=%s&text=%s";

    private final String botToken;
    private final String chatId;

    private final ExecutorService queue = Executors.newSingleThreadExecutor();

    public TelegramNotifyService() {
        try {
            TelegramNotifyConfig telegramNotifyConfig = new TelegramNotifyConfig();
            this.botToken = telegramNotifyConfig.getBotToken();
            this.chatId = telegramNotifyConfig.getChatId();
        } catch (Exception ex) {
            throw new RuntimeException("Failed instance TelegramNotifyService: " + ex.getMessage());
        }
    }

    public void sendMessage(String message) {
        queue.execute(() -> requestWithRetry(() -> {
            String text = URLEncoder.encode(message, StandardCharsets.UTF_8);
            String uri = String.format(TELEGRAM_SEND_MESSAGE_URL, botToken, chatId, text);
            HttpRequest request = HttpRequest.newBuilder()
                    .POST(HttpRequest.BodyPublishers.noBody())
                    .uri(URI.create(uri))
                    .setHeader(HEADER_USER_AGENT, USER_AGENT)
                    .timeout(Duration.of(10, ChronoUnit.SECONDS))
                    .build();
            try {
                return httpClient.send(request, HttpResponse.BodyHandlers.ofString());
            } catch (Exception ex) {
                out.println("Error: " + ex.getMessage());
                return null;
            }
        }));
    }
}
