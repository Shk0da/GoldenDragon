package com.github.shk0da.GoldenDragon.utils;

import java.net.http.HttpResponse;
import java.util.concurrent.TimeUnit;
import java.util.function.Supplier;

import static java.lang.System.out;

public final class RequestUtils {

    public static HttpResponse<String> requestWithRetry(Supplier<HttpResponse<String>> supplier) {
        int tryCount = 0;
        HttpResponse<String> response = null;
        do {
            try {
                response = supplier.get();
                if (null != response && 200 == response.statusCode()) break;
            } catch (Exception ex) {
                out.println("Error: " + ex.getMessage());
            }

            try {
                int timeout = (null != response && response.statusCode() >= 500) ? 2 : 1;
                TimeUnit.SECONDS.sleep(timeout);
            } catch (InterruptedException ex) {
                out.println("Error: " + ex.getMessage());
            }
            tryCount++;
        } while (tryCount < 5);
        if (null == response) {
            throw new RuntimeException("Failed execute request with retry");
        }
        return response;
    }
}
