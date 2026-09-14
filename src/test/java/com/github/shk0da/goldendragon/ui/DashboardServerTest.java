package com.github.shk0da.goldendragon.ui;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.LocalTime;
import java.util.HashMap;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;

class DashboardServerTest {

    @Test
    @DisplayName("Should fall back to next port when requested port is busy")
    void shouldFallBackToNextPort_WhenRequestedPortBusy() throws IOException {
        try (ServerSocket blocker = new ServerSocket(0)) {
            int busyPort = blocker.getLocalPort();

            DashboardServer dashboard = new DashboardServer(null, busyPort);

            assertThat(dashboard.getPort()).isEqualTo(busyPort + 1);

            dashboard.start();
            dashboard.stop();
        }
    }

    @Test
    @DisplayName("Should exclude cash parking ticker from win rate stats")
    void shouldExcludeParkingTicker_FromCountableTrades() {
        Map<String, Object> parkingSell = new HashMap<>();
        parkingSell.put("type", "SELL");
        parkingSell.put("ticker", "TMON@");
        assertThat(DashboardServer.isCountableTrade(parkingSell)).isFalse();

        Map<String, Object> regularSell = new HashMap<>();
        regularSell.put("type", "SELL");
        regularSell.put("ticker", "GMKN");
        assertThat(DashboardServer.isCountableTrade(regularSell)).isTrue();

        Map<String, Object> parkingBuy = new HashMap<>();
        parkingBuy.put("type", "BUY");
        parkingBuy.put("ticker", "TMON@");
        assertThat(DashboardServer.isCountableTrade(parkingBuy)).isFalse();
    }

    @Test
    @DisplayName("Should render end-of-day countdown in dashboard HTML")
    void shouldRenderEodCountdown_InDashboardHtml() throws Exception {
        int freePort;
        try (ServerSocket socket = new ServerSocket(0)) {
            freePort = socket.getLocalPort();
        }

        DashboardServer dashboard = new DashboardServer(null, freePort, 120, LocalTime.of(19, 0));
        dashboard.start();
        try {
            HttpClient client = HttpClient.newHttpClient();
            HttpRequest request = HttpRequest.newBuilder(
                    URI.create("http://localhost:" + dashboard.getPort() + "/")).GET().build();
            HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());
            assertThat(response.body()).contains("eod-countdown").contains("eodTarget");
        } finally {
            dashboard.stop();
        }
    }
}