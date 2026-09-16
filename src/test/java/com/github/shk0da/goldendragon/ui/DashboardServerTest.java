package com.github.shk0da.goldendragon.ui;

import com.google.gson.Gson;
import com.google.gson.JsonObject;
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

    private int findFreePort() throws IOException {
        try (ServerSocket socket = new ServerSocket(0)) {
            return socket.getLocalPort();
        }
    }

    @Test
    @DisplayName("Should calculate winRate as 0 when no trades")
    void shouldCalculateWinRate_ZeroTrades() throws IOException {
        int freePort = findFreePort();
        DashboardServer dashboard = new DashboardServer(null, freePort);
        dashboard.updateBalance(100000.0);
        dashboard.updateAvailableCash(100000.0);

        Map<String, Object> stats = getStats(dashboard);

        assertThat(stats.get("winRate")).isEqualTo(0.0);
        assertThat(stats.get("totalTrades")).isEqualTo(0);
        assertThat(stats.get("winningTrades")).isEqualTo(0);
    }

    @Test
    @DisplayName("Should calculate winRate as 100% when all trades are winning")
    void shouldCalculateWinRate_AllWinning() throws IOException {
        int freePort = findFreePort();
        DashboardServer dashboard = new DashboardServer(null, freePort);
        dashboard.updateBalance(100000.0);
        dashboard.updateAvailableCash(100000.0);

        dashboard.addTrade("GMKN", "SELL", 100, 250.0, 5000.0);
        dashboard.updateStats(5000.0, true);
        dashboard.addTrade("SBER", "SELL", 200, 300.0, 3000.0);
        dashboard.updateStats(3000.0, true);
        dashboard.addTrade("YNDX", "SELL", 50, 4000.0, 2000.0);
        dashboard.updateStats(2000.0, true);

        Map<String, Object> stats = getStats(dashboard);

        assertThat(stats.get("totalTrades")).isEqualTo(3);
        assertThat(stats.get("winningTrades")).isEqualTo(3);
        assertThat((Double) stats.get("winRate")).isEqualTo(1.0);
    }

    @Test
    @DisplayName("Should calculate winRate as 0% when all trades are losing")
    void shouldCalculateWinRate_AllLosing() throws IOException {
        int freePort = findFreePort();
        DashboardServer dashboard = new DashboardServer(null, freePort);
        dashboard.updateBalance(100000.0);
        dashboard.updateAvailableCash(100000.0);

        dashboard.addTrade("GMKN", "SELL", 100, 250.0, -5000.0);
        dashboard.updateStats(-5000.0, false);
        dashboard.addTrade("SBER", "SELL", 200, 300.0, -3000.0);
        dashboard.updateStats(-3000.0, false);
        dashboard.addTrade("YNDX", "SELL", 50, 4000.0, -2000.0);
        dashboard.updateStats(-2000.0, false);

        Map<String, Object> stats = getStats(dashboard);

        assertThat(stats.get("totalTrades")).isEqualTo(3);
        assertThat(stats.get("winningTrades")).isEqualTo(0);
        assertThat((Double) stats.get("winRate")).isEqualTo(0.0);
    }

    @Test
    @DisplayName("Should calculate winRate correctly for mixed trades")
    void shouldCalculateWinRate_Mixed() throws IOException {
        int freePort = findFreePort();
        DashboardServer dashboard = new DashboardServer(null, freePort);
        dashboard.updateBalance(100000.0);
        dashboard.updateAvailableCash(100000.0);

        dashboard.addTrade("GMKN", "SELL", 100, 250.0, 5000.0);
        dashboard.updateStats(5000.0, true);
        dashboard.addTrade("SBER", "SELL", 200, 300.0, -3000.0);
        dashboard.updateStats(-3000.0, false);
        dashboard.addTrade("YNDX", "SELL", 50, 4000.0, 2000.0);
        dashboard.updateStats(2000.0, true);
        dashboard.addTrade("TATN", "SELL", 150, 700.0, -1000.0);
        dashboard.updateStats(-1000.0, false);

        Map<String, Object> stats = getStats(dashboard);

        assertThat(stats.get("totalTrades")).isEqualTo(4);
        assertThat(stats.get("winningTrades")).isEqualTo(2);
        assertThat((Double) stats.get("winRate")).isEqualTo(0.5);
    }

    @Test
    @DisplayName("Should not count BUY trades in winRate calculation")
    void shouldNotCountBuyTrades_InWinRate() throws IOException {
        int freePort = findFreePort();
        DashboardServer dashboard = new DashboardServer(null, freePort);
        dashboard.updateBalance(100000.0);
        dashboard.updateAvailableCash(100000.0);

        dashboard.addTrade("GMKN", "BUY", 100, 250.0, 0.0);
        dashboard.addTrade("GMKN", "SELL", 100, 260.0, 1000.0);
        dashboard.updateStats(1000.0, true);

        Map<String, Object> stats = getStats(dashboard);

        assertThat(stats.get("totalTrades")).isEqualTo(1);
        assertThat(stats.get("winningTrades")).isEqualTo(1);
    }

    @Test
    @DisplayName("Should not count TMON@ cash parking trades in winRate")
    void shouldNotCountParkingTrades_InWinRate() throws IOException {
        int freePort = findFreePort();
        DashboardServer dashboard = new DashboardServer(null, freePort);
        dashboard.updateBalance(100000.0);
        dashboard.updateAvailableCash(100000.0);

        dashboard.addTrade("TMON@", "SELL", 1000, 10.0, 100.0);
        dashboard.addTrade("GMKN", "SELL", 100, 250.0, 5000.0);
        dashboard.updateStats(5000.0, true);

        Map<String, Object> stats = getStats(dashboard);

        assertThat(stats.get("totalTrades")).isEqualTo(1);
        assertThat(stats.get("winningTrades")).isEqualTo(1);
    }

    @Test
    @DisplayName("Should treat zero PnL as non-winning trade")
    void shouldTreatZeroPnl_AsNonWinning() throws IOException {
        int freePort = findFreePort();
        DashboardServer dashboard = new DashboardServer(null, freePort);
        dashboard.updateBalance(100000.0);
        dashboard.updateAvailableCash(100000.0);

        dashboard.addTrade("GMKN", "SELL", 100, 250.0, 0.0);
        dashboard.updateStats(0.0, false);

        Map<String, Object> stats = getStats(dashboard);

        assertThat(stats.get("totalTrades")).isEqualTo(1);
        assertThat(stats.get("winningTrades")).isEqualTo(0);
        assertThat((Double) stats.get("winRate")).isEqualTo(0.0);
    }

    @Test
    @DisplayName("Should return correct totalPnl in stats")
    void shouldCalculateTotalPnl_Correctly() throws IOException {
        int freePort = findFreePort();
        DashboardServer dashboard = new DashboardServer(null, freePort);
        dashboard.updateBalance(100000.0);
        dashboard.updateAvailableCash(100000.0);

        dashboard.addTrade("GMKN", "SELL", 100, 250.0, 5000.0);
        dashboard.updateStats(5000.0, true);
        dashboard.addTrade("SBER", "SELL", 200, 300.0, -3000.0);
        dashboard.updateStats(-3000.0, false);
        dashboard.addTrade("YNDX", "SELL", 50, 4000.0, 2000.0);
        dashboard.updateStats(2000.0, true);

        Map<String, Object> stats = getStats(dashboard);

        assertThat(stats.get("totalPnl")).isEqualTo(4000.0);
    }

    @Test
    @DisplayName("Should return stats via /api/stats endpoint")
    void shouldReturnStats_ViaApiEndpoint() throws Exception {
        int freePort;
        try (ServerSocket socket = new ServerSocket(0)) {
            freePort = socket.getLocalPort();
        }

        DashboardServer dashboard = new DashboardServer(null, freePort);
        dashboard.updateBalance(100000.0);
        dashboard.updateAvailableCash(95000.0);
        dashboard.addTrade("GMKN", "SELL", 100, 250.0, 5000.0);
        dashboard.updateStats(5000.0, true);
        dashboard.addTrade("SBER", "SELL", 200, 300.0, -2000.0);
        dashboard.updateStats(-2000.0, false);
        dashboard.start();

        try {
            HttpClient client = HttpClient.newHttpClient();
            HttpRequest request = HttpRequest.newBuilder(
                    URI.create("http://localhost:" + dashboard.getPort() + "/api/stats")).GET().build();
            HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());

            assertThat(response.statusCode()).isEqualTo(200);

            Gson gson = new Gson();
            JsonObject json = gson.fromJson(response.body(), JsonObject.class);

            assertThat(json.get("totalTrades").getAsInt()).isEqualTo(2);
            assertThat(json.get("winningTrades").getAsInt()).isEqualTo(1);
            assertThat(json.get("winRate").getAsDouble()).isEqualTo(0.5);
            assertThat(json.get("totalPnl").getAsDouble()).isEqualTo(3000.0);
            assertThat(json.get("balance").getAsDouble()).isEqualTo(100000.0);
            assertThat(json.get("availableCash").getAsDouble()).isEqualTo(95000.0);
        } finally {
            dashboard.stop();
        }
    }

    private Map<String, Object> getStats(DashboardServer dashboard) {
        int totalTrades = (int) getFieldValue(dashboard, "totalTrades");
        int winningTrades = (int) getFieldValue(dashboard, "winningTrades");
        double totalPnl = (double) getFieldValue(dashboard, "totalPnl");

        Map<String, Object> stats = new HashMap<>();
        stats.put("totalTrades", totalTrades);
        stats.put("winningTrades", winningTrades);
        stats.put("totalPnl", totalPnl);
        stats.put("winRate", totalTrades > 0 ? (double) winningTrades / totalTrades : 0.0);
        return stats;
    }

    private Object getFieldValue(DashboardServer dashboard, String fieldName) {
        try {
            java.lang.reflect.Field field = DashboardServer.class.getDeclaredField(fieldName);
            field.setAccessible(true);
            return field.get(dashboard);
        } catch (Exception e) {
            throw new RuntimeException("Failed to access field: " + fieldName, e);
        }
    }
}
