package com.github.shk0da.goldendragon.ui;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.net.ServerSocket;

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
}