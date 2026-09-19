package com.github.shk0da.goldendragon.money;

import com.github.shk0da.goldendragon.model.TickerInfo;
import com.github.shk0da.goldendragon.service.TradingService;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.time.Instant;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import static org.assertj.core.api.BDDAssertions.then;

@DisplayName("LossStreakMonitor")
class LossStreakMonitorTest {

    private static Map<String, Object> trade(String time, double pnl) {
        return trade(time, "NLMK", pnl);
    }

    private static Map<String, Object> trade(String time, String ticker, double pnl) {
        Map<String, Object> trade = new LinkedHashMap<>();
        trade.put("time", time);
        trade.put("ticker", ticker);
        trade.put("type", "SELL");
        trade.put("quantity", 10);
        trade.put("price", 100.0);
        trade.put("pnl", pnl);
        return trade;
    }

    @Nested
    @DisplayName("countConsecutiveLosses")
    class CountConsecutiveLosses {

        @Test
        @DisplayName("Should return 0 for null or empty history")
        void shouldReturnZero_ForNullOrEmpty() {
            then(LossStreakMonitor.countConsecutiveLosses(null, "")).isZero();
            then(LossStreakMonitor.countConsecutiveLosses(List.of(), "")).isZero();
        }

        @Test
        @DisplayName("Should count consecutive losses from the end")
        void shouldCountConsecutiveLosses_FromEnd() {
            List<Map<String, Object>> trades =
                    List.of(trade("2026-09-11 10:00:00", -100.0), trade("2026-09-11 11:00:00", -50.0));

            then(LossStreakMonitor.countConsecutiveLosses(trades, "")).isEqualTo(2);
        }

        @Test
        @DisplayName("Should stop counting at a winning trade")
        void shouldStop_AtWinningTrade() {
            List<Map<String, Object>> trades =
                    List.of(
                            trade("2026-09-11 10:00:00", -100.0),
                            trade("2026-09-11 11:00:00", 50.0),
                            trade("2026-09-11 12:00:00", -30.0));

            then(LossStreakMonitor.countConsecutiveLosses(trades, "")).isEqualTo(1);
        }

        @Test
        @DisplayName("Should skip opening operations with zero pnl")
        void shouldSkip_ZeroPnlOperations() {
            List<Map<String, Object>> trades =
                    List.of(
                            trade("2026-09-11 10:00:00", 0.0), // opening BUY
                            trade("2026-09-11 11:00:00", -100.0), // closing SELL (loss)
                            trade("2026-09-11 12:00:00", 0.0), // opening BUY
                            trade("2026-09-11 13:00:00", -50.0)); // closing SELL (loss)

            then(LossStreakMonitor.countConsecutiveLosses(trades, "")).isEqualTo(2);
        }

        @Test
        @DisplayName("Should sort by time before counting")
        void shouldSort_ByTime() {
            List<Map<String, Object>> trades =
                    List.of(
                            trade("2026-09-11 13:00:00", -50.0),
                            trade("2026-09-11 10:00:00", -100.0));

            then(LossStreakMonitor.countConsecutiveLosses(trades, "")).isEqualTo(2);
        }
    }

    @Nested
    @DisplayName("countConsecutiveLosses with excluded ticker")
    class ParkingTickerExclusion {

        @Test
        @DisplayName("Should skip losses of the excluded parking ticker")
        void shouldSkipParkingTickerLosses() {
            List<Map<String, Object>> trades =
                    List.of(
                            trade("2026-09-11 10:00:00", "SNGSP", -100.0),
                            trade("2026-09-11 11:00:00", "OZON", -50.0),
                            trade("2026-09-11 12:00:00", "TMON@", -0.43));

            then(LossStreakMonitor.countConsecutiveLosses(trades, "TMON@")).isEqualTo(2);
        }

        @Test
        @DisplayName("Should not break streak at a profit of the excluded parking ticker")
        void shouldNotBreakStreak_AtParkingTickerProfit() {
            List<Map<String, Object>> trades =
                    List.of(
                            trade("2026-09-11 10:00:00", "SNGSP", -100.0),
                            trade("2026-09-11 11:00:00", "TMON@", 50.0),
                            trade("2026-09-11 12:00:00", "OZON", -50.0));

            then(LossStreakMonitor.countConsecutiveLosses(trades, "TMON@")).isEqualTo(2);
        }

        @Test
        @DisplayName("Should match the excluded ticker case-insensitively")
        void shouldIgnoreCase_WhenMatchingTicker() {
            List<Map<String, Object>> trades =
                    List.of(trade("2026-09-11 10:00:00", "tmon@", -0.43));

            then(LossStreakMonitor.countConsecutiveLosses(trades, "TMON@")).isZero();
        }

        @Test
        @DisplayName("Should count all losses when excluded ticker is empty")
        void shouldCountAll_WhenExcludedTickerEmpty() {
            List<Map<String, Object>> trades =
                    List.of(
                            trade("2026-09-11 10:00:00", "SNGSP", -100.0),
                            trade("2026-09-11 11:00:00", "TMON@", -0.43));

            then(LossStreakMonitor.countConsecutiveLosses(trades, "")).isEqualTo(2);
        }
    }

    @Nested
    @DisplayName("checkLossStreak")
    class CheckLossStreak {

        @Test
        @DisplayName("Should halt when streak reaches threshold")
        void shouldHalt_WhenStreakReachesThreshold() {
            AtomicBoolean halted = new AtomicBoolean(false);
            AtomicInteger haltCalls = new AtomicInteger(0);
            FakeTradingService service = new FakeTradingService(
                    List.of(trade("2026-09-11 10:00:00", -100.0), trade("2026-09-11 11:00:00", -50.0)));
            LossStreakMonitor monitor =
                    new LossStreakMonitor(service, 2, 60_000L, null, () -> {
                        halted.set(true);
                        haltCalls.incrementAndGet();
                    });

            monitor.checkLossStreak();

            then(halted).isTrue();
            then(haltCalls).hasValue(1);
        }

        @Test
        @DisplayName("Should not halt when streak is below threshold")
        void shouldNotHalt_WhenStreakBelowThreshold() {
            AtomicBoolean halted = new AtomicBoolean(false);
            FakeTradingService service = new FakeTradingService(
                    List.of(trade("2026-09-11 10:00:00", -100.0)));
            LossStreakMonitor monitor =
                    new LossStreakMonitor(service, 3, 60_000L, null, () -> halted.set(true));

            monitor.checkLossStreak();

            then(halted).isFalse();
        }

        @Test
        @DisplayName("Should not count parking ticker losses toward the halt threshold")
        void shouldNotCountParkingLosses_TowardThreshold() {
            AtomicBoolean halted = new AtomicBoolean(false);
            FakeTradingService service = new FakeTradingService(
                    List.of(
                            trade("2026-09-11 10:00:00", "SNGSP", -100.0),
                            trade("2026-09-11 11:00:00", "OZON", -50.0),
                            trade("2026-09-11 12:00:00", "TMON@", -0.43)));
            LossStreakMonitor monitor =
                    new LossStreakMonitor(service, 3, 60_000L, "TMON@", () -> halted.set(true));

            monitor.checkLossStreak();

            then(halted).isFalse();
        }

        @Test
        @DisplayName("Should not halt on API error")
        void shouldNotHalt_OnApiError() {
            AtomicBoolean halted = new AtomicBoolean(false);
            TradingService failingService = new TradingService() {
                @Override
                public List<Map<String, Object>> getTradeHistory(Instant since) {
                    throw new RuntimeException("API down");
                }
            };
            LossStreakMonitor monitor =
                    new LossStreakMonitor(failingService, 2, 60_000L, null, () -> halted.set(true));

            monitor.checkLossStreak();

            then(halted).isFalse();
        }

        @Test
        @DisplayName("Should not re-halt after already halted")
        void shouldNotReHalt_WhenAlreadyHalted() {
            AtomicInteger haltCalls = new AtomicInteger(0);
            FakeTradingService service = new FakeTradingService(
                    List.of(trade("2026-09-11 10:00:00", -100.0), trade("2026-09-11 11:00:00", -50.0)));
            LossStreakMonitor monitor =
                    new LossStreakMonitor(service, 2, 60_000L, null, haltCalls::incrementAndGet);

            monitor.checkLossStreak();
            monitor.checkLossStreak();

            then(haltCalls).hasValue(1);
        }
    }

    /** Minimal TradingService fake returning a fixed trade history. */
    private static class FakeTradingService implements TradingService {

        private final List<Map<String, Object>> trades;

        FakeTradingService(List<Map<String, Object>> trades) {
            this.trades = new ArrayList<>(trades);
        }

        @Override
        public List<Map<String, Object>> getTradeHistory(Instant since) {
            return trades;
        }

        @Override
        public TickerInfo searchTicker(TickerInfo.Key key) {
            String ticker = key.getTicker();
            return new TickerInfo("FIGI", ticker, "ISIN", 0.01, 1, "RUB", ticker, "STOCK");
        }
    }

    @Nested
    @DisplayName("stop() should halt the monitoring thread")
    class StopMonitoring {

        @Test
        @DisplayName("Should stop running flag when stop() is called")
        void shouldStopRunning_WhenStopCalled() throws Exception {
            FakeTradingService service = new FakeTradingService(List.of());
            LossStreakMonitor monitor = new LossStreakMonitor(service, 3, 1_000L, null, () -> {});
            Thread thread = new Thread(monitor, "test-monitor");
            thread.start();

            Thread.sleep(1_500);

            monitor.stop();
            thread.join(3_000);

            then(thread.isAlive()).isFalse();
        }

        @Test
        @DisplayName("Should not log continuously after stop() is called")
        void shouldNotLogContinuously_AfterStop() throws Exception {
            AtomicInteger checkCount = new AtomicInteger(0);
            FakeTradingService service = new FakeTradingService(
                    List.of(trade("2026-09-11 10:00:00", -100.0)));
            LossStreakMonitor monitor = new LossStreakMonitor(service, 3, 200L, null, () -> {});

            Thread thread = new Thread(() -> {
                monitor.checkLossStreak();
                monitor.stop();
                monitor.checkLossStreak();
            }, "test-monitor-no-loop");
            thread.start();
            thread.join(2_000);

            then(thread.isAlive()).isFalse();
        }
    }

    @Nested
    @DisplayName("EOD shutdown should call stop() on LossStreakMonitor")
    class EodShutdown {

        @Test
        @DisplayName("Should stop monitor before strategy exits")
        void shouldCallStopOnShutdown() throws Exception {
            AtomicBoolean stopped = new AtomicBoolean(false);
            FakeTradingService service = new FakeTradingService(List.of());
            LossStreakMonitor monitor = new LossStreakMonitor(
                    service, 3, 1_000L, null, () -> {});

            Thread thread = new Thread(monitor, "eod-test");
            thread.start();
            Thread.sleep(500);

            monitor.stop();
            thread.join(3_000);

            then(thread.isAlive()).isFalse();
        }
    }
}
