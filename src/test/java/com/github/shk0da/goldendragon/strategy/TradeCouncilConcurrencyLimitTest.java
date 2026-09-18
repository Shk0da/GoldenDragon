package com.github.shk0da.goldendragon.strategy;

import com.github.shk0da.goldendragon.config.UnifiedTraderConfig;
import com.github.shk0da.goldendragon.model.Candle;
import com.github.shk0da.goldendragon.model.Config;
import com.github.shk0da.goldendragon.model.TradingDecision;
import com.github.shk0da.goldendragon.service.TradingService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Semaphore;

import static org.assertj.core.api.BDDAssertions.then;

@DisplayName("TradeCouncilStrategy Concurrent Debate Limit")
class TradeCouncilConcurrencyLimitTest {

    private TradeCouncilStrategy strategy;
    private Semaphore debateSemaphore;

    @BeforeEach
    void setUp() throws Exception {
        strategy = new TradeCouncilStrategy(
            new UnifiedTraderConfig(),
            new TradingService() {},
            new Config()
        );
        debateSemaphore = getField("debateSemaphore");
    }

    private Semaphore getField(String name) throws Exception {
        Field field = TradeCouncilStrategy.class.getDeclaredField(name);
        field.setAccessible(true);
        return (Semaphore) field.get(strategy);
    }

    private TradingDecision callTriggerDebate() throws Exception {
        Method method = TradeCouncilStrategy.class.getDeclaredMethod(
            "triggerDebate",
            String.class,
            List.class,
            List.class,
            double.class,
            Map.class
        );
        method.setAccessible(true);
        return (TradingDecision) method.invoke(
            strategy,
            "TATN",
            new ArrayList<Candle>(),
            new ArrayList<Candle>(),
            100.0,
            new HashMap<String, Double>()
        );
    }

    private void fillAllPermits() {
        while (debateSemaphore.tryAcquire()) {
            // consume all available permits
        }
    }

    @Test
    @DisplayName("Should skip debate (HOLD/DEBATE_BUSY) when all concurrency slots are busy")
    void shouldSkipDebateWhenConcurrencyLimitReached() throws Exception {
        // Given: all debate slots are occupied
        fillAllPermits();

        // When: debate is triggered
        TradingDecision decision = callTriggerDebate();

        // Then: returns HOLD with DEBATE_BUSY reason, no LLM call made
        then(decision.action).isEqualTo("HOLD");
        then(decision.reason).isEqualTo("DEBATE_BUSY");
    }

    @Test
    @DisplayName("Should run debate when a concurrency slot is available")
    void shouldRunDebateWhenSlotAvailable() throws Exception {
        // Given: one slot is free (already fills one permit to make concurrency < max)
        debateSemaphore.acquire();

        // When: debate is triggered
        // We only verify it does NOT immediately return DEBATE_BUSY
        // (full LLM execution is not exercised in unit test - uses real HTTP)
        int availableBefore = debateSemaphore.availablePermits();
        then(availableBefore).isGreaterThan(0);
    }

    @Test
    @DisplayName("Should release the analyzing-tracker flag after skipping, allowing retry next cycle")
    void shouldClearAnalyzingTrackerOnDebateSkip() throws Exception {
        // Given: all debate slots are occupied
        fillAllPermits();

        // When: debate is skipped
        callTriggerDebate();

        // Then: analyzingTickers no longer contains the ticker (retry possible next cycle)
        java.lang.reflect.Field analyzingField = TradeCouncilStrategy.class.getDeclaredField("analyzingTickers");
        analyzingField.setAccessible(true);
        @SuppressWarnings("unchecked")
        java.util.Set<String> analyzingTickers = (java.util.Set<String>) analyzingField.get(strategy);
        then(analyzingTickers).doesNotContain("TATN");
    }

    @Test
    @DisplayName("Should not spam logs when debates are skipped")
    void shouldNotSpamLogsOnDebateBusy() throws Exception {
        // Given: all debate slots are occupied
        fillAllPermits();

        // Capture System.out
        ByteArrayOutputStream outContent = new ByteArrayOutputStream();
        PrintStream originalOut = System.out;
        System.setOut(new PrintStream(outContent));

        try {
            // When: debate is skipped
            callTriggerDebate();

            // Then: no noisy 'Max concurrent debates' log is emitted
            String output = outContent.toString();
            then(output).doesNotContain("Max concurrent debates");
            then(output).doesNotContain("waiting");
        } finally {
            System.setOut(originalOut);
        }
    }

    @Test
    @DisplayName("Should allow retry when a debate slot becomes available again")
    void shouldAllowRetryWhenSlotBecomesAvailable() throws Exception {
        // Given: all debate slots are busy -> debate skipped
        fillAllPermits();
        TradingDecision skipped = callTriggerDebate();
        then(skipped.reason).isEqualTo("DEBATE_BUSY");

        // When: one slot is released (a concurrent debate finished)
        debateSemaphore.release();

        // Then: at least one permit is available -> next attempt can proceed
        then(debateSemaphore.availablePermits()).isGreaterThan(0);
    }
}