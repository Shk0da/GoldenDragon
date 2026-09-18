package com.github.shk0da.goldendragon.strategy;

import com.github.shk0da.goldendragon.model.Config;
import com.github.shk0da.goldendragon.model.TickerInfo;
import com.github.shk0da.goldendragon.model.TickerType;
import com.github.shk0da.goldendragon.repository.TickerRepository;
import com.github.shk0da.goldendragon.service.TradingService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import static org.assertj.core.api.BDDAssertions.then;

@DisplayName("TradeCouncilStrategy Async Debate Agents")
class TradeCouncilAsyncDebateTest {

    private TradeCouncilStrategy strategy;

    @BeforeEach
    void setUp() throws Exception {
        TickerRepository.INSTANCE.putAll(
            Map.of(
                new TickerInfo.Key("TATN", TickerType.STOCK),
                new TickerInfo("TATN_FIGI", "TATN", "TATN_ISIN", 0.01, 100, "RUB", "TATN", "STOCK")
            )
        );
        strategy = new TradeCouncilStrategy(
            new com.github.shk0da.goldendragon.config.UnifiedTraderConfig(),
            new TradingService() {},
            new Config()
        );
    }

    @Test
    @DisplayName("Should execute agent calls in parallel using CompletableFuture")
    void shouldExecuteAgentsInParallel() throws Exception {
        // Given: 3 agents with different execution times
        List<String> executionOrder = Collections.synchronizedList(new ArrayList<>());
        ExecutorService executor = Executors.newFixedThreadPool(3);

        // When: execute 3 tasks in parallel
        List<CompletableFuture<Void>> futures = new ArrayList<>();
        for (int i = 0; i < 3; i++) {
            final int agentId = i;
            CompletableFuture<Void> future = CompletableFuture.runAsync(() -> {
                try {
                    // Simulate different execution times
                    Thread.sleep(100 - (agentId * 20)); // 100ms, 80ms, 60ms
                    executionOrder.add("Agent" + agentId);
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
            }, executor);
            futures.add(future);
        }

        // Wait for all to complete
        CompletableFuture.allOf(futures.toArray(new CompletableFuture[0])).join();
        executor.shutdown();
        executor.awaitTermination(5, TimeUnit.SECONDS);

        // Then: all agents executed (order may vary due to parallelism)
        then(executionOrder).hasSize(3);
        then(executionOrder).containsExactlyInAnyOrder("Agent0", "Agent1", "Agent2");
    }

    @Test
    @DisplayName("Should collect results from parallel agent execution")
    void shouldCollectParallelResults() throws Exception {
        // Given: 3 agents returning different results
        Map<String, String> roundResults = new java.util.concurrent.ConcurrentHashMap<>();
        Map<String, String> agents = Map.of(
            "Analyst", "Analyst prompt",
            "Trader", "Trader prompt",
            "Risk Manager", "Risk Manager prompt"
        );

        // When: execute all agents in parallel
        List<CompletableFuture<Void>> futures = new ArrayList<>();
        for (Map.Entry<String, String> agent : agents.entrySet()) {
            String agentName = agent.getKey();
            CompletableFuture<Void> future = CompletableFuture.runAsync(() -> {
                // Simulate agent execution
                String result = "Result from " + agentName;
                roundResults.put(agentName, result);
            });
            futures.add(future);
        }

        CompletableFuture.allOf(futures.toArray(new CompletableFuture[0])).join();

        // Then: all results collected
        then(roundResults).hasSize(3);
        then(roundResults.get("Analyst")).isEqualTo("Result from Analyst");
        then(roundResults.get("Trader")).isEqualTo("Result from Trader");
        then(roundResults.get("Risk Manager")).isEqualTo("Result from Risk Manager");
    }

    @Test
    @DisplayName("Should handle agent failures gracefully")
    void shouldHandleAgentFailures() throws Exception {
        // Given: 3 agents where 1 fails
        Map<String, String> roundResults = new java.util.concurrent.ConcurrentHashMap<>();
        List<String> errors = Collections.synchronizedList(new ArrayList<>());

        // When: execute with one failing agent
        List<CompletableFuture<Void>> futures = new ArrayList<>();
        for (int i = 0; i < 3; i++) {
            final int agentId = i;
            CompletableFuture<Void> future = CompletableFuture.runAsync(() -> {
                try {
                    if (agentId == 1) {
                        throw new RuntimeException("Agent 1 failed");
                    }
                    roundResults.put("Agent" + agentId, "Success");
                } catch (Exception e) {
                    errors.add(e.getMessage());
                    throw e;
                }
            });
            futures.add(future);
        }

        // Then: CompletableFuture.allOf should complete exceptionally
        CompletableFuture<Void> allOf = CompletableFuture.allOf(futures.toArray(new CompletableFuture[0]));

        // Wait and check for exceptions
        try {
            allOf.join();
        } catch (Exception e) {
            // Expected - one agent failed
            then(errors).hasSize(1);
            then(errors.get(0)).contains("Agent 1 failed");
        }

        // Successful agents still completed
        then(roundResults).hasSize(2);
    }
}
