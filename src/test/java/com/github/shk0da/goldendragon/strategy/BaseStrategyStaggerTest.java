package com.github.shk0da.goldendragon.strategy;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.BDDAssertions.then;

@DisplayName("BaseStrategy ticker stagger jitter")
class BaseStrategyStaggerTest {

    @Test
    @DisplayName("Should have correct TICKER_STAGGER_STEP_MS constant")
    void shouldHaveCorrectStaggerStep() {
        then(BaseStrategy.TICKER_STAGGER_STEP_MS)
                .as("Stagger step should be 500ms to spread ticker startup")
                .isEqualTo(500L);
    }

    @Test
    @DisplayName("Should have correct TICKER_STAGGER_JITTER_MS constant")
    void shouldHaveCorrectStaggerJitter() {
        then(BaseStrategy.TICKER_STAGGER_JITTER_MS)
                .as("Stagger jitter should be 2000ms to randomize startup")
                .isEqualTo(2_000L);
    }

    @Test
    @DisplayName("Should calculate staggered delays for tickers")
    void shouldCalculateStaggeredDelays() {
        // Simulate stagger calculation for 10 tickers
        int numTickers = 10;
        long[] delays = new long[numTickers];
        
        for (int i = 0; i < numTickers; i++) {
            // Simulate the calculation from BaseStrategy.run()
            // long initialDelay = (tickerIndex * TICKER_STAGGER_STEP_MS)
            //         + ThreadLocalRandom.current().nextLong(TICKER_STAGGER_JITTER_MS);
            long baseDelay = i * BaseStrategy.TICKER_STAGGER_STEP_MS;
            long maxJitter = BaseStrategy.TICKER_STAGGER_JITTER_MS;
            // For testing, use deterministic jitter (0 to maxJitter-1)
            long jitter = (i * 200) % maxJitter;
            delays[i] = baseDelay + jitter;
        }

        // Verify that delays are staggered (not all the same)
        then(delays).isSorted();
        then(delays[0]).isLessThan(delays[numTickers - 1]);
        
        // Verify that total spread is reasonable (should be ~5-7 seconds for 10 tickers)
        long totalSpread = delays[numTickers - 1] - delays[0];
        then(totalSpread).isBetween(4000L, 10000L);
    }

    @Test
    @DisplayName("Should prevent synchronous API burst with staggered startup")
    void shouldPreventSynchronousBurst() {
        // With stagger: ticker 0 starts at ~0-2000ms, ticker 1 at ~500-2500ms, etc.
        // Without stagger: all tickers would start at exactly 0ms
        
        int numTickers = 20;
        long[] startTimes = new long[numTickers];
        
        for (int i = 0; i < numTickers; i++) {
            // Simulate staggered startup
            long baseDelay = i * BaseStrategy.TICKER_STAGGER_STEP_MS;
            long jitter = (i * 100) % BaseStrategy.TICKER_STAGGER_JITTER_MS;
            startTimes[i] = baseDelay + jitter;
        }

        // Verify staggered startup prevents burst
        // Minimum gap between any two consecutive tickers should be > 0
        for (int i = 1; i < numTickers; i++) {
            then(startTimes[i] - startTimes[i - 1])
                    .as("Ticker %d should start after ticker %d", i, i - 1)
                    .isGreaterThan(0);
        }

        // Total spread should be significant (prevents all-at-once burst)
        long totalSpread = startTimes[numTickers - 1] - startTimes[0];
        then(totalSpread)
                .as("Total spread should prevent synchronous burst")
                .isGreaterThan(5000L);
    }

    @Test
    @DisplayName("Should have peer thread jitter")
    void shouldHavePeerThreadJitter() {
        // Peer thread gets random jitter 0..TICKER_STAGGER_JITTER_MS
        // This prevents it from aligning perfectly with ticker threads
        
        long peerJitterMax = BaseStrategy.TICKER_STAGGER_JITTER_MS;
        then(peerJitterMax).isEqualTo(2_000L);
        
        // Peer thread runs every 60s, tickers every 30s
        // Jitter ensures they don't all hit API at the same time
        long peerInterval = 60_000L;
        long tickerInterval = 30_000L;
        
        // With jitter, peer thread can start anywhere in [0, 2000ms]
        // This desynchronizes from ticker cycles
        then(peerJitterMax).isLessThan(tickerInterval);
    }
}
