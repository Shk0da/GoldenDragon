package com.github.shk0da.goldendragon.filters;

import com.github.shk0da.goldendragon.model.Candle;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.assertj.core.api.BDDAssertions.then;

/**
 * BDD-style tests for {@link GroupConfirmationFilter}.
 *
 * Filter requires MIN_PEER_CONFIRMATIONS = 2 peers moving in same direction.
 * LOOKBACK_BARS = 3 (compares current close vs close 3 bars ago).
 */
@DisplayName("Group Confirmation Filter - Peer Validation")
class GroupConfirmationFilterTest {

    @Nested
    @DisplayName("When peer data is empty or null")
    class NoPeerData {

        @Test
        @DisplayName("Should allow trading with null peer candles")
        void shouldAllowTrading_WhenNullPeerCandles() {
            // Given/When
            boolean confirmed = GroupConfirmationFilter.isConfirmed(
                    "TICKER", true, null);

            // Then
            then(confirmed).isTrue()
                    .as("Should allow trading when no peer data available");
        }

        @Test
        @DisplayName("Should allow trading with empty peer candles")
        void shouldAllowTrading_WhenEmptyPeerCandles() {
            // Given/When
            boolean confirmed = GroupConfirmationFilter.isConfirmed(
                    "TICKER", true, new HashMap<>());

            // Then
            then(confirmed).isTrue()
                    .as("Should allow trading when no peer data available");
        }
    }

    @Nested
    @DisplayName("When peer has insufficient data")
    class InsufficientPeerData {

        @Test
        @DisplayName("Should allow trading when peer has less than 4 candles")
        void shouldAllowTrading_WhenPeerHasInsufficientData() {
            // Given
            Map<String, List<Candle>> peerCandles = new HashMap<>();
            peerCandles.put("PEER1", buildCandles(3)); // Less than LOOKBACK_BARS + 1 = 4

            // When
            boolean confirmed = GroupConfirmationFilter.isConfirmed(
                    "TICKER", true, peerCandles);

            // Then
            then(confirmed).isFalse()
                    .as("Should not confirm when peer has insufficient data");
        }
    }

    @Nested
    @DisplayName("When checking peer confirmations")
    class PeerConfirmations {

        @Test
        @DisplayName("Should confirm when 3 peers move up (for buy signal)")
        void shouldConfirm_WhenAllPeersMoveUp() {
            // Given
            String ticker = "TICKER";
            boolean isBuy = true;
            Map<String, List<Candle>> peerCandles = new HashMap<>();
            
            peerCandles.put("PEER1", buildBullishCandles(5));
            peerCandles.put("PEER2", buildBullishCandles(5));
            peerCandles.put("PEER3", buildBullishCandles(5));

            // When
            boolean confirmed = GroupConfirmationFilter.isConfirmed(ticker, isBuy, peerCandles);

            // Then
            then(confirmed).isTrue()
                    .as("3 peers moving up should confirm buy signal");
        }

        @Test
        @DisplayName("Should not confirm when all peers move down (for buy signal)")
        void shouldNotConfirm_WhenAllPeersMoveDown() {
            // Given
            String ticker = "TICKER";
            boolean isBuy = true;
            Map<String, List<Candle>> peerCandles = new HashMap<>();
            
            peerCandles.put("PEER1", buildBearishCandles(5));
            peerCandles.put("PEER2", buildBearishCandles(5));
            peerCandles.put("PEER3", buildBearishCandles(5));

            // When
            boolean confirmed = GroupConfirmationFilter.isConfirmed(ticker, isBuy, peerCandles);

            // Then
            then(confirmed).isFalse()
                    .as("Peers moving down should not confirm buy signal");
        }

        @Test
        @DisplayName("Should confirm when exactly 2 peers confirm (minimum)")
        void shouldConfirm_WhenExactlyTwoPeersConfirm() {
            // Given
            String ticker = "TICKER";
            boolean isBuy = true;
            Map<String, List<Candle>> peerCandles = new HashMap<>();
            
            // Peer 1: moving up (confirms)
            peerCandles.put("PEER1", buildBullishCandles(5));
            // Peer 2: moving up (confirms)
            peerCandles.put("PEER2", buildBullishCandles(5));
            // Peer 3: moving down (doesn't confirm)
            peerCandles.put("PEER3", buildBearishCandles(5));

            // When
            boolean confirmed = GroupConfirmationFilter.isConfirmed(ticker, isBuy, peerCandles);

            // Then
            then(confirmed).isTrue()
                    .as("Exactly 2 confirming peers should be sufficient");
        }

        @Test
        @DisplayName("Should not confirm when only 1 peer confirms")
        void shouldNotConfirm_WhenOnlyOnePeerConfirms() {
            // Given
            String ticker = "TICKER";
            boolean isBuy = true;
            Map<String, List<Candle>> peerCandles = new HashMap<>();
            
            // Peer 1: moving up (confirms)
            peerCandles.put("PEER1", buildBullishCandles(5));
            // Peer 2: moving down (doesn't confirm)
            peerCandles.put("PEER2", buildBearishCandles(5));
            // Peer 3: moving down (doesn't confirm)
            peerCandles.put("PEER3", buildBearishCandles(5));

            // When
            boolean confirmed = GroupConfirmationFilter.isConfirmed(ticker, isBuy, peerCandles);

            // Then
            then(confirmed).isFalse()
                    .as("Only 1 confirming peer should not be sufficient");
        }

        @Test
        @DisplayName("Should skip ticker itself from peer list")
        void shouldSkipSelfFromPeers() {
            // Given
            String ticker = "TICKER";
            boolean isBuy = true;
            Map<String, List<Candle>> peerCandles = new HashMap<>();
            
            // Self: moving down (should be ignored)
            peerCandles.put("TICKER", buildBearishCandles(5));
            // Peer 1: moving up (confirms)
            peerCandles.put("PEER1", buildBullishCandles(5));
            // Peer 2: moving up (confirms)
            peerCandles.put("PEER2", buildBullishCandles(5));

            // When
            boolean confirmed = GroupConfirmationFilter.isConfirmed(ticker, isBuy, peerCandles);

            // Then
            then(confirmed).isTrue()
                    .as("Ticker itself should be excluded from peer count");
        }

        @Test
        @DisplayName("Should skip null peer candles from count")
        void shouldHandleNullPeerCandles() {
            // Given
            String ticker = "TICKER";
            boolean isBuy = true;
            Map<String, List<Candle>> peerCandles = new HashMap<>();
            peerCandles.put("PEER1", null); // Skipped
            peerCandles.put("PEER2", buildBullishCandles(5)); // Confirms
            peerCandles.put("PEER3", buildBullishCandles(5)); // Confirms

            // When
            boolean confirmed = GroupConfirmationFilter.isConfirmed(ticker, isBuy, peerCandles);

            // Then
            then(confirmed).isTrue()
                    .as("Null peer should be skipped, remaining peers should confirm");
        }
    }

    @Nested
    @DisplayName("When testing sell signals")
    class SellSignals {

        @Test
        @DisplayName("Should confirm sell when 3 peers moving down")
        void shouldConfirmSell_WhenAllPeersMovingDown() {
            // Given
            String ticker = "TICKER";
            boolean isBuy = false; // Sell signal
            Map<String, List<Candle>> peerCandles = new HashMap<>();
            
            peerCandles.put("PEER1", buildBearishCandles(5));
            peerCandles.put("PEER2", buildBearishCandles(5));
            peerCandles.put("PEER3", buildBearishCandles(5));

            // When
            boolean confirmed = GroupConfirmationFilter.isConfirmed(ticker, isBuy, peerCandles);

            // Then
            then(confirmed).isTrue()
                    .as("Peers moving down should confirm sell signal");
        }

        @Test
        @DisplayName("Should not confirm sell when all peers moving up")
        void shouldNotConfirmSell_WhenAllPeersMovingUp() {
            // Given
            String ticker = "TICKER";
            boolean isBuy = false; // Sell signal
            Map<String, List<Candle>> peerCandles = new HashMap<>();
            
            peerCandles.put("PEER1", buildBullishCandles(5));
            peerCandles.put("PEER2", buildBullishCandles(5));
            peerCandles.put("PEER3", buildBullishCandles(5));

            // When
            boolean confirmed = GroupConfirmationFilter.isConfirmed(ticker, isBuy, peerCandles);

            // Then
            then(confirmed).isFalse()
                    .as("Peers moving up should not confirm sell signal");
        }
    }

    // ===== Helper methods =====

    private List<Candle> buildCandles(int count) {
        List<Candle> candles = new ArrayList<>();
        for (int i = 0; i < count; i++) {
            candles.add(new Candle(
                    "2024-01-01 10:00:00",
                    100.0, 101.0, 99.0, 100.5, 100000L
            ));
        }
        return candles;
    }

    private List<Candle> buildBullishCandles(int count) {
        List<Candle> candles = new ArrayList<>();
        for (int i = 0; i < count; i++) {
            candles.add(new Candle(
                    "2024-01-01 10:00:00",
                    100.0 + i,  // rising open
                    101.0 + i,
                    99.0 + i,
                    100.5 + i,  // rising close
                    100000L
            ));
        }
        return candles;
    }

    private List<Candle> buildBearishCandles(int count) {
        List<Candle> candles = new ArrayList<>();
        for (int i = 0; i < count; i++) {
            candles.add(new Candle(
                    "2024-01-01 10:00:00",
                    100.0 - i,  // falling open
                    101.0 - i,
                    99.0 - i,
                    100.5 - i,  // falling close
                    100000L
            ));
        }
        return candles;
    }
}
