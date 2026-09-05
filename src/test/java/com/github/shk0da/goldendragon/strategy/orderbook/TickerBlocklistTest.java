package com.github.shk0da.goldendragon.strategy.orderbook;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.BDDAssertions.then;

@DisplayName("TickerBlocklist")
class TickerBlocklistTest {

    @Nested
    @DisplayName("When blocking tickers")
    class BlockingTickers {

        @Test
        @DisplayName("Should block ticker for specified duration")
        void shouldBlockTickerForSpecifiedDuration() {
            // Given
            TickerBlocklist blocklist = new TickerBlocklist();

            // When
            blocklist.block("AAPL", 5000); // 5 seconds

            // Then
            then(blocklist.isBlocked("AAPL")).isTrue();
        }

        @Test
        @DisplayName("Should not block with null ticker")
        void shouldNotBlock_WithNullTicker() {
            // Given
            TickerBlocklist blocklist = new TickerBlocklist();

            // When
            blocklist.block(null, 5000);

            // Then
            then(blocklist.size()).isZero();
        }

        @Test
        @DisplayName("Should not block with empty ticker")
        void shouldNotBlock_WithEmptyTicker() {
            // Given
            TickerBlocklist blocklist = new TickerBlocklist();

            // When
            blocklist.block("", 5000);

            // Then
            then(blocklist.size()).isZero();
        }

        @Test
        @DisplayName("Should not block with zero duration")
        void shouldNotBlock_WithZeroDuration() {
            // Given
            TickerBlocklist blocklist = new TickerBlocklist();

            // When
            blocklist.block("AAPL", 0);

            // Then
            then(blocklist.size()).isZero();
        }

        @Test
        @DisplayName("Should not block with negative duration")
        void shouldNotBlock_WithNegativeDuration() {
            // Given
            TickerBlocklist blocklist = new TickerBlocklist();

            // When
            blocklist.block("AAPL", -1000);

            // Then
            then(blocklist.size()).isZero();
        }

        @Test
        @DisplayName("Should block multiple tickers")
        void shouldBlockMultipleTickers() {
            // Given
            TickerBlocklist blocklist = new TickerBlocklist();

            // When
            blocklist.block("AAPL", 5000);
            blocklist.block("GOOGL", 5000);
            blocklist.block("MSFT", 5000);

            // Then
            then(blocklist.size()).isEqualTo(3);
            then(blocklist.isBlocked("AAPL")).isTrue();
            then(blocklist.isBlocked("GOOGL")).isTrue();
            then(blocklist.isBlocked("MSFT")).isTrue();
        }
    }

    @Nested
    @DisplayName("When checking blocked tickers")
    class CheckingBlockedTickers {

        @Test
        @DisplayName("Should return false for non-blocked ticker")
        void shouldReturnFalse_ForNonBlockedTicker() {
            // Given
            TickerBlocklist blocklist = new TickerBlocklist();

            // When
            boolean blocked = blocklist.isBlocked("AAPL");

            // Then
            then(blocked).isFalse();
        }

        @Test
        @DisplayName("Should return false for null ticker")
        void shouldReturnFalse_ForNullTicker() {
            // Given
            TickerBlocklist blocklist = new TickerBlocklist();

            // When
            boolean blocked = blocklist.isBlocked(null);

            // Then
            then(blocked).isFalse();
        }

        @Test
        @DisplayName("Should return true for blocked ticker")
        void shouldReturnTrue_ForBlockedTicker() {
            // Given
            TickerBlocklist blocklist = new TickerBlocklist();
            blocklist.block("AAPL", 10000);

            // When
            boolean blocked = blocklist.isBlocked("AAPL");

            // Then
            then(blocked).isTrue();
        }

        @Test
        @DisplayName("Should return false after block expires")
        void shouldReturnFalse_AfterBlockExpires() throws InterruptedException {
            // Given
            TickerBlocklist blocklist = new TickerBlocklist();
            blocklist.block("AAPL", 100); // 100ms

            // When - wait for expiration
            Thread.sleep(150);
            boolean blocked = blocklist.isBlocked("AAPL");

            // Then
            then(blocked).isFalse();
        }
    }

    @Nested
    @DisplayName("When getting remaining block time")
    class RemainingBlockTime {

        @Test
        @DisplayName("Should return remaining time for blocked ticker")
        void shouldReturnRemainingTime_ForBlockedTicker() {
            // Given
            TickerBlocklist blocklist = new TickerBlocklist();
            blocklist.block("AAPL", 5000);

            // When
            long remaining = blocklist.getRemainingMs("AAPL");

            // Then
            then(remaining).isPositive();
            then(remaining).isLessThanOrEqualTo(5000);
        }

        @Test
        @DisplayName("Should return zero for non-blocked ticker")
        void shouldReturnZero_ForNonBlockedTicker() {
            // Given
            TickerBlocklist blocklist = new TickerBlocklist();

            // When
            long remaining = blocklist.getRemainingMs("AAPL");

            // Then
            then(remaining).isZero();
        }

        @Test
        @DisplayName("Should return zero for null ticker")
        void shouldReturnZero_ForNullTicker() {
            // Given
            TickerBlocklist blocklist = new TickerBlocklist();

            // When
            long remaining = blocklist.getRemainingMs(null);

            // Then
            then(remaining).isZero();
        }

        @Test
        @DisplayName("Should return zero after expiration")
        void shouldReturnZero_AfterExpiration() throws InterruptedException {
            // Given
            TickerBlocklist blocklist = new TickerBlocklist();
            blocklist.block("AAPL", 100);

            // When
            Thread.sleep(150);
            long remaining = blocklist.getRemainingMs("AAPL");

            // Then
            then(remaining).isZero();
        }
    }

    @Nested
    @DisplayName("When unblocking tickers")
    class UnblockingTickers {

        @Test
        @DisplayName("Should unblock ticker")
        void shouldUnblockTicker() {
            // Given
            TickerBlocklist blocklist = new TickerBlocklist();
            blocklist.block("AAPL", 10000);

            // When
            blocklist.unblock("AAPL");

            // Then
            then(blocklist.isBlocked("AAPL")).isFalse();
            then(blocklist.size()).isZero();
        }

        @Test
        @DisplayName("Should handle unblocking non-blocked ticker")
        void shouldHandleUnblockingNonBlockedTicker() {
            // Given
            TickerBlocklist blocklist = new TickerBlocklist();

            // When
            blocklist.unblock("AAPL");

            // Then
            then(blocklist.size()).isZero();
        }

        @Test
        @DisplayName("Should handle unblocking null ticker")
        void shouldHandleUnblockingNullTicker() {
            // Given
            TickerBlocklist blocklist = new TickerBlocklist();

            // When
            blocklist.unblock(null);

            // Then
            then(blocklist.size()).isZero();
        }
    }

    @Nested
    @DisplayName("When cleaning up expired entries")
    class Cleanup {

        @Test
        @DisplayName("Should remove expired entries")
        void shouldRemoveExpiredEntries() throws InterruptedException {
            // Given
            TickerBlocklist blocklist = new TickerBlocklist();
            blocklist.block("AAPL", 100);
            blocklist.block("GOOGL", 100);
            blocklist.block("MSFT", 10000); // long duration

            // When
            Thread.sleep(150);
            blocklist.cleanup();

            // Then
            then(blocklist.size()).isEqualTo(1);
            then(blocklist.isBlocked("MSFT")).isTrue();
            then(blocklist.isBlocked("AAPL")).isFalse();
            then(blocklist.isBlocked("GOOGL")).isFalse();
        }

        @Test
        @DisplayName("Should handle cleanup with no expired entries")
        void shouldHandleCleanup_NoExpiredEntries() {
            // Given
            TickerBlocklist blocklist = new TickerBlocklist();
            blocklist.block("AAPL", 10000);
            blocklist.block("GOOGL", 10000);

            // When
            blocklist.cleanup();

            // Then
            then(blocklist.size()).isEqualTo(2);
        }

        @Test
        @DisplayName("Should handle cleanup with empty blocklist")
        void shouldHandleCleanup_EmptyBlocklist() {
            // Given
            TickerBlocklist blocklist = new TickerBlocklist();

            // When
            blocklist.cleanup();

            // Then
            then(blocklist.size()).isZero();
        }
    }

    @Nested
    @DisplayName("When getting blocked tickers")
    class GetBlockedTickers {

        @Test
        @DisplayName("Should return set of blocked tickers")
        void shouldReturnSetOfBlockedTickers() {
            // Given
            TickerBlocklist blocklist = new TickerBlocklist();
            blocklist.block("AAPL", 10000);
            blocklist.block("GOOGL", 10000);
            blocklist.block("MSFT", 10000);

            // When
            var blockedTickers = blocklist.getBlockedTickers();

            // Then
            then(blockedTickers).hasSize(3);
            then(blockedTickers).contains("AAPL", "GOOGL", "MSFT");
        }

        @Test
        @DisplayName("Should return empty set when no blocked tickers")
        void shouldReturnEmptySet_WhenNoBlockedTickers() {
            // Given
            TickerBlocklist blocklist = new TickerBlocklist();

            // When
            var blockedTickers = blocklist.getBlockedTickers();

            // Then
            then(blockedTickers).isEmpty();
        }

        @Test
        @DisplayName("Should exclude expired tickers")
        void shouldExcludeExpiredTickers() throws InterruptedException {
            // Given
            TickerBlocklist blocklist = new TickerBlocklist();
            blocklist.block("AAPL", 100);
            blocklist.block("GOOGL", 10000);

            // When
            Thread.sleep(150);
            var blockedTickers = blocklist.getBlockedTickers();

            // Then
            then(blockedTickers).hasSize(1);
            then(blockedTickers).contains("GOOGL");
            then(blockedTickers).doesNotContain("AAPL");
        }
    }
}
