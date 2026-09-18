package com.github.shk0da.goldendragon.service;

import com.github.shk0da.goldendragon.model.TickerInfo;
import com.github.shk0da.goldendragon.model.TickerType;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.util.Map;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

@DisplayName("TradingServiceCache")
class TradingServiceCacheTest {

    @Test
    @DisplayName("Should cache getAvailableCash for TTL duration")
    void shouldCacheGetAvailableCash() {
        TradingService mockDelegate = mock(TradingService.class);
        when(mockDelegate.getAvailableCash()).thenReturn(1000.0, 2000.0);

        TradingServiceCache cache = new TradingServiceCache(mockDelegate);

        // First call - should call delegate
        double cash1 = cache.getAvailableCash();
        verify(mockDelegate).getAvailableCash();
        assertEquals(1000.0, cash1);

        // Second call within TTL - should use cache
        double cash2 = cache.getAvailableCash();
        verifyNoMoreInteractions(mockDelegate);
        assertEquals(1000.0, cash2);
    }

    @Test
    @DisplayName("Should return cached value for getLiveAskPrice")
    void shouldCacheGetLiveAskPrice() {
        TradingService mockDelegate = mock(TradingService.class);
        when(mockDelegate.getLiveAskPrice(new TickerInfo.Key("TEST", TickerType.STOCK)))
                .thenReturn(100.0);

        TradingServiceCache cache = new TradingServiceCache(mockDelegate);
        TickerInfo.Key key = new TickerInfo.Key("TEST", TickerType.STOCK);

        double price1 = cache.getLiveAskPrice(key);
        assertEquals(100.0, price1);

        // Second call should use cache
        double price2 = cache.getLiveAskPrice(key);
        assertEquals(100.0, price2);
    }

    @Test
    @DisplayName("Should invalidate cash cache after order")
    void shouldInvalidateCashAfterOrder() {
        TradingService mockDelegate = mock(TradingService.class);
        when(mockDelegate.getAvailableCash()).thenReturn(1000.0);

        TradingServiceCache cache = new TradingServiceCache(mockDelegate);
        cache.getAvailableCash();
        verify(mockDelegate, times(1)).getAvailableCash();

        // Execute order - should call delegate and invalidate cash cache
        cache.buyByMarketWithDetails("TEST", TickerType.STOCK, 100.0, 0.0, 0.0);
        verify(mockDelegate).buyByMarketWithDetails("TEST", TickerType.STOCK, 100.0, 0.0, 0.0);

        // Next getAvailableCash after order should be a cache miss (delegate called again)
        when(mockDelegate.getAvailableCash()).thenReturn(2000.0);
        cache.getAvailableCash();
        verify(mockDelegate, times(2)).getAvailableCash();
    }

    @Test
    @DisplayName("Should invalidate positions cache after closing position with quantity")
    void shouldInvalidatePositionsAfterCloseWithQuantity() {
        TradingService mockDelegate = mock(TradingService.class);
        when(mockDelegate.getCurrentPositions(TickerType.STOCK)).thenReturn(Map.of());
        when(mockDelegate.closeLongByMarketWithDetails("TEST", TickerType.STOCK, 10))
                .thenReturn(null);

        TradingServiceCache cache = new TradingServiceCache(mockDelegate);

        // Populate positions cache
        cache.getCurrentPositions(TickerType.STOCK);
        verify(mockDelegate, times(1)).getCurrentPositions(TickerType.STOCK);

        // Close position - should invalidate positions cache
        cache.closeLongByMarketWithDetails("TEST", TickerType.STOCK, 10);
        verify(mockDelegate).closeLongByMarketWithDetails("TEST", TickerType.STOCK, 10);

        // Next getCurrentPositions after close should be a cache miss (delegate called again)
        cache.getCurrentPositions(TickerType.STOCK);
        verify(mockDelegate, times(2)).getCurrentPositions(TickerType.STOCK);
    }

    @Test
    @DisplayName("Should track cache size")
    void shouldTrackCacheSize() {
        TradingService mockDelegate = mock(TradingService.class);
        when(mockDelegate.getAvailableCash()).thenReturn(1000.0);

        TradingServiceCache cache = new TradingServiceCache(mockDelegate);
        assertEquals(0, cache.getCacheSize());

        // Trigger cache population
        cache.getAvailableCash();
        assertTrue(cache.getCacheSize() > 0);
    }

    @Test
    @DisplayName("Should delegate getInitialBalance without caching")
    void shouldDelegateGetInitialBalance() {
        TradingService mockDelegate = mock(TradingService.class);
        when(mockDelegate.getInitialBalance()).thenReturn(50000.0);

        TradingServiceCache cache = new TradingServiceCache(mockDelegate);

        double balance = cache.getInitialBalance();
        assertEquals(50000.0, balance);
        verify(mockDelegate).getInitialBalance();
    }

    @Test
    @DisplayName("Should cache getTotalPortfolioValue for TTL duration")
    void shouldCacheGetTotalPortfolioValue() {
        TradingService mockDelegate = mock(TradingService.class);
        when(mockDelegate.getTotalPortfolioValue()).thenReturn(100000.0, 200000.0);

        TradingServiceCache cache = new TradingServiceCache(mockDelegate);

        // First call - should call delegate
        double value1 = cache.getTotalPortfolioValue();
        verify(mockDelegate).getTotalPortfolioValue();
        assertEquals(100000.0, value1);

        // Second call within TTL - should use cache
        double value2 = cache.getTotalPortfolioValue();
        verifyNoMoreInteractions(mockDelegate);
        assertEquals(100000.0, value2);
    }

    @Test
    @DisplayName("Should cache getTotalPortfolioCost and share cache with getTotalPortfolioValue")
    void shouldShareCacheBetweenPortfolioMethods() {
        TradingService mockDelegate = mock(TradingService.class);
        when(mockDelegate.getTotalPortfolioCost()).thenReturn(150000.0);
        when(mockDelegate.getTotalPortfolioValue()).thenReturn(150000.0);

        TradingServiceCache cache = new TradingServiceCache(mockDelegate);

        // First call to getTotalPortfolioCost - should call delegate
        double cost1 = cache.getTotalPortfolioCost();
        verify(mockDelegate).getTotalPortfolioCost();
        assertEquals(150000.0, cost1);

        // Call getTotalPortfolioValue - should use same cache (delegate not called again)
        double value1 = cache.getTotalPortfolioValue();
        verifyNoMoreInteractions(mockDelegate);
        assertEquals(150000.0, value1);

        // Next call to getTotalPortfolioCost - should use cache
        double cost2 = cache.getTotalPortfolioCost();
        verifyNoMoreInteractions(mockDelegate);
        assertEquals(150000.0, cost2);
    }

    @Test
    @DisplayName("Should cache portfolio value across multiple ticker calls")
    void shouldCachePortfolioValueAcrossMultipleTickerCalls() {
        TradingService mockDelegate = mock(TradingService.class);
        when(mockDelegate.getTotalPortfolioValue()).thenReturn(500000.0);

        TradingServiceCache cache = new TradingServiceCache(mockDelegate);

        // Simulate N ticker calls to getTotalPortfolioValue (like UnifiedStrategy.decide)
        for (int i = 0; i < 10; i++) {
            double value = cache.getTotalPortfolioValue();
            assertEquals(500000.0, value);
        }

        // Delegate should be called only ONCE, not 10 times
        verify(mockDelegate, times(1)).getTotalPortfolioValue();
    }

    @Test
    @DisplayName("Should return stale cache when loader fails")
    void shouldReturnStaleCacheOnLoaderFailure() throws Exception {
        AtomicInteger callCount = new AtomicInteger(0);
        
        // Create mock with custom answer for default method
        TradingService mockDelegate = new TradingService() {
            @Override
            public double getTotalPortfolioCost() {
                int count = callCount.incrementAndGet();
                if (count == 1) {
                    return 100000.0;  // First call succeeds
                } else {
                    throw new RuntimeException("API unavailable"); // Second call fails
                }
            }
        };

        TradingServiceCache cache = new TradingServiceCache(mockDelegate);

        // First call - succeeds and caches
        double value1 = cache.getTotalPortfolioCost();
        assertEquals(100000.0, value1);

        // Wait for TTL to expire
        Thread.sleep(5100); // 5.1 seconds

        // Second call - loader fails, but should return stale cache
        double value2 = cache.getTotalPortfolioCost();
        assertEquals(100000.0, value2); // Returns stale value, no exception
    }

    @Test
    @DisplayName("Should prevent cache stampede - only one API call when multiple threads request")
    void shouldPreventCacheStampede() throws Exception {
        TradingService mockDelegate = mock(TradingService.class);
        when(mockDelegate.getTotalPortfolioCost())
            .thenReturn(100000.0);  // Always returns same value

        TradingServiceCache cache = new TradingServiceCache(mockDelegate);

        // First call to populate cache
        cache.getTotalPortfolioCost();
        
        // Wait for TTL to expire
        Thread.sleep(5100);

        // Simulate 10 threads calling simultaneously after TTL expires
        CountDownLatch latch = new CountDownLatch(10);
        ExecutorService executor = Executors.newFixedThreadPool(10);
        
        for (int i = 0; i < 10; i++) {
            executor.submit(() -> {
                try {
                    cache.getTotalPortfolioCost();
                } finally {
                    latch.countDown();
                }
            });
        }
        
        latch.await(10, TimeUnit.SECONDS);
        executor.shutdown();

        // Verify delegate was called only ONCE for refresh (1 initial + 1 refresh = 2 total)
        verify(mockDelegate, times(2)).getTotalPortfolioCost();
    }
}
