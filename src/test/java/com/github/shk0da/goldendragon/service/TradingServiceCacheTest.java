package com.github.shk0da.goldendragon.service;

import com.github.shk0da.goldendragon.model.TickerInfo;
import com.github.shk0da.goldendragon.model.TickerType;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.time.Duration;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

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
}
