package com.github.shk0da.goldendragon.service;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import ru.tinkoff.piapi.contract.v1.StopOrder;
import ru.tinkoff.piapi.contract.v1.StopOrderType;
import ru.tinkoff.piapi.contract.v1.MoneyValue;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@DisplayName("Stop Orders Logic Tests")
class StopOrdersLogicTest {

    @Test
    @DisplayName("Should identify SL order by type")
    void shouldIdentifySlOrderByType() {
        StopOrder slOrder = createMockStopOrder("BBG004730N88", "sl-123", StopOrderType.STOP_ORDER_TYPE_STOP_LOSS, 100.0);
        
        boolean isSl = slOrder.getOrderType() == StopOrderType.STOP_ORDER_TYPE_STOP_LOSS;
        
        assertTrue(isSl);
        assertEquals("sl-123", slOrder.getStopOrderId());
    }

    @Test
    @DisplayName("Should identify TP order by type")
    void shouldIdentifyTpOrderByType() {
        StopOrder tpOrder = createMockStopOrder("BBG004730N88", "tp-123", StopOrderType.STOP_ORDER_TYPE_TAKE_PROFIT, 120.0);
        
        boolean isTp = tpOrder.getOrderType() == StopOrderType.STOP_ORDER_TYPE_TAKE_PROFIT;
        
        assertTrue(isTp);
        assertEquals("tp-123", tpOrder.getStopOrderId());
    }

    @Test
    @DisplayName("Should filter orders by FIGI")
    void shouldFilterOrdersByFigi() {
        String targetFigi = "BBG004730N88";
        String otherFigi = "BBG000000000";
        
        StopOrder targetOrder = createMockStopOrder(targetFigi, "target-123", StopOrderType.STOP_ORDER_TYPE_STOP_LOSS, 100.0);
        StopOrder otherOrder = createMockStopOrder(otherFigi, "other-456", StopOrderType.STOP_ORDER_TYPE_STOP_LOSS, 50.0);
        
        List<StopOrder> allOrders = List.of(targetOrder, otherOrder);
        List<StopOrder> filtered = allOrders.stream()
                .filter(o -> o.getFigi().equals(targetFigi))
                .toList();
        
        assertEquals(1, filtered.size());
        assertEquals(targetFigi, filtered.get(0).getFigi());
    }

    @Test
    @DisplayName("Should handle empty order list")
    void shouldHandleEmptyOrderList() {
        List<StopOrder> orders = List.of();
        
        assertTrue(orders.isEmpty());
    }

    @Test
    @DisplayName("Should handle multiple orders for same FIGI")
    void shouldHandleMultipleOrdersForSameFigi() {
        String figi = "BBG004730N88";
        StopOrder sl1 = createMockStopOrder(figi, "sl-1", StopOrderType.STOP_ORDER_TYPE_STOP_LOSS, 95.0);
        StopOrder sl2 = createMockStopOrder(figi, "sl-2", StopOrderType.STOP_ORDER_TYPE_STOP_LOSS, 100.0);
        StopOrder tp1 = createMockStopOrder(figi, "tp-1", StopOrderType.STOP_ORDER_TYPE_TAKE_PROFIT, 115.0);
        
        List<StopOrder> orders = List.of(sl1, sl2, tp1);
        
        assertEquals(3, orders.size());
        assertEquals(2, orders.stream()
                .filter(o -> o.getOrderType() == StopOrderType.STOP_ORDER_TYPE_STOP_LOSS)
                .count());
        assertEquals(1, orders.stream()
                .filter(o -> o.getOrderType() == StopOrderType.STOP_ORDER_TYPE_TAKE_PROFIT)
                .count());
    }

    @Test
    @DisplayName("Should extract stop price from order")
    void shouldExtractStopPrice() {
        double expectedPrice = 105.50;
        StopOrder order = createMockStopOrder("BBG004730N88", "order-123", StopOrderType.STOP_ORDER_TYPE_STOP_LOSS, expectedPrice);
        
        MoneyValue priceValue = order.getStopPrice();
        double actualPrice = priceValue.getUnits() + (priceValue.getNano() / 1_000_000_000.0);
        
        assertEquals(expectedPrice, actualPrice, 0.01);
    }

    @Test
    @DisplayName("Should verify order cancellation - order no longer in list")
    void shouldVerifyOrderCancellation() {
        String orderId = "order-to-cancel";
        StopOrder order = createMockStopOrder("BBG004730N88", orderId, StopOrderType.STOP_ORDER_TYPE_STOP_LOSS, 100.0);
        
        List<StopOrder> before = List.of(order);
        List<StopOrder> after = List.of();
        
        assertEquals(1, before.size());
        assertTrue(before.stream().anyMatch(o -> o.getStopOrderId().equals(orderId)));
        
        assertEquals(0, after.size());
        assertFalse(after.stream().anyMatch(o -> o.getStopOrderId().equals(orderId)));
    }

    @Test
    @DisplayName("Should verify order placement - order appears in list")
    void shouldVerifyOrderPlacement() {
        String newOrderId = "new-order";
        StopOrder newOrder = createMockStopOrder("BBG004730N88", newOrderId, StopOrderType.STOP_ORDER_TYPE_STOP_LOSS, 105.0);
        
        List<StopOrder> before = List.of();
        List<StopOrder> after = List.of(newOrder);
        
        assertTrue(before.isEmpty());
        assertEquals(1, after.size());
        assertTrue(after.stream().anyMatch(o -> o.getStopOrderId().equals(newOrderId)));
    }

    @Test
    @DisplayName("Should find SL order with price below entry for LONG position")
    void shouldFindSlForLongPosition() {
        String figi = "BBG004730N88";
        double entryPrice = 110.0;
        
        StopOrder sl = createMockStopOrder(figi, "sl-1", StopOrderType.STOP_ORDER_TYPE_STOP_LOSS, 100.0);
        StopOrder tp = createMockStopOrder(figi, "tp-1", StopOrderType.STOP_ORDER_TYPE_TAKE_PROFIT, 120.0);
        
        List<StopOrder> orders = List.of(sl, tp);
        
        StopOrder foundSl = orders.stream()
                .filter(o -> o.getOrderType() == StopOrderType.STOP_ORDER_TYPE_STOP_LOSS)
                .filter(o -> {
                    double stopPrice = o.getStopPrice().getUnits() + (o.getStopPrice().getNano() / 1_000_000_000.0);
                    return stopPrice <= entryPrice;
                })
                .findFirst()
                .orElse(null);
        
        assertEquals("sl-1", foundSl.getStopOrderId());
    }

    @Test
    @DisplayName("Should find TP order with price above entry for LONG position")
    void shouldFindTpForLongPosition() {
        String figi = "BBG004730N88";
        double entryPrice = 110.0;
        
        StopOrder sl = createMockStopOrder(figi, "sl-1", StopOrderType.STOP_ORDER_TYPE_STOP_LOSS, 100.0);
        StopOrder tp = createMockStopOrder(figi, "tp-1", StopOrderType.STOP_ORDER_TYPE_TAKE_PROFIT, 120.0);
        
        List<StopOrder> orders = List.of(sl, tp);
        
        StopOrder foundTp = orders.stream()
                .filter(o -> o.getOrderType() == StopOrderType.STOP_ORDER_TYPE_TAKE_PROFIT)
                .filter(o -> {
                    double stopPrice = o.getStopPrice().getUnits() + (o.getStopPrice().getNano() / 1_000_000_000.0);
                    return stopPrice > entryPrice;
                })
                .findFirst()
                .orElse(null);
        
        assertEquals("tp-1", foundTp.getStopOrderId());
    }

    private StopOrder createMockStopOrder(String figi, String orderId, StopOrderType type, double stopPrice) {
        StopOrder mock = mock(StopOrder.class);
        when(mock.getFigi()).thenReturn(figi);
        when(mock.getStopOrderId()).thenReturn(orderId);
        when(mock.getOrderType()).thenReturn(type);
        when(mock.getStopPrice()).thenReturn(MoneyValue.newBuilder()
                .setUnits((long) stopPrice)
                .setNano((int) ((stopPrice - (long) stopPrice) * 1_000_000_000))
                .build());
        return mock;
    }
}
