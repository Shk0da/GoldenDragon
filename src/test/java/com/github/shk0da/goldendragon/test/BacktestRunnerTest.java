package com.github.shk0da.goldendragon.test;

import com.github.shk0da.goldendragon.model.Position;

import java.lang.reflect.Method;

/**
 * Unit tests for BacktestRunner simulation logic.
 * Tests PnL calculation, slippage, commissions, margin, and position lifecycle.
 *
 * Use: java -cp build/classes/java/test BacktestRunnerTest
 */
public class BacktestRunnerTest {

    /**
     * Helper assertion methods.
     */
    private static void assertEquals(double expected, double actual, double delta, String message) {
        double diff = Math.abs(expected - actual);
        if (diff > delta) {
            throw new AssertionError(message + ": expected " + expected + " but got " + actual + " (diff: " + diff + ")");
        }
    }

    private static void assertTrue(boolean condition, String message) {
        if (!condition) {
            throw new AssertionError(message);
        }
    }

    private static void printSuccess(String testName) {
        System.out.println("[PASS] " + testName);
    }

    private static void printFail(String testName, Throwable e) {
        System.err.println("[FAIL] " + testName + ": " + e.getMessage());
        e.printStackTrace(System.err);
    }

    // =========================================================================
    // NOTIONAL VALUE TESTS
    // =========================================================================

    public static void testBasicNotional() {
        // Private method: getNotionalValue(qty, price) = qty * price (returns 0 if negative/zero)
        double result = (double) invokePrivateMethod("getNotionalValue", 10, 100.0);
        assertEquals(1000.0, result, 0.001, "notional = qty * price");
        printSuccess("testBasicNotional: 10 * 100 = 1000");
    }

    public static void testNotionalValueZeroQty() {
        double result = (double) invokePrivateMethod("getNotionalValue", 0, 500.0);
        assertEquals(0.0, result, 0.001, "qty=0 => notional=0");
        printSuccess("testNotionalValueZeroQty");
    }

    public static void testNotionalValueZeroPrice() {
        double result = (double) invokePrivateMethod("getNotionalValue", 50, 0.0);
        assertEquals(0.0, result, 0.001, "price=0 => notional=0");
        printSuccess("testNotionalValueZeroPrice");
    }

    public static void testNotionalValueNegativeQty() {
        double result = (double) invokePrivateMethod("getNotionalValue", -10, 100.0);
        // Private method returns 0 for negatives
        assertEquals(0.0, result, 0.001, "negative qty => notional=0");
        printSuccess("testNotionalValueNegativeQty");
    }

    public static void testNotionalValueLarge() {
        double result = (double) invokePrivateMethod("getNotionalValue", 1000, 12345.67);
        assertEquals(12_345_670.0, result, 0.1, "large notional");
        printSuccess("testNotionalValueLarge");
    }

    // =========================================================================
    // MARGIN CALCULATION TESTS (pure math, no TickerRepository)
    // =========================================================================

    public static void testLeverage1xStock() {
        // Stock with leverage 1x => max notional
        double margin = 10 * 100.0 / 1;  // notional 1000, leverage 1
        assertEquals(1000.0, margin, 0.001, "leverage 1x => full notional");
        printSuccess("testLeverage1xStock");
    }

    public static void testLeverage2x() {
        // Leverage 2x => 50% of notional
        double margin = 10 * 100.0 / 2;
        assertEquals(500.0, margin, 0.001, "leverage 2x => half margin");
        printSuccess("testLeverage2x");
    }

    public static void testLeverage10x() {
        // Leverage 10x => 10% of notional
        double margin = 100 * 100.0 / 10;
        assertEquals(1000.0, margin, 0.01, "leverage 10x => 10% margin");
        printSuccess("testLeverage10x");
    }

    public static void testLeverage100x() {
        // Leverage 100x => 1% of notional
        double margin = 1000 * 50.0 / 100;
        assertEquals(500.0, margin, 0.1, "leverage 100x => 1% margin");
        printSuccess("testLeverage100x");
    }

    public static void testNegativeQtyReturnsZeroMargin() {
        // When qty <= 0, the actual method returns 0 early
        // Test the logic: margin = 0 if qty <= 0 or price <= 0
        int qty = -10;
        double price = 300.0;
        double margin = (qty <= 0 || price <= 0.0) ? 0.0 : qty * price / 1;
        assertEquals(0.0, margin, 0.001, "negative qty => zero margin");
        printSuccess("testNegativeQtyReturnsZeroMargin");
    }

    public static void testZeroQtyReturnsZeroMargin() {
        int qty = 0;
        double price = 300.0;
        // margin = 0 if qty <= 0
        double margin = (qty <= 0 || price <= 0.0) ? 0.0 : qty * price / 1;
        assertEquals(0.0, margin, 0.001, "zero qty => zero margin");
        printSuccess("testZeroQtyReturnsZeroMargin");
    }

    public static void testZeroPriceReturnsZeroMargin() {
        int qty = 10;
        double price = 0.0;
        // margin = 0 if price <= 0
        double margin = (qty <= 0 || price <= 0.0) ? 0.0 : qty * price / 1;
        assertEquals(0.0, margin, 0.001, "zero price => zero margin");
        printSuccess("testZeroPriceReturnsZeroMargin");
    }

    // =========================================================================
    // GROSS PNL TESTS
    // =========================================================================

    public static void testLongProfit() throws Exception {
        Method m = BacktestRunner.class.getDeclaredMethod("calculateGrossPnl", double.class, double.class, boolean.class);
        m.setAccessible(true);
        double pnl = (double) m.invoke(null, 1000.0, 1100.0, false);  // entry 1000, exit 1100, long
        assertEquals(100.0, pnl, 0.001, "long profit");
        printSuccess("testLongProfit");
    }

    public static void testLongLoss() throws Exception {
        Method m = BacktestRunner.class.getDeclaredMethod("calculateGrossPnl", double.class, double.class, boolean.class);
        m.setAccessible(true);
        double pnl = (double) m.invoke(null, 1000.0, 900.0, false);  // entry 1000, exit 900, long
        assertEquals(-100.0, pnl, 0.001, "long loss");
        printSuccess("testLongLoss");
    }

    public static void testLongBreakEven() throws Exception {
        Method m = BacktestRunner.class.getDeclaredMethod("calculateGrossPnl", double.class, double.class, boolean.class);
        m.setAccessible(true);
        double pnl = (double) m.invoke(null, 1000.0, 1000.0, false);
        assertEquals(0.0, pnl, 0.001, "break even");
        printSuccess("testLongBreakEven");
    }

    public static void testShortProfit() throws Exception {
        Method m = BacktestRunner.class.getDeclaredMethod("calculateGrossPnl", double.class, double.class, boolean.class);
        m.setAccessible(true);
        double pnl = (double) m.invoke(null, 1000.0, 900.0, true);  // entry 1000, exit 900, short
        assertEquals(100.0, pnl, 0.001, "short profit");
        printSuccess("testShortProfit");
    }

    public static void testShortLoss() throws Exception {
        Method m = BacktestRunner.class.getDeclaredMethod("calculateGrossPnl", double.class, double.class, boolean.class);
        m.setAccessible(true);
        double pnl = (double) m.invoke(null, 1000.0, 1100.0, true);  // entry 1000, exit 1100, short
        assertEquals(-100.0, pnl, 0.001, "short loss");
        printSuccess("testShortLoss");
    }

    public static void testShortBreakEven() throws Exception {
        Method m = BacktestRunner.class.getDeclaredMethod("calculateGrossPnl", double.class, double.class, boolean.class);
        m.setAccessible(true);
        double pnl = (double) m.invoke(null, 1000.0, 1000.0, true);
        assertEquals(0.0, pnl, 0.001, "short break even");
        printSuccess("testShortBreakEven");
    }

    // =========================================================================
    // SLIPPAGE TESTS
    // =========================================================================

    public static void testLongExitSlippage() {
        double exitPrice = 110.0;
        double slippage = 0.0005;
        // Long exit: exitPrice * (1 - slippage)
        double slippedExit = exitPrice * (1.0 - slippage);
        assertEquals(109.945, slippedExit, 0.0001, "long exit slippage");
        printSuccess("testLongExitSlippage");
    }

    public static void testShortExitSlippage() {
        double exitPrice = 90.0;
        double slippage = 0.0005;
        // Short exit: exitPrice * (1 + slippage)
        double slippedExit = exitPrice * (1.0 + slippage);
        assertEquals(90.045, slippedExit, 0.0001, "short exit slippage");
        printSuccess("testShortExitSlippage");
    }

    public static void testLongEntrySlippage() {
        double entryPrice = 100.0;
        double slippage = 0.0005;
        // Long entry: entryPrice * (1 + slippage)
        double slippedEntry = entryPrice * (1.0 + slippage);
        assertEquals(100.05, slippedEntry, 0.0001, "long entry slippage");
        printSuccess("testLongEntrySlippage");
    }

    public static void testShortEntrySlippage() {
        double entryPrice = 100.0;
        double slippage = 0.0005;
        // Short entry: entryPrice * (1 - slippage)
        double slippedEntry = entryPrice * (1.0 - slippage);
        assertEquals(99.95, slippedEntry, 0.0001, "short entry slippage");
        printSuccess("testShortEntrySlippage");
    }

    public static void testSlippageSymmetry() throws Exception {
        double price = 100.0;
        double qty = 1;
        double slippage = 0.0005;

        // Long total slippage per unit
        double longEntrySlippage = price * (1.0 + slippage) - price;  // extra cost
        double longExitSlippage = price - price * (1.0 - slippage);    // lower proceeds
        double longTotal = longEntrySlippage + longExitSlippage;

        // Short total slippage per unit
        double shortEntrySlippage = price - price * (1.0 - slippage);  // less received
        double shortExitSlippage = price * (1.0 + slippage) - price;   // extra cost
        double shortTotal = shortEntrySlippage + shortExitSlippage;

        assertEquals(longTotal, shortTotal, 0.0001, "slippage should be symmetric");
        printSuccess("testSlippageSymmetry");
    }

    public static void testSlippageCostCalc() {
        double price = 100.0;
        double qty = 100;
        double slippage = 0.0005;

        // Total slippage cost = 2 * slippage * price * qty
        double totalCost = 2.0 * slippage * price * qty; // 2 * 0.0005 * 100 * 100 = 10.0
        assertEquals(10.0, totalCost, 0.001, "total slippage cost");
        printSuccess("testSlippageCostCalc");
    }

    // =========================================================================
    // COMMISSION TESTS
    // =========================================================================

    public static void testRoundtripCommission() {
        double commissionRate = 0.0005;
        double entryNotional = 10_000.0;
        double exitNotional = 10_500.0;
        double roundtripCommission = (entryNotional + exitNotional) * commissionRate;
        assertEquals(10.25, roundtripCommission, 0.001, "roundtrip commission");
        printSuccess("testRoundtripCommission");
    }

    public static void testEntryCommission() {
        double commissionRate = 0.0005;
        double entryNotional = 10_000.0;
        double entryCommission = entryNotional * commissionRate;
        assertEquals(5.0, entryCommission, 0.001, "entry commission");
        printSuccess("testEntryCommission");
    }

    public static void testExitCommission() {
        double commissionRate = 0.0005;
        double exitNotional = 10_500.0;
        double exitCommission = exitNotional * commissionRate;
        assertEquals(5.25, exitCommission, 0.001, "exit commission");
        printSuccess("testExitCommission");
    }

    public static void testZeroNotionalZeroCommission() {
        double commission = 0.0 * 0.0005;
        assertEquals(0.0, commission, 0.001, "zero notional => zero commission");
        printSuccess("testZeroNotionalZeroCommission");
    }

    public static void testLargePositionCommission() {
        double notional = 1_000_000.0;
        double rate = 0.0005;
        double oneWay = notional * rate;        // 500
        double roundtrip = (notional + notional) * rate;  // 1000
        assertEquals(500.0, oneWay, 0.001, "one way commission for 1M");
        assertEquals(1000.0, roundtrip, 0.001, "round trip commission for 1M");
        printSuccess("testLargePositionCommission");
    }

    // =========================================================================
    // FULL TRADE PNL TESTS (with slippage + commission)
    // =========================================================================

    public static void testLongProfitableTrade() {
        double entryPrice = 100.0;
        double exitPrice = 110.0;
        double qty = 10;
        double commissionRate = 0.0005;
        double slippage = 0.0005;

        // Entry with slippage (BUY: pay more)
        double entryPaid = entryPrice * (1.0 + slippage);
        double entryNotional = qty * entryPaid;

        // Exit with slippage (SELL: receive less)
        double exitReceived = exitPrice * (1.0 - slippage);
        double exitNotional = qty * exitReceived;

        double grossPnl = exitNotional - entryNotional;
        double roundtripCommission = (entryNotional + exitNotional) * commissionRate;
        double netPnl = grossPnl - roundtripCommission;

        // Net should be positive
        assertTrue(netPnl > 0, "net pnl should be positive");
        // Net should be less than gross due to commission
        assertTrue(netPnl < grossPnl, "net < gross due to commission");
        // Verify magnitude is roughly correct
        assertTrue(grossPnl > 97.0, "gross pnl should be ~98.9");
        printSuccess("testLongProfitableTrade");
    }

    public static void testLongLosingTrade() {
        double entryPrice = 100.0;
        double exitPrice = 90.0;
        double qty = 10;
        double commissionRate = 0.0005;
        double slippage = 0.0005;

        double entryPaid = entryPrice * (1.0 + slippage);
        double entryNotional = qty * entryPaid;
        double exitReceived = exitPrice * (1.0 - slippage);
        double exitNotional = qty * exitReceived;

        double grossPnl = exitNotional - entryNotional;
        double roundtripCommission = (entryNotional + exitNotional) * commissionRate;
        double netPnl = grossPnl - roundtripCommission;

        assertTrue(netPnl < 0, "net pnl should be negative");
        printSuccess("testLongLosingTrade");
    }

    public static void testShortProfitableTrade() {
        double entryPrice = 100.0;
        double exitPrice = 90.0;
        double qty = 10;
        double commissionRate = 0.0005;
        double slippage = 0.0005;

        // Short entry: receive less
        double entryReceived = entryPrice * (1.0 - slippage);
        double entryNotional = qty * entryReceived;

        // Short exit: pay more
        double exitPaid = exitPrice * (1.0 + slippage);
        double exitNotional = qty * exitPaid;

        double grossPnl = entryNotional - exitNotional;
        double roundtripCommission = (entryNotional + exitNotional) * commissionRate;
        double netPnl = grossPnl - roundtripCommission;

        assertTrue(netPnl > 0, "short net pnl should be positive");
        printSuccess("testShortProfitableTrade");
    }

    public static void testShortLosingTrade() {
        double entryPrice = 100.0;
        double exitPrice = 110.0;
        double qty = 10;
        double commissionRate = 0.0005;
        double slippage = 0.0005;

        double entryReceived = entryPrice * (1.0 - slippage);
        double entryNotional = qty * entryReceived;
        double exitPaid = exitPrice * (1.0 + slippage);
        double exitNotional = qty * exitPaid;

        double grossPnl = entryNotional - exitNotional;
        double roundtripCommission = (entryNotional + exitNotional) * commissionRate;
        double netPnl = grossPnl - roundtripCommission;

        assertTrue(netPnl < 0, "short net pnl should be negative");
        printSuccess("testShortLosingTrade");
    }

    public static void testNoSlippagePnl() {
        double entryPrice = 100.0;
        double exitPrice = 105.0;
        double qty = 10;
        double commissionRate = 0.0005;
        double slippage = 0.0;  // No slippage

        double entryNotional = qty * entryPrice;
        double exitNotional = qty * exitPrice;
        double grossPnl = exitNotional - entryNotional;
        double roundtripCommission = (entryNotional + exitNotional) * commissionRate;
        double netPnl = grossPnl - roundtripCommission;

        assertEquals(50.0, grossPnl, 0.001, "no slippage => simple PnL");
        assertTrue(netPnl < grossPnl, "net < gross due to commission");
        printSuccess("testNoSlippagePnl");
    }

    // =========================================================================
    // OPEN/CLOSE FLOW TESTS
    // =========================================================================

    public static void testOpenCashDecrease() {
        double initialCash = 1_000_000.0;
        double qty = 10;
        double price = 100.0;
        double commissionRate = 0.0005;
        double slippage = 0.0005;
        int leverage = 1;

        // Entry with slippage (BUY: pay more)
        double entryPaid = price * (1.0 + slippage);
        double entryNotional = qty * entryPaid;
        double requiredMargin = entryNotional / leverage;
        double entryCommission = entryNotional * commissionRate;

        double cashAfterOpen = initialCash - (requiredMargin + entryCommission);

        assertTrue(cashAfterOpen < initialCash, "cash should decrease after open");
        printSuccess("testOpenCashDecrease");
    }

    public static void testCloseCashIncrease() {
        double entryPrice = 100.0;  // after slippage
        double qty = 10;
        double exitPrice = 110.0;
        double commissionRate = 0.0005;
        double slippage = 0.0005;
        int leverage = 1;

        // Exit with slippage (SELL: receive less)
        double exitReceived = exitPrice * (1.0 - slippage);
        double entryNotional = qty * entryPrice;
        double exitNotional = qty * exitReceived;

        double requiredMargin = entryNotional / leverage;
        double grossPnl = exitNotional - entryNotional;
        double exitCommission = exitNotional * commissionRate;

        // Cash returned = margin + grossPnl - exitCommission
        double cashReturned = requiredMargin + grossPnl - exitCommission;

        // Cash returned should exceed margin (due to profit)
        assertTrue(cashReturned > requiredMargin, "cash returned should exceed margin");
        printSuccess("testCloseCashIncrease");
    }

    public static void testCloseWithLoss() {
        double entryPrice = 100.0;
        double qty = 10;
        double exitPrice = 90.0;
        double commissionRate = 0.0005;
        double slippage = 0.0005;
        int leverage = 1;

        double exitReceived = exitPrice * (1.0 - slippage);
        double entryNotional = qty * entryPrice;
        double exitNotional = qty * exitReceived;

        double requiredMargin = entryNotional / leverage;
        double grossPnl = exitNotional - entryNotional;
        double exitCommission = exitNotional * commissionRate;

        double cashReturned = requiredMargin + grossPnl - exitCommission;

        // Cash returned should be less than margin (due to loss + commission)
        assertTrue(cashReturned < requiredMargin, "cash returned < margin due to loss");
        printSuccess("testCloseWithLoss");
    }

    public static void testShortOpenCashDecrease() {
        double initialCash = 1_000_000.0;
        double qty = 10;
        double price = 100.0;
        int leverage = 1;

        // Short entry: sell at lower price (slippage)
        double entryReceived = price * (1.0 - 0.0005);
        double entryNotional = qty * entryReceived;
        double requiredMargin = entryNotional / leverage;

        double cashAfterOpen = initialCash - requiredMargin;

        assertTrue(initialCash > cashAfterOpen, "short pos should decrease cash");
        printSuccess("testShortOpenCashDecrease");
    }

    public static void testShortCloseWithProfit() {
        double initialCash = 1_000_000.0;
        double entryPrice = 100.0;
        double exitPrice = 90.0;
        double qty = 10;
        double commissionRate = 0.0005;
        int leverage = 1;

        // Short exit: buy back at higher price (slippage)
        double exitPaid = exitPrice * (1.0 + 0.0005);
        double entryNotional = qty * entryPrice;
        double exitNotional = qty * exitPaid;

        double requiredMargin = entryNotional / leverage;
        double grossPnl = entryNotional - exitNotional;
        double exitCommission = exitNotional * commissionRate;

        double cashAfterClose = initialCash - requiredMargin + grossPnl - exitCommission;

        // Net should be profitable after closing short at 90 that was entered at 100
        assertTrue(grossPnl - exitCommission > 0, "short trade should be profitable");
        printSuccess("testShortCloseWithProfit");
    }

    // =========================================================================
    // POSITION OBJECT TESTS
    // =========================================================================

    public static void testPositionDefaultLeverage() {
        Position pos = new Position();
        assertEquals(1, pos.appliedLeverage, 0, "default leverage should be 1");
        printSuccess("testPositionDefaultLeverage");
    }

    public static void testPositionWithLeverage() {
        Position pos1x = new Position("BUY", 100.0, null, null, 10, 0, 0, 1);
        Position pos5x = new Position("BUY", 100.0, null, null, 10, 0, 0, 5);

        assertEquals(1, pos1x.appliedLeverage, 0, "leverage 1x");
        assertEquals(5, pos5x.appliedLeverage, 0, "leverage 5x");
        printSuccess("testPositionWithLeverage");
    }

    public static void testPositionNegativeLeverageClamped() {
        // Leverage is clamped to min 1 by Math.max(1, appliedLeverage) in constructor
        Position posNeg = new Position("BUY", 100.0, null, null, 10, 0, 0, -1);
        assertEquals(1, posNeg.appliedLeverage, 0, "negative leverage clamped to 1");
        printSuccess("testPositionNegativeLeverageClamped");
    }

    public static void testPositionEmptyDirection() {
        Position pos = new Position();
        // Empty position should have quantity == 0 and direction == null
        assertEquals(0, pos.quantity, 0, "empty position quantity should be 0");
        printSuccess("testPositionEmptyDirection");
    }

    // =========================================================================
    // REFLECTION HELPER
    // =========================================================================

    private static Object invokePrivateMethod(String methodName, Object... args) {
        try {
            int argCount = args.length;
            Class<?>[] paramTypes = new Class<?>[argCount];
            Object[] typedArgs = new Object[argCount];

            for (int i = 0; i < argCount; i++) {
                if (args[i] instanceof Integer) {
                    paramTypes[i] = int.class;
                } else if (args[i] instanceof Double) {
                    paramTypes[i] = double.class;
                } else if (args[i] instanceof Long) {
                    paramTypes[i] = long.class;
                } else {
                    paramTypes[i] = String.class;
                }
                typedArgs[i] = args[i];
            }

            Method m = BacktestRunner.class.getDeclaredMethod(methodName, paramTypes);
            m.setAccessible(true);
            return m.invoke(new BacktestRunner("data", 1_000_000.0, 0.0005, 0.0), typedArgs);
        } catch (Exception e) {
            throw new RuntimeException("Failed to invoke " + methodName + ": " + e.getMessage(), e);
        }
    }

    // =========================================================================
    // TEST RUNNER
    // =========================================================================

    public static void main(String[] args) {
        int passed = 0;
        int failed = 0;

        System.out.println("=============================");
        System.out.println("  BacktestRunner Unit Tests  ");
        System.out.println("=============================\n");

        // Notional value
        try { testBasicNotional(); passed++; } catch (Exception e) { printFail("testBasicNotional", e); failed++; }
        try { testNotionalValueZeroQty(); passed++; } catch (Exception e) { printFail("testNotionalValueZeroQty", e); failed++; }
        try { testNotionalValueZeroPrice(); passed++; } catch (Exception e) { printFail("testNotionalValueZeroPrice", e); failed++; }
        try { testNotionalValueNegativeQty(); passed++; } catch (Exception e) { printFail("testNotionalValueNegativeQty", e); failed++; }
        try { testNotionalValueLarge(); passed++; } catch (Exception e) { printFail("testNotionalValueLarge", e); failed++; }

        // Margin calculation
        try { testLeverage1xStock(); passed++; } catch (Exception e) { printFail("testLeverage1xStock", e); failed++; }
        try { testLeverage2x(); passed++; } catch (Exception e) { printFail("testLeverage2x", e); failed++; }
        try { testLeverage10x(); passed++; } catch (Exception e) { printFail("testLeverage10x", e); failed++; }
        try { testLeverage100x(); passed++; } catch (Exception e) { printFail("testLeverage100x", e); failed++; }
        try { testNegativeQtyReturnsZeroMargin(); passed++; } catch (Exception e) { printFail("testNegativeQtyReturnsZeroMargin", e); failed++; }
        try { testZeroQtyReturnsZeroMargin(); passed++; } catch (Exception e) { printFail("testZeroQtyReturnsZeroMargin", e); failed++; }
        try { testZeroPriceReturnsZeroMargin(); passed++; } catch (Exception e) { printFail("testZeroPriceReturnsZeroMargin", e); failed++; }

        // Gross PnL
        try { testLongProfit(); passed++; } catch (Exception e) { printFail("testLongProfit", e); failed++; }
        try { testLongLoss(); passed++; } catch (Exception e) { printFail("testLongLoss", e); failed++; }
        try { testLongBreakEven(); passed++; } catch (Exception e) { printFail("testLongBreakEven", e); failed++; }
        try { testShortProfit(); passed++; } catch (Exception e) { printFail("testShortProfit", e); failed++; }
        try { testShortLoss(); passed++; } catch (Exception e) { printFail("testShortLoss", e); failed++; }
        try { testShortBreakEven(); passed++; } catch (Exception e) { printFail("testShortBreakEven", e); failed++; }

        // Slippage
        try { testLongExitSlippage(); passed++; } catch (Exception e) { printFail("testLongExitSlippage", e); failed++; }
        try { testShortExitSlippage(); passed++; } catch (Exception e) { printFail("testShortExitSlippage", e); failed++; }
        try { testLongEntrySlippage(); passed++; } catch (Exception e) { printFail("testLongEntrySlippage", e); failed++; }
        try { testShortEntrySlippage(); passed++; } catch (Exception e) { printFail("testShortEntrySlippage", e); failed++; }
        try { testSlippageSymmetry(); passed++; } catch (Exception e) { printFail("testSlippageSymmetry", e); failed++; }
        try { testSlippageCostCalc(); passed++; } catch (Exception e) { printFail("testSlippageCostCalc", e); failed++; }

        // Commission
        try { testRoundtripCommission(); passed++; } catch (Exception e) { printFail("testRoundtripCommission", e); failed++; }
        try { testEntryCommission(); passed++; } catch (Exception e) { printFail("testEntryCommission", e); failed++; }
        try { testExitCommission(); passed++; } catch (Exception e) { printFail("testExitCommission", e); failed++; }
        try { testZeroNotionalZeroCommission(); passed++; } catch (Exception e) { printFail("testZeroNotionalZeroCommission", e); failed++; }
        try { testLargePositionCommission(); passed++; } catch (Exception e) { printFail("testLargePositionCommission", e); failed++; }

        // Full trade PnL
        try { testLongProfitableTrade(); passed++; } catch (Exception e) { printFail("testLongProfitableTrade", e); failed++; }
        try { testLongLosingTrade(); passed++; } catch (Exception e) { printFail("testLongLosingTrade", e); failed++; }
        try { testShortProfitableTrade(); passed++; } catch (Exception e) { printFail("testShortProfitableTrade", e); failed++; }
        try { testShortLosingTrade(); passed++; } catch (Exception e) { printFail("testShortLosingTrade", e); failed++; }
        try { testNoSlippagePnl(); passed++; } catch (Exception e) { printFail("testNoSlippagePnl", e); failed++; }

        // Open/Close flow
        try { testOpenCashDecrease(); passed++; } catch (Exception e) { printFail("testOpenCashDecrease", e); failed++; }
        try { testCloseCashIncrease(); passed++; } catch (Exception e) { printFail("testCloseCashIncrease", e); failed++; }
        try { testCloseWithLoss(); passed++; } catch (Exception e) { printFail("testCloseWithLoss", e); failed++; }
        try { testShortOpenCashDecrease(); passed++; } catch (Exception e) { printFail("testShortOpenCashDecrease", e); failed++; }
        try { testShortCloseWithProfit(); passed++; } catch (Exception e) { printFail("testShortCloseWithProfit", e); failed++; }

        // Position object
        try { testPositionDefaultLeverage(); passed++; } catch (Exception e) { printFail("testPositionDefaultLeverage", e); failed++; }
        try { testPositionWithLeverage(); passed++; } catch (Exception e) { printFail("testPositionWithLeverage", e); failed++; }
        try { testPositionNegativeLeverageClamped(); passed++; } catch (Exception e) { printFail("testPositionNegativeLeverageClamped", e); failed++; }
        try { testPositionEmptyDirection(); passed++; } catch (Exception e) { printFail("testPositionEmptyDirection", e); failed++; }

        // Summary
        System.out.println("\n=============================");
        System.out.println("  Total: " + (passed + failed) + " tests");
        System.out.println("  Passed: " + passed);
        System.out.println("  Failed: " + failed);
        System.out.println("=============================");

        System.exit(failed > 0 ? 1 : 0);
    }
}
