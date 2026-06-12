package com.github.shk0da.goldendragon.test;

import static com.github.shk0da.goldendragon.model.Market.MOEX;
import static java.lang.System.out;

import com.github.shk0da.goldendragon.config.MainConfig;
import com.github.shk0da.goldendragon.config.MarketConfig;
import com.github.shk0da.goldendragon.config.UnifiedTraderConfig;
import com.github.shk0da.goldendragon.model.Candle;
import com.github.shk0da.goldendragon.model.Config;
import com.github.shk0da.goldendragon.model.Position;
import com.github.shk0da.goldendragon.model.PositionInfo;
import com.github.shk0da.goldendragon.model.TickerInfo;
import com.github.shk0da.goldendragon.model.TickerType;
import com.github.shk0da.goldendragon.model.TradingDecision;
import com.github.shk0da.goldendragon.service.TCSService;
import com.github.shk0da.goldendragon.strategy.UnifiedStrategy;
import java.util.ArrayList;
import java.util.List;

/**
 * Manual smoke test for UnifiedStrategy execution flow. Uses credentials from
 * application.properties.
 *
 * <p>Warning: this test performs real trading operations. Make sure sandbox mode
 * (tcs.isSandbox=true) or test mode (tcs.testMode=true) is enabled.
 */
public class UnifiedStrategySmokeTest {

    public static String testTicker = "SBERF";
    public static TickerType tickerType = TickerType.FEATURE;
    public static double testCashAmount = 100_000;
    public static double takeProfitPercent = 2.0;
    public static double stopLossPercent = 1.0;

    public static void main(String[] args) {
        try {
            out.println("=== Starting UnifiedStrategy Smoke Test ===");

            MainConfig mainConfig = new MainConfig();
            MarketConfig marketConfig = MarketConfig.byMarket(MOEX);
            UnifiedTraderConfig traderConfig = new UnifiedTraderConfig();
            Config config = new Config();
            TCSService tcsService = new TCSService(mainConfig, marketConfig);
            StrategyTestAdapter strategy =
                    new StrategyTestAdapter(traderConfig, tcsService, config);

            out.println("Test Mode: " + mainConfig.isTestMode());
            out.println("Sandbox: " + mainConfig.isSandbox());
            out.println("Account ID: " + mainConfig.getTcsAccountId());
            out.println("Ticker: " + testTicker + " (" + tickerType + ")");

            TickerInfo tickerInfo = strategy.findTicker(testTicker);
            if (tickerInfo == null) {
                out.println("Ticker not found in repository: " + testTicker);
                return;
            }

            List<Candle> candles = buildStubCandles(tcsService, tickerInfo);
            double marketPrice = candles.get(candles.size() - 1).close;
            int lotSize = Math.max(1, tickerInfo.getLot());
            int quantity = (int) Math.floor(testCashAmount / (marketPrice * lotSize));
            if (quantity <= 0) {
                out.println(
                        "Calculated quantity is zero. Increase testCashAmount or use another ticker.");
                return;
            }

            double stopLossPrice = marketPrice * (1.0 - stopLossPercent / 100.0);
            double takeProfitPrice = marketPrice * (1.0 + takeProfitPercent / 100.0);

            out.println("Reference price: " + marketPrice);
            out.println("Calculated quantity: " + quantity);
            out.println("Stop loss price: " + stopLossPrice);
            out.println("Take profit price: " + takeProfitPrice);

            out.println("\n=== Test 1: Open BUY via UnifiedStrategy ===");
            try {
                TradingDecision buyDecision =
                        new TradingDecision(
                                "OPEN",
                                "SMOKE_BUY",
                                1.0,
                                quantity,
                                stopLossPrice,
                                takeProfitPrice,
                                marketPrice,
                                new Position(
                                        "BUY",
                                        marketPrice,
                                        stopLossPrice,
                                        takeProfitPrice,
                                        quantity,
                                        0));
                strategy.open(testTicker, tickerInfo, candles, buyDecision);
                Thread.sleep(2000);
                verifyPositionExists(tcsService, "Position after BUY", true);
            } catch (Exception e) {
                out.println("BUY open test failed: " + e.getMessage());
                e.printStackTrace();
            }

            out.println("\n=== Test 2: Close BUY via UnifiedStrategy ===");
            try {
                Position buyPosition = strategy.getStoredPosition(testTicker);
                strategy.close(
                        testTicker,
                        tickerInfo,
                        buyPosition,
                        new TradingDecision(
                                "CLOSE",
                                "SMOKE_CLOSE_BUY",
                                0.0,
                                buyPosition.quantity,
                                null,
                                null,
                                marketPrice,
                                new Position(config.cooldownCandles)));
                Thread.sleep(2000);
                verifyPositionExists(tcsService, "Position after BUY close", false);
            } catch (Exception e) {
                out.println("BUY close test failed: " + e.getMessage());
                e.printStackTrace();
            }

            out.println("\n=== Test 3: Open SELL via UnifiedStrategy ===");
            try {
                TradingDecision sellDecision =
                        new TradingDecision(
                                "OPEN",
                                "SMOKE_SELL",
                                1.0,
                                quantity,
                                marketPrice * (1.0 + stopLossPercent / 100.0),
                                marketPrice * (1.0 - takeProfitPercent / 100.0),
                                marketPrice,
                                new Position(
                                        "SELL",
                                        marketPrice,
                                        marketPrice * (1.0 + stopLossPercent / 100.0),
                                        marketPrice * (1.0 - takeProfitPercent / 100.0),
                                        quantity,
                                        0));
                strategy.open(testTicker, tickerInfo, candles, sellDecision);
                Thread.sleep(2000);
                verifyPositionExists(tcsService, "Position after SELL", true);
            } catch (Exception e) {
                out.println("SELL open test failed: " + e.getMessage());
                e.printStackTrace();
            }

            out.println("\n=== Test 4: Close SELL via UnifiedStrategy ===");
            try {
                Position sellPosition = strategy.getStoredPosition(testTicker);
                strategy.close(
                        testTicker,
                        tickerInfo,
                        sellPosition,
                        new TradingDecision(
                                "CLOSE",
                                "SMOKE_CLOSE_SELL",
                                0.0,
                                sellPosition.quantity,
                                null,
                                null,
                                marketPrice,
                                new Position(config.cooldownCandles)));
                Thread.sleep(2000);
                verifyPositionExists(tcsService, "Position after SELL close", false);
            } catch (Exception e) {
                out.println("SELL close test failed: " + e.getMessage());
                e.printStackTrace();
            }

            out.println("\n=== UnifiedStrategy Smoke Test Complete ===");
        } catch (Exception e) {
            out.println("Smoke test failed with exception: " + e.getMessage());
            e.printStackTrace();
        }
    }

    private static void printPosition(TCSService tcsService, String title) {
        PositionInfo position = tcsService.getCurrentPositions(tickerType, testTicker);
        out.println(title + ":");
        if (position == null) {
            out.println("No position found");
            return;
        }

        out.println("Ticker: " + position.getTicker());
        out.println("Balance: " + position.getBalance());
        out.println("Expected Yield: " + position.getExpectedYield() + "%");
    }

    private static void verifyPositionExists(
            TCSService tcsService, String title, boolean expectedExists) {
        PositionInfo position = tcsService.getCurrentPositions(tickerType, testTicker);
        printPosition(tcsService, title);

        boolean actualExists = position != null && position.getBalance() != 0;
        if (expectedExists != actualExists) {
            out.println(
                    "Position verification FAILED: expectedExists="
                            + expectedExists
                            + ", actualExists="
                            + actualExists);
        } else {
            out.println("Position verification PASSED");
        }
    }

    private static List<Candle> buildStubCandles(TCSService tcsService, TickerInfo tickerInfo) {
        double price = tcsService.getAvailablePrice(tickerInfo.getKey());
        List<Candle> candles = new ArrayList<>();
        for (int i = 0; i < 60; i++) {
            candles.add(
                    new Candle(
                            "2026-01-01 10:" + String.format("%02d", i % 60),
                            price,
                            price,
                            price,
                            price,
                            1000L));
        }
        return candles;
    }

    private static final class StrategyTestAdapter extends UnifiedStrategy {

        private StrategyTestAdapter(
                UnifiedTraderConfig unifiedTraderConfig, TCSService tcsService, Config config) {
            super(unifiedTraderConfig, tcsService, config, false);
        }

        private void open(
                String name, TickerInfo ticker, List<Candle> candles, TradingDecision decision) {
            openPosition(name, ticker, candles, decision);
        }

        private void close(
                String name, TickerInfo ticker, Position position, TradingDecision decision) {
            closePosition(name, ticker, position, decision);
        }

        private Position getStoredPosition(String ticker) {
            return positionStore.getOrDefault(ticker, new Position());
        }

        private TickerInfo findTicker(String ticker) {
            return findTickerInfo(ticker);
        }
    }
}
