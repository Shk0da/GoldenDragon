package com.github.shk0da.goldendragon.test;

import static com.github.shk0da.goldendragon.model.Market.MOEX;
import static com.github.shk0da.goldendragon.model.TickerInfo.Key;
import static java.lang.System.out;

import com.github.shk0da.goldendragon.config.MainConfig;
import com.github.shk0da.goldendragon.config.MarketConfig;
import com.github.shk0da.goldendragon.model.PositionInfo;
import com.github.shk0da.goldendragon.model.TickerType;
import com.github.shk0da.goldendragon.service.TCSService;
import java.util.Map;

/**
 * Manual smoke test for TCSService. Uses credentials from application.properties.
 *
 * <p>Warning: this test performs real trading operations. Make sure sandbox mode
 * (tcs.isSandbox=true) or test mode (tcs.testMode=true) is enabled.
 */
public final class TCSServiceSmokeTest {

  private static final String TEST_TICKER = "SBERF";
  private static final TickerType TICKER_TYPE = TickerType.FEATURE;
  private static final double TEST_CASH_AMOUNT = 100_000;
  private static final int SLEEP_MS = 2000;
  private static final double TEST_PRICE = 123.456789;
  private static final double QUOTATION_TOLERANCE = 0.000001;

  private TCSServiceSmokeTest() {
    // Utility class - prevent instantiation
  }

  public static void main(final String[] args) {
    try {
      out.println("=== Starting TCSService Test ===");

      final MainConfig mainConfig = new MainConfig();
      final MarketConfig marketConfig = MarketConfig.byMarket(MOEX);

      out.println("Test Mode: " + mainConfig.isTestMode());
      out.println("Sandbox: " + mainConfig.isSandbox());
      out.println("Account ID: " + mainConfig.getTcsAccountId());

      final TCSService tcsService = new TCSService(mainConfig, marketConfig);
      out.println("TCSService created successfully");

      testPortfolio(tcsService, marketConfig);
      testPositions(tcsService);
      testOrderBook(tcsService);
      testQuotation();
      testInstrumentSearch(tcsService);
      testMarketBuy(tcsService);
      testPositionAfterBuy(tcsService);
      testMarketSell(tcsService);
      testBuyWithTpSl(tcsService);
      testLimitBuy(tcsService);
      testLimitSell(tcsService);
      testClosePosition(tcsService);
      testFinalPositionCheck(tcsService);
      testFinalCashCheck(tcsService, marketConfig);

      out.println("\n=== TCSService Test Complete ===");
      out.println("All tests finished. Check results above.");

    } catch (final Exception e) {
      out.println("Test failed with exception: " + e.getMessage());
      e.printStackTrace();
    }
  }

  private static void testPortfolio(final TCSService tcsService, final MarketConfig marketConfig) {
    out.println("\n=== Test 1: Portfolio ===");
    try {
      final double totalPortfolio = tcsService.getTotalPortfolioCost();
      out.println("Total Portfolio: " + totalPortfolio + " " + marketConfig.getCurrency());

      final double availableCash = tcsService.getAvailableCash();
      out.println("Available Cash: " + availableCash + " " + marketConfig.getCurrency());
    } catch (final Exception e) {
      out.println("Portfolio test failed: " + e.getMessage());
      e.printStackTrace();
    }
  }

  private static void testPositions(final TCSService tcsService) {
    out.println("\n=== Test 2: Positions ===");
    try {
      final Map<Key, PositionInfo> positions = tcsService.getCurrentPositions(TickerType.ALL);
      out.println("Total positions: " + positions.size());
      positions.forEach(
          (key, pos) -> out.println("  " + pos.getTicker() + ": " + pos.getBalance() + " lots"));
    } catch (final Exception e) {
      out.println("Positions test failed: " + e.getMessage());
      e.printStackTrace();
    }
  }

  private static void testOrderBook(final TCSService tcsService) {
    out.println("\n=== Test 3: Order Book ===");
    try {
      final Key key = new Key(TEST_TICKER, TICKER_TYPE);
      final Map<String, Map<Double, Integer>> prices = tcsService.getCurrentPrices(key, false);

      out.println("Ticker: " + TEST_TICKER);
      out.println("Bids: " + prices.get("bids").size() + ", Asks: " + prices.get("asks").size());

      if (!prices.get("bids").isEmpty()) {
        final double bestBid =
            prices.get("bids").keySet().stream().max(Double::compareTo).orElse(0.0);
        out.println("Best Bid: " + bestBid);
      }
      if (!prices.get("asks").isEmpty()) {
        final double bestAsk =
            prices.get("asks").keySet().stream().min(Double::compareTo).orElse(0.0);
        out.println("Best Ask: " + bestAsk);
      }
    } catch (final Exception e) {
      out.println("Order book test failed: " + e.getMessage());
      e.printStackTrace();
    }
  }

  private static void testQuotation() {
    out.println("\n=== Test 4: Quotation Creation ===");
    try {
      final var quotation = createQuotation(TEST_PRICE);
      final double restoredPrice = toDouble(quotation.getUnits(), quotation.getNano());
      out.println("Original price: " + TEST_PRICE);
      out.println("Quotation units: " + quotation.getUnits() + ", nano: " + quotation.getNano());
      out.println("Restored price: " + restoredPrice);
      out.println("Difference: " + Math.abs(TEST_PRICE - restoredPrice));

      if (Math.abs(TEST_PRICE - restoredPrice) < QUOTATION_TOLERANCE) {
        out.println("Quotation test PASSED");
      } else {
        out.println("Quotation test FAILED");
      }
    } catch (final Exception e) {
      out.println("Quotation test failed: " + e.getMessage());
      e.printStackTrace();
    }
  }

  private static void testInstrumentSearch(final TCSService tcsService) {
    out.println("\n=== Test 5: Instrument Search ===");
    try {
      final Key key = new Key(TEST_TICKER, TICKER_TYPE);
      final var tickerInfo = tcsService.searchTicker(key);

      out.println("Ticker: " + tickerInfo.getTicker());
      out.println("FIGI: " + tickerInfo.getFigi());
      out.println("Lot: " + tickerInfo.getLot());
      out.println("Currency: " + tickerInfo.getCurrency());
      out.println("Min Price Increment: " + tickerInfo.getMinPriceIncrement());
    } catch (final Exception e) {
      out.println("Instrument search test failed: " + e.getMessage());
      e.printStackTrace();
    }
  }

  private static void testMarketBuy(final TCSService tcsService) {
    out.println("\n=== Test 6: Buy by Market ===");
    try {
      final boolean buyResult =
          tcsService.buyByMarket(TEST_TICKER, TICKER_TYPE, TEST_CASH_AMOUNT, 0, 0);
      out.println("Buy by market result: " + buyResult);
      sleepQuietly(SLEEP_MS);
    } catch (final Exception e) {
      out.println("Buy by market test failed: " + e.getMessage());
      e.printStackTrace();
    }
  }

  private static void testPositionAfterBuy(final TCSService tcsService) {
    out.println("\n=== Test 7: Position After Buy ===");
    try {
      sleepQuietly(1000);
      final PositionInfo position = tcsService.getCurrentPositions(TICKER_TYPE, TEST_TICKER);
      if (position != null) {
        out.println("Position: " + position.getTicker());
        out.println("Balance: " + position.getBalance());
        out.println("Expected Yield: " + position.getExpectedYield() + "%");
      } else {
        out.println("No position found");
      }
    } catch (final Exception e) {
      out.println("Position check failed: " + e.getMessage());
      e.printStackTrace();
    }
  }

  private static void testMarketSell(final TCSService tcsService) {
    out.println("\n=== Test 8: Sell by Market ===");
    try {
      final boolean sellResult =
          tcsService.sellByMarket(TEST_TICKER, TICKER_TYPE, TEST_CASH_AMOUNT, 0, 0);
      out.println("Sell by market result: " + sellResult);
      sleepQuietly(SLEEP_MS);
    } catch (final Exception e) {
      out.println("Sell by market test failed: " + e.getMessage());
      e.printStackTrace();
    }
  }

  private static void testBuyWithTpSl(final TCSService tcsService) {
    out.println("\n=== Test 9: Buy with TP/SL ===");
    try {
      final double takeProfit = 2.0;
      final double stopLoss = 1.0;
      final boolean buyResult =
          tcsService.buyByMarket(TEST_TICKER, TICKER_TYPE, TEST_CASH_AMOUNT, takeProfit, stopLoss);
      out.println("Buy with TP/SL result: " + buyResult);
      sleepQuietly(SLEEP_MS);
    } catch (final Exception e) {
      out.println("Buy with TP/SL test failed: " + e.getMessage());
      e.printStackTrace();
    }
  }

  private static void testLimitBuy(final TCSService tcsService) {
    out.println("\n=== Test 10: Limit Buy ===");
    try {
      final Key key = new Key(TEST_TICKER, TICKER_TYPE);
      final var orderBook = tcsService.getCurrentPrices(key, false);
      final double bestAsk =
          orderBook.get("asks").keySet().stream().min(Double::compareTo).orElse(0.0);
      final double priceStep = tcsService.searchTicker(key).getMinPriceIncrement();
      final double limitPrice = bestAsk > 0.0 ? bestAsk + priceStep : 0.0;

      out.println("Best ask: " + bestAsk);
      out.println("Limit price: " + limitPrice);

      final boolean limitBuyResult =
          tcsService.buyLimit(TEST_TICKER, TICKER_TYPE, TEST_CASH_AMOUNT, limitPrice).isSuccess();
      out.println("Limit buy result: " + limitBuyResult);
      sleepQuietly(SLEEP_MS);
    } catch (final Exception e) {
      out.println("Limit buy test failed: " + e.getMessage());
      e.printStackTrace();
    }
  }

  private static void testLimitSell(final TCSService tcsService) {
    out.println("\n=== Test 10b: Limit Sell ===");
    try {
      final Key key = new Key(TEST_TICKER, TICKER_TYPE);
      final var orderBook = tcsService.getCurrentPrices(key, false);
      final double bestBid =
          orderBook.get("bids").keySet().stream().max(Double::compareTo).orElse(0.0);
      final double priceStep = tcsService.searchTicker(key).getMinPriceIncrement();
      final double limitPrice = bestBid > 0.0 ? Math.max(priceStep, bestBid - priceStep) : 0.0;

      out.println("Best bid: " + bestBid);
      out.println("Limit price: " + limitPrice);

      final boolean limitSellResult =
          tcsService.sellLimit(TEST_TICKER, TICKER_TYPE, TEST_CASH_AMOUNT, limitPrice).isSuccess();
      out.println("Limit sell result: " + limitSellResult);
      sleepQuietly(SLEEP_MS);
    } catch (final Exception e) {
      out.println("Limit sell test failed: " + e.getMessage());
      e.printStackTrace();
    }
  }

  private static void testClosePosition(final TCSService tcsService) {
    out.println("\n=== Test 11: Close Position by Market ===");
    try {
      final boolean closeResult = tcsService.closeByMarket(TEST_TICKER, TICKER_TYPE);
      out.println("Close position result: " + closeResult);
      sleepQuietly(SLEEP_MS);
    } catch (final Exception e) {
      out.println("Close position test failed: " + e.getMessage());
      e.printStackTrace();
    }
  }

  private static void testFinalPositionCheck(final TCSService tcsService) {
    out.println("\n=== Test 12: Final Position Check ===");
    try {
      sleepQuietly(1000);
      final PositionInfo position = tcsService.getCurrentPositions(TICKER_TYPE, TEST_TICKER);
      if (position != null) {
        out.println("Final position: " + position.getTicker());
        out.println("Balance: " + position.getBalance());
      } else {
        out.println("No position found (closed successfully)");
      }
    } catch (final Exception e) {
      out.println("Final position check failed: " + e.getMessage());
      e.printStackTrace();
    }
  }

  private static void testFinalCashCheck(
      final TCSService tcsService, final MarketConfig marketConfig) {
    out.println("\n=== Test 13: Final Cash Check ===");
    try {
      final double finalCash = tcsService.getAvailableCash();
      out.println("Final Available Cash: " + finalCash + " " + marketConfig.getCurrency());
    } catch (final Exception e) {
      out.println("Final cash check failed: " + e.getMessage());
      e.printStackTrace();
    }
  }

  private static ru.tinkoff.piapi.contract.v1.Quotation createQuotation(final double price) {
    final long units = (long) price;
    final double fractional = price - units;
    final int nano = (int) Math.round(fractional * 1_000_000_000);
    return ru.tinkoff.piapi.contract.v1.Quotation.newBuilder()
        .setUnits(units)
        .setNano(nano)
        .build();
  }

  private static Double toDouble(final long units, final int nano) {
    final double fractional = nano / 1_000_000_000.0;
    return units + fractional;
  }

  private static void sleepQuietly(final int milliseconds) {
    try {
      Thread.sleep(milliseconds);
    } catch (final InterruptedException e) {
      Thread.currentThread().interrupt();
      out.println("Sleep interrupted");
    }
  }
}
