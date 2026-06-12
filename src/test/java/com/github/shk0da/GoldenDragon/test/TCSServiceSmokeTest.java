package com.github.shk0da.GoldenDragon.test;

import static com.github.shk0da.GoldenDragon.model.Market.MOEX;
import static com.github.shk0da.GoldenDragon.model.TickerInfo.Key;
import static java.lang.System.out;

import com.github.shk0da.GoldenDragon.config.MainConfig;
import com.github.shk0da.GoldenDragon.config.MarketConfig;
import com.github.shk0da.GoldenDragon.model.PositionInfo;
import com.github.shk0da.GoldenDragon.model.TickerType;
import com.github.shk0da.GoldenDragon.service.TCSService;
import java.util.Map;

/**
 * Manual smoke test for TCSService. Uses credentials from application.properties.
 *
 * <p>Warning: this test performs real trading operations. Make sure sandbox mode
 * (tcs.isSandbox=true) or test mode (tcs.testMode=true) is enabled.
 */
public class TCSServiceSmokeTest {

  public static String testTicker = "SBERF";
  public static TickerType tickerType = TickerType.FEATURE;
  public static double testCashAmount = 100_000; // Сумма для теста в рублях

  public static void main(String[] args) {
    try {
      out.println("=== Starting TCSService Test ===");

      // Initialize config
      MainConfig mainConfig = new MainConfig();
      MarketConfig marketConfig = MarketConfig.byMarket(MOEX);

      out.println("Test Mode: " + mainConfig.isTestMode());
      out.println("Sandbox: " + mainConfig.isSandbox());
      out.println("Account ID: " + mainConfig.getTcsAccountId());

      // Create service
      TCSService tcsService = new TCSService(mainConfig, marketConfig);
      out.println("TCSService created successfully");

      // Test 1: Portfolio snapshot
      out.println("\n=== Test 1: Portfolio ===");
      try {
        double totalPortfolio = tcsService.getTotalPortfolioCost();
        out.println("Total Portfolio: " + totalPortfolio + " " + marketConfig.getCurrency());

        double availableCash = tcsService.getAvailableCash();
        out.println("Available Cash: " + availableCash + " " + marketConfig.getCurrency());
      } catch (Exception e) {
        out.println("Portfolio test failed: " + e.getMessage());
        e.printStackTrace();
      }

      // Test 2: Current positions snapshot
      out.println("\n=== Test 2: Positions ===");
      try {
        Map<Key, PositionInfo> positions = tcsService.getCurrentPositions(TickerType.ALL);
        out.println("Total positions: " + positions.size());
        positions.forEach(
            (key, pos) -> out.println("  " + pos.getTicker() + ": " + pos.getBalance() + " lots"));
      } catch (Exception e) {
        out.println("Positions test failed: " + e.getMessage());
        e.printStackTrace();
      }

      // Test 3: Order book snapshot
      out.println("\n=== Test 3: Order Book ===");
      try {
        var key = new Key(testTicker, tickerType);
        var prices = tcsService.getCurrentPrices(key, false);

        out.println("Ticker: " + testTicker);
        out.println("Bids: " + prices.get("bids").size() + ", Asks: " + prices.get("asks").size());

        // Print best bid/ask
        if (!prices.get("bids").isEmpty()) {
          double bestBid = prices.get("bids").keySet().stream().max(Double::compareTo).orElse(0.0);
          out.println("Best Bid: " + bestBid);
        }
        if (!prices.get("asks").isEmpty()) {
          double bestAsk = prices.get("asks").keySet().stream().min(Double::compareTo).orElse(0.0);
          out.println("Best Ask: " + bestAsk);
        }
      } catch (Exception e) {
        out.println("Order book test failed: " + e.getMessage());
        e.printStackTrace();
      }

      // Test 4: Quotation conversion
      out.println("\n=== Test 4: Quotation Creation ===");
      try {
        // Verify quotation conversion precision
        double testPrice = 123.456789;
        var quotation = createQuotation(testPrice);
        double restoredPrice = toDouble(quotation.getUnits(), quotation.getNano());
        out.println("Original price: " + testPrice);
        out.println("Quotation units: " + quotation.getUnits() + ", nano: " + quotation.getNano());
        out.println("Restored price: " + restoredPrice);
        out.println("Difference: " + Math.abs(testPrice - restoredPrice));

        if (Math.abs(testPrice - restoredPrice) < 0.000001) {
          out.println("Quotation test PASSED");
        } else {
          out.println("Quotation test FAILED");
        }
      } catch (Exception e) {
        out.println("Quotation test failed: " + e.getMessage());
        e.printStackTrace();
      }

      // Test 5: Instrument lookup
      out.println("\n=== Test 5: Instrument Search ===");
      try {
        var key = new Key(testTicker, tickerType);
        var tickerInfo = tcsService.searchTicker(key);

        out.println("Ticker: " + tickerInfo.getTicker());
        out.println("FIGI: " + tickerInfo.getFigi());
        out.println("Lot: " + tickerInfo.getLot());
        out.println("Currency: " + tickerInfo.getCurrency());
        out.println("Min Price Increment: " + tickerInfo.getMinPriceIncrement());
      } catch (Exception e) {
        out.println("Instrument search test failed: " + e.getMessage());
        e.printStackTrace();
      }

      // Test 6: Market buy
      out.println("\n=== Test 6: Buy by Market ===");
      try {
        boolean buyResult = tcsService.buyByMarket(testTicker, tickerType, testCashAmount, 0, 0);
        out.println("Buy by market result: " + buyResult);
        Thread.sleep(2000); // wait for execution
      } catch (Exception e) {
        out.println("Buy by market test failed: " + e.getMessage());
        e.printStackTrace();
      }

      // Test 7: Position after market buy
      out.println("\n=== Test 7: Position After Buy ===");
      try {
        Thread.sleep(1000);
        PositionInfo position = tcsService.getCurrentPositions(tickerType, testTicker);
        if (position != null) {
          out.println("Position: " + position.getTicker());
          out.println("Balance: " + position.getBalance());
          out.println("Expected Yield: " + position.getExpectedYield() + "%");
        } else {
          out.println("No position found");
        }
      } catch (Exception e) {
        out.println("Position check failed: " + e.getMessage());
        e.printStackTrace();
      }

      // Test 8: Market sell
      out.println("\n=== Test 8: Sell by Market ===");
      try {
        boolean sellResult = tcsService.sellByMarket(testTicker, tickerType, testCashAmount, 0, 0);
        out.println("Sell by market result: " + sellResult);
        Thread.sleep(2000); // wait for execution
      } catch (Exception e) {
        out.println("Sell by market test failed: " + e.getMessage());
        e.printStackTrace();
      }

      // Test 9: Market buy with TP/SL
      out.println("\n=== Test 9: Buy with TP/SL ===");
      try {
        double takeProfit = 2.0; // 2%
        double stopLoss = 1.0; // 1%
        boolean buyResult =
            tcsService.buyByMarket(testTicker, tickerType, testCashAmount, takeProfit, stopLoss);
        out.println("Buy with TP/SL result: " + buyResult);
        Thread.sleep(2000);
      } catch (Exception e) {
        out.println("Buy with TP/SL test failed: " + e.getMessage());
        e.printStackTrace();
      }

      // Test 10: Limit buy
      out.println("\n=== Test 10: Limit Buy ===");
      try {
        var key = new Key(testTicker, tickerType);
        var orderBook = tcsService.getCurrentPrices(key, false);
        double bestAsk = orderBook.get("asks").keySet().stream().min(Double::compareTo).orElse(0.0);
        double priceStep = tcsService.searchTicker(key).getMinPriceIncrement();
        double limitPrice = bestAsk > 0.0 ? bestAsk + priceStep : 0.0;

        out.println("Best ask: " + bestAsk);
        out.println("Limit price: " + limitPrice);

        boolean limitBuyResult =
            tcsService.buyLimit(testTicker, tickerType, testCashAmount, limitPrice).isSuccess();
        out.println("Limit buy result: " + limitBuyResult);
        Thread.sleep(2000);
      } catch (Exception e) {
        out.println("Limit buy test failed: " + e.getMessage());
        e.printStackTrace();
      }

      // Test 10b: Limit sell
      out.println("\n=== Test 10b: Limit Sell ===");
      try {
        var key = new Key(testTicker, tickerType);
        var orderBook = tcsService.getCurrentPrices(key, false);
        double bestBid = orderBook.get("bids").keySet().stream().max(Double::compareTo).orElse(0.0);
        double priceStep = tcsService.searchTicker(key).getMinPriceIncrement();
        double limitPrice = bestBid > 0.0 ? Math.max(priceStep, bestBid - priceStep) : 0.0;

        out.println("Best bid: " + bestBid);
        out.println("Limit price: " + limitPrice);

        boolean limitSellResult =
            tcsService.sellLimit(testTicker, tickerType, testCashAmount, limitPrice).isSuccess();
        out.println("Limit sell result: " + limitSellResult);
        Thread.sleep(2000);
      } catch (Exception e) {
        out.println("Limit sell test failed: " + e.getMessage());
        e.printStackTrace();
      }

      // Test 11: Close position by market
      out.println("\n=== Test 11: Close Position by Market ===");
      try {
        boolean closeResult = tcsService.closeByMarket(testTicker, tickerType);
        out.println("Close position result: " + closeResult);
        Thread.sleep(2000);
      } catch (Exception e) {
        out.println("Close position test failed: " + e.getMessage());
        e.printStackTrace();
      }

      // Test 12: Final position check
      out.println("\n=== Test 12: Final Position Check ===");
      try {
        Thread.sleep(1000);
        PositionInfo position = tcsService.getCurrentPositions(tickerType, testTicker);
        if (position != null) {
          out.println("Final position: " + position.getTicker());
          out.println("Balance: " + position.getBalance());
        } else {
          out.println("No position found (closed successfully)");
        }
      } catch (Exception e) {
        out.println("Final position check failed: " + e.getMessage());
        e.printStackTrace();
      }

      // Test 13: Final cash check
      out.println("\n=== Test 13: Final Cash Check ===");
      try {
        double finalCash = tcsService.getAvailableCash();
        out.println("Final Available Cash: " + finalCash + " " + marketConfig.getCurrency());
      } catch (Exception e) {
        out.println("Final cash check failed: " + e.getMessage());
        e.printStackTrace();
      }

      out.println("\n=== TCSService Test Complete ===");
      out.println("All tests finished. Check results above.");

    } catch (Exception e) {
      out.println("Test failed with exception: " + e.getMessage());
      e.printStackTrace();
    }
  }

  // Copy of TCSService helper for quotation testing
  private static ru.tinkoff.piapi.contract.v1.Quotation createQuotation(double price) {
    long units = (long) price;
    double fractional = price - units;
    int nano = (int) Math.round(fractional * 1_000_000_000);
    return ru.tinkoff.piapi.contract.v1.Quotation.newBuilder()
        .setUnits(units)
        .setNano(nano)
        .build();
  }

  private static Double toDouble(long units, int nano) {
    double fractional = nano / 1_000_000_000.0;
    return units + fractional;
  }
}
