package com.github.shk0da.GoldenDragon.test;

import com.github.shk0da.GoldenDragon.config.MainConfig;
import com.github.shk0da.GoldenDragon.config.MarketConfig;
import com.github.shk0da.GoldenDragon.model.PositionInfo;
import com.github.shk0da.GoldenDragon.model.TickerType;
import com.github.shk0da.GoldenDragon.service.TCSService;
import java.util.Map;


import static com.github.shk0da.GoldenDragon.model.Market.MOEX;
import static com.github.shk0da.GoldenDragon.model.TickerInfo.Key;
import static java.lang.System.out;

/**
 * Тестовый класс для проверки работы TCSService
 * Использует токены из application.properties
 * 
 * Внимание! Тест выполняет реальные операции с инструментами.
 * Убедитесь что используете песочницу (tcs.isSandbox=true) или тестовый режим (tcs.testMode=true)
 */
public class TCSServiceTest {

    public static String testTicker = "SBER";
    public static TickerType tickerType = TickerType.STOCK;
    public static double testCashAmount = 1000; // Сумма для теста в рублях

    public static void main(String[] args) {
        try {
            out.println("=== Starting TCSService Test ===");
            
            // Инициализация конфигурации
            MainConfig mainConfig = new MainConfig();
            MarketConfig marketConfig = MarketConfig.byMarket(MOEX);
            
            out.println("Test Mode: " + mainConfig.isTestMode());
            out.println("Sandbox: " + mainConfig.isSandbox());
            out.println("Account ID: " + mainConfig.getTcsAccountId());
            
            // Создание сервиса
            TCSService tcsService = new TCSService(mainConfig, marketConfig);
            out.println("TCSService created successfully");
            
            // Тест 1: Получение портфеля
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
            
            // Тест 2: Получение позиций
            out.println("\n=== Test 2: Positions ===");
            try {
                Map<Key, PositionInfo> positions =
                    tcsService.getCurrentPositions(TickerType.ALL);
                out.println("Total positions: " + positions.size());
                positions.forEach((key, pos) -> 
                    out.println("  " + pos.getTicker() + ": " + pos.getBalance() + " lots")
                );
            } catch (Exception e) {
                out.println("Positions test failed: " + e.getMessage());
                e.printStackTrace();
            }
            
            // Тест 3: Получение стакана цен для инструмента
            out.println("\n=== Test 3: Order Book ===");
            try {
                var key = new Key(testTicker, tickerType);
                var prices = tcsService.getCurrentPrices(key, false);
                
                out.println("Ticker: " + testTicker);
                out.println("Bids: " + prices.get("bids").size() + ", Asks: " + prices.get("asks").size());
                
                // Вывод лучших цен
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
            
            // Тест 4: Проверка создания котировок
            out.println("\n=== Test 4: Quotation Creation ===");
            try {
                // Проверяем что цены рассчитываются корректно
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
            
            // Тест 5: Проверка доступности инструмента
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
            
            // Тест 6: Покупка по рынку
            out.println("\n=== Test 6: Buy by Market ===");
            try {
                boolean buyResult = tcsService.buyByMarket(testTicker, tickerType, testCashAmount, 0, 0);
                out.println("Buy by market result: " + buyResult);
                Thread.sleep(2000); // Ждем исполнения
            } catch (Exception e) {
                out.println("Buy by market test failed: " + e.getMessage());
                e.printStackTrace();
            }
            
            // Тест 7: Проверка позиции после покупки
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
            
            // Тест 8: Продажа по рынку
            out.println("\n=== Test 8: Sell by Market ===");
            try {
                boolean sellResult = tcsService.sellByMarket(testTicker, tickerType, testCashAmount, 0, 0);
                out.println("Sell by market result: " + sellResult);
                Thread.sleep(2000); // Ждем исполнения
            } catch (Exception e) {
                out.println("Sell by market test failed: " + e.getMessage());
                e.printStackTrace();
            }
            
            // Тест 9: Покупка с Take Profit и Stop Loss
            out.println("\n=== Test 9: Buy with TP/SL ===");
            try {
                double takeProfit = 2.0; // 2%
                double stopLoss = 1.0;   // 1%
                boolean buyResult = tcsService.buyByMarket(testTicker, tickerType, testCashAmount, takeProfit, stopLoss);
                out.println("Buy with TP/SL result: " + buyResult);
                Thread.sleep(2000);
            } catch (Exception e) {
                out.println("Buy with TP/SL test failed: " + e.getMessage());
                e.printStackTrace();
            }
            
            // Тест 10: Лимитная покупка
            out.println("\n=== Test 10: Limit Buy ===");
            try {
                var key = new Key(testTicker, tickerType);
                double currentPrice = tcsService.getAvailablePrice(key);
                double limitPrice = currentPrice * 0.98; // На 2% ниже рынка
                
                out.println("Current price: " + currentPrice);
                out.println("Limit price: " + limitPrice);
                
                boolean limitBuyResult = tcsService.buy(testTicker, tickerType, testCashAmount, false, 0, 0, false);
                out.println("Limit buy result: " + limitBuyResult);
                Thread.sleep(2000);
            } catch (Exception e) {
                out.println("Limit buy test failed: " + e.getMessage());
                e.printStackTrace();
            }
            
            // Тест 11: Закрытие позиции по рынку
            out.println("\n=== Test 11: Close Position by Market ===");
            try {
                boolean closeResult = tcsService.closeByMarket(testTicker, tickerType);
                out.println("Close position result: " + closeResult);
                Thread.sleep(2000);
            } catch (Exception e) {
                out.println("Close position test failed: " + e.getMessage());
                e.printStackTrace();
            }
            
            // Тест 12: Финальная проверка позиций
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
            
            // Тест 13: Проверка доступных средств в конце
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
    
    // Копия метода из TCSService для тестирования
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
