package com.github.shk0da.GoldenDragon.test;

/**
 * Performance test for parallel tick loading and timeline building.
 * 
 * Note: Current implementation always uses parallel processing.
 * This test shows timing breakdown for different phases.
 */
public class MarketTickBacktestRunnerPerformanceTest {

    public static void main(String[] args) throws Exception {
        System.out.println("=== MarketTickBacktestRunner Performance Test ===");
        System.out.println("Available processors: " + Runtime.getRuntime().availableProcessors());
        System.out.println();

        System.out.println("--- Running Backtest (Parallel by Default) ---");
        long startTime = System.currentTimeMillis();
        MarketTickBacktestRunner runner = new MarketTickBacktestRunner();
        runner.run();
        long totalTime = System.currentTimeMillis() - startTime;
        
        System.out.println();
        System.out.println("=== Performance Summary ===");
        System.out.println("Total time: " + totalTime + "ms");
        System.out.println("See detailed timing in output above");
    }
}
