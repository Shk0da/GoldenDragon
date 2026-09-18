package com.github.shk0da.goldendragon.backtest;

import com.github.shk0da.goldendragon.config.UnifiedTraderConfig;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import static com.github.shk0da.goldendragon.backtest.BacktestRunner.BacktestExecutionResult;
import static com.github.shk0da.goldendragon.backtest.BacktestRunner.PortfolioPeriodResult;

/**
 * Walk-forward analysis for strategy validation.
 *
 * <p>Performs iterative train/test splits:
 * <ul>
 *   <li>Train period (in-sample): Validate strategy logic</li>
 *   <li>Test period (out-of-sample): Validate performance on unseen data</li>
 * </ul>
 *
 * <p>Typical configuration:
 * <ul>
 *   <li>Train window: 3-6 months</li>
 *   <li>Test window: 1-2 months</li>
 *   <li>Step: 1 month (rolling window)</li>
 * </ul>
 *
 * <p>Output metrics:
 * <ul>
 *   <li>IS/OOS performance ratio (should be 0.7-1.3 for robust strategies)</li>
 *   <li>Win rate consistency</li>
 *   <li>Max drawdown comparison</li>
 * </ul>
 */
public class WalkForwardAnalyzer {

    private final String strategyName;
    private final String ticker;
    private final UnifiedTraderConfig config;
    private final int trainMonths;
    private final int testMonths;
    private final int stepMonths;

    public static class WalkForwardResult {
        public final List<PeriodPair> periods;
        public final Map<String, BacktestExecutionResult> trainResults;
        public final Map<String, BacktestExecutionResult> testResults;
        public final SummaryMetrics summary;

        public WalkForwardResult(
                List<PeriodPair> periods,
                Map<String, BacktestExecutionResult> trainResults,
                Map<String, BacktestExecutionResult> testResults,
                SummaryMetrics summary) {
            this.periods = periods;
            this.trainResults = trainResults;
            this.testResults = testResults;
            this.summary = summary;
        }
    }

    public static class PeriodPair {
        public final String trainStart;
        public final String trainEnd;
        public final String testStart;
        public final String testEnd;
        public final String label;

        public PeriodPair(String trainStart, String trainEnd, String testStart, String testEnd, String label) {
            this.trainStart = trainStart;
            this.trainEnd = trainEnd;
            this.testStart = testStart;
            this.testEnd = testEnd;
            this.label = label;
        }
    }

    public static class SummaryMetrics {
        public final double avgTrainPnl;
        public final double avgTestPnl;
        public final double pnlRatio;
        public final double avgTrainWinRate;
        public final double avgTestWinRate;
        public final double winRateRatio;
        public final double avgTrainMaxDD;
        public final double avgTestMaxDD;
        public final double ddRatio;
        public final boolean isRobust;

        public SummaryMetrics(
                double avgTrainPnl,
                double avgTestPnl,
                double pnlRatio,
                double avgTrainWinRate,
                double avgTestWinRate,
                double winRateRatio,
                double avgTrainMaxDD,
                double avgTestMaxDD,
                double ddRatio) {
            this.avgTrainPnl = avgTrainPnl;
            this.avgTestPnl = avgTestPnl;
            this.pnlRatio = pnlRatio;
            this.avgTrainWinRate = avgTrainWinRate;
            this.avgTestWinRate = avgTestWinRate;
            this.winRateRatio = winRateRatio;
            this.avgTrainMaxDD = avgTrainMaxDD;
            this.avgTestMaxDD = avgTestMaxDD;
            this.ddRatio = ddRatio;
            this.isRobust = pnlRatio >= 0.7 && pnlRatio <= 1.3;
        }
    }

    public WalkForwardAnalyzer(
            String strategyName,
            String ticker,
            UnifiedTraderConfig config,
            int trainMonths,
            int testMonths,
            int stepMonths) {
        this.strategyName = strategyName;
        this.ticker = ticker;
        this.config = config;
        this.trainMonths = trainMonths;
        this.testMonths = testMonths;
        this.stepMonths = stepMonths;
    }

    public WalkForwardResult analyze(String startDate, String endDate) throws IOException {
        System.out.println("=== Walk-Forward Analysis ===");
        System.out.println("Strategy: " + strategyName);
        System.out.println("Ticker: " + ticker);
        System.out.println("Period: " + startDate + " to " + endDate);
        System.out.println("Train window: " + trainMonths + " months");
        System.out.println("Test window: " + testMonths + " months");
        System.out.println("Step: " + stepMonths + " month(s)");
        System.out.println();

        List<PeriodPair> periodPairs = generatePeriodPairs(startDate, endDate);
        Map<String, BacktestExecutionResult> trainResults = new LinkedHashMap<>();
        Map<String, BacktestExecutionResult> testResults = new LinkedHashMap<>();

        for (PeriodPair pair : periodPairs) {
            System.out.println("Running period: " + pair.label);

            System.out.println("  Train: " + pair.trainStart + " to " + pair.trainEnd);
            BacktestExecutionResult trainResult = runBacktest(
                    pair.trainStart, pair.trainEnd, Collections.singletonList(ticker));
            trainResults.put(pair.label, trainResult);

            System.out.println("  Test: " + pair.testStart + " to " + pair.testEnd);
            BacktestExecutionResult testResult = runBacktest(
                    pair.testStart, pair.testEnd, Collections.singletonList(ticker));
            testResults.put(pair.label, testResult);

            System.out.println();
        }

        SummaryMetrics summary = calculateSummary(trainResults, testResults);
        printSummary(summary);

        return new WalkForwardResult(periodPairs, trainResults, testResults, summary);
    }

    private List<PeriodPair> generatePeriodPairs(String startDate, String endDate) {
        List<PeriodPair> pairs = new ArrayList<>();

        Calendar trainStartCal = parseDate(startDate);
        Calendar endCal = parseDate(endDate);

        int pairIndex = 1;
        while (true) {
            Calendar trainEndCal = (Calendar) trainStartCal.clone();
            trainEndCal.add(Calendar.MONTH, trainMonths);

            if (trainEndCal.after(endCal)) {
                break;
            }

            Calendar testStartCal = (Calendar) trainEndCal.clone();
            testStartCal.add(Calendar.DAY_OF_MONTH, 1);

            Calendar testEndCal = (Calendar) testStartCal.clone();
            testEndCal.add(Calendar.MONTH, testMonths);

            if (testEndCal.after(endCal)) {
                testEndCal = (Calendar) endCal.clone();
            }

            String label = "WF-" + String.format("%02d", pairIndex);
            pairs.add(new PeriodPair(
                    formatDate(trainStartCal),
                    formatDate(trainEndCal),
                    formatDate(testStartCal),
                    formatDate(testEndCal),
                    label));

            trainStartCal.add(Calendar.MONTH, stepMonths);
            pairIndex++;
        }

        return pairs;
    }

    private BacktestExecutionResult runBacktest(
            String startDate,
            String endDate,
            List<String> tickers) throws IOException {
        BacktestRunner runner = new BacktestRunner("data", 100_000, 0.0005, 0.0);
        return runner.execute(strategyName, startDate, endDate, tickers, config);
    }

    private SummaryMetrics calculateSummary(
            Map<String, BacktestExecutionResult> trainResults,
            Map<String, BacktestExecutionResult> testResults) {

        double totalTrainPnl = 0.0;
        double totalTestPnl = 0.0;
        double totalTrainWinRate = 0.0;
        double totalTestWinRate = 0.0;
        double totalTrainMaxDD = 0.0;
        double totalTestMaxDD = 0.0;
        int count = 0;

        for (String label : trainResults.keySet()) {
            BacktestExecutionResult train = trainResults.get(label);
            BacktestExecutionResult test = testResults.get(label);

            if (train != null && test != null) {
                PortfolioPeriodResult trainPort = train.portfolioResult;
                PortfolioPeriodResult testPort = test.portfolioResult;

                totalTrainPnl += trainPort.pnl;
                totalTestPnl += testPort.pnl;

                totalTrainWinRate += trainPort.winRate;
                totalTestWinRate += testPort.winRate;

                totalTrainMaxDD += Math.abs(trainPort.dd);
                totalTestMaxDD += Math.abs(testPort.dd);

                count++;
            }
        }

        if (count == 0) {
            return new SummaryMetrics(0, 0, 0, 0, 0, 0, 0, 0, 0);
        }

        double avgTrainPnl = totalTrainPnl / count;
        double avgTestPnl = totalTestPnl / count;
        double pnlRatio = avgTrainPnl != 0 ? avgTestPnl / avgTrainPnl : 0;

        double avgTrainWinRate = totalTrainWinRate / count;
        double avgTestWinRate = totalTestWinRate / count;
        double winRateRatio = avgTrainWinRate != 0 ? avgTestWinRate / avgTrainWinRate : 0;

        double avgTrainMaxDD = totalTrainMaxDD / count;
        double avgTestMaxDD = totalTestMaxDD / count;
        double ddRatio = avgTrainMaxDD != 0 ? avgTestMaxDD / avgTrainMaxDD : 0;

        return new SummaryMetrics(
                avgTrainPnl,
                avgTestPnl,
                pnlRatio,
                avgTrainWinRate,
                avgTestWinRate,
                winRateRatio,
                avgTrainMaxDD,
                avgTestMaxDD,
                ddRatio);
    }

    private void printSummary(SummaryMetrics summary) {
        System.out.println("=== Walk-Forward Summary ===");
        System.out.printf("Average Train PnL: %.2f%%\n", summary.avgTrainPnl * 100);
        System.out.printf("Average Test PnL: %.2f%%\n", summary.avgTestPnl * 100);
        System.out.printf("PnL Ratio (Test/Train): %.2f\n", summary.pnlRatio);
        System.out.println();
        System.out.printf("Average Train Win Rate: %.1f%%\n", summary.avgTrainWinRate * 100);
        System.out.printf("Average Test Win Rate: %.1f%%\n", summary.avgTestWinRate * 100);
        System.out.printf("Win Rate Ratio: %.2f\n", summary.winRateRatio);
        System.out.println();
        System.out.printf("Average Train Max DD: %.2f%%\n", summary.avgTrainMaxDD * 100);
        System.out.printf("Average Test Max DD: %.2f%%\n", summary.avgTestMaxDD * 100);
        System.out.printf("Drawdown Ratio: %.2f\n", summary.ddRatio);
        System.out.println();
        System.out.println("Robustness: " + (summary.isRobust ? "PASS" : "FAIL"));
        System.out.println("(Robust if PnL ratio 0.7-1.3)");
        System.out.println("============================");
    }

    private Calendar parseDate(String dateStr) {
        // Support both ISO (YYYY-MM-DD) and European (DD.MM.YYYY) formats
        Calendar cal = Calendar.getInstance();
        if (dateStr.contains("-")) {
            // ISO format: YYYY-MM-DD
            String[] parts = dateStr.split("-");
            cal.set(Calendar.YEAR, Integer.parseInt(parts[0]));
            cal.set(Calendar.MONTH, Integer.parseInt(parts[1]) - 1);
            cal.set(Calendar.DAY_OF_MONTH, Integer.parseInt(parts[2]));
        } else {
            // European format: DD.MM.YYYY
            String[] parts = dateStr.split("\\.");
            cal.set(Calendar.DAY_OF_MONTH, Integer.parseInt(parts[0]));
            cal.set(Calendar.MONTH, Integer.parseInt(parts[1]) - 1);
            cal.set(Calendar.YEAR, Integer.parseInt(parts[2]));
        }
        cal.set(Calendar.HOUR_OF_DAY, 0);
        cal.set(Calendar.MINUTE, 0);
        cal.set(Calendar.SECOND, 0);
        cal.set(Calendar.MILLISECOND, 0);
        return cal;
    }

    private String formatDate(Calendar cal) {
        return String.format("%04d-%02d-%02d",
                cal.get(Calendar.YEAR),
                cal.get(Calendar.MONTH) + 1,
                cal.get(Calendar.DAY_OF_MONTH));
    }
}
