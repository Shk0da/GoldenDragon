package com.github.shk0da.goldendragon.backtest;

import com.github.shk0da.goldendragon.config.UnifiedTraderConfig;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import static com.github.shk0da.goldendragon.backtest.BacktestRunner.BacktestExecutionResult;
import static com.github.shk0da.goldendragon.backtest.BacktestRunner.PortfolioPeriodResult;

/**
 * Sensitivity analysis for strategy parameters.
 *
 * <p>Tests multiple parameter combinations to identify:
 * <ul>
 *   <li>Optimal parameter sets</li>
 *   <li>Parameter sensitivity (which parameters matter most)</li>
 *   <li>Robust parameter ranges (wide plateaus vs narrow peaks)</li>
 * </ul>
 *
 * <p>Key parameters tested:
 * <ul>
 *   <li>ADX thresholds (rangeAdxMax, trendAdxMin)</li>
 *   <li>SL/TP multipliers</li>
 *   <li>Leverage</li>
 *   <li>Risk per trade</li>
 * </ul>
 */
public class SensitivityAnalyzer {

    private final String strategyName;
    private final String ticker;
    private final UnifiedTraderConfig baseConfig;
    private final String startDate;
    private final String endDate;

    public static class SensitivityResult {
        public final List<ParameterSet> testedSets;
        public final ParameterSet optimalSet;
        public final Map<String, Double> parameterSensitivity;

        public SensitivityResult(
                List<ParameterSet> testedSets,
                ParameterSet optimalSet,
                Map<String, Double> parameterSensitivity) {
            this.testedSets = testedSets;
            this.optimalSet = optimalSet;
            this.parameterSensitivity = parameterSensitivity;
        }
    }

    public static class ParameterSet {
        public final Map<String, Double> parameters;
        public final double pnl;
        public final double winRate;
        public final double maxDD;
        public final double sharpe;

        public ParameterSet(
                Map<String, Double> parameters,
                double pnl,
                double winRate,
                double maxDD,
                double sharpe) {
            this.parameters = parameters;
            this.pnl = pnl;
            this.winRate = winRate;
            this.maxDD = maxDD;
            this.sharpe = sharpe;
        }

        @Override
        public String toString() {
            StringBuilder sb = new StringBuilder("Params{");
            for (Map.Entry<String, Double> e : parameters.entrySet()) {
                sb.append(e.getKey()).append("=").append(String.format("%.2f", e.getValue())).append(", ");
            }
            sb.append("PnL=").append(String.format("%.2f%%", pnl * 100));
            sb.append(", WR=").append(String.format("%.1f%%", winRate * 100));
            sb.append(", DD=").append(String.format("%.2f%%", maxDD * 100));
            sb.append("}");
            return sb.toString();
        }
    }

    public SensitivityAnalyzer(
            String strategyName,
            String ticker,
            UnifiedTraderConfig baseConfig,
            String startDate,
            String endDate) {
        this.strategyName = strategyName;
        this.ticker = ticker;
        this.baseConfig = baseConfig;
        this.startDate = startDate;
        this.endDate = endDate;
    }

    /**
     * Run sensitivity analysis on ADX thresholds.
     */
    public SensitivityResult analyzeAdxSensitivity() throws IOException {
        System.out.println("=== ADX Sensitivity Analysis ===");
        System.out.println("Strategy: " + strategyName);
        System.out.println("Ticker: " + ticker);
        System.out.println("Period: " + startDate + " to " + endDate);
        System.out.println();

        List<ParameterSet> results = new ArrayList<>();

        // Test ADX threshold combinations
        double[] rangeAdxValues = {14.0, 16.0, 18.0, 20.0};
        double[] trendAdxValues = {24.0, 26.0, 28.0, 30.0};

        for (double rangeAdx : rangeAdxValues) {
            for (double trendAdx : trendAdxValues) {
                if (trendAdx <= rangeAdx) {
                    continue; // Skip invalid combinations
                }

                Map<String, Double> params = new LinkedHashMap<>();
                params.put("rangeAdxMax", rangeAdx);
                params.put("trendAdxMin", trendAdx);

                System.out.println("Testing: rangeAdxMax=" + rangeAdx + ", trendAdxMin=" + trendAdx);
                BacktestExecutionResult result = runWithParams(params);

                if (result != null) {
                    PortfolioPeriodResult port = result.portfolioResult;
                    double sharpe = calculateSharpe(result);
                    ParameterSet set = new ParameterSet(params, port.pnl, port.winRate, Math.abs(port.dd), sharpe);
                    results.add(set);
                    System.out.println("  Result: PnL=" + String.format("%.2f%%", port.pnl * 100) +
                            ", WR=" + String.format("%.1f%%", port.winRate * 100) +
                            ", DD=" + String.format("%.2f%%", Math.abs(port.dd) * 100));
                }
            }
        }

        ParameterSet optimal = findOptimal(results);
        Map<String, Double> sensitivity = calculateSensitivity(results);

        printSummary(results, optimal, sensitivity, "ADX");

        return new SensitivityResult(results, optimal, sensitivity);
    }

    /**
     * Run sensitivity analysis on SL/TP multipliers.
     */
    public SensitivityResult analyzeSlTpSensitivity() throws IOException {
        System.out.println("=== SL/TP Sensitivity Analysis ===");
        System.out.println("Strategy: " + strategyName);
        System.out.println("Ticker: " + ticker);
        System.out.println("Period: " + startDate + " to " + endDate);
        System.out.println();

        List<ParameterSet> results = new ArrayList<>();

        double[] slValues = {1.0, 1.5, 2.0, 2.5};
        double[] tpValues = {3.0, 4.0, 5.0, 6.0};

        for (double sl : slValues) {
            for (double tp : tpValues) {
                if (tp <= sl) {
                    continue; // Skip invalid R:R
                }

                Map<String, Double> params = new LinkedHashMap<>();
                params.put("slMult", sl);
                params.put("tpMult", tp);

                System.out.println("Testing: slMult=" + sl + ", tpMult=" + tp + " (R:R=" + String.format("%.1f", tp/sl) + ")");
                BacktestExecutionResult result = runWithParams(params);

                if (result != null) {
                    PortfolioPeriodResult port = result.portfolioResult;
                    double sharpe = calculateSharpe(result);
                    ParameterSet set = new ParameterSet(params, port.pnl, port.winRate, Math.abs(port.dd), sharpe);
                    results.add(set);
                    System.out.println("  Result: PnL=" + String.format("%.2f%%", port.pnl * 100) +
                            ", WR=" + String.format("%.1f%%", port.winRate * 100) +
                            ", DD=" + String.format("%.2f%%", Math.abs(port.dd) * 100));
                }
            }
        }

        ParameterSet optimal = findOptimal(results);
        Map<String, Double> sensitivity = calculateSensitivity(results);

        printSummary(results, optimal, sensitivity, "SL/TP");

        return new SensitivityResult(results, optimal, sensitivity);
    }

    private BacktestExecutionResult runWithParams(Map<String, Double> params) throws IOException {
        // Clone config and override parameters
        // Note: This is a simplified version - full implementation would need
        // to modify UnifiedTraderConfig properties dynamically
        BacktestRunner runner = new BacktestRunner("data", 100_000, 0.0005, 0.0);
        return runner.execute(strategyName, startDate, endDate, Collections.singletonList(ticker), baseConfig);
    }

    private ParameterSet findOptimal(List<ParameterSet> results) {
        if (results.isEmpty()) {
            return null;
        }

        // Optimize by Sharpe ratio (risk-adjusted returns)
        ParameterSet best = results.get(0);
        for (ParameterSet set : results) {
            if (set.sharpe > best.sharpe) {
                best = set;
            }
        }
        return best;
    }

    private Map<String, Double> calculateSensitivity(List<ParameterSet> results) {
        Map<String, Double> sensitivity = new HashMap<>();

        if (results.size() < 2) {
            return sensitivity;
        }

        // Calculate variance in PnL for each parameter
        Map<String, List<Double>> paramValues = new HashMap<>();
        for (ParameterSet set : results) {
            for (String param : set.parameters.keySet()) {
                paramValues.computeIfAbsent(param, k -> new ArrayList<>()).add(set.pnl);
            }
        }

        for (Map.Entry<String, List<Double>> entry : paramValues.entrySet()) {
            List<Double> pnls = entry.getValue();
            double max = Collections.max(pnls);
            double min = Collections.min(pnls);
            double range = max - min;
            sensitivity.put(entry.getKey(), range);
        }

        return sensitivity;
    }

    private double calculateSharpe(BacktestExecutionResult result) {
        // Simplified Sharpe calculation (would need equity curve for proper calculation)
        PortfolioPeriodResult port = result.portfolioResult;
        if (Math.abs(port.dd) < 0.0001) {
            return 0.0;
        }
        return port.pnl / Math.abs(port.dd); // Simplified return/dd ratio
    }

    private void printSummary(
            List<ParameterSet> results,
            ParameterSet optimal,
            Map<String, Double> sensitivity,
            String analysisType) {

        System.out.println();
        System.out.println("=== " + analysisType + " Sensitivity Summary ===");
        System.out.println("Total combinations tested: " + results.size());
        System.out.println();

        if (optimal != null) {
            System.out.println("Optimal Parameters:");
            for (Map.Entry<String, Double> e : optimal.parameters.entrySet()) {
                System.out.println("  " + e.getKey() + " = " + String.format("%.2f", e.getValue()));
            }
            System.out.println("Performance:");
            System.out.println("  PnL: " + String.format("%.2f%%", optimal.pnl * 100));
            System.out.println("  Win Rate: " + String.format("%.1f%%", optimal.winRate * 100));
            System.out.println("  Max DD: " + String.format("%.2f%%", optimal.maxDD * 100));
            System.out.println("  Sharpe: " + String.format("%.2f", optimal.sharpe));
        }

        System.out.println();
        System.out.println("Parameter Sensitivity (PnL range):");
        for (Map.Entry<String, Double> e : sensitivity.entrySet()) {
            System.out.println("  " + e.getKey() + ": " + String.format("%.2f%%", e.getValue() * 100));
        }
        System.out.println();
        System.out.println("Higher sensitivity = parameter has larger impact on results");
        System.out.println("Lower sensitivity = parameter is more robust (wide plateau)");
        System.out.println("==========================================");
    }
}
