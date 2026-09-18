package com.github.shk0da.goldendragon.backtest;

import com.github.shk0da.goldendragon.backtest.BacktestRunner.BacktestExecutionResult;
import com.github.shk0da.goldendragon.backtest.BacktestRunner.PortfolioPeriodResult;
import com.github.shk0da.goldendragon.config.UnifiedTraderConfig;
import com.github.shk0da.goldendragon.model.Config;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

/**
 * Optimizer for trailing stop parameters.
 * Runs backtests with different parameter combinations and selects the best configuration.
 */
public class BacktestTrailingOptimizer {

    private static final double INITIAL_BALANCE = 100_000;
    private static final double COMMISSION = 0.0005;
    private static final double SLIPPAGE = 0.0;
    // Use full date range like standard backtest (4+ years)
    private static final String START_DATE = "2022-01-01";
    private static final String END_DATE = "2026-12-31";
    // Use all tickers from standard backtest for meaningful optimization
    private static final List<String> TICKERS = List.of(
        "GMKN", "VTBR", "T", "SNGS", "GLDRUBF", "IMOEXF", "MGNT", "PLZL", "YDEX",
        "MTSS", "GAZPF", "SNGSP", "SIBN", "TATN", "OZON", "X5", "AKRN", "NLMK",
        "RUAL", "ALRS", "LENT", "RTKM", "HYDR", "VKCO", "FESH", "UPRO", "UWGN"
    );

    public static class TrailingConfig {
        public final double stepPercent;
        public final double deltaPercent;
        public final int checkInterval;

        public TrailingConfig(double stepPercent, double deltaPercent, int checkInterval) {
            this.stepPercent = stepPercent;
            this.deltaPercent = deltaPercent;
            this.checkInterval = checkInterval;
        }

        public Config toConfig() {
            Config.TrailingParams params = new Config.TrailingParams(true, stepPercent, deltaPercent, checkInterval, 0.5);
            return new Config(params);
        }
    }

    public static class BacktestResult {
        public final TrailingConfig config;
        public final double pnl;
        public final int trades;
        public final double winRate;
        public final double maxDD;

        public BacktestResult(TrailingConfig config, double pnl, int trades,
                              double winRate, double maxDD) {
            this.config = config;
            this.pnl = pnl;
            this.trades = trades;
            this.winRate = winRate;
            this.maxDD = maxDD;
        }
    }

    /**
     * Run backtest with specific trailing configuration.
     */
    private static BacktestResult runBacktestWithConfig(TrailingConfig config) throws IOException {
        Config modelConfig = config.toConfig();
        UnifiedTraderConfig traderConfig = new UnifiedTraderConfig();
        BacktestRunner runner = new BacktestRunner("data", INITIAL_BALANCE, COMMISSION, SLIPPAGE, 0.0);

        BacktestExecutionResult result = runner.execute(
                "UnifiedStrategy", START_DATE, END_DATE, TICKERS, traderConfig, modelConfig);

        PortfolioPeriodResult portfolio = result.portfolioResult;

        return new BacktestResult(
                config,
                portfolio.pnl,
                portfolio.totalTrades,
                portfolio.winRate,
                Math.abs(portfolio.dd));
    }

    /**
     * Run backtest with specific trailing configuration.
     */
    public static BacktestResult runOptimized(TrailingConfig config) throws IOException {
        return runBacktestWithConfig(config);
    }

    /**
     * Run optimization with multiple parameter combinations.
     */
    public static void optimize() throws IOException {
        System.out.println("=== Trailing Stop Parameter Optimization ===");
        System.out.println("Period: " + START_DATE + " to " + END_DATE);
        System.out.println("Tickers: " + TICKERS);
        System.out.println();

        // Define parameter search space (reduced for faster execution)
        List<TrailingConfig> configs = new ArrayList<>();

        // Step percentages: 0.5%, 1.0%, 1.5%
        double[] stepPercents = {0.005, 0.01, 0.015};
        // Delta percentages: 0.3%, 0.5%
        double[] deltaPercents = {0.003, 0.005};
        // Check intervals: 1, 2 candles
        int[] checkIntervals = {1, 2};

        for (double step : stepPercents) {
            for (double delta : deltaPercents) {
                for (int interval : checkIntervals) {
                    configs.add(new TrailingConfig(step, delta, interval));
                }
            }
        }

        System.out.println("Testing " + configs.size() + " configurations...");
        System.out.println();

        List<BacktestResult> results = new ArrayList<>();
        for (int i = 0; i < configs.size(); i++) {
            TrailingConfig config = configs.get(i);
            System.out.printf("Running config %d/%d: step=%.1f%%, delta=%.1f%%, interval=%d\n",
                    i + 1, configs.size(), config.stepPercent * 100,
                    config.deltaPercent * 100, config.checkInterval);

            try {
                BacktestResult result = runBacktestWithConfig(config);
                results.add(result);
                System.out.printf("  PnL: %.0f, Trades: %d, WinRate: %.1f%%, MaxDD: %.1f%%\n",
                        result.pnl, result.trades, result.winRate * 100, result.maxDD * 100);
            } catch (Exception e) {
                System.err.println("  Error: " + e.getMessage());
            }
        }

        // Find best configuration by PnL
        BacktestResult best = results.stream()
                .max(Comparator.comparingDouble(r -> r.pnl))
                .orElse(null);

        if (best != null) {
            System.out.println();
            System.out.println("=== Best Configuration ===");
            System.out.printf("Step: %.1f%%\n", best.config.stepPercent * 100);
            System.out.printf("Delta: %.1f%%\n", best.config.deltaPercent * 100);
            System.out.printf("Check Interval: %d candles\n", best.config.checkInterval);
            System.out.printf("PnL: %.0f\n", best.pnl);
            System.out.printf("Trades: %d\n", best.trades);
            System.out.printf("Win Rate: %.1f%%\n", best.winRate * 100);
            System.out.printf("Max DD: %.1f%%\n", best.maxDD * 100);
        }
    }

    public static void main(String[] args) throws IOException {
        optimize();
    }
}
