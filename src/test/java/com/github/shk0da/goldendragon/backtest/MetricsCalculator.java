package com.github.shk0da.goldendragon.backtest;

import java.util.ArrayList;
import java.util.List;

/**
 * Calculator for advanced trading performance metrics.
 *
 * <p>Provides risk-adjusted return metrics following industry standards:
 * <ul>
 *   <li>Sharpe Ratio - risk-adjusted return (total volatility)</li>
 *   <li>Sortino Ratio - risk-adjusted return (downside volatility only)</li>
 *   <li>Calmar Ratio - CAGR / MaxDrawdown</li>
 *   <li>Recovery Factor - Gross Profit / MaxDrawdown</li>
 *   <li>R-Multiple - PnL normalized by initial risk</li>
 * </ul>
 *
 * <p>All metrics follow CFA Institute and industry-standard calculations.
 */
public class MetricsCalculator {

    private MetricsCalculator() {
        // Utility class
    }

    /**
     * Calculate annualized Sharpe Ratio.
     *
     * @param returns list of periodic returns (e.g., monthly returns as decimals: 0.05 = 5%)
     * @param riskFreeRate annual risk-free rate (e.g., 0.05 for 5%)
     * @param periodsPerYear number of periods per year (12 for monthly, 252 for daily)
     * @return annualized Sharpe Ratio, or 0.0 if insufficient data
     */
    public static double calculateSharpeRatio(
        List<Double> returns,
        double riskFreeRate,
        int periodsPerYear) {

        if (returns == null || returns.size() < 2) {
            return 0.0;
        }

        // Calculate average return
        double avgReturn = returns.stream()
            .mapToDouble(Double::doubleValue)
            .average()
            .orElse(0.0);

        // Calculate standard deviation
        double variance = returns.stream()
            .mapToDouble(r -> Math.pow(r - avgReturn, 2))
            .average()
            .orElse(0.0);
        double stdDev = Math.sqrt(variance);

        if (stdDev == 0.0) {
            return 0.0;
        }

        // Annualize: (avgReturn - riskFreeRate/periodsPerYear) * periodsPerYear / (stdDev * sqrt(periodsPerYear))
        double periodicRiskFreeRate = riskFreeRate / periodsPerYear;
        double excessReturn = avgReturn - periodicRiskFreeRate;
        double annualizedSharpe = (excessReturn * periodsPerYear) / (stdDev * Math.sqrt(periodsPerYear));

        return annualizedSharpe;
    }

    /**
     * Calculate annualized Sortino Ratio (uses downside deviation only).
     *
     * @param returns list of periodic returns (e.g., monthly returns as decimals)
     * @param riskFreeRate annual risk-free rate
     * @param periodsPerYear number of periods per year (12 for monthly, 252 for daily)
     * @return annualized Sortino Ratio, or 0.0 if insufficient data or no negative returns
     */
    public static double calculateSortinoRatio(
        List<Double> returns,
        double riskFreeRate,
        int periodsPerYear) {

        if (returns == null || returns.size() < 2) {
            return 0.0;
        }

        // Calculate average return
        double avgReturn = returns.stream()
            .mapToDouble(Double::doubleValue)
            .average()
            .orElse(0.0);

        // Calculate downside deviation (only negative returns)
        double periodicRiskFreeRate = riskFreeRate / periodsPerYear;
        double sumSquaredDownside = returns.stream()
            .filter(r -> r < periodicRiskFreeRate)
            .mapToDouble(r -> Math.pow(r - periodicRiskFreeRate, 2))
            .sum();

        double downsideDeviation = Math.sqrt(sumSquaredDownside / returns.size());

        if (downsideDeviation == 0.0) {
            return 0.0;
        }

        // Annualize
        double excessReturn = avgReturn - periodicRiskFreeRate;
        double annualizedSortino = (excessReturn * periodsPerYear) / (downsideDeviation * Math.sqrt(periodsPerYear));

        return annualizedSortino;
    }

    /**
     * Calculate Calmar Ratio (CAGR / MaxDrawdown).
     *
     * @param totalReturn total return over the period (e.g., 0.25 for 25%)
     * @param years number of years in the backtest period
     * @param maxDrawdown maximum drawdown as decimal (e.g., 0.15 for 15%)
     * @return Calmar Ratio, or 0.0 if maxDrawdown is zero or negative
     */
    public static double calculateCalmarRatio(
        double totalReturn,
        double years,
        double maxDrawdown) {

        if (maxDrawdown <= 0.0 || years <= 0.0) {
            return 0.0;
        }

        // CAGR = (1 + totalReturn)^(1/years) - 1
        double cagr = Math.pow(1.0 + totalReturn, 1.0 / years) - 1.0;

        return cagr / maxDrawdown;
    }

    /**
     * Calculate Recovery Factor (Gross Profit / MaxDrawdown).
     *
     * @param grossProfit sum of all winning trades
     * @param grossLoss sum of absolute values of all losing trades
     * @param maxDrawdown maximum drawdown as decimal
     * @return Recovery Factor, or 0.0 if maxDrawdown is zero
     */
    public static double calculateRecoveryFactor(
        double grossProfit,
        double grossLoss,
        double maxDrawdown) {

        if (maxDrawdown <= 0.0) {
            return 0.0;
        }

        double netProfit = grossProfit - grossLoss;
        return netProfit / maxDrawdown;
    }

    /**
     * Calculate R-Multiple for a trade.
     *
     * R-Multiple = PnL / Initial Risk
     * where Initial Risk = entryValue * riskPercent (e.g., 2% of position)
     *
     * @param pnl trade PnL
     * @param entryPrice entry price
     * @param quantity position quantity
     * @param riskPercent risk percentage per trade (e.g., 0.02 for 2%)
     * @return R-Multiple, or 0.0 if risk is zero
     */
    public static double calculateRMultiple(
        double pnl,
        double entryPrice,
        int quantity,
        double riskPercent) {

        if (riskPercent <= 0.0 || entryPrice <= 0.0 || quantity <= 0) {
            return 0.0;
        }

        double initialRisk = entryPrice * quantity * riskPercent;
        return pnl / initialRisk;
    }

    /**
     * Calculate R-Multiple distribution statistics.
     *
     * @param trades list of trade results
     * @param riskPercent risk percentage per trade
     * @return R-Multiple statistics (avg, median, stdDev)
     */
    public static class RMultipleStats {
        public final double avg;
        public final double median;
        public final double stdDev;
        public final int count;

        public RMultipleStats(double avg, double median, double stdDev, int count) {
            this.avg = avg;
            this.median = median;
            this.stdDev = stdDev;
            this.count = count;
        }
    }

    public static RMultipleStats calculateRMultipleStats(
        List<BacktestRunner.TradeResult> trades,
        double riskPercent) {

        if (trades == null || trades.isEmpty()) {
            return new RMultipleStats(0.0, 0.0, 0.0, 0);
        }

        List<Double> rMultiples = new ArrayList<>();
        for (BacktestRunner.TradeResult trade : trades) {
            double r = calculateRMultiple(trade.pnl, trade.entry, trade.qty, riskPercent);
            rMultiples.add(r);
        }

        // Average
        double avg = rMultiples.stream()
            .mapToDouble(Double::doubleValue)
            .average()
            .orElse(0.0);

        // Median
        rMultiples.sort(Double::compareTo);
        double median;
        int mid = rMultiples.size() / 2;
        if (rMultiples.size() % 2 == 0) {
            median = (rMultiples.get(mid - 1) + rMultiples.get(mid)) / 2.0;
        } else {
            median = rMultiples.get(mid);
        }

        // Standard deviation
        double variance = rMultiples.stream()
            .mapToDouble(r -> Math.pow(r - avg, 2))
            .average()
            .orElse(0.0);
        double stdDev = Math.sqrt(variance);

        return new RMultipleStats(avg, median, stdDev, rMultiples.size());
    }

    /**
     * Calculate CAGR (Compound Annual Growth Rate).
     *
     * @param beginningValue starting capital
     * @param endingValue ending capital
     * @param years number of years
     * @return CAGR as decimal, or 0.0 if invalid inputs
     */
    public static double calculateCAGR(
        double beginningValue,
        double endingValue,
        double years) {

        if (beginningValue <= 0.0 || endingValue <= 0.0 || years <= 0.0) {
            return 0.0;
        }

        return Math.pow(endingValue / beginningValue, 1.0 / years) - 1.0;
    }

    /**
     * Calculate profit factor (Gross Profit / Gross Loss).
     *
     * @param grossProfit sum of all winning trades
     * @param grossLoss sum of absolute values of all losing trades
     * @return profit factor, or 0.0 if grossLoss is zero
     */
    public static double calculateProfitFactor(
        double grossProfit,
        double grossLoss) {

        if (grossLoss <= 0.0) {
            return grossProfit > 0.0 ? Double.POSITIVE_INFINITY : 0.0;
        }

        return grossProfit / grossLoss;
    }

    /**
     * Calculate expectancy per trade.
     *
     * @param winRate win rate as decimal (e.g., 0.55 for 55%)
     * @param avgWin average winning trade PnL
     * @param avgLoss average losing trade PnL (absolute value)
     * @return expectancy per trade
     */
    public static double calculateExpectancy(
        double winRate,
        double avgWin,
        double avgLoss) {

        return (winRate * avgWin) - ((1.0 - winRate) * avgLoss);
    }
}
