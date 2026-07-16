package com.github.shk0da.goldendragon.test;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * Backtest quality evaluator implementing the 5-dimension scoring framework.
 *
 * <p>Based on the "Backtest Expert" methodology that evaluates backtest results across five
 * dimensions: Sample Size, Expectancy, Risk Management, Robustness, and Execution Realism. Each
 * dimension is scored 0–20 points, yielding a total score of 0–100.
 *
 * <h3>Dimensions</h3>
 *
 * <ol>
 *   <li><b>Sample Size</b> (0–20): Statistical significance based on number of trades
 *   <li><b>Expectancy</b> (0–20): Mathematical expectation per trade (avg_win vs avg_loss)
 *   <li><b>Risk Management</b> (0–20): Max drawdown, risk control, position sizing
 *   <li><b>Robustness</b> (0–20): Parameter sensitivity, in-sample vs out-of-sample consistency
 *   <li><b>Execution Realism</b> (0–20): Slippage, spread, commissions, liquidity
 * </ol>
 *
 * <h3>Verdicts</h3>
 *
 * <ul>
 *   <li><b>DEPLOY</b> (≥80): Strategy is ready for live trading with proper risk controls
 *   <li><b>REFINE</b> (50–79): Strategy shows promise but needs improvements
 *   <li><b>ABANDON</b> (&lt;50): Strategy has fundamental issues, consider redesign
 * </ul>
 *
 * @see BacktestRunner
 */
public class BacktestExpertEvaluator {

    /** Evaluation result for a single dimension. */
    public static class DimensionResult {
        private final String name;
        private final double score; // 0–20
        private final List<String> findings;
        private final List<String> recommendations;

        DimensionResult(String name, double score) {
            this.name = name;
            this.score = Math.max(0.0, Math.min(20.0, score));
            this.findings = new ArrayList<>();
            this.recommendations = new ArrayList<>();
        }

        void addFinding(String finding) {
            findings.add(finding);
        }

        void addRecommendation(String recommendation) {
            recommendations.add(recommendation);
        }

        public String getName() {
            return name;
        }

        public double getScore() {
            return score;
        }

        public List<String> getFindings() {
            return Collections.unmodifiableList(findings);
        }

        public List<String> getRecommendations() {
            return Collections.unmodifiableList(recommendations);
        }
    }

    /** Complete evaluation result. */
    public static class EvaluationResult {
        private final DimensionResult sampleSize;
        private final DimensionResult expectancy;
        private final DimensionResult riskManagement;
        private final DimensionResult robustness;
        private final DimensionResult executionRealism;
        private final double totalScore;
        private final String verdict;
        private final List<String> redFlags;
        private final List<String> summaryRecommendations;

        EvaluationResult(
                DimensionResult sampleSize,
                DimensionResult expectancy,
                DimensionResult riskManagement,
                DimensionResult robustness,
                DimensionResult executionRealism,
                List<String> redFlags,
                List<String> summaryRecommendations) {
            this.sampleSize = sampleSize;
            this.expectancy = expectancy;
            this.riskManagement = riskManagement;
            this.robustness = robustness;
            this.executionRealism = executionRealism;
            this.redFlags = redFlags;
            this.summaryRecommendations = summaryRecommendations;

            this.totalScore =
                    sampleSize.getScore()
                            + expectancy.getScore()
                            + riskManagement.getScore()
                            + robustness.getScore()
                            + executionRealism.getScore();

            if (this.totalScore >= 80.0) {
                this.verdict = "DEPLOY";
            } else if (this.totalScore >= 50.0) {
                this.verdict = "REFINE";
            } else {
                this.verdict = "ABANDON";
            }
        }

        public DimensionResult getSampleSize() {
            return sampleSize;
        }

        public DimensionResult getExpectancy() {
            return expectancy;
        }

        public DimensionResult getRiskManagement() {
            return riskManagement;
        }

        public DimensionResult getRobustness() {
            return robustness;
        }

        public DimensionResult getExecutionRealism() {
            return executionRealism;
        }

        public double getTotalScore() {
            return totalScore;
        }

        public String getVerdict() {
            return verdict;
        }

        public List<String> getRedFlags() {
            return Collections.unmodifiableList(redFlags);
        }

        public List<String> getSummaryRecommendations() {
            return Collections.unmodifiableList(summaryRecommendations);
        }
    }

    /**
     * Evaluate backtest quality using the 5-dimension framework.
     *
     * @param allTrades all trades across all tickers and periods
     * @param portfolioResults per-period portfolio results (for robustness check)
     * @param initialBalance starting balance
     * @param commissionRate commission rate per trade (e.g. 0.0005 for 0.05%)
     * @return evaluation result with scores, red flags, and verdict
     */
    public static EvaluationResult evaluate(
            List<BacktestRunner.TradeResult> allTrades,
            Map<String, BacktestRunner.PortfolioPeriodResult> portfolioResults,
            double initialBalance,
            double commissionRate) {

        DimensionResult sampleSize = evaluateSampleSize(allTrades);
        DimensionResult expectancy = evaluateExpectancy(allTrades, initialBalance);
        DimensionResult riskManagement =
                evaluateRiskManagement(allTrades, portfolioResults, initialBalance);
        DimensionResult robustness = evaluateRobustness(portfolioResults);
        DimensionResult executionRealism = evaluateExecutionRealism(allTrades, commissionRate);

        List<String> redFlags = detectRedFlags(allTrades, portfolioResults, initialBalance);
        List<String> summaryRecommendations =
                buildSummaryRecommendations(
                        sampleSize,
                        expectancy,
                        riskManagement,
                        robustness,
                        executionRealism,
                        redFlags);

        return new EvaluationResult(
                sampleSize,
                expectancy,
                riskManagement,
                robustness,
                executionRealism,
                redFlags,
                summaryRecommendations);
    }

    // =========================================================================
    // Dimension 1: Sample Size (0–20)
    // =========================================================================

    private static DimensionResult evaluateSampleSize(List<BacktestRunner.TradeResult> allTrades) {
        DimensionResult result = new DimensionResult("Sample Size", 0.0);
        int totalTrades = allTrades.size();
        result.addFinding("Total trades: " + totalTrades);

        if (totalTrades == 0) {
            result.addFinding("No trades executed — cannot evaluate statistical significance");
            result.addRecommendation("Extend backtest period or adjust entry conditions");
            return new DimensionResult("Sample Size", 0.0);
        }

        // Count winners/losers
        long winners = allTrades.stream().filter(t -> t.pnl > 0).count();
        long losers = allTrades.stream().filter(t -> t.pnl < 0).count();
        long breakeven = totalTrades - winners - losers;

        result.addFinding("Winners: " + winners + " (" + pct(winners, totalTrades) + ")");
        result.addFinding("Losers: " + losers + " (" + pct(losers, totalTrades) + ")");
        result.addFinding("Breakeven: " + breakeven);

        // Minimum for statistical significance: ~100 trades
        // Excellent: 500+ trades
        double score;
        if (totalTrades >= 500) {
            score = 20.0;
            result.addFinding("Excellent sample size (>500 trades)");
        } else if (totalTrades >= 300) {
            score = 16.0;
            result.addFinding("Good sample size (300–500 trades)");
        } else if (totalTrades >= 100) {
            score = 12.0;
            result.addFinding("Adequate sample size (100–300 trades)");
        } else if (totalTrades >= 30) {
            score = 6.0;
            result.addFinding("Limited sample size (30–100 trades) — interpret with caution");
            result.addRecommendation("Extend backtest period to increase trade count");
        } else {
            score = 2.0;
            result.addFinding("Very small sample size (<30 trades) — statistically unreliable");
            result.addRecommendation("Increase sample size to at least 100 trades");
        }

        // Check per-ticker distribution
        Map<String, Long> tradesByTicker = countTradesByTicker(allTrades);
        long maxTradesInTicker =
                tradesByTicker.values().stream().mapToLong(Long::longValue).max().orElse(0);
        long minTradesInTicker =
                tradesByTicker.values().stream().mapToLong(Long::longValue).min().orElse(0);

        if (tradesByTicker.size() > 1) {
            result.addFinding("Trades across " + tradesByTicker.size() + " tickers");
            result.addFinding(
                    "Min/Max trades per ticker: " + minTradesInTicker + "/" + maxTradesInTicker);

            if (maxTradesInTicker > 0 && minTradesInTicker > 0) {
                double ratio = (double) maxTradesInTicker / minTradesInTicker;
                if (ratio > 5.0) {
                    score *= 0.9;
                    result.addFinding(
                            "Uneven trade distribution across tickers (ratio="
                                    + String.format("%.1f", ratio)
                                    + ")");
                    result.addRecommendation("Check if some tickers are under-represented");
                }
            }
        }

        return new DimensionResult("Sample Size", score);
    }

    // =========================================================================
    // Dimension 2: Expectancy (0–20)
    // =========================================================================

    private static DimensionResult evaluateExpectancy(
            List<BacktestRunner.TradeResult> allTrades, double initialBalance) {
        DimensionResult result = new DimensionResult("Expectancy", 0.0);

        if (allTrades.isEmpty()) {
            result.addFinding("No trades — cannot evaluate expectancy");
            return new DimensionResult("Expectancy", 0.0);
        }

        // Calculate avg win and avg loss
        List<BacktestRunner.TradeResult> winningTrades = new ArrayList<>();
        List<BacktestRunner.TradeResult> losingTrades = new ArrayList<>();

        for (BacktestRunner.TradeResult trade : allTrades) {
            if (trade.pnl > 0) {
                winningTrades.add(trade);
            } else if (trade.pnl < 0) {
                losingTrades.add(trade);
            }
        }

        double avgWin =
                winningTrades.isEmpty()
                        ? 0.0
                        : winningTrades.stream().mapToDouble(t -> t.pnl).average().orElse(0.0);
        double avgLoss =
                losingTrades.isEmpty()
                        ? 0.0
                        : losingTrades.stream()
                                .mapToDouble(t -> Math.abs(t.pnl))
                                .average()
                                .orElse(0.0);

        double winRate = (double) winningTrades.size() / allTrades.size();
        double expectancy = (winRate * avgWin) - ((1.0 - winRate) * avgLoss);

        result.addFinding("Avg win: " + formatMoney(avgWin));
        result.addFinding("Avg loss: " + formatMoney(avgLoss));
        result.addFinding("Win rate: " + String.format("%.1f%%", winRate * 100));
        result.addFinding("Expectancy per trade: " + formatMoney(expectancy));

        // Win/Loss ratio
        double winLossRatio = avgLoss > 0 ? avgWin / avgLoss : 0.0;
        result.addFinding("Win/Loss ratio: " + String.format("%.2f", winLossRatio));

        // Expectancy as % of initial balance
        double expectancyPct = initialBalance > 0 ? (expectancy / initialBalance) * 100.0 : 0.0;
        result.addFinding(
                "Expectancy per trade (% of balance): " + String.format("%.4f%%", expectancyPct));

        // Score based on expectancy quality
        double score;

        if (expectancy <= 0) {
            score = 0.0;
            result.addFinding("Negative expectancy — strategy loses money on average");
            result.addRecommendation(
                    "Fundamental issue: strategy has negative mathematical expectation");
        } else if (expectancyPct < 0.001) {
            score = 4.0;
            result.addFinding("Very low positive expectancy — may be within noise");
            result.addRecommendation("Increase win rate or improve avg win/loss ratio");
        } else if (expectancyPct < 0.01) {
            score = 8.0;
            result.addFinding("Low positive expectancy");
            result.addRecommendation("Consider tightening stops or adding profit targets");
        } else if (expectancyPct < 0.05) {
            score = 14.0;
            result.addFinding("Moderate positive expectancy");
        } else if (expectancyPct < 0.1) {
            score = 17.0;
            result.addFinding("Good positive expectancy");
        } else {
            score = 20.0;
            result.addFinding("Excellent positive expectancy");
        }

        // Bonus for win/loss ratio
        if (winLossRatio >= 2.0) {
            score = Math.min(20.0, score + 2.0);
            result.addFinding("Strong win/loss ratio (≥2.0) — good risk/reward");
        } else if (winLossRatio < 0.5) {
            score = Math.max(0.0, score - 3.0);
            result.addFinding("Weak win/loss ratio (<0.5) — risk/reward imbalance");
            result.addRecommendation("Improve profit targets or tighten stop losses");
        }

        return new DimensionResult("Expectancy", score);
    }

    // =========================================================================
    // Dimension 3: Risk Management (0–20)
    // =========================================================================

    private static DimensionResult evaluateRiskManagement(
            List<BacktestRunner.TradeResult> allTrades,
            Map<String, BacktestRunner.PortfolioPeriodResult> portfolioResults,
            double initialBalance) {
        DimensionResult result = new DimensionResult("Risk Management", 0.0);

        // Find max drawdown across all periods
        double maxDD = 0.0;
        for (BacktestRunner.PortfolioPeriodResult pr : portfolioResults.values()) {
            if (pr.dd > maxDD) {
                maxDD = pr.dd;
            }
        }

        double maxDDPct = maxDD * 100.0;
        result.addFinding("Maximum drawdown: " + String.format("%.2f%%", maxDDPct));

        // Calculate max consecutive losses
        int maxConsecutiveLosses = 0;
        int currentStreak = 0;
        for (BacktestRunner.TradeResult trade : allTrades) {
            if (trade.pnl < 0) {
                currentStreak++;
                if (currentStreak > maxConsecutiveLosses) {
                    maxConsecutiveLosses = currentStreak;
                }
            } else {
                currentStreak = 0;
            }
        }
        result.addFinding("Max consecutive losses: " + maxConsecutiveLosses);

        // Calculate max single loss as % of balance
        double maxSingleLoss = 0.0;
        for (BacktestRunner.TradeResult trade : allTrades) {
            double lossPct = initialBalance > 0 ? Math.abs(trade.pnl) / initialBalance : 0.0;
            if (lossPct > maxSingleLoss) {
                maxSingleLoss = lossPct;
            }
        }
        result.addFinding("Max single loss: " + String.format("%.2f%%", maxSingleLoss * 100));

        // Scoring
        double score;

        // MaxDD scoring (0–10 points)
        if (maxDDPct <= 5.0) {
            score = 10.0;
            result.addFinding("Excellent drawdown control (<5%)");
        } else if (maxDDPct <= 10.0) {
            score = 8.0;
            result.addFinding("Good drawdown control (5–10%)");
        } else if (maxDDPct <= 20.0) {
            score = 5.0;
            result.addFinding("Moderate drawdown (10–20%)");
            result.addRecommendation("Consider tightening position sizes or stop losses");
        } else if (maxDDPct <= 30.0) {
            score = 2.0;
            result.addFinding("High drawdown (20–30%)");
            result.addRecommendation("Reduce position sizes, add portfolio-level risk controls");
        } else {
            score = 0.0;
            result.addFinding("Severe drawdown (>30%)");
            result.addRecommendation("Critical: Implement strict risk limits before live trading");
        }

        // Consecutive losses penalty
        if (maxConsecutiveLosses > 10) {
            score = Math.max(0.0, score - 3.0);
            result.addFinding(
                    "High consecutive losses ("
                            + maxConsecutiveLosses
                            + ") — potential regime issue");
            result.addRecommendation("Check if strategy adapts to changing market conditions");
        } else if (maxConsecutiveLosses > 5) {
            score = Math.max(0.0, score - 1.0);
        }

        // Max single loss check
        if (maxSingleLoss > 0.05) {
            score = Math.max(0.0, score - 4.0);
            result.addFinding("Single trade risk exceeds 5% of balance");
            result.addRecommendation("Implement position-level stop loss (max 1–2% per trade)");
        } else if (maxSingleLoss > 0.02) {
            score = Math.max(0.0, score - 1.0);
            result.addFinding("Single trade risk is 2–5% of balance");
        }

        // Risk/reward bonus
        if (maxDDPct > 0) {
            double totalPnL = portfolioResults.values().stream().mapToDouble(pr -> pr.pnl).sum();
            double returnToDrawdown =
                    maxDDPct > 0 ? (totalPnL / initialBalance * 100.0) / maxDDPct : 0.0;
            result.addFinding("Return/Drawdown ratio: " + String.format("%.2f", returnToDrawdown));

            if (returnToDrawdown > 2.0) {
                score = Math.min(20.0, score + 2.0);
                result.addFinding("Excellent return/drawdown ratio (>2.0)");
            }
        }

        return new DimensionResult("Risk Management", score);
    }

    // =========================================================================
    // Dimension 4: Robustness (0–20)
    // =========================================================================

    private static DimensionResult evaluateRobustness(
            Map<String, BacktestRunner.PortfolioPeriodResult> portfolioResults) {
        DimensionResult result = new DimensionResult("Robustness", 0.0);

        if (portfolioResults.isEmpty()) {
            result.addFinding("No period results — cannot evaluate robustness");
            return new DimensionResult("Robustness", 0.0);
        }

        // Analyze consistency across periods
        List<Double> periodReturns = new ArrayList<>();
        List<Double> periodDrawdowns = new ArrayList<>();
        int profitablePeriods = 0;
        int totalPeriods = portfolioResults.size();

        for (BacktestRunner.PortfolioPeriodResult pr : portfolioResults.values()) {
            if (pr.equityCurve != null && !pr.equityCurve.isEmpty()) {
                double startEquity = pr.equityCurve.get(0).equity;
                if (startEquity > 0) {
                    periodReturns.add(pr.pnl / startEquity);
                }
            }
            periodDrawdowns.add(pr.dd);
            if (pr.pnl > 0) {
                profitablePeriods++;
            }
        }

        result.addFinding("Total periods: " + totalPeriods);
        result.addFinding(
                "Profitable periods: "
                        + profitablePeriods
                        + " ("
                        + pct(profitablePeriods, totalPeriods)
                        + ")");

        // Consistency of returns
        if (periodReturns.size() > 1) {
            double avgReturn =
                    periodReturns.stream().mapToDouble(Double::doubleValue).average().orElse(0.0);
            double variance =
                    periodReturns.stream()
                            .mapToDouble(r -> Math.pow(r - avgReturn, 2))
                            .average()
                            .orElse(0.0);
            double stdDev = Math.sqrt(variance);
            result.addFinding("Average period return: " + String.format("%.4f%%", avgReturn * 100));
            result.addFinding("Return std dev: " + String.format("%.4f%%", stdDev * 100));

            // Coefficient of variation
            if (avgReturn != 0) {
                double cv = Math.abs(stdDev / avgReturn);
                result.addFinding("Coefficient of variation: " + String.format("%.2f", cv));
            }
        }

        // Drawdown consistency
        double avgDD =
                periodDrawdowns.stream().mapToDouble(Double::doubleValue).average().orElse(0.0);
        double maxDD = periodDrawdowns.stream().mapToDouble(Double::doubleValue).max().orElse(0.0);
        result.addFinding("Average drawdown: " + String.format("%.2f%%", avgDD * 100));
        result.addFinding("Max drawdown: " + String.format("%.2f%%", maxDD * 100));

        // Scoring
        double score;

        // Profitable periods ratio (0–8 points)
        double profitableRatio = (double) profitablePeriods / totalPeriods;
        if (profitableRatio >= 0.8) {
            score = 8.0;
            result.addFinding("Highly consistent (>80% profitable periods)");
        } else if (profitableRatio >= 0.6) {
            score = 6.0;
            result.addFinding("Good consistency (60–80% profitable periods)");
        } else if (profitableRatio >= 0.5) {
            score = 4.0;
            result.addFinding("Moderate consistency (50–60% profitable periods)");
        } else {
            score = 1.0;
            result.addFinding("Low consistency (<50% profitable periods)");
            result.addRecommendation("Strategy may be overfitting or sensitive to market regime");
        }

        // Drawdown stability (0–6 points)
        if (maxDD > 0 && avgDD > 0) {
            double ddRatio = maxDD / avgDD;
            if (ddRatio < 1.5) {
                score += 6.0;
                result.addFinding("Very stable drawdowns across periods");
            } else if (ddRatio < 2.0) {
                score += 4.0;
                result.addFinding("Moderately stable drawdowns");
            } else if (ddRatio < 3.0) {
                score += 2.0;
                result.addFinding("Unstable drawdowns — some periods much worse than others");
                result.addRecommendation("Investigate periods with extreme drawdowns");
            } else {
                score += 0.0;
                result.addFinding("Highly unstable drawdowns");
                result.addRecommendation("Strategy performance varies dramatically across periods");
            }
        }

        // Return stability (0–6 points)
        if (periodReturns.size() > 1) {
            double avgReturn =
                    periodReturns.stream().mapToDouble(Double::doubleValue).average().orElse(0.0);
            double variance =
                    periodReturns.stream()
                            .mapToDouble(r -> Math.pow(r - avgReturn, 2))
                            .average()
                            .orElse(0.0);
            double stdDev = Math.sqrt(variance);

            if (avgReturn > 0 && stdDev > 0) {
                double sharpe = avgReturn / stdDev;
                if (sharpe > 1.5) {
                    score += 6.0;
                    result.addFinding(
                            "Excellent return stability (Sharpe="
                                    + String.format("%.2f", sharpe)
                                    + ")");
                } else if (sharpe > 1.0) {
                    score += 4.0;
                    result.addFinding(
                            "Good return stability (Sharpe=" + String.format("%.2f", sharpe) + ")");
                } else if (sharpe > 0.5) {
                    score += 2.0;
                    result.addFinding(
                            "Moderate return stability (Sharpe="
                                    + String.format("%.2f", sharpe)
                                    + ")");
                } else {
                    score += 0.0;
                    result.addFinding(
                            "Poor return stability (Sharpe=" + String.format("%.2f", sharpe) + ")");
                    result.addRecommendation(
                            "Returns are noisy — consider smoothing or longer holding periods");
                }
            }
        }

        return new DimensionResult("Robustness", Math.min(20.0, score));
    }

    // =========================================================================
    // Dimension 5: Execution Realism (0–20)
    // =========================================================================

    private static DimensionResult evaluateExecutionRealism(
            List<BacktestRunner.TradeResult> allTrades, double commissionRate) {
        DimensionResult result = new DimensionResult("Execution Realism", 0.0);

        if (allTrades.isEmpty()) {
            result.addFinding("No trades — cannot evaluate execution realism");
            return new DimensionResult("Execution Realism", 0.0);
        }

        // Analyze trade timing and execution assumptions
        double totalCommission = 0.0;
        double avgTradeValue = 0.0;
        int tradesWithZeroPnl = 0;

        for (BacktestRunner.TradeResult trade : allTrades) {
            double tradeValue = trade.entry * trade.qty;
            double commission = tradeValue * commissionRate * 2.0; // round-trip
            totalCommission += commission;
            avgTradeValue += tradeValue;

            if (Math.abs(trade.pnl) < 0.01) {
                tradesWithZeroPnl++;
            }
        }

        avgTradeValue /= allTrades.size();

        result.addFinding("Total commission paid: " + formatMoney(totalCommission));
        result.addFinding("Average trade value: " + formatMoney(avgTradeValue));
        result.addFinding("Trades with zero PnL: " + tradesWithZeroPnl);

        // Analyze trade distribution by reason
        Map<String, Long> tradesByReason = countTradesByReason(allTrades);
        result.addFinding("Trade exit reasons:");
        for (Map.Entry<String, Long> entry : tradesByReason.entrySet()) {
            result.addFinding(
                    "  "
                            + entry.getKey()
                            + ": "
                            + entry.getValue()
                            + " ("
                            + pct(entry.getValue(), allTrades.size())
                            + ")");
        }

        // Scoring
        double score;

        // Commission impact (0–5 points)
        double totalPnL = allTrades.stream().mapToDouble(t -> t.pnl).sum();
        double commissionImpact = totalPnL != 0 ? Math.abs(totalCommission / totalPnL) : 0.0;
        result.addFinding(
                "Commission impact on PnL: " + String.format("%.2f%%", commissionImpact * 100));

        if (commissionImpact < 0.1) {
            score = 5.0;
            result.addFinding("Commission impact is minimal (<10% of PnL)");
        } else if (commissionImpact < 0.3) {
            score = 3.0;
            result.addFinding("Commission impact is moderate (10–30% of PnL)");
        } else if (commissionImpact < 0.5) {
            score = 1.0;
            result.addFinding("Commission impact is high (30–50% of PnL)");
            result.addRecommendation("Consider fewer, higher-conviction trades");
        } else {
            score = 0.0;
            result.addFinding("Commission impact is excessive (>50% of PnL)");
            result.addRecommendation("Strategy is not profitable after commissions");
        }

        // Trade frequency realism (0–5 points)
        // MOEX has limited liquidity for some instruments
        double avgDailyTrades = estimateDailyTradeFrequency(allTrades);
        result.addFinding("Estimated avg daily trades: " + String.format("%.1f", avgDailyTrades));

        if (avgDailyTrades <= 5.0) {
            score += 5.0;
            result.addFinding("Realistic trade frequency for MOEX");
        } else if (avgDailyTrades <= 15.0) {
            score += 3.0;
            result.addFinding("Moderate trade frequency — may face liquidity constraints");
        } else if (avgDailyTrades <= 30.0) {
            score += 1.0;
            result.addFinding("High trade frequency — potential execution issues");
            result.addRecommendation("Check if all trades can be filled at expected prices");
        } else {
            score += 0.0;
            result.addFinding("Excessive trade frequency — unrealistic for MOEX");
            result.addRecommendation("Reduce trading frequency or add liquidity filters");
        }

        // Zero-PnL trade check (0–5 points)
        double zeroPnlRatio = (double) tradesWithZeroPnl / allTrades.size();
        result.addFinding("Zero PnL trades: " + String.format("%.1f%%", zeroPnlRatio * 100));

        if (zeroPnlRatio < 0.05) {
            score += 5.0;
            result.addFinding("Few zero-PnL trades — realistic execution");
        } else if (zeroPnlRatio < 0.15) {
            score += 3.0;
            result.addFinding("Some zero-PnL trades — possible look-ahead or execution issues");
        } else {
            score += 0.0;
            result.addFinding("Many zero-PnL trades — suspicious execution");
            result.addRecommendation("Review trade logic for look-ahead bias or unrealistic fills");
        }

        // Slippage estimation (0–5 points)
        // Note: Current backtest doesn't model slippage explicitly
        // Conservative assumption: 0.1% slippage per trade
        double estimatedSlippage = avgTradeValue * 0.001 * allTrades.size();
        result.addFinding("Estimated slippage (0.1%): " + formatMoney(estimatedSlippage));

        if (estimatedSlippage < Math.abs(totalPnL) * 0.2) {
            score += 5.0;
            result.addFinding("Estimated slippage impact is manageable");
        } else if (estimatedSlippage < Math.abs(totalPnL) * 0.5) {
            score += 3.0;
            result.addFinding("Estimated slippage may impact profitability");
            result.addRecommendation("Add slippage model to backtest for more accurate results");
        } else {
            score += 0.0;
            result.addFinding("Estimated slippage significantly impacts results");
            result.addRecommendation("Strategy may not be executable at expected prices");
        }

        return new DimensionResult("Execution Realism", Math.min(20.0, score));
    }

    // =========================================================================
    // Red Flag Detection
    // =========================================================================

    private static List<String> detectRedFlags(
            List<BacktestRunner.TradeResult> allTrades,
            Map<String, BacktestRunner.PortfolioPeriodResult> portfolioResults,
            double initialBalance) {
        List<String> redFlags = new ArrayList<>();

        if (allTrades.isEmpty()) {
            redFlags.add("No trades executed");
            return redFlags;
        }

        // 1. Curved equity curve (too perfect)
        // Check if win rate is suspiciously high
        long winners = allTrades.stream().filter(t -> t.pnl > 0).count();
        double winRate = (double) winners / allTrades.size();
        if (winRate > 0.85) {
            redFlags.add(
                    "Suspiciously high win rate ("
                            + String.format("%.1f%%", winRate * 100)
                            + ") — possible overfitting");
        }

        // 2. Negative expectancy with positive total PnL (survivorship bias)
        double totalPnL = allTrades.stream().mapToDouble(t -> t.pnl).sum();
        List<BacktestRunner.TradeResult> losingTrades = new ArrayList<>();
        for (BacktestRunner.TradeResult t : allTrades) {
            if (t.pnl < 0) losingTrades.add(t);
        }
        double avgLoss =
                losingTrades.isEmpty()
                        ? 0.0
                        : losingTrades.stream()
                                .mapToDouble(t -> Math.abs(t.pnl))
                                .average()
                                .orElse(0.0);
        List<BacktestRunner.TradeResult> winningTrades = new ArrayList<>();
        for (BacktestRunner.TradeResult t : allTrades) {
            if (t.pnl > 0) winningTrades.add(t);
        }
        double avgWin =
                winningTrades.isEmpty()
                        ? 0.0
                        : winningTrades.stream().mapToDouble(t -> t.pnl).average().orElse(0.0);
        double expectancy = (winRate * avgWin) - ((1.0 - winRate) * avgLoss);

        if (totalPnL > 0 && expectancy < 0) {
            redFlags.add(
                    "Positive total PnL but negative expectancy — may be lucky with few big wins");
        }

        // 3. Concentrated returns (few trades account for most PnL)
        if (allTrades.size() > 10) {
            List<BacktestRunner.TradeResult> sortedByPnl = new ArrayList<>(allTrades);
            sortedByPnl.sort((a, b) -> Double.compare(b.pnl, a.pnl));

            double top10PnL = 0.0;
            int top10Count = Math.min(10, sortedByPnl.size());
            for (int i = 0; i < top10Count; i++) {
                top10PnL += sortedByPnl.get(i).pnl;
            }

            if (totalPnL > 0 && top10PnL / totalPnL > 0.8) {
                redFlags.add("Top 10 trades account for >80% of total PnL — concentrated returns");
            }
        }

        // 4. Excessive drawdown
        double maxDD = 0.0;
        for (BacktestRunner.PortfolioPeriodResult pr : portfolioResults.values()) {
            if (pr.dd > maxDD) maxDD = pr.dd;
        }
        if (maxDD > 0.3) {
            redFlags.add(
                    "Maximum drawdown exceeds 30% (" + String.format("%.1f%%", maxDD * 100) + ")");
        }

        // 5. Trading in same direction always (no shorts in bear market)
        long longTrades = allTrades.stream().filter(t -> "BUY".equals(t.dir)).count();
        long shortTrades = allTrades.stream().filter(t -> "SELL".equals(t.dir)).count();
        if (allTrades.size() > 20) {
            double longRatio = (double) longTrades / allTrades.size();
            if (longRatio > 0.95) {
                redFlags.add(
                        "Almost all trades are long ("
                                + String.format("%.1f%%", longRatio * 100)
                                + ") — no short exposure");
            } else if (longRatio < 0.05) {
                redFlags.add(
                        "Almost all trades are short ("
                                + String.format("%.1f%%", (1 - longRatio) * 100)
                                + ") — unusual bias");
            }
        }

        // 6. Many consecutive losses (regime sensitivity)
        int maxConsecutiveLosses = 0;
        int currentStreak = 0;
        for (BacktestRunner.TradeResult trade : allTrades) {
            if (trade.pnl < 0) {
                currentStreak++;
                if (currentStreak > maxConsecutiveLosses) {
                    maxConsecutiveLosses = currentStreak;
                }
            } else {
                currentStreak = 0;
            }
        }
        if (maxConsecutiveLosses > 15) {
            redFlags.add(
                    "Max consecutive losses: "
                            + maxConsecutiveLosses
                            + " — strategy struggles in certain regimes");
        }

        // 7. Inconsistent period performance
        int profitablePeriods = 0;
        int totalPeriods = portfolioResults.size();
        for (BacktestRunner.PortfolioPeriodResult pr : portfolioResults.values()) {
            if (pr.pnl > 0) profitablePeriods++;
        }
        if (totalPeriods > 3) {
            double profitableRatio = (double) profitablePeriods / totalPeriods;
            if (profitableRatio < 0.4) {
                redFlags.add(
                        "Only "
                                + profitablePeriods
                                + "/"
                                + totalPeriods
                                + " periods profitable — inconsistent performance");
            }
        }

        // 8. Zero-PnL trades (possible look-ahead)
        long zeroPnlTrades = allTrades.stream().filter(t -> Math.abs(t.pnl) < 0.01).count();
        if (zeroPnlTrades > allTrades.size() * 0.1) {
            redFlags.add(
                    "High number of zero-PnL trades ("
                            + zeroPnlTrades
                            + "/"
                            + allTrades.size()
                            + ") — check for look-ahead bias");
        }

        // 9. Single ticker dominance
        Map<String, Long> tradesByTicker = countTradesByTicker(allTrades);
        if (tradesByTicker.size() > 1) {
            long maxTickerTrades =
                    tradesByTicker.values().stream().mapToLong(Long::longValue).max().orElse(0);
            double tickerDominance = (double) maxTickerTrades / allTrades.size();
            if (tickerDominance > 0.7) {
                String dominantTicker =
                        tradesByTicker.entrySet().stream()
                                .max(Map.Entry.comparingByValue())
                                .map(Map.Entry::getKey)
                                .orElse("unknown");
                redFlags.add(
                        "Single ticker dominates: "
                                + dominantTicker
                                + " ("
                                + String.format("%.1f%%", tickerDominance * 100)
                                + " of trades)");
            }
        }

        // 10. Very small position sizes (may not be executable)
        double avgQty = allTrades.stream().mapToDouble(t -> t.qty).average().orElse(0.0);
        if (avgQty < 1.0 && avgQty > 0) {
            redFlags.add("Average position size < 1 lot — may not be executable on MOEX");
        }

        return redFlags;
    }

    // =========================================================================
    // Summary Recommendations
    // =========================================================================

    private static List<String> buildSummaryRecommendations(
            DimensionResult sampleSize,
            DimensionResult expectancy,
            DimensionResult riskManagement,
            DimensionResult robustness,
            DimensionResult executionRealism,
            List<String> redFlags) {
        List<String> recommendations = new ArrayList<>();

        // Priority recommendations based on weakest dimensions
        List<DimensionResult> dims =
                List.of(sampleSize, expectancy, riskManagement, robustness, executionRealism);
        dims.stream()
                .sorted((a, b) -> Double.compare(a.getScore(), b.getScore()))
                .limit(2)
                .forEach(d -> recommendations.addAll(d.getRecommendations()));

        // Critical red flags
        if (redFlags.size() > 3) {
            recommendations.add(
                    0, "CRITICAL: Multiple red flags detected — thorough review required");
        }

        // General recommendations
        if (sampleSize.getScore() < 10) {
            recommendations.add("Increase sample size for statistical reliability");
        }
        if (expectancy.getScore() < 10) {
            recommendations.add("Improve trade selection or exit logic for better expectancy");
        }
        if (riskManagement.getScore() < 10) {
            recommendations.add("Implement stricter risk controls (position sizing, stop losses)");
        }

        return recommendations;
    }

    // =========================================================================
    // Utility Methods
    // =========================================================================

    private static Map<String, Long> countTradesByTicker(
            List<BacktestRunner.TradeResult> allTrades) {
        Map<String, Long> counts = new java.util.HashMap<>();
        for (BacktestRunner.TradeResult trade : allTrades) {
            counts.merge(trade.ticker, 1L, Long::sum);
        }
        return counts;
    }

    private static Map<String, Long> countTradesByReason(
            List<BacktestRunner.TradeResult> allTrades) {
        Map<String, Long> counts = new java.util.HashMap<>();
        for (BacktestRunner.TradeResult trade : allTrades) {
            counts.merge(trade.reason, 1L, Long::sum);
        }
        return counts;
    }

    private static double estimateDailyTradeFrequency(List<BacktestRunner.TradeResult> allTrades) {
        if (allTrades.isEmpty()) return 0.0;

        // Find date range
        String firstTime = allTrades.get(0).time;
        String lastTime = allTrades.get(allTrades.size() - 1).time;

        try {
            java.time.LocalDateTime first =
                    java.time.LocalDateTime.parse(
                            firstTime,
                            java.time.format.DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm:ss"));
            java.time.LocalDateTime last =
                    java.time.LocalDateTime.parse(
                            lastTime,
                            java.time.format.DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm:ss"));

            long days =
                    java.time.temporal.ChronoUnit.DAYS.between(
                            first.toLocalDate(), last.toLocalDate());
            if (days <= 0) days = 1;

            return (double) allTrades.size() / days;
        } catch (Exception e) {
            return 0.0;
        }
    }

    private static String pct(long part, long total) {
        if (total == 0) return "0%";
        return String.format("%.1f%%", (double) part / total * 100);
    }

    private static String pct(double part, double total) {
        if (total == 0) return "0%";
        return String.format("%.1f%%", part / total * 100);
    }

    private static String formatMoney(double amount) {
        String sign = amount >= 0 ? "+" : "-";
        double abs = Math.abs(amount);
        if (abs >= 1_000_000) {
            return sign + String.format("%.2fM", abs / 1_000_000);
        }
        if (abs >= 1_000) {
            return sign + String.format("%.1fK", abs / 1_000);
        }
        return sign + String.format("%.0f", abs);
    }

    /** Print evaluation report in a formatted way. */
    public static void printEvaluationReport(EvaluationResult result, String strategyName) {
        System.out.println();
        System.out.println("=".repeat(80));
        System.out.println("BACKTEST EXPERT EVALUATION: " + strategyName);
        System.out.println("=".repeat(80));

        // Dimensions
        System.out.println();
        System.out.println("DIMENSION SCORES (0–20 each, total 0–100):");
        System.out.println("-".repeat(60));
        printDimension(result.getSampleSize());
        printDimension(result.getExpectancy());
        printDimension(result.getRiskManagement());
        printDimension(result.getRobustness());
        printDimension(result.getExecutionRealism());

        // Total
        System.out.println();
        System.out.println("-".repeat(60));
        System.out.printf("TOTAL SCORE: %.1f / 100%n", result.getTotalScore());
        System.out.println("VERDICT: " + result.getVerdict());

        // Verdict explanation
        System.out.println();
        switch (result.getVerdict()) {
            case "DEPLOY":
                System.out.println("✅ Strategy shows strong performance across all dimensions.");
                System.out.println("   Ready for live trading with proper risk controls.");
                break;
            case "REFINE":
                System.out.println("⚠️  Strategy shows promise but needs improvements.");
                System.out.println("   Review recommendations below before live trading.");
                break;
            case "ABANDON":
                System.out.println("❌ Strategy has fundamental issues.");
                System.out.println("   Consider redesign or significant changes.");
                break;
        }

        // Red Flags
        if (!result.getRedFlags().isEmpty()) {
            System.out.println();
            System.out.println("RED FLAGS (" + result.getRedFlags().size() + "):");
            System.out.println("-".repeat(60));
            for (int i = 0; i < result.getRedFlags().size(); i++) {
                System.out.println("  " + (i + 1) + ". " + result.getRedFlags().get(i));
            }
        }

        // Recommendations
        if (!result.getSummaryRecommendations().isEmpty()) {
            System.out.println();
            System.out.println("RECOMMENDATIONS:");
            System.out.println("-".repeat(60));
            for (int i = 0; i < result.getSummaryRecommendations().size(); i++) {
                System.out.println(
                        "  " + (i + 1) + ". " + result.getSummaryRecommendations().get(i));
            }
        }

        System.out.println();
        System.out.println("=".repeat(80));
    }

    private static void printDimension(DimensionResult dim) {
        System.out.printf("  %-20s %5.1f / 20%n", dim.getName(), dim.getScore());

        for (String finding : dim.getFindings()) {
            System.out.println("    • " + finding);
        }

        for (String rec : dim.getRecommendations()) {
            System.out.println("    → " + rec);
        }
    }
}
