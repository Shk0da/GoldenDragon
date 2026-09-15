package com.github.shk0da.goldendragon.backtest;

import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

/**
 * Unit tests for {@link MetricsCalculator}.
 *
 * <p>Verifies the financial metric calculations against hand-computed expected values
 * (Квант-инженер requirement: metrics must be testable or they are unreliable).</p>
 */
class MetricsCalculatorTest {

    private static final double EPSILON = 1e-6;

    // =====================================================
    // Sharpe Ratio
    // =====================================================

    @Test
    void calculateSharpeRatio_withKnownReturns_returnsExpected() {
        // Monthly returns: [0.02, -0.01, 0.03, 0.01, -0.02]
        // This is a hand-verifiable dataset
        List<Double> returns = Arrays.asList(0.02, -0.01, 0.03, 0.01, -0.02);

        // avgReturn = (0.02 - 0.01 + 0.03 + 0.01 - 0.02) / 5 = 0.03 / 5 = 0.006
        // stdDev:
        //   (0.02-0.006)^2 = (0.014)^2 = 0.000196
        //   (-0.01-0.006)^2 = (-0.016)^2 = 0.000256
        //   (0.03-0.006)^2 = (0.024)^2 = 0.000576
        //   (0.01-0.006)^2 = (0.004)^2 = 0.000016
        //   (-0.02-0.006)^2 = (-0.026)^2 = 0.000676
        //   sum = 0.001720, variance = 0.001720/5 = 0.000344
        //   stdDev = sqrt(0.000344) ≈ 0.018548
        //
        // Annualized Sharpe = (avgReturn - rf/12) * 12 / (stdDev * sqrt(12))
        //   risk-free = 0.05, rf/12 = 0.0041667
        //   excessAnnualized = (0.006 - 0.004167) * 12 = 0.022000
        //   annualized = 0.022000 / (0.018548 * 3.4641) = 0.022000 / 0.064252
        //   annualized ≈ 0.3424
        double sharpe = MetricsCalculator.calculateSharpeRatio(returns, 0.05, 12);

        assertThat(sharpe).isCloseTo(0.3424, within(0.001));
    }

    @Test
    void calculateSharpeRatio_insufficientData_returnsZero() {
        // Single return -> stdDev undefined
        assertThat(MetricsCalculator.calculateSharpeRatio(Collections.singletonList(0.01), 0.05, 12))
            .isEqualTo(0.0);
        // Null returns
        assertThat(MetricsCalculator.calculateSharpeRatio(null, 0.05, 12)).isEqualTo(0.0);
        // Empty returns
        assertThat(MetricsCalculator.calculateSharpeRatio(Collections.emptyList(), 0.05, 12))
            .isEqualTo(0.0);
    }

    @Test
    void calculateSharpeRatio_flatReturns_returnsZero() {
        // All returns equal -> stdDev = 0 -> avoid division by zero
        List<Double> returns = Arrays.asList(0.01, 0.01, 0.01, 0.01);
        assertThat(MetricsCalculator.calculateSharpeRatio(returns, 0.05, 12)).isEqualTo(0.0);
    }

    // =====================================================
    // Sortino Ratio
    // =====================================================

    @Test
    void calculateSortinoRatio_withKnownReturns_returnsExpected() {
        // Monthly returns: [0.02, -0.01, 0.03, 0.01, -0.02]
        // risk-free = 0.05, rf/12 = 0.004167
        //
        // Downside deviation: only returns below rf/12
        //   (-0.01 - 0.004167)^2 = (-0.014167)^2 = 0.0002007
        //   (-0.02 - 0.004167)^2 = (-0.024167)^2 = 0.0005840
        //   sum = 0.0007847, /5 = 0.0001569
        //   downsideDev = sqrt(0.0001569) ≈ 0.012527
        //
        // Annualized Sortino = (avgReturn - rf/12) * 12 / (downsideDev * sqrt(12))
        //   excessAnnualized = (0.006 - 0.004167) * 12 = 0.022000
        //   annualized = 0.022000 / (0.012527 * 3.4641) = 0.022000 / 0.043395
        //   annualized ≈ 0.5070
        List<Double> returns = Arrays.asList(0.02, -0.01, 0.03, 0.01, -0.02);

        double sortino = MetricsCalculator.calculateSortinoRatio(returns, 0.05, 12);

        assertThat(sortino).isCloseTo(0.5070, within(0.001));
    }

    @Test
    void calculateSortinoRatio_noDownside_returnsZero() {
        // All returns positive -> no downside deviation -> might be very profitable,
        // but we cannot compute a meaningful Sortino
        List<Double> returns = Arrays.asList(0.01, 0.02, 0.03);
        assertThat(MetricsCalculator.calculateSortinoRatio(returns, 0.05, 12)).isEqualTo(0.0);
    }

    // =====================================================
    // Calmar Ratio
    // =====================================================

    @Test
    void calculateCalmarRatio_withKnownValues_returnsExpected() {
        // totalReturn = 0.25 (25% over the period), years = 1, maxDD = 0.10
        // CAGR = (1.25)^(1/1) - 1 = 0.25
        // Calmar = 0.25 / 0.10 = 2.5
        assertThat(MetricsCalculator.calculateCalmarRatio(0.25, 1.0, 0.10))
            .isCloseTo(2.5, within(EPSILON));
    }

    @Test
    void calculateCalmarRatio_multiYearCompounding() {
        // totalReturn = 0.44 (44%), years = 2, maxDD = 0.15
        // CAGR = (1.44)^(1/2) - 1 = 1.2 - 1 = 0.20
        // Calmar = 0.20 / 0.15 = 1.3333
        assertThat(MetricsCalculator.calculateCalmarRatio(0.44, 2.0, 0.15))
            .isCloseTo(1.3333, within(0.0001));
    }

    @Test
    void calculateCalmarRatio_maxDrawdownZero_returnsZero() {
        assertThat(MetricsCalculator.calculateCalmarRatio(0.25, 1.0, 0.0)).isEqualTo(0.0);
        assertThat(MetricsCalculator.calculateCalmarRatio(0.25, 1.0, -0.05)).isEqualTo(0.0);
        assertThat(MetricsCalculator.calculateCalmarRatio(0.25, 0.0, 0.10)).isEqualTo(0.0);
    }

    // =====================================================
    // Recovery Factor
    // =====================================================

    @Test
    void calculateRecoveryFactor_withKnownValues_returnsExpected() {
        // grossProfit = 100, grossLoss = 40, maxDD = 20
        // netProfit = 60, Recovery = 60/20 = 3.0
        assertThat(MetricsCalculator.calculateRecoveryFactor(100.0, 40.0, 20.0))
            .isCloseTo(3.0, within(EPSILON));
    }

    @Test
    void calculateRecoveryFactor_maxDrawdownZero_returnsZero() {
        assertThat(MetricsCalculator.calculateRecoveryFactor(100.0, 40.0, 0.0)).isEqualTo(0.0);
    }

    // =====================================================
    // R-Multiple
    // =====================================================

    @Test
    void calculateRMultiple_withKnownValues_returnsExpected() {
        // pnl = 100, entryPrice = 50, qty = 100, riskPercent = 0.02
        // initialRisk = 50 * 100 * 0.02 = 100
        // R = 100/100 = 1.0
        assertThat(MetricsCalculator.calculateRMultiple(100.0, 50.0, 100, 0.02))
            .isCloseTo(1.0, within(EPSILON));
    }

    @Test
    void calculateRMultiple_negativePnl_returnsNegative() {
        // pnl = -50, entryPrice = 50, qty = 100, riskPercent = 0.02
        // initialRisk = 100, R = -50/100 = -0.5
        assertThat(MetricsCalculator.calculateRMultiple(-50.0, 50.0, 100, 0.02))
            .isCloseTo(-0.5, within(EPSILON));
    }

    @Test
    void calculateRMultiple_invalidInput_returnsZero() {
        assertThat(MetricsCalculator.calculateRMultiple(100.0, 50.0, 100, 0.0)).isEqualTo(0.0);
        assertThat(MetricsCalculator.calculateRMultiple(100.0, 0.0, 100, 0.02)).isEqualTo(0.0);
        assertThat(MetricsCalculator.calculateRMultiple(100.0, 50.0, 0, 0.02)).isEqualTo(0.0);
    }

    @Test
    void calculateRMultipleStats_withTrades_returnsExpected() {
        List<BacktestRunner.TradeResult> trades = new ArrayList<>();
        // T1: entry=100, exit=101, qty=2, pnl=2 (profit)
        // T2: entry=50, exit=49, qty=1, pnl=-1 (loss)
        // T3: entry=50, exit=51, qty=1, pnl=1 (profit)
        trades.add(makeTrade("T1", "BUY", 100.0, 101.0, 2, 2.0, 0.1, "strategy_close", "01.01.2024 00:00:00"));
        trades.add(makeTrade("T2", "SELL", 50.0, 49.0, 1, -1.0, 0.05, "sl_hit", "02.01.2024 00:00:00"));
        trades.add(makeTrade("T3", "BUY", 50.0, 51.0, 1, 1.0, 0.05, "tp_hit", "03.01.2024 00:00:00"));

        // riskPercent = 0.02
        // T1: initialRisk = 100 * 2 * 0.02 = 4, R = 2/4 = 0.5
        // T2: initialRisk = 50 * 1 * 0.02 = 1, R = -1/1 = -1.0
        // T3: initialRisk = 50 * 1 * 0.02 = 1, R = 1/1 = 1.0
        // avg = (0.5 - 1.0 + 1.0)/3 = 0.5/3 ≈ 0.1667
        // median = 0.5 (sorted: -1.0, 0.5, 1.0)
        MetricsCalculator.RMultipleStats stats = MetricsCalculator.calculateRMultipleStats(trades, 0.02);

        assertThat(stats.count).isEqualTo(3);
        assertThat(stats.avg).isCloseTo(0.1667, within(0.001));
        assertThat(stats.median).isCloseTo(0.5, within(EPSILON));
        assertThat(stats.stdDev).isPositive();
    }

    @Test
    void calculateRMultipleStats_emptyTrades_returnsZeroStats() {
        MetricsCalculator.RMultipleStats stats = MetricsCalculator.calculateRMultipleStats(Collections.emptyList(), 0.02);
        assertThat(stats.count).isEqualTo(0);
        assertThat(stats.avg).isEqualTo(0.0);
        assertThat(stats.median).isEqualTo(0.0);
        assertThat(stats.stdDev).isEqualTo(0.0);
    }

    // =====================================================
    // CAGR / Profit Factor / Expectancy
    // =====================================================

    @Test
    void calculateCAGR_withKnownValues_returnsExpected() {
        // 100 -> 200 over 1 year: CAGR = 200/100 - 1 = 1.0 (100%)
        assertThat(MetricsCalculator.calculateCAGR(100.0, 200.0, 1.0)).isCloseTo(1.0, within(EPSILON));
        // 100 -> 121 over 2 years: CAGR = sqrt(121/100) - 1 = 1.1 - 1 = 0.10
        assertThat(MetricsCalculator.calculateCAGR(100.0, 121.0, 2.0))
            .isCloseTo(0.10, within(EPSILON));
    }

    @Test
    void calculateCAGR_invalidInput_returnsZero() {
        assertThat(MetricsCalculator.calculateCAGR(0.0, 100.0, 1.0)).isEqualTo(0.0);
        assertThat(MetricsCalculator.calculateCAGR(100.0, 0.0, 1.0)).isEqualTo(0.0);
        assertThat(MetricsCalculator.calculateCAGR(100.0, 100.0, 0.0)).isEqualTo(0.0);
    }

    @Test
    void calculateProfitFactor_withKnownValues_returnsExpected() {
        // grossProfit = 100, grossLoss = 40
        assertThat(MetricsCalculator.calculateProfitFactor(100.0, 40.0))
            .isCloseTo(2.5, within(EPSILON));
    }

    @Test
    void calculateProfitFactor_zeroLoss_returnsInfinity() {
        // All trades winning, no losses
        assertThat(MetricsCalculator.calculateProfitFactor(100.0, 0.0))
            .isEqualTo(Double.POSITIVE_INFINITY);
    }

    @Test
    void calculateExpectancy_withKnownValues_returnsExpected() {
        // winRate = 0.55, avgWin = 100, avgLoss = 50
        // expectancy = 0.55 * 100 - 0.45 * 50 = 55 - 22.5 = 32.5
        assertThat(MetricsCalculator.calculateExpectancy(0.55, 100.0, 50.0))
            .isCloseTo(32.5, within(EPSILON));
    }

    // =====================================================
    // Value at Risk (VaR)
    // =====================================================

    @Test
    void calculateHistoricalVaR_withKnownReturns_returnsExpected() {
        // Returns: [0.02, -0.01, 0.03, 0.01, -0.02]
        // Sorted: [-0.02, -0.01, 0.01, 0.02, 0.03]
        // For 95% confidence: index = ceil(0.05 * 5) - 1 = ceil(0.25) - 1 = 1 - 1 = 0
        // VaR = -(-0.02) = 0.02 (2% loss at 95% confidence)
        List<Double> returns = Arrays.asList(0.02, -0.01, 0.03, 0.01, -0.02);
        double var = MetricsCalculator.calculateHistoricalVaR(returns, 0.95);
        assertThat(var).isCloseTo(0.02, within(EPSILON));
    }

    @Test
    void calculateHistoricalVaR_withLargerDataset_returnsExpected() {
        // 100 returns with known distribution
        List<Double> returns = new ArrayList<>();
        for (int i = 0; i < 100; i++) {
            returns.add((i - 50) * 0.001); // Returns from -0.05 to 0.049
        }
        // For 95% confidence: index = ceil(0.05 * 100) - 1 = 5 - 1 = 4
        // returns[4] = (4 - 50) * 0.001 = -0.046
        // VaR = -(-0.046) = 0.046
        double var = MetricsCalculator.calculateHistoricalVaR(returns, 0.95);
        assertThat(var).isCloseTo(0.046, within(0.001));
    }

    @Test
    void calculateHistoricalVaR_insufficientData_returnsZero() {
        assertThat(MetricsCalculator.calculateHistoricalVaR(Collections.singletonList(0.01), 0.95))
            .isEqualTo(0.0);
        assertThat(MetricsCalculator.calculateHistoricalVaR(null, 0.95))
            .isEqualTo(0.0);
        assertThat(MetricsCalculator.calculateHistoricalVaR(Collections.emptyList(), 0.95))
            .isEqualTo(0.0);
    }

    @Test
    void calculateHistoricalVaR_invalidConfidence_throwsException() {
        assertThatThrownBy(() -> MetricsCalculator.calculateHistoricalVaR(Arrays.asList(0.01, 0.02), 1.5))
            .isInstanceOf(IllegalArgumentException.class);
        assertThatThrownBy(() -> MetricsCalculator.calculateHistoricalVaR(Arrays.asList(0.01, 0.02), -0.1))
            .isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    void calculateParametricVaR_withKnownReturns_returnsExpected() {
        // Returns: [0.02, -0.01, 0.03, 0.01, -0.02]
        // mean = 0.006
        // variance (sample) = sum((r - mean)^2) / (n-1) = 0.00172 / 4 = 0.00043
        // stdDev = sqrt(0.00043) ≈ 0.020736
        // For 95% confidence: zScore = 1.645
        // VaR = 1.645 * 0.020736 - 0.006 ≈ 0.03411 - 0.006 = 0.02811
        List<Double> returns = Arrays.asList(0.02, -0.01, 0.03, 0.01, -0.02);
        double var = MetricsCalculator.calculateParametricVaR(returns, 0.95);
        assertThat(var).isCloseTo(0.028, within(0.005));
    }

    @Test
    void calculateParametricVaR_insufficientData_returnsZero() {
        assertThat(MetricsCalculator.calculateParametricVaR(Collections.singletonList(0.01), 0.95))
            .isEqualTo(0.0);
        assertThat(MetricsCalculator.calculateParametricVaR(null, 0.95))
            .isEqualTo(0.0);
        assertThat(MetricsCalculator.calculateParametricVaR(Collections.emptyList(), 0.95))
            .isEqualTo(0.0);
    }

    @Test
    void calculateParametricVaR_invalidConfidence_throwsException() {
        assertThatThrownBy(() -> MetricsCalculator.calculateParametricVaR(Arrays.asList(0.01, 0.02), 1.5))
            .isInstanceOf(IllegalArgumentException.class);
        assertThatThrownBy(() -> MetricsCalculator.calculateParametricVaR(Arrays.asList(0.01, 0.02), -0.1))
            .isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    void calculateParametricVaR_differentConfidenceLevels_returnsExpected() {
        List<Double> returns = Arrays.asList(0.02, -0.01, 0.03, 0.01, -0.02, 0.015, -0.005, 0.025);
        
        // Higher confidence should give higher VaR (more conservative)
        double var90 = MetricsCalculator.calculateParametricVaR(returns, 0.90);
        double var95 = MetricsCalculator.calculateParametricVaR(returns, 0.95);
        double var99 = MetricsCalculator.calculateParametricVaR(returns, 0.99);
        
        assertThat(var99).isGreaterThan(var95);
        assertThat(var95).isGreaterThan(var90);
        assertThat(var90).isPositive();
    }

    // =====================================================
    // Helpers
    // =====================================================

    private static BacktestRunner.TradeResult makeTrade(
        String ticker, String dir, double entry, double exit, int qty, double pnl, double commission, String reason, String time) {
        return new BacktestRunner.TradeResult(ticker, dir, entry, exit, qty, pnl, commission, reason, time);
    }

    private static org.assertj.core.data.Offset<Double> within(double tolerance) {
        return org.assertj.core.data.Offset.offset(tolerance);
    }
}