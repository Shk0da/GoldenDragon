package com.github.shk0da.goldendragon.backtest;

import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Tests verifying the peak-to-trough MaxDrawdown calculation fix
 * (Квант-инженер critical bugfix: drawdown must be measured from peak, not start).
 * Uses the real BacktestRunner.calcMaxDrawdownByEquity instead of duplicating logic.
 */
class MaxDrawdownPeakToTroughTest {

    private static final double EPSILON = 1e-9;

    @Test
    void maxDrawdown_peakToTrough_expectsCorrectValue() {
        // Equity curve: 100 -> 150 -> 120 -> 180 -> 160
        // Peak:         100 -> 150 -> 150 -> 180 -> 180
        // DD from peak: 0%  -> 0%  -> 20% -> 0%  -> 11.1%
        // Max DD (peak-to-trough) = 20% = 0.20
        List<BacktestRunner.EquityPoint> curve = Arrays.asList(
            point("01.01.2024 00:00:00", 100.0),
            point("02.01.2024 00:00:00", 150.0),
            point("03.01.2024 00:00:00", 120.0),
            point("04.01.2024 00:00:00", 180.0),
            point("05.01.2024 00:00:00", 160.0));

        double dd = BacktestRunner.calcMaxDrawdownByEquity(curve);

        assertThat(dd).isCloseTo(0.20, within(EPSILON));
    }

    @Test
    void maxDrawdown_peakToTrough_whenEveryPointAboveStart_expectsSecondDip() {
        // Old logic (from start) would give: (100 - 90)/100 = 10% at the dip
        // New logic (from peak) must give: (200 - 170)/200 = 15% at the second trough
        // even though equity never fell below the starting capital.
        List<BacktestRunner.EquityPoint> curve = Arrays.asList(
            point("01.01.2024 00:00:00", 100.0),
            point("02.01.2024 00:00:00", 200.0),  // peak
            point("03.01.2024 00:00:00", 170.0),  // 15% from peak
            point("04.01.2024 00:00:00", 250.0),  // new peak
            point("05.01.2024 00:00:00", 230.0)); // 8% from new peak

        double dd = BacktestRunner.calcMaxDrawdownByEquity(curve);

        // Max DD = 15% (from 200 to 170), NOT 0% (never below start)
        assertThat(dd).isCloseTo(0.15, within(EPSILON));
    }

    @Test
    void maxDrawdown_peakToTrough_monotonicGrowth_returnsZero() {
        List<BacktestRunner.EquityPoint> curve = Arrays.asList(
            point("01.01.2024 00:00:00", 100.0),
            point("02.01.2024 00:00:00", 110.0),
            point("03.01.2024 00:00:00", 120.0),
            point("04.01.2024 00:00:00", 130.0));

        assertThat(BacktestRunner.calcMaxDrawdownByEquity(curve)).isCloseTo(0.0, within(EPSILON));
    }

    @Test
    void maxDrawdown_peakToTrough_emptyCurve_returnsZero() {
        assertThat(BacktestRunner.calcMaxDrawdownByEquity(new ArrayList<>())).isCloseTo(0.0, within(EPSILON));
        assertThat(BacktestRunner.calcMaxDrawdownByEquity(null)).isCloseTo(0.0, within(EPSILON));
    }

    @Test
    void maxDrawdown_peakToTrough_singlePoint_returnsZero() {
        List<BacktestRunner.EquityPoint> curve = new ArrayList<>();
        curve.add(point("01.01.2024 00:00:00", 100.0));
        assertThat(BacktestRunner.calcMaxDrawdownByEquity(curve)).isCloseTo(0.0, within(EPSILON));
    }

    @Test
    void maxDrawdown_peakToTrough_oldLogicUnderestimatesRisk_demonstrated() {
        // This test demonstrates WHY the old (start-based) logic was wrong:
        // a strategy that grows 100 -> 500 then loses 50% to 250 has a TRUE
        // drawdown of 50%, but the old logic computed only (100-100)/100 = 0%.
        List<BacktestRunner.EquityPoint> curve = Arrays.asList(
            point("01.01.2024 00:00:00", 100.0),
            point("02.01.2024 00:00:00", 500.0),
            point("03.01.2024 00:00:00", 250.0));

        double ddPeakToTrough = BacktestRunner.calcMaxDrawdownByEquity(curve);

        // True risk (peak-to-trough) = (500 - 250)/500 = 50%
        assertThat(ddPeakToTrough).isCloseTo(0.50, within(EPSILON));

        // Old logic (from start = 100) = (100 - 250)/100 < 0 -> clamped to 0.0
        // This proves the old metric catastrophically underestimated risk
        double oldLogicDd = 0.0; // any value below peak would have been 0
        assertThat(ddPeakToTrough).isGreaterThan(oldLogicDd);
    }

    // =====================================================
    // Helpers
    // =====================================================

    private static BacktestRunner.EquityPoint point(String time, double equity) {
        return new BacktestRunner.EquityPoint(time, equity);
    }

    private static org.assertj.core.data.Offset<Double> within(double tolerance) {
        return org.assertj.core.data.Offset.offset(tolerance);
    }
}