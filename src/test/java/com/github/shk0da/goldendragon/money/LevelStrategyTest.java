package com.github.shk0da.goldendragon.money;

import com.github.shk0da.goldendragon.model.Candle;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;

import static org.assertj.core.api.BDDAssertions.then;
import static org.assertj.core.api.BDDAssertions.within;

@DisplayName("LevelStrategy with 2 ATR cap")
class LevelStrategyTest {

    private static final double ENTRY_PRICE = 100.0;
    private static final double ATR = 2.0;
    private static final double SL_MULT = 1.2;
    private static final double TP_MULT = 2.5;
    private static final int ATR_PERIOD = 14;

    private static final double MAX_TP_PERCENT = 0.015;
    private static final double MAX_ATR_MULT = 2.0;

    private final LevelStrategy strategy = new LevelStrategy();

    private List<Candle> createCandlesBetween(double support, double resistance) {
        List<Candle> candles = new ArrayList<>();
        double mid = (support + resistance) / 2.0;
        for (int i = 0; i < 20; i++) {
            candles.add(new Candle(
                    "2024-01-01T" + String.format("%02d", i) + ":00:00",
                    mid,
                    resistance,
                    support,
                    mid,
                    1000L
            ));
        }
        return candles;
    }

    private List<Candle> createMonotonicCandles() {
        List<Candle> candles = new ArrayList<>();
        for (int i = 0; i < 20; i++) {
            candles.add(new Candle(
                    "2024-01-01T" + String.format("%02d", i) + ":00:00",
                    ENTRY_PRICE + i,
                    ENTRY_PRICE + i + 1,
                    ENTRY_PRICE + i,
                    ENTRY_PRICE + i,
                    1000L
            ));
        }
        return candles;
    }

    @Nested
    @DisplayName("When levels are close (< 2 ATR)")
    class CloseLevels {

        @Test
        @DisplayName("Should place SL below entry and TP above entry (LONG)")
        void long_SlBelow_TpAbove() {
            List<Candle> candles = createCandlesBetween(98.0, 101.0);

            StopLossTakeProfitStrategy.SLTPResult result = strategy.calculate(
                    ENTRY_PRICE, true, candles, ATR, ATR, 25.0, SL_MULT, TP_MULT, ATR_PERIOD
            );

            then(result).isNotNull();
            then(ENTRY_PRICE - result.slDistance).isLessThan(ENTRY_PRICE);
            then(ENTRY_PRICE + result.tpDistance).isGreaterThan(ENTRY_PRICE);
        }

        @Test
        @DisplayName("Should place SL above entry and TP below entry (SHORT)")
        void short_SlAbove_TpBelow() {
            List<Candle> candles = createCandlesBetween(99.0, 102.0);

            StopLossTakeProfitStrategy.SLTPResult result = strategy.calculate(
                    ENTRY_PRICE, false, candles, ATR, ATR, 25.0, SL_MULT, TP_MULT, ATR_PERIOD
            );

            then(result).isNotNull();
            then(ENTRY_PRICE + result.slDistance).isGreaterThan(ENTRY_PRICE);
            then(ENTRY_PRICE - result.tpDistance).isLessThan(ENTRY_PRICE);
        }

        @Test
        @DisplayName("Should keep SL within 2 ATR cap")
        void sl_WithinCap() {
            List<Candle> candles = createCandlesBetween(98.0, 101.0);

            StopLossTakeProfitStrategy.SLTPResult result = strategy.calculate(
                    ENTRY_PRICE, true, candles, ATR, ATR, 25.0, SL_MULT, TP_MULT, ATR_PERIOD
            );

            then(result).isNotNull();
            then(result.slDistance).isLessThanOrEqualTo(ATR * MAX_ATR_MULT);
        }

        @Test
        @DisplayName("Should keep TP within 1.5% of entry")
        void tp_WithinCap() {
            List<Candle> candles = createCandlesBetween(98.0, 101.0);

            StopLossTakeProfitStrategy.SLTPResult result = strategy.calculate(
                    ENTRY_PRICE, true, candles, ATR, ATR, 25.0, SL_MULT, TP_MULT, ATR_PERIOD
            );

            then(result).isNotNull();
            then(result.tpDistance).isLessThanOrEqualTo(ENTRY_PRICE * MAX_TP_PERCENT);
        }
    }

    @Nested
    @DisplayName("When levels are far (> 2 ATR)")
    class FarLevels {

        @Test
        @DisplayName("Should place SL below entry and TP above entry (LONG, fallback)")
        void long_FarLevels_SlBelow_TpAbove() {
            List<Candle> candles = createCandlesBetween(50.0, 150.0);

            StopLossTakeProfitStrategy.SLTPResult result = strategy.calculate(
                    ENTRY_PRICE, true, candles, ATR, ATR, 25.0, SL_MULT, TP_MULT, ATR_PERIOD
            );

            then(result).isNotNull();
            then(ENTRY_PRICE - result.slDistance).isLessThan(ENTRY_PRICE);
            then(ENTRY_PRICE + result.tpDistance).isGreaterThan(ENTRY_PRICE);
        }

        @Test
        @DisplayName("Should place SL above entry and TP below entry (SHORT, fallback)")
        void short_FarLevels_SlAbove_TpBelow() {
            List<Candle> candles = createCandlesBetween(50.0, 150.0);

            StopLossTakeProfitStrategy.SLTPResult result = strategy.calculate(
                    ENTRY_PRICE, false, candles, ATR, ATR, 25.0, SL_MULT, TP_MULT, ATR_PERIOD
            );

            then(result).isNotNull();
            then(ENTRY_PRICE + result.slDistance).isGreaterThan(ENTRY_PRICE);
            then(ENTRY_PRICE - result.tpDistance).isLessThan(ENTRY_PRICE);
        }

        @Test
        @DisplayName("Should keep both SL and TP within caps (fallback)")
        void fallback_WithinCaps() {
            List<Candle> candles = createCandlesBetween(50.0, 150.0);

            StopLossTakeProfitStrategy.SLTPResult result = strategy.calculate(
                    ENTRY_PRICE, true, candles, ATR, ATR, 25.0, SL_MULT, TP_MULT, ATR_PERIOD
            );

            then(result).isNotNull();
            then(result.slDistance).isLessThanOrEqualTo(ATR * MAX_ATR_MULT);
            then(result.tpDistance).isLessThanOrEqualTo(ENTRY_PRICE * MAX_TP_PERCENT);
            then(result.tpDistance).isLessThanOrEqualTo(ATR * MAX_ATR_MULT);
        }

        @Test
        @DisplayName("Should never produce negative distances for LONG")
        void long_NoNegativeDistances() {
            List<Candle> candles = createCandlesBetween(50.0, 150.0);

            StopLossTakeProfitStrategy.SLTPResult result = strategy.calculate(
                    ENTRY_PRICE, true, candles, ATR, ATR, 25.0, SL_MULT, TP_MULT, ATR_PERIOD
            );

            then(result).isNotNull();
            then(result.slDistance).isGreaterThan(0.0);
            then(result.tpDistance).isGreaterThan(0.0);
        }

        @Test
        @DisplayName("Should never produce negative distances for SHORT")
        void short_NoNegativeDistances() {
            List<Candle> candles = createCandlesBetween(50.0, 150.0);

            StopLossTakeProfitStrategy.SLTPResult result = strategy.calculate(
                    ENTRY_PRICE, false, candles, ATR, ATR, 25.0, SL_MULT, TP_MULT, ATR_PERIOD
            );

            then(result).isNotNull();
            then(result.slDistance).isGreaterThan(0.0);
            then(result.tpDistance).isGreaterThan(0.0);
        }
    }

    @Nested
    @DisplayName("When no levels found")
    class NoLevels {

        @Test
        @DisplayName("Should place SL below entry and TP above entry (LONG)")
        void long_NoLevels_SlBelow_TpAbove() {
            List<Candle> candles = createMonotonicCandles();

            StopLossTakeProfitStrategy.SLTPResult result = strategy.calculate(
                    ENTRY_PRICE, true, candles, ATR, ATR, 25.0, SL_MULT, TP_MULT, ATR_PERIOD
            );

            then(result).isNotNull();
            then(ENTRY_PRICE - result.slDistance).isLessThan(ENTRY_PRICE);
            then(ENTRY_PRICE + result.tpDistance).isGreaterThan(ENTRY_PRICE);
        }

        @Test
        @DisplayName("Should place SL above entry and TP below entry (SHORT)")
        void short_NoLevels_SlAbove_TpBelow() {
            List<Candle> candles = createMonotonicCandles();

            StopLossTakeProfitStrategy.SLTPResult result = strategy.calculate(
                    ENTRY_PRICE, false, candles, ATR, ATR, 25.0, SL_MULT, TP_MULT, ATR_PERIOD
            );

            then(result).isNotNull();
            then(ENTRY_PRICE + result.slDistance).isGreaterThan(ENTRY_PRICE);
            then(ENTRY_PRICE - result.tpDistance).isLessThan(ENTRY_PRICE);
        }

        @Test
        @DisplayName("Should keep SL and TP within caps when no levels")
        void noLevels_WithinCaps() {
            List<Candle> candles = createMonotonicCandles();

            StopLossTakeProfitStrategy.SLTPResult result = strategy.calculate(
                    ENTRY_PRICE, true, candles, ATR, ATR, 25.0, SL_MULT, TP_MULT, ATR_PERIOD
            );

            then(result).isNotNull();
            then(result.slDistance).isLessThanOrEqualTo(ATR * MAX_ATR_MULT);
            then(result.tpDistance).isLessThanOrEqualTo(ENTRY_PRICE * MAX_TP_PERCENT);
            then(result.tpDistance).isGreaterThan(0.0);
        }
    }

    @Nested
    @DisplayName("When TP approaches the 1.5% cap")
    class TpCap {

        @Test
        @DisplayName("Should use resistance level for TP when close enough")
        void useLevel_WhenTpClose() {
            double resistance = ENTRY_PRICE * (1.0 + 0.008);
            List<Candle> candles = createCandlesBetween(98.0, resistance);

            StopLossTakeProfitStrategy.SLTPResult result = strategy.calculate(
                    ENTRY_PRICE, true, candles, ATR, ATR, 25.0, SL_MULT, TP_MULT, ATR_PERIOD
            );

            then(result).isNotNull();
            then(result.tpDistance).isGreaterThan(0.0);
            then(result.tpDistance).isLessThanOrEqualTo(ENTRY_PRICE * MAX_TP_PERCENT);
        }

        @Test
        @DisplayName("Should cap TP at 1.5% when level is far")
        void capTp_WhenLevelFar() {
            double resistance = ENTRY_PRICE * 1.10;
            List<Candle> candles = createCandlesBetween(98.0, resistance);

            StopLossTakeProfitStrategy.SLTPResult result = strategy.calculate(
                    ENTRY_PRICE, true, candles, ATR, ATR, 25.0, SL_MULT, TP_MULT, ATR_PERIOD
            );

            then(result).isNotNull();
            then(result.tpDistance).isLessThanOrEqualTo(ENTRY_PRICE * MAX_TP_PERCENT);
            then(result.tpDistance).isGreaterThan(0.0);
        }
    }

    @Nested
    @DisplayName("Edge cases")
    class EdgeCases {

        @Test
        @DisplayName("Should return null when candles list is empty")
        void shouldReturnNull_EmptyCandles() {
            StopLossTakeProfitStrategy.SLTPResult result = strategy.calculate(
                    ENTRY_PRICE, true, new ArrayList<>(), ATR, ATR, 25.0, SL_MULT, TP_MULT, ATR_PERIOD
            );

            then(result).isNull();
        }

        @Test
        @DisplayName("Should return null when ATR is zero")
        void shouldReturnNull_ZeroAtr() {
            List<Candle> candles = createCandlesBetween(98.0, 101.0);

            StopLossTakeProfitStrategy.SLTPResult result = strategy.calculate(
                    ENTRY_PRICE, true, candles, 0.0, 0.0, 25.0, SL_MULT, TP_MULT, ATR_PERIOD
            );

            then(result).isNull();
        }

        @Test
        @DisplayName("Should return null when entry price is zero")
        void shouldReturnNull_ZeroEntry() {
            List<Candle> candles = createCandlesBetween(0.5, 1.5);

            StopLossTakeProfitStrategy.SLTPResult result = strategy.calculate(
                    0.0, true, candles, ATR, ATR, 25.0, SL_MULT, TP_MULT, ATR_PERIOD
            );

            then(result).isNull();
        }
    }
}