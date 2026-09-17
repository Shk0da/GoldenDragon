package com.github.shk0da.goldendragon.model;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.BDDAssertions.then;

@DisplayName("TickerCandle")
class TickerCandleTest {

    @Nested
    @DisplayName("Constructor")
    class Constructor {

        @Test
        @DisplayName("Should initialize with all fields")
        void shouldInitializeWithAllFields() {
            TickerCandle candle = new TickerCandle(
                    "AAPL", "2024-01-01",
                    150.0, 155.0, 148.0, 153.0, 152.0, 1000000L
            );

            then(candle.getSymbol()).isEqualTo("AAPL");
            then(candle.getDate()).isEqualTo("2024-01-01");
            then(candle.getOpen()).isEqualTo(150.0);
            then(candle.getHigh()).isEqualTo(155.0);
            then(candle.getLow()).isEqualTo(148.0);
            then(candle.getClose()).isEqualTo(153.0);
            then(candle.getAdjClose()).isEqualTo(152.0);
            then(candle.getVolume()).isEqualTo(1000000L);
        }

        @Test
        @DisplayName("Should handle null values")
        void shouldHandleNullValues() {
            TickerCandle candle = new TickerCandle(
                    null, null, null, null, null, null, null, null
            );

            then(candle.getSymbol()).isNull();
            then(candle.getDate()).isNull();
            then(candle.getOpen()).isNull();
            then(candle.getHigh()).isNull();
            then(candle.getLow()).isNull();
            then(candle.getClose()).isNull();
            then(candle.getAdjClose()).isNull();
            then(candle.getVolume()).isNull();
        }
    }

    @Nested
    @DisplayName("equals()")
    class Equals {

        @Test
        @DisplayName("Should be equal when symbol and date match")
        void shouldBeEqualWhenSymbolAndDateMatch() {
            TickerCandle candle1 = new TickerCandle(
                    "AAPL", "2024-01-01",
                    150.0, 155.0, 148.0, 153.0, 152.0, 1000000L
            );
            TickerCandle candle2 = new TickerCandle(
                    "AAPL", "2024-01-01",
                    151.0, 156.0, 149.0, 154.0, 153.0, 2000000L
            );

            then(candle1).isEqualTo(candle2);
        }

        @Test
        @DisplayName("Should not be equal when symbol differs")
        void shouldNotBeEqualWhenSymbolDiffers() {
            TickerCandle candle1 = new TickerCandle(
                    "AAPL", "2024-01-01",
                    150.0, 155.0, 148.0, 153.0, 152.0, 1000000L
            );
            TickerCandle candle2 = new TickerCandle(
                    "GOOGL", "2024-01-01",
                    150.0, 155.0, 148.0, 153.0, 152.0, 1000000L
            );

            then(candle1).isNotEqualTo(candle2);
        }

        @Test
        @DisplayName("Should not be equal when date differs")
        void shouldNotBeEqualWhenDateDiffers() {
            TickerCandle candle1 = new TickerCandle(
                    "AAPL", "2024-01-01",
                    150.0, 155.0, 148.0, 153.0, 152.0, 1000000L
            );
            TickerCandle candle2 = new TickerCandle(
                    "AAPL", "2024-01-02",
                    150.0, 155.0, 148.0, 153.0, 152.0, 1000000L
            );

            then(candle1).isNotEqualTo(candle2);
        }

        @Test
        @DisplayName("Should be equal to itself")
        void shouldBeEqualToItself() {
            TickerCandle candle = new TickerCandle(
                    "AAPL", "2024-01-01",
                    150.0, 155.0, 148.0, 153.0, 152.0, 1000000L
            );

            then(candle).isEqualTo(candle);
        }

        @Test
        @DisplayName("Should not be equal to null")
        void shouldNotBeEqualToNull() {
            TickerCandle candle = new TickerCandle(
                    "AAPL", "2024-01-01",
                    150.0, 155.0, 148.0, 153.0, 152.0, 1000000L
            );

            then(candle).isNotNull();
            then(candle).isNotEqualTo(null);
        }

        @Test
        @DisplayName("Should not be equal to different class")
        void shouldNotBeEqualToDifferentClass() {
            TickerCandle candle = new TickerCandle(
                    "AAPL", "2024-01-01",
                    150.0, 155.0, 148.0, 153.0, 152.0, 1000000L
            );

            then(candle).isNotEqualTo("AAPL");
            then(candle).isNotEqualTo(123);
        }

        @Test
        @DisplayName("Should handle null symbol and date")
        void shouldHandleNullSymbolAndDate() {
            TickerCandle candle1 = new TickerCandle(
                    null, null, 150.0, 155.0, 148.0, 153.0, 152.0, 1000000L
            );
            TickerCandle candle2 = new TickerCandle(
                    null, null, 151.0, 156.0, 149.0, 154.0, 153.0, 2000000L
            );

            then(candle1).isEqualTo(candle2);
        }
    }

    @Nested
    @DisplayName("hashCode()")
    class HashCode {

        @Test
        @DisplayName("Should return same hash for equal candles")
        void shouldReturnSameHashForEqualCandles() {
            TickerCandle candle1 = new TickerCandle(
                    "AAPL", "2024-01-01",
                    150.0, 155.0, 148.0, 153.0, 152.0, 1000000L
            );
            TickerCandle candle2 = new TickerCandle(
                    "AAPL", "2024-01-01",
                    151.0, 156.0, 149.0, 154.0, 153.0, 2000000L
            );

            then(candle1.hashCode()).isEqualTo(candle2.hashCode());
        }

        @Test
        @DisplayName("Should return different hash for different candles")
        void shouldReturnDifferentHashForDifferentCandles() {
            TickerCandle candle1 = new TickerCandle(
                    "AAPL", "2024-01-01",
                    150.0, 155.0, 148.0, 153.0, 152.0, 1000000L
            );
            TickerCandle candle2 = new TickerCandle(
                    "GOOGL", "2024-01-01",
                    150.0, 155.0, 148.0, 153.0, 152.0, 1000000L
            );

            then(candle1.hashCode()).isNotEqualTo(candle2.hashCode());
        }

        @Test
        @DisplayName("Should be consistent across multiple calls")
        void shouldBeConsistentAcrossMultipleCalls() {
            TickerCandle candle = new TickerCandle(
                    "AAPL", "2024-01-01",
                    150.0, 155.0, 148.0, 153.0, 152.0, 1000000L
            );

            int hash1 = candle.hashCode();
            int hash2 = candle.hashCode();
            int hash3 = candle.hashCode();

            then(hash1).isEqualTo(hash2);
            then(hash2).isEqualTo(hash3);
        }
    }

    @Nested
    @DisplayName("toString()")
    class ToString {

        @Test
        @DisplayName("Should return formatted string with all fields")
        void shouldReturnFormattedString() {
            TickerCandle candle = new TickerCandle(
                    "AAPL", "2024-01-01",
                    150.0, 155.0, 148.0, 153.0, 152.0, 1000000L
            );

            String result = candle.toString();

            then(result).contains("TickerCandle");
            then(result).contains("symbol='AAPL'");
            then(result).contains("date='2024-01-01'");
            then(result).contains("open=150.0");
            then(result).contains("high=155.0");
            then(result).contains("low=148.0");
            then(result).contains("close=153.0");
            then(result).contains("adjClose=152.0");
            then(result).contains("volume=1000000");
        }

        @Test
        @DisplayName("Should handle null values in toString")
        void shouldHandleNullValuesInToString() {
            TickerCandle candle = new TickerCandle(
                    null, null, null, null, null, null, null, null
            );

            String result = candle.toString();

            then(result).contains("TickerCandle");
            then(result).contains("symbol='null'");
            then(result).contains("date='null'");
        }
    }
}
