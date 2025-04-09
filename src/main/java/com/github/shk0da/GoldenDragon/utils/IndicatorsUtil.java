package com.github.shk0da.GoldenDragon.utils;

import com.github.shk0da.GoldenDragon.model.TickerCandle;
import com.tictactec.ta.lib.Core;
import com.tictactec.ta.lib.MAType;
import com.tictactec.ta.lib.MInteger;
import com.tictactec.ta.lib.RetCode;
import ru.tinkoff.piapi.contract.v1.HistoricCandle;
import ru.tinkoff.piapi.contract.v1.Quotation;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static java.time.LocalDateTime.ofInstant;
import static java.time.ZoneId.systemDefault;

public class IndicatorsUtil {

    private static final Core talib = new Core();

    public static final int INDICATORS_SHIFT = 70;
    public static final int SHIFT_SIZE = 14;
    public static final int MACD_FAST_PERIOD = 21;
    public static final int MACD_SLOW_PERIOD = 26;
    public static final int MACD_PERIOD = 9;
    public static final int RSI_PERIOD = 14;
    public static final int ADX_PERIOD = 14;
    public static final int MA_BLACK_PERIOD = 63;
    public static final int MA_WHITE_PERIOD = 7;
    public static final int SMA_PERIOD = 14;
    public static final int OBV_PERIOD = 14;

    private static final DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm:ss");
    private static final DateFormat dateTimeFormat = new SimpleDateFormat("dd.MM.yyyy HH:mm:ss");

    public static final class Indicator {

        private String name;
        private double value;
        private LocalDateTime dateTime;
        private double close;

        public Indicator(String name, double value, LocalDateTime dateTime, double close) {
            this.name = name;
            this.value = value;
            this.dateTime = dateTime;
            this.close = close;
        }

        public String getName() {
            return name;
        }

        public double getValue() {
            return value;
        }

        public LocalDateTime getDateTime() {
            return dateTime;
        }

        public double getClose() {
            return close;
        }

        @Override
        public String toString() {
            return "Indicator{" +
                    "name='" + name + '\'' +
                    ", value=" + value +
                    ", dateTime=" + dateTime +
                    ", close=" + close +
                    '}';
        }
    }

    public static Double calculateATR(List<TickerCandle> candles, int period) {
        List<TickerCandle> D1 = convertCandles(candles, 24, ChronoUnit.HOURS);
        if (D1.size() < period + 1) {
            return D1.get(D1.size() - 1).getHigh() - D1.get(D1.size() - 1).getLow();
        }
        double atr = 0.0;
        for (TickerCandle candle : D1.subList(D1.size() - period + 1, D1.size() - 1)) {
            atr += candle.getHigh() - candle.getLow();
        }
        return atr / period;
    }

    public static Map<String, List<Indicator>> getIndicators(List<TickerCandle> candles) {
        Map<String, List<Indicator>> indicators = new HashMap<>();

        // MA Black
        List<Indicator> MABlack = new ArrayList<>();
        for (int i = MA_BLACK_PERIOD; i < candles.size(); i++) {
            double[] inClose = new double[MA_BLACK_PERIOD];
            int k = 0;
            int j = i - MA_BLACK_PERIOD;
            while (k < MA_BLACK_PERIOD) {
                inClose[k] = candles.get(j).getClose();
                k++;
                j++;
            }
            double value = movingAverageBlack(inClose);
            MABlack.add(new Indicator(
                    "MABlack",
                    value,
                    LocalDateTime.parse(candles.get(j).getDate(), formatter),
                    candles.get(j).getClose()));
        }
        indicators.put("MABlack", MABlack);

        // MA White
        List<Indicator> MAWhite = new ArrayList<>();
        for (int i = MA_WHITE_PERIOD; i < candles.size(); i++) {
            double[] inClose = new double[MA_WHITE_PERIOD];
            int k = 0;
            int j = i - MA_WHITE_PERIOD;
            while (k < MA_WHITE_PERIOD) {
                inClose[k] = candles.get(j).getClose();
                k++;
                j++;
            }
            double value = movingAverageWhite(inClose);
            MAWhite.add(new Indicator(
                    "MAWhite",
                    value,
                    LocalDateTime.parse(candles.get(j).getDate(), formatter),
                    candles.get(j).getClose()));
        }
        indicators.put("MAWhite", MAWhite);

        // MACD
        int chunkMacdSize = MACD_SLOW_PERIOD + SHIFT_SIZE;
        List<Indicator> MACD = new ArrayList<>();
        for (int i = chunkMacdSize; i < candles.size(); i++) {
            double[] inClose = new double[chunkMacdSize];
            int k = 0;
            int j = i - chunkMacdSize;
            while (k < chunkMacdSize) {
                inClose[k] = candles.get(j).getClose();
                k++;
                j++;
            }
            double value = macd(inClose);
            MACD.add(new Indicator(
                    "MACD",
                    value,
                    LocalDateTime.parse(candles.get(j).getDate(), formatter),
                    candles.get(j).getClose()));
        }
        indicators.put("MACD", MACD);

        // RSI
        List<Indicator> RSI = new ArrayList<>();
        int chunkRsiSize = RSI_PERIOD + SHIFT_SIZE;
        for (int i = chunkRsiSize; i < candles.size(); i++) {
            double[] inClose = new double[chunkRsiSize];
            int k = 0;
            int j = i - chunkRsiSize;
            while (k < chunkRsiSize) {
                inClose[k] = candles.get(j).getClose();
                k++;
                j++;
            }
            double value = rsi(inClose);
            RSI.add(new Indicator(
                    "RSI",
                    value,
                    LocalDateTime.parse(candles.get(j).getDate(), formatter),
                    candles.get(j).getClose()));
        }
        indicators.put("RSI", RSI);

        // OBV
        List<Indicator> OBV = new ArrayList<>();
        for (int i = OBV_PERIOD; i < candles.size(); i++) {
            double[] inClose = new double[OBV_PERIOD];
            double[] inVolume = new double[OBV_PERIOD];
            int k = 0;
            int j = i - OBV_PERIOD;
            while (k < OBV_PERIOD) {
                inClose[k] = candles.get(j).getClose();
                inVolume[k] = candles.get(j).getVolume();
                k++;
                j++;
            }
            double value = obv(inClose, inVolume);
            OBV.add(new Indicator(
                    "OBV",
                    value,
                    LocalDateTime.parse(candles.get(j).getDate(), formatter),
                    candles.get(j).getClose()));
        }
        indicators.put("OBV", OBV);

        // ADX
        List<Indicator> ADX = new ArrayList<>();
        int chunkAdxSize = ADX_PERIOD + SHIFT_SIZE;
        for (int i = chunkAdxSize; i < candles.size(); i++) {
            double[] inClose = new double[chunkAdxSize];
            double[] inHigh = new double[chunkAdxSize];
            double[] inLow = new double[chunkAdxSize];
            int k = 0;
            int j = i - chunkAdxSize;
            while (k < chunkAdxSize) {
                inClose[k] = candles.get(j).getClose();
                inHigh[k] = candles.get(j).getHigh();
                inLow[k] = candles.get(j).getLow();
                k++;
                j++;
            }
            double value = adx(inClose, inLow, inHigh);
            ADX.add(new Indicator(
                    "ADX",
                    value,
                    LocalDateTime.parse(candles.get(j).getDate(), formatter),
                    candles.get(j).getClose()));
        }
        indicators.put("ADX", ADX);

        return indicators;
    }

    public static Map<String, List<Indicator>> initializeIndicators(List<HistoricCandle> candles) {
        Map<String, List<Indicator>> indicators = new HashMap<>();

        // MA Black
        List<Indicator> MABlack = new ArrayList<>();
        for (int i = MA_BLACK_PERIOD; i < candles.size(); i++) {
            double[] inClose = new double[MA_BLACK_PERIOD];
            int k = 0;
            int j = i - MA_BLACK_PERIOD;
            while (k < MA_BLACK_PERIOD) {
                inClose[k] = toDouble(candles.get(j).getClose());
                k++;
                j++;
            }
            double value = movingAverageBlack(inClose);
            MABlack.add(new Indicator(
                    "MABlack",
                    value,
                    LocalDateTime.ofInstant(Instant.ofEpochSecond(candles.get(j).getTime().getSeconds()), ZoneId.systemDefault()),
                    toDouble(candles.get(j).getClose())));
        }
        indicators.put("MABlack", MABlack);

        // MA White
        List<Indicator> MAWhite = new ArrayList<>();
        for (int i = MA_WHITE_PERIOD; i < candles.size(); i++) {
            double[] inClose = new double[MA_WHITE_PERIOD];
            int k = 0;
            int j = i - MA_WHITE_PERIOD;
            while (k < MA_WHITE_PERIOD) {
                inClose[k] = toDouble(candles.get(j).getClose());
                k++;
                j++;
            }
            double value = movingAverageWhite(inClose);
            MAWhite.add(new Indicator(
                    "MAWhite",
                    value,
                    LocalDateTime.ofInstant(Instant.ofEpochSecond(candles.get(j).getTime().getSeconds()), ZoneId.systemDefault()),
                    toDouble(candles.get(j).getClose())));
        }
        indicators.put("MAWhite", MAWhite);

        // MACD
        int chunkMacdSize = MACD_SLOW_PERIOD + SHIFT_SIZE;
        List<Indicator> MACD = new ArrayList<>();
        for (int i = chunkMacdSize; i < candles.size(); i++) {
            double[] inClose = new double[chunkMacdSize];
            int k = 0;
            int j = i - chunkMacdSize;
            while (k < chunkMacdSize) {
                inClose[k] = toDouble(candles.get(j).getClose());
                k++;
                j++;
            }
            double value = macd(inClose);
            MACD.add(new Indicator(
                    "MACD",
                    value,
                    LocalDateTime.ofInstant(Instant.ofEpochSecond(candles.get(j).getTime().getSeconds()), ZoneId.systemDefault()),
                    toDouble(candles.get(j).getClose())));
        }
        indicators.put("MACD", MACD);

        // MACD SIGN
        int chunkMacdSignSize = MACD_SLOW_PERIOD + SHIFT_SIZE;
        List<Indicator> MACD_SIGN = new ArrayList<>();
        for (int i = chunkMacdSignSize; i < candles.size(); i++) {
            double[] inClose = new double[chunkMacdSignSize];
            int k = 0;
            int j = i - chunkMacdSignSize;
            while (k < chunkMacdSignSize) {
                inClose[k] = toDouble(candles.get(j).getClose());
                k++;
                j++;
            }
            double value = macdSign(inClose);
            MACD_SIGN.add(new Indicator(
                    "MACD_SIGN",
                    value,
                    LocalDateTime.ofInstant(Instant.ofEpochSecond(candles.get(j).getTime().getSeconds()), ZoneId.systemDefault()),
                    toDouble(candles.get(j).getClose())));
        }
        indicators.put("MACD_SIGN", MACD_SIGN);

        // MACD SMA
        int chunkSmaMacdSize = SMA_PERIOD;
        List<Indicator> MACD_SMA = new ArrayList<>();
        for (int i = chunkSmaMacdSize; i < MACD.size(); i++) {
            double[] inClose = new double[chunkSmaMacdSize];
            int k = 0;
            int j = i - chunkSmaMacdSize;
            while (k < chunkSmaMacdSize) {
                inClose[k] = MACD.get(j).getValue();
                k++;
                j++;
            }
            double value = sma(inClose);
            MACD_SMA.add(new Indicator(
                    "MACD_SMA",
                    value,
                    MACD.get(j).getDateTime(),
                    MACD.get(j).getValue()));
        }
        indicators.put("MACD_SMA", MACD_SMA);

        // RSI
        List<Indicator> RSI = new ArrayList<>();
        int chunkRsiSize = RSI_PERIOD + SHIFT_SIZE;
        for (int i = chunkRsiSize; i < candles.size(); i++) {
            double[] inClose = new double[chunkRsiSize];
            int k = 0;
            int j = i - chunkRsiSize;
            while (k < chunkRsiSize) {
                inClose[k] = toDouble(candles.get(j).getClose());
                k++;
                j++;
            }
            double value = rsi(inClose);
            RSI.add(new Indicator(
                    "RSI",
                    value,
                    LocalDateTime.ofInstant(Instant.ofEpochSecond(candles.get(j).getTime().getSeconds()), ZoneId.systemDefault()),
                    toDouble(candles.get(j).getClose())));
        }
        indicators.put("RSI", RSI);

        // RSI SMA
        List<Indicator> RSI_SMA = new ArrayList<>();
        int chunkRsiSmaSize = SMA_PERIOD;
        for (int i = chunkRsiSmaSize; i < RSI.size(); i++) {
            double[] inClose = new double[chunkRsiSmaSize];
            int k = 0;
            int j = i - chunkRsiSmaSize;
            while (k < chunkRsiSmaSize) {
                inClose[k] = RSI.get(j).getValue();
                k++;
                j++;
            }
            double value = sma(inClose);
            RSI_SMA.add(new Indicator(
                    "RSI_SMA",
                    value,
                    RSI.get(j).getDateTime(),
                    RSI.get(j).getValue()));
        }
        indicators.put("RSI_SMA", RSI_SMA);

        // OBV
        List<Indicator> OBV = new ArrayList<>();
        for (int i = OBV_PERIOD; i < candles.size(); i++) {
            double[] inClose = new double[OBV_PERIOD];
            double[] inVolume = new double[OBV_PERIOD];
            int k = 0;
            int j = i - OBV_PERIOD;
            while (k < OBV_PERIOD) {
                inClose[k] = toDouble(candles.get(j).getClose());
                inVolume[k] = candles.get(j).getVolume();
                k++;
                j++;
            }
            double value = obv(inClose, inVolume);
            OBV.add(new Indicator(
                    "OBV",
                    value,
                    LocalDateTime.ofInstant(Instant.ofEpochSecond(candles.get(j).getTime().getSeconds()), ZoneId.systemDefault()),
                    toDouble(candles.get(j).getClose())));
        }
        indicators.put("OBV", OBV);

        // OBV SMA
        List<Indicator> OBV_SMA = new ArrayList<>();
        int chunkObvSmaSize = SMA_PERIOD;
        for (int i = chunkObvSmaSize; i < OBV.size(); i++) {
            double[] inClose = new double[chunkObvSmaSize];
            int k = 0;
            int j = i - chunkRsiSmaSize;
            while (k < chunkRsiSmaSize) {
                inClose[k] = OBV.get(j).getValue();
                k++;
                j++;
            }
            double value = sma(inClose);
            OBV_SMA.add(new Indicator(
                    "OBV_SMA",
                    value,
                    OBV.get(j).getDateTime(),
                    OBV.get(j).getValue()));
        }
        indicators.put("OBV_SMA", OBV_SMA);

        // ADX
        List<Indicator> ADX = new ArrayList<>();
        int chunkAdxSize = ADX_PERIOD + SHIFT_SIZE;
        for (int i = chunkAdxSize; i < candles.size(); i++) {
            double[] inClose = new double[chunkAdxSize];
            double[] inHigh = new double[chunkAdxSize];
            double[] inLow = new double[chunkAdxSize];
            int k = 0;
            int j = i - chunkAdxSize;
            while (k < chunkAdxSize) {
                inClose[k] = toDouble(candles.get(j).getClose());
                inHigh[k] = toDouble(candles.get(j).getHigh());
                inLow[k] = toDouble(candles.get(j).getLow());
                k++;
                j++;
            }
            double value = adx(inClose, inLow, inHigh);
            ADX.add(new Indicator(
                    "ADX",
                    value,
                    LocalDateTime.ofInstant(Instant.ofEpochSecond(candles.get(j).getTime().getSeconds()), ZoneId.systemDefault()),
                    toDouble(candles.get(j).getClose())));
        }
        indicators.put("ADX", ADX);

        return indicators;
    }

    public static double obv(double[] inClose, double[] inVolume) {
        MInteger outBegIdx = new MInteger();
        MInteger outNBElement = new MInteger();
        double[] outReal = new double[inClose.length];
        RetCode retCode = talib.obv(0, inClose.length - 1, inClose, inVolume, outBegIdx, outNBElement, outReal);
        return (RetCode.Success == retCode && outNBElement.value > 0) ? outReal[outNBElement.value - 1] : 0.0;
    }

    public static double macd(double[] inClose) {
        double[] outMACD = new double[inClose.length];
        double[] outMACDSignal = new double[inClose.length];
        double[] outMACDHist = new double[inClose.length];
        MInteger outBegIdx = new MInteger();
        MInteger outNBElement = new MInteger();
        RetCode retCode = talib.macd(
                0, inClose.length - 1, inClose, MACD_FAST_PERIOD, MACD_SLOW_PERIOD, MACD_PERIOD,
                outBegIdx, outNBElement, outMACD, outMACDSignal, outMACDHist
        );
        return (RetCode.Success == retCode && outNBElement.value > 0) ? outMACD[outNBElement.value - 1] : 0.0;
    }

    public static double macdSign(double[] inClose) {
        double[] outMACD = new double[inClose.length];
        double[] outMACDSignal = new double[inClose.length];
        double[] outMACDHist = new double[inClose.length];
        MInteger outBegIdx = new MInteger();
        MInteger outNBElement = new MInteger();
        RetCode retCode = talib.macd(
                0, inClose.length - 1, inClose, MACD_FAST_PERIOD, MACD_SLOW_PERIOD, MACD_PERIOD,
                outBegIdx, outNBElement, outMACD, outMACDSignal, outMACDHist
        );
        return (RetCode.Success == retCode && outNBElement.value > 0) ? outMACDSignal[outNBElement.value - 1] : 0.0;
    }

    public static double rsi(double[] inClose) {
        double[] outReal = new double[inClose.length];
        MInteger outBegIdx = new MInteger();
        MInteger outNBElement = new MInteger();
        RetCode retCodeRSI = talib.rsi(
                0, inClose.length - 1, inClose, RSI_PERIOD,
                outBegIdx, outNBElement, outReal
        );
        return (RetCode.Success == retCodeRSI && outNBElement.value > 0) ? outReal[outNBElement.value - 1] : 0.0;
    }

    public static double adx(double[] inClose, double[] inLow, double[] inHigh) {
        double[] outADX = new double[inClose.length];
        MInteger beginADX = new MInteger();
        MInteger lengthADX = new MInteger();
        RetCode retCodeADX = talib.adx(
                0, inClose.length - 1,
                inHigh, inLow, inClose, ADX_PERIOD,
                beginADX, lengthADX, outADX
        );
        return (RetCode.Success == retCodeADX && lengthADX.value > 0) ? outADX[lengthADX.value - 1] : 0.0;
    }

    public static double movingAverageBlack(double[] inClose) {
        double[] outBlackMA = new double[inClose.length];
        MInteger beginBlackMA = new MInteger();
        MInteger lengthBlackMA = new MInteger();
        RetCode retCodeBlackMA = talib.trima(
                0, inClose.length - 1, inClose, MA_BLACK_PERIOD, beginBlackMA, lengthBlackMA, outBlackMA
        );
        return (RetCode.Success == retCodeBlackMA && lengthBlackMA.value > 0) ? outBlackMA[lengthBlackMA.value - 1] : 0.0;
    }

    public static double movingAverageWhite(double[] inClose) {
        double[] outWhiteMA = new double[inClose.length];
        MInteger beginWhiteMA = new MInteger();
        MInteger lengthWhiteMA = new MInteger();
        RetCode retCodeWhiteMA = talib.trima(
                0, inClose.length - 1, inClose, MA_WHITE_PERIOD, beginWhiteMA, lengthWhiteMA, outWhiteMA
        );
        return (RetCode.Success == retCodeWhiteMA && lengthWhiteMA.value > 0) ? outWhiteMA[lengthWhiteMA.value - 1] : 0.0;
    }

    public static double sma(double[] inClose) {
        double[] outReal = new double[inClose.length];
        MInteger outBegIdx = new MInteger();
        MInteger outNBElement = new MInteger();
        RetCode retCode = talib.sma(
                0, inClose.length - 1, inClose, SMA_PERIOD, outBegIdx, outNBElement, outReal
        );
        return (RetCode.Success == retCode && outNBElement.value > 0) ? outReal[outNBElement.value - 1] : 0.0;
    }

    private static double ema(double[] prices, int period) {
        double[] ema = new double[prices.length];
        MInteger begin = new MInteger();
        MInteger length = new MInteger();
        RetCode retCode = talib.ema(0, prices.length - 1, prices, period, begin, length, ema);
        return (RetCode.Success == retCode && length.value > 0) ? ema[length.value - 1] : 0.0;
    }

    private static double rsi(double[] prices, int period) {
        double[] rsi = new double[prices.length];
        MInteger begin = new MInteger();
        MInteger length = new MInteger();
        RetCode retCode = talib.rsi(0, prices.length - 1, prices, period, begin, length, rsi);
        return (RetCode.Success == retCode && length.value > 0) ? rsi[length.value - 1] : 0.0;
    }

    private static double stochasticCross(double[] closePrices,
                                          double[] minPrices,
                                          double[] maxPrices,
                                          int fastKPeriod, int slowKPeriod, int slowDPeriod) {
        double[] highPrices = new double[closePrices.length];
        double[] lowPrices = new double[closePrices.length];
        for (int i = 0; i < closePrices.length; i++) {
            highPrices[i] = maxPrices[i];
            lowPrices[i] = minPrices[i];
        }

        double[] outK = new double[closePrices.length];
        double[] outD = new double[closePrices.length];
        MInteger begin = new MInteger();
        MInteger length = new MInteger();
        RetCode retCode = talib.stoch(0, closePrices.length - 1, highPrices, lowPrices, closePrices,
                fastKPeriod, slowKPeriod, MAType.Sma, slowDPeriod, MAType.Sma,
                begin, length, outK, outD);
        return RetCode.Success == retCode && length.value > 0
                && outK[length.value - 1] > outD[length.value - 1] && outK[length.value - 2] <= outD[length.value - 2]
                ? 1.0 : 0.0;
    }

    public static double toDouble(Quotation quotation) {
        return toDouble(quotation.getUnits(), quotation.getNano());
    }

    private static double toDouble(long units, int nano) {
        return units + Double.parseDouble("0." + nano);
    }

    public static List<TickerCandle> convertCandles(List<TickerCandle> candles, long newTimeFrame, ChronoUnit unit) {
        List<TickerCandle> newCandles = new ArrayList<>();
        if (candles.isEmpty()) return newCandles;

        double open = 0.0;
        double high = Double.MIN_VALUE;
        double low = Double.MAX_VALUE;
        int volume = 0;

        try {
            var currentTimestamp = ofInstant(dateTimeFormat.parse(candles.get(0).getDate()).toInstant(), systemDefault());
            for (TickerCandle candle : candles) {
                open = 0.0 == open ? candle.getOpen() : open;
                var timestamp = ofInstant(dateTimeFormat.parse(candle.getDate()).toInstant(), systemDefault());
                if (timestamp.isEqual(currentTimestamp.plus(newTimeFrame, unit))) {
                    newCandles.add(new TickerCandle(
                            candle.getSymbol(),
                            dateTimeFormat.format(Date.from(currentTimestamp.atZone(systemDefault()).toInstant())),
                            open,
                            high,
                            low,
                            candle.getClose(),
                            candle.getClose(),
                            volume
                    ));
                } else if (timestamp.isAfter(currentTimestamp.plus(newTimeFrame, unit))) {
                    currentTimestamp = timestamp;//.minusHours(1);
                    open = candle.getOpen();
                    high = candle.getHigh();
                    low = candle.getLow();
                } else {
                    high = Math.max(high, candle.getHigh());
                    low = Math.min(low, candle.getLow());
                    volume += candle.getVolume();
                }
            }
            return newCandles;
        } catch (Exception ex) {
            ex.printStackTrace();
            throw new RuntimeException(ex);
        }
    }
}


