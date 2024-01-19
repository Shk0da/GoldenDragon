package com.github.shk0da.GoldenDragon.strategy;

import com.github.shk0da.GoldenDragon.service.TCSService;
import com.github.shk0da.GoldenDragon.utils.IndicatorsUtil;
import ru.tinkoff.piapi.contract.v1.CandleInterval;
import ru.tinkoff.piapi.contract.v1.HistoricCandle;
import ru.tinkoff.piapi.contract.v1.Share;

import java.time.OffsetDateTime;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.TimeUnit;

import static com.github.shk0da.GoldenDragon.service.TelegramNotifyService.telegramNotifyService;
import static java.lang.System.out;

public class IndicatorTrader {

    private final TCSService tcsService;

    private final Set<String> purchases = new HashSet<>();

    public IndicatorTrader(TCSService tcsService) {
        this.tcsService = tcsService;
    }

    public void run() {
        List<Share> stocks = tcsService.getMoexShares();
        while (true) {
            stocks.forEach(stock -> {
                sleep(300, 450);
                try {
                    if (purchases.contains(stock.getFigi())) {
                        List<HistoricCandle> m15candles = tcsService.getCandles(
                                stock.getFigi(),
                                OffsetDateTime.now().minusHours(24),
                                OffsetDateTime.now(),
                                CandleInterval.CANDLE_INTERVAL_15_MIN
                        );
                        boolean isM15SignalDown = calculateSignalDown(m15candles);
                        if (isM15SignalDown) {
                            purchases.remove(stock.getFigi());
                            telegramNotifyService.sendMessage(stock.getName() + ": SELL");
                        }
                    } else {
                        List<HistoricCandle> m5candles = tcsService.getCandles(
                                stock.getFigi(),
                                OffsetDateTime.now().minusHours(24),
                                OffsetDateTime.now(),
                                CandleInterval.CANDLE_INTERVAL_5_MIN
                        );
                        boolean isM5SignalUp = calculateSignalUp(m5candles, 30.0);
                        if (isM5SignalUp) {
                            List<HistoricCandle> h1candles = tcsService.getCandles(
                                    stock.getFigi(),
                                    OffsetDateTime.now().minusDays(7),
                                    OffsetDateTime.now(),
                                    CandleInterval.CANDLE_INTERVAL_HOUR
                            );
                            boolean isH1SignalUp = calculateSignalUp(h1candles, 20.0);
                            if (isH1SignalUp) {
                                purchases.add(stock.getFigi());
                                telegramNotifyService.sendMessage(stock.getName() + ": BUY");
                            }
                        }
                    }
                } catch (Exception ex) {
                    out.println(stock.getName() + " Error: " + ex.getMessage());
                }
            });
        }
    }

    public static boolean calculateSignalDown(List<HistoricCandle> candles) {
        if (candles.size() < 30) return false;
        var indicators = IndicatorsUtil.initializeIndicators(candles);

        var maWhite = indicators.get("MAWhite");
        boolean maTrendDown = maWhite.get(maWhite.size() - 1).getClose() < maWhite.get(maWhite.size() - 1).getValue();

        var rsi = indicators.get("RSI");
        var rsiSma = indicators.get("RSI_SMA");
        boolean rsiTrendDown = rsi.get(rsi.size() - 1).getValue() < rsi.get(rsi.size() - 2).getValue();
        boolean rsiCrossoverSma = rsi.get(rsi.size() - 1).getValue() < rsiSma.get(rsiSma.size() - 1).getValue();

        var obv = indicators.get("OBV");
        var obvSma = indicators.get("OBV_SMA");
        boolean obvTrendDown = obv.get(obv.size() - 1).getValue() < obv.get(obv.size() - 2).getValue();
        boolean obvCrossoverSma = obv.get(obv.size() - 1).getValue() < obvSma.get(obvSma.size() - 1).getValue();

        return maTrendDown && rsiTrendDown && rsiCrossoverSma && obvTrendDown && obvCrossoverSma;
    }

    public static boolean calculateSignalUp(List<HistoricCandle> candles, double adxLevel) {
        if (candles.size() < 80) return false;

        var indicators = IndicatorsUtil.initializeIndicators(candles);

        var maWhite = indicators.get("MAWhite");
        var maBlack = indicators.get("MABlack");
        boolean maSuperTrendUp =
                maWhite.get(maWhite.size() - 3).getValue() < maWhite.get(maWhite.size() - 2).getValue() &&
                        maWhite.get(maWhite.size() - 2).getValue() < maWhite.get(maWhite.size() - 1).getValue();
        boolean maWhiteUpperBlack = maWhite.get(maWhite.size() - 1).getValue() > maBlack.get(maBlack.size() - 1).getValue();
        boolean closeCrossoverMa = maWhite.get(maWhite.size() - 1).getClose() > maWhite.get(maWhite.size() - 1).getValue();

        var macd = indicators.get("MACD");
        var macdSign = indicators.get("MACD_SIGN");
        boolean macdSuperTrendUp =
                macdSign.get(macdSign.size() - 3).getValue() < macdSign.get(macdSign.size() - 2).getValue() &&
                        macdSign.get(macdSign.size() - 2).getValue() < macdSign.get(macdSign.size() - 1).getValue();
        boolean macdUpperZero = macd.get(macd.size() - 1).getValue() > 0.000;
        boolean macdCrossoverSignal = macd.get(macd.size() - 1).getValue() >= macdSign.get(macdSign.size() - 1).getValue();

        var rsi = indicators.get("RSI");
        var rsiSma = indicators.get("RSI_SMA");
        boolean rsiSuperTrendUp =
                rsiSma.get(rsiSma.size() - 3).getValue() < rsiSma.get(rsiSma.size() - 2).getValue() &&
                        rsiSma.get(rsiSma.size() - 2).getValue() < rsiSma.get(rsiSma.size() - 1).getValue();
        boolean rsiUpper50 = rsi.get(rsi.size() - 1).getValue() >= 50.0;
        boolean rsiCrossoverSma = rsi.get(rsi.size() - 1).getValue() > rsiSma.get(rsiSma.size() - 1).getValue();

        var obv = indicators.get("OBV");
        var obvSma = indicators.get("OBV_SMA");
        boolean obvSuperTrendUp =
                obvSma.get(obvSma.size() - 3).getValue() < obvSma.get(obvSma.size() - 2).getValue() &&
                        obvSma.get(obvSma.size() - 2).getValue() < obvSma.get(obvSma.size() - 1).getValue();
        boolean obvUpperZero = obv.get(obv.size() - 1).getValue() > 0.0;
        boolean obvCrossoverSma = obv.get(obv.size() - 1).getValue() > obvSma.get(obvSma.size() - 1).getValue();

        var adx = indicators.get("ADX");
        boolean adxUpper30 = adx.get(adx.size() - 1).getValue() >= adxLevel;

        return maSuperTrendUp && maWhiteUpperBlack && closeCrossoverMa &&
                macdSuperTrendUp && macdUpperZero && macdCrossoverSignal &&
                rsiSuperTrendUp && rsiUpper50 && rsiCrossoverSma &&
                obvSuperTrendUp && obvUpperZero && obvCrossoverSma &&
                adxUpper30;
    }

    private static void sleep(int start, int end) {
        try {
            TimeUnit.MILLISECONDS.sleep(ThreadLocalRandom.current().nextInt(start, end));
        } catch (InterruptedException ex) {
            out.println("Error: " + ex.getMessage());
        }
    }
}
