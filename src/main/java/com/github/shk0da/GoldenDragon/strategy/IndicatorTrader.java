package com.github.shk0da.GoldenDragon.strategy;

import com.github.shk0da.GoldenDragon.service.TCSService;
import com.github.shk0da.GoldenDragon.utils.IndicatorsUtil;
import ru.tinkoff.piapi.contract.v1.CandleInterval;
import ru.tinkoff.piapi.contract.v1.GetOrderBookResponse;
import ru.tinkoff.piapi.contract.v1.HistoricCandle;
import ru.tinkoff.piapi.contract.v1.Order;
import ru.tinkoff.piapi.contract.v1.Share;

import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.TimeUnit;

import static com.github.shk0da.GoldenDragon.service.TelegramNotifyService.telegramNotifyService;
import static com.github.shk0da.GoldenDragon.utils.IndicatorsUtil.toDouble;
import static java.lang.System.out;
import static java.time.OffsetDateTime.now;
import static java.util.Collections.max;
import static java.util.stream.Collectors.toList;

public class IndicatorTrader {

    private final TCSService tcsService;

    private final Set<String> purchases = new HashSet<>();
    private final Map<String, OffsetDateTime> anomalies = new HashMap<>();

    public IndicatorTrader(TCSService tcsService) {
        this.tcsService = tcsService;
    }

    public void run() {
        List<Share> stocks = tcsService.getMoexShares();
        while (true) {
            stocks.forEach(stock -> {

                boolean hasNotActiveDetectAnomaly = null == anomalies.get(stock.getFigi()) || anomalies.get(stock.getFigi()).isBefore(now().minusMinutes(5));
                if (hasNotActiveDetectAnomaly) {
                    var orderBook = tcsService.getOrderBook(stock.getFigi(), 10);
                    boolean hasGlassAnomaly = hasGlassAnomaly(orderBook);
                    if (hasGlassAnomaly) {
                        telegramNotifyService.sendMessage(stock.getName() + ": ANOMALY");
                        anomalies.put(stock.getFigi(), now());
                    }
                }

                sleep(300, 450);
                try {
                    if (purchases.contains(stock.getFigi())) {
                        List<HistoricCandle> m15candles = tcsService.getCandles(
                                stock.getFigi(),
                                now().minusHours(24),
                                now(),
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
                                now().minusHours(24),
                                now(),
                                CandleInterval.CANDLE_INTERVAL_5_MIN
                        );
                        boolean isM5SignalUp = calculateSignalUp(m5candles, 30.0);
                        if (isM5SignalUp) {
                            List<HistoricCandle> h1candles = tcsService.getCandles(
                                    stock.getFigi(),
                                    now().minusDays(7),
                                    now(),
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

    private boolean hasGlassAnomaly(GetOrderBookResponse orderBook) {
        if (orderBook.getAsksCount() == 0 || orderBook.getBidsCount() == 0) {
            return false;
        }

        int result = 0;

        // Покупка
        Double maxAsk = max(orderBook.getAsksList().stream().map(it -> toDouble(it.getPrice())).collect(toList()));
        // Продажа
        Double maxBid = max(orderBook.getBidsList().stream().map(it -> toDouble(it.getPrice())).collect(toList()));

        // Spread-Based Strategy
        var spread = maxAsk - maxBid;
        var spreadThreshold = (maxAsk / 100) * 0.05;
        if (Math.abs(spread) < Math.abs(spreadThreshold)) {
            result++;
        }

        // Volume-Based Strategy
        var totalBidVolume = 0.0;
        for (Order order : orderBook.getBidsList()) {
            totalBidVolume += order.getQuantity();
        }

        var totalAskVolume = 0.0;
        for (Order order : orderBook.getAsksList()) {
            totalAskVolume += order.getQuantity();
        }

        var meanBidVolume = totalBidVolume / orderBook.getBidsList().size();
        var meanAskVolume = totalAskVolume / orderBook.getAsksList().size();
        var meanVolume = (meanBidVolume + meanAskVolume) / 2;

        double squaredDifferencesSum = 0.0;

        var allOrders = new ArrayList<Order>();
        allOrders.addAll(orderBook.getBidsList());
        allOrders.addAll(orderBook.getAsksList());
        for (Order order : allOrders) {
            squaredDifferencesSum += Math.pow(order.getQuantity() - meanVolume, 2);
        }
        var stdVolume = Math.sqrt(squaredDifferencesSum / allOrders.size());

        double threshold = ((meanBidVolume + meanAskVolume) / 2) + 5 * stdVolume;
        for (Order order : allOrders) {
            if (order.getQuantity() > threshold) {
                result++;
                break;
            }
        }

        // Side Dominance Strategy
        var totalVolume = totalBidVolume + totalAskVolume;
        var bidDominance = totalBidVolume / totalVolume;
        if (bidDominance > threshold || (1 - bidDominance) > threshold) {
            result++;
        }

        return result >= 2;
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
