package com.github.shk0da.goldendragon.market;

import com.github.shk0da.goldendragon.model.Candle;
import com.github.shk0da.goldendragon.model.TickerInfo;
import com.github.shk0da.goldendragon.repository.TickerRepository;
import com.github.shk0da.goldendragon.service.TradingService;
import com.github.shk0da.goldendragon.utils.PropertiesUtils;

import java.time.OffsetDateTime;
import java.util.Collections;
import java.util.List;
import java.util.Properties;

/**
 * Live market data provider using TCS API.
 * Fetches real-time data from the Tinkoff Invest API.
 */
public class LiveMarketDataProvider implements MarketDataProvider {

    private static final int DEFAULT_HOUR_LOOKBACK_DAYS = 60;
    private static final int DEFAULT_MINUTE_LOOKBACK_HOURS = 72;

    private final TradingService tcsService;
    private final TickerRepository tickerRepository;
    private final int hourLookbackDays;
    private final int minuteLookbackHours;

    public LiveMarketDataProvider(TradingService tcsService) {
        this.tcsService = tcsService;
        this.tickerRepository = TickerRepository.INSTANCE;
        int hourDays = DEFAULT_HOUR_LOOKBACK_DAYS;
        int minuteHours = DEFAULT_MINUTE_LOOKBACK_HOURS;
        try {
            Properties properties = PropertiesUtils.loadProperties();
            hourDays = Integer.parseInt(properties.getProperty(
                    "unifiedTrader.live.hourLookbackDays",
                    String.valueOf(DEFAULT_HOUR_LOOKBACK_DAYS)));
            minuteHours = Integer.parseInt(properties.getProperty(
                    "unifiedTrader.live.minuteLookbackHours",
                    String.valueOf(DEFAULT_MINUTE_LOOKBACK_HOURS)));
        } catch (Exception ignored) {
            // fall back to defaults when config is unavailable
        }
        this.hourLookbackDays = hourDays;
        this.minuteLookbackHours = minuteHours;
    }

    @Override
    public List<Candle> getCandles(String ticker, String interval) {
        TickerInfo info = tickerRepository.getByName(ticker);
        if (info == null) {
            return Collections.emptyList();
        }

        String figi = info.getFigi();
        OffsetDateTime now = OffsetDateTime.now();
        OffsetDateTime start;

        if ("HOUR".equals(interval)) {
            start = now.minusDays(hourLookbackDays);
        } else if ("5_MIN".equals(interval)) {
            start = now.minusHours(minuteLookbackHours);
        } else {
            return Collections.emptyList();
        }

        try {
            return tcsService.getCandles(figi, start, now, interval);
        } catch (Exception e) {
            return Collections.emptyList();
        }
    }

    @Override
    public MarketPrices getLivePrices(String ticker) {
        TickerInfo info = tickerRepository.getByName(ticker);
        if (info == null) {
            return new MarketPrices(null, null);
        }

        try {
            TickerInfo.Key key = new TickerInfo.Key(ticker, info.getType());
            double ask = tcsService.getLiveAskPrice(key);
            double bid = tcsService.getLiveBidPrice(key);
            if (ask <= 0 || bid <= 0) {
                return new MarketPrices(null, null);
            }
            return new MarketPrices(bid, ask);
        } catch (Exception e) {
            return new MarketPrices(null, null);
        }
    }
}
