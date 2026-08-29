package com.github.shk0da.goldendragon.market;

import com.github.shk0da.goldendragon.model.Candle;
import com.github.shk0da.goldendragon.model.TickerInfo;
import com.github.shk0da.goldendragon.repository.TickerRepository;
import com.github.shk0da.goldendragon.service.TCSService;

import java.sql.Timestamp;
import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import ru.tinkoff.piapi.contract.v1.CandleInterval;
import ru.tinkoff.piapi.contract.v1.HistoricCandle;
import ru.tinkoff.piapi.contract.v1.Quotation;

/**
 * Live market data provider using TCS API.
 * Fetches real-time data from the Tinkoff Invest API.
 */
public class LiveMarketDataProvider implements MarketDataProvider {

    private final TCSService tcsService;
    private final TickerRepository tickerRepository;
    private static final ThreadLocal<java.text.SimpleDateFormat> CANDLE_TIME_FORMAT =
            ThreadLocal.withInitial(() -> new java.text.SimpleDateFormat("dd.MM.yyyy HH:mm:ss"));

    public LiveMarketDataProvider(TCSService tcsService) {
        this.tcsService = tcsService;
        this.tickerRepository = TickerRepository.INSTANCE;
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
            start = now.minusDays(60);
        } else if ("5_MIN".equals(interval)) {
            start = now.minusHours(72);
        } else {
            return Collections.emptyList();
        }

        try {
            List<HistoricCandle> historicCandles = tcsService.getCandles(figi, start, now,
                    "HOUR".equals(interval)
                            ? CandleInterval.CANDLE_INTERVAL_HOUR
                            : CandleInterval.CANDLE_INTERVAL_5_MIN);

            List<Candle> candles = new ArrayList<>();
            for (HistoricCandle hc : historicCandles) {
                Timestamp ts = new Timestamp(hc.getTime().getSeconds() * 1000);
                candles.add(new Candle(
                        CANDLE_TIME_FORMAT.get().format(ts),
                        toDouble(hc.getOpen()),
                        toDouble(hc.getHigh()),
                        toDouble(hc.getLow()),
                        toDouble(hc.getClose()),
                        hc.getVolume()
                ));
            }
            return candles;
        } catch (Exception e) {
            return Collections.emptyList();
        }
    }

    private static double toDouble(Quotation quotation) {
        return toDouble(quotation.getUnits(), quotation.getNano());
    }

    private static double toDouble(long units, int nano) {
        return units + nano / 1_000_000_000.0;
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
