package com.github.shk0da.GoldenDragon.strategy;

import com.github.shk0da.GoldenDragon.config.MainConfig;
import com.github.shk0da.GoldenDragon.config.MarketConfig;
import com.github.shk0da.GoldenDragon.config.RSXConfig;
import com.github.shk0da.GoldenDragon.model.PortfolioPosition;
import com.github.shk0da.GoldenDragon.model.TickerInfo;
import com.github.shk0da.GoldenDragon.model.TickerScan;
import com.github.shk0da.GoldenDragon.model.TickerType;
import com.github.shk0da.GoldenDragon.repository.Repository;
import com.github.shk0da.GoldenDragon.repository.TickerRepository;
import com.github.shk0da.GoldenDragon.service.TCSService;
import com.github.shk0da.GoldenDragon.service.TradingViewService;
import com.google.gson.reflect.TypeToken;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;
import ru.tinkoff.piapi.contract.v1.HistoricCandle;


import static com.github.shk0da.GoldenDragon.utils.SerializationUtils.loadDataFromDisk;
import static com.github.shk0da.GoldenDragon.utils.SerializationUtils.saveDataToDisk;
import static java.lang.System.out;
import static java.util.stream.Collectors.toList;

/**
 * CRON SPB: Every Mon at 17:00 MSK
 */
public class RSX extends Rebalancing {

    private final MarketConfig marketConfig;
    private final RSXConfig rsxConfig;
    private final String serializeName;

    private final TCSService tcsService;
    private final TradingViewService tradingViewService = TradingViewService.INSTANCE;

    private final Repository<TickerInfo.Key, TickerInfo> tickerRepository = TickerRepository.INSTANCE;

    public RSX(MainConfig mainConfig, MarketConfig marketConfig, RSXConfig rsxConfig, TCSService tcsService) {
        super(marketConfig, tcsService);
        this.marketConfig = marketConfig;
        this.rsxConfig = rsxConfig;
        this.tcsService = tcsService;
        this.serializeName = mainConfig.getTcsAccountId() + "_" + RSXConfig.SERIALIZE_NAME;
    }

    public void run() throws Exception {
        boolean isTrendUp = isTrendUp();
        double availableCash = tcsService.getAvailableCash();
        Map<TickerInfo.Key, PortfolioPosition> previousPositions = loadDataFromDisk(serializeName, new TypeToken<>() {});
        List<TickerScan> tickers = tradingViewService.scanMarket(marketConfig.getMarket(), 200);
        List<String> topSymbols = topSymbols(tickers);
        Map<TickerInfo.Key, PortfolioPosition> targetPositions = topSymbols
                .stream()
                .map(it -> {
                    var pos = new PortfolioPosition(it, TickerType.STOCK, 100.0 / rsxConfig.getStockPortfolioMaxSize());
                    var key = new TickerInfo.Key(pos.getName(), pos.getType());
                    if (!isTrendUp && null != previousPositions && !previousPositions.isEmpty() && previousPositions.containsKey(key)) {
                        return previousPositions.get(key);
                    }
                    if (isTrendUp) {
                        return pos;
                    }
                    return null;
                })
                .filter(Objects::nonNull)
                .collect(Collectors.toMap(it -> new TickerInfo.Key(it.getName(), it.getType()), it -> it));
        if (targetPositions.isEmpty()) {
            out.println("There are no suitable conditions for trading...");
        }

        Map<TickerInfo.Key, PortfolioPosition> positionsToSave = doRebalance(availableCash, targetPositions, 1.0);
        if (!positionsToSave.isEmpty()) {
            saveDataToDisk(serializeName, positionsToSave);
        }
    }

    public boolean isTrendUp() {
        List<HistoricCandle> trendCandles50 = tcsService.getLastCandles(rsxConfig.getTrendStock(), TickerType.ETF, 50);
        if (trendCandles50.isEmpty()) {
            out.println("trendUp: false (no candles)");
            return false;
        }
        List<HistoricCandle> trendCandles5 = trendCandles50.stream()
                .skip(Math.max(0, trendCandles50.size() - 5))
                .collect(toList());
        if (trendCandles5.isEmpty()) {
            out.println("trendUp: false (not enough candles)");
            return false;
        }
        double longSMA = trendCandles50.stream().mapToDouble(c -> tcsService.convertQuotationToDouble(c.getClose())).sum() / trendCandles50.size();
        double shortSMA = trendCandles5.stream().mapToDouble(c -> tcsService.convertQuotationToDouble(c.getClose())).sum() / trendCandles5.size();
        boolean isTrendUp = shortSMA > longSMA;
        out.println("trendUp: " + isTrendUp);
        return isTrendUp;
    }

    public List<String> topSymbols(List<TickerScan> tickers) {
        List<Map<String, Object>> dataForFiler = new ArrayList<>(tickers.size());
        for (TickerScan ticker : tickers) {
            if (ticker.getDebtToEquity() == 0.0) continue;
            if (!tickerRepository.containsKey(new TickerInfo.Key(ticker.getName(), TickerType.STOCK))) continue;
            Map<String, Object> data = new HashMap<>();
            data.put("symbol", ticker.getName());
            data.put("recommend1M", ticker.getRecommend1M());
            data.put("debtToEquity", ticker.getDebtToEquity());
            dataForFiler.add(data);
        }

        List<Map<String, Object>> stocks = dataForFiler.stream()
                .sorted((it1, it2) -> ((Double) it2.get("recommend1M")).compareTo((Double) it1.get("recommend1M")))
                .limit(100)
                .sorted(Comparator.comparingDouble(it -> ((Double) it.get("debtToEquity"))))
                .limit(50)
                .collect(toList());

        stocks.forEach(stock -> {
            List<HistoricCandle> candles = tcsService.getLastCandles((String) stock.get("symbol"), TickerType.STOCK, 128);
            if (!candles.isEmpty() && candles.size() >= 2) {
                var topCandles = candles.stream().skip(candles.size() - 2).collect(toList());
                double lastClose = tcsService.convertQuotationToDouble(candles.get(candles.size() - 1).getClose());
                double firstClose = tcsService.convertQuotationToDouble(candles.get(0).getClose());
                double topLastClose = tcsService.convertQuotationToDouble(topCandles.get(topCandles.size() - 1).getClose());
                double topFirstClose = tcsService.convertQuotationToDouble(topCandles.get(0).getClose());
                var overall = ((lastClose - firstClose) / firstClose) * 100;
                var recent = ((topLastClose - topFirstClose) / topFirstClose) * 100;
                var momentum = overall - recent;
                stock.put("momentum", momentum);
            } else {
                stock.put("momentum", 0.0);
            }
        });

        return stocks.stream()
                .sorted((it1, it2) -> ((Double) it2.get("momentum")).compareTo((Double) it1.get("momentum")))
                .filter(it -> (double) it.get("momentum") > 0.0)
                .limit(rsxConfig.getStockPortfolioMaxSize())
                .map(it -> (String) it.get("symbol"))
                .collect(Collectors.toList());
    }
}
