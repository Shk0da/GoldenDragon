package com.github.shk0da.GoldenDragon.strategy;

import com.github.shk0da.GoldenDragon.config.MainConfig;
import com.github.shk0da.GoldenDragon.config.MarketConfig;
import com.github.shk0da.GoldenDragon.config.RSXConfig;
import com.github.shk0da.GoldenDragon.model.PortfolioPosition;
import com.github.shk0da.GoldenDragon.model.TickerCandle;
import com.github.shk0da.GoldenDragon.model.TickerInfo;
import com.github.shk0da.GoldenDragon.model.TickerScan;
import com.github.shk0da.GoldenDragon.model.TickerType;
import com.github.shk0da.GoldenDragon.repository.Repository;
import com.github.shk0da.GoldenDragon.repository.TickerRepository;
import com.github.shk0da.GoldenDragon.service.TCSService;
import com.github.shk0da.GoldenDragon.service.TradingViewService;
import com.github.shk0da.GoldenDragon.service.YahooService;
import com.google.gson.reflect.TypeToken;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

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
    private final YahooService yahooService = YahooService.INSTANCE;
    private final TradingViewService tradingViewService = TradingViewService.INSTANCE;

    private final Repository<TickerInfo.Key, TickerInfo> tickerRepository = TickerRepository.INSTANCE;

    public RSX(MainConfig mainConfig, MarketConfig marketConfig, RSXConfig rsxConfig, TCSService tcsService) {
        super(mainConfig, marketConfig, tcsService);
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
        Map<TickerInfo.Key, PortfolioPosition> targetPositions = topSymbols(tickers)
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
        Map<TickerInfo.Key, PortfolioPosition> positionsToSave = doRebalance(availableCash, previousPositions, targetPositions, 1.0);
        if (!positionsToSave.isEmpty()) {
            saveDataToDisk(serializeName, positionsToSave);
        }
    }

    public boolean isTrendUp() {
        List<TickerCandle> trendCandles50 = yahooService.getLastCandles(rsxConfig.getTrendStock(), 50);
        List<TickerCandle> trendCandles5 = trendCandles50.stream().skip(45).collect(toList());
        double longSMA = trendCandles50.stream().mapToDouble(TickerCandle::getClose).sum() / trendCandles50.size();
        double shortSMA = trendCandles5.stream().mapToDouble(TickerCandle::getClose).sum() / trendCandles5.size();
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

            var debtEquityRatio = yahooService.getLongTermDebtEquityAndTotalDebtEquityRatio(ticker);
            data.put("longTermDebtEquityRatio", debtEquityRatio[0]);
            data.put("totalDebtEquityRatio", debtEquityRatio[1]);
            dataForFiler.add(data);
        }

        List<Map<String, Object>> stocks = dataForFiler.stream()
                .filter(it -> (double) it.get("longTermDebtEquityRatio") != 0.0)
                .filter(it -> (double) it.get("totalDebtEquityRatio") != 0.0)
                .sorted((it1, it2) -> ((Double) it2.get("recommend1M")).compareTo((Double) it1.get("recommend1M")))
                .limit(100)
                .sorted(Comparator.comparingDouble(it -> ((Double) it.get("debtToEquity"))))
                .limit(50)
                .sorted(Comparator.comparingDouble(it -> ((Double) it.get("longTermDebtEquityRatio"))))
                .skip(dataForFiler.size() >= 25 ? 5 : 0)
                .sorted(Comparator.comparingDouble(it -> ((Double) it.get("totalDebtEquityRatio"))))
                .limit(20)
                .collect(toList());

        stocks.forEach(stock -> {
            var candles = yahooService.getLastCandles((String) stock.get("symbol"), 128);
            if (!candles.isEmpty() && candles.size() >= 2) {
                var topCandles = candles.stream().skip(candles.size() - 2).collect(toList());
                var overall = ((candles.get(candles.size() - 1).getClose() - candles.get(0).getClose()) / candles.get(0).getClose()) * 100;
                var recent = ((topCandles.get(topCandles.size() - 1).getClose() - topCandles.get(0).getClose()) / topCandles.get(0).getClose()) * 100;
                var momentum = overall - recent;
                stock.put("momentum", momentum);
            } else {
                stock.put("momentum", 0.0);
            }
        });

        // topQualityMomentum
        return stocks.stream()
                .sorted((it1, it2) -> ((Double) it2.get("momentum")).compareTo((Double) it1.get("momentum")))
                .filter(it -> (double) it.get("momentum") > 0.0)
                .limit(rsxConfig.getStockPortfolioMaxSize())
                .map(it -> (String) it.get("symbol"))
                .collect(Collectors.toList());
    }
}
