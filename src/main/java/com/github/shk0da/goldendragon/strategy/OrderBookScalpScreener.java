package com.github.shk0da.goldendragon.strategy;

import com.github.shk0da.goldendragon.config.OrderBookScalpConfig;
import com.github.shk0da.goldendragon.model.MarketTradeTick;
import com.github.shk0da.goldendragon.model.TickerInfo;
import com.github.shk0da.goldendragon.model.TickerType;
import com.github.shk0da.goldendragon.service.TradingService;
import com.github.shk0da.goldendragon.service.TradingService.TradingServiceType;
import com.github.shk0da.goldendragon.utils.LoggingUtils;

import java.time.Instant;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

import static com.github.shk0da.goldendragon.utils.TimeUtils.sleep;
import static java.util.Comparator.comparing;
import static java.util.Comparator.comparingDouble;
import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.toList;

/** Ranks futures by spread, depth and recent trade activity for order-book scalping. */
public final class OrderBookScalpScreener {

    private static final Pattern DATED_TICKER_SUFFIX = Pattern.compile(".*[FGHJKMNQUVXZ]\\d$");

    private static final Set<String> CORE_MOEX_PERPETUALS =
            Set.of(
                    "USDRUBF", "EURRUBF", "CNYRUBF", "SBERF", "GAZPF", "IMOEXF", "GLDRUBF", "LKOHF",
                    "VTBRF", "ROSNF", "GMKNF", "TATNF", "NVTKF", "YDEXF");

    private static final Set<String> CORE_COMMODITY_ASSETS =
            Set.of(
                    "BRENT", "BR", "BM", "WHEAT", "W4", "WU", "NG", "NATGAS", "GAS", "GOLD", "GL",
                    "SILVER", "SV", "COPPER", "CU", "SUGAR", "COFFEE");

    // First-echelon MOEX stocks suitable for order-book scalping
    private static final Set<String> CORE_STOCKS =
            Set.of(
                    "SBER", "GAZP", "GMKN", "VTBR", "LKOH", "ROSN",
                    "YDEX", "MGNT", "PLZL", "MTSS", "TATN", "NVTK",
                    "SNGS", "SNGSP", "CHMF", "NLMK", "ALRS", "RUAL",
                    "MOEX", "AFLT", "IRAO", "HYDR", "VKCO", "OZON",
                    "X5", "FIVE", "CBOM", "MAGN");

    // Core ByBit crypto pairs suitable for order-book scalping (USDT perpetuals)
    private static final Set<String> CORE_CRYPTO =
            Set.of(
                    "BTCUSDT", "ETHUSDT", "SOLUSDT", "BNBUSDT",
                    "XRPUSDT", "DOGEUSDT", "ADAUSDT", "AVAXUSDT",
                    "DOTUSDT", "POLUSDT", "LINKUSDT", "LTCUSDT",
                    "UNIUSDT", "ATOMUSDT", "ETCUSDT", "FILUSDT");

    private OrderBookScalpScreener() {}

    public static List<TickerInfo> selectTop(
            TradingService tradingService,
            List<TickerInfo> candidates,
            OrderBookScalpConfig config,
            double availableCash) {
        // Crypto mode: screen only crypto pairs for ByBit
        if (tradingService.getServiceType() == TradingServiceType.BYBIT) {
            return selectCryptoTop(tradingService, candidates, config, availableCash);
        }

        List<TickerInfo> perpetuals =
                candidates.stream()
                        .filter(info -> isPerpetualCandidate(info.getTicker()))
                        .collect(toList());
        List<TickerInfo> datedCommodities =
                candidates.stream()
                        .filter(OrderBookScalpScreener::isCommodityDatedFuture)
                        .collect(toList());
        // Filter stocks for scalping
        List<TickerInfo> stocks =
                candidates.stream()
                        .filter(info -> isStockCandidate(info))
                        .collect(toList());

        List<ScoredTicker> scored = new ArrayList<>();
        scored.addAll(pickNearestLiquidDated(tradingService, datedCommodities, config, availableCash));

        // Score perpetual futures
        for (TickerInfo info : perpetuals) {
            try {
                ScoredTicker ranked = scoreTicker(tradingService, info, config, availableCash);
                if (ranked != null) {
                    scored.add(ranked);
                }
            } catch (Exception ex) {
                LoggingUtils.log("Screen skip " + info.getTicker() + ": " + ex.getMessage());
            }
            sleep(120);
        }

        // Score stocks
        for (TickerInfo info : stocks) {
            try {
                ScoredTicker ranked = scoreTicker(tradingService, info, config, availableCash);
                if (ranked != null) {
                    scored.add(ranked);
                }
            } catch (Exception ex) {
                LoggingUtils.log("Screen skip stock " + info.getTicker() + ": " + ex.getMessage());
            }
            sleep(120);
        }

        List<ScoredTicker> selected =
                scored.stream()
                        .sorted(comparingDouble(ScoredTicker::score).reversed())
                        .limit(config.getScreeningTopN())
                        .collect(toList());

        LoggingUtils.log(
                "Screening: candidates="
                        + candidates.size()
                        + ", perpetuals="
                        + perpetuals.size()
                        + ", datedCommodities="
                        + datedCommodities.size()
                        + ", stocks="
                        + stocks.size()
                        + ", ranked="
                        + scored.size()
                        + ", selected="
                        + selected.size());
        for (ScoredTicker ranked : selected.stream().limit(10).collect(toList())) {
            LoggingUtils.log(
                    "Screen pick "
                            + ranked.info().getTicker()
                            + " ("
                            + ranked.assetLabel()
                            + "): score="
                            + String.format("%.0f", ranked.score())
                            + " spreadBps="
                            + String.format("%.2f", ranked.spreadBps())
                            + " topDepth="
                            + ranked.topDepth()
                            + " bookDepth="
                            + ranked.bookDepth()
                            + " flow="
                            + String.format("%.0f", ranked.tradeFlow()));
        }
        return selected.stream().map(ScoredTicker::info).collect(toList());
    }

    /**
     * MOEX perpetual futures suitable for order-book scalping.
     *
     * <p>Excludes US equity perps (*perpA) — they often have thin books on MOEX compared to
     * currency and Russian stock perpetuals (*F, *RUBF).
     */
    static boolean isPerpetualCandidate(String ticker) {
        if (ticker == null || ticker.isEmpty()) {
            return false;
        }
        String normalized = ticker.toUpperCase();
        if (normalized.contains("PERP")) {
            return false;
        }
        if (normalized.endsWith("RUBF")) {
            return true;
        }
        return normalized.endsWith("F") && !isDatedTicker(normalized);
    }

    static boolean isCommodityDatedFuture(TickerInfo info) {
        if (info == null || isPerpetualCandidate(info.getTicker())) {
            return false;
        }
        if (!isDatedTicker(info.getTicker())) {
            return false;
        }
        if ("commodity".equalsIgnoreCase(info.getAssetType())) {
            return true;
        }
        String assetKey = assetGroupKey(info);
        return CORE_COMMODITY_ASSETS.stream().anyMatch(assetKey::contains);
    }

    /**
     * Check if ticker is a stock suitable for order-book scalping.
     * Includes first-echelon MOEX stocks with good liquidity.
     */
    static boolean isStockCandidate(TickerInfo info) {
        if (info == null) {
            return false;
        }
        // Must be a stock type
        if (info.getType() != TickerType.STOCK) {
            return false;
        }
        String ticker = info.getTicker();
        if (ticker == null || ticker.isEmpty()) {
            return false;
        }
        // Check if it's in our core stocks list
        return CORE_STOCKS.contains(ticker.toUpperCase());
    }

    /**
     * Check if ticker is a crypto pair suitable for ByBit scalping.
     * USDT perpetuals follow pattern: XXXXXUSDT (e.g., BTCUSDT, ETHUSDT).
     */
    static boolean isCryptoCandidate(TickerInfo info) {
        if (info == null) {
            return false;
        }
        String ticker = info.getTicker();
        if (ticker == null || ticker.isEmpty()) {
            return false;
        }
        String upper = ticker.toUpperCase();
        if (upper.endsWith("USDT")) {
            return true;
        }
        return false;
    }

    /**
     * Check if ticker is in the core crypto list.
     */
    static boolean isCoreCrypto(String ticker) {
        if (ticker == null || ticker.isEmpty()) {
            return false;
        }
        return CORE_CRYPTO.contains(ticker.toUpperCase());
    }

    /**
     * Select top crypto pairs for ByBit scalping.
     * Filters for core crypto pairs and ranks by liquidity.
     */
    private static List<TickerInfo> selectCryptoTop(
            TradingService tradingService,
            List<TickerInfo> candidates,
            OrderBookScalpConfig config,
            double availableCash) {
        List<TickerInfo> crypto =
                candidates.stream()
                        .filter(info -> isCryptoCandidate(info))
                        .filter(info -> isCoreCrypto(info.getTicker()))
                        .collect(toList());

        List<ScoredTicker> scored = new ArrayList<>();
        for (TickerInfo info : crypto) {
            try {
                ScoredTicker ranked = scoreTicker(tradingService, info, config, availableCash);
                if (ranked != null) {
                    scored.add(ranked);
                }
            } catch (Exception ex) {
                LoggingUtils.log("Screen skip crypto " + info.getTicker() + ": " + ex.getMessage());
            }
            sleep(120);
        }

        List<ScoredTicker> selected =
                scored.stream()
                        .sorted(comparingDouble(ScoredTicker::score).reversed())
                        .limit(config.getScreeningTopN())
                        .collect(toList());

        LoggingUtils.log(
                "Crypto screening: candidates="
                        + candidates.size()
                        + ", crypto="
                        + crypto.size()
                        + ", ranked="
                        + scored.size()
                        + ", selected="
                        + selected.size());
        for (ScoredTicker ranked : selected.stream().limit(10).collect(toList())) {
            LoggingUtils.log(
                    "Crypto pick "
                            + ranked.info().getTicker()
                            + ": score="
                            + String.format("%.0f", ranked.score())
                            + " spreadBps="
                            + String.format("%.2f", ranked.spreadBps())
                            + " topDepth="
                            + ranked.topDepth()
                            + " bookDepth="
                            + ranked.bookDepth()
                            + " flow="
                            + String.format("%.0f", ranked.tradeFlow()));
        }
        return selected.stream().map(ScoredTicker::info).collect(toList());
    }

    private static List<ScoredTicker> pickNearestLiquidDated(
            TradingService tradingService,
            List<TickerInfo> datedCommodities,
            OrderBookScalpConfig config,
            double availableCash) {
        Instant now = Instant.now();
        Map<String, List<TickerInfo>> byAsset =
                datedCommodities.stream()
                        .filter(
                                info ->
                                        info.getExpirationDate() == null
                                                || info.getExpirationDate().isAfter(now))
                        .collect(
                                groupingBy(
                                        OrderBookScalpScreener::assetGroupKey,
                                        LinkedHashMap::new,
                                        toList()));

        List<ScoredTicker> picked = new ArrayList<>();
        for (Map.Entry<String, List<TickerInfo>> entry : byAsset.entrySet()) {
            List<TickerInfo> group =
                    entry.getValue().stream()
                            .sorted(
                                    comparing(
                                            TickerInfo::getExpirationDate,
                                            (left, right) -> {
                                                if (left == null && right == null) {
                                                    return 0;
                                                }
                                                if (left == null) {
                                                    return 1;
                                                }
                                                if (right == null) {
                                                    return -1;
                                                }
                                                return left.compareTo(right);
                                            }))
                            .collect(toList());

            ScoredTicker best = null;
            int probeLimit = Math.min(config.getScreeningNearestContracts(), group.size());
            for (int index = 0; index < probeLimit; index++) {
                TickerInfo candidate = group.get(index);
                try {
                    ScoredTicker ranked = scoreTicker(tradingService, candidate, config, availableCash);
                    if (ranked != null && (best == null || ranked.score() > best.score())) {
                        best = ranked;
                    }
                } catch (Exception ex) {
                    LoggingUtils.log(
                            "Screen skip dated " + candidate.getTicker() + ": " + ex.getMessage());
                }
                sleep(120);
            }

            if (best != null) {
                picked.add(best);
                LoggingUtils.log(
                        "Nearest dated "
                                + entry.getKey()
                                + ": "
                                + best.info().getTicker()
                                + " exp="
                                + best.info().getExpirationDate());
            }
        }
        return picked;
    }

    private static boolean isDatedTicker(String ticker) {
        return DATED_TICKER_SUFFIX.matcher(ticker.toUpperCase()).matches();
    }

    private static String assetGroupKey(TickerInfo info) {
        if (info.getBasicAsset() != null && !info.getBasicAsset().isBlank()) {
            return info.getBasicAsset().trim().toUpperCase();
        }
        return extractRootFromTicker(info.getTicker());
    }

    private static String extractRootFromTicker(String ticker) {
        String normalized = ticker.toUpperCase();
        if (DATED_TICKER_SUFFIX.matcher(normalized).matches()) {
            return normalized.replaceAll("[FGHJKMNQUVXZ]\\d$", "");
        }
        return normalized;
    }

    private static ScoredTicker scoreTicker(
            TradingService tradingService, TickerInfo info, OrderBookScalpConfig config, double availableCash) {
        String ticker = info.getTicker();
        Map<String, Map<Double, Long>> book = tradingService.getCurrentPrices(info.getKey(), false);
        if (book == null || !book.containsKey("bids") || !book.containsKey("asks")) {
            LoggingUtils.log("Screen skip " + ticker + ": no orderbook data");
            return null;
        }
        if (book.get("bids").isEmpty() || book.get("asks").isEmpty()) {
            LoggingUtils.log("Screen skip " + ticker + ": empty bids/asks");
            return null;
        }

        double bestBid =
                book.get("bids").keySet().stream()
                        .mapToDouble(Double::doubleValue)
                        .max()
                        .orElse(0.0);
        double bestAsk =
                book.get("asks").keySet().stream()
                        .mapToDouble(Double::doubleValue)
                        .min()
                        .orElse(0.0);
        if (bestAsk <= bestBid) {
            LoggingUtils.log("Screen skip " + ticker + ": inverted spread");
            return null;
        }

        double spread = bestAsk - bestBid;
        double mid = (bestBid + bestAsk) / 2.0;
        double spreadBps = mid > 0.0 ? spread / mid * 10_000.0 : Double.MAX_VALUE;
        if (spreadBps > config.getMaxSpreadBps()) {
            LoggingUtils.log(
                    "Screen skip "
                            + ticker
                            + ": spread "
                            + String.format("%.1f", spreadBps)
                            + "bps > max "
                            + String.format("%.1f", config.getMaxSpreadBps())
                            + "bps");
            return null;
        }

        // Check lot affordability — skip if minimum lot cost exceeds position budget (percent of available cash)
        int lotSize = info.getLot() != null ? Math.max(1, info.getLot()) : 1;
        double minLotCost = bestAsk * lotSize;
        double maxCost = availableCash * config.getPositionCashPercent() / 100.0;
        if (minLotCost > maxCost) {
            LoggingUtils.log(
                    "Screen skip "
                            + ticker
                            + ": lot too expensive (lot="
                            + lotSize
                            + ", cost="
                            + String.format("%.0f", minLotCost)
                            + " > cash="
                            + String.format("%.0f", maxCost)
                            + ")");
            return null;
        }

        long bidQty0 = book.get("bids").getOrDefault(bestBid, 0L);
        long askQty0 = book.get("asks").getOrDefault(bestAsk, 0L);
        if (bidQty0 < config.getMinBestLevelQty() || askQty0 < config.getMinBestLevelQty()) {
            LoggingUtils.log(
                    "Screen skip "
                            + ticker
                            + ": low level qty (bid="
                            + bidQty0
                            + ", ask="
                            + askQty0
                            + ", min="
                            + config.getMinBestLevelQty()
                            + ")");
            return null;
        }

        long topDepth = bidQty0 + askQty0;
        long bookDepth =
                sumTopLevels(book.get("bids"), config.getScreeningBookLevels())
                        + sumTopLevels(book.get("asks"), config.getScreeningBookLevels());
        if (topDepth < config.getScreeningMinTopDepth()
                || bookDepth < config.getScreeningMinBookDepth()) {
            LoggingUtils.log(
                    "Screen skip "
                            + ticker
                            + ": low depth (top="
                            + topDepth
                            + "/"
                            + config.getScreeningMinTopDepth()
                            + ", book="
                            + bookDepth
                            + "/"
                            + config.getScreeningMinBookDepth()
                            + ")");
            return null;
        }

        // For crypto, skip trade flow check since stubbed data has 0 volume
        double tradeVolume = 0.0;
        if (info.getType() != TickerType.CRYPTO) {
            tradeVolume = loadRecentTradeVolume(tradingService, info.getKey());
            if (tradeVolume < config.getMinScreeningTradeFlow()) {
                LoggingUtils.log(
                        "Screen skip "
                                + ticker
                                + ": low trade flow ("
                                + String.format("%.0f", tradeVolume)
                                + " < "
                                + String.format("%.0f", config.getMinScreeningTradeFlow())
                                + ")");
                return null;
            }
        }

        // Calculate economics in RUB (accounting for contract size)
        int lot = Math.max(1, info.getLot() != null ? info.getLot() : 1);
        // For futures, use basicAssetSize to get real contract value
        // e.g., CNYRUBF: price=12.5, basicAssetSize=1000, contract_value=12500 RUB
        double contractMultiplier = lot;
        if (info.getBasicAssetSize() != null && info.getBasicAssetSize() > 0) {
            contractMultiplier = info.getBasicAssetSize();
        }
        double expectedTpDistance = spread * config.getTakeProfitSpreads();
        double expectedTpProfitPerContract = expectedTpDistance * contractMultiplier; // PnL in RUB
        
        // For futures (FEATURE), Tinkoff charges FIXED commission per contract (~4-7 RUB)
        // For stocks, commission is percentage-based in RUB
        // For crypto, commission is percentage-based in USDT (much lower, ~0.03%)
        double effectiveCommission;
        if (info.getType() == TickerType.FEATURE) {
            // Futures: use fixed commission only
            effectiveCommission = config.getFuturesCommissionPerContract() * 2.0;
        } else if (info.getType() == TickerType.CRYPTO) {
            // Crypto: percentage-based in USDT, typical ~0.03%
            effectiveCommission = bestAsk * config.getCryptoCommissionRate() * 2.0 * contractMultiplier;
        } else {
            // Stocks: use percentage-based commission
            effectiveCommission = bestAsk * config.getCommissionRate() * 2.0 * contractMultiplier;
        }
        
        // Calculate economics ratio (skip check for crypto)
        double economicsRatio =
                effectiveCommission > 0.0 ? expectedTpProfitPerContract / effectiveCommission : 0.0;
        
        // Skip economics check for crypto — low commissions make ratio artificially low
        if (info.getType() != TickerType.CRYPTO) {
            if (economicsRatio < config.getMinEconomicsRatio()) {
                LoggingUtils.log(
                        "Screen skip "
                                + ticker
                                + ": bad economics (ratio="
                                + String.format("%.2f", economicsRatio)
                                + " < "
                                + String.format("%.2f", config.getMinEconomicsRatio())
                                + ", tpProfit="
                                + String.format("%.2f", expectedTpProfitPerContract)
                                + " RUB, commission="
                                + String.format("%.2f", effectiveCommission)
                                + " RUB, multiplier="
                                + String.format("%.0f", contractMultiplier)
                                + ", type="
                                + info.getType()
                                + ")");
                return null;
            }
        }

        double spreadScore = Math.max(0.0, config.getMaxSpreadBps() - spreadBps) * 2.0;
        double topDepthScore = Math.log1p(topDepth) * 80.0;
        double bookDepthScore = Math.log1p(bookDepth) * 120.0;
        double flowScore = Math.min(tradeVolume, 5_000.0) * 0.02;
        double economicsScore = Math.max(0.0, Math.min(economicsRatio, 3.0)) * 120.0;
        double economicsPenalty = economicsRatio < 1.5 ? (1.5 - economicsRatio) * 400.0 : 0.0;
        double microContractPenalty = spread < effectiveCommission * 0.75 ? 250.0 : 0.0;
        double coreBonus = 0.0;
        if (isCoreMoexPerpetual(info.getTicker())) {
            coreBonus += 200.0;
        }
        if (isCoreCommodityAsset(assetGroupKey(info))) {
            coreBonus += 150.0;
        }
        if (isCoreStock(info.getTicker())) {
            coreBonus += 250.0; // High priority for liquid stocks
        }
        if (isCoreCrypto(info.getTicker())) {
            coreBonus += 250.0; // High priority for liquid crypto pairs
        }
        // Stocks generally have tighter spreads and better for scalping
        double stockBonus = (info.getType() == TickerType.STOCK && spreadBps < 5.0) ? 100.0 : 0.0;
        double score =
                spreadScore
                        + topDepthScore
                        + bookDepthScore
                        + flowScore
                        + economicsScore
                        + coreBonus
                        + stockBonus
                        - economicsPenalty
                        - microContractPenalty;
        return new ScoredTicker(info, score, spreadBps, topDepth, bookDepth, tradeVolume);
    }

    private static boolean isCoreMoexPerpetual(String ticker) {
        return CORE_MOEX_PERPETUALS.contains(ticker.toUpperCase());
    }

    private static boolean isCoreCommodityAsset(String assetKey) {
        String normalized = assetKey.toUpperCase();
        return CORE_COMMODITY_ASSETS.stream().anyMatch(normalized::contains);
    }

    private static boolean isCoreStock(String ticker) {
        return ticker != null && CORE_STOCKS.contains(ticker.toUpperCase());
    }

    private static int sumTopLevels(Map<Double, Long> side, int maxLevels) {
        int sum = 0;
        int level = 0;
        for (Long quantity : side.values()) {
            sum += quantity.intValue();
            level++;
            if (level >= maxLevels) {
                break;
            }
        }
        return sum;
    }

    private static double loadRecentTradeVolume(TradingService tradingService, TickerInfo.Key key) {
        Instant to = Instant.now();
        Instant from = to.minusSeconds(300);
        List<MarketTradeTick> trades = tradingService.getLastTrades(key, from, to);
        long volume = 0;
        for (MarketTradeTick trade : trades) {
            volume += trade.getQuantity();
        }
        return volume;
    }

    private static final class ScoredTicker {

        private final TickerInfo info;
        private final double score;
        private final double spreadBps;
        private final long topDepth;
        private final long bookDepth;
        private final double tradeFlow;

        private ScoredTicker(
                TickerInfo info,
                double score,
                double spreadBps,
                long topDepth,
                long bookDepth,
                double tradeFlow) {
            this.info = info;
            this.score = score;
            this.spreadBps = spreadBps;
            this.topDepth = topDepth;
            this.bookDepth = bookDepth;
            this.tradeFlow = tradeFlow;
        }

        private TickerInfo info() {
            return info;
        }

        private double score() {
            return score;
        }

        private double spreadBps() {
            return spreadBps;
        }

        private long topDepth() {
            return topDepth;
        }

        private long bookDepth() {
            return bookDepth;
        }

        private double tradeFlow() {
            return tradeFlow;
        }

        private String assetLabel() {
            String asset = info.getBasicAsset();
            if (asset != null && !asset.isBlank()) {
                return asset;
            }
            return extractRootFromTicker(info.getTicker());
        }
    }
}
