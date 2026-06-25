package com.github.shk0da.goldendragon.strategy;

import static com.github.shk0da.goldendragon.utils.TimeUtils.sleep;
import static java.util.Comparator.comparing;
import static java.util.Comparator.comparingDouble;
import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.toList;

import com.github.shk0da.goldendragon.config.OrderBookScalpConfig;
import com.github.shk0da.goldendragon.model.MarketTradeTick;
import com.github.shk0da.goldendragon.model.TickerInfo;
import com.github.shk0da.goldendragon.service.TCSService;
import com.github.shk0da.goldendragon.utils.LoggingUtils;
import java.time.Instant;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

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

    private OrderBookScalpScreener() {}

    public static List<TickerInfo> selectTop(
            TCSService tcsService, List<TickerInfo> candidates, OrderBookScalpConfig config) {
        List<TickerInfo> perpetuals =
                candidates.stream()
                        .filter(info -> isPerpetualCandidate(info.getTicker()))
                        .collect(toList());
        List<TickerInfo> datedCommodities =
                candidates.stream()
                        .filter(OrderBookScalpScreener::isCommodityDatedFuture)
                        .collect(toList());

        List<ScoredTicker> scored = new ArrayList<>();
        scored.addAll(pickNearestLiquidDated(tcsService, datedCommodities, config));

        for (TickerInfo info : perpetuals) {
            try {
                ScoredTicker ranked = scoreTicker(tcsService, info, config);
                if (ranked != null) {
                    scored.add(ranked);
                }
            } catch (Exception ex) {
                LoggingUtils.log("Screen skip " + info.getTicker() + ": " + ex.getMessage());
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

    private static List<ScoredTicker> pickNearestLiquidDated(
            TCSService tcsService, List<TickerInfo> datedCommodities, OrderBookScalpConfig config) {
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
                    ScoredTicker ranked = scoreTicker(tcsService, candidate, config);
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
            TCSService tcsService, TickerInfo info, OrderBookScalpConfig config) {
        Map<String, Map<Double, Integer>> book = tcsService.getCurrentPrices(info.getKey(), false);
        if (book == null || !book.containsKey("bids") || !book.containsKey("asks")) {
            return null;
        }
        if (book.get("bids").isEmpty() || book.get("asks").isEmpty()) {
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
            return null;
        }

        double spread = bestAsk - bestBid;
        double mid = (bestBid + bestAsk) / 2.0;
        double spreadBps = mid > 0.0 ? spread / mid * 10_000.0 : Double.MAX_VALUE;
        if (spreadBps > config.getMaxSpreadBps()) {
            return null;
        }

        int bidQty0 = book.get("bids").getOrDefault(bestBid, 0);
        int askQty0 = book.get("asks").getOrDefault(bestAsk, 0);
        if (bidQty0 < config.getMinBestLevelQty() || askQty0 < config.getMinBestLevelQty()) {
            return null;
        }

        int topDepth = bidQty0 + askQty0;
        int bookDepth =
                sumTopLevels(book.get("bids"), config.getScreeningBookLevels())
                        + sumTopLevels(book.get("asks"), config.getScreeningBookLevels());
        if (topDepth < config.getScreeningMinTopDepth()
                || bookDepth < config.getScreeningMinBookDepth()) {
            return null;
        }

        double tradeVolume = loadRecentTradeVolume(tcsService, info.getKey());

        double spreadScore = Math.max(0.0, config.getMaxSpreadBps() - spreadBps) * 2.0;
        double topDepthScore = Math.log1p(topDepth) * 80.0;
        double bookDepthScore = Math.log1p(bookDepth) * 120.0;
        double flowScore = Math.min(tradeVolume, 5_000.0) * 0.02;
        double coreBonus = 0.0;
        if (isCoreMoexPerpetual(info.getTicker())) {
            coreBonus += 200.0;
        }
        if (isCoreCommodityAsset(assetGroupKey(info))) {
            coreBonus += 150.0;
        }
        double score = spreadScore + topDepthScore + bookDepthScore + flowScore + coreBonus;
        return new ScoredTicker(info, score, spreadBps, topDepth, bookDepth, tradeVolume);
    }

    private static boolean isCoreMoexPerpetual(String ticker) {
        return CORE_MOEX_PERPETUALS.contains(ticker.toUpperCase());
    }

    private static boolean isCoreCommodityAsset(String assetKey) {
        String normalized = assetKey.toUpperCase();
        return CORE_COMMODITY_ASSETS.stream().anyMatch(normalized::contains);
    }

    private static int sumTopLevels(Map<Double, Integer> side, int maxLevels) {
        int sum = 0;
        int level = 0;
        for (Integer quantity : side.values()) {
            sum += quantity;
            level++;
            if (level >= maxLevels) {
                break;
            }
        }
        return sum;
    }

    private static double loadRecentTradeVolume(TCSService tcsService, TickerInfo.Key key) {
        Instant to = Instant.now();
        Instant from = to.minusSeconds(300);
        List<MarketTradeTick> trades = tcsService.getLastTrades(key, from, to);
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
        private final int topDepth;
        private final int bookDepth;
        private final double tradeFlow;

        private ScoredTicker(
                TickerInfo info,
                double score,
                double spreadBps,
                int topDepth,
                int bookDepth,
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

        private int topDepth() {
            return topDepth;
        }

        private int bookDepth() {
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
