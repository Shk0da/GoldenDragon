package com.github.shk0da.GoldenDragon.strategy;

import com.github.shk0da.GoldenDragon.config.OrderFlowScalpingConfig;
import com.github.shk0da.GoldenDragon.model.MarketDepthLevel;
import com.github.shk0da.GoldenDragon.model.MarketDepthSnapshot;
import com.github.shk0da.GoldenDragon.model.MarketTickListener;
import com.github.shk0da.GoldenDragon.model.MarketTradeTick;
import com.github.shk0da.GoldenDragon.model.Position;
import com.github.shk0da.GoldenDragon.model.PositionInfo;
import com.github.shk0da.GoldenDragon.model.TickerInfo;
import com.github.shk0da.GoldenDragon.repository.TickerRepository;
import com.github.shk0da.GoldenDragon.service.TCSService;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Deque;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;

import static com.github.shk0da.GoldenDragon.service.TelegramNotifyService.telegramNotifyService;
import static com.github.shk0da.GoldenDragon.utils.TimeUtils.sleep;
import static java.lang.Math.abs;
import static java.lang.Math.max;
import static java.lang.Math.min;
import static java.lang.System.out;

public class OrderFlowScalpingStrategy implements MarketTickListener {

    private final OrderFlowScalpingConfig config;
    private final TCSService tcsService;
    private final Map<String, ScalpingState> stateByTicker = new ConcurrentHashMap<>();
    private final double startBalance;
    private volatile boolean running = true;
    private double dailyPnl = 0.0;

    public OrderFlowScalpingStrategy(OrderFlowScalpingConfig config, TCSService tcsService) {
        this.config = config;
        this.tcsService = tcsService;
        this.startBalance = tcsService.getAvailableCash();
    }

    public void run() {
        Runtime.getRuntime().addShutdownHook(new Thread(this::stopGracefully));

        List<OrderFlowScalpingConfig.Instrument> instruments = config.getInstruments();
        if (instruments.isEmpty()) {
            out.println("OrderFlowScalpingStrategy: no configured tickers");
            return;
        }

        telegramNotifyService.sendMessage("Run OrderFlowScalpingStrategy");
        instruments.forEach(this::subscribeTicker);

        while (running) {
            if (isDailyLossLimitReached()) {
                out.println("OrderFlowScalpingStrategy: daily loss limit reached");
                running = false;
                break;
            }
            instruments.forEach(instrument -> processTicker(instrument.getTicker()));
            sleep(config.getLoopSleepMs());
        }

        forceCloseAllPositions();
        instruments.forEach(instrument -> unsubscribeTicker(instrument.getTicker()));
        telegramNotifyService.sendMessage("Stop OrderFlowScalpingStrategy");
    }

    public void stopGracefully() {
        running = false;
        forceCloseAllPositions();
        new ArrayList<>(stateByTicker.keySet()).forEach(this::unsubscribeTicker);
    }

    private void subscribeTicker(OrderFlowScalpingConfig.Instrument instrument) {
        String ticker = instrument.getTicker();
        TickerInfo tickerInfo = TickerRepository.INSTANCE.getById(new TickerInfo.Key(ticker, instrument.getType()));
        if (tickerInfo == null) {
            tickerInfo = tcsService.searchTicker(new TickerInfo.Key(ticker, instrument.getType()));
        }
        if (tickerInfo == null) {
            return;
        }
        stateByTicker.putIfAbsent(ticker, new ScalpingState(tickerInfo));
        tcsService.subscribeMarketData(tickerInfo.getKey(), config.getOrderBookDepth(), this);
    }

    private void unsubscribeTicker(String ticker) {
        ScalpingState state = stateByTicker.get(ticker);
        if (state == null) {
            return;
        }
        tcsService.unsubscribeMarketData(state.tickerInfo.getKey(), this);
    }

    private void processTicker(String ticker) {
        ScalpingState state = stateByTicker.get(ticker);
        if (state == null || state.lastOrderBook == null) {
            return;
        }

        cleanup(state);
        updateMarketRegime(state);

        if (hasOpenPosition(state)) {
            manageOpenPosition(state);
            return;
        }

        if (!isRegimeTradable(state)) {
            return;
        }

        Signal signal = detectSignal(state);
        if (signal != null) {
            openSignalPosition(state, signal);
        }
    }

    private void updateMarketRegime(ScalpingState state) {
        Double bestBid = state.lastOrderBook.getBestBid();
        Double bestAsk = state.lastOrderBook.getBestAsk();
        if (bestBid == null || bestAsk == null) {
            state.marketRegime = MarketRegime.LOW_LIQUIDITY;
            return;
        }

        double tickSize = max(state.tickerInfo.getMinPriceIncrement(), 0.01);
        double spreadTicks = (bestAsk - bestBid) / tickSize;
        long topBookVolume = topBookVolume(state.lastOrderBook, 5);
        double priceRangeTicks = tickSize > 0.0 ? abs(state.priceDriftShortWindow) / tickSize : 0.0;

        if (topBookVolume < config.getLowLiquidityBookVolume()) {
            state.marketRegime = MarketRegime.LOW_LIQUIDITY;
        } else if (spreadTicks >= config.getHighVolatilitySpreadMultiplier()) {
            state.marketRegime = MarketRegime.HIGH_VOLATILITY;
        } else if (priceRangeTicks >= 8.0 && abs(state.cvdShortWindow) > 0) {
            state.marketRegime = MarketRegime.TREND;
        } else {
            state.marketRegime = MarketRegime.FLAT;
        }
    }

    private Signal detectSignal(ScalpingState state) {
        List<Signal> signals = new ArrayList<>();
        addSignal(signals, detectImbalanceSignal(state));
        addSignal(signals, detectIcebergSignal(state));
        addSignal(signals, detectSpoofingSignal(state));
        addSignal(signals, detectAbsorptionSignal(state));
        addSignal(signals, detectStackedBreakoutSignal(state));
        return signals.stream().max(Comparator.comparingDouble(it -> it.confidence)).orElse(null);
    }

    private void addSignal(List<Signal> signals, Signal signal) {
        if (signal != null) {
            signals.add(signal);
        }
    }

    private Signal detectImbalanceSignal(ScalpingState state) {
        if (state.marketRegime == MarketRegime.LOW_LIQUIDITY || state.marketRegime == MarketRegime.HIGH_VOLATILITY) {
            return null;
        }

        double obi = calcObi(state.lastOrderBook, config.getImbalanceThresholdLevels());
        Instant now = Instant.now();

        if (obi > config.getImbalanceThreshold()) {
            if (state.imbalancePositiveSince == null) {
                state.imbalancePositiveSince = now;
                state.imbalanceAnchorBidPrice = state.lastOrderBook.getBestBid();
            }
        } else {
            state.imbalancePositiveSince = null;
            state.imbalanceAnchorBidPrice = null;
        }

        if (obi < -config.getImbalanceThreshold()) {
            if (state.imbalanceNegativeSince == null) {
                state.imbalanceNegativeSince = now;
                state.imbalanceAnchorAskPrice = state.lastOrderBook.getBestAsk();
            }
        } else {
            state.imbalanceNegativeSince = null;
            state.imbalanceAnchorAskPrice = null;
        }

        Double mid = state.lastOrderBook.getMidPrice();
        if (mid == null) {
            return null;
        }

        if (state.imbalancePositiveSince != null
                && Duration.between(state.imbalancePositiveSince, now).toMillis() >= config.getImbalanceHoldMs()
                && hasLargeBestBid(state)
                && !isLocalHigh(state, mid)) {
            double stopPrice = state.imbalanceAnchorBidPrice != null
                    ? state.imbalanceAnchorBidPrice - state.tickerInfo.getMinPriceIncrement() * 2
                    : getEntryPrice(state, "BUY") - state.tickerInfo.getMinPriceIncrement() * 2;
            return Signal.imbalance("BUY", getEntryPrice(state, "BUY"), stopPrice, config.getBaseRiskPercent(), state.imbalanceAnchorBidPrice);
        }

        if (state.imbalanceNegativeSince != null
                && Duration.between(state.imbalanceNegativeSince, now).toMillis() >= config.getImbalanceHoldMs()
                && hasLargeBestAsk(state)
                && !isLocalLow(state, mid)) {
            double stopPrice = state.imbalanceAnchorAskPrice != null
                    ? state.imbalanceAnchorAskPrice + state.tickerInfo.getMinPriceIncrement() * 2
                    : getEntryPrice(state, "SELL") + state.tickerInfo.getMinPriceIncrement() * 2;
            return Signal.imbalance("SELL", getEntryPrice(state, "SELL"), stopPrice, config.getBaseRiskPercent(), state.imbalanceAnchorAskPrice);
        }
        return null;
    }

    private Signal detectIcebergSignal(ScalpingState state) {
        if (state.lastOrderBook == null || state.recentTrades.isEmpty() || state.marketRegime == MarketRegime.LOW_LIQUIDITY) {
            return null;
        }

        IcebergCandidate bidIceberg = detectIcebergCandidate(state, true);
        IcebergCandidate askIceberg = detectIcebergCandidate(state, false);
        state.activeBidIceberg = bidIceberg;
        state.activeAskIceberg = askIceberg;

        if (bidIceberg != null
                && bidIceberg.replenishmentCycles >= config.getIcebergMinReplenishments()
                && bidIceberg.aggressiveVolume >= bidIceberg.visibleVolume * config.getIcebergTradeToVisibleRatio()
                && state.cvdThirtySeconds >= -bidIceberg.visibleVolume
                && !hasOppositeIcebergNearby(bidIceberg, askIceberg, state.tickerInfo.getMinPriceIncrement())) {
            double entryPrice = bidIceberg.price + state.tickerInfo.getMinPriceIncrement();
            double stopPrice = bidIceberg.price - state.tickerInfo.getMinPriceIncrement() * 2;
            return Signal.iceberg("BUY", entryPrice, stopPrice, config.getHighQualityRiskPercent(), bidIceberg.price);
        }

        if (askIceberg != null
                && askIceberg.replenishmentCycles >= config.getIcebergMinReplenishments()
                && askIceberg.aggressiveVolume >= askIceberg.visibleVolume * config.getIcebergTradeToVisibleRatio()
                && state.cvdThirtySeconds <= askIceberg.visibleVolume
                && !hasOppositeIcebergNearby(askIceberg, bidIceberg, state.tickerInfo.getMinPriceIncrement())) {
            double entryPrice = askIceberg.price - state.tickerInfo.getMinPriceIncrement();
            double stopPrice = askIceberg.price + state.tickerInfo.getMinPriceIncrement() * 2;
            return Signal.iceberg("SELL", entryPrice, stopPrice, config.getHighQualityRiskPercent(), askIceberg.price);
        }
        return null;
    }

    private IcebergCandidate detectIcebergCandidate(ScalpingState state, boolean bidSide) {
        List<MarketDepthLevel> levels = bidSide ? state.lastOrderBook.getBids() : state.lastOrderBook.getAsks();
        if (levels.isEmpty()) {
            return null;
        }

        MarketDepthLevel best = levels.get(0);
        double price = best.getPrice();
        int visibleVolume = best.getQuantity();
        long aggressiveVolume = 0L;
        int replenishments = 0;
        int lastVisible = -1;
        Instant lastTradeTime = null;

        for (MarketTradeTick trade : state.thirtySecondTrades) {
            if (abs(trade.getPrice() - price) > state.tickerInfo.getMinPriceIncrement() * 0.5) {
                continue;
            }
            if (bidSide && !trade.getDirection().contains("SELL")) {
                continue;
            }
            if (!bidSide && !trade.getDirection().contains("BUY")) {
                continue;
            }
            aggressiveVolume += trade.getQuantity();
            if (lastTradeTime != null && Duration.between(lastTradeTime, trade.getTime()).toMillis() <= config.getIcebergReplenishMs()) {
                replenishments++;
            }
            lastTradeTime = trade.getTime();
            lastVisible = visibleVolume;
        }

        if (lastVisible <= 0 || aggressiveVolume <= 0) {
            return null;
        }
        return new IcebergCandidate(price, visibleVolume, aggressiveVolume, replenishments);
    }

    private boolean hasOppositeIcebergNearby(IcebergCandidate current, IcebergCandidate opposite, double tickSize) {
        if (current == null || opposite == null) {
            return false;
        }
        return abs(current.price - opposite.price) <= tickSize * config.getIcebergOppositeTicks()
                && opposite.visibleVolume > current.visibleVolume;
    }

    private Signal detectSpoofingSignal(ScalpingState state) {
        if (state.marketRegime == MarketRegime.LOW_LIQUIDITY) {
            return null;
        }

        Instant now = Instant.now();
        if (state.lastBidSpoofRemovedAt != null
                && Duration.between(state.lastBidSpoofRemovedAt, now).toMillis() <= 1000
                && state.lastBidSpoofWasLarge
                && !state.lastBidSpoofLikelyExecuted
                && hasSpoofHistory(state.bidSpoofHistory, now)) {
            return Signal.spoof("SELL", getEntryPrice(state, "SELL"), getEntryPrice(state, "SELL") + state.tickerInfo.getMinPriceIncrement() * 3, config.getBaseRiskPercent());
        }
        if (state.lastAskSpoofRemovedAt != null
                && Duration.between(state.lastAskSpoofRemovedAt, now).toMillis() <= 1000
                && state.lastAskSpoofWasLarge
                && !state.lastAskSpoofLikelyExecuted
                && hasSpoofHistory(state.askSpoofHistory, now)) {
            return Signal.spoof("BUY", getEntryPrice(state, "BUY"), getEntryPrice(state, "BUY") - state.tickerInfo.getMinPriceIncrement() * 3, config.getBaseRiskPercent());
        }
        return null;
    }

    private Signal detectAbsorptionSignal(ScalpingState state) {
        if (state.marketRegime == MarketRegime.LOW_LIQUIDITY || state.marketRegime == MarketRegime.HIGH_VOLATILITY) {
            return null;
        }

        double flatRange = state.tickerInfo.getMinPriceIncrement() * config.getAbsorptionFlatTicks();
        if (state.sellAggressionVolume > 0
                && state.cvdShortWindow < 0
                && abs(state.priceDriftShortWindow) <= flatRange
                && hasAbsorptionBidLevel(state)
                && state.sellAggressionVolume >= state.lastVisibleBidAtSignal
                && state.sellAggressionSlowdown) {
            double entryPrice = getEntryPrice(state, "BUY");
            double stopPrice = entryPrice - state.tickerInfo.getMinPriceIncrement() * 2;
            return Signal.absorption("BUY", entryPrice, stopPrice, config.getHighQualityRiskPercent());
        }

        if (state.buyAggressionVolume > 0
                && state.cvdShortWindow > 0
                && abs(state.priceDriftShortWindow) <= flatRange
                && hasAbsorptionAskLevel(state)
                && state.buyAggressionVolume >= state.lastVisibleAskAtSignal
                && state.buyAggressionSlowdown) {
            double entryPrice = getEntryPrice(state, "SELL");
            double stopPrice = entryPrice + state.tickerInfo.getMinPriceIncrement() * 2;
            return Signal.absorption("SELL", entryPrice, stopPrice, config.getHighQualityRiskPercent());
        }
        return null;
    }

    private Signal detectStackedBreakoutSignal(ScalpingState state) {
        if (state.lastOrderBook == null || state.marketRegime == MarketRegime.LOW_LIQUIDITY) {
            return null;
        }

        StackedLevel bidStack = detectStack(state, true);
        StackedLevel askStack = detectStack(state, false);
        state.bidStack = bidStack;
        state.askStack = askStack;

        if (askStack != null
                && askStack.persistedMs >= config.getStackedPersistenceSeconds() * 1000L
                && crossedBreakoutLevel(state, askStack.breakoutPrice, true)
                && aggressiveVolumeAfterBreakout(state, askStack.breakoutPrice, true) >= askStack.breakoutVisibleVolume * config.getBreakoutTradeMultiplier()
                && shortWindowCvdAfterBreakout(state, askStack.breakoutPrice, true) > 0
                && askStack.remainingVolumeCollapsed) {
            double entryPrice = getEntryPrice(state, "BUY");
            double stopPrice = askStack.basePrice - state.tickerInfo.getMinPriceIncrement();
            double riskPercent = state.marketRegime == MarketRegime.TREND ? config.getStackedRiskPercent() : config.getBaseRiskPercent();
            return Signal.breakout("BUY", entryPrice, stopPrice, riskPercent);
        }

        if (bidStack != null
                && bidStack.persistedMs >= config.getStackedPersistenceSeconds() * 1000L
                && crossedBreakoutLevel(state, bidStack.breakoutPrice, false)
                && aggressiveVolumeAfterBreakout(state, bidStack.breakoutPrice, false) >= bidStack.breakoutVisibleVolume * config.getBreakoutTradeMultiplier()
                && shortWindowCvdAfterBreakout(state, bidStack.breakoutPrice, false) < 0
                && bidStack.remainingVolumeCollapsed) {
            double entryPrice = getEntryPrice(state, "SELL");
            double stopPrice = bidStack.basePrice + state.tickerInfo.getMinPriceIncrement();
            double riskPercent = state.marketRegime == MarketRegime.TREND ? config.getStackedRiskPercent() : config.getBaseRiskPercent();
            return Signal.breakout("SELL", entryPrice, stopPrice, riskPercent);
        }
        return null;
    }

    private StackedLevel detectStack(ScalpingState state, boolean bidSide) {
        List<MarketDepthLevel> levels = bidSide ? state.lastOrderBook.getBids() : state.lastOrderBook.getAsks();
        if (levels.size() < config.getStackedLevels()) {
            return null;
        }
        long segmentVolume = levels.stream().limit(config.getStackedLevels()).mapToLong(MarketDepthLevel::getQuantity).sum();
        double medianSegment = bidSide ? state.bidSegmentMedian : state.askSegmentMedian;
        if (medianSegment <= 0.0 || segmentVolume < medianSegment * config.getStackedVolumeMultiplier()) {
            return null;
        }

        Instant since = bidSide ? state.bidStackSince : state.askStackSince;
        if (since == null) {
            since = Instant.now();
            if (bidSide) {
                state.bidStackSince = since;
            } else {
                state.askStackSince = since;
            }
        }

        long remainingLevelsVolume = levels.stream().skip(1).limit(config.getStackedLevels() - 1).mapToLong(MarketDepthLevel::getQuantity).sum();
        long breakoutVisibleVolume = levels.get(config.getStackedLevels() - 1).getQuantity();
        double breakoutPrice = levels.get(config.getStackedLevels() - 1).getPrice();
        double basePrice = levels.get(0).getPrice();
        boolean collapsed = remainingLevelsVolume < segmentVolume * 0.4;
        return new StackedLevel(basePrice, breakoutPrice, breakoutVisibleVolume, Duration.between(since, Instant.now()).toMillis(), collapsed);
    }

    private boolean crossedBreakoutLevel(ScalpingState state, double breakoutPrice, boolean breakoutUp) {
        Instant threshold = Instant.now().minusMillis(config.getBreakoutConfirmationMs());
        TimedPrice first = state.midPrices.stream().filter(it -> !it.time.isBefore(threshold)).findFirst().orElse(null);
        TimedPrice last = state.midPrices.peekLast();
        if (first == null || last == null) {
            return false;
        }
        if (breakoutUp) {
            return first.price < breakoutPrice && last.price >= breakoutPrice;
        }
        return first.price > breakoutPrice && last.price <= breakoutPrice;
    }

    private void openSignalPosition(ScalpingState state, Signal signal) {
        if (countOpenPositions() >= config.getMaxTotalPositions() || countOpenPositions(state.tickerInfo) >= config.getMaxPositionsPerTicker()) {
            return;
        }

        double tickSize = max(state.tickerInfo.getMinPriceIncrement(), 0.01);
        double riskCash = max(0.0, tcsService.getAvailableCash()) * signal.riskPercent;
        double stopDistance = max(tickSize, abs(signal.entryPrice - signal.stopPrice));
        int quantity = max(1, (int) Math.floor(riskCash / stopDistance));
        if (quantity <= 0) {
            return;
        }

        TCSService.OrderExecutionResult orderResult = "BUY".equals(signal.direction)
                ? openLong(state, signal, quantity)
                : openShort(state, signal, quantity);
        if (!orderResult.isSuccess()) {
            return;
        }

        LivePosition position = createPositionFromSignal(state, signal, orderResult, quantity, tickSize);
        state.openPosition = position;
        syncProtectiveOrders(state, position);
        String explanation = buildEntryExplanation(state, signal, position, riskCash, stopDistance);
        out.println(explanation);
        telegramNotifyService.sendMessage(explanation);
    }

    private String buildEntryExplanation(ScalpingState state,
                                         Signal signal,
                                         LivePosition position,
                                         double riskCash,
                                         double stopDistance) {
        String base = String.format(
                "OrderFlowScalpingStrategy OPEN %s %s by %s: entry=%.4f stop=%.4f take=%.4f qty=%d riskCash=%.2f stopTicks=%.2f regime=%s",
                signal.direction,
                state.tickerInfo.getTicker(),
                signal.reason,
                position.entryPrice,
                position.stopPrice,
                position.takePrice,
                position.remainingQuantity,
                riskCash,
                stopDistance / max(position.tickSize, 0.01),
                state.marketRegime.name()
        );

        if ("OBI".equals(signal.reason)) {
            double obi = calcObi(state.lastOrderBook, config.getImbalanceThresholdLevels());
            long bestBid = state.lastOrderBook.getBids().isEmpty() ? 0 : state.lastOrderBook.getBids().get(0).getQuantity();
            long bestAsk = state.lastOrderBook.getAsks().isEmpty() ? 0 : state.lastOrderBook.getAsks().get(0).getQuantity();
            double avgBid = averageInt(state.bidVolumeHistory);
            double avgAsk = averageInt(state.askVolumeHistory);
            return base + String.format(
                    "; reasonDetails: obi=%.3f holdMs=%d bestBid=%d avgBid=%.1f bestAsk=%d avgAsk=%.1f anchor=%.4f priceDrift=%.4f",
                    obi,
                    getImbalanceHoldDurationMs(state, signal.direction),
                    bestBid,
                    avgBid,
                    bestAsk,
                    avgAsk,
                    signal.anchorPrice != null ? signal.anchorPrice : 0.0,
                    state.priceDriftShortWindow
            );
        }

        if ("ICEBERG".equals(signal.reason)) {
            IcebergCandidate candidate = "BUY".equals(signal.direction) ? state.activeBidIceberg : state.activeAskIceberg;
            return base + String.format(
                    "; reasonDetails: icebergPrice=%.4f visible=%d aggressive=%d replenishments=%d cvd30=%d oppositeIceberg=%s",
                    candidate != null ? candidate.price : 0.0,
                    candidate != null ? candidate.visibleVolume : 0,
                    candidate != null ? candidate.aggressiveVolume : 0,
                    candidate != null ? candidate.replenishmentCycles : 0,
                    state.cvdThirtySeconds,
                    hasOppositeIcebergNearby(state.activeBidIceberg, state.activeAskIceberg, position.tickSize)
            );
        }

        if ("SPOOF".equals(signal.reason)) {
            boolean bidSideSpoof = "SELL".equals(signal.direction);
            SpoofCandidate candidate = bidSideSpoof ? state.bidSpoofCandidate : state.askSpoofCandidate;
            return base + String.format(
                    "; reasonDetails: spoofPrice=%.4f spoofVisible=%d fastCancelMs=%d likelyExecuted=%s historyCount=%d",
                    candidate != null ? candidate.price : 0.0,
                    candidate != null ? candidate.visibleVolume : 0,
                    config.getSpoofFastCancelMs(),
                    bidSideSpoof ? state.lastBidSpoofLikelyExecuted : state.lastAskSpoofLikelyExecuted,
                    bidSideSpoof ? state.bidSpoofHistory.size() : state.askSpoofHistory.size()
            );
        }

        if ("ABSORPTION".equals(signal.reason)) {
            return base + String.format(
                    "; reasonDetails: cvdShort=%d cvd30=%d sellAggression=%d buyAggression=%d slowdownBuy=%s slowdownSell=%s priceDrift=%.4f bestBid=%d bestAsk=%d",
                    state.cvdShortWindow,
                    state.cvdThirtySeconds,
                    state.sellAggressionVolume,
                    state.buyAggressionVolume,
                    state.buyAggressionSlowdown,
                    state.sellAggressionSlowdown,
                    state.priceDriftShortWindow,
                    state.lastOrderBook.getBids().isEmpty() ? 0 : state.lastOrderBook.getBids().get(0).getQuantity(),
                    state.lastOrderBook.getAsks().isEmpty() ? 0 : state.lastOrderBook.getAsks().get(0).getQuantity()
            );
        }

        if ("STACKED_BREAKOUT".equals(signal.reason)) {
            StackedLevel stack = "BUY".equals(signal.direction) ? state.askStack : state.bidStack;
            long breakoutAggression = stack != null
                    ? aggressiveVolumeAfterBreakout(state, stack.breakoutPrice, "BUY".equals(signal.direction))
                    : 0;
            long breakoutCvd = stack != null
                    ? shortWindowCvdAfterBreakout(state, stack.breakoutPrice, "BUY".equals(signal.direction))
                    : 0;
            return base + String.format(
                    "; reasonDetails: breakoutPrice=%.4f visible=%d persistedMs=%d collapsed=%s breakoutAggression=%d breakoutCvd=%d bidMedian=%.1f askMedian=%.1f",
                    stack != null ? stack.breakoutPrice : 0.0,
                    stack != null ? stack.breakoutVisibleVolume : 0,
                    stack != null ? stack.persistedMs : 0,
                    stack != null && stack.remainingVolumeCollapsed,
                    breakoutAggression,
                    breakoutCvd,
                    state.bidSegmentMedian,
                    state.askSegmentMedian
            );
        }

        return base;
    }

    private long getImbalanceHoldDurationMs(ScalpingState state, String direction) {
        Instant since = "BUY".equals(direction) ? state.imbalancePositiveSince : state.imbalanceNegativeSince;
        return since == null ? 0L : Duration.between(since, Instant.now()).toMillis();
    }

    private TCSService.OrderExecutionResult openLong(ScalpingState state, Signal signal, int quantity) {
        double cash = signal.entryPrice * quantity;
        if (signal.useLimitEntry) {
            return tcsService.buyLimit(state.tickerInfo.getTicker(), state.tickerInfo.getType(), cash, signal.entryPrice);
        }
        return tcsService.buyByMarketWithDetails(
                state.tickerInfo.getTicker(),
                state.tickerInfo.getType(),
                cash,
                abs(signal.takePrice - signal.entryPrice) / signal.entryPrice * 100.0,
                abs(signal.entryPrice - signal.stopPrice) / signal.entryPrice * 100.0
        );
    }

    private TCSService.OrderExecutionResult openShort(ScalpingState state, Signal signal, int quantity) {
        double cash = signal.entryPrice * quantity;
        if (signal.useLimitEntry) {
            return tcsService.sellLimit(state.tickerInfo.getTicker(), state.tickerInfo.getType(), cash, signal.entryPrice);
        }
        return tcsService.sellByMarketWithDetails(
                state.tickerInfo.getTicker(),
                state.tickerInfo.getType(),
                cash,
                abs(signal.entryPrice - signal.takePrice) / signal.entryPrice * 100.0,
                abs(signal.stopPrice - signal.entryPrice) / signal.entryPrice * 100.0
        );
    }

    private LivePosition createPositionFromSignal(ScalpingState state,
                                                  Signal signal,
                                                  TCSService.OrderExecutionResult orderResult,
                                                  int quantity,
                                                  double tickSize) {
        double executedEntry = orderResult.getExecutedPrice() != null && orderResult.getExecutedPrice() > 0.0
                ? orderResult.getExecutedPrice()
                : signal.entryPrice;
        int executedQuantity = orderResult.getExecutedCount() > 0 ? orderResult.getExecutedCount() : quantity;
        List<PartialExit> partialExits = buildPartialPlan(signal, executedEntry, tickSize, executedQuantity);
        return new LivePosition(
                signal.direction,
                signal.reason,
                executedEntry,
                executedQuantity,
                Instant.now(),
                signal.stopPrice,
                signal.takePrice,
                tickSize,
                partialExits,
                signal.anchorPrice
        );
    }

    private void manageOpenPosition(ScalpingState state) {
        LivePosition position = state.openPosition;
        if (position == null || state.lastOrderBook == null) {
            return;
        }

        double currentPrice = getEntryPrice(state, position.direction);
        if (currentPrice <= 0.0) {
            return;
        }

        executePartialExits(state, position, currentPrice);
        if (position.remainingQuantity <= 0) {
            state.openPosition = null;
            return;
        }

        updateStopByScenario(state, position, currentPrice);
        syncProtectiveOrders(state, position);

        if (shouldCloseByStop(position, currentPrice)
                || shouldCloseByTake(position, currentPrice)
                || shouldCloseBySignalInvalidation(state, position, currentPrice)
                || shouldCloseByTimeout(position)) {
            closeOpenPosition(state, currentPrice, "managed_exit");
        }
    }

    private void executePartialExits(ScalpingState state, LivePosition position, double currentPrice) {
        for (PartialExit partialExit : position.partialExits) {
            if (partialExit.executed || partialExit.quantity <= 0) {
                continue;
            }
            boolean hit = "BUY".equals(position.direction) ? currentPrice >= partialExit.price : currentPrice <= partialExit.price;
            if (!hit) {
                continue;
            }
            if (!tcsService.closePartiallyByMarket(state.tickerInfo.getTicker(), state.tickerInfo.getType(), partialExit.quantity)) {
                continue;
            }
            partialExit.executed = true;
            position.remainingQuantity = max(0, position.remainingQuantity - partialExit.quantity);
            if (partialExit.moveStopToPrice != null) {
                position.stopPrice = partialExit.moveStopToPrice;
            }
            telegramNotifyService.sendMessage("OrderFlowScalpingStrategy PARTIAL " + state.tickerInfo.getTicker() + " qty=" + partialExit.quantity);
            syncProtectiveOrders(state, position);
            if (position.remainingQuantity <= 0) {
                return;
            }
        }
    }

    private void updateStopByScenario(ScalpingState state, LivePosition position, double currentPrice) {
        double move = "BUY".equals(position.direction) ? currentPrice - position.entryPrice : position.entryPrice - currentPrice;
        double tick = position.tickSize;

        if ("OBI".equals(position.reason)) {
            if (move >= tick * 2) {
                position.stopPrice = adjustedPrice(position.direction, position.entryPrice, tick, 1);
            }
            if (move >= tick * 4) {
                position.stopPrice = adjustedPrice(position.direction, position.entryPrice, tick, 2);
            }
            if (move > tick * 4) {
                position.stopPrice = trailingStop(position.direction, currentPrice, tick * config.getTrailingTicks(), position.stopPrice);
            }
            return;
        }

        if ("ICEBERG".equals(position.reason)) {
            if (position.firstPartialDone()) {
                position.stopPrice = adjustedPrice(position.direction, position.anchorPrice, tick, 1);
            }
            position.stopPrice = trailingStop(position.direction, currentPrice, tick * config.getIcebergTrailingTicks(), position.stopPrice);
            return;
        }

        if ("ABSORPTION".equals(position.reason)) {
            if (position.firstPartialDone()) {
                position.stopPrice = position.entryPrice;
            }
            position.stopPrice = trailingStop(position.direction, currentPrice, tick * config.getAbsorptionTrailingTicks(), position.stopPrice);
            return;
        }

        if (move >= tick * 2) {
            position.stopPrice = adjustedPrice(position.direction, position.entryPrice, tick, 1);
        }
        if (move >= tick * 4) {
            position.stopPrice = trailingStop(position.direction, currentPrice, tick * config.getTrailingTicks(), position.stopPrice);
        }
    }

    private double adjustedPrice(String direction, double base, double tick, int ticks) {
        return "BUY".equals(direction) ? base + tick * ticks : base - tick * ticks;
    }

    private double trailingStop(String direction, double currentPrice, double offset, double currentStop) {
        if ("BUY".equals(direction)) {
            return max(currentStop, currentPrice - offset);
        }
        return min(currentStop, currentPrice + offset);
    }

    private void syncProtectiveOrders(ScalpingState state, LivePosition position) {
        if (position.remainingQuantity <= 0) {
            tcsService.clearProtectiveOrders(state.tickerInfo.getTicker(), state.tickerInfo.getType());
            return;
        }
        tcsService.syncProtectiveOrders(
                state.tickerInfo.getTicker(),
                state.tickerInfo.getType(),
                new Position(position.direction, position.entryPrice, position.stopPrice, position.takePrice, position.remainingQuantity, 0)
        );
    }

    private boolean shouldCloseBySignalInvalidation(ScalpingState state, LivePosition position, double currentPrice) {
        double obi = calcObi(state.lastOrderBook, config.getImbalanceThresholdLevels());
        if ("BUY".equals(position.direction) && obi < 0.0 && "OBI".equals(position.reason)) {
            return true;
        }
        if ("SELL".equals(position.direction) && obi > 0.0 && "OBI".equals(position.reason)) {
            return true;
        }
        if ("OBI".equals(position.reason) && position.anchorPrice != null) {
            boolean anchorRemoved = "BUY".equals(position.direction)
                    ? !hasPriceLevel(state.lastOrderBook.getBids(), position.anchorPrice)
                    : !hasPriceLevel(state.lastOrderBook.getAsks(), position.anchorPrice);
            if (anchorRemoved && !position.firstPartialDone()) {
                return true;
            }
        }
        if ("SPOOF".equals(position.reason)
                && Duration.between(position.openTime, Instant.now()).getSeconds() >= config.getSpoofTimeoutSeconds()) {
            double move = "BUY".equals(position.direction) ? currentPrice - position.entryPrice : position.entryPrice - currentPrice;
            return move < position.tickSize;
        }
        if ("ICEBERG".equals(position.reason)) {
            IcebergCandidate activeSameSide = "BUY".equals(position.direction) ? state.activeBidIceberg : state.activeAskIceberg;
            if (activeSameSide == null || activeSameSide.replenishmentCycles < config.getIcebergMinReplenishments() - 1) {
                return true;
            }
            if (position.lastOppositeCvdFlipAt != null && Duration.between(position.lastOppositeCvdFlipAt, Instant.now()).getSeconds() > 10) {
                return true;
            }
        }
        if ("ABSORPTION".equals(position.reason) && hasOppositeAbsorption(state, position.direction)) {
            return true;
        }
        return false;
    }

    private boolean shouldCloseByTimeout(LivePosition position) {
        return Duration.between(position.openTime, Instant.now()).getSeconds() > config.getTimeoutSeconds();
    }

    private boolean hasOppositeAbsorption(ScalpingState state, String direction) {
        return "BUY".equals(direction)
                ? state.buyAggressionVolume > 0 && state.buyAggressionSlowdown && hasAbsorptionAskLevel(state)
                : state.sellAggressionVolume > 0 && state.sellAggressionSlowdown && hasAbsorptionBidLevel(state);
    }

    private boolean hasPriceLevel(List<MarketDepthLevel> levels, double price) {
        return levels.stream().anyMatch(level -> abs(level.getPrice() - price) <= 1e-9);
    }

    private void closeOpenPosition(ScalpingState state, double exitPrice, String reason) {
        LivePosition position = state.openPosition;
        if (position == null) {
            return;
        }
        boolean closed = "BUY".equals(position.direction)
                ? tcsService.closeLongByMarket(state.tickerInfo.getTicker(), state.tickerInfo.getType())
                : tcsService.closeShortByMarket(state.tickerInfo.getTicker(), state.tickerInfo.getType());
        if (!closed) {
            return;
        }
        tcsService.clearProtectiveOrders(state.tickerInfo.getTicker(), state.tickerInfo.getType());
        double pnl = "BUY".equals(position.direction)
                ? (exitPrice - position.entryPrice) * position.remainingQuantity
                : (position.entryPrice - exitPrice) * position.remainingQuantity;
        dailyPnl += pnl;
        state.openPosition = null;
        telegramNotifyService.sendMessage("OrderFlowScalpingStrategy CLOSE " + state.tickerInfo.getTicker() + " reason=" + reason + " pnl=" + String.format("%.2f", pnl));
    }

    private boolean shouldCloseByStop(LivePosition position, double currentPrice) {
        return "BUY".equals(position.direction) ? currentPrice <= position.stopPrice : currentPrice >= position.stopPrice;
    }

    private boolean shouldCloseByTake(LivePosition position, double currentPrice) {
        return "BUY".equals(position.direction) ? currentPrice >= position.takePrice : currentPrice <= position.takePrice;
    }

    private void cleanup(ScalpingState state) {
        Instant tradeThreshold = Instant.now().minus(Duration.ofSeconds(config.getVolumeHistorySeconds()));
        while (!state.recentTrades.isEmpty() && state.recentTrades.peekFirst().getTime().isBefore(tradeThreshold)) {
            state.recentTrades.pollFirst();
        }

        Instant priceThreshold = Instant.now().minusSeconds(config.getLocalWindowSeconds());
        while (!state.midPrices.isEmpty() && state.midPrices.peekFirst().time.isBefore(priceThreshold)) {
            state.midPrices.pollFirst();
        }

        Instant bookThreshold = Instant.now().minusSeconds(config.getBookHistorySeconds());
        while (!state.bookHistory.isEmpty() && state.bookHistory.peekFirst().time.isBefore(bookThreshold)) {
            state.bookHistory.pollFirst();
        }

        state.thirtySecondTrades.removeIf(it -> it.getTime().isBefore(Instant.now().minusSeconds(30)));
        recalculateLongWindow(state);
    }

    private boolean isRegimeTradable(ScalpingState state) {
        if (state.lastOrderBook == null) {
            return false;
        }

        Double bestBid = state.lastOrderBook.getBestBid();
        Double bestAsk = state.lastOrderBook.getBestAsk();
        if (bestBid == null || bestAsk == null || bestAsk <= bestBid) {
            return false;
        }

        double tickSize = max(state.tickerInfo.getMinPriceIncrement(), 0.01);
        double spreadTicks = (bestAsk - bestBid) / tickSize;
        long totalVolume = topBookVolume(state.lastOrderBook, 5);
        if (totalVolume < config.getLowLiquidityBookVolume()) {
            return false;
        }
        return spreadTicks <= config.getMaxSpreadTicks();
    }

    private long topBookVolume(MarketDepthSnapshot snapshot, int levels) {
        return snapshot.getBids().stream().limit(levels).mapToLong(MarketDepthLevel::getQuantity).sum()
                + snapshot.getAsks().stream().limit(levels).mapToLong(MarketDepthLevel::getQuantity).sum();
    }

    private boolean hasOpenPosition(ScalpingState state) {
        if (state.openPosition != null) {
            return true;
        }
        PositionInfo currentPosition = tcsService.getCurrentPositions(state.tickerInfo.getType(), state.tickerInfo.getTicker());
        return currentPosition != null && currentPosition.getBalance() != 0;
    }

    private int countOpenPositions() {
        return (int) stateByTicker.values().stream().filter(it -> it.openPosition != null).count();
    }

    private int countOpenPositions(TickerInfo tickerInfo) {
        return (int) stateByTicker.values().stream()
                .filter(it -> Objects.equals(it.tickerInfo.getTicker(), tickerInfo.getTicker()))
                .filter(it -> it.openPosition != null)
                .count();
    }

    private boolean isDailyLossLimitReached() {
        return startBalance > 0.0 && dailyPnl <= -startBalance * config.getDailyLossLimitPercent();
    }

    private double calcObi(MarketDepthSnapshot snapshot, int levels) {
        long bidVolume = snapshot.getBids().stream().limit(levels).mapToLong(MarketDepthLevel::getQuantity).sum();
        long askVolume = snapshot.getAsks().stream().limit(levels).mapToLong(MarketDepthLevel::getQuantity).sum();
        if (bidVolume + askVolume == 0) {
            return 0.0;
        }
        return (double) (bidVolume - askVolume) / (bidVolume + askVolume);
    }

    private boolean isLocalHigh(ScalpingState state, double price) {
        return state.midPrices.stream().mapToDouble(it -> it.price).max().orElse(price) <= price;
    }

    private boolean isLocalLow(ScalpingState state, double price) {
        return state.midPrices.stream().mapToDouble(it -> it.price).min().orElse(price) >= price;
    }

    private boolean hasLargeBestBid(ScalpingState state) {
        if (state.lastOrderBook.getBids().isEmpty()) {
            return false;
        }
        int bestBid = state.lastOrderBook.getBids().get(0).getQuantity();
        double avg = averageInt(state.bidVolumeHistory);
        return avg > 0.0 && bestBid >= avg * 3.0;
    }

    private boolean hasAbsorptionBidLevel(ScalpingState state) {
        if (state.lastOrderBook.getBids().isEmpty()) {
            return false;
        }
        double median = medianInt(state.bidVolumeHistory);
        return median > 0.0 && state.lastOrderBook.getBids().get(0).getQuantity() >= median * 3.0;
    }

    private boolean hasAbsorptionAskLevel(ScalpingState state) {
        if (state.lastOrderBook.getAsks().isEmpty()) {
            return false;
        }
        double median = medianInt(state.askVolumeHistory);
        return median > 0.0 && state.lastOrderBook.getAsks().get(0).getQuantity() >= median * 3.0;
    }

    private boolean hasLargeBestAsk(ScalpingState state) {
        if (state.lastOrderBook.getAsks().isEmpty()) {
            return false;
        }
        int bestAsk = state.lastOrderBook.getAsks().get(0).getQuantity();
        double avg = averageInt(state.askVolumeHistory);
        return avg > 0.0 && bestAsk >= avg * 3.0;
    }

    private double averageInt(Deque<Integer> values) {
        return values.stream().mapToInt(Integer::intValue).average().orElse(0.0);
    }

    private double medianInt(Deque<Integer> values) {
        if (values.isEmpty()) {
            return 0.0;
        }
        List<Integer> sorted = new ArrayList<>(values);
        sorted.sort(Integer::compareTo);
        int middle = sorted.size() / 2;
        if (sorted.size() % 2 == 0) {
            return (sorted.get(middle - 1) + sorted.get(middle)) / 2.0;
        }
        return sorted.get(middle);
    }

    private double getEntryPrice(ScalpingState state, String direction) {
        Double bestAsk = state.lastOrderBook.getBestAsk();
        Double bestBid = state.lastOrderBook.getBestBid();
        if ("BUY".equals(direction)) {
            return bestAsk != null ? bestAsk : 0.0;
        }
        return bestBid != null ? bestBid : 0.0;
    }

    @Override
    public void onOrderBook(MarketDepthSnapshot snapshot) {
        ScalpingState state = stateByTicker.values().stream()
                .filter(it -> it.tickerInfo.getFigi().equals(snapshot.getFigi()))
                .findFirst()
                .orElse(null);
        if (state == null) {
            return;
        }

        state.lastOrderBook = snapshot;
        state.bookHistory.addLast(new TimedBook(snapshot.getTime(), snapshot));

        Double mid = snapshot.getMidPrice();
        if (mid != null) {
            state.midPrices.addLast(new TimedPrice(Instant.now(), mid));
        }

        if (!snapshot.getBids().isEmpty()) {
            int bidQty = snapshot.getBids().get(0).getQuantity();
            state.bidVolumeHistory.addLast(bidQty);
            while (state.bidVolumeHistory.size() > config.getBookHistorySeconds()) {
                state.bidVolumeHistory.pollFirst();
            }
            state.lastVisibleBidAtSignal = bidQty;
        }

        if (!snapshot.getAsks().isEmpty()) {
            int askQty = snapshot.getAsks().get(0).getQuantity();
            state.askVolumeHistory.addLast(askQty);
            while (state.askVolumeHistory.size() > config.getBookHistorySeconds()) {
                state.askVolumeHistory.pollFirst();
            }
            state.lastVisibleAskAtSignal = askQty;
        }

        detectSpoofRemoval(state, snapshot);
        updateSegmentMedians(state, snapshot);
        updatePriceDrift(state);
        cleanupSpoofHistory(state);
    }

    @Override
    public void onTrade(MarketTradeTick trade) {
        ScalpingState state = stateByTicker.values().stream()
                .filter(it -> it.tickerInfo.getFigi().equals(trade.getFigi()))
                .findFirst()
                .orElse(null);
        if (state == null) {
            return;
        }

        state.recentTrades.addLast(trade);
        state.thirtySecondTrades.add(trade);
        if (trade.getDirection().contains("SELL")) {
            state.sellAggressionVolume += trade.getQuantity();
            state.cvdShortWindow -= trade.getQuantity();
        } else if (trade.getDirection().contains("BUY")) {
            state.buyAggressionVolume += trade.getQuantity();
            state.cvdShortWindow += trade.getQuantity();
        }

        Instant threshold = Instant.now().minusSeconds(10);
        while (!state.recentTrades.isEmpty() && state.recentTrades.peekFirst().getTime().isBefore(threshold)) {
            MarketTradeTick expired = state.recentTrades.pollFirst();
            if (expired.getDirection().contains("SELL")) {
                state.sellAggressionVolume -= expired.getQuantity();
                state.cvdShortWindow += expired.getQuantity();
            } else if (expired.getDirection().contains("BUY")) {
                state.buyAggressionVolume -= expired.getQuantity();
                state.cvdShortWindow -= expired.getQuantity();
            }
        }

        recalculateLongWindow(state);
        if (state.openPosition != null) {
            if (("BUY".equals(state.openPosition.direction) && state.cvdThirtySeconds < 0)
                    || ("SELL".equals(state.openPosition.direction) && state.cvdThirtySeconds > 0)) {
                state.openPosition.lastOppositeCvdFlipAt = Instant.now();
            } else {
                state.openPosition.lastOppositeCvdFlipAt = null;
            }
        }
    }

    @Override
    public void onError(Throwable throwable) {
        out.println("OrderFlowScalpingStrategy stream error: " + throwable.getMessage());
    }

    private void detectSpoofRemoval(ScalpingState state, MarketDepthSnapshot snapshot) {
        detectSpoofRemovalOnSide(state, snapshot.getBids(), true);
        detectSpoofRemovalOnSide(state, snapshot.getAsks(), false);
    }

    private void detectSpoofRemovalOnSide(ScalpingState state, List<MarketDepthLevel> levels, boolean bidSide) {
        if (levels.isEmpty()) {
            return;
        }

        double tick = max(state.tickerInfo.getMinPriceIncrement(), 0.01);
        Double bestPrice = bidSide ? state.lastOrderBook.getBestBid() : state.lastOrderBook.getBestAsk();
        if (bestPrice == null) {
            return;
        }

        for (int i = 0; i < levels.size(); i++) {
            MarketDepthLevel level = levels.get(i);
            int depthTicks = (int) Math.round(abs(level.getPrice() - bestPrice) / tick);
            if (depthTicks < config.getSpoofMinDepthTicks() || depthTicks > config.getSpoofMaxDepthTicks()) {
                continue;
            }

            double avgLevel = bidSide ? averageInt(state.bidVolumeHistory) : averageInt(state.askVolumeHistory);
            if (avgLevel <= 0.0 || level.getQuantity() < avgLevel * config.getSpoofLevelMultiplier()) {
                continue;
            }

            SpoofCandidate candidate = bidSide ? state.bidSpoofCandidate : state.askSpoofCandidate;
            if (candidate == null || abs(candidate.price - level.getPrice()) > tick * 0.5) {
                candidate = new SpoofCandidate(level.getPrice(), level.getQuantity(), Instant.now());
                if (bidSide) {
                    state.bidSpoofCandidate = candidate;
                } else {
                    state.askSpoofCandidate = candidate;
                }
            }

            if (candidate.visibleVolume > 0
                    && level.getQuantity() <= candidate.visibleVolume * 0.2
                    && Duration.between(candidate.detectedAt, Instant.now()).toMillis() <= config.getSpoofFastCancelMs()) {
                boolean likelyExecuted = tradedNearPrice(state, candidate.price, bidSide ? "SELL" : "BUY", candidate.visibleVolume * 0.1);
                if (bidSide) {
                    state.lastBidSpoofRemovedAt = Instant.now();
                    state.lastBidSpoofWasLarge = true;
                    state.lastBidSpoofLikelyExecuted = likelyExecuted;
                    state.bidSpoofHistory.addLast(candidate);
                } else {
                    state.lastAskSpoofRemovedAt = Instant.now();
                    state.lastAskSpoofWasLarge = true;
                    state.lastAskSpoofLikelyExecuted = likelyExecuted;
                    state.askSpoofHistory.addLast(candidate);
                }
            }
        }
    }

    private boolean tradedNearPrice(ScalpingState state, double price, String direction, double thresholdVolume) {
        long volume = state.recentTrades.stream()
                .filter(trade -> trade.getDirection().contains(direction))
                .filter(trade -> abs(trade.getPrice() - price) <= state.tickerInfo.getMinPriceIncrement() * 0.5)
                .mapToLong(MarketTradeTick::getQuantity)
                .sum();
        return volume >= thresholdVolume;
    }

    private boolean hasSpoofHistory(Deque<SpoofCandidate> history, Instant now) {
        return history.stream().filter(it -> Duration.between(it.detectedAt, now).getSeconds() <= 60).count() > 0;
    }

    private void cleanupSpoofHistory(ScalpingState state) {
        Instant threshold = Instant.now().minusSeconds(60);
        while (!state.bidSpoofHistory.isEmpty() && state.bidSpoofHistory.peekFirst().detectedAt.isBefore(threshold)) {
            state.bidSpoofHistory.pollFirst();
        }
        while (!state.askSpoofHistory.isEmpty() && state.askSpoofHistory.peekFirst().detectedAt.isBefore(threshold)) {
            state.askSpoofHistory.pollFirst();
        }
    }

    private long aggressiveVolumeAfterBreakout(ScalpingState state, double breakoutPrice, boolean breakoutUp) {
        Instant threshold = Instant.now().minusMillis(config.getBreakoutConfirmationMs());
        return state.recentTrades.stream()
                .filter(trade -> !trade.getTime().isBefore(threshold))
                .filter(trade -> breakoutUp ? trade.getDirection().contains("BUY") : trade.getDirection().contains("SELL"))
                .filter(trade -> breakoutUp ? trade.getPrice() >= breakoutPrice : trade.getPrice() <= breakoutPrice)
                .mapToLong(MarketTradeTick::getQuantity)
                .sum();
    }

    private long shortWindowCvdAfterBreakout(ScalpingState state, double breakoutPrice, boolean breakoutUp) {
        Instant threshold = Instant.now().minusSeconds(10);
        long cvd = 0L;
        for (MarketTradeTick trade : state.thirtySecondTrades) {
            if (trade.getTime().isBefore(threshold)) {
                continue;
            }
            if (breakoutUp && trade.getPrice() < breakoutPrice) {
                continue;
            }
            if (!breakoutUp && trade.getPrice() > breakoutPrice) {
                continue;
            }
            if (trade.getDirection().contains("BUY")) {
                cvd += trade.getQuantity();
            } else if (trade.getDirection().contains("SELL")) {
                cvd -= trade.getQuantity();
            }
        }
        return cvd;
    }

    private void updateSegmentMedians(ScalpingState state, MarketDepthSnapshot snapshot) {
        long bidSegment = snapshot.getBids().stream().limit(config.getStackedLevels()).mapToLong(MarketDepthLevel::getQuantity).sum();
        long askSegment = snapshot.getAsks().stream().limit(config.getStackedLevels()).mapToLong(MarketDepthLevel::getQuantity).sum();
        state.bidSegments.addLast(bidSegment);
        state.askSegments.addLast(askSegment);
        while (state.bidSegments.size() > config.getBookHistorySeconds()) {
            state.bidSegments.pollFirst();
        }
        while (state.askSegments.size() > config.getBookHistorySeconds()) {
            state.askSegments.pollFirst();
        }
        state.bidSegmentMedian = median(state.bidSegments);
        state.askSegmentMedian = median(state.askSegments);
    }

    private void updatePriceDrift(ScalpingState state) {
        if (state.midPrices.size() < 2) {
            state.priceDriftShortWindow = 0.0;
            return;
        }
        TimedPrice first = state.midPrices.peekFirst();
        TimedPrice last = state.midPrices.peekLast();
        state.priceDriftShortWindow = last.price - first.price;
    }

    private void recalculateLongWindow(ScalpingState state) {
        state.thirtySecondTrades.removeIf(it -> it.getTime().isBefore(Instant.now().minusSeconds(30)));
        state.cvdThirtySeconds = 0;
        long buyLastFiveSeconds = 0;
        long sellLastFiveSeconds = 0;
        Instant fiveSecondsThreshold = Instant.now().minusSeconds(5);

        for (MarketTradeTick trade : state.thirtySecondTrades) {
            if (trade.getDirection().contains("SELL")) {
                state.cvdThirtySeconds -= trade.getQuantity();
                if (!trade.getTime().isBefore(fiveSecondsThreshold)) {
                    sellLastFiveSeconds += trade.getQuantity();
                }
            } else if (trade.getDirection().contains("BUY")) {
                state.cvdThirtySeconds += trade.getQuantity();
                if (!trade.getTime().isBefore(fiveSecondsThreshold)) {
                    buyLastFiveSeconds += trade.getQuantity();
                }
            }
        }

        long buyPrev = max(1L, state.buyAggressionVolume - buyLastFiveSeconds);
        long sellPrev = max(1L, state.sellAggressionVolume - sellLastFiveSeconds);
        state.buyAggressionSlowdown = buyLastFiveSeconds * 100L <= buyPrev * 30L;
        state.sellAggressionSlowdown = sellLastFiveSeconds * 100L <= sellPrev * 30L;
    }

    private List<PartialExit> buildPartialPlan(Signal signal, double entryPrice, double tickSize, int quantity) {
        List<PartialExit> plan = new ArrayList<>();
        if (quantity <= 1) {
            return plan;
        }

        if ("ICEBERG".equals(signal.reason)) {
            int q1 = max(1, quantity / 3);
            int q2 = max(1, quantity / 3);
            int q3 = max(0, quantity - q1 - q2);
            plan.add(new PartialExit(q1, targetPrice(signal.direction, entryPrice, tickSize * 5), adjustedPrice(signal.direction, signal.anchorPrice, tickSize, 1)));
            plan.add(new PartialExit(q2, targetPrice(signal.direction, entryPrice, tickSize * 10), null));
            if (q3 > 0) {
                plan.add(new PartialExit(q3, targetPrice(signal.direction, entryPrice, tickSize * 15), null));
            }
            return plan;
        }

        if ("ABSORPTION".equals(signal.reason)) {
            int q1 = max(1, (int) Math.floor(quantity * 0.5));
            int q2 = max(1, (int) Math.floor(quantity * 0.3));
            int q3 = max(0, quantity - q1 - q2);
            plan.add(new PartialExit(q1, targetPrice(signal.direction, entryPrice, tickSize * 6), entryPrice));
            plan.add(new PartialExit(q2, targetPrice(signal.direction, entryPrice, tickSize * 12), null));
            if (q3 > 0) {
                plan.add(new PartialExit(q3, targetPrice(signal.direction, entryPrice, tickSize * 15), null));
            }
            return plan;
        }

        if ("OBI".equals(signal.reason)) {
            int q1 = max(1, quantity / 2);
            int q2 = max(0, quantity - q1);
            plan.add(new PartialExit(q1, targetPrice(signal.direction, entryPrice, tickSize * 4), adjustedPrice(signal.direction, entryPrice, tickSize, 2)));
            if (q2 > 0) {
                plan.add(new PartialExit(q2, targetPrice(signal.direction, entryPrice, tickSize * config.getImbalanceTakeTicksMax()), null));
            }
            return plan;
        }

        if ("SPOOF".equals(signal.reason)) {
            plan.add(new PartialExit(quantity, targetPrice(signal.direction, entryPrice, tickSize * config.getSpoofTakeTicks()), null));
            return plan;
        }

        int q1 = max(1, quantity / 2);
        int q2 = max(0, quantity - q1);
        plan.add(new PartialExit(q1, targetPrice(signal.direction, entryPrice, tickSize * 5), adjustedPrice(signal.direction, entryPrice, tickSize, 1)));
        if (q2 > 0) {
            plan.add(new PartialExit(q2, targetPrice(signal.direction, entryPrice, tickSize * 10), null));
        }
        return plan;
    }

    private double targetPrice(String direction, double entryPrice, double delta) {
        return "BUY".equals(direction) ? entryPrice + delta : entryPrice - delta;
    }

    private void forceCloseAllPositions() {
        stateByTicker.values().forEach(state -> {
            if (state.openPosition == null) {
                return;
            }
            double exitPrice = getEntryPrice(state, state.openPosition.direction);
            closeOpenPosition(state, exitPrice, "shutdown");
        });
    }

    private double median(Deque<Long> values) {
        if (values.isEmpty()) {
            return 0.0;
        }
        List<Long> sorted = new ArrayList<>(values);
        sorted.sort(Long::compareTo);
        int middle = sorted.size() / 2;
        if (sorted.size() % 2 == 0) {
            return (sorted.get(middle - 1) + sorted.get(middle)) / 2.0;
        }
        return sorted.get(middle);
    }

    private enum MarketRegime {
        TREND, FLAT, HIGH_VOLATILITY, LOW_LIQUIDITY
    }

    private static class ScalpingState {

        private final TickerInfo tickerInfo;
        private final Deque<MarketTradeTick> recentTrades = new ArrayDeque<>();
        private final Deque<MarketTradeTick> thirtySecondTrades = new ArrayDeque<>();
        private final Deque<TimedPrice> midPrices = new ArrayDeque<>();
        private final Deque<TimedBook> bookHistory = new ArrayDeque<>();
        private final Deque<Integer> bidVolumeHistory = new ArrayDeque<>();
        private final Deque<Integer> askVolumeHistory = new ArrayDeque<>();
        private final Deque<Long> bidSegments = new ArrayDeque<>();
        private final Deque<Long> askSegments = new ArrayDeque<>();
        private final Deque<SpoofCandidate> bidSpoofHistory = new ArrayDeque<>();
        private final Deque<SpoofCandidate> askSpoofHistory = new ArrayDeque<>();
        private MarketDepthSnapshot lastOrderBook;
        private LivePosition openPosition;
        private MarketRegime marketRegime = MarketRegime.FLAT;
        private Instant imbalancePositiveSince;
        private Instant imbalanceNegativeSince;
        private Double imbalanceAnchorBidPrice;
        private Double imbalanceAnchorAskPrice;
        private long buyAggressionVolume;
        private long sellAggressionVolume;
        private long cvdShortWindow;
        private long cvdThirtySeconds;
        private double priceDriftShortWindow;
        private long lastVisibleBidAtSignal;
        private long lastVisibleAskAtSignal;
        private double bidSegmentMedian;
        private double askSegmentMedian;
        private boolean buyAggressionSlowdown;
        private boolean sellAggressionSlowdown;
        private Instant lastBidSpoofRemovedAt;
        private Instant lastAskSpoofRemovedAt;
        private boolean lastBidSpoofWasLarge;
        private boolean lastAskSpoofWasLarge;
        private boolean lastBidSpoofLikelyExecuted;
        private boolean lastAskSpoofLikelyExecuted;
        private SpoofCandidate bidSpoofCandidate;
        private SpoofCandidate askSpoofCandidate;
        private IcebergCandidate activeBidIceberg;
        private IcebergCandidate activeAskIceberg;
        private StackedLevel bidStack;
        private StackedLevel askStack;
        private Instant bidStackSince;
        private Instant askStackSince;

        private ScalpingState(TickerInfo tickerInfo) {
            this.tickerInfo = tickerInfo;
        }
    }

    private static class LivePosition {

        private final String direction;
        private final String reason;
        private final double entryPrice;
        private final int quantity;
        private int remainingQuantity;
        private final Instant openTime;
        private double stopPrice;
        private final double takePrice;
        private final double tickSize;
        private final List<PartialExit> partialExits;
        private final Double anchorPrice;
        private Instant lastOppositeCvdFlipAt;

        private LivePosition(String direction,
                             String reason,
                             double entryPrice,
                             int quantity,
                             Instant openTime,
                             double stopPrice,
                             double takePrice,
                             double tickSize,
                             List<PartialExit> partialExits,
                             Double anchorPrice) {
            this.direction = direction;
            this.reason = reason;
            this.entryPrice = entryPrice;
            this.quantity = quantity;
            this.remainingQuantity = quantity;
            this.openTime = openTime;
            this.stopPrice = stopPrice;
            this.takePrice = takePrice;
            this.tickSize = tickSize;
            this.partialExits = partialExits;
            this.anchorPrice = anchorPrice;
        }

        private boolean firstPartialDone() {
            return !partialExits.isEmpty() && partialExits.get(0).executed;
        }
    }

    private static class PartialExit {

        private final int quantity;
        private final double price;
        private final Double moveStopToPrice;
        private boolean executed;

        private PartialExit(int quantity, double price, Double moveStopToPrice) {
            this.quantity = quantity;
            this.price = price;
            this.moveStopToPrice = moveStopToPrice;
        }
    }

    private static class Signal {

        private final String direction;
        private final String reason;
        private final double confidence;
        private final double entryPrice;
        private final double stopPrice;
        private final double takePrice;
        private final double riskPercent;
        private final boolean useLimitEntry;
        private final Double anchorPrice;

        private Signal(String direction,
                       String reason,
                       double confidence,
                       double entryPrice,
                       double stopPrice,
                       double takePrice,
                       double riskPercent,
                       boolean useLimitEntry,
                       Double anchorPrice) {
            this.direction = direction;
            this.reason = reason;
            this.confidence = confidence;
            this.entryPrice = entryPrice;
            this.stopPrice = stopPrice;
            this.takePrice = takePrice;
            this.riskPercent = riskPercent;
            this.useLimitEntry = useLimitEntry;
            this.anchorPrice = anchorPrice;
        }

        private static Signal imbalance(String direction, double entryPrice, double stopPrice, double riskPercent, Double anchorPrice) {
            double takePrice = "BUY".equals(direction) ? entryPrice + abs(entryPrice - stopPrice) * 2.0 : entryPrice - abs(entryPrice - stopPrice) * 2.0;
            return new Signal(direction, "OBI", 0.70, entryPrice, stopPrice, takePrice, riskPercent, false, anchorPrice);
        }

        private static Signal iceberg(String direction, double entryPrice, double stopPrice, double riskPercent, double anchorPrice) {
            double takePrice = "BUY".equals(direction) ? entryPrice + abs(entryPrice - stopPrice) * 5.0 : entryPrice - abs(entryPrice - stopPrice) * 5.0;
            return new Signal(direction, "ICEBERG", 0.86, entryPrice, stopPrice, takePrice, riskPercent, true, anchorPrice);
        }

        private static Signal spoof(String direction, double entryPrice, double stopPrice, double riskPercent) {
            double takePrice = "BUY".equals(direction) ? entryPrice + abs(entryPrice - stopPrice) * 2.0 : entryPrice - abs(entryPrice - stopPrice) * 2.0;
            return new Signal(direction, "SPOOF", 0.80, entryPrice, stopPrice, takePrice, riskPercent, false, null);
        }

        private static Signal absorption(String direction, double entryPrice, double stopPrice, double riskPercent) {
            double takePrice = "BUY".equals(direction) ? entryPrice + abs(entryPrice - stopPrice) * 6.0 : entryPrice - abs(entryPrice - stopPrice) * 6.0;
            return new Signal(direction, "ABSORPTION", 0.83, entryPrice, stopPrice, takePrice, riskPercent, true, entryPrice);
        }

        private static Signal breakout(String direction, double entryPrice, double stopPrice, double riskPercent) {
            double takePrice = "BUY".equals(direction) ? entryPrice + abs(entryPrice - stopPrice) * 3.0 : entryPrice - abs(entryPrice - stopPrice) * 3.0;
            return new Signal(direction, "STACKED_BREAKOUT", 0.88, entryPrice, stopPrice, takePrice, riskPercent, false, null);
        }
    }

    private static class TimedPrice {

        private final Instant time;
        private final double price;

        private TimedPrice(Instant time, double price) {
            this.time = time;
            this.price = price;
        }
    }

    private static class TimedBook {

        private final Instant time;
        private final MarketDepthSnapshot snapshot;

        private TimedBook(Instant time, MarketDepthSnapshot snapshot) {
            this.time = time;
            this.snapshot = snapshot;
        }
    }

    private static class IcebergCandidate {

        private final double price;
        private final int visibleVolume;
        private final long aggressiveVolume;
        private final int replenishmentCycles;

        private IcebergCandidate(double price, int visibleVolume, long aggressiveVolume, int replenishmentCycles) {
            this.price = price;
            this.visibleVolume = visibleVolume;
            this.aggressiveVolume = aggressiveVolume;
            this.replenishmentCycles = replenishmentCycles;
        }
    }

    private static class SpoofCandidate {

        private final double price;
        private final int visibleVolume;
        private final Instant detectedAt;

        private SpoofCandidate(double price, int visibleVolume, Instant detectedAt) {
            this.price = price;
            this.visibleVolume = visibleVolume;
            this.detectedAt = detectedAt;
        }
    }

    private static class StackedLevel {

        private final double basePrice;
        private final double breakoutPrice;
        private final long breakoutVisibleVolume;
        private final long persistedMs;
        private final boolean remainingVolumeCollapsed;

        private StackedLevel(double basePrice,
                             double breakoutPrice,
                             long breakoutVisibleVolume,
                             long persistedMs,
                             boolean remainingVolumeCollapsed) {
            this.basePrice = basePrice;
            this.breakoutPrice = breakoutPrice;
            this.breakoutVisibleVolume = breakoutVisibleVolume;
            this.persistedMs = persistedMs;
            this.remainingVolumeCollapsed = remainingVolumeCollapsed;
        }
    }
}
