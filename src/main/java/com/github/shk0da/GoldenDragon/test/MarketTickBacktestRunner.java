package com.github.shk0da.GoldenDragon.test;

import com.github.shk0da.GoldenDragon.config.DataCollectorConfig;
import com.github.shk0da.GoldenDragon.config.MainConfig;
import com.github.shk0da.GoldenDragon.config.MarketConfig;
import com.github.shk0da.GoldenDragon.config.OrderFlowScalpingConfig;
import com.github.shk0da.GoldenDragon.model.Market;
import com.github.shk0da.GoldenDragon.model.MarketDepthLevel;
import com.github.shk0da.GoldenDragon.model.MarketDepthSnapshot;
import com.github.shk0da.GoldenDragon.model.Position;
import com.github.shk0da.GoldenDragon.model.PositionInfo;
import com.github.shk0da.GoldenDragon.model.TickerCandle;
import com.github.shk0da.GoldenDragon.model.TickerInfo;
import com.github.shk0da.GoldenDragon.model.TickerInfo.Key;
import com.github.shk0da.GoldenDragon.model.TickerType;
import com.github.shk0da.GoldenDragon.repository.Repository;
import com.github.shk0da.GoldenDragon.repository.TickerRepository;
import com.github.shk0da.GoldenDragon.service.TCSService;
import com.github.shk0da.GoldenDragon.strategy.DataCollector;
import com.github.shk0da.GoldenDragon.strategy.OrderFlowScalpingStrategy;
import com.github.shk0da.GoldenDragon.utils.TickerTypeResolver;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.stream.Stream;
import ru.tinkoff.piapi.contract.v1.CandleInterval;

public class MarketTickBacktestRunner {

    private static final Repository<Key, TickerInfo> tickerRepository = TickerRepository.INSTANCE;

    private static final DateTimeFormatter DATE_TIME_FMT = DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm:ss");
    private static final String HEADER = "time,best_bid,best_ask,mid_price,bids,asks";
    private static final String CANDLES_HEADER = "Datetime,Open,High,Low,Close,Volume";
    private static final double INITIAL_BALANCE = 1_000_000.0;
    private static final double COMMISSION_RATE = 0.0005;

    private final String dataDir;
    private final double initialBalance;
    private final double commissionRate;

    public MarketTickBacktestRunner() {
        this("data", INITIAL_BALANCE, COMMISSION_RATE);
    }

    public MarketTickBacktestRunner(String dataDir, double initialBalance, double commissionRate) {
        this.dataDir = dataDir;
        this.initialBalance = initialBalance;
        this.commissionRate = commissionRate;
    }

    public static void main(String[] args) throws Exception {
        new MarketTickBacktestRunner().run();
    }

    public void run() throws Exception {
        OrderFlowScalpingConfig config = new OrderFlowScalpingConfig();
        List<OrderFlowScalpingConfig.Instrument> instruments = config.getInstruments();
        if (instruments.isEmpty()) {
            System.out.println("MarketTickBacktestRunner: no configured instruments");
            return;
        }

        Map<String, InstrumentTicks> ticksByTicker = loadTicks(instruments);
        if (ticksByTicker.isEmpty()) {
            System.out.println("MarketTickBacktestRunner: no ticks found in " + dataDir);
            return;
        }

        DataCollector dataCollector = new DataCollector(
                new DataCollectorConfig(),
                new TCSService(new MainConfig(), MarketConfig.byMarket(Market.MOEX))
        );
        SimulationGateway gateway = new SimulationGateway(dataDir, initialBalance, commissionRate, dataCollector);
        OrderFlowScalpingStrategy strategy = new OrderFlowScalpingStrategy(config, gateway);
        ticksByTicker.values().forEach(it -> strategy.registerTicker(it.tickerInfo));

        List<TimelineEvent> timeline = buildTimeline(ticksByTicker);
        for (TimelineEvent event : timeline) {
            gateway.updateSnapshot(event.instrumentTicks.tickerInfo, event.tickRow.snapshot);
            strategy.processBacktestTick(event.instrumentTicks.tickerInfo, event.tickRow.snapshot);
        }

        gateway.closeAll(timeline.isEmpty() ? Instant.now() : timeline.get(timeline.size() - 1).tickRow.snapshot.getTime());
        printSummary(ticksByTicker, gateway, strategy);
    }

    private void printSummary(Map<String, InstrumentTicks> ticksByTicker,
                              SimulationGateway gateway,
                              OrderFlowScalpingStrategy strategy) {
        long totalTicks = ticksByTicker.values().stream().mapToLong(it -> it.ticks.size()).sum();
        System.out.println("MarketTickBacktestRunner loaded " + totalTicks + " ticks for " + ticksByTicker.size() + " tickers");
        for (InstrumentTicks instrumentTicks : ticksByTicker.values()) {
            if (instrumentTicks.ticks.isEmpty()) {
                continue;
            }
            TickRow first = instrumentTicks.ticks.get(0);
            TickRow last = instrumentTicks.ticks.get(instrumentTicks.ticks.size() - 1);
            System.out.println(instrumentTicks.tickerInfo.getTicker() + ": " + instrumentTicks.ticks.size() + " ticks from " + first.time + " to " + last.time);
        }

        double finalEquity = gateway.getEquity();
        double pnl = finalEquity - initialBalance;
        long trades = gateway.getClosedTrades();
        double winRate = trades == 0 ? 0.0 : gateway.getWinningTrades() * 100.0 / trades;

        System.out.println("=".repeat(120));
        System.out.println("OrderFlowScalpingStrategy tick backtest summary");
        System.out.println("Initial balance: " + initialBalance);
        System.out.println("Final equity:    " + round2(finalEquity));
        System.out.println("PnL:             " + round2(pnl));
        System.out.println("Trades:          " + trades);
        System.out.println("Win rate:        " + round2(winRate) + "%");
        System.out.println("Max drawdown:    " + round2(gateway.getMaxDrawdownPercent()) + "%");
        System.out.println("Strategy dailyPnl: " + round2(strategy.getDailyPnl()));
        System.out.println("=".repeat(120));
    }

    private List<TimelineEvent> buildTimeline(Map<String, InstrumentTicks> ticksByTicker) {
        List<TimelineEvent> timeline = new ArrayList<>();
        for (InstrumentTicks instrumentTicks : ticksByTicker.values()) {
            for (TickRow tick : instrumentTicks.ticks) {
                timeline.add(new TimelineEvent(instrumentTicks, tick));
            }
        }
        timeline.sort(Comparator.comparing(it -> it.tickRow.time));
        return timeline;
    }

    private Map<String, InstrumentTicks> loadTicks(List<OrderFlowScalpingConfig.Instrument> instruments) throws IOException {
        Map<String, InstrumentTicks> result = new LinkedHashMap<>();
        for (OrderFlowScalpingConfig.Instrument instrument : instruments) {
            Path ticksPath = Path.of(dataDir, instrument.getTicker(), "ticks.txt");
            if (!Files.exists(ticksPath)) {
                continue;
            }

            TickerInfo tickerInfo = resolveTickerInfo(instrument);
            if (tickerInfo == null) {
                System.out.println("Warn: ticker info not found for " + instrument);
                continue;
            }

            List<TickRow> ticks = readTicks(ticksPath, tickerInfo);
            if (!ticks.isEmpty()) {
                result.put(instrument.getTicker(), new InstrumentTicks(tickerInfo, ticks));
            }
        }
        return result;
    }

    private TickerInfo resolveTickerInfo(OrderFlowScalpingConfig.Instrument instrument) {
        String ticker = instrument.getTicker();
        TickerType type = instrument.getType();
        
        TickerInfo.Key key = new TickerInfo.Key(ticker, type);
        TickerInfo tickerInfo = tickerRepository.getById(key);
        if (tickerInfo != null) {
            return tickerInfo;
        }
        
        tickerInfo = TickerTypeResolver.resolveTickerInfo(ticker);
        if (tickerInfo != null) {
            return tickerInfo;
        }
        
        tickerInfo = tickerRepository.getAll().values().stream()
                .filter(it -> it.getName().equalsIgnoreCase(ticker) || it.getTicker().equalsIgnoreCase(ticker))
                .findFirst()
                .orElse(null);
        
        return tickerInfo;
    }

    private List<TickRow> readTicks(Path ticksPath, TickerInfo tickerInfo) throws IOException {
        List<TickRow> result = new ArrayList<>();
        try (Stream<String> lines = Files.lines(ticksPath)) {
            lines.filter(line -> !line.isBlank())
                    .filter(line -> !HEADER.equals(line))
                    .map(line -> parseTickRow(line, tickerInfo))
                    .sorted(Comparator.comparing(it -> it.time))
                    .forEach(result::add);
        }
        return result;
    }

    private TickRow parseTickRow(String line, TickerInfo tickerInfo) {
        String[] parts = splitCsv(line);
        if (parts.length != 6) {
            throw new IllegalArgumentException("Invalid tick row: " + line);
        }

        LocalDateTime time = LocalDateTime.parse(parts[0], DATE_TIME_FMT);
        List<MarketDepthLevel> bids = parseLevels(parts[4]);
        List<MarketDepthLevel> asks = parseLevels(parts[5]);
        return new TickRow(
                time,
                new MarketDepthSnapshot(
                        tickerInfo.getFigi(),
                        time.atZone(ZoneId.systemDefault()).toInstant(),
                        true,
                        bids,
                        asks
                )
        );
    }

    private String[] splitCsv(String line) {
        List<String> parts = new ArrayList<>();
        StringBuilder current = new StringBuilder();
        boolean inQuotes = false;
        for (int i = 0; i < line.length(); i++) {
            char ch = line.charAt(i);
            if (ch == '"') {
                inQuotes = !inQuotes;
                continue;
            }
            if (ch == ',' && !inQuotes) {
                parts.add(current.toString());
                current.setLength(0);
                continue;
            }
            current.append(ch);
        }
        parts.add(current.toString());
        return parts.toArray(new String[0]);
    }

    private List<MarketDepthLevel> parseLevels(String raw) {
        List<MarketDepthLevel> levels = new ArrayList<>();
        if (raw == null || raw.isBlank()) {
            return levels;
        }
        String[] items = raw.split("\\|");
        for (String item : items) {
            String[] pair = item.split(":");
            if (pair.length != 2) {
                continue;
            }
            levels.add(new MarketDepthLevel(Double.parseDouble(pair[0]), Integer.parseInt(pair[1])));
        }
        return levels;
    }

    private double round2(double value) {
        return Math.round(value * 100.0) / 100.0;
    }

    private static class InstrumentTicks {

        private final TickerInfo tickerInfo;
        private final List<TickRow> ticks;

        private InstrumentTicks(TickerInfo tickerInfo, List<TickRow> ticks) {
            this.tickerInfo = tickerInfo;
            this.ticks = ticks;
        }
    }

    private static class TickRow {

        private final LocalDateTime time;
        private final MarketDepthSnapshot snapshot;

        private TickRow(LocalDateTime time, MarketDepthSnapshot snapshot) {
            this.time = time;
            this.snapshot = snapshot;
        }
    }

    private static class TimelineEvent {

        private final InstrumentTicks instrumentTicks;
        private final TickRow tickRow;

        private TimelineEvent(InstrumentTicks instrumentTicks, TickRow tickRow) {
            this.instrumentTicks = instrumentTicks;
            this.tickRow = tickRow;
        }
    }

    private static class SimulationGateway implements OrderFlowScalpingStrategy.TradingGateway {

        private final String dataDir;
        private final double commissionRate;
        private final DataCollector dataCollector;
        private final Map<String, SimulatedPosition> positionsByTicker = new LinkedHashMap<>();
        private final Map<String, MarketDepthSnapshot> lastSnapshotByTicker = new TreeMap<>();
        private final Map<String, List<TickerCandle>> hourCandlesByTicker = new HashMap<>();
        private double cash;
        private double peakEquity;
        private double maxDrawdownPercent;
        private long closedTrades;
        private long winningTrades;

        private SimulationGateway(String dataDir, double initialBalance, double commissionRate, DataCollector dataCollector) {
            this.dataDir = dataDir;
            this.cash = initialBalance;
            this.peakEquity = initialBalance;
            this.commissionRate = commissionRate;
            this.dataCollector = dataCollector;
        }

        @Override
        public double getAvailableCash() {
            return cash;
        }

        @Override
        public TickerInfo searchTicker(TickerInfo.Key key) {
            TickerInfo tickerInfo = tickerRepository.getById(key);
            if (tickerInfo != null) {
                return tickerInfo;
            }
            return TickerTypeResolver.resolveTickerInfo(key.getTicker());
        }

        @Override
        public void subscribeMarketData(TickerInfo.Key key, int depth, com.github.shk0da.GoldenDragon.model.MarketTickListener listener) {
        }

        @Override
        public void unsubscribeMarketData(TickerInfo.Key key, com.github.shk0da.GoldenDragon.model.MarketTickListener listener) {
        }

        @Override
        public PositionInfo getCurrentPosition(TickerType type, String ticker) {
            SimulatedPosition position = positionsByTicker.get(ticker);
            if (position == null || position.quantity <= 0) {
                return null;
            }
            int balance = "BUY".equals(position.direction) ? position.quantity : -position.quantity;
            return new PositionInfo(
                    position.tickerInfo.getFigi(),
                    ticker,
                    position.tickerInfo.getIsin(),
                    type.name(),
                    balance,
                    0.0,
                    position.quantity,
                    position.entryPrice,
                    position.tickerInfo.getName()
            );
        }

        @Override
        public List<TickerCandle> getCandles(TickerInfo.Key key, int count) {
            if (count <= 0) {
                return Collections.emptyList();
            }
            MarketDepthSnapshot snapshot = lastSnapshotByTicker.get(key.getTicker());
            if (snapshot == null) {
                return Collections.emptyList();
            }
            List<TickerCandle> candles = ensureHourCandlesLoaded(key.getTicker(), count, snapshot);
            if (candles.isEmpty()) {
                return Collections.emptyList();
            }

            LocalDateTime snapshotTime = LocalDateTime.ofInstant(snapshot.getTime(), ZoneId.systemDefault());
            List<TickerCandle> availableCandles = new ArrayList<>();
            for (TickerCandle candle : candles) {
                LocalDateTime candleTime = LocalDateTime.parse(candle.getDate(), DATE_TIME_FMT);
                if (candleTime.isAfter(snapshotTime)) {
                    break;
                }
                availableCandles.add(candle);
            }
            if (availableCandles.isEmpty()) {
                return Collections.emptyList();
            }

            int fromIndex = Math.max(0, availableCandles.size() - count);
            return new ArrayList<>(availableCandles.subList(fromIndex, availableCandles.size()));
        }

        @Override
        public int calculateTradeCount(TickerInfo.Key key, double cashToUse, double price) {
            if (cashToUse <= 0.0 || price <= 0.0) {
                return 0;
            }
            TickerInfo tickerInfo = tickerRepository.getById(key);
            int lot = tickerInfo != null && tickerInfo.getLot() != null && tickerInfo.getLot() > 0 ? tickerInfo.getLot() : 1;
            double orderCost = lot * price * (1.0 + commissionRate);
            if (cashToUse < orderCost) {
                return 0;
            }
            int lots = (int) Math.floor(cashToUse / orderCost);
            return lots * lot;
        }

        private List<TickerCandle> ensureHourCandlesLoaded(String ticker, int count, MarketDepthSnapshot snapshot) {
            List<TickerCandle> candles = hourCandlesByTicker.computeIfAbsent(ticker, this::loadHourCandles);
            int availableCount = countAvailableCandles(candles, snapshot.getTime());
            if (availableCount >= count) {
                return candles;
            }

            try {
                dataCollector.updateCandlesFile(ticker, dataDir, CandleInterval.CANDLE_INTERVAL_HOUR, false);
                List<TickerCandle> reloadedCandles = loadHourCandles(ticker);
                hourCandlesByTicker.put(ticker, reloadedCandles);
                return reloadedCandles;
            } catch (Exception ex) {
                System.out.println("Warn: failed update candles for " + ticker + ": " + ex.getMessage());
                return candles;
            }
        }

        private int countAvailableCandles(List<TickerCandle> candles, Instant snapshotTime) {
            LocalDateTime time = LocalDateTime.ofInstant(snapshotTime, ZoneId.systemDefault());
            int count = 0;
            for (TickerCandle candle : candles) {
                LocalDateTime candleTime = LocalDateTime.parse(candle.getDate(), DATE_TIME_FMT);
                if (candleTime.isAfter(time)) {
                    break;
                }
                count++;
            }
            return count;
        }

        private List<TickerCandle> loadHourCandles(String ticker) {
            Path candlesPath = Path.of(dataDir, ticker, "candlesHOUR.txt");
            if (!Files.exists(candlesPath)) {
                return Collections.emptyList();
            }

            List<TickerCandle> result = new ArrayList<>();
            try (Stream<String> lines = Files.lines(candlesPath)) {
                lines.filter(line -> !line.isBlank())
                        .filter(line -> !CANDLES_HEADER.equals(line))
                        .map(this::parseCandleRow)
                        .filter(java.util.Objects::nonNull)
                        .sorted(Comparator.comparing(candle -> LocalDateTime.parse(candle.getDate(), DATE_TIME_FMT)))
                        .forEach(result::add);
            } catch (IOException ex) {
                System.out.println("Warn: failed read candles for " + ticker + ": " + ex.getMessage());
                return Collections.emptyList();
            }
            return result;
        }

        private TickerCandle parseCandleRow(String line) {
            String[] parts = line.split(",");
            if (parts.length < 6) {
                return null;
            }
            try {
                return new TickerCandle(
                        "",
                        parts[0],
                        Double.parseDouble(parts[1]),
                        Double.parseDouble(parts[2]),
                        Double.parseDouble(parts[3]),
                        Double.parseDouble(parts[4]),
                        Double.parseDouble(parts[4]),
                        Integer.parseInt(parts[5])
                );
            } catch (Exception ex) {
                return null;
            }
        }

        @Override
        public TCSService.OrderExecutionResult buy(TickerInfo tickerInfo, double cashToUse, double entryPrice, double takePrice, double stopPrice, boolean useLimitEntry) {
            MarketDepthSnapshot snapshot = lastSnapshotByTicker.get(tickerInfo.getTicker());
            Double bestAsk = snapshot != null ? snapshot.getBestAsk() : null;
            if (bestAsk == null || bestAsk <= 0.0) {
                return TCSService.OrderExecutionResult.failed();
            }
            int quantity = Math.max(1, (int) Math.floor(cashToUse / bestAsk));
            double executedNotional = bestAsk * quantity;
            double commission = executedNotional * commissionRate;
            if (cash < executedNotional + commission) {
                return TCSService.OrderExecutionResult.failed();
            }
            cash -= executedNotional + commission;
            positionsByTicker.put(tickerInfo.getTicker(), new SimulatedPosition(tickerInfo, "BUY", quantity, bestAsk));
            return TCSService.OrderExecutionResult.success(bestAsk, quantity, commission, null);
        }

        @Override
        public TCSService.OrderExecutionResult sell(TickerInfo tickerInfo, double cashToUse, double entryPrice, double takePrice, double stopPrice, boolean useLimitEntry) {
            MarketDepthSnapshot snapshot = lastSnapshotByTicker.get(tickerInfo.getTicker());
            Double bestBid = snapshot != null ? snapshot.getBestBid() : null;
            if (bestBid == null || bestBid <= 0.0) {
                return TCSService.OrderExecutionResult.failed();
            }
            int quantity = Math.max(1, (int) Math.floor(cashToUse / bestBid));
            double executedNotional = bestBid * quantity;
            double commission = executedNotional * commissionRate;
            if (cash < commission) {
                return TCSService.OrderExecutionResult.failed();
            }
            // For short sell: receive cash from sale, pay commission
            cash += executedNotional - commission;
            positionsByTicker.put(tickerInfo.getTicker(), new SimulatedPosition(tickerInfo, "SELL", quantity, bestBid, commission));
            return TCSService.OrderExecutionResult.success(bestBid, quantity, commission, null);
        }

        @Override
        public TCSService.OrderExecutionResult closeLong(TickerInfo tickerInfo) {
            SimulatedPosition position = positionsByTicker.get(tickerInfo.getTicker());
            if (position == null) {
                return TCSService.OrderExecutionResult.failed();
            }
            return closeLong(tickerInfo, position.quantity);
        }

        @Override
        public TCSService.OrderExecutionResult closeLong(TickerInfo tickerInfo, int count) {
            SimulatedPosition position = positionsByTicker.get(tickerInfo.getTicker());
            MarketDepthSnapshot snapshot = lastSnapshotByTicker.get(tickerInfo.getTicker());
            Double bestBid = snapshot != null ? snapshot.getBestBid() : null;
            if (position == null || bestBid == null || count <= 0) {
                return TCSService.OrderExecutionResult.failed();
            }
            int executedCount = Math.min(count, position.quantity);
            double proceeds = bestBid * executedCount;
            double commission = proceeds * commissionRate;
            cash += proceeds - commission;
            completeTrade(position, bestBid, executedCount, commission);
            return TCSService.OrderExecutionResult.success(bestBid, executedCount, commission, null);
        }

        @Override
        public TCSService.OrderExecutionResult closeShort(TickerInfo tickerInfo) {
            SimulatedPosition position = positionsByTicker.get(tickerInfo.getTicker());
            if (position == null) {
                return TCSService.OrderExecutionResult.failed();
            }
            return closeShort(tickerInfo, position.quantity);
        }

        @Override
        public TCSService.OrderExecutionResult closeShort(TickerInfo tickerInfo, int count) {
            SimulatedPosition position = positionsByTicker.get(tickerInfo.getTicker());
            MarketDepthSnapshot snapshot = lastSnapshotByTicker.get(tickerInfo.getTicker());
            Double bestAsk = snapshot != null ? snapshot.getBestAsk() : null;
            if (position == null || bestAsk == null || count <= 0) {
                return TCSService.OrderExecutionResult.failed();
            }
            int executedCount = Math.min(count, position.quantity);
            double buyback = bestAsk * executedCount;
            double commission = buyback * commissionRate;
            cash -= buyback + commission;
            completeTrade(position, bestAsk, executedCount, commission);
            return TCSService.OrderExecutionResult.success(bestAsk, executedCount, commission, null);
        }

        @Override
        public void syncProtectiveOrders(TickerInfo tickerInfo, Position position) {
        }

        @Override
        public void clearProtectiveOrders(TickerInfo tickerInfo) {
        }

        private void completeTrade(SimulatedPosition position, double exitPrice, int executedCount, double exitCommission) {
            double grossPnl = "BUY".equals(position.direction)
                    ? (exitPrice - position.entryPrice) * executedCount
                    : (position.entryPrice - exitPrice) * executedCount;
            double entryCommissionPart = position.allocateEntryCommission(executedCount);
            double netPnl = grossPnl - entryCommissionPart - exitCommission;
            closedTrades++;
            if (netPnl > 0.0) {
                winningTrades++;
            }
            position.quantity -= executedCount;
            if (position.quantity <= 0) {
                positionsByTicker.remove(position.tickerInfo.getTicker());
            }
        }

        private void updateSnapshot(TickerInfo tickerInfo, MarketDepthSnapshot snapshot) {
            lastSnapshotByTicker.put(tickerInfo.getTicker(), snapshot);
            double equity = getEquity();
            peakEquity = Math.max(peakEquity, equity);
            if (peakEquity > 0.0) {
                maxDrawdownPercent = Math.max(maxDrawdownPercent, (peakEquity - equity) / peakEquity * 100.0);
            }
        }

        private double getEquity() {
            double equity = cash;
            for (SimulatedPosition position : positionsByTicker.values()) {
                MarketDepthSnapshot snapshot = lastSnapshotByTicker.get(position.tickerInfo.getTicker());
                if (snapshot == null) {
                    continue;
                }
                if ("BUY".equals(position.direction) && snapshot.getBestBid() != null) {
                    // Long position: add market value of holdings
                    equity += snapshot.getBestBid() * position.quantity;
                } else if ("SELL".equals(position.direction) && snapshot.getBestAsk() != null) {
                    // Short position: subtract buyback cost (liability)
                    // Cash already includes proceeds from sale, so subtract current buyback cost
                    equity -= snapshot.getBestAsk() * position.quantity;
                }
            }
            return equity;
        }

        private void closeAll(Instant time) {
            List<SimulatedPosition> positions = new ArrayList<>(positionsByTicker.values());
            for (SimulatedPosition position : positions) {
                if ("BUY".equals(position.direction)) {
                    closeLong(position.tickerInfo);
                } else {
                    closeShort(position.tickerInfo);
                }
            }
            updateSnapshotOnClose(time);
        }

        private void updateSnapshotOnClose(Instant time) {
            double equity = getEquity();
            peakEquity = Math.max(peakEquity, equity);
            if (peakEquity > 0.0) {
                maxDrawdownPercent = Math.max(maxDrawdownPercent, (peakEquity - equity) / peakEquity * 100.0);
            }
        }

        private long getClosedTrades() {
            return closedTrades;
        }

        private long getWinningTrades() {
            return winningTrades;
        }

        private double getMaxDrawdownPercent() {
            return maxDrawdownPercent;
        }
    }

    private static class SimulatedPosition {

        private final TickerInfo tickerInfo;
        private final String direction;
        private final double entryPrice;
        private final double entryCommission;
        private int quantity;
        private double remainingEntryCommission;

        private SimulatedPosition(TickerInfo tickerInfo, String direction, int quantity, double entryPrice) {
            this(tickerInfo, direction, quantity, entryPrice, entryPrice * quantity * COMMISSION_RATE);
        }

        private SimulatedPosition(TickerInfo tickerInfo, String direction, int quantity, double entryPrice, double commission) {
            this.tickerInfo = tickerInfo;
            this.direction = direction;
            this.quantity = quantity;
            this.entryPrice = entryPrice;
            this.entryCommission = commission;
            this.remainingEntryCommission = commission;
        }

        private double allocateEntryCommission(int executedCount) {
            if (quantity <= 0) {
                return 0.0;
            }
            double allocated = remainingEntryCommission * executedCount / Math.max(executedCount, quantity + executedCount);
            remainingEntryCommission = Math.max(0.0, remainingEntryCommission - allocated);
            return allocated;
        }
    }
}
