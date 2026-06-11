package com.github.shk0da.GoldenDragon.test;

import com.github.shk0da.GoldenDragon.config.UnifiedTraderConfig;
import com.github.shk0da.GoldenDragon.model.Candle;
import com.github.shk0da.GoldenDragon.model.Config;
import com.github.shk0da.GoldenDragon.model.Position;
import com.github.shk0da.GoldenDragon.model.TickerInfo;
import com.github.shk0da.GoldenDragon.model.TickerType;
import com.github.shk0da.GoldenDragon.model.TradingDecision;
import com.github.shk0da.GoldenDragon.repository.TickerRepository;
import com.github.shk0da.GoldenDragon.service.TCSService;
import com.github.shk0da.GoldenDragon.strategy.BaseStrategy;
import com.github.shk0da.GoldenDragon.strategy.RegimeAwareStrategy;
import com.github.shk0da.GoldenDragon.strategy.RegimeAwareStrategyMl;
import com.github.shk0da.GoldenDragon.strategy.UnifiedStrategy;
import com.github.shk0da.GoldenDragon.utils.PropertiesUtils;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

/**
 * Движок бэктестинга торговых стратегий на исторических данных.
 *
 * <p>Класс симулирует исполнение одной или нескольких стратегий
 * ({@link UnifiedStrategy}, {@link RegimeAwareStrategy}, {@link RegimeAwareStrategyMl})
 * на массиве тикеров и временных периодов, имитируя реальную торговлю с учётом
 * комиссий, рабочих часов, EOD-закрытий и портфельного управления капиталом.
 * По завершении формирует сводную статистику и сравнительный рейтинг стратегий.</p>
 *
 * <h2>Точка входа</h2>
 * <ul>
 *   <li>{@link #main(String[])} — если передан аргумент, прогоняется одна
 *       указанная стратегия; иначе перебираются все из {@link #ALL_STRATEGIES}
 *       и в конце печатается сравнительная таблица.</li>
 *   <li>{@link #run(String)} — основной публичный метод запуска для конкретной
 *       стратегии: загружает тикеры, периоды, выполняет симуляцию и печатает
 *       результаты.</li>
 * </ul>
 *
 * <h2>Режимы и конфигурация</h2>
 * <ul>
 *   <li>{@code backtest.mode} (system property):
 *     <ul>
 *       <li>{@code "fast"} — 6 коротких периодов.</li>
 *       <li>иначе (по умолчанию {@code "full"}) — 6 годовых периодов.</li>
 *     </ul>
 *   </li>
 *   <li>{@code backtest.threads} — число потоков для параллельной загрузки данных
 *       (по умолчанию {@code availableProcessors - 1}).</li>
 *   <li>Начальный баланс: 1 000 000, комиссия: 0.05% (двусторонняя).</li>
 *   <li>Рабочие часы симуляции: {@link #WORK_START_TIME} (10:00) –
 *       {@link #EOD_CLOSE_TIME} (21:00), пн–пт.</li>
 *   <li>Минимум {@link #MIN_HOURS_REQUIRED} = 60 часовых свечей до старта
 *       принятия решений (warm-up для индикаторов).</li>
 * </ul>
 *
 * <h2>Загрузка тикеров и данных</h2>
 * <ol>
 *   <li>{@link #loadTickers()} — собирает список тикеров из properties:
 *       {@code datacollector.instruments} + ключи {@code unifiedTrader.ticker.<NAME>.*}.</li>
 *   <li>{@link #filterEnabledTickers} — отсев по флагу {@code params.enabled}.</li>
 *   <li>{@link #loadMarketDataParallel} — параллельная загрузка для всех тикеров
 *       через {@link ExecutorService} с пулом {@link #BACKTEST_THREADS}.</li>
 *   <li>{@link #loadCandles} / {@link #loadCandles5Min} — чтение CSV-файлов
 *       {@code candlesHOUR.txt} / {@code candles5_MIN.txt} с фильтрацией по
 *       диапазону дат и сортировкой по времени.</li>
 *   <li>Для каждого тикера формируется {@link MarketData}:
 *       часовые свечи, минутные свечи (или часовые, если
 *       {@code useMinuteCandles == false}) и их временные метки.</li>
 * </ol>
 *
 * <h2>Основной цикл симуляции ({@link #execute})</h2>
 * <p>Все тикеры обрабатываются <b>совместно в едином портфеле</b> с общим кэшем
 * ({@code sharedCash}). Алгоритм:</p>
 * <ol>
 *   <li>Создаётся отдельный экземпляр стратегии для каждого тикера через
 *       {@link StrategyFactory}.</li>
 *   <li>Строится <b>глобальный таймлайн</b> ({@link #buildGlobalTimeline}) —
 *       объединение всех минутных меток времени всех тикеров (TreeSet с
 *       хронологическим компаратором).</li>
 *   <li>Группировка тикеров по {@code allocationGroup} для построения
 *       peer-свечей при групповом подтверждении сигналов.</li>
 *   <li>На каждой временной метке глобального таймлайна:
 *     <ul>
 *       <li>Для каждого тикера, у которого свеча совпадает с текущим временем,
 *           вызывается логика обработки.</li>
 *       <li>Вне рабочих часов / в выходные: позиция принудительно закрывается
 *           один раз в день в {@link #EOD_CLOSE_TIME} с причиной {@code eod_close}.</li>
 *       <li>В рабочие часы: продвигается часовой индекс, формируются
 *           {@code hourHistory} и {@code minHistory}, строятся peer-свечи
 *           ({@link #buildCurrentPeerCandles}), вызывается {@link BaseStrategy#decide}.</li>
 *       <li>Результат решения применяется через {@link #applyPortfolioDecision}.</li>
 *     </ul>
 *   </li>
 *   <li>В конце каждой временной метки фиксируется суммарная эквити-кривая
 *       портфеля (кэш + рыночная стоимость всех открытых позиций).</li>
 *   <li>По завершении периода: все открытые позиции закрываются с причиной
 *       {@code period_end}.</li>
 * </ol>
 *
 * <h2>Управление позициями ({@link #applyPortfolioDecision})</h2>
 * <ul>
 *   <li><b>OPEN</b>: только {@code BUY}; проверяется отсутствие открытой позиции
 *       и достаточность кэша с учётом комиссии входа.
 *       Кэш уменьшается на {@code positionValue + commission}.</li>
 *   <li><b>CLOSE</b>: вызывается {@link #closePortfolioPosition} — расчёт PnL с
 *       двухсторонней комиссией ({@code (entryValue + exitValue) × commission}),
 *       запись {@link TradeResult}, регистрация результата в стратегии через
 *       {@code recordBacktestTradeOutcome}, возврат средств в кэш.</li>
 *   <li><b>HOLD</b>: только обновление состояния позиции (трейлинг SL, cooldown).</li>
 * </ul>
 *
 * <h2>Peer-свечи и групповое подтверждение</h2>
 * <p>{@link #buildCurrentPeerCandles} для каждого тикера с заданным
 * {@code allocationGroup} собирает срезы часовых свечей всех peer-инструментов
 * группы <b>до текущего момента времени</b> (через бинарный поиск
 * {@link #upperBound} по времени), что предотвращает look-ahead bias.
 * Карта передаётся стратегии через {@link BaseStrategy#setPeerCandles}.</p>
 *
 * <h2>Метрики и отчётность</h2>
 * <ul>
 *   <li>{@link TickerPeriodResult} — на тикер за период: PnL, max drawdown,
 *       win rate, эквити-кривая, список сделок.</li>
 *   <li>{@link PortfolioPeriodResult} — портфельные метрики за период.</li>
 *   <li>{@link #calcMaxDrawdownByEquity} — максимальная просадка как
 *       {@code (peak - equity) / peak}.</li>
 *   <li>{@link #calculateWinRate} / {@link #calculatePortfolioWinRate} —
 *       доля прибыльных сделок.</li>
 *   <li>{@link #printResults} — таблица «Тикер × Период» с PnL, DD, числом сделок
 *       и win rate; компактное форматирование ({@code K}/{@code M} для PnL,
 *       маркеры риска {@code * / ! / пробел} для DD).</li>
 * </ul>
 *
 * <h2>Сравнение стратегий</h2>
 * <p>При запуске без аргументов прогоняются все стратегии из {@link #ALL_STRATEGIES},
 * по каждой собираются {@link StrategyMetrics}:</p>
 * <ul>
 *   <li>Total PnL, средний Win Rate, Max Drawdown, общее число сделок.</li>
 *   <li><b>Sharpe Ratio</b>: {@code avgReturn / stdDev} доходностей по периодам.</li>
 *   <li><b>Profit Factor</b>: {@code grossProfit / grossLoss} по всем сделкам.</li>
 *   <li><b>Composite Score</b> ({@link #calculateScore}):
 *     <pre>
 *     score = WinRate% × 0.4
 *           + max(0, 20 - MaxDD%) × 0.3
 *           + max(0, Sharpe) × 10 × 0.2
 *           + min(PF, 3.0)    × 10 × 0.1
 *     </pre>
 *   </li>
 * </ul>
 * <p>Стратегии сортируются по score (по убыванию) и печатается «🏆 BEST STRATEGY».</p>
 *
 * <h2>Реалистичность симуляции</h2>
 * <ul>
 *   <li>Учёт двусторонней комиссии при каждой сделке.</li>
 *   <li>Принудительное закрытие позиций в EOD и в конце периода.</li>
 *   <li>Исключение нерабочих часов и выходных из принятия решений.</li>
 *   <li>Warm-up индикаторов (минимум 60 часовых свечей).</li>
 *   <li>Защита от look-ahead bias: все срезы данных формируются строго до
 *       текущего момента симуляции через {@code subList(0, idx+1)} и
 *       {@code upperBound}.</li>
 *   <li>Единый кэш для всего портфеля — конкуренция тикеров за капитал.</li>
 * </ul>
 *
 * <h2>Параллелизм</h2>
 * <ul>
 *   <li>Загрузка рыночных данных — параллельно через {@link ExecutorService}.</li>
 *   <li>Сама симуляция — однопоточная (требуется детерминированный порядок
 *       событий по глобальному таймлайну).</li>
 * </ul>
 *
 * <h2>Хуки интеграции со стратегией</h2>
 * <ul>
 *   <li>{@link BaseStrategy#recordBacktestTradeEntry} — вызывается при OPEN
 *       для записи входа в ML-pipeline.</li>
 *   <li>{@link BaseStrategy#recordBacktestTradeOutcome} — при CLOSE для записи
 *       результата сделки (PnL, SL, qty).</li>
 *   <li>{@link BaseStrategy#setPeerCandles} — обновление peer-данных перед
 *       каждым принятием решения.</li>
 * </ul>
 *
 * <h2>Ограничения текущей реализации</h2>
 * <ul>
 *   <li>Поддерживается только long-only торговля (OPEN с {@code BUY}).</li>
 *   <li>Срабатывание SL/TP внутри минутной свечи зависит от логики стратегии
 *       (high/low касание проверяется в {@code decide}).</li>
 *   <li>Проскальзывание ({@code slippage}) не моделируется — исполнение по
 *       {@code close} или {@code entryPrice} из решения.</li>
 * </ul>
 */
public class BacktestRunner {

    private static class StrategyFactory {
        public static BaseStrategy createStrategy(String strategyName, UnifiedTraderConfig config) {
            switch (strategyName) {
                case "UnifiedStrategy":
                    return new UnifiedStrategy(config, null, new Config(), true);
                case "RegimeAwareStrategy":
                    return new RegimeAwareStrategy(config, null, new Config(), true);
                case "RegimeAwareStrategyMl":
                    return new RegimeAwareStrategyMl(config, null, new Config(), true, true, true);
                default:
                    throw new IllegalArgumentException("Unknown strategy: " + strategyName);
            }
        }
    }

    private static final DateTimeFormatter DATE_FMT = DateTimeFormatter.ofPattern("dd.MM.yyyy");
    private static final DateTimeFormatter DATE_TIME_FMT = DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm:ss");

    private static final int MIN_HOURS_REQUIRED = 60;
    private static final LocalTime WORK_START_TIME = LocalTime.of(10, 0);
    private static final LocalTime EOD_CLOSE_TIME = LocalTime.of(21, 0);
    private static final String BACKTEST_MODE = System.getProperty("backtest.mode", "full");
    private static final int BACKTEST_THREADS = Math.max(
            1,
            Integer.getInteger("backtest.threads", Math.max(1, Runtime.getRuntime().availableProcessors() - 1))
    );

    private static final String[] ALL_STRATEGIES = {
            "UnifiedStrategy",
            "RegimeAwareStrategy",
    };

    public static class RawCandle {
        public final String time;
        public final double open;
        public final double high;
        public final double low;
        public final double close;
        public final long volume;
        public final LocalDateTime dateTime;

        public RawCandle(String time, double open, double high, double low, double close, long volume) {
            this.time = time;
            this.open = open;
            this.high = high;
            this.low = low;
            this.close = close;
            this.volume = volume;
            this.dateTime = LocalDateTime.parse(time, DATE_TIME_FMT);
        }
    }

    public static class TradeResult {
        public final String ticker;
        public final String dir;
        public final double entry;
        public final double exit;
        public final double pnl;
        public final String reason;
        public final String time;

        public TradeResult(String ticker, String dir, double entry, double exit, double pnl, String reason, String time) {
            this.ticker = ticker;
            this.dir = dir;
            this.entry = entry;
            this.exit = exit;
            this.pnl = pnl;
            this.reason = reason;
            this.time = time;
        }
    }

    private static class EquityPoint {
        final String time;
        final double equity;

        EquityPoint(String time, double equity) {
            this.time = time;
            this.equity = equity;
        }
    }

    private static class SimulateResult {
        final List<TradeResult> trades;
        final List<EquityPoint> equityCurve;
        final double finalBalance;

        SimulateResult(List<TradeResult> trades, List<EquityPoint> equityCurve, double finalBalance) {
            this.trades = trades;
            this.equityCurve = equityCurve;
            this.finalBalance = finalBalance;
        }
    }

    private static class TickerPeriodResult {
        final List<TradeResult> trades;
        final List<EquityPoint> equityCurve;
        final double pnl;
        final double dd;
        final double startBalance;
        final double winRate;

        TickerPeriodResult(List<TradeResult> trades,
                           List<EquityPoint> equityCurve,
                           double pnl,
                           double dd,
                           double startBalance,
                           double winRate) {
            this.trades = trades;
            this.equityCurve = equityCurve;
            this.pnl = pnl;
            this.dd = dd;
            this.startBalance = startBalance;
            this.winRate = winRate;
        }
    }

    private static class MarketData {
        final List<Candle> hourCandles;
        final List<RawCandle> minuteCandlesRaw;
        final List<Candle> minuteCandles;
        final List<LocalDateTime> minuteTimes;
        final List<LocalDateTime> hourTimes;

        MarketData(List<Candle> hourCandles,
                   List<RawCandle> minuteCandlesRaw,
                   List<Candle> minuteCandles,
                   List<LocalDateTime> minuteTimes,
                   List<LocalDateTime> hourTimes) {
            this.hourCandles = hourCandles;
            this.minuteCandlesRaw = minuteCandlesRaw;
            this.minuteCandles = minuteCandles;
            this.minuteTimes = minuteTimes;
            this.hourTimes = hourTimes;
        }
    }

    private static class PortfolioPositionState {
        Position position = new Position();
        double entryPrice = 0.0;
        double realizedPnl = 0.0;
        int hourIdx = -1;
        int lastSeenHourIdx = -1;
        LocalDate lastEodCloseDate = null;
    }

    private static class PortfolioPeriodResult {
        final double pnl;
        final double dd;
        final List<EquityPoint> equityCurve;
        final int totalTrades;
        final double winRate;

        PortfolioPeriodResult(double pnl, double dd, List<EquityPoint> equityCurve, int totalTrades, double winRate) {
            this.pnl = pnl;
            this.dd = dd;
            this.equityCurve = equityCurve;
            this.totalTrades = totalTrades;
            this.winRate = winRate;
        }
    }

    private static class ExecutionResult {
        final Map<String, TickerPeriodResult> tickerResults;
        final PortfolioPeriodResult portfolioResult;

        ExecutionResult(Map<String, TickerPeriodResult> tickerResults,
                        PortfolioPeriodResult portfolioResult) {
            this.tickerResults = tickerResults;
            this.portfolioResult = portfolioResult;
        }
    }

    private static class PeriodDefinition {
        final String start;
        final String endExclusive;
        final String label;

        PeriodDefinition(String start, String endExclusive, String label) {
            this.start = start;
            this.endExclusive = endExclusive;
            this.label = label;
        }
    }

    private static class MarketDataLoadResult {
        final String ticker;
        final List<Candle> hourCandles;
        final MarketData marketData;

        MarketDataLoadResult(String ticker,
                             List<Candle> hourCandles,
                             MarketData marketData) {
            this.ticker = ticker;
            this.hourCandles = hourCandles;
            this.marketData = marketData;
        }
    }

    private final String dataDir;
    private final double initialBalance;
    private final double commission;

    public BacktestRunner() {
        this("data", 1_000_000.0, 0.0005);
    }

    public BacktestRunner(String dataDir, double initialBalance, double commission) {
        this.dataDir = dataDir;
        this.initialBalance = initialBalance;
        this.commission = commission;
    }

    // Store results for comparison
    private static final Map<String, StrategyMetrics> strategyMetricsMap = new LinkedHashMap<>();

    public static void main(String[] args) throws IOException {
        BacktestRunner runner = new BacktestRunner();
        boolean singleStrategyRun = args != null && args.length > 0;

        String dataPath = "ml_strategy/data_pipeline/trades.csv";
        Files.deleteIfExists(Paths.get(dataPath));
        
        // Check if specific strategy is provided via args
        if (singleStrategyRun) {
            // Run for single strategy
            String strategyName = args[0];
            System.out.println("\n" + "=".repeat(100));
            System.out.println("RUNNING BACKTEST FOR STRATEGY: " + strategyName);
            System.out.println("=".repeat(100));
            runner.run(strategyName);
        } else {
            // Run for all strategies
            for (String strategyName : ALL_STRATEGIES) {
                System.out.println("\n" + "=".repeat(100));
                System.out.println("RUNNING BACKTEST FOR STRATEGY: " + strategyName);
                System.out.println("=".repeat(100));
                runner.run(strategyName);
            }
        }
        
        if (!singleStrategyRun) {
            System.out.println("\n" + "=".repeat(200));
            System.out.println("СРАВНИТЕЛЬНАЯ ТАБЛИЦА ЭФФЕКТИВНОСТИ СТРАТЕГИЙ");
            System.out.println("=".repeat(200));
            printStrategyComparison();
        }
    }

    public void run() throws IOException {
        run("UnifiedStrategy");
    }

    public void run(String strategyName) throws IOException {
        List<PeriodDefinition> periods = getPeriods();

        UnifiedTraderConfig config = new UnifiedTraderConfig();
        List<String> loadedTickers = loadTickers();
        List<String> activeTickers = filterEnabledTickers(loadedTickers, config);

        List<String> periodLabels = new ArrayList<>();
        Map<String, Map<String, TickerPeriodResult>> allData = new LinkedHashMap<>();
        Map<String, PortfolioPeriodResult> portfolioData = new LinkedHashMap<>();

        for (PeriodDefinition period : periods) {
            String start = period.start;
            String endExclusive = period.endExclusive;
            String label = period.label;

            periodLabels.add(label);

            ExecutionResult executionResult = execute(strategyName, start, endExclusive, activeTickers, config);
            Map<String, TickerPeriodResult> tickerResults = executionResult.tickerResults;
            allData.put(label, tickerResults);

            PortfolioPeriodResult portfolioResult = executionResult.portfolioResult;
            portfolioData.put(label, portfolioResult);
        }

        printResults(strategyName, periodLabels, allData, portfolioData, activeTickers);
        
        // Collect metrics for comparison
        collectStrategyMetrics(strategyName, allData, portfolioData);
    }

    private List<PeriodDefinition> getPeriods() {
        java.time.LocalDate today = java.time.LocalDate.now();
        
        if ("fast".equalsIgnoreCase(BACKTEST_MODE)) {
            // Last 6 months (monthly periods)
            List<PeriodDefinition> periods = new ArrayList<>();
            java.time.LocalDate currentMonthStart = today.withDayOfMonth(1);
            
            for (int i = 5; i >= 0; i--) {
                java.time.LocalDate monthStart = currentMonthStart.minusMonths(i);
                java.time.LocalDate monthEnd = monthStart.plusMonths(1);
                String label = String.format("%d.%02d", monthStart.getYear(), monthStart.getMonthValue());
                periods.add(new PeriodDefinition(
                        monthStart.toString(),
                        monthEnd.toString(),
                        label
                ));
            }
            return periods;
        }

        // Last 5 years (yearly periods)
        List<PeriodDefinition> periods = new ArrayList<>();
        int currentYear = today.getYear();
        
        for (int i = 4; i >= 0; i--) {
            int year = currentYear - i;
            String start = year + "-01-01";
            String end = year + "-12-31";
            String label = String.valueOf(year);
            periods.add(new PeriodDefinition(start, end, label));
        }
        return periods;
    }

    private void printResults(String strategyName,
                              List<String> periodLabels,
                              Map<String, Map<String, TickerPeriodResult>> allData,
                              Map<String, PortfolioPeriodResult> portfolioData,
                              List<String> allTickers) {
        System.out.println("\n" + "=".repeat(170));
        System.out.println("РЕЗУЛЬТАТЫ ПО ПЕРИОДАМ ДЛЯ СТРАТЕГИИ: " + strategyName);
        System.out.println("=".repeat(170));

        StringBuilder header = new StringBuilder();
        header.append(String.format("%-10s", "Тикер"));
        for (String label : periodLabels) {
            header.append(String.format(" %22s", label));
        }
        System.out.println(header);

        StringBuilder subHeader = new StringBuilder();
        subHeader.append(String.format("%-10s", ""));
        for (String ignored : periodLabels) {
            subHeader.append(String.format(" %6s %5s %4s %5s", "PnL", "DD%", "Trd", "WR%"));
        }
        System.out.println(subHeader);
        System.out.println("-".repeat(header.length()));

        for (String ticker : allTickers) {
            StringBuilder row = new StringBuilder();
            row.append(String.format("%-10s", ticker));
            boolean hasAny = false;

            for (String label : periodLabels) {
                Map<String, TickerPeriodResult> tickerData = allData.get(label);
                TickerPeriodResult result = tickerData != null ? tickerData.get(ticker) : null;

                if (result == null || result.trades.isEmpty()) {
                    row.append(String.format(" %22s", "—"));
                } else {
                    hasAny = true;
                    row.append(String.format(" %6s %5s %4d %5.1f",
                            formatCompactPnL(result.pnl),
                            formatCompactDD(result.dd * 100.0),
                            result.trades.size(),
                            result.winRate * 100.0));
                }
            }

            if (hasAny) {
                System.out.println(row);
            }
        }

        System.out.println("-".repeat(header.length()));

        StringBuilder portRow = new StringBuilder();
        portRow.append(String.format("%-10s", "ПОРТФЕЛЬ"));

        for (String label : periodLabels) {
            PortfolioPeriodResult result = portfolioData.get(label);
            if (result == null) {
                portRow.append(String.format(" %22s", "—"));
            } else {
                portRow.append(String.format(" %6s %5s %4d %5.1f",
                        formatCompactPnL(result.pnl),
                        formatCompactDD(result.dd * 100.0),
                        result.totalTrades,
                        result.winRate * 100.0));
            }
        }

        System.out.println(portRow);
        System.out.println("=".repeat(170));
    }

    private ExecutionResult execute(String strategyName,
                                    String start,
                                    String endExclusive,
                                    List<String> tickers,
                                    UnifiedTraderConfig config) throws IOException {
        if (tickers.isEmpty()) {
            return new ExecutionResult(Collections.emptyMap(), new PortfolioPeriodResult(0.0, 0.0, Collections.emptyList(), 0, 0.0));
        }

        Map<String, List<Candle>> allHourlyCandles = new LinkedHashMap<>();
        Map<String, MarketData> marketDataByTicker = new LinkedHashMap<>();
        Map<String, List<LocalDateTime>> peerTimesMap = new LinkedHashMap<>();

        List<MarketDataLoadResult> loadedMarketData = loadMarketDataParallel(tickers, config, start, endExclusive);
        for (MarketDataLoadResult loadResult : loadedMarketData) {
            allHourlyCandles.put(loadResult.ticker, loadResult.hourCandles);
            marketDataByTicker.put(loadResult.ticker, loadResult.marketData);
            peerTimesMap.put(loadResult.ticker, loadResult.marketData.hourTimes);
        }

        Map<String, List<String>> groupTickers = new LinkedHashMap<>();
        for (String ticker : allHourlyCandles.keySet()) {
            String allocGroup = config.getTickerParams(ticker).allocationGroup;
            if (allocGroup != null && !allocGroup.isEmpty()) {
                groupTickers.computeIfAbsent(allocGroup, k -> new ArrayList<>()).add(ticker);
            }
        }

        Map<String, TickerPeriodResult> tickerResults = new LinkedHashMap<>();

        if (marketDataByTicker.isEmpty()) {
            return new ExecutionResult(tickerResults, new PortfolioPeriodResult(0.0, 0.0, Collections.emptyList(), 0, 0.0));
        }

        Map<String, BaseStrategy> strategies = new LinkedHashMap<>();
        Map<String, PortfolioPositionState> positionStates = new LinkedHashMap<>();
        Map<String, List<TradeResult>> tradesByTicker = new LinkedHashMap<>();
        Map<String, List<EquityPoint>> equityByTicker = new LinkedHashMap<>();
        Map<String, Double> lastPriceByTicker = new LinkedHashMap<>();
        Map<String, Integer> minuteIndexByTicker = new LinkedHashMap<>();

        for (String ticker : marketDataByTicker.keySet()) {
            BaseStrategy strategy = StrategyFactory.createStrategy(strategyName, config);
            strategies.put(ticker, strategy);
            positionStates.put(ticker, new PortfolioPositionState());
            tradesByTicker.put(ticker, new ArrayList<>());
            equityByTicker.put(ticker, new ArrayList<>());
            minuteIndexByTicker.put(ticker, 0);
            lastPriceByTicker.put(ticker, marketDataByTicker.get(ticker).minuteCandles.get(0).close);
        }

        List<String> globalTimeline = buildGlobalTimeline(marketDataByTicker);
        double sharedCash = initialBalance;
        List<EquityPoint> portfolioEquity = new ArrayList<>();

        for (String time : globalTimeline) {
            LocalDateTime currentTime = LocalDateTime.parse(time, DATE_TIME_FMT);

            for (String ticker : marketDataByTicker.keySet()) {
                MarketData marketData = marketDataByTicker.get(ticker);
                int idx = minuteIndexByTicker.get(ticker);
                if (idx >= marketData.minuteTimes.size()) {
                    continue;
                }

                LocalDateTime tickerTime = marketData.minuteTimes.get(idx);
                if (!tickerTime.equals(currentTime)) {
                    continue;
                }

                BaseStrategy strategy = strategies.get(ticker);
                PortfolioPositionState state = positionStates.get(ticker);
                Candle current = marketData.minuteCandles.get(idx);
                lastPriceByTicker.put(ticker, current.close);

                if (!isTradingDay(currentTime.toLocalDate()) || !isWithinWorkingHours(currentTime.toLocalTime())) {
                    if (state.position.quantity > 0
                            && !currentTime.toLocalTime().isBefore(EOD_CLOSE_TIME)
                            && !currentTime.toLocalDate().equals(state.lastEodCloseDate)) {
                        sharedCash = closePortfolioPosition(
                                ticker, strategy, state, current.close, current.time, "eod_close", sharedCash, tradesByTicker.get(ticker)
                        );
                        state.lastEodCloseDate = currentTime.toLocalDate();
                    }

                    equityByTicker.get(ticker).add(new EquityPoint(current.time, tickerEquity(ticker, state, current.close)));
                    minuteIndexByTicker.put(ticker, idx + 1);
                    continue;
                }

                while (state.hourIdx + 1 < marketData.hourTimes.size() && !marketData.hourTimes.get(state.hourIdx + 1).isAfter(currentTime)) {
                    state.hourIdx++;
                }

                boolean hourChanged = state.hourIdx != state.lastSeenHourIdx;
                equityByTicker.get(ticker).add(new EquityPoint(current.time, tickerEquity(ticker, state, current.close)));

                if (state.hourIdx + 1 >= MIN_HOURS_REQUIRED) {
                    List<Candle> hourHistory = marketData.hourCandles.subList(0, state.hourIdx + 1);
                    List<Candle> minHistory = marketData.minuteCandles.subList(0, idx + 1);

                    Map<String, List<Candle>> currentPeerCandles = buildCurrentPeerCandles(
                            ticker, currentTime, allHourlyCandles, groupTickers, peerTimesMap, hourHistory, config
                    );
                    strategy.setPeerCandles(currentPeerCandles.isEmpty() ? Collections.emptyMap() : currentPeerCandles);

                    TradingDecision decision = strategy.decide(ticker, hourHistory, minHistory, state.position, sharedCash, hourChanged);
                    sharedCash = applyPortfolioDecision(
                            ticker, strategy, state, decision, current, current.time, hourHistory, minHistory, sharedCash, tradesByTicker.get(ticker)
                    );
                }

                state.lastSeenHourIdx = state.hourIdx;
                minuteIndexByTicker.put(ticker, idx + 1);
            }

            double totalEquity = sharedCash;
            for (Map.Entry<String, PortfolioPositionState> entry : positionStates.entrySet()) {
                String ticker = entry.getKey();
                PortfolioPositionState state = entry.getValue();
                if (state.position.quantity > 0) {
                    totalEquity += getRequiredCash(ticker, state.position.quantity, lastPriceByTicker.getOrDefault(ticker, state.entryPrice));
                }
            }
            portfolioEquity.add(new EquityPoint(time, totalEquity));
        }

        for (String ticker : marketDataByTicker.keySet()) {
            MarketData marketData = marketDataByTicker.get(ticker);
            PortfolioPositionState state = positionStates.get(ticker);
            if (state.position.quantity > 0 && !marketData.minuteCandles.isEmpty()) {
                Candle lastCandle = marketData.minuteCandles.get(marketData.minuteCandles.size() - 1);
                sharedCash = closePortfolioPosition(
                        ticker, strategies.get(ticker), state, lastCandle.close, lastCandle.time, "period_end", sharedCash, tradesByTicker.get(ticker)
                );
            }
        }

        double finalPortfolioValue = sharedCash;
        for (Map.Entry<String, PortfolioPositionState> entry : positionStates.entrySet()) {
            String ticker = entry.getKey();
            PortfolioPositionState state = entry.getValue();
            if (state.position.quantity > 0) {
                finalPortfolioValue += getRequiredCash(ticker, state.position.quantity, lastPriceByTicker.getOrDefault(ticker, state.entryPrice));
            }
        }

        int totalTrades = 0;
        int winningTrades = 0;
        for (String ticker : marketDataByTicker.keySet()) {
            List<TradeResult> tickerTrades = tradesByTicker.get(ticker);
            List<EquityPoint> tickerEquity = equityByTicker.get(ticker);
            double tickerPnl = tickerTrades.stream().mapToDouble(t -> t.pnl).sum();
            double tickerDd = calcMaxDrawdownByEquity(tickerEquity);
            double tickerWinRate = calculateWinRate(tickerTrades);
            totalTrades += tickerTrades.size();
            winningTrades += (int) tickerTrades.stream().filter(t -> t.pnl > 0.0).count();
            tickerResults.put(ticker, new TickerPeriodResult(
                    tickerTrades,
                    tickerEquity,
                    tickerPnl,
                    tickerDd,
                    initialBalance,
                    tickerWinRate
            ));
        }

        double portfolioPnl = finalPortfolioValue - initialBalance;
        double portfolioDd = calcMaxDrawdownByEquity(portfolioEquity);
        double portfolioWinRate = totalTrades > 0 ? (double) winningTrades / totalTrades : 0.0;
        PortfolioPeriodResult portfolioResult = new PortfolioPeriodResult(
                portfolioPnl,
                portfolioDd,
                portfolioEquity,
                totalTrades,
                portfolioWinRate
        );

        return new ExecutionResult(tickerResults, portfolioResult);
    }

    private List<MarketDataLoadResult> loadMarketDataParallel(List<String> tickers,
                                                              UnifiedTraderConfig config,
                                                              String start,
                                                              String endExclusive) throws IOException {
        ExecutorService executor = Executors.newFixedThreadPool(Math.min(BACKTEST_THREADS, Math.max(1, tickers.size())));
        try {
            List<Callable<MarketDataLoadResult>> tasks = new ArrayList<>();
            for (String ticker : tickers) {
                tasks.add(() -> loadMarketData(ticker, config, start, endExclusive));
            }

            List<Future<MarketDataLoadResult>> futures = executor.invokeAll(tasks);
            List<MarketDataLoadResult> results = new ArrayList<>();
            for (Future<MarketDataLoadResult> future : futures) {
                try {
                    MarketDataLoadResult loadResult = future.get();
                    if (loadResult != null) {
                        results.add(loadResult);
                    }
                } catch (ExecutionException e) {
                    Throwable cause = e.getCause();
                    if (cause instanceof IOException) {
                        throw (IOException) cause;
                    }
                    throw new IOException("Failed to load market data", cause);
                }
            }
            return results;
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new IOException("Market data loading interrupted", e);
        } finally {
            executor.shutdownNow();
        }
    }

    private MarketDataLoadResult loadMarketData(String ticker,
                                                UnifiedTraderConfig config,
                                                String start,
                                                String endExclusive) {
        UnifiedTraderConfig.TickerParams params = config.getTickerParams(ticker);
        if (!params.enabled) {
            return null;
        }

        List<RawCandle> raw = loadCandles(ticker, start, endExclusive);
        if (raw.size() < MIN_HOURS_REQUIRED) {
            return null;
        }

        List<Candle> wrapped = new ArrayList<>(raw.size());
        List<LocalDateTime> hourTimes = new ArrayList<>(raw.size());
        for (RawCandle c : raw) {
            wrapped.add(new Candle(c.time, c.open, c.high, c.low, c.close, c.volume));
            hourTimes.add(c.dateTime);
        }

        List<RawCandle> minuteCandlesRaw = params.useMinuteCandles
                ? loadCandles5Min(ticker, start, endExclusive)
                : raw;
        if (minuteCandlesRaw.isEmpty()) {
            return null;
        }

        List<Candle> wrappedMin = new ArrayList<>(minuteCandlesRaw.size());
        List<LocalDateTime> minTimes = new ArrayList<>(minuteCandlesRaw.size());
        for (RawCandle c : minuteCandlesRaw) {
            wrappedMin.add(new Candle(c.time, c.open, c.high, c.low, c.close, c.volume));
            minTimes.add(c.dateTime);
        }

        MarketData marketData = new MarketData(wrapped, minuteCandlesRaw, wrappedMin, minTimes, hourTimes);
        return new MarketDataLoadResult(ticker, wrapped, marketData);
    }

    private Map<String, List<LocalDateTime>> buildPeerTimesMap(Map<String, List<Candle>> allHourlyCandles) {
        Map<String, List<LocalDateTime>> peerTimesMap = new HashMap<>();
        for (Map.Entry<String, List<Candle>> e : allHourlyCandles.entrySet()) {
            List<LocalDateTime> peerTimes = new ArrayList<>(e.getValue().size());
            for (Candle c : e.getValue()) {
                peerTimes.add(LocalDateTime.parse(c.time, DATE_TIME_FMT));
            }
            peerTimesMap.put(e.getKey(), peerTimes);
        }
        return peerTimesMap;
    }

    private List<String> buildGlobalTimeline(Map<String, MarketData> marketDataByTicker) {
        Set<String> timeline = new TreeSet<>(this::compareTime);
        for (MarketData marketData : marketDataByTicker.values()) {
            for (Candle candle : marketData.minuteCandles) {
                timeline.add(candle.time);
            }
        }
        return new ArrayList<>(timeline);
    }

    private double applyPortfolioDecision(String ticker,
                                          BaseStrategy strategy,
                                          PortfolioPositionState state,
                                          TradingDecision decision,
                                          Candle current,
                                          String currentTime,
                                          List<Candle> hourHistory,
                                          List<Candle> minHistory,
                                          double sharedCash,
                                          List<TradeResult> trades) {
        if (decision == null) {
            return sharedCash;
        }

        switch (decision.action) {
            case "OPEN":
                if (decision.updatedPosition == null || decision.quantity <= 0) return sharedCash;
                if (!"BUY".equals(decision.updatedPosition.direction) || state.position.quantity > 0) return sharedCash;

                double openEntry = decision.updatedPosition.entryPrice != null ? decision.updatedPosition.entryPrice : current.close;
                int openQty = decision.quantity;
                double positionValue = getRequiredCash(ticker, openQty, openEntry);
                double entryCommission = positionValue * commission;
                if (positionValue + entryCommission > sharedCash) {
                    return sharedCash;
                }

                sharedCash -= (positionValue + entryCommission);
                state.position = decision.updatedPosition;
                state.entryPrice = openEntry;
                // Use minute candles for accurate timestamp (5-min precision instead of hourly)
                strategy.recordBacktestTradeEntry(ticker, minHistory, decision);
                return sharedCash;

            case "CLOSE":
                if (state.position.quantity <= 0) {
                    state.position = decision.updatedPosition != null ? decision.updatedPosition : state.position;
                    return sharedCash;
                }

                double exitPrice = decision.entryPrice != null ? decision.entryPrice : current.close;
                return closePortfolioPosition(ticker, strategy, state, exitPrice, currentTime, decision.reason, sharedCash, trades);

            case "HOLD":
                if (decision.updatedPosition != null) {
                    state.position = decision.updatedPosition;
                }
                return sharedCash;

            default:
                return sharedCash;
        }
    }

    private double closePortfolioPosition(String ticker,
                                          BaseStrategy strategy,
                                          PortfolioPositionState state,
                                          double exitPrice,
                                          String time,
                                          String reason,
                                          double sharedCash,
                                          List<TradeResult> trades) {
        int quantity = state.position.quantity;
        double exitValue = getRequiredCash(ticker, quantity, exitPrice);
        double entryValue = getRequiredCash(ticker, quantity, state.entryPrice);
        double totalCommission = (entryValue + exitValue) * commission;
        double pnl = exitValue - entryValue - totalCommission;

        trades.add(new TradeResult(ticker, "BUY", state.entryPrice, exitPrice, pnl, reason, time));
        double stopLoss = state.position.stopLoss != null ? state.position.stopLoss : state.entryPrice;
        strategy.recordBacktestTradeOutcome(ticker, pnl, state.entryPrice, stopLoss, state.position.quantity);
        state.realizedPnl += pnl;

        double exitCommission = exitValue * commission;
        sharedCash += (exitValue - exitCommission);
        state.position = new Position();
        state.entryPrice = 0.0;
        return sharedCash;
    }

    private double tickerEquity(String ticker, PortfolioPositionState state, double currentPrice) {
        double unrealizedPnl = 0.0;
        if (state.position.quantity > 0) {
            double exitValue = getRequiredCash(ticker, state.position.quantity, currentPrice);
            double entryValue = getRequiredCash(ticker, state.position.quantity, state.entryPrice);
            double exitCommission = exitValue * commission;
            unrealizedPnl = exitValue - entryValue - exitCommission;
        }

        return initialBalance + state.realizedPnl + unrealizedPnl;
    }

    private SimulateResult simulateUnifiedLongOnly(BaseStrategy strategy,
                                                   String ticker,
                                                   List<Candle> wrappedHour,
                                                   List<RawCandle> minuteCandlesRaw,
                                                   double startBalance,
                                                   Map<String, List<Candle>> allHourlyCandles,
                                                   Map<String, List<String>> groupTickers,
                                                   UnifiedTraderConfig strategyConfig) {
        if (wrappedHour == null || wrappedHour.isEmpty() || minuteCandlesRaw == null || minuteCandlesRaw.isEmpty()) {
            return new SimulateResult(Collections.emptyList(), Collections.emptyList(), startBalance);
        }

        List<TradeResult> trades = new ArrayList<>();
        List<EquityPoint> equityCurve = new ArrayList<>();

        List<Candle> wrappedMin = new ArrayList<>(minuteCandlesRaw.size());
        List<LocalDateTime> minTimes = new ArrayList<>(minuteCandlesRaw.size());
        for (RawCandle c : minuteCandlesRaw) {
            wrappedMin.add(new Candle(c.time, c.open, c.high, c.low, c.close, c.volume));
            minTimes.add(c.dateTime);
        }

        List<LocalDateTime> hourTimes = new ArrayList<>(wrappedHour.size());
        for (Candle c : wrappedHour) {
            hourTimes.add(LocalDateTime.parse(c.time, DATE_TIME_FMT));
        }

        Map<String, List<LocalDateTime>> peerTimesMap = new HashMap<>();
        for (Map.Entry<String, List<Candle>> e : allHourlyCandles.entrySet()) {
            List<LocalDateTime> peerTimes = new ArrayList<>(e.getValue().size());
            for (Candle c : e.getValue()) {
                peerTimes.add(LocalDateTime.parse(c.time, DATE_TIME_FMT));
            }
            peerTimesMap.put(e.getKey(), peerTimes);
        }

        double cash = startBalance;
        Position pos = new Position();
        double entryPrice = 0.0;
        int hourIdx = -1;
        int lastSeenHourIdx = -1;
        LocalDate lastEodCloseDate = null;
        for (int i = 0; i < wrappedMin.size(); i++) {
            Candle current = wrappedMin.get(i);
            LocalDateTime minDt = minTimes.get(i);

            if (!isTradingDay(minDt.toLocalDate()) || !isWithinWorkingHours(minDt.toLocalTime())) {
                if (pos.quantity > 0
                        && !minDt.toLocalTime().isBefore(EOD_CLOSE_TIME)
                        && !minDt.toLocalDate().equals(lastEodCloseDate)) {

                    double exitPrice = current.close;
                    int q = pos.quantity;

                    double exitValue = getRequiredCash(ticker, q, exitPrice);
                    double entryValue = getRequiredCash(ticker, q, entryPrice);
                    double totalCommission = (entryValue + exitValue) * commission;
                    double pnl = exitValue - entryValue - totalCommission;

                    trades.add(new TradeResult(
                            ticker, "BUY", entryPrice, exitPrice, pnl, "eod_close", current.time
                    ));

                    double exitCommission = exitValue * commission;
                    cash += (exitValue - exitCommission);
                    pos = new Position();
                    entryPrice = 0.0;
                    lastEodCloseDate = minDt.toLocalDate();
                }

                double offHoursEquity = cash;
                if (pos.quantity > 0) {
                    offHoursEquity += getRequiredCash(ticker, pos.quantity, current.close);
                }
                equityCurve.add(new EquityPoint(current.time, offHoursEquity));
                continue;
            }

            while (hourIdx + 1 < hourTimes.size() && !hourTimes.get(hourIdx + 1).isAfter(minDt)) {
                hourIdx++;
            }

            boolean hourChanged = hourIdx != lastSeenHourIdx;

            double equity = cash;
            if (pos.quantity > 0) {
                equity += getRequiredCash(ticker, pos.quantity, current.close);
            }
            equityCurve.add(new EquityPoint(current.time, equity));

            if (hourIdx + 1 < MIN_HOURS_REQUIRED) {
                lastSeenHourIdx = hourIdx;
                continue;
            }

            List<Candle> hourHistory = wrappedHour.subList(0, hourIdx + 1);
            List<Candle> minHistory = wrappedMin.subList(0, i + 1);

            Map<String, List<Candle>> currentPeerCandles = buildCurrentPeerCandles(
                    ticker, minDt, allHourlyCandles, groupTickers, peerTimesMap, hourHistory, strategyConfig
            );
            if (!currentPeerCandles.isEmpty()) {
                strategy.setPeerCandles(currentPeerCandles);
            } else {
                strategy.setPeerCandles(Collections.emptyMap());
            }

            TradingDecision decision = strategy.decide(ticker, hourHistory, minHistory, pos, cash, hourChanged);

            switch (decision.action) {
                case "OPEN":
                    if (decision.updatedPosition == null || decision.quantity <= 0) break;
                    if (!"BUY".equals(decision.updatedPosition.direction)) break;
                    if (pos.quantity > 0) break;

                    double openEntry = decision.updatedPosition.entryPrice != null
                            ? decision.updatedPosition.entryPrice
                            : current.close;

                    int openQty = decision.quantity;
                    double positionValue = getRequiredCash(ticker, openQty, openEntry);
                    double entryCommission = positionValue * commission;

                    if (positionValue + entryCommission > cash) break;

                    cash -= (positionValue + entryCommission);
                    pos = decision.updatedPosition;
                    entryPrice = openEntry;
                    // Use minute candles for accurate timestamp (5-min precision instead of hourly)
                    strategy.recordBacktestTradeEntry(ticker, minHistory, decision);
                    break;

                case "CLOSE":
                    if (pos.quantity <= 0) {
                        pos = decision.updatedPosition != null ? decision.updatedPosition : pos;
                        break;
                    }

                    double exitPrice = decision.entryPrice != null ? decision.entryPrice : current.close;
                    int q = pos.quantity;

                    double exitValue = getRequiredCash(ticker, q, exitPrice);
                    double entryValue = getRequiredCash(ticker, q, entryPrice);
                    double totalCommission = (entryValue + exitValue) * commission;
                    double pnl = exitValue - entryValue - totalCommission;

                    trades.add(new TradeResult(
                            ticker, "BUY", entryPrice, exitPrice, pnl, decision.reason, current.time
                    ));
                    double stopLoss = pos.stopLoss != null ? pos.stopLoss : entryPrice;
                    strategy.recordBacktestTradeOutcome(ticker, pnl, entryPrice, stopLoss, q);

                    double exitCommission = exitValue * commission;
                    cash += (exitValue - exitCommission);

                    pos = decision.updatedPosition != null ? decision.updatedPosition : new Position();
                    entryPrice = 0.0;
                    break;

                case "HOLD":
                    if (decision.updatedPosition != null) {
                        pos = decision.updatedPosition;
                    }
                    break;
            }

            lastSeenHourIdx = hourIdx;
        }

        double finalBalance = cash;
        if (pos.quantity > 0 && !wrappedMin.isEmpty()) {
            double lastPrice = wrappedMin.get(wrappedMin.size() - 1).close;
            int q = pos.quantity;
            double exitValue = getRequiredCash(ticker, q, lastPrice);
            double entryValue = getRequiredCash(ticker, q, entryPrice);
            double totalCommission = (entryValue + exitValue) * commission;
            double pnl = exitValue - entryValue - totalCommission;

            trades.add(new TradeResult(
                    ticker, "BUY", entryPrice, lastPrice, pnl,
                    "period_end", wrappedMin.get(wrappedMin.size() - 1).time
            ));
            double stopLoss = pos.stopLoss != null ? pos.stopLoss : entryPrice;
            strategy.recordBacktestTradeOutcome(ticker, pnl, entryPrice, stopLoss, q);

            double exitCommission = exitValue * commission;
            finalBalance += (exitValue - exitCommission);
        }

        return new SimulateResult(trades, equityCurve, finalBalance);
    }

    private double getRequiredCash(String ticker, int quantity, double price) {
        if (quantity <= 0 || price <= 0.0) {
            return 0.0;
        }

        TickerInfo tickerInfo = resolveTickerInfo(ticker);
        double fullValue = quantity * price;
        if (tickerInfo != null && TickerType.FEATURE == tickerInfo.getType()) {
            return fullValue * TCSService.FUTURES_MARGIN_RATE;
        }
        return fullValue;
    }

    private TickerInfo resolveTickerInfo(String ticker) {
        TickerInfo.Key stockKey = new TickerInfo.Key(ticker, TickerType.STOCK);
        if (TickerRepository.INSTANCE.containsKey(stockKey)) {
            return TickerRepository.INSTANCE.getById(stockKey);
        }

        TickerInfo.Key futureKey = new TickerInfo.Key(ticker, TickerType.FEATURE);
        if (TickerRepository.INSTANCE.containsKey(futureKey)) {
            return TickerRepository.INSTANCE.getById(futureKey);
        }

        return null;
    }

    private Map<String, List<Candle>> buildCurrentPeerCandles(String ticker,
                                                              LocalDateTime minDt,
                                                              Map<String, List<Candle>> allHourlyCandles,
                                                              Map<String, List<String>> groupTickers,
                                                              Map<String, List<LocalDateTime>> peerTimesMap,
                                                              List<Candle> currentTickerHourHistory,
                                                              UnifiedTraderConfig config) {
        Map<String, List<Candle>> result = new HashMap<>();
        result.put(ticker, currentTickerHourHistory);

        String allocGroup = config.getTickerParams(ticker).allocationGroup;
        if (allocGroup == null || allocGroup.isEmpty()) {
            return result;
        }

        List<String> members = groupTickers.getOrDefault(allocGroup, Collections.emptyList());
        for (String peer : members) {
            if (peer.equals(ticker)) {
                continue;
            }

            List<Candle> peerAll = allHourlyCandles.get(peer);
            List<LocalDateTime> peerTimes = peerTimesMap.get(peer);
            if (peerAll == null || peerTimes == null || peerTimes.isEmpty()) {
                continue;
            }

            int peerIdx = upperBound(peerTimes, minDt);
            if (peerIdx >= 0) {
                result.put(peer, peerAll.subList(0, peerIdx + 1));
            }
        }

        return result;
    }

    private PortfolioPeriodResult buildPortfolioPeriodResult(Map<String, TickerPeriodResult> tickerResults) {
        if (tickerResults == null || tickerResults.isEmpty()) {
            return new PortfolioPeriodResult(0.0, 0.0, Collections.emptyList(), 0, 0.0);
        }

        double totalStartBalance = 0.0;
        double totalFinalBalance = 0.0;
        int totalTrades = 0;

        for (TickerPeriodResult result : tickerResults.values()) {
            totalStartBalance += result.startBalance;
            totalFinalBalance += result.startBalance + result.pnl;
            totalTrades += result.trades.size();
        }

        List<EquityPoint> portfolioEquity = mergePortfolioEquity(tickerResults);
        double dd = calcMaxDrawdownByEquity(portfolioEquity);
        double pnl = totalFinalBalance - totalStartBalance;
        double winRate = calculatePortfolioWinRate(tickerResults);

        return new PortfolioPeriodResult(pnl, dd, portfolioEquity, totalTrades, winRate);
    }

    private List<EquityPoint> mergePortfolioEquity(Map<String, TickerPeriodResult> tickerResults) {
        if (tickerResults == null || tickerResults.isEmpty()) {
            return Collections.emptyList();
        }

        Set<String> allTimes = new TreeSet<>((a, b) -> {
            LocalDateTime ta = LocalDateTime.parse(a, DATE_TIME_FMT);
            LocalDateTime tb = LocalDateTime.parse(b, DATE_TIME_FMT);
            return ta.compareTo(tb);
        });

        for (TickerPeriodResult result : tickerResults.values()) {
            for (EquityPoint point : result.equityCurve) {
                allTimes.add(point.time);
            }
        }

        Map<String, Double> lastEquityByTicker = new HashMap<>();
        Map<String, Integer> indexByTicker = new HashMap<>();
        List<EquityPoint> merged = new ArrayList<>();

        for (String ticker : tickerResults.keySet()) {
            TickerPeriodResult result = tickerResults.get(ticker);
            indexByTicker.put(ticker, 0);
            lastEquityByTicker.put(ticker, result.startBalance);
        }

        for (String time : allTimes) {
            double totalEquity = 0.0;

            for (Map.Entry<String, TickerPeriodResult> entry : tickerResults.entrySet()) {
                String ticker = entry.getKey();
                TickerPeriodResult result = entry.getValue();
                int idx = indexByTicker.get(ticker);

                while (idx < result.equityCurve.size()
                        && compareTime(result.equityCurve.get(idx).time, time) <= 0) {
                    lastEquityByTicker.put(ticker, result.equityCurve.get(idx).equity);
                    idx++;
                }

                indexByTicker.put(ticker, idx);
                totalEquity += lastEquityByTicker.get(ticker);
            }

            merged.add(new EquityPoint(time, totalEquity));
        }

        return merged;
    }

    private int compareTime(String t1, String t2) {
        LocalDateTime d1 = LocalDateTime.parse(t1, DATE_TIME_FMT);
        LocalDateTime d2 = LocalDateTime.parse(t2, DATE_TIME_FMT);
        return d1.compareTo(d2);
    }

    private Map<String, Double> buildCapitalAllocation(List<String> tickers, UnifiedTraderConfig config) {
        Map<String, Double> weights = new LinkedHashMap<>();
        double totalWeight = 0.0;

        for (String ticker : tickers) {
            UnifiedTraderConfig.TickerParams params = config.getTickerParams(ticker);
            if (!params.enabled) continue;

            double weight = params.allocationWeight > 0.0 ? params.allocationWeight : 1.0;
            weights.put(ticker, weight);
            totalWeight += weight;
        }

        if (weights.isEmpty() || totalWeight <= 0.0) {
            return Collections.emptyMap();
        }

        Map<String, Double> allocation = new LinkedHashMap<>();
        for (Map.Entry<String, Double> e : weights.entrySet()) {
            allocation.put(e.getKey(), initialBalance * (e.getValue() / totalWeight));
        }

        return allocation;
    }

    private List<String> filterEnabledTickers(List<String> tickers, UnifiedTraderConfig config) {
        List<String> result = new ArrayList<>();
        for (String ticker : tickers) {
            try {
                UnifiedTraderConfig.TickerParams params = config.getTickerParams(ticker);
                if (params.enabled) {
                    result.add(ticker);
                }
            } catch (Exception ignored) {
            }
        }
        return result;
    }

    private int upperBound(List<LocalDateTime> times, LocalDateTime target) {
        int left = 0;
        int right = times.size() - 1;
        int ans = -1;

        while (left <= right) {
            int mid = (left + right) >>> 1;
            if (!times.get(mid).isAfter(target)) {
                ans = mid;
                left = mid + 1;
            } else {
                right = mid - 1;
            }
        }
        return ans;
    }

    private List<String> loadTickers() throws IOException {
        Properties props = PropertiesUtils.loadProperties();
        Set<String> tickers = new LinkedHashSet<>();

        for (String s : props.getProperty("datacollector.instruments", "").split(",")) {
            String t = s.trim();
            if (!t.isEmpty()) tickers.add(t);
        }

        for (String key : props.stringPropertyNames()) {
            if (key.startsWith("unifiedTrader.ticker.")) {
                String[] parts = key.split("\\.");
                if (parts.length > 2) {
                    tickers.add(parts[2]);
                }
            }
        }

        return new ArrayList<>(tickers);
    }

    private List<RawCandle> loadCandles(String ticker, String startDate, String endExclusiveDate) {
        File file = new File(dataDir, ticker + "/candlesHOUR.txt");
        if (!file.exists()) return Collections.emptyList();

        LocalDate start = LocalDate.parse(startDate);
        LocalDate endExclusive = LocalDate.parse(endExclusiveDate);

        try {
            List<String> lines = Files.readAllLines(file.toPath());
            List<RawCandle> result = new ArrayList<>();

            for (int i = 1; i < lines.size(); i++) {
                String line = lines.get(i).trim();
                if (line.isEmpty()) continue;

                try {
                    String[] parts = line.split(",");
                    if (parts.length < 6) continue;

                    int spaceIdx = parts[0].indexOf(' ');
                    String datePart = spaceIdx >= 0 ? parts[0].substring(0, spaceIdx) : parts[0];
                    LocalDate dt = LocalDate.parse(datePart, DATE_FMT);

                    if (dt.isBefore(start) || !dt.isBefore(endExclusive)) continue;

                    result.add(new RawCandle(
                            parts[0],
                            Double.parseDouble(parts[1]),
                            Double.parseDouble(parts[2]),
                            Double.parseDouble(parts[3]),
                            Double.parseDouble(parts[4]),
                            Long.parseLong(parts[5])
                    ));
                } catch (Exception ignored) {
                }
            }

            result.sort(Comparator.comparing(c -> c.dateTime));
            return result;
        } catch (IOException e) {
            return Collections.emptyList();
        }
    }

    private List<RawCandle> loadCandles5Min(String ticker, String startDate, String endExclusiveDate) {
        File file = new File(dataDir, ticker + "/candles5_MIN.txt");
        if (!file.exists()) return Collections.emptyList();

        LocalDate start = LocalDate.parse(startDate);
        LocalDate endExclusive = LocalDate.parse(endExclusiveDate);

        try {
            List<String> lines = Files.readAllLines(file.toPath());
            List<RawCandle> result = new ArrayList<>();

            for (int i = 1; i < lines.size(); i++) {
                String line = lines.get(i).trim();
                if (line.isEmpty()) continue;

                try {
                    String[] parts = line.split(",");
                    if (parts.length < 6) continue;

                    int spaceIdx = parts[0].indexOf(' ');
                    String datePart = spaceIdx >= 0 ? parts[0].substring(0, spaceIdx) : parts[0];
                    LocalDate dt = LocalDate.parse(datePart, DATE_FMT);

                    if (dt.isBefore(start) || !dt.isBefore(endExclusive)) continue;

                    result.add(new RawCandle(
                            parts[0],
                            Double.parseDouble(parts[1]),
                            Double.parseDouble(parts[2]),
                            Double.parseDouble(parts[3]),
                            Double.parseDouble(parts[4]),
                            Long.parseLong(parts[5])
                    ));
                } catch (Exception ignored) {
                }
            }

            result.sort(Comparator.comparing(c -> c.dateTime));
            return result;
        } catch (IOException e) {
            return Collections.emptyList();
        }
    }

    private boolean isTradingDay(LocalDate date) {
        DayOfWeek day = date.getDayOfWeek();
        return day != DayOfWeek.SATURDAY && day != DayOfWeek.SUNDAY;
    }

    private boolean isWithinWorkingHours(LocalTime time) {
        return !time.isBefore(WORK_START_TIME) && time.isBefore(EOD_CLOSE_TIME);
    }

    private double calcMaxDrawdownByEquity(List<EquityPoint> equityCurve) {
        if (equityCurve == null || equityCurve.isEmpty()) return 0.0;

        double peak = equityCurve.get(0).equity;
        double maxDd = 0.0;

        for (EquityPoint point : equityCurve) {
            if (point.equity > peak) peak = point.equity;
            double dd = peak > 0 ? (peak - point.equity) / peak : 0.0;
            if (dd > maxDd) maxDd = dd;
        }

        return maxDd;
    }

    private double calculateWinRate(List<TradeResult> trades) {
        if (trades == null || trades.isEmpty()) return 0.0;
        
        long winningTrades = trades.stream().filter(trade -> trade.pnl > 0).count();
        return (double) winningTrades / trades.size();
    }

    private double calculatePortfolioWinRate(Map<String, TickerPeriodResult> tickerResults) {
        if (tickerResults == null || tickerResults.isEmpty()) return 0.0;
        
        long totalTrades = 0;
        long winningTrades = 0;
        
        for (TickerPeriodResult result : tickerResults.values()) {
            for (TradeResult trade : result.trades) {
                totalTrades++;
                if (trade.pnl > 0) {
                    winningTrades++;
                }
            }
        }
        
        return totalTrades > 0 ? (double) winningTrades / totalTrades : 0.0;
    }

    private static String formatCompactPnL(double pnl) {
        String sign = pnl >= 0 ? "+" : "-";
        double abs = Math.abs(pnl);

        if (abs >= 1_000_000) {
            return String.format("%6s", sign + String.format("%.2f", abs / 1_000_000) + "M");
        }
        if (abs >= 1_000) {
            return String.format("%6s", sign + String.format("%.1f", abs / 1_000) + "K");
        }
        return String.format("%6s", sign + String.format("%.0f", abs));
    }

    private String formatCompactDD(double dd) {
        String risk = assessRisk(dd);
        String marker;
        if ("High".equals(risk)) marker = "*";
        else if ("Med".equals(risk)) marker = "!";
        else marker = " ";

        return String.format("%5s", marker + String.format("%.1f", dd));
    }

    private String assessRisk(double ddPct) {
        if (ddPct < 10.0) return "Low";
        if (ddPct < 25.0) return "Med";
        return "High";
    }

    /**
     * Strategy metrics for comparison.
     */
    private static class StrategyMetrics {
        final String strategyName;
        double totalPnL;
        double avgWinRate;
        double maxDrawdown;
        int totalTrades;
        double sharpeRatio;
        double profitFactor;
        double averageMonthlyReturn;

        StrategyMetrics(String strategyName) {
            this.strategyName = strategyName;
        }
    }

    /**
     * Collect metrics for strategy comparison.
     */
    private void collectStrategyMetrics(String strategyName,
                                        Map<String, Map<String, TickerPeriodResult>> allData,
                                        Map<String, PortfolioPeriodResult> portfolioData) {
        StrategyMetrics metrics = new StrategyMetrics(strategyName);

        double totalPnL = 0.0;
        double totalWinRate = 0.0;
        double maxDD = 0.0;
        int periodCount = 0;
        List<Double> returns = new ArrayList<>();
        double grossProfit = 0.0;
        double grossLoss = 0.0;

        for (PortfolioPeriodResult portfolioResult : portfolioData.values()) {
            totalPnL += portfolioResult.pnl;
            if (portfolioResult.dd > maxDD) {
                maxDD = portfolioResult.dd;
            }
            totalWinRate += portfolioResult.winRate;
            periodCount++;

            if (portfolioResult.equityCurve != null && !portfolioResult.equityCurve.isEmpty()) {
                double startEquity = portfolioResult.equityCurve.get(0).equity;
                if (startEquity > 0.0) {
                    returns.add(portfolioResult.pnl / startEquity);
                }
            }
        }

        metrics.totalPnL = totalPnL;
        metrics.avgWinRate = periodCount > 0 ? totalWinRate / periodCount : 0.0;
        metrics.maxDrawdown = maxDD;
        metrics.totalTrades = portfolioData.values().stream()
                .mapToInt(p -> p.totalTrades)
                .sum();

        if (returns.size() > 1) {
            double avgReturn = returns.stream().mapToDouble(r -> r).average().orElse(0.0);
            double variance = returns.stream()
                    .mapToDouble(r -> Math.pow(r - avgReturn, 2))
                    .average().orElse(0.0);
            double stdDev = Math.sqrt(variance);
            metrics.averageMonthlyReturn = avgReturn;
            metrics.sharpeRatio = stdDev > 0 ? avgReturn / stdDev : 0.0;
        }

        for (Map.Entry<String, Map<String, TickerPeriodResult>> entry : allData.entrySet()) {
            Map<String, TickerPeriodResult> tickerPeriodResults = entry.getValue();
            for (TickerPeriodResult result : tickerPeriodResults.values()) {
                for (TradeResult trade : result.trades) {
                    if (trade.pnl > 0.0) {
                        grossProfit += trade.pnl;
                    } else if (trade.pnl < 0.0) {
                        grossLoss += Math.abs(trade.pnl);
                    }
                }
            }
        }
        metrics.profitFactor = grossLoss > 0 ? grossProfit / grossLoss : 0.0;

        strategyMetricsMap.put(strategyName, metrics);
    }

    /**
     * Print strategy comparison table.
     */
    private static void printStrategyComparison() {
        if (strategyMetricsMap.isEmpty()) {
            System.out.println("No strategy metrics collected.");
            return;
        }

        // Header
        System.out.println();
        String header = String.format("%-20s %15s %10s %10s %10s %10s %10s %10s",
                "Стратегия", "Total PnL", "WinRate%", "MaxDD%", "Trades", "Sharpe", "PF", "Score");
        System.out.println(header);
        System.out.println("-".repeat(header.length()));

        // Sort by score (descending)
        List<StrategyMetrics> sortedMetrics = new ArrayList<>(strategyMetricsMap.values());
        sortedMetrics.sort((a, b) -> Double.compare(calculateScore(b), calculateScore(a)));

        // Rows
        for (StrategyMetrics m : sortedMetrics) {
            double score = calculateScore(m);
            System.out.println(String.format("%-20s %15s %10.1f %10.1f %10d %10.2f %10.2f %10.1f",
                    m.strategyName,
                    formatCompactPnL(m.totalPnL),
                    m.avgWinRate * 100.0,
                    m.maxDrawdown * 100.0,
                    m.totalTrades,
                    m.sharpeRatio,
                    m.profitFactor,
                    score));
        }

        System.out.println();
        System.out.println("Legend:");
        System.out.println("  Score = (WinRate% × 0.4) + ((20 - MaxDD%) × 0.3) + (Sharpe × 0.2) + (PF × 0.1)");
        System.out.println("  Best strategy has highest score");
        System.out.println();

        // Highlight best strategy
        if (!sortedMetrics.isEmpty()) {
            StrategyMetrics best = sortedMetrics.get(0);
            System.out.println("🏆 BEST STRATEGY: " + best.strategyName +
                    " (Score: " + String.format("%.1f", calculateScore(best)) + ")");
        }
    }

    /**
     * Calculate composite score for strategy ranking.
     */
    private static double calculateScore(StrategyMetrics m) {
        double winRateScore = m.avgWinRate * 100.0 * 0.4;  // 40% weight
        double ddScore = Math.max(0, (20.0 - (m.maxDrawdown * 100.0))) * 0.3;  // 30% weight (target <10%)
        double sharpeScore = Math.max(0, m.sharpeRatio) * 10.0 * 0.2;  // 20% weight
        double pfScore = Math.min(m.profitFactor, 3.0) * 10.0 * 0.1;  // 10% weight (cap at 3.0)

        return winRateScore + ddScore + sharpeScore + pfScore;
    }
}
