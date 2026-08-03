package com.github.shk0da.goldendragon.strategy;

import static com.github.shk0da.goldendragon.utils.IndicatorsUtil.toDouble;
import static com.github.shk0da.goldendragon.utils.SerializationUtils.getDateOfContentOnDisk;
import static com.github.shk0da.goldendragon.utils.SerializationUtils.loadDataFromDisk;
import static com.github.shk0da.goldendragon.utils.SerializationUtils.saveDataToDisk;
import static com.github.shk0da.goldendragon.utils.TimeUtils.sleep;
import static java.lang.System.out;
import static java.nio.file.Files.createDirectories;
import static java.nio.file.Files.deleteIfExists;
import static java.time.OffsetDateTime.now;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.shk0da.goldendragon.config.DataCollectorConfig;
import com.github.shk0da.goldendragon.config.MainConfig;
import com.github.shk0da.goldendragon.config.MarketConfig;
import com.github.shk0da.goldendragon.model.Market;
import com.github.shk0da.goldendragon.model.TickerCandle;
import com.github.shk0da.goldendragon.model.TickerInfo;
import com.github.shk0da.goldendragon.model.TickerType;
import com.github.shk0da.goldendragon.repository.Repository;
import com.github.shk0da.goldendragon.repository.TickerRepository;
import com.github.shk0da.goldendragon.service.BybitService;
import com.github.shk0da.goldendragon.service.TCSService;
import com.github.shk0da.goldendragon.utils.LevelUtils;
import com.github.shk0da.goldendragon.utils.LevelUtils.Level;
import com.github.shk0da.goldendragon.utils.TickerTypeResolver;
import com.google.gson.reflect.TypeToken;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.Timestamp;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;
import ru.tinkoff.piapi.contract.v1.CandleInterval;
import ru.tinkoff.piapi.contract.v1.HistoricCandle;

public class DataCollector {

    private static final ObjectMapper objectMapper = new ObjectMapper();
    private static final DateFormat dateTimeFormat = new SimpleDateFormat("dd.MM.yyyy HH:mm:ss");
    private static final Repository<TickerInfo.Key, TickerInfo> tickerRepository =
            TickerRepository.INSTANCE;

    private final TCSService tcsService;
    private final BybitService bybitService;
    private final DataCollectorConfig config;

    public DataCollector(DataCollectorConfig config, TCSService tcsService) {
        this.tcsService = tcsService;
        this.bybitService = new BybitService(config.getDataDir());
        this.config = config;
    }

    public static void main(String[] args) throws Exception {
        DataCollectorConfig config = new DataCollectorConfig();
        var dataDir = config.getDataDir();
        var tickers = config.getInstruments();
        var cryptoTickers = config.getCryptoInstruments();
        var isReplace = config.isReplace();
        var historyDays = config.getHistoryDays();

        createDirectories(Paths.get(dataDir));

        // Process traditional instruments (stocks, bonds, etc.) via Tinkoff API
        if (tickers != null && !tickers.isEmpty()) {
            out.println("=== Downloading instrument data from Tinkoff API ===");
            out.println("Instruments: " + String.join(", ", tickers));

            MainConfig mainConfig = new MainConfig();
            MarketConfig marketConfig = MarketConfig.byMarket(Market.MOEX);
            TCSService tcsService =
                    new TCSService(
                            mainConfig.withAccountId(mainConfig.getTcsAccountId()), marketConfig);

            // Update ticker repository (load from disk or fetch from API if empty/stale)
            refreshTickerRepository(tcsService);

            DataCollector dataCollector = new DataCollector(config, tcsService);
            for (String name : tickers) {
                try {
                    createDirectories(Paths.get(dataDir + "/" + name));
                    dataCollector.updateCandlesFile(
                            name, dataDir, CandleInterval.CANDLE_INTERVAL_5_MIN, isReplace);
                    dataCollector.updateCandlesFile(
                            name, dataDir, CandleInterval.CANDLE_INTERVAL_HOUR, isReplace);
                    if (name.contains("@")) {
                        dataCollector.updateCandlesFile(
                                name, dataDir, CandleInterval.CANDLE_INTERVAL_DAY, isReplace);
                    }
                    var levels =
                            dataCollector.calculatePriceLevels(
                                    name, dataDir, CandleInterval.CANDLE_INTERVAL_HOUR);
                    dataCollector.createTickerJson(name, dataDir, levels);
                } catch (Exception ex) {
                    out.println(ex.getMessage());
                }
            }
            out.println("=== Instrument data download completed ===");
        } else {
            out.println("No instruments configured");
        }

        // Download and convert crypto data from Bybit
        if (cryptoTickers != null && !cryptoTickers.isEmpty()) {
            BybitService bybitService = new BybitService(dataDir);
            out.println("=== Downloading crypto data from Bybit ===");
            out.println("Crypto instruments: " + String.join(", ", cryptoTickers));

            LocalDate endDate = LocalDate.now();
            LocalDate startDate = LocalDate.now().minusDays(historyDays);

            bybitService.downloadAndConvert(cryptoTickers, startDate, endDate);

            out.println("=== Crypto data download completed ===");
        } else {
            out.println("No crypto instruments configured");
        }
    }

    private static void refreshTickerRepository(TCSService tcsService) throws Exception {
        AtomicReference<Map<TickerInfo.Key, TickerInfo>> tickerRegister =
                new AtomicReference<>(new HashMap<>());

        Callable<Boolean> isEmpty =
                () -> {
                    Map<TickerInfo.Key, TickerInfo> dataFromDisk =
                            loadDataFromDisk(TickerRepository.SERIALIZE_NAME, new TypeToken<>() {});
                    if (null == dataFromDisk) {
                        return true;
                    }
                    tickerRegister.set(dataFromDisk);
                    return null == tickerRegister.get() || tickerRegister.get().isEmpty();
                };

        Callable<Boolean> isOld =
                () -> {
                    Date weekAgo = new Date(System.currentTimeMillis() - TimeUnit.DAYS.toMillis(7));
                    return getDateOfContentOnDisk(TickerRepository.SERIALIZE_NAME).before(weekAgo);
                };

        if (isEmpty.call() || isOld.call()) {
            tickerRepository.putAll(tcsService.getCurrenciesList());
            tickerRepository.putAll(tcsService.getEtfList());
            tickerRepository.putAll(tcsService.getStockList());
            tickerRepository.putAll(tcsService.getBondList());
            tickerRepository.putAll(tcsService.getFuturesList());
            saveDataToDisk(TickerRepository.SERIALIZE_NAME, tickerRepository.getAll());
        } else {
            tickerRepository.putAll(tickerRegister.get());
        }
    }

    public void run() throws Exception {
        var dataDir = config.getDataDir();
        var tickers = config.getInstruments();
        var cryptoTickers = config.getCryptoInstruments();
        var isReplace = config.isReplace();
        var historyDays = config.getHistoryDays();

        createDirectories(Paths.get(dataDir));

        // Download and convert crypto data from Bybit
        if (cryptoTickers != null && !cryptoTickers.isEmpty()) {
            downloadCryptoData(cryptoTickers, historyDays);
        }

        // Process traditional instruments (stocks, bonds, etc.)
        for (String name : tickers) {
            try {
                createDirectories(Paths.get(dataDir + "/" + name));
                updateCandlesFile(name, dataDir, CandleInterval.CANDLE_INTERVAL_5_MIN, isReplace);
                updateCandlesFile(name, dataDir, CandleInterval.CANDLE_INTERVAL_HOUR, isReplace);
                if (name.contains("@")) {
                    updateCandlesFile(name, dataDir, CandleInterval.CANDLE_INTERVAL_DAY, isReplace);
                }
                var levels =
                        calculatePriceLevels(name, dataDir, CandleInterval.CANDLE_INTERVAL_HOUR);
                createTickerJson(name, dataDir, levels);
            } catch (Exception ex) {
                out.println(ex.getMessage());
            }
        }
    }

    /**
     * Download and convert crypto data from Bybit. Checks existing candles and downloads only
     * missing data. Processes all coins in parallel.
     */
    private void downloadCryptoData(List<String> cryptoTickers, int historyDays) throws Exception {
        out.println("=== Downloading crypto data from Bybit ===");
        out.println("Crypto instruments: " + String.join(", ", cryptoTickers));

        LocalDate endDate = LocalDate.now();

        // Process all coins in parallel
        bybitService.downloadAndConvert(
                cryptoTickers, LocalDate.now().minusDays(historyDays), endDate);

        out.println("=== Crypto data download completed ===");
    }

    /**
     * Get start date from existing candles file, or calculate from historyDays if file doesn't
     * exist.
     */
    private LocalDate getStartDateFromExistingCandles(String ticker, int historyDays)
            throws IOException {
        Path candlesFile = Paths.get(config.getDataDir(), ticker, "candlesHOUR.txt");

        if (Files.exists(candlesFile)) {
            // Read last line to get the last candle date
            try (BufferedReader br = new BufferedReader(new FileReader(candlesFile.toFile()))) {
                String line = null;
                String lastLine = null;
                while ((line = br.readLine()) != null) {
                    if (!line.trim().isEmpty() && !line.startsWith("Datetime")) {
                        lastLine = line;
                    }
                }

                if (lastLine != null) {
                    String[] parts = lastLine.split(",");
                    if (parts.length > 0) {
                        // Parse date from format "dd.MM.yyyy HH:mm:ss"
                        String dateTimeStr = parts[0];
                        int spaceIdx = dateTimeStr.indexOf(' ');
                        String datePart =
                                spaceIdx > 0 ? dateTimeStr.substring(0, spaceIdx) : dateTimeStr;

                        try {
                            LocalDate lastCandleDate =
                                    LocalDate.parse(
                                            datePart, DateTimeFormatter.ofPattern("dd.MM.yyyy"));
                            out.println(
                                    "Found existing candles for "
                                            + ticker
                                            + " until "
                                            + lastCandleDate);

                            // Start from the day after the last candle
                            return lastCandleDate.plusDays(1);
                        } catch (Exception e) {
                            out.println("Could not parse last candle date: " + e.getMessage());
                        }
                    }
                }
            }
        }

        // No existing candles, use historyDays
        out.println("No existing candles for " + ticker + ", using historyDays=" + historyDays);
        return LocalDate.now().minusDays(historyDays);
    }

    public void updateCandlesFile(
            String name, String dir, CandleInterval period, boolean isReplace) {
        var namePeriod = period.name().replace("CANDLE_INTERVAL_", "");
        var file = dir + "/" + name + "/candles" + namePeriod + ".txt";
        if (!isReplace && isTodayFile(file)) {
            out.println("Exists candles '" + namePeriod + "' file: " + name);
            return;
        }

        out.println("Create candles '" + namePeriod + "' file: " + name);
        var historyDays = config.getHistoryDays();
        var lastCandleTime =
                Date.from(
                        LocalDate.now()
                                .minusDays(historyDays)
                                .atTime(LocalTime.MIDNIGHT)
                                .atZone(ZoneId.systemDefault())
                                .toInstant());
        if (!isReplace) {
            var currentCandles = readCandlesFile(name, dir, period);
            if (!currentCandles.isEmpty()) {
                try {
                    lastCandleTime =
                            dateTimeFormat.parse(
                                    currentCandles.get(currentCandles.size() - 1).getDate());
                } catch (ParseException ex) {
                    ex.printStackTrace();
                }
            }
        }
        List<TickerCandle> candles =
                getTickerCandles(name.toLowerCase(), period, lastCandleTime, 0);
        if (candles.isEmpty()) {
            throw new RuntimeException("empty candles");
        }

        if (isReplace) {
            try {
                deleteIfExists(Path.of(file));
            } catch (IOException ex) {
                ex.printStackTrace();
            }
        }

        var isFileExists = Files.exists(Path.of(file));
        try (FileWriter writer = new FileWriter(file, true)) {
            if (isReplace || !isFileExists) {
                writer.write("Datetime,Open,High,Low,Close,Volume" + System.lineSeparator());
            }
            for (TickerCandle candle : candles) {
                writer.write(
                        String.format(
                                        "%s,%s,%s,%s,%s,%s",
                                        candle.getDate(),
                                        candle.getOpen(),
                                        candle.getHigh(),
                                        candle.getLow(),
                                        candle.getClose(),
                                        candle.getVolume())
                                + System.lineSeparator());
            }
        } catch (Exception ex) {
            out.println(ex.getMessage());
            throw new RuntimeException(ex);
        }
    }

    private List<TickerCandle> getTickerCandles(
            String name, CandleInterval period, Date lastCandleTime, int counter) {
        Set<TickerCandle> candles = new LinkedHashSet<>();
        try {
            final Instant currentTime = now().toInstant();
            final Instant startTime = lastCandleTime.toInstant();

            TickerType type = TickerTypeResolver.resolve(name);
            TickerInfo.Key key = new TickerInfo.Key(name, type);
            TickerInfo tickerInfo = tickerRepository.getById(key);

            if (tickerInfo == null) {
                tickerInfo =
                        tickerRepository.getAll().values().stream()
                                .filter(
                                        it ->
                                                it.getName().equalsIgnoreCase(name)
                                                        || it.getTicker().equalsIgnoreCase(name))
                                .findFirst()
                                .orElse(null);
            }

            if (tickerInfo == null) {
                throw new RuntimeException("Ticker not found: " + name);
            }

            String ticker = tickerInfo.getFigi();
            var start = getStartWithShift(period, startTime);
            while (start.isBefore(currentTime)) {
                var end = start.plus(1, ChronoUnit.DAYS);
                out.println("Loading: " + name.toUpperCase() + "[" + start + " -> " + end + "]");
                List<HistoricCandle> periodCandles =
                        tcsService.getCandles(ticker, start, end, period);
                start = end;

                periodCandles.forEach(
                        candle -> {
                            var dateTime = new Timestamp(candle.getTime().getSeconds() * 1000);
                            var open = toDouble(candle.getOpen());
                            var high = toDouble(candle.getHigh());
                            var low = toDouble(candle.getLow());
                            var close = toDouble(candle.getClose());
                            var volume = candle.getVolume();
                            candles.add(
                                    new TickerCandle(
                                            name,
                                            dateTimeFormat.format(dateTime),
                                            open,
                                            high,
                                            low,
                                            close,
                                            close,
                                            (int) volume));
                        });
                sleep(100);
            }
        } catch (Exception ex) {
            if (counter++ < 2) {
                return getTickerCandles(name, period, lastCandleTime, counter);
            } else {
                out.println(ex.getMessage());
            }
        }
        return new ArrayList<>(candles);
    }

    private static Instant getStartWithShift(CandleInterval period, Instant startTime) {
        switch (period) {
            case CANDLE_INTERVAL_1_MIN:
                return startTime.plus(1, ChronoUnit.MINUTES);
            case CANDLE_INTERVAL_5_MIN:
                return startTime.plus(5, ChronoUnit.MINUTES);
            case CANDLE_INTERVAL_10_MIN:
                return startTime.plus(10, ChronoUnit.MINUTES);
            case CANDLE_INTERVAL_15_MIN:
                return startTime.plus(15, ChronoUnit.MINUTES);
            case CANDLE_INTERVAL_30_MIN:
                return startTime.plus(30, ChronoUnit.MINUTES);
            case CANDLE_INTERVAL_HOUR:
                return startTime.plus(1, ChronoUnit.HOURS);
            case CANDLE_INTERVAL_2_HOUR:
                return startTime.plus(2, ChronoUnit.HOURS);
            case CANDLE_INTERVAL_4_HOUR:
                return startTime.plus(4, ChronoUnit.HOURS);
            case CANDLE_INTERVAL_DAY:
                return startTime.plus(1, ChronoUnit.DAYS);
            case CANDLE_INTERVAL_WEEK:
                return startTime.plus(1, ChronoUnit.WEEKS);
            case CANDLE_INTERVAL_MONTH:
                return startTime.plus(1, ChronoUnit.MONTHS);
        }
        return startTime;
    }

    private List<Double> calculatePriceLevels(String name, String dir, CandleInterval period) {
        return new LevelUtils()
                .identifyKeyLevels(readCandlesFile(name, dir, period)).stream()
                        .map(Level::getPrice)
                        .sorted()
                        .collect(Collectors.toList());
    }

    private void createTickerJson(String name, String dir, List<Double> levels) {
        out.println("Create ticker json: " + name);
        try {
            deleteIfExists(Paths.get(dir + "/" + name + "/ticker.json"));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }

        try (FileWriter writer = new FileWriter(dir + "/" + name + "/ticker.json")) {
            TickerType type = TickerTypeResolver.resolve(name);
            TickerInfo.Key key = new TickerInfo.Key(name, type);
            TickerInfo tickerInfo = tickerRepository.getById(key);

            final TickerInfo resolvedTicker;
            if (tickerInfo == null) {
                resolvedTicker =
                        tickerRepository.getAll().values().stream()
                                .filter(
                                        it ->
                                                it.getName().equalsIgnoreCase(name)
                                                        || it.getTicker().equalsIgnoreCase(name))
                                .findFirst()
                                .orElse(null);
            } else {
                resolvedTicker = tickerInfo;
            }

            if (resolvedTicker == null) {
                throw new RuntimeException("Ticker not found: " + name);
            }

            Map<String, Object> json =
                    new HashMap<>() {
                        {
                            put("ticker", resolvedTicker);
                            put("levels", levels);
                        }
                    };
            objectMapper.writerWithDefaultPrettyPrinter().writeValue(writer, json);
        } catch (Exception ex) {
            out.println(ex.getMessage());
            ex.printStackTrace();
            throw new RuntimeException(ex);
        }
    }

    private boolean isTodayFile(String path) {
        var lastModified = new File(path).lastModified();
        var startOfDayDate = LocalDate.now().atStartOfDay();
        var fileDate =
                LocalDateTime.ofInstant(new Date(lastModified).toInstant(), ZoneId.systemDefault());
        return fileDate.isAfter(startOfDayDate);
    }

    public static List<TickerCandle> readCandlesFile(
            String name, String dir, CandleInterval period) {
        var namePeriod = period.name().replace("CANDLE_INTERVAL_", "");
        List<TickerCandle> tickers = new ArrayList<>();
        try (BufferedReader br =
                new BufferedReader(
                        new FileReader(dir + "/" + name + "/candles" + namePeriod + ".txt"))) {
            boolean skipHeader = true;
            String line = br.readLine();
            while (line != null) {
                if (skipHeader) {
                    skipHeader = false;
                    line = br.readLine();
                    continue;
                }

                String[] values = line.split(",");
                tickers.add(
                        new TickerCandle(
                                name,
                                values[0],
                                Double.valueOf(values[1]),
                                Double.valueOf(values[2]),
                                Double.valueOf(values[3]),
                                Double.valueOf(values[4]),
                                Double.valueOf(values[4]),
                                Integer.valueOf(values[5])));
                line = br.readLine();
            }
        } catch (Exception ex) {
            out.println(ex.getMessage());
        }
        return tickers;
    }
}
