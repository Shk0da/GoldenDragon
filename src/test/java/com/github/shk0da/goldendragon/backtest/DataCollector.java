package com.github.shk0da.goldendragon.backtest;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.github.shk0da.goldendragon.config.MainConfig;
import com.github.shk0da.goldendragon.model.Candle;
import com.github.shk0da.goldendragon.model.TickerInfo;
import com.github.shk0da.goldendragon.model.TickerType;
import com.github.shk0da.goldendragon.repository.Repository;
import com.github.shk0da.goldendragon.repository.TickerRepository;
import com.github.shk0da.goldendragon.service.TCSService;
import com.github.shk0da.goldendragon.service.TradingService;
import com.github.shk0da.goldendragon.utils.TickerTypeResolver;
import com.google.gson.reflect.TypeToken;

import java.io.BufferedReader;
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
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import static com.github.shk0da.goldendragon.utils.SerializationUtils.getDateOfContentOnDisk;
import static com.github.shk0da.goldendragon.utils.SerializationUtils.loadDataFromDisk;
import static com.github.shk0da.goldendragon.utils.SerializationUtils.saveDataToDisk;
import static com.github.shk0da.goldendragon.utils.TimeUtils.sleep;
import static java.lang.System.out;
import static java.nio.file.Files.createDirectories;
import static java.nio.file.Files.deleteIfExists;
import static java.time.OffsetDateTime.now;

public class DataCollector {

    private static final ObjectMapper objectMapper =
        new ObjectMapper().registerModule(new JavaTimeModule());
    private static final DateFormat dateTimeFormat = new SimpleDateFormat("dd.MM.yyyy HH:mm:ss");
    private static final Repository<TickerInfo.Key, TickerInfo> tickerRepository =
        TickerRepository.INSTANCE;

    private final TradingService tcsService;
    private final DataCollectorConfig config;

    public DataCollector(DataCollectorConfig config, TradingService tcsService) {
        this.tcsService = tcsService;
        this.config = config;
    }

    public static void main(String[] args) throws Exception {
        DataCollectorConfig config = new DataCollectorConfig();
        var dataDir = config.getDataDir();
        var tickers = config.getInstruments();
        var isReplace = config.isReplace();
        var historyDays = config.getHistoryDays();

        createDirectories(Paths.get(dataDir));

        // Process instruments via Tinkoff API
        if (tickers != null && !tickers.isEmpty()) {
            out.println("=== Downloading instrument data from Tinkoff API ===");

            MainConfig mainConfig = new MainConfig();
            TCSService tcsService =
                new TCSService(
                    mainConfig.withAccountId(mainConfig.getTcsAccountId()));

            // Update ticker repository (load from disk or fetch from API if empty/stale)
            refreshTickerRepository(tcsService);

            DataCollector dataCollector = new DataCollector(config, tcsService);
            for (String name : tickers) {
                try {
                    createDirectories(Paths.get(dataDir + "/" + name));
                    dataCollector.updateCandlesFile(
                        name, dataDir, "5_MIN", isReplace);
                    dataCollector.updateCandlesFile(
                        name, dataDir, "HOUR", isReplace);
                    if (name.contains("@")) {
                        dataCollector.updateCandlesFile(
                            name, dataDir, "1_DAY", isReplace);
                    }
                    dataCollector.createTickerJson(name, dataDir);
                } catch (Exception ex) {
                    out.println(ex.getMessage());
                }
            }
            out.println("=== Instrument data download completed ===");
        }

        if (tickers == null || tickers.isEmpty()) {
            out.println("No instruments configured");
        }
    }

    private static void refreshTickerRepository(TCSService tcsService) throws Exception {
        AtomicReference<Map<TickerInfo.Key, TickerInfo>> tickerRegister =
            new AtomicReference<>(new HashMap<>());

        Callable<Boolean> isEmpty =
            () -> {
                Map<TickerInfo.Key, TickerInfo> dataFromDisk =
                    loadDataFromDisk(TickerRepository.SERIALIZE_NAME, new TypeToken<>() {
                    });
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
        var isReplace = config.isReplace();
        var historyDays = config.getHistoryDays();

        createDirectories(Paths.get(dataDir));

        // Process instruments
        for (String name : tickers) {
            try {
                createDirectories(Paths.get(dataDir + "/" + name));
                updateCandlesFile(name, dataDir, "5_MIN", isReplace);
                updateCandlesFile(name, dataDir, "HOUR", isReplace);
                if (name.contains("@")) {
                    updateCandlesFile(name, dataDir, "1_DAY", isReplace);
                }
                createTickerJson(name, dataDir);
            } catch (Exception ex) {
                out.println(ex.getMessage());
            }
        }
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

                            // Start from the day after the last candle
                            return lastCandleDate.plusDays(1);
                        } catch (Exception e) {
                        }
                    }
                }
            }
        }

        // No existing candles, use historyDays
        return LocalDate.now().minusDays(historyDays);
    }

    public void updateCandlesFile(
        String name, String dir, String period, boolean isReplace) {
        var file = dir + "/" + name + "/candles" + period + ".txt";
        var historyDays = config.getHistoryDays();
        var lastCandleTime =
            Date.from(
                LocalDate.now()
                    .minusDays(historyDays)
                    .atTime(LocalTime.MIDNIGHT)
                    .atZone(ZoneId.systemDefault())
                    .toInstant());
        String lastDateStr = null;
        if (!isReplace) {
            var currentCandles = readCandlesFile(name, dir, period);
            if (!currentCandles.isEmpty()) {
                lastDateStr = currentCandles.get(currentCandles.size() - 1).time;
                try {
                    lastCandleTime = dateTimeFormat.parse(lastDateStr);
                } catch (ParseException ex) {
                    ex.printStackTrace();
                }
            }
        }
        List<Candle> candles =
            getTickerCandles(name, period, lastCandleTime, lastDateStr, 0);

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
            for (Candle candle : candles) {
                writer.write(
                    String.format(
                        "%s,%s,%s,%s,%s,%s",
                        candle.time,
                        candle.open,
                        candle.high,
                        candle.low,
                        candle.close,
                        candle.volume)
                        + System.lineSeparator());
            }
        } catch (Exception ex) {
            out.println(ex.getMessage());
            throw new RuntimeException(ex);
        }
    }

    private List<Candle> getTickerCandles(
        String name, String period, Date lastCandleTime, String lastDateStr, int counter) {
        Set<Candle> candles = new LinkedHashSet<>();
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
            int daysChecked = 0;
            AtomicInteger newCandles = new AtomicInteger(0);
            while (start.isBefore(currentTime)) {
                var end = start.plus(1, ChronoUnit.DAYS);
                daysChecked++;

                List<Candle> periodCandles = tcsService.getCandles(ticker, start, end, period);
                start = end;

                periodCandles.forEach(
                    candle -> {
                        var dateTime = Timestamp.valueOf(candle.time);
                        var candleDate = dateTimeFormat.format(dateTime);
                        if (lastDateStr != null && candleDate.compareTo(lastDateStr) <= 0) {
                            return;
                        }
                        var open = candle.open;
                        var high = candle.high;
                        var low = candle.low;
                        var close = candle.close;
                        var volume = candle.volume;
                        candles.add(
                            new Candle(
                                candleDate,
                                open,
                                high,
                                low,
                                close,
                                volume));
                        newCandles.incrementAndGet();
                    });
                sleep(100);
            }
            if (daysChecked > 0 && newCandles.get() > 0) {
                out.println("Downloaded " + newCandles.get() + " new candles for " + name + " " + period);
            }
        } catch (Exception ex) {
            if (counter++ < 2) {
                return getTickerCandles(name, period, lastCandleTime, lastDateStr, counter);
            } else {
                out.println(ex.getMessage());
            }
        }
        return new ArrayList<>(candles);
    }

    private static Instant getStartWithShift(String period, Instant startTime) {
        switch (period) {
            case "1_MIN":
                return startTime.plus(1, ChronoUnit.MINUTES);
            case "5_MIN":
                return startTime.plus(5, ChronoUnit.MINUTES);
            case "10_MIN":
                return startTime.plus(10, ChronoUnit.MINUTES);
            case "15_MIN":
                return startTime.plus(15, ChronoUnit.MINUTES);
            case "30_MIN":
                return startTime.plus(30, ChronoUnit.MINUTES);
            case "HOUR":
                return startTime.plus(1, ChronoUnit.HOURS);
            case "2_HOUR":
                return startTime.plus(2, ChronoUnit.HOURS);
            case "4_HOUR":
                return startTime.plus(4, ChronoUnit.HOURS);
            case "1_DAY":
                return startTime.plus(1, ChronoUnit.DAYS);
            case "WEEK":
                return startTime.plus(1, ChronoUnit.WEEKS);
            case "MONTH":
                return startTime.plus(1, ChronoUnit.MONTHS);
        }
        return startTime;
    }

    private void createTickerJson(String name, String dir) {
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
                    }
                };
            objectMapper.writerWithDefaultPrettyPrinter().writeValue(writer, json);
        } catch (Exception ex) {
            out.println(ex.getMessage());
            ex.printStackTrace();
            throw new RuntimeException(ex);
        }
    }

    public static List<Candle> readCandlesFile(
        String name, String dir, String period) {
        List<Candle> tickers = new ArrayList<>();
        String filePath = dir + "/" + name + "/candles" + period + ".txt";

        // Return empty list if file doesn't exist (first run)
        if (!Files.exists(Path.of(filePath))) {
            return tickers;
        }

        try (BufferedReader br =
                 new BufferedReader(
                     new FileReader(filePath))) {
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
                    new Candle(
                        values[0],
                        Double.valueOf(values[1]),
                        Double.valueOf(values[2]),
                        Double.valueOf(values[3]),
                        Double.valueOf(values[4]),
                        Long.valueOf(values[5])));
                line = br.readLine();
            }
        } catch (Exception ex) {
            out.println(ex.getMessage());
        }
        return tickers;
    }
}
