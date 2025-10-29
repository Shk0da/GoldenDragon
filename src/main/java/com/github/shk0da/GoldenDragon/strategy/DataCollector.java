package com.github.shk0da.GoldenDragon.strategy;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.shk0da.GoldenDragon.config.DataCollectorConfig;
import com.github.shk0da.GoldenDragon.model.TickerCandle;
import com.github.shk0da.GoldenDragon.model.TickerInfo;
import com.github.shk0da.GoldenDragon.repository.Repository;
import com.github.shk0da.GoldenDragon.repository.TickerRepository;
import com.github.shk0da.GoldenDragon.service.TCSService;
import ru.tinkoff.piapi.contract.v1.CandleInterval;
import ru.tinkoff.piapi.contract.v1.HistoricCandle;

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
import java.time.ZoneId;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static com.github.shk0da.GoldenDragon.model.TickerType.FEATURE;
import static com.github.shk0da.GoldenDragon.model.TickerType.STOCK;
import static com.github.shk0da.GoldenDragon.utils.IndicatorsUtil.toDouble;
import static com.github.shk0da.GoldenDragon.utils.TimeUtils.sleep;
import static java.lang.System.out;
import static java.nio.file.Files.copy;
import static java.nio.file.Files.createDirectories;
import static java.nio.file.Files.deleteIfExists;
import static java.nio.file.Files.move;
import static java.nio.file.StandardCopyOption.REPLACE_EXISTING;
import static java.time.OffsetDateTime.now;

public class DataCollector {

    private static final ObjectMapper objectMapper = new ObjectMapper();
    private static final DateFormat dateTimeFormat = new SimpleDateFormat("dd.MM.yyyy HH:mm:ss");
    private static final Repository<TickerInfo.Key, TickerInfo> tickerRepository = TickerRepository.INSTANCE;

    private final TCSService tcsService;
    private final DataCollectorConfig config;

    public DataCollector(DataCollectorConfig config, TCSService tcsService) {
        this.tcsService = tcsService;
        this.config = config;
    }

    public void run() throws Exception {
        var dataDir = config.getDataDir();
        var tickers = config.getStocks();
        var isReplace = config.isReplace();
        createDirectories(Paths.get(dataDir));
        for (String name : tickers) {
            try {
                createDirectories(Paths.get(dataDir + "/" + name));
                createCandlesFile(name, dataDir, CandleInterval.CANDLE_INTERVAL_5_MIN, isReplace);
                createCandlesFile(name, dataDir, CandleInterval.CANDLE_INTERVAL_HOUR, isReplace);
                var levels = calculatePriceLevels(name, dataDir, CandleInterval.CANDLE_INTERVAL_HOUR, isReplace);
                createTickerJson(name, dataDir, levels);
            } catch (Exception ex) {
                out.println(ex.getMessage());
            }
        }
    }

    private void createCandlesFile(String name, String dir, CandleInterval period, boolean isReplace) {
        var namePeriod = period.name().replace("CANDLE_INTERVAL_", "");
        var file = dir + "/" + name + "/candles" + namePeriod + ".txt";
        if (!isReplace && isTodayFile(file)) {
            out.println("Exists candles '" + namePeriod + "' file: " + name);
            return;
        }

        out.println("Create candles '" + namePeriod + "' file: " + name);
        var lastCandleTime = new org.joda.time.LocalDate().minusYears(5).toDate();
        if (!isReplace) {
            var currentCandles = readCandlesFile(name, dir, period);
            if (!currentCandles.isEmpty()) {
                try {
                    lastCandleTime = dateTimeFormat.parse(currentCandles.get(currentCandles.size() - 1).getDate());
                } catch (ParseException ex) {
                    ex.printStackTrace();
                }
            }
        }
        List<TickerCandle> candles = getTickerCandles(name.toLowerCase(), period, lastCandleTime, 0);
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
                writer.write(String.format(
                        "%s,%s,%s,%s,%s,%s",
                        candle.getDate(),
                        candle.getOpen(),
                        candle.getHigh(),
                        candle.getLow(),
                        candle.getClose(),
                        candle.getVolume()
                ) + System.lineSeparator());
            }
        } catch (Exception ex) {
            out.println(ex.getMessage());
            throw new RuntimeException(ex);
        }
    }

    private List<TickerCandle> getTickerCandles(String name, CandleInterval period, Date lastCandleTime, int counter) {
        Set<TickerCandle> candles = new LinkedHashSet<>();
        try {
            final Instant currentTime = now().toInstant();
            final Instant startTime = lastCandleTime.toInstant();
            String ticker = tickerRepository.getAll().values().stream()
                    .filter(it -> it.getType().equals(STOCK) || it.getType().equals(FEATURE))
                    .filter(it -> it.getName().equalsIgnoreCase(name) || it.getTicker().equalsIgnoreCase(name))
                    .map(TickerInfo::getFigi)
                    .findFirst()
                    .orElseThrow();
            var start = getStartWithShift(period, startTime);
            while (start.isBefore(currentTime)) {
                var end = start.plus(1, ChronoUnit.DAYS);
                out.println("Loading: " + name.toUpperCase() + "[" + start + " -> " + end + "]");
                List<HistoricCandle> periodCandles = tcsService.getCandles(ticker, start, end, period);
                start = end;

                periodCandles.forEach(candle -> {
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
                                    (int) volume
                            )
                    );
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

    private List<Double> calculatePriceLevels(String name, String dir, CandleInterval period, boolean isReplace) {
        if (!isReplace && isTodayFile(dir + "/" + name + "/levels.txt")) {
            out.println("Exists price levels: " + name);
            return readLevels(name, dir);
        }

        var namePeriod = period.name().replace("CANDLE_INTERVAL_", "");
        out.println("Calculate price levels: " + name);
        String command;
        String os = System.getProperty("os.name").toLowerCase();
        if (os.contains("windows")) {
            command = "calculate_levels.exe";
        } else if (os.contains("arm")) {
            command = "./calculate_levels_arm";
        } else {
            command = "./calculate_levels";
        }
        try {
            copy(Paths.get(dir + "/" + name + "/candles" + namePeriod + ".txt"), Paths.get("candles.txt"), REPLACE_EXISTING);
            if (0 != Runtime.getRuntime().exec(command).waitFor()) {
                throw new RuntimeException("Not executed: calculate_levels");
            }
            move(Paths.get("levels.txt"), Paths.get(dir + "/" + name + "/levels.txt"), REPLACE_EXISTING);
            deleteIfExists(Paths.get("candles.txt"));
        } catch (Exception ex) {
            out.println(ex.getMessage());
            throw new RuntimeException(ex);
        }

        return readLevels(name, dir);
    }

    private List<Double> readLevels(String name, String dir) {
        out.println("Read levels: " + name);
        List<Double> levels = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader(dir + "/" + name + "/levels.txt"))) {
            String line = br.readLine();
            while (line != null) {
                levels.add(Double.valueOf(line));
                line = br.readLine();
            }
        } catch (Exception ex) {
            out.println(ex.getMessage());
            throw new RuntimeException(ex);
        }
        return levels;
    }

    private void createTickerJson(String name, String dir, List<Double> levels) {
        out.println("Create ticker json: " + name);
        try {
            deleteIfExists(Paths.get(dir + "/" + name + "/ticker.json"));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }

        try (FileWriter writer = new FileWriter(dir + "/" + name + "/ticker.json")) {
            TickerInfo ticker = tickerRepository.getAll().values().stream()
                    .filter(it -> it.getType().equals(STOCK))
                    .filter(it -> it.getName().equalsIgnoreCase(name) || it.getTicker().equalsIgnoreCase(name))
                    .findFirst()
                    .orElseThrow();
            Map<String, Object> json = new HashMap<>() {{
                put("ticker", ticker);
                put("levels", levels);
            }};
            objectMapper.writerWithDefaultPrettyPrinter().writeValue(writer, json);
        } catch (Exception ex) {
            out.println(ex.getMessage());
            throw new RuntimeException(ex);
        }
    }

    private boolean isTodayFile(String path) {
        var lastModified = new File(path).lastModified();
        var startOfDayDate = LocalDate.now().atStartOfDay();
        var fileDate = LocalDateTime.ofInstant(new Date(lastModified).toInstant(), ZoneId.systemDefault());
        return fileDate.isAfter(startOfDayDate);
    }

    public static List<TickerCandle> readCandlesFile(String name, String dir, CandleInterval period) {
        var namePeriod = period.name().replace("CANDLE_INTERVAL_", "");
        out.println("Read '" + namePeriod + "' candles file: " + name);
        List<TickerCandle> tickers = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader(dir + "/" + name + "/candles" + namePeriod + ".txt"))) {
            boolean skipHeader = true;
            String line = br.readLine();
            while (line != null) {
                if (skipHeader) {
                    skipHeader = false;
                    line = br.readLine();
                    continue;
                }

                String[] values = line.split(",");
                tickers.add(new TickerCandle(
                        name,
                        values[0],
                        Double.valueOf(values[1]),
                        Double.valueOf(values[2]),
                        Double.valueOf(values[3]),
                        Double.valueOf(values[4]),
                        Double.valueOf(values[4]),
                        Integer.valueOf(values[5])
                ));
                line = br.readLine();
            }
        } catch (Exception ex) {
            out.println(ex.getMessage());
        }
        return tickers;
    }
}
