package com.github.shk0da.GoldenDragon.strategy;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.shk0da.GoldenDragon.config.AILConfig;
import com.github.shk0da.GoldenDragon.model.TickerCandle;
import com.github.shk0da.GoldenDragon.model.TickerInfo;
import com.github.shk0da.GoldenDragon.model.TickerType;
import com.github.shk0da.GoldenDragon.repository.Repository;
import com.github.shk0da.GoldenDragon.repository.TickerRepository;
import com.github.shk0da.GoldenDragon.service.TCSService;
import ru.tinkoff.piapi.contract.v1.CandleInterval;
import ru.tinkoff.piapi.contract.v1.HistoricCandle;
import ru.tinkoff.piapi.contract.v1.Share;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.Timestamp;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import static com.github.shk0da.GoldenDragon.utils.IndicatorsUtil.toDouble;
import static java.lang.System.out;
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
    private final AILConfig ailConfig;

    public DataCollector(AILConfig ailConfig, TCSService tcsService) {
        this.tcsService = tcsService;
        this.ailConfig = ailConfig;
    }

    public void run() throws Exception {
        var dataDir = ailConfig.getDataDir();
        var tickers = ailConfig.getStocks();
        createDirectories(Paths.get(dataDir));
        for (String name : tickers) {
            try {
                createDirectories(Paths.get(dataDir + "/" + name));
                createCandlesFile(name, dataDir, CandleInterval.CANDLE_INTERVAL_5_MIN);
                createCandlesFile(name, dataDir, CandleInterval.CANDLE_INTERVAL_HOUR);
                var levels = calculatePriceLevels(name, dataDir, CandleInterval.CANDLE_INTERVAL_HOUR);
                createTickerJson(name, dataDir, levels);
            } catch (Exception ex) {
                out.println(ex.getMessage());
            }
        }
    }

    private void createCandlesFile(String name, String dir, CandleInterval period) {
        var namePeriod = period.name().replace("CANDLE_INTERVAL_", "");
        var file = dir + "/" + name + "/candles" + namePeriod + ".txt";
        if (isTodayFile(file)) {
            out.println("Exists candles '" + namePeriod + "' file: " + name);
            return;
        }

        out.println("Create candles '" + namePeriod + "' file: " + name);
        List<TickerCandle> candles = getTickerCandles(name.toLowerCase(), period, 0);
        if (candles.isEmpty()) {
            throw new RuntimeException("empty candles");
        }

        try {
            deleteIfExists(Path.of(file));
        } catch (IOException ex) {
            ex.printStackTrace();
        }
        try (FileWriter writer = new FileWriter(file)) {
            writer.write("Datetime,Open,High,Low,Close,Volume" + System.lineSeparator());
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

    private List<TickerCandle> getTickerCandles(String name, CandleInterval period, int counter) {
        List<TickerCandle> candles = new ArrayList<>();
        try {
            var currentTime = now();
            List<Share> stocks = tcsService.getMoexShares();
            String ticker = tickerRepository.getAll().values().stream()
                    .filter(it -> it.getType().equals(TickerType.STOCK))
                    .filter(it -> it.getName().equalsIgnoreCase(name) || it.getTicker().equalsIgnoreCase(name))
                    .map(TickerInfo::getFigi)
                    .findFirst()
                    .orElseThrow();
            stocks.stream().filter(it -> ticker.equals(it.getFigi())).forEach(stock -> {
                for (int i = 365 * 5; i >= 365; i = i - 1) {
                    var start = currentTime.minusDays(i + 1);
                    var end = currentTime.minusDays(i);
                    List<HistoricCandle> h1candles = tcsService.getCandles(
                            stock.getFigi(),
                            start,
                            end,
                            period);
                    out.println("Loading: " + stock.getTicker() + "[" + start + " -> " + end + "]");
                    h1candles.forEach(candle -> {
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
                    try {
                        TimeUnit.MILLISECONDS.sleep(100);
                    } catch (InterruptedException skip) {
                        // nothing
                    }
                }
            });
        } catch (Exception ex) {
            if (counter++ < 2) {
                return getTickerCandles(name, period, counter);
            } else {
                out.println(ex.getMessage());
            }
        }
        return candles;
    }

    private List<Double> calculatePriceLevels(String name, String dir, CandleInterval period) {
        if (isTodayFile(dir + "/" + name + "/levels.txt")) {
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
            command = "calculate_levels_arm";
        } else {
            command = "calculate_levels";
        }
        try {
            move(Paths.get(dir + "/" + name + "/candles" + namePeriod + ".txt"), Paths.get("candles.txt"), REPLACE_EXISTING);
            if (0 != Runtime.getRuntime().exec(command).waitFor()) {
                throw new RuntimeException("Not executed: calculate_levels");
            }
            move(Paths.get("candles.txt"), Paths.get(dir + "/" + name + "/candles" + namePeriod + ".txt"), REPLACE_EXISTING);
            move(Paths.get("levels.txt"), Paths.get(dir + "/" + name + "/levels.txt"), REPLACE_EXISTING);
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
                    .filter(it -> it.getType().equals(TickerType.STOCK))
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
}
