package com.github.shk0da.GoldenDragon.strategy;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.shk0da.GoldenDragon.config.MainConfig;
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
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.Timestamp;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.time.temporal.ChronoUnit;
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
import static java.nio.file.Files.exists;
import static java.nio.file.Files.move;
import static java.nio.file.StandardCopyOption.REPLACE_EXISTING;
import static java.time.LocalDateTime.ofInstant;
import static java.time.OffsetDateTime.now;
import static java.time.ZoneId.systemDefault;

public class DataCollector {

    public static final String DATA_DIR = "data";
    public static final List<String> TICKERS = List.of("GAZP", "ROSN", "LKOH", "NLMK", "SBER", "PIKK", "RTKM", "MGNT");

    private static final ObjectMapper objectMapper = new ObjectMapper();
    private static final DateFormat dateTimeFormat = new SimpleDateFormat("dd.MM.yyyy HH:mm:ss");
    private static final Repository<TickerInfo.Key, TickerInfo> tickerRepository = TickerRepository.INSTANCE;

    private final TCSService tcsService;
    private final MainConfig mainConfig;

    public DataCollector(MainConfig mainConfig, TCSService tcsService) {
        this.tcsService = tcsService;
        this.mainConfig = mainConfig;
    }

    public void run() throws Exception {
        createDirectories(Paths.get(DATA_DIR));
        for (String name : TICKERS) {
            try {
                createDirectories(Paths.get(DATA_DIR + "/" + name));
                var candles = createCandlesFile(name, DATA_DIR);
                var levels = calculatePriceLevels(name, DATA_DIR);
                var atr = calculateATR(name, candles, 7);
                createTickerJson(name, DATA_DIR, levels, atr);
            } catch (Exception ex) {
                out.println(ex.getMessage());
            }
        }
    }

    private List<TickerCandle> createCandlesFile(String name, String dir) {
        if (exists(Path.of(dir + "/" + name + "/candles.txt"))) {
            out.println("Candles file exists: " + name);
            return readCandlesFile(name, dir);
        }

        out.println("Create candles file: " + name);
        List<TickerCandle> H1 = getTickerCandles(name.toLowerCase(), CandleInterval.CANDLE_INTERVAL_HOUR, 0);
        if (H1.isEmpty()) {
            throw new RuntimeException("empty candles");
        }
        try (FileWriter writer = new FileWriter(dir + "/" + name + "/candles.txt")) {
            writer.write("Datetime,Open,High,Low,Close,Volume" + System.lineSeparator());
            for (TickerCandle candle : H1) {
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
        return H1;
    }

    public static List<TickerCandle> readCandlesFile(String name, String dir) {
        out.println("Read candles file: " + name);
        List<TickerCandle> tickers = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader(dir + "/" + name + "/candles.txt"))) {
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
            throw new RuntimeException(ex);
        }
        return tickers;
    }

    private List<TickerCandle> getTickerCandles(String name, CandleInterval period, int counter) {
        List<TickerCandle> candles = new ArrayList<>();
        try {
            var currentTime = now();
            List<Share> stocks = tcsService.getMoexShares();
            String ticker = tickerRepository.getAll().values().stream()
                    .filter(it -> it.getType().equals(TickerType.STOCK))
                    .filter(it -> it.getName().toLowerCase().contains(name) || it.getTicker().toLowerCase().contains(name))
                    .map(TickerInfo::getFigi)
                    .findFirst()
                    .orElseThrow();
            stocks.stream().filter(it -> ticker.equals(it.getFigi())).forEach(stock -> {
                for (int i = 365 * 3; i >= 365; i = i - 1) {
                    var start = currentTime.minusDays(i + 1);
                    var end = currentTime.minusDays(i);
                    List<HistoricCandle> h1candles = tcsService.getCandles(
                            stock.getFigi(),
                            start,
                            end,
                            period);
                    out.println("Loading: " + stock.getFigi() + "[" + start + " -> " + end + "]");
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

    private List<Double> calculatePriceLevels(String name, String dir) {
        if (exists(Path.of(dir + "/" + name + "/levels.txt"))) {
            out.println("Price levels file exists: " + name);
            return readLevels(name, DATA_DIR);
        }

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
            move(Paths.get(dir + "/" + name + "/candles.txt"), Paths.get("candles.txt"), REPLACE_EXISTING);
            if (0 != Runtime.getRuntime().exec(command).waitFor()) {
                throw new RuntimeException("Not executed: calculate_levels");
            }
            move(Paths.get("candles.txt"), Paths.get(dir + "/" + name + "/candles.txt"), REPLACE_EXISTING);
            move(Paths.get("levels.txt"), Paths.get(dir + "/" + name + "/levels.txt"), REPLACE_EXISTING);
        } catch (Exception ex) {
            out.println(ex.getMessage());
            throw new RuntimeException(ex);
        }

        return readLevels(name, DATA_DIR);
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

    private Double calculateATR(String name, List<TickerCandle> candles, int period) throws Exception {
        out.println("Calculate ATR: " + name);
        List<TickerCandle> D1 = convertCandles(candles, 24, ChronoUnit.HOURS);
        if (D1.size() < period + 1) {
            return Double.MAX_VALUE;
        }
        double atr = 0.0;
        for (TickerCandle candle : D1.subList(D1.size() - period + 1, D1.size() - 1)) {
            atr += candle.getHigh() - candle.getLow();
        }
        return atr / period;
    }

    private List<TickerCandle> convertCandles(List<TickerCandle> candles, long newTimeFrame, ChronoUnit unit) throws Exception {
        List<TickerCandle> newCandles = new ArrayList<>();
        if (candles.isEmpty()) return newCandles;

        double open = 0.0;
        double high = Double.MIN_VALUE;
        double low = Double.MAX_VALUE;
        int volume = 0;

        var currentTimestamp = ofInstant(dateTimeFormat.parse(candles.get(0).getDate()).toInstant(), systemDefault());
        for (TickerCandle candle : candles) {
            open = 0.0 == open ? candle.getOpen() : open;
            var timestamp = ofInstant(dateTimeFormat.parse(candle.getDate()).toInstant(), systemDefault());
            if (timestamp.isEqual(currentTimestamp.plus(newTimeFrame, unit))) {
                newCandles.add(new TickerCandle(
                        candle.getSymbol(),
                        dateTimeFormat.format(Date.from(currentTimestamp.atZone(systemDefault()).toInstant())),
                        open,
                        high,
                        low,
                        candle.getClose(),
                        candle.getClose(),
                        volume
                ));
            } else if (timestamp.isAfter(currentTimestamp.plus(newTimeFrame, unit))) {
                currentTimestamp = timestamp;//.minusHours(1);
                open = candle.getOpen();
                high = candle.getHigh();
                low = candle.getLow();
            } else {
                high = Math.max(high, candle.getHigh());
                low = Math.min(low, candle.getLow());
                volume += candle.getVolume();
            }
        }
        return newCandles;
    }

    private void createTickerJson(String name, String dir, List<Double> levels, Double atr) {
        out.println("Create ticker json: " + name);
        try {
            deleteIfExists(Paths.get(dir + "/" + name + "/ticker.json"));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }

        try (FileWriter writer = new FileWriter(dir + "/" + name + "/ticker.json")) {
            TickerInfo ticker = tickerRepository.getAll().values().stream()
                    .filter(it -> it.getType().equals(TickerType.STOCK))
                    .filter(it -> it.getName().toLowerCase().contains(name.toLowerCase())
                            || it.getTicker().toLowerCase().contains(name.toLowerCase()))
                    .findFirst()
                    .orElseThrow();
            Map<String, Object> json = new HashMap<>() {{
                put("ticker", ticker);
                put("levels", levels);
                put("atr", atr);
            }};
            objectMapper.writeValue(writer, json);
        } catch (Exception ex) {
            out.println(ex.getMessage());
            throw new RuntimeException(ex);
        }
    }
}
