package com.github.shk0da.GoldenDragon.strategy;

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
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;

import static com.github.shk0da.GoldenDragon.utils.IndicatorsUtil.toDouble;
import static java.lang.System.out;
import static java.nio.file.Files.copy;
import static java.nio.file.Files.createDirectories;
import static java.nio.file.Files.deleteIfExists;
import static java.nio.file.Files.exists;
import static java.nio.file.StandardCopyOption.REPLACE_EXISTING;
import static java.time.OffsetDateTime.now;

public class DataCollector {

    private static final String DATA_DIR = "data2";
    private static final List<String> TICKERS = List.of("GAZP", "ROSN", "LKOH", "NLMK", "SBER", "PIKK", "RTKM", "MGNT");

    Repository<TickerInfo.Key, TickerInfo> tickerRepository = TickerRepository.INSTANCE;

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
                createCandlesFile(name, DATA_DIR);
                calculatePriceLevels(name, DATA_DIR);
                readLevels(name, DATA_DIR);
            } catch (Exception ex) {
                out.println(ex.getMessage());
            }
        }
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
                                        stock.getFigi(),
                                        dateTime.toLocalDateTime().toString(),
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

    public void createCandlesFile(String name, String dir) {
        if (exists(Path.of(dir + "/candles.txt"))) {
            out.println("Candles file exists: " + name);
            return;
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
    }

    public void calculatePriceLevels(String name, String dir) {
        out.println("Calculate price levels: " + name);
        String command;
        try {
            String os = System.getProperty("os.name").toLowerCase();
            if (os.contains("windows")) {
                command = "calculate_levels.exe";
            } else if (os.contains("arm")) {
                command = "calculate_levels_arm";
            } else {
                command = "calculate_levels";
            }
            String commandTarget = dir + "/" + name + "/" + command;
            Path commandTargetPath = Paths.get(commandTarget);
            copy(Paths.get(command), commandTargetPath, REPLACE_EXISTING);
            if (0 != Runtime.getRuntime().exec(commandTarget).waitFor()) {
                throw new RuntimeException("Not executed: calculate_levels");
            }
            deleteIfExists(commandTargetPath);
        } catch (Exception ex) {
            out.println(ex.getMessage());
            throw new RuntimeException(ex);
        }
    }

    public List<Double> readLevels(String name, String dir) {
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
}
