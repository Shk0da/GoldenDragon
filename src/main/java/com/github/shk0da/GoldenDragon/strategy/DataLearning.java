package com.github.shk0da.GoldenDragon.strategy;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.shk0da.GoldenDragon.config.AILConfig;
import com.github.shk0da.GoldenDragon.model.TickerCandle;
import com.github.shk0da.GoldenDragon.model.TickerInfo;
import com.github.shk0da.GoldenDragon.model.TickerJson;
import com.github.shk0da.GoldenDragon.model.TickerType;
import com.github.shk0da.GoldenDragon.repository.Repository;
import com.github.shk0da.GoldenDragon.repository.TickerRepository;
import com.github.shk0da.GoldenDragon.service.TCSService;
import org.deeplearning4j.nn.multilayer.MultiLayerNetwork;
import org.deeplearning4j.util.ModelSerializer;
import org.nd4j.linalg.dataset.api.iterator.DataSetIterator;
import ru.tinkoff.piapi.contract.v1.CandleInterval;
import ru.tinkoff.piapi.contract.v1.HistoricCandle;
import ru.tinkoff.piapi.contract.v1.Share;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.nio.file.Path;
import java.sql.Timestamp;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;

import static com.github.shk0da.GoldenDragon.utils.DeepLearningUtils.LSTMNetwork.buildLstmNetworks;
import static com.github.shk0da.GoldenDragon.utils.DeepLearningUtils.StockDataSetIterator.createStockDataSetIterator;
import static com.github.shk0da.GoldenDragon.utils.IndicatorsUtil.toDouble;
import static java.lang.System.out;
import static java.nio.file.Files.exists;
import static java.time.OffsetDateTime.now;

public class DataLearning {

    private static final ObjectMapper objectMapper = new ObjectMapper();
    private static final DateFormat dateTimeFormat = new SimpleDateFormat("dd.MM.yyyy HH:mm:ss");
    private static final Repository<TickerInfo.Key, TickerInfo> tickerRepository = TickerRepository.INSTANCE;

    private final AILConfig ailConfig;
    private final TCSService tcsService;

    public DataLearning(AILConfig ailConfig, TCSService tcsService) {
        this.ailConfig = ailConfig;
        this.tcsService = tcsService;
    }

    public void run() {
        var dataDir = ailConfig.getDataDir();
        var tickers = ailConfig.getStocks();
        for (String name : tickers) {
            try {
                var ticker = readTickerFile(name, dataDir);
                var candles = createCandlesFile(name, dataDir);
                learnNetwork(dataDir, ticker, candles);
            } catch (Exception ex) {
                out.println(ex.getMessage());
            }
        }
    }

    private void learnNetwork(String dataDir, TickerJson ticker, List<TickerCandle> candles) throws Exception {
        String name = ticker.getTicker().getTicker().toUpperCase();
        out.println("Learn network: " + name);
        DataSetIterator dataSetIterator = createStockDataSetIterator(ticker, candles);
        MultiLayerNetwork neuralNetwork = buildLstmNetworks(dataSetIterator);

        String filePath = dataDir + "/" + name + "/network.nn";
        ModelSerializer.writeModel(neuralNetwork, filePath, true);
    }

    private TickerJson readTickerFile(String name, String dataDir) throws Exception {
        out.println("Read ticker file: " + name);
        return objectMapper.readValue(new File(dataDir + "/" + name + "/ticker.json"), TickerJson.class);
    }

    private List<TickerCandle> createCandlesFile(String name, String dir) {
        if (exists(Path.of(dir + "/" + name + "/candlesM5.txt"))) {
            out.println("Candles M5 file exists: " + name);
            return readCandlesFile(name, dir);
        }

        out.println("Create M5 candles file: " + name);
        List<TickerCandle> M5 = getTickerCandles(name.toLowerCase(), CandleInterval.CANDLE_INTERVAL_5_MIN, 0);
        if (M5.isEmpty()) {
            throw new RuntimeException("empty candles");
        }
        try (FileWriter writer = new FileWriter(dir + "/" + name + "/candlesM5.txt")) {
            writer.write("Datetime,Open,High,Low,Close,Volume" + System.lineSeparator());
            for (TickerCandle candle : M5) {
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
        return M5;
    }

    public static List<TickerCandle> readCandlesFile(String name, String dir) {
        out.println("Read M5 candles file: " + name);
        List<TickerCandle> tickers = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader(dir + "/" + name + "/candlesM5.txt"))) {
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
                    .filter(it -> it.getName().equalsIgnoreCase(name) || it.getTicker().equalsIgnoreCase(name))
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
}
