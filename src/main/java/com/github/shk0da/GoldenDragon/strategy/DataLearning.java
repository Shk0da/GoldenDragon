package com.github.shk0da.GoldenDragon.strategy;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.shk0da.GoldenDragon.config.AILConfig;
import com.github.shk0da.GoldenDragon.model.TickerCandle;
import com.github.shk0da.GoldenDragon.model.TickerJson;
import org.deeplearning4j.nn.multilayer.MultiLayerNetwork;
import org.deeplearning4j.util.ModelSerializer;
import org.nd4j.linalg.dataset.api.iterator.DataSetIterator;
import ru.tinkoff.piapi.contract.v1.CandleInterval;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.List;

import static com.github.shk0da.GoldenDragon.utils.DataLearningUtils.LSTMNetwork.buildLstmNetworks;
import static com.github.shk0da.GoldenDragon.utils.DataLearningUtils.StockDataSetIterator.createStockDataSetIterator;
import static java.lang.System.out;

public class DataLearning {

    private static final ObjectMapper objectMapper = new ObjectMapper();

    private final AILConfig ailConfig;

    public DataLearning(AILConfig ailConfig) {
        this.ailConfig = ailConfig;
    }

    public void run() {
        var dataDir = ailConfig.getDataDir();
        var tickers = ailConfig.getStocks();
        for (String name : tickers) {
            try {
                var ticker = readTickerFile(name, dataDir);
                var candles = readCandlesFile(name, dataDir, CandleInterval.CANDLE_INTERVAL_5_MIN);
                learnNetwork(dataDir, ticker, candles);
            } catch (Exception ex) {
                out.println(ex.getMessage());
                ex.printStackTrace();
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
            throw new RuntimeException(ex);
        }
        return tickers;
    }
}
