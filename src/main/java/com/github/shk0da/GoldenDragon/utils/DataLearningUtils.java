package com.github.shk0da.GoldenDragon.utils;

import com.github.shk0da.GoldenDragon.config.AILConfig.NetworkProperties;
import com.github.shk0da.GoldenDragon.model.TickerCandle;
import com.github.shk0da.GoldenDragon.model.TickerJson;
import org.deeplearning4j.datasets.iterator.impl.ListDataSetIterator;
import org.deeplearning4j.nn.api.OptimizationAlgorithm;
import org.deeplearning4j.nn.conf.BackpropType;
import org.deeplearning4j.nn.conf.NeuralNetConfiguration;
import org.deeplearning4j.nn.conf.Updater;
import org.deeplearning4j.nn.conf.layers.DenseLayer;
import org.deeplearning4j.nn.conf.layers.LSTM;
import org.deeplearning4j.nn.conf.layers.RnnOutputLayer;
import org.deeplearning4j.nn.multilayer.MultiLayerNetwork;
import org.deeplearning4j.nn.weights.WeightInit;
import org.deeplearning4j.optimize.listeners.ScoreIterationListener;
import org.nd4j.linalg.activations.Activation;
import org.nd4j.linalg.dataset.DataSet;
import org.nd4j.linalg.dataset.api.iterator.DataSetIterator;
import org.nd4j.linalg.factory.Nd4j;
import org.nd4j.linalg.lossfunctions.LossFunctions;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static com.github.shk0da.GoldenDragon.utils.IndicatorsUtil.INDICATORS_SHIFT;
import static com.github.shk0da.GoldenDragon.utils.IndicatorsUtil.getIndicators;

public class DataLearningUtils {

    public static final class LSTMNetwork {

        public static MultiLayerNetwork buildLstmNetworks(DataSetIterator iterator, NetworkProperties properties) {
            var conf = new NeuralNetConfiguration.Builder()
                    .seed(properties.getSeed())
                    .iterations(properties.getIterations())
                    .learningRate(properties.getLearningRate())
                    .optimizationAlgo(OptimizationAlgorithm.STOCHASTIC_GRADIENT_DESCENT)
                    .weightInit(WeightInit.XAVIER)
                    .updater(Updater.RMSPROP)
                    .regularization(true)
                    .l2(properties.getL2())
                    .list()
                    .layer(0, new LSTM.Builder()
                            .nIn(iterator.inputColumns())
                            .nOut(properties.getLstmLayer1Size())
                            .activation(Activation.TANH)
                            .gateActivationFunction(Activation.HARDSIGMOID)
                            .dropOut(properties.getDropoutRatio())
                            .build())
                    .layer(1, new LSTM.Builder()
                            .nIn(properties.getLstmLayer1Size())
                            .nOut(properties.getLstmLayer2Size())
                            .activation(Activation.TANH)
                            .gateActivationFunction(Activation.HARDSIGMOID)
                            .dropOut(properties.getDropoutRatio())
                            .build())
                    .layer(2, new DenseLayer.Builder()
                            .nIn(properties.getLstmLayer2Size())
                            .nOut(properties.getDenseLayerSize())
                            .activation(Activation.RELU)
                            .build())
                    .layer(3, new RnnOutputLayer.Builder()
                            .nIn(properties.getDenseLayerSize())
                            .nOut(iterator.totalOutcomes())
                            .activation(Activation.IDENTITY)
                            .lossFunction(LossFunctions.LossFunction.MSE)
                            .build())
                    .backpropType(BackpropType.TruncatedBPTT)
                    .tBPTTForwardLength(properties.getTruncatedBPTTLength())
                    .tBPTTBackwardLength(properties.getTruncatedBPTTLength())
                    .pretrain(false)
                    .backprop(true)
                    .build();

            var net = new MultiLayerNetwork(conf);
            net.init();
            net.setListeners(new ScoreIterationListener(properties.getScore()));

            for (int i = 0; i < properties.getScore(); i++) {
                while (iterator.hasNext()) net.fit(iterator.next()); // fit model using mini-batch data
                iterator.reset(); // reset iterator
                net.rnnClearPreviousState(); // clear previous state
            }

            return net;
        }
    }

    public static final class StockDataSetIterator {

        private static final DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm:ss");

        public static float normalize(Double input) {
            var max = 10F;
            var min = 0.001F;
            if (input.intValue() > 10 || input.intValue() < -10) {
                var sizeMin = String.valueOf(input.intValue() / 10).length();
                var sizeMax = String.valueOf(input.intValue() * 10).length();
                min = Float.parseFloat(String.format("1%0" + sizeMin + "d", 0));
                max = Float.parseFloat(String.format("1%0" + sizeMax + "d", 0));
            }
            var result = (input.floatValue() - min) / (max - min);
            return (result <= 0.0F) ? 0.0F - result : result;
        }

        public static float[] getNetworkInput(List<TickerCandle> stockDataList, int i, Double startPrice, List<Double> levels) {
            var candle5 = stockDataList.get(i - 5); // свеча 5 назад
            var candle4 = stockDataList.get(i - 4); // свеча 4 назад
            var candle3 = stockDataList.get(i - 3); // свеча 3 назад
            var candle2 = stockDataList.get(i - 2); // свеча 2 назад
            var candle1 = stockDataList.get(i - 1); // свеча 1 назад

            var min5 = candle5.getLow(); // лой 5 свечей назад (25 мин)
            var min4 = candle4.getLow(); // лой 4 свечей назад (20 мин)
            var min3 = candle3.getLow(); // лой 3 свечей назад (15 мин)
            var min2 = candle2.getLow(); // лой 2 свечей назад (10 мин)
            var min1 = candle1.getLow(); // лой 1 свечей назад (5 мин)

            var max5 = candle5.getHigh(); // хай 5 свечей назад (25 мин)
            var max4 = candle4.getHigh(); // хай 4 свечей назад (20 мин)
            var max3 = candle3.getHigh(); // хай 3 свечей назад (15 мин)
            var max2 = candle2.getHigh(); // хай 2 свечей назад (10 мин)
            var max1 = candle1.getHigh(); // хай 1 свечей назад (5 мин)

            var volume5 = candle5.getVolume().doubleValue(); // объем 5 свечей назад (25 мин)
            var volume4 = candle4.getVolume().doubleValue(); // объем 4 свечей назад (20 мин)
            var volume3 = candle3.getVolume().doubleValue(); // объем 3 свечей назад (15 мин)
            var volume2 = candle2.getVolume().doubleValue(); // объем 2 свечей назад (10 мин)
            var volume1 = candle1.getVolume().doubleValue(); // объем 1 свечей назад (5 мин)

            var currentPrice = stockDataList.get(i).getClose(); // тек.цена

            var supportLevel = 0.0; // уровень снизу
            var resistanceLevel = 0.0; // уровень сверху
            for (Double level : levels) {
                if (level < currentPrice) {
                    supportLevel = level;
                }
                if (level > currentPrice) {
                    resistanceLevel = level;
                    break;
                }
            }

            var potentialToSupportLevel = currentPrice - supportLevel; // потенциал до уровня снизу
            var potentialToResistanceLevel = resistanceLevel - currentPrice; // потенциал до уровня сверху

            // индикаторы
            var indicators = getIndicators(stockDataList.subList(i - INDICATORS_SHIFT, i));
            var MABlack = indicators.get("MABlack").get(indicators.get("MABlack").size() - 1).getValue();
            var MAWhite = indicators.get("MAWhite").get(indicators.get("MAWhite").size() - 1).getValue();
            var MACD = indicators.get("MACD").get(indicators.get("MACD").size() - 1).getValue();
            var RSI = indicators.get("RSI").get(indicators.get("RSI").size() - 1).getValue();
            var OBV = indicators.get("OBV").get(indicators.get("OBV").size() - 1).getValue();
            var ADX = indicators.get("ADX").get(indicators.get("ADX").size() - 1).getValue();

            return new float[]{
                    normalize(startPrice),
                    normalize(min5), normalize(min4), normalize(min3), normalize(min2), normalize(min1),
                    normalize(max5), normalize(max4), normalize(max3), normalize(max2), normalize(max1),
                    normalize(volume5), normalize(volume4), normalize(volume3), normalize(volume2), normalize(volume1),
                    normalize(currentPrice),
                    normalize(supportLevel), normalize(resistanceLevel),
                    normalize(potentialToSupportLevel), normalize(potentialToResistanceLevel),
                    normalize(MABlack), normalize(MAWhite), normalize(MACD),
                    normalize(RSI), normalize(OBV), normalize(ADX),
            };
        }

        public static DataSetIterator createStockDataSetIterator(TickerJson ticker, List<TickerCandle> stockDataList) {
            List<DataSet> dataSets = new ArrayList<>();
            var startPrice = stockDataList.get(0).getClose(); // цена начала дня
            var startDay = LocalDateTime.parse(stockDataList.get(0).getDate(), formatter);
            for (int i = INDICATORS_SHIFT + 2016; i < stockDataList.size() - 10; i++) {
                LocalDateTime time = LocalDateTime.parse(stockDataList.get(i).getDate(), formatter);
                if (!startDay.equals(time) && time.getHour() <= 10) {
                    startPrice = stockDataList.get(i).getClose(); // цена начала дня
                }

                var levels = ticker.getLevels();
                var networkInput = getNetworkInput(stockDataList, i, startPrice, levels);

                var action = 0.0; // нейтрально

                var targetsMin = new ArrayList<Double>();
                targetsMin.add(stockDataList.get(i + 1).getLow());
                targetsMin.add(stockDataList.get(i + 2).getLow());
                targetsMin.add(stockDataList.get(i + 3).getLow());
                targetsMin.add(stockDataList.get(i + 4).getLow());
                targetsMin.add(stockDataList.get(i + 5).getLow());
                targetsMin.add(stockDataList.get(i + 6).getLow());
                targetsMin.add(stockDataList.get(i + 7).getLow());
                targetsMin.add(stockDataList.get(i + 8).getLow());
                targetsMin.add(stockDataList.get(i + 9).getLow());
                targetsMin.add(stockDataList.get(i + 10).getLow());

                var targetsMax = new ArrayList<Double>();
                targetsMax.add(stockDataList.get(i + 1).getHigh());
                targetsMax.add(stockDataList.get(i + 2).getHigh());
                targetsMax.add(stockDataList.get(i + 3).getHigh());
                targetsMax.add(stockDataList.get(i + 4).getHigh());
                targetsMax.add(stockDataList.get(i + 5).getHigh());
                targetsMax.add(stockDataList.get(i + 6).getHigh());
                targetsMax.add(stockDataList.get(i + 7).getHigh());
                targetsMax.add(stockDataList.get(i + 8).getHigh());
                targetsMax.add(stockDataList.get(i + 9).getHigh());
                targetsMax.add(stockDataList.get(i + 10).getHigh());

                var targetMax = Collections.max(targetsMax);
                var targetMin = Collections.min(targetsMin);

                var currentPrice = stockDataList.get(i).getClose(); // тек.цена
                if (targetMax > currentPrice && targetMin > currentPrice) {
                    if (((targetMax - currentPrice) * 100 / targetMax) > 1) {
                        action = 0.6; // покупка с тп
                    }
                    if (((targetMax - currentPrice) * 100 / targetMax) > 1.5) {
                        action = 0.7; // покупка с тп
                    }
                    if (((targetMax - currentPrice) * 100 / targetMax) > 2) {
                        action = 0.8; // покупка с тп
                    }
                    if (((targetMax - currentPrice) * 100 / targetMax) > 2.5) {
                        action = 0.9; // покупка с тп
                    }
                    if (((targetMax - currentPrice) * 100 / targetMax) > 3) {
                        action = 1.0; // покупка с тп
                    }
                }

                if (targetMin < currentPrice && targetMax < currentPrice) {
                    if (((currentPrice - targetMin) * 100 / currentPrice) > 1) {
                        action = -0.6; // продажа с тп
                    }
                    if (((currentPrice - targetMin) * 100 / currentPrice) > 1.5) {
                        action = -0.7; // продажа с тп
                    }
                    if (((currentPrice - targetMin) * 100 / currentPrice) > 2) {
                        action = -0.8; // продажа с тп
                    }
                    if (((currentPrice - targetMin) * 100 / currentPrice) > 2.5) {
                        action = -0.9; // продажа с тп
                    }
                    if (((currentPrice - targetMin) * 100 / currentPrice) > 3) {
                        action = -1.0; // продажа с тп
                    }
                }

                var input = Nd4j.create(networkInput);
                var label = Nd4j.create(new double[]{action});
                dataSets.add(new DataSet(input, label));
            }
            return new ListDataSetIterator<>(dataSets);
        }
    }
}
