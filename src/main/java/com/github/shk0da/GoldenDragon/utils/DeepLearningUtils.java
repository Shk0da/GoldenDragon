package com.github.shk0da.GoldenDragon.utils;

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
import java.util.List;

public class DeepLearningUtils {

    public static final class LSTMNetwork {

        private static final Double learningRate = 0.05;
        private static final Integer iterations = 1;
        private static final Integer seed = 777;
        private static final Integer score = 100;

        private static final Integer lstmLayer1Size = 256;
        private static final Integer lstmLayer2Size = 256;
        private static final Integer denseLayerSize = 32;
        private static final Double dropoutRatio = 0.2;
        private static final Integer truncatedBPTTLength = 22;

        public static MultiLayerNetwork buildLstmNetworks(DataSetIterator iterator) {
            var conf = new NeuralNetConfiguration.Builder()
                    .seed(seed)
                    .iterations(iterations)
                    .learningRate(learningRate)
                    .optimizationAlgo(OptimizationAlgorithm.STOCHASTIC_GRADIENT_DESCENT)
                    .weightInit(WeightInit.XAVIER)
                    .updater(Updater.RMSPROP)
                    .regularization(true)
                    .l2(1e-4)
                    .list()
                    .layer(0, new LSTM.Builder()
                            .nIn(iterator.inputColumns())
                            .nOut(lstmLayer1Size)
                            .activation(Activation.TANH)
                            .gateActivationFunction(Activation.HARDSIGMOID)
                            .dropOut(dropoutRatio)
                            .build())
                    .layer(1, new LSTM.Builder()
                            .nIn(lstmLayer1Size)
                            .nOut(lstmLayer2Size)
                            .activation(Activation.TANH)
                            .gateActivationFunction(Activation.HARDSIGMOID)
                            .dropOut(dropoutRatio)
                            .build())
                    .layer(2, new DenseLayer.Builder()
                            .nIn(lstmLayer2Size)
                            .nOut(denseLayerSize)
                            .activation(Activation.RELU)
                            .build())
                    .layer(3, new RnnOutputLayer.Builder()
                            .nIn(denseLayerSize)
                            .nOut(iterator.totalOutcomes())
                            .activation(Activation.IDENTITY)
                            .lossFunction(LossFunctions.LossFunction.MSE)
                            .build())
                    .backpropType(BackpropType.TruncatedBPTT)
                    .tBPTTForwardLength(truncatedBPTTLength)
                    .tBPTTBackwardLength(truncatedBPTTLength)
                    .pretrain(false)
                    .backprop(true)
                    .build();

            var net = new MultiLayerNetwork(conf);
            net.init();
            net.setListeners(new ScoreIterationListener(score));

            for (int i = 0; i < score; i++) {
                while (iterator.hasNext()) net.fit(iterator.next()); // fit model using mini-batch data
                iterator.reset(); // reset iterator
                net.rnnClearPreviousState(); // clear previous state
            }

            return net;
        }
    }

    public static final class StockDataSetIterator {

        private static final DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm:ss");

        public static DataSetIterator createStockDataSetIterator(TickerJson ticker, List<TickerCandle> stockDataList) {
            List<DataSet> dataSets = new ArrayList<>();
            var startPrice = stockDataList.get(0).getClose(); // цена начала дня
            var startDay = LocalDateTime.parse(stockDataList.get(0).getDate(), formatter);
            for (int i = 6; i < stockDataList.size() - 10; i++) {
                LocalDateTime time = LocalDateTime.parse(stockDataList.get(i).getDate(), formatter);
                if (!startDay.equals(time) && time.getHour() <= 9) {
                    startPrice = stockDataList.get(i).getClose(); // цена начала дня
                }

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

                var volume5 = candle5.getVolume(); // объем 5 свечей назад (25 мин)
                var volume4 = candle4.getVolume(); // объем 4 свечей назад (20 мин)
                var volume3 = candle3.getVolume(); // объем 3 свечей назад (15 мин)
                var volume2 = candle2.getVolume(); // объем 2 свечей назад (10 мин)
                var volume1 = candle1.getVolume(); // объем 1 свечей назад (5 мин)

                var currentPrice = stockDataList.get(i).getClose(); // тек.цена

                var supportLevel = 0.0; // уровень снизу
                var resistanceLevel = 0.0; // уровень сверху
                for (Double level : ticker.getLevels()) {
                    if (level < currentPrice) {
                        supportLevel = level;
                    }
                    if (level > currentPrice) {
                        resistanceLevel = level;
                        break;
                    }
                }

                var atr = ticker.getAtr(); // atr
                var potentialToSupportLevel = currentPrice - supportLevel; // потенциал до уровня снизу
                var potentialToResistanceLevel = resistanceLevel - currentPrice; // потенциал до уровня сверху

                var action = 0.5; // нейтрально
                var target = stockDataList.get(i + 10).getClose();
                if (target > currentPrice) {
                    if (((target - currentPrice) * 100 / target) > 1) {
                        action = 0.6; // покупка с тп
                    }
                    if (((target - currentPrice) * 100 / target) > 2) {
                        action = 0.7; // покупка с тп
                    }
                    if (((target - currentPrice) * 100 / target) > 3) {
                        action = 0.8; // покупка с тп
                    }
                    if (((target - currentPrice) * 100 / target) > 4) {
                        action = 0.9; // покупка с тп
                    }
                    if (((target - currentPrice) * 100 / target) > 5) {
                        action = 1.0; // покупка с тп
                    }
                }
                if (target < currentPrice) {
                    if (((currentPrice - target) * 100 / currentPrice) > 1) {
                        action = 0.4; // продажа с тп
                    }
                    if (((currentPrice - target) * 100 / currentPrice) > 2) {
                        action = 0.3; // продажа с тп
                    }
                    if (((currentPrice - target) * 100 / currentPrice) > 3) {
                        action = 0.2; // продажа с тп
                    }
                    if (((currentPrice - target) * 100 / currentPrice) > 4) {
                        action = 0.1; // продажа с тп
                    }
                    if (((currentPrice - target) * 100 / currentPrice) > 5) {
                        action = 0.0; // продажа с тп
                    }
                }

                var input = Nd4j.create(new double[]{
                        startPrice,
                        min5, min4, min3, min2, min1,
                        max5, max4, max3, max2, max1,
                        volume5, volume4, volume3, volume2, volume1,
                        currentPrice,
                        supportLevel, resistanceLevel,
                        atr,
                        potentialToSupportLevel, potentialToResistanceLevel
                });
                var label = Nd4j.create(new double[]{action});
                dataSets.add(new DataSet(input, label));
            }
            return new ListDataSetIterator<>(dataSets);
        }
    }
}
