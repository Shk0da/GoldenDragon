package com.github.shk0da.GoldenDragon.utils;

import com.github.shk0da.GoldenDragon.model.TickerCandle;
import com.github.shk0da.GoldenDragon.model.TickerJson;
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
import org.nd4j.linalg.dataset.api.DataSetPreProcessor;
import org.nd4j.linalg.dataset.api.iterator.DataSetIterator;
import org.nd4j.linalg.factory.Nd4j;
import org.nd4j.linalg.lossfunctions.LossFunctions;

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

    public static final class StockDataSetIterator implements DataSetIterator {

        private final TickerJson ticker;
        private final List<TickerCandle> stockDataList;

        public StockDataSetIterator(TickerJson ticker, List<TickerCandle> stockDataList) {
            this.ticker = ticker;
            this.stockDataList = stockDataList;

            List<DataSet> dataSets = new ArrayList<>();
            var currentPrice = 0.0;  // тек.цена
            var supportLevel = 0.0; // уровень снизу
            var resistanceLevel = 0.0; // уровень сверху
            var atr = 0.0; // ATR
            var startPrice = 0.0; // цена начала дня
            var potentialToSupportLevel = 0.0; // потенциал до уровня снизу
            var potentialToResistanceLevel = 0.0; // потенциал до уровня сверху

            var volume5 = 0.0; // объем 5 свечей назад (25 мин)
            var volume4 = 0.0; // объем 4 свечей назад (20 мин)
            var volume3 = 0.0; // объем 3 свечей назад (15 мин)
            var volume2 = 0.0; // объем 2 свечей назад (10 мин)
            var volume1 = 0.0; // объем 1 свечей назад (5 мин)
            var volume0 = 0.0; // объем 0 свечей назад (0 мин)

            var candle5 = 0.0; // свеча 5 назад

            // Тест: пробой, отскок, ложный пробой

        }

        @Override
        public DataSet next(int num) {
            var input = Nd4j.create(new int[]{1,2,3,4});
            var label = Nd4j.create(new int[]{5}, 'f');
            return new DataSet(input, label);
        }

        @Override
        public int totalExamples() {
            return 0;
        }

        @Override
        public int inputColumns() {
            return 0;
        }

        @Override
        public int totalOutcomes() {
            return 0;
        }

        @Override
        public boolean resetSupported() {
            return false;
        }

        @Override
        public boolean asyncSupported() {
            return false;
        }

        @Override
        public void reset() {

        }

        @Override
        public int batch() {
            return 0;
        }

        @Override
        public int cursor() {
            return 0;
        }

        @Override
        public int numExamples() {
            return 0;
        }

        @Override
        public void setPreProcessor(DataSetPreProcessor preProcessor) {

        }

        @Override
        public DataSetPreProcessor getPreProcessor() {
            return null;
        }

        @Override
        public List<String> getLabels() {
            return null;
        }

        @Override
        public boolean hasNext() {
            return false;
        }

        @Override
        public DataSet next() {
            return null;
        }
    }
}
