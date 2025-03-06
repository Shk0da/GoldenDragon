package com.github.shk0da.GoldenDragon.utils;

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
import org.nd4j.linalg.dataset.api.iterator.DataSetIterator;
import org.nd4j.linalg.lossfunctions.LossFunctions;

public class DeepLearningUtils {

    private final class LSTMNetwork {

        private final Double learningRate = 0.05;
        private final Integer iterations = 1;
        private final Integer seed = 777;
        private final Integer score = 100;

        private final Integer lstmLayer1Size = 256;
        private final Integer lstmLayer2Size = 256;
        private final Integer denseLayerSize = 32;
        private final Double dropoutRatio = 0.2;
        private final Integer truncatedBPTTLength = 22;

        public MultiLayerNetwork buildLstmNetworks(DataSetIterator iterator) {
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
}
