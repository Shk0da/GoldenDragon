/**
 * Machine learning and data collection for GoldenDragon application.
 *
 * <h2>Package Purpose</h2>
 *
 * <p>The {@code ml} package contains components for collecting labeled data, training
 * classification models (XGBoost), predicting market regimes and trading signals. ML models are
 * used in {@code RegimeAwareStrategyMl} for market adaptation.
 *
 * <h2>Data Collection</h2>
 *
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.ml.TradeDataCollector} — collect labeled trade data
 *       for training. Saves to CSV:
 *       <ul>
 *         <li>Entry/exit time, ticker, strategy.
 *         <li>Entry/exit price, PnL, commission.
 *         <li>Indicators at entry (RSI, MACD, ATR, ADX).
 *         <li>Market regime (trend/range).
 *         <li>Label: profitable/unprofitable trade.
 *       </ul>
 *       <br>
 *       <b>Format</b>: {@code trades.csv} for XGBoost training.
 *   <li>{@link com.github.shk0da.GoldenDragon.ml.TradeFeatures} — ML model features: technical
 *       indicators, market regime, volatility, volume. Used as input vector for prediction.
 * </ul>
 *
 * <h2>Model Training</h2>
 *
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.ml.MlModelTrainer} — train ML model (XGBoost) on
 *       historical data. Supports:
 *       <ul>
 *         <li>Training on all data (general model).
 *         <li>Per-ticker training (personal model for each instrument).
 *         <li>Cross-validation, hyperparameter tuning.
 *         <li>Model save to text format ({@code .txt}).
 *       </ul>
 *       <br>
 *       <b>Run</b>: {@code ./gradlew runMlTraining} or {@code ./gradlew trainAllTickers}.
 *   <li>{@link com.github.shk0da.GoldenDragon.ml.MlAutoTrainingService} — automatic model
 *       retraining. Checks accumulated data, triggers training when minimum new trades reached.
 *       Used in {@code RegimeAwareStrategyMl}.
 * </ul>
 *
 * <h2>Prediction</h2>
 *
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.ml.MlPredictionService} — prediction service based on
 *       trained model. Provides:
 *       <ul>
 *         <li>{@code predictProbability(features)} — profitable trade probability.
 *         <li>{@code shouldTakeTrade(features)} — binary decision (take trade or not).
 *         <li>{@code getPositionSizeMultiplier(features)} — position size multiplier (increase on
 *             high confidence, decrease on low).
 *         <li>{@code getStopLossMultiplier(features)} — stop-loss adjustment.
 *       </ul>
 *       <br>
 *       <b>Models</b>: supports general model and per-ticker models.
 * </ul>
 *
 * <h2>Strategy Integration</h2>
 *
 * <p>ML components are used in {@code RegimeAwareStrategyMl}:
 *
 * <ol>
 *   <li>Before entry: create {@code TradeFeatures} with current indicators.
 *   <li>Prediction: {@code MlPredictionService.shouldTakeTrade(features)}.
 *   <li>Position size: {@code getPositionSizeMultiplier(features)} * base size.
 *   <li>After exit: {@code TradeDataCollector.registerTrade()} for data collection.
 *   <li>Retraining: {@code MlAutoTrainingService.tryRetrain()} periodically.
 * </ol>
 *
 * <h2>Gradle Commands</h2>
 *
 * <ul>
 *   <li>{@code ./gradlew runMlTraining} — train default model.
 *   <li>{@code ./gradlew runMlTraining -Pdata=... -Poutput=...} — custom paths.
 *   <li>{@code ./gradlew runMlTraining -Pticker=SBER} — model for specific ticker.
 *   <li>{@code ./gradlew trainAllTickers} — models for all tickers.
 *   <li>{@code ./gradlew generateModel} — full pipeline (backtest + training).
 * </ul>
 *
 * <h2>Thread Safety</h2>
 *
 * <p>{@code TradeDataCollector} and {@code MlPredictionService} are thread-safe (synchronized
 * methods, concurrent collections). {@code MlModelTrainer} runs in separate thread during
 * retraining.
 *
 * @see com.github.shk0da.GoldenDragon.strategy.RegimeAwareStrategyMl
 * @see com.github.shk0da.GoldenDragon.strategy.BaseStrategy
 */
package com.github.shk0da.GoldenDragon.ml;
