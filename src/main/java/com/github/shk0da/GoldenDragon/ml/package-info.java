/**
 * Machine learning and data collection for GoldenDragon application.
 *
 * <h2>Package Purpose</h2>
 * <p>The {@code ml} package contains components for collecting labeled data, training
 * classification models (XGBoost), predicting market regimes and trading signals.
 * ML models are used in {@code RegimeAwareStrategyMl} for market adaptation.</p>
 *
 * <h2>Data Collection</h2>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.ml.TradeDataCollector} — collect labeled trade
 *       data for training. Saves to CSV:
 *       <ul>
 *         <li>Entry/exit time, ticker, strategy.</li>
 *         <li>Entry/exit price, PnL, commission.</li>
 *         <li>Indicators at entry (RSI, MACD, ATR, ADX).</li>
 *         <li>Market regime (trend/range).</li>
 *         <li>Label: profitable/unprofitable trade.</li>
 *       </ul>
 *       <br><b>Format</b>: {@code trades.csv} for XGBoost training.</li>
 *
 *   <li>{@link com.github.shk0da.GoldenDragon.ml.TradeFeatures} — ML model features:
 *       technical indicators, market regime, volatility, volume.
 *       Used as input vector for prediction.</li>
 * </ul>
 *
 * <h2>Model Training</h2>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.ml.MlModelTrainer} — train ML model (XGBoost)
 *       on historical data. Supports:
 *       <ul>
 *         <li>Training on all data (general model).</li>
 *         <li>Per-ticker training (personal model for each instrument).</li>
 *         <li>Cross-validation, hyperparameter tuning.</li>
 *         <li>Model save to text format ({@code .txt}).</li>
 *       </ul>
 *       <br><b>Run</b>: {@code ./gradlew runMlTraining} or {@code ./gradlew trainAllTickers}.</li>
 *
 *   <li>{@link com.github.shk0da.GoldenDragon.ml.MlAutoTrainingService} — automatic
 *       model retraining. Checks accumulated data, triggers training when minimum
 *       new trades reached. Used in {@code RegimeAwareStrategyMl}.</li>
 * </ul>
 *
 * <h2>Prediction</h2>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.ml.MlPredictionService} — prediction service
 *       based on trained model. Provides:
 *       <ul>
 *         <li>{@code predictProbability(features)} — profitable trade probability.</li>
 *         <li>{@code shouldTakeTrade(features)} — binary decision (take trade or not).</li>
 *         <li>{@code getPositionSizeMultiplier(features)} — position size multiplier
 *             (increase on high confidence, decrease on low).</li>
 *         <li>{@code getStopLossMultiplier(features)} — stop-loss adjustment.</li>
 *       </ul>
 *       <br><b>Models</b>: supports general model and per-ticker models.</li>
 * </ul>
 *
 * <h2>Strategy Integration</h2>
 * <p>ML components are used in {@code RegimeAwareStrategyMl}:</p>
 * <ol>
 *   <li>Before entry: create {@code TradeFeatures} with current indicators.</li>
 *   <li>Prediction: {@code MlPredictionService.shouldTakeTrade(features)}.</li>
 *   <li>Position size: {@code getPositionSizeMultiplier(features)} * base size.</li>
 *   <li>After exit: {@code TradeDataCollector.registerTrade()} for data collection.</li>
 *   <li>Retraining: {@code MlAutoTrainingService.tryRetrain()} periodically.</li>
 * </ol>
 *
 * <h2>Gradle Commands</h2>
 * <ul>
 *   <li>{@code ./gradlew runMlTraining} — train default model.</li>
 *   <li>{@code ./gradlew runMlTraining -Pdata=... -Poutput=...} — custom paths.</li>
 *   <li>{@code ./gradlew runMlTraining -Pticker=SBER} — model for specific ticker.</li>
 *   <li>{@code ./gradlew trainAllTickers} — models for all tickers.</li>
 *   <li>{@code ./gradlew generateModel} — full pipeline (backtest + training).</li>
 * </ul>
 *
 * <h2>Thread Safety</h2>
 * <p>{@code TradeDataCollector} and {@code MlPredictionService} are thread-safe
 * (synchronized methods, concurrent collections).
 * {@code MlModelTrainer} runs in separate thread during retraining.</p>
 *
 * @see com.github.shk0da.GoldenDragon.strategy.RegimeAwareStrategyMl
 * @see com.github.shk0da.GoldenDragon.strategy.BaseStrategy
 */
package com.github.shk0da.GoldenDragon.ml;
