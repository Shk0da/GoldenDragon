package com.github.shk0da.goldendragon.strategy;

import static java.lang.System.out;

import com.github.shk0da.goldendragon.ml.MlModelTrainer;
import com.github.shk0da.goldendragon.test.BacktestRunner;

public class ModelGenerator {

    public void runGenerateModel(String[] args) throws Exception {
        out.println("============================================================");
        out.println("GOLDENDRAGON GENERATE MODEL");
        out.println("============================================================");

        String dataPath = "ml_strategy/data_pipeline/trades.csv";
        String outputPath = "ml_strategy/models/trade_classifier_v2.txt";
        String reportDir = "ml_strategy";
        boolean skipBacktest = false;

        for (int i = 1; i < args.length; i++) {
            if ("--data".equals(args[i]) && i + 1 < args.length) {
                dataPath = args[++i];
            } else if ("--output".equals(args[i]) && i + 1 < args.length) {
                outputPath = args[++i];
            } else if ("--report-dir".equals(args[i]) && i + 1 < args.length) {
                reportDir = args[++i];
            } else if ("--skip-backtest".equals(args[i])) {
                skipBacktest = true;
            }
        }

        if (!skipBacktest) {
            out.println("Step 1: Running backtest...");
            try {
                BacktestRunner.main(new String[] {});
                out.println("Backtest completed successfully.");
            } catch (Exception ex) {
                out.println("Backtest completed with warnings: " + ex.getMessage());
            }
        } else {
            out.println("Step 1: Skipping backtest (--skip-backtest specified)");
        }

        out.println("Step 2: Training ML model...");
        MlModelTrainer.main(
                new String[] {
                    "--data", dataPath, "--output", outputPath, "--report-dir", reportDir
                });

        out.println("============================================================");
        out.println("Model generation completed.");
        out.println("Model: " + outputPath);
        out.println("Report: " + reportDir + "/reports/training_report.json");
        out.println("============================================================");
    }
}
