package com.github.shk0da.goldendragon.backtest;

import org.junit.jupiter.api.Test;

import java.io.BufferedReader;
import java.io.File;
import java.io.InputStreamReader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * Runs backtest for all SL/TP algorithms and compares results.
 * Each algorithm is tested with its own configuration.
 */
class StopLossTakeProfitAlgorithmTest {

    private static final String[] ALGORITHMS = {
            "PERCENTAGE", "ATR", "WAVE_ATR", "HYBRID", "VOLATILITY_ADAPTIVE"
    };

    private static final Map<String, String> ALGO_PROPS = Map.of(
            "PERCENTAGE", "unifiedTrader.ticker.T.slMult=1.2\nunifiedTrader.ticker.T.tpMult=2.5\nunifiedTrader.ticker.T.slTpAlgorithm=PERCENTAGE",
            "ATR", "unifiedTrader.ticker.T.slMult=2.0\nunifiedTrader.ticker.T.tpMult=4.0\nunifiedTrader.ticker.T.slTpAlgorithm=ATR",
            "WAVE_ATR", "unifiedTrader.ticker.T.slMult=20\nunifiedTrader.ticker.T.tpMult=2.5\nunifiedTrader.ticker.T.slTpAlgorithm=WAVE_ATR",
            "HYBRID", "unifiedTrader.ticker.T.slMult=2.0\nunifiedTrader.ticker.T.tpMult=2.5\nunifiedTrader.ticker.T.slTpAlgorithm=HYBRID",
            "VOLATILITY_ADAPTIVE", "unifiedTrader.ticker.T.slMult=2.0\nunifiedTrader.ticker.T.tpMult=3.0\nunifiedTrader.ticker.T.slTpAlgorithm=VOLATILITY_ADAPTIVE"
    );

    @Test
    void runAllAlgorithmsComparison() throws Exception {
        String propsFile = "src/main/resources/application.properties";
        Path propsPath = Path.of(propsFile);
        String original = Files.readString(propsPath);

        List<AlgorithmResult> results = new ArrayList<>();

        for (String algo : ALGORITHMS) {
            System.out.println("\n============================================================");
            System.out.println("Testing algorithm: " + algo);
            System.out.println("============================================================");

            // Apply algorithm-specific properties
            String newProps = applyAlgorithmProps(original, algo);
            Files.writeString(propsPath, newProps);

            // Run backtest
            String output = runBacktest();
            String summary = extractSummary(output);
            results.add(new AlgorithmResult(algo, summary));

            System.out.println(summary);

            // Restore original
            Files.writeString(propsPath, original);
            Thread.sleep(2000); // Brief pause between runs
        }

        // Print comparison table
        System.out.println("\n\n============================================================");
        System.out.println("SL/TP ALGORITHM COMPARISON");
        System.out.println("============================================================");
        System.out.printf("%-30s %s%n", "Algorithm", "Result");
        System.out.println("-".repeat(80));
        for (AlgorithmResult r : results) {
            System.out.printf("%-30s %s%n", r.algo, r.summary);
        }
        System.out.println("============================================================");

        // Verify all ran successfully
        for (AlgorithmResult r : results) {
            if (r.summary.contains("BUILD FAILED") || r.summary.contains("ERROR")) {
                throw new RuntimeException("Algorithm " + r.algo + " failed: " + r.summary);
            }
        }
    }

    private String applyAlgorithmProps(String original, String algo) {
        StringBuilder sb = new StringBuilder(original);
        // Add the slTpAlgorithm and tuned slMult/tpMult for ticker T (main ticker)
        sb.append("\n");
        sb.append(ALGO_PROPS.getOrDefault(algo, ""));
        return sb.toString();
    }

    private String runBacktest() throws Exception {
        ProcessBuilder pb = new ProcessBuilder("./gradlew", "runBacktest");
        pb.directory(new File("."));
        pb.redirectErrorStream(true);
        Process proc = pb.start();
        BufferedReader reader = new BufferedReader(new InputStreamReader(proc.getInputStream()));
        StringBuilder output = new StringBuilder();
        String line;
        while ((line = reader.readLine()) != null) {
            output.append(line).append("\n");
        }
        proc.waitFor();
        return output.toString();
    }

    private String extractSummary(String output) {
        return output.lines()
                .filter(l -> l.contains("ПОРТФЕЛЬ") || l.contains("СРАВНИТЕЛЬНАЯ")
                        || l.contains("Total PnL") || l.contains("WinRate")
                        || l.contains("MaxDD") || l.contains("Trades")
                        || l.contains("Score") || l.contains("BUILD")
                        || l.contains("ERROR") || l.contains("FAILURE"))
                .collect(Collectors.joining("\n"));
    }

    static class AlgorithmResult {
        final String algo;
        final String summary;
        AlgorithmResult(String algo, String summary) {
            this.algo = algo;
            this.summary = summary;
        }
    }
}
