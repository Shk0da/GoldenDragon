package com.github.shk0da.goldendragon.service;

import static com.github.shk0da.goldendragon.utils.LoggingUtils.log;
import static com.github.shk0da.goldendragon.utils.LoggingUtils.logError;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.TreeMap;
import java.util.zip.GZIPInputStream;

/**
 * Service for downloading and converting historical data from Bybit. Integrates with DataCollector
 * to provide crypto market data.
 *
 * <p>Encapsulates all Bybit-related functionality: - Downloading raw tick/trade data - Converting
 * to candlestick format (5-min, 1-hour) - Managing local data files
 */
public class BybitService {

    private static final String BASE_URL = "https://public.bybit.com/";
    private static final List<String> KNOWN_DATA_TYPES =
            Arrays.asList(
                    "trading", "spot", "kline_for_metatrader4", "premium_index", "spot_index");

    private final String dataDir;

    public BybitService(String dataDir) {
        this.dataDir = dataDir;
    }

    private static final int COIN_DOWNLOAD_THREADS = 10;

    /**
     * Download and convert historical data for specified crypto tokens. Downloads and converts
     * incrementally - converts each file immediately after download. Processes multiple coins in
     * parallel.
     *
     * @param coins List of coin pairs (e.g., BTCUSDT, ETHUSDT)
     * @param startDate Start date (inclusive)
     * @param endDate End date (inclusive), null for today
     * @throws IOException If download or conversion fails
     */
    public void downloadAndConvert(List<String> coins, LocalDate startDate, LocalDate endDate)
            throws IOException {
        if (coins == null || coins.isEmpty()) {
            log("No crypto coins configured for download");
            return;
        }

        log("=== Starting Bybit data download ===");
        log("Coins: " + String.join(", ", coins));
        log("Date range: " + startDate + " to " + (endDate != null ? endDate : "today"));
        log(
                "Processing "
                        + coins.size()
                        + " coins in parallel with "
                        + COIN_DOWNLOAD_THREADS
                        + " threads");

        try {
            java.util.concurrent.ExecutorService executor =
                    java.util.concurrent.Executors.newFixedThreadPool(COIN_DOWNLOAD_THREADS);
            java.util.concurrent.CountDownLatch latch =
                    new java.util.concurrent.CountDownLatch(coins.size());

            for (String coin : coins) {
                log(">>> Submitting task for coin: " + coin);
                executor.submit(
                        () -> {
                            try {
                                log(">>> Task STARTED for coin: " + coin);
                                downloadAndConvertSingleCoin(coin, startDate, endDate);
                                log(">>> Task COMPLETED for coin: " + coin);
                            } catch (Exception e) {
                                logError("Failed to process coin " + coin, e);
                            } finally {
                                latch.countDown();
                            }
                        });
            }

            log("All coin tasks submitted, waiting for completion...");
            latch.await();
            executor.shutdown();
            log("Executor shutdown complete");

            log("=== Bybit data download completed successfully ===");
        } catch (Exception e) {
            logError("Failed to download/convert Bybit data", e);
            throw new IOException("Bybit data download failed", e);
        }
    }

    /** Download and convert a single coin incrementally. */
    private void downloadAndConvertSingleCoin(String coin, LocalDate startDate, LocalDate endDate)
            throws Exception {
        log("Processing coin: " + coin);

        String coinDataDir = Paths.get(dataDir, coin).toString();
        Files.createDirectories(Paths.get(coinDataDir));

        CandleIncrementalConverter converter =
                new CandleIncrementalConverter(coinDataDir, coinDataDir);

        // Check if candle files already exist and have data
        Path candlesHourFile = Paths.get(coinDataDir, "candlesHOUR.txt");
        boolean candlesExist = Files.exists(candlesHourFile) && Files.size(candlesHourFile) > 0;

        if (!candlesExist) {
            // Try to process existing CSV files first instead of re-downloading
            List<Path> existingCsvFiles = findExistingCsvFiles(coinDataDir);
            if (!existingCsvFiles.isEmpty()) {
                log("Found " + existingCsvFiles.size()
                        + " existing CSV files for " + coin + ", processing...");
                for (Path csvFile : existingCsvFiles) {
                    converter.convertSingleFile(csvFile);
                }
            }
        }

        // Download any missing files from Bybit (skips files already on disk)
        BybitDataDownloader downloader =
                new BybitDataDownloader(
                        coinDataDir,
                        coin,
                        startDate,
                        endDate,
                        converter);

        downloader.downloadAll();

        log("Completed processing coin: " + coin);
    }

    /** Find existing CSV files in the coin data directory. */
    private List<Path> findExistingCsvFiles(String dir) throws IOException {
        List<Path> csvFiles = new ArrayList<>();
        try (java.util.stream.Stream<Path> stream = Files.walk(Paths.get(dir))) {
            stream.filter(p -> p.toString().endsWith(".csv")).sorted().forEach(csvFiles::add);
        }
        return csvFiles;
    }

    // ========================================================================
    // INNER CLASS: BybitDataDownloader
    // Encapsulated - not visible outside BybitService
    // ========================================================================

    /** Internal downloader for Bybit historical data. Not exposed outside BybitService. */
    private static class BybitDataDownloader {

        private static final int DOWNLOAD_THREADS = 5;

        private final String outputDir;
        private final String targetCoin;
        private final LocalDate startDate;
        private final LocalDate endDate;
        private final CandleIncrementalConverter incrementalConverter;

        BybitDataDownloader(
                String outputDir,
                String targetCoin,
                LocalDate startDate,
                LocalDate endDate,
                CandleIncrementalConverter incrementalConverter) {
            this.outputDir = outputDir;
            this.targetCoin = targetCoin;
            this.startDate = startDate;
            this.endDate = endDate != null ? endDate : LocalDate.now();
            this.incrementalConverter = incrementalConverter;
        }

        void downloadAll() throws Exception {
            System.out.println("Fetching directory: " + BASE_URL + "trading/" + targetCoin + "/");

            String coinUrl = BASE_URL + "trading/" + targetCoin + "/";
            List<FileToDownload> filesToDownload = collectFilesToDownload(coinUrl);

            System.out.println("Found " + filesToDownload.size() + " files to download");
            System.out.println(
                    "Starting multi-threaded download with " + DOWNLOAD_THREADS + " threads...");

            downloadFilesMultiThreaded(filesToDownload);

            System.out.println("\n=== Download complete for " + targetCoin + " ===");
        }

        /** Collect all files to download by traversing directory structure. */
        private List<FileToDownload> collectFilesToDownload(String dirUrl) throws Exception {
            List<FileToDownload> files = new ArrayList<>();
            collectFilesRecursive(dirUrl, outputDir, files);
            return files;
        }

        private void collectFilesRecursive(
                String dirUrl, String localDir, List<FileToDownload> files) {
            try {
                String html = fetchUrl(dirUrl);
                List<String> links = extractLinks(html);

                for (String link : links) {
                    if (link.toLowerCase().endsWith(".csv.gz")) {
                        LocalDate fileDate = extractDateFromFileName(link);
                        if (fileDate != null
                                && !fileDate.isBefore(startDate)
                                && !fileDate.isAfter(endDate)) {
                            String extractedFileName =
                                    link.endsWith(".gz")
                                            ? link.substring(0, link.length() - 3)
                                            : link;
                            Path extractedPath = Paths.get(localDir, extractedFileName);

                            if (!Files.exists(extractedPath)) {
                                files.add(
                                        new FileToDownload(
                                                dirUrl + link, localDir, link, fileDate));
                            }
                        }
                    }
                }

                for (String link : links) {
                    if (link.endsWith("/") && !link.equals("../")) {
                        String subdirName = link.replace("/", "");
                        String subdirUrl = dirUrl + link;
                        String subdirPath = Paths.get(localDir, subdirName).toString();
                        Files.createDirectories(Paths.get(subdirPath));
                        collectFilesRecursive(subdirUrl, subdirPath, files);
                    }
                }
            } catch (Exception e) {
                System.err.println("Error collecting files from " + dirUrl + ": " + e.getMessage());
            }
        }

        /** Download files using multiple threads. */
        private void downloadFilesMultiThreaded(List<FileToDownload> files) throws Exception {
            java.util.concurrent.ExecutorService executor =
                    java.util.concurrent.Executors.newFixedThreadPool(DOWNLOAD_THREADS);
            java.util.concurrent.CountDownLatch latch =
                    new java.util.concurrent.CountDownLatch(files.size());
            java.util.concurrent.atomic.AtomicInteger downloadedCount =
                    new java.util.concurrent.atomic.AtomicInteger(0);

            for (FileToDownload file : files) {
                executor.submit(
                        () -> {
                            try {
                                System.out.println(
                                        "Downloading ["
                                                + (downloadedCount.get() + 1)
                                                + "/"
                                                + files.size()
                                                + "]: "
                                                + file.fileName
                                                + " (date: "
                                                + file.date
                                                + ")");

                                Path extractedPath =
                                        Paths.get(
                                                file.outputDir,
                                                file.fileName.endsWith(".gz")
                                                        ? file.fileName.substring(
                                                                0, file.fileName.length() - 3)
                                                        : file.fileName);

                                downloadAndExtract(file.url, extractedPath);
                                System.out.println("Successfully downloaded: " + file.fileName);

                                if (incrementalConverter != null) {
                                    incrementalConverter.convertSingleFile(extractedPath);
                                }

                                downloadedCount.incrementAndGet();
                            } catch (Exception e) {
                                System.err.println(
                                        "Failed to download "
                                                + file.fileName
                                                + ": "
                                                + e.getMessage());
                            } finally {
                                latch.countDown();
                            }
                        });
            }

            latch.await();
            executor.shutdown();
        }

        /** Helper class to store file download information. */
        private static class FileToDownload {
            final String url;
            final String outputDir;
            final String fileName;
            final LocalDate date;

            FileToDownload(String url, String outputDir, String fileName, LocalDate date) {
                this.url = url;
                this.outputDir = outputDir;
                this.fileName = fileName;
                this.date = date;
            }
        }

        private void downloadAndExtract(String fileUrl, Path extractedPath) throws Exception {
            Path tempGzPath = Files.createTempFile("bybit_", ".gz");

            try {
                URL url = new URL(fileUrl);
                HttpURLConnection conn = (HttpURLConnection) url.openConnection();
                conn.setRequestMethod("GET");
                conn.setConnectTimeout(60000); // 60 секунд
                conn.setReadTimeout(120000); // 120 секунд

                try (InputStream in = conn.getInputStream();
                        OutputStream out = Files.newOutputStream(tempGzPath)) {

                    byte[] buffer = new byte[8192];
                    int bytesRead;
                    while ((bytesRead = in.read(buffer)) != -1) {
                        out.write(buffer, 0, bytesRead);
                    }
                }

                try (GZIPInputStream gzipIn =
                                new GZIPInputStream(Files.newInputStream(tempGzPath));
                        OutputStream out = Files.newOutputStream(extractedPath)) {

                    byte[] buffer = new byte[8192];
                    int bytesRead;
                    while ((bytesRead = gzipIn.read(buffer)) != -1) {
                        out.write(buffer, 0, bytesRead);
                    }
                }

            } finally {
                Files.deleteIfExists(tempGzPath);
            }
        }

        private LocalDate extractDateFromFileName(String fileName) {
            if (fileName.contains("-") && fileName.contains(".csv")) {
                int dateStart = fileName.indexOf('-', fileName.indexOf('-') + 1);
                if (dateStart > 0) {
                    String datePart = fileName.substring(dateStart + 1, fileName.indexOf(".csv"));
                    try {
                        return LocalDate.parse(datePart + "-01", DateTimeFormatter.ISO_LOCAL_DATE);
                    } catch (Exception e) {
                        // Try without day
                    }
                }
            }

            for (int i = 0; i < fileName.length() - 10; i++) {
                if (Character.isDigit(fileName.charAt(i))) {
                    String dateStr = fileName.substring(i, i + 10);
                    try {
                        return LocalDate.parse(dateStr, DateTimeFormatter.ISO_LOCAL_DATE);
                    } catch (Exception e) {
                        // Continue searching
                    }
                }
            }
            return null;
        }

        private String fetchUrl(String urlString) throws Exception {
            URL url = new URL(urlString);
            HttpURLConnection conn = (HttpURLConnection) url.openConnection();
            conn.setRequestMethod("GET");
            conn.setConnectTimeout(30000);

            StringBuilder sb = new StringBuilder();
            try (BufferedReader reader =
                    new BufferedReader(new InputStreamReader(conn.getInputStream()))) {
                String line;
                while ((line = reader.readLine()) != null) {
                    sb.append(line).append("\n");
                }
            }
            return sb.toString();
        }

        private List<String> extractLinks(String html) {
            List<String> links = new ArrayList<>();
            int pos = 0;
            while ((pos = html.indexOf("<a href=\"", pos)) != -1) {
                int start = pos + 9;
                int end = html.indexOf("\"", start);
                if (end != -1) {
                    links.add(html.substring(start, end));
                    pos = end;
                }
            }
            return links;
        }
    }

    // ========================================================================
    // INNER CLASS: CandleIncrementalConverter
    // For incremental conversion of single files
    // ========================================================================

    /**
     * Incremental converter for converting single CSV files to candles. Accumulates ticks and
     * updates candle files incrementally.
     */
    private static class CandleIncrementalConverter {

        private static final DateTimeFormatter OUTPUT_DATE_FORMAT =
                DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm:ss");
        private final Path dataDir;
        private final List<Tick> allTicks = Collections.synchronizedList(new ArrayList<>());
        private boolean initialized = false;

        CandleIncrementalConverter(String dataDir, String outputDir) {
            this.dataDir = Paths.get(dataDir);
        }

        synchronized void convertSingleFile(Path csvFile) {
            try {
                List<Tick> ticks = readTickFile(csvFile);
                allTicks.addAll(ticks);

                // Sort all ticks by timestamp
                allTicks.sort(Comparator.comparingLong(t -> t.timestamp));

                // Convert to candles and write
                if (!allTicks.isEmpty()) {
                    // Write 5-minute candles
                    List<Candle> candles5Min = aggregateCandles(allTicks, 5 * 60 * 1000);
                    writeCandlesFile(dataDir.resolve("candles5_MIN.txt"), candles5Min, true);

                    // Write 1-hour candles
                    List<Candle> candles1H = aggregateCandles(allTicks, 60 * 60 * 1000);
                    writeCandlesFile(dataDir.resolve("candlesHOUR.txt"), candles1H, true);

                    // Write ticker.json only once
                    if (!initialized) {
                        writeTickerJson(
                                dataDir.resolve("ticker.json"), dataDir.getFileName().toString());
                        initialized = true;
                    }

                    System.out.println(
                            "Converted "
                                    + csvFile.getFileName()
                                    + " - Total ticks: "
                                    + allTicks.size()
                                    + ", 5-min candles: "
                                    + candles5Min.size()
                                    + ", 1-hour candles: "
                                    + candles1H.size());
                }
            } catch (Exception e) {
                System.err.println("Error converting file " + csvFile + ": " + e.getMessage());
            }
        }

        private List<Tick> readTickFile(Path csvFile) throws IOException {
            List<Tick> ticks = new ArrayList<>();
            try (BufferedReader reader = Files.newBufferedReader(csvFile)) {
                String line = reader.readLine(); // Skip header
                if (line == null) return ticks;

                while ((line = reader.readLine()) != null) {
                    String[] parts = line.split(",");
                    if (parts.length >= 5) {
                        try {
                            long timestamp = (long) (Double.parseDouble(parts[0]) * 1000);
                            double price = Double.parseDouble(parts[4]);
                            double volume = Double.parseDouble(parts[3]);
                            ticks.add(new Tick(timestamp, price, volume));
                        } catch (NumberFormatException e) {
                            // Skip malformed lines
                        }
                    }
                }
            }
            return ticks;
        }

        private List<Candle> aggregateCandles(List<Tick> ticks, long intervalMs) {
            Map<Long, List<Tick>> intervals = new TreeMap<>();
            for (Tick tick : ticks) {
                long intervalKey = (tick.timestamp / intervalMs) * intervalMs;
                intervals.computeIfAbsent(intervalKey, k -> new ArrayList<>()).add(tick);
            }

            List<Candle> candles = new ArrayList<>();
            for (Map.Entry<Long, List<Tick>> entry : intervals.entrySet()) {
                List<Tick> intervalTicks = entry.getValue();
                if (intervalTicks.isEmpty()) continue;

                double open = intervalTicks.get(0).price;
                double high = intervalTicks.stream().mapToDouble(t -> t.price).max().orElse(open);
                double low = intervalTicks.stream().mapToDouble(t -> t.price).min().orElse(open);
                double close = intervalTicks.get(intervalTicks.size() - 1).price;
                long volume = (long) intervalTicks.stream().mapToDouble(t -> t.volume).sum();

                candles.add(new Candle(entry.getKey(), open, high, low, close, volume));
            }

            return candles;
        }

        private void writeCandlesFile(Path outputFile, List<Candle> candles, boolean writeHeader)
                throws IOException {
            try (BufferedWriter writer = Files.newBufferedWriter(outputFile)) {
                if (writeHeader) {
                    writer.write("Datetime,Open,High,Low,Close,Volume");
                    writer.newLine();
                }

                for (Candle candle : candles) {
                    LocalDateTime dateTime =
                            LocalDateTime.ofEpochSecond(
                                    candle.timestamp / 1000, 0, java.time.ZoneOffset.UTC);
                    String dateTimeStr = dateTime.format(OUTPUT_DATE_FORMAT);

                    writer.write(
                            String.format(
                                    Locale.US,
                                    "%s,%.2f,%.2f,%.2f,%.2f,%d",
                                    dateTimeStr,
                                    candle.open,
                                    candle.high,
                                    candle.low,
                                    candle.close,
                                    candle.volume));
                    writer.newLine();
                }
            }
        }

        private void writeTickerJson(Path outputFile, String tickerName) throws IOException {
            // Extract ticker name from directory name (e.g., "ETHUSDT" from "/data/ETHUSDT")
            String name =
                    tickerName != null && !tickerName.isEmpty()
                            ? tickerName
                            : dataDir.getFileName().toString();
            String displayName = name.replace("USDT", "");

            String json =
                    String.format(
                            "{\n"
                                    + "  \"ticker\": {\n"
                                    + "    \"figi\": \"%s\",\n"
                                    + "    \"ticker\": \"%s\",\n"
                                    + "    \"isin\": \"\",\n"
                                    + "    \"minPriceIncrement\": 0.01,\n"
                                    + "    \"lot\": 1,\n"
                                    + "    \"currency\": \"USDT\",\n"
                                    + "    \"name\": \"%s\",\n"
                                    + "    \"type\": \"CRYPTO\"\n"
                                    + "  },\n"
                                    + "  \"levels\": []\n"
                                    + "}",
                            name, name, displayName);
            Files.writeString(outputFile, json);
        }

        // Inner classes for Tick and Candle
        private static class Tick {
            final long timestamp;
            final double price;
            final double volume;

            Tick(long timestamp, double price, double volume) {
                this.timestamp = timestamp;
                this.price = price;
                this.volume = volume;
            }
        }

        private static class Candle {
            final long timestamp;
            final double open;
            final double high;
            final double low;
            final double close;
            final long volume;

            Candle(
                    long timestamp,
                    double open,
                    double high,
                    double low,
                    double close,
                    long volume) {
                this.timestamp = timestamp;
                this.open = open;
                this.high = high;
                this.low = low;
                this.close = close;
                this.volume = volume;
            }
        }
    }
}
