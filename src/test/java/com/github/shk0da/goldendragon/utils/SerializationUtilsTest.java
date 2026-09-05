package com.github.shk0da.goldendragon.utils;

import com.github.shk0da.goldendragon.model.TickerInfo;
import com.github.shk0da.goldendragon.model.TickerType;
import com.google.gson.reflect.TypeToken;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import static org.assertj.core.api.BDDAssertions.then;

@DisplayName("SerializationUtils")
class SerializationUtilsTest {

    @TempDir
    Path tempDir;

    @Nested
    @DisplayName("When saving and loading data")
    class SaveAndLoad {

        @Test
        @DisplayName("Should save and load simple POJO")
        void shouldSaveAndLoadSimplePojo() throws IOException {
            // Given
            Path tempFile = tempDir.resolve("test.json");
            Map<String, String> testData = new HashMap<>();
            testData.put("key1", "value1");
            testData.put("key2", "value2");

            // When
            SerializationUtils.saveDataToDisk(tempFile.toString(), testData);
            @SuppressWarnings("unchecked")
            Map<String, String> loaded = SerializationUtils.loadDataFromDisk(
                    tempFile.toString(),
                    new TypeToken<Map<String, String>>() {});

            // Then
            then(loaded).isEqualTo(testData);
        }

        @Test
        @DisplayName("Should save and load TickerInfo with Instant")
        void shouldSaveAndLoadTickerInfoWithInstant() throws IOException {
            // Given
            Path tempFile = tempDir.resolve("ticker.json");
            TickerInfo info = new TickerInfo(
                    "FIGI123",
                    "TEST",
                    "ISIN123",
                    0.01,
                    1,
                    "RUB",
                    "Test Instrument",
                    "STOCK"
            );

            // When
            SerializationUtils.saveDataToDisk(tempFile.toString(), info);
            TickerInfo loaded = SerializationUtils.loadDataFromDisk(
                    tempFile.toString(),
                    new TypeToken<TickerInfo>() {});

            // Then
            then(loaded).isNotNull();
            then(loaded.getName()).isEqualTo("Test Instrument");
            then(loaded.getTicker()).isEqualTo("TEST");
            then(loaded.getFigi()).isEqualTo("FIGI123");
            then(loaded.getType()).isEqualTo(TickerType.STOCK);
        }

        @Test
        @DisplayName("Should save and load map of TickerInfo")
        void shouldSaveAndLoadMapOfTickerInfo() throws IOException {
            // Given
            Path tempFile = tempDir.resolve("tickers.json");
            Map<TickerInfo.Key, TickerInfo> testData = new HashMap<>();
            testData.put(new TickerInfo.Key("STOCK/AAPL"),
                    new TickerInfo("FIGI_AAPL", "AAPL", "US0378331005", 0.01, 1, "USD", "Apple Inc", "STOCK"));
            testData.put(new TickerInfo.Key("STOCK/GOOGL"),
                    new TickerInfo("FIGI_GOOGL", "GOOGL", "US02079K3059", 0.01, 1, "USD", "Alphabet Inc", "STOCK"));

            // When
            SerializationUtils.saveDataToDisk(tempFile.toString(), testData);
            @SuppressWarnings("unchecked")
            Map<TickerInfo.Key, TickerInfo> loaded = SerializationUtils.loadDataFromDisk(
                    tempFile.toString(),
                    new TypeToken<Map<TickerInfo.Key, TickerInfo>>() {});

            // Then
            then(loaded).hasSize(2);
            then(loaded.get(new TickerInfo.Key("STOCK/AAPL")).getName())
                    .isEqualTo("Apple Inc");
            then(loaded.get(new TickerInfo.Key("STOCK/GOOGL")).getName())
                    .isEqualTo("Alphabet Inc");
        }

        @Test
        @DisplayName("Should return null when file does not exist")
        void shouldReturnNull_WhenFileDoesNotExist() {
            // Given
            Path nonExistentFile = tempDir.resolve("nonexistent.json");

            // When
            Map<String, String> loaded = SerializationUtils.loadDataFromDisk(
                    nonExistentFile.toString(),
                    new TypeToken<Map<String, String>>() {});

            // Then
            then(loaded).isNull();
        }

        @Test
        @DisplayName("Should overwrite existing file")
        void shouldOverwriteExistingFile() throws IOException {
            // Given
            Path tempFile = tempDir.resolve("overwrite.json");
            Map<String, String> firstData = new HashMap<>();
            firstData.put("key", "first");
            SerializationUtils.saveDataToDisk(tempFile.toString(), firstData);

            Map<String, String> secondData = new HashMap<>();
            secondData.put("key", "second");

            // When
            SerializationUtils.saveDataToDisk(tempFile.toString(), secondData);
            @SuppressWarnings("unchecked")
            Map<String, String> loaded = SerializationUtils.loadDataFromDisk(
                    tempFile.toString(),
                    new TypeToken<Map<String, String>>() {});

            // Then
            then(loaded.get("key")).isEqualTo("second");
        }
    }

    @Nested
    @DisplayName("When getting file date")
    class GetFileDate {

        @Test
        @DisplayName("Should return file creation date")
        void shouldReturnFileCreationDate() throws Exception {
            // Given
            Path tempFile = tempDir.resolve("dated.json");
            Files.writeString(tempFile, "test content");

            // When
            Date fileDate = SerializationUtils.getDateOfContentOnDisk(tempFile.toString());

            // Then
            then(fileDate).isNotNull();
            then(fileDate.getTime()).isGreaterThan(0);
            // Should be within last second
            then(System.currentTimeMillis() - fileDate.getTime()).isLessThan(1000);
        }

        @Test
        @DisplayName("Should return epoch date when file does not exist")
        void shouldReturnEpochDate_WhenFileDoesNotExist() throws Exception {
            // Given
            Path nonExistentFile = tempDir.resolve("nonexistent.json");

            // When
            Date fileDate = SerializationUtils.getDateOfContentOnDisk(nonExistentFile.toString());

            // Then
            then(fileDate).isEqualTo(new Date(0));
        }

        @Test
        @DisplayName("Should return max of creation and modification time")
        void shouldReturnMaxOfCreationAndModification() throws Exception {
            // Given
            Path tempFile = tempDir.resolve("modified.json");
            Files.writeString(tempFile, "initial content");
            Date initialDate = SerializationUtils.getDateOfContentOnDisk(tempFile.toString());

            // Wait a bit and modify
            Thread.sleep(10);
            Files.writeString(tempFile, "modified content");

            // When
            Date modifiedDate = SerializationUtils.getDateOfContentOnDisk(tempFile.toString());

            // Then
            then(modifiedDate.getTime()).isGreaterThanOrEqualTo(initialDate.getTime());
        }
    }

    @Nested
    @DisplayName("When handling edge cases")
    class EdgeCases {

        @Test
        @DisplayName("Should handle empty map")
        void shouldHandleEmptyMap() throws IOException {
            // Given
            Path tempFile = tempDir.resolve("empty.json");
            Map<String, String> emptyMap = new HashMap<>();

            // When
            SerializationUtils.saveDataToDisk(tempFile.toString(), emptyMap);
            @SuppressWarnings("unchecked")
            Map<String, String> loaded = SerializationUtils.loadDataFromDisk(
                    tempFile.toString(),
                    new TypeToken<Map<String, String>>() {});

            // Then
            then(loaded).isEmpty();
        }

        @Test
        @DisplayName("Should handle null values in map")
        void shouldHandleNullValues() throws IOException {
            // Given
            Path tempFile = tempDir.resolve("nulls.json");
            Map<String, String> mapWithNulls = new HashMap<>();
            mapWithNulls.put("key1", "value1");
            mapWithNulls.put("key2", null);

            // When
            SerializationUtils.saveDataToDisk(tempFile.toString(), mapWithNulls);
            @SuppressWarnings("unchecked")
            Map<String, String> loaded = SerializationUtils.loadDataFromDisk(
                    tempFile.toString(),
                    new TypeToken<Map<String, String>>() {});

            // Then
            then(loaded.get("key1")).isEqualTo("value1");
            then(loaded.get("key2")).isNull();
        }

        @Test
        @DisplayName("Should handle special characters in strings")
        void shouldHandleSpecialCharacters() throws IOException {
            // Given
            Path tempFile = tempDir.resolve("special.json");
            Map<String, String> specialData = new HashMap<>();
            specialData.put("unicode", "Привет мир");
            specialData.put("symbols", "!@#$%^&*()");
            specialData.put("newline", "line1\nline2");

            // When
            SerializationUtils.saveDataToDisk(tempFile.toString(), specialData);
            @SuppressWarnings("unchecked")
            Map<String, String> loaded = SerializationUtils.loadDataFromDisk(
                    tempFile.toString(),
                    new TypeToken<Map<String, String>>() {});

            // Then
            then(loaded.get("unicode")).isEqualTo("Привет мир");
            then(loaded.get("symbols")).isEqualTo("!@#$%^&*()");
            then(loaded.get("newline")).isEqualTo("line1\nline2");
        }
    }
}
