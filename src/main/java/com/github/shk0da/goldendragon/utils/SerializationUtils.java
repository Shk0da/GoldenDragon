package com.github.shk0da.goldendragon.utils;

import com.github.shk0da.goldendragon.model.TickerInfo;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.google.gson.JsonPrimitive;
import com.google.gson.JsonSerializer;
import com.google.gson.reflect.TypeToken;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.attribute.BasicFileAttributes;
import java.time.Instant;
import java.util.Date;

/**
 * Utility class for JSON serialization/deserialization using Gson. Provides methods to load and
 * save data to disk with custom type adapters.
 */
public final class SerializationUtils {

    private static final GsonBuilder gsonBuilder = new GsonBuilder();
    private static volatile Gson gsonInstance;

    static {
        var tickerInfoToken = new TypeToken<TickerInfo.Key>() {};
        gsonBuilder.registerTypeAdapter(
                tickerInfoToken.getType(),
                (JsonDeserializer<TickerInfo.Key>)
                        (jsonElement, type, jsonDeserializationContext) ->
                                new TickerInfo.Key(jsonElement.getAsString()));

        // Support for java.time.Instant (used in TickerInfo.expirationDate)
        gsonBuilder.registerTypeAdapter(
                Instant.class,
                (JsonSerializer<Instant>)
                        (src, typeOfSrc, context) -> new JsonPrimitive(src.toString()));
        gsonBuilder.registerTypeAdapter(
                Instant.class,
                (JsonDeserializer<Instant>)
                        (json, typeOfT, context) -> {
                            try {
                                return Instant.parse(json.getAsString());
                            } catch (Exception e) {
                                return null;
                            }
                        });
    }

    /**
     * Returns a thread-safe, pre-configured Gson instance.
     * Creates on first call, uses double-checked locking for thread safety.
     */
    private static Gson getGsonInstance() {
        if (gsonInstance == null) {
            synchronized (SerializationUtils.class) {
                if (gsonInstance == null) {
                    gsonInstance = gsonBuilder.create();
                }
            }
        }
        return gsonInstance;
    }

    public static Date getDateOfContentOnDisk(String name) throws Exception {
        File content = new File(name);
        if (!content.exists()) {
            return new Date(0);
        }

        BasicFileAttributes attrs =
                Files.readAttributes(content.toPath(), BasicFileAttributes.class);
        return new Date(
                Math.max(attrs.creationTime().toMillis(), attrs.lastModifiedTime().toMillis()));
    }

    public static synchronized <T> T loadDataFromDisk(String name, TypeToken<T> typeToken) {
        File content = new File(name);
        if (!content.exists()) {
            return null;
        }

        try {
            JsonObject jsonObject =
                    JsonParser.parseString(Files.readString(content.toPath())).getAsJsonObject();
            return getGsonInstance().fromJson(jsonObject, typeToken.getType());
        } catch (Exception ex) {
            throw new RuntimeException(ex);
        }
    }

    public static <T> void saveDataToDisk(String name, T data) throws IOException {
        String content = getGsonInstance().toJson(data);
        File toSave = new File(name);
        if (!toSave.exists() || (toSave.exists() && toSave.delete())) {
            if (toSave.createNewFile()) {
                Files.writeString(toSave.toPath(), content);
            }
        }
    }
}
