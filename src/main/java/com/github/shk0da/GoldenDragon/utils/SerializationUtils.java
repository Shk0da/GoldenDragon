package com.github.shk0da.GoldenDragon.utils;

import com.github.shk0da.GoldenDragon.model.TickerInfo;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.google.gson.reflect.TypeToken;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.Date;

public final class SerializationUtils {

    private static final GsonBuilder gsonBuilder = new GsonBuilder();

    static {
        var tickerInfoToken = new TypeToken<TickerInfo.Key>() {};
        gsonBuilder.registerTypeAdapter(
                tickerInfoToken.getType(),
                (JsonDeserializer<TickerInfo.Key>) (jsonElement, type, jsonDeserializationContext) -> new TickerInfo.Key(jsonElement.getAsString())
        );
    }

    public static Date getDateOfContentOnDisk(String name) throws Exception {
        File content = new File(name);
        if (!content.exists()) {
            return new Date(0);
        }

        BasicFileAttributes attrs = Files.readAttributes(content.toPath(), BasicFileAttributes.class);
        return new Date(Math.max(attrs.creationTime().toMillis(), attrs.lastModifiedTime().toMillis()));
    }

    public static <T> T loadDataFromDisk(String name, TypeToken<T> typeToken) throws Exception {
        File content = new File(name);
        if (!content.exists()) {
            return null;
        }

        JsonObject jsonObject = JsonParser.parseString(Files.readString(content.toPath())).getAsJsonObject();
        return gsonBuilder.create().fromJson(jsonObject, typeToken.getType());
    }

    public static <T> void saveDataToDisk(String name, T data) throws IOException {
        String content = new GsonBuilder().create().toJson(data);
        File toSave = new File(name);
        if (!toSave.exists() || (toSave.exists() && toSave.delete())) {
            if (toSave.createNewFile()) {
                Files.writeString(toSave.toPath(), content);
            }
        }
    }
}
