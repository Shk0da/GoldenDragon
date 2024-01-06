package com.github.shk0da.GoldenDragon.config;

import com.github.shk0da.GoldenDragon.utils.PropertiesUtils;

import java.util.Properties;

public class PulseConfig {

    private final String followSessionId;
    private final String cookies;
    private final String[] followProfileIds;
    private final int maxPositions;

    public PulseConfig() throws Exception {
        final Properties properties = PropertiesUtils.loadProperties();
        this.followSessionId = properties.getProperty("pulse.follow.sessionId");
        this.cookies = properties.getProperty("pulse.follow.cookies");
        this.followProfileIds = properties.getProperty("pulse.follow.profileIds").split(";");
        this.maxPositions = Integer.parseInt(properties.getProperty("pulse.follow.maxPositions"));
    }

    public String getFollowSessionId() {
        return followSessionId;
    }

    public String getCookies() {
        return cookies;
    }

    public String[] getFollowProfileId() {
        return followProfileIds;
    }

    public int getMaxPositions() {
        return maxPositions;
    }
}
