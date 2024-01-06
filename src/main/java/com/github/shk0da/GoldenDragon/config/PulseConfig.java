package com.github.shk0da.GoldenDragon.config;

import com.github.shk0da.GoldenDragon.utils.PropertiesUtils;

import java.util.Properties;

public class PulseConfig {

    private final String followSessionId;
    private final String followProfileId;
    private final int maxPositions;

    public PulseConfig() throws Exception {
        final Properties properties = PropertiesUtils.loadProperties();
        this.followSessionId = properties.getProperty("pulse.follow.sessionId");
        this.followProfileId = properties.getProperty("pulse.follow.profileId");
        this.maxPositions = Integer.parseInt(properties.getProperty("pulse.follow.maxPositions"));
    }

    public String getFollowSessionId() {
        return followSessionId;
    }

    public String getFollowProfileId() {
        return followProfileId;
    }

    public int getMaxPositions() {
        return maxPositions;
    }
}
