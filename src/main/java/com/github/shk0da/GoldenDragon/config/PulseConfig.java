package com.github.shk0da.GoldenDragon.config;

import com.github.shk0da.GoldenDragon.utils.PropertiesUtils;

import java.util.Properties;

public class PulseConfig {

    private final int httpPort;
    private final String followSessionId;
    private final String[] followProfileIds;
    private final int maxPositions;
    private final double takeProfit;
    private final double stopLose;

    public PulseConfig() throws Exception {
        final Properties properties = PropertiesUtils.loadProperties();
        this.httpPort = Integer.parseInt(properties.getProperty("pulse.follow.httpPort"));
        this.followSessionId = properties.getProperty("pulse.follow.sessionId");
        this.followProfileIds = properties.getProperty("pulse.follow.profileIds").split(";");
        this.maxPositions = Integer.parseInt(properties.getProperty("pulse.follow.maxPositions"));
        this.takeProfit = Double.parseDouble(properties.getProperty("pulse.follow.takeProfit"));
        this.stopLose = Double.parseDouble(properties.getProperty("pulse.follow.stopLose"));
    }

    public int getHttpPort() {
        return httpPort;
    }

    public String getFollowSessionId() {
        return followSessionId;
    }

    public String[] getFollowProfileId() {
        return followProfileIds;
    }

    public int getMaxPositions() {
        return maxPositions;
    }

    public double getTakeProfit() {
        return takeProfit;
    }

    public double getStopLose() {
        return stopLose;
    }
}
