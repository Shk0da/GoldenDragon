package com.github.shk0da.GoldenDragon.config;

import com.github.shk0da.GoldenDragon.model.PortfolioPosition;
import com.github.shk0da.GoldenDragon.model.TickerInfo;
import com.github.shk0da.GoldenDragon.model.TickerType;
import com.github.shk0da.GoldenDragon.utils.PropertiesUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.stream.Collectors;

public class RebalanceConfig {

    public static final String SERIALIZE_NAME = "rebalance.json";

    private final List<PortfolioPosition> portfolioPositions = new ArrayList<>();

    public RebalanceConfig() throws Exception {
        final Properties properties = PropertiesUtils.loadProperties();

        double totalPercentage = 0;
        String portfolioRatio = properties.getProperty("rebalance.portfolio.ratio");
        for (String item : portfolioRatio.split(";")) {
            String[] values = item.split(":");
            PortfolioPosition portfolioPosition = new PortfolioPosition(
                    values[1], TickerType.valueOf(values[0]), Double.parseDouble(values[2])
            );
            this.portfolioPositions.add(portfolioPosition);
            totalPercentage += portfolioPosition.getPercent();
        }
        if (totalPercentage > 100.00) {
            throw new IllegalArgumentException("rebalance.portfolio > 100%!");
        }
    }

    public Map<TickerInfo.Key, PortfolioPosition> getPortfolioPositions() {
        return portfolioPositions.stream().collect(Collectors.toMap(it -> new TickerInfo.Key(it.getName(), it.getType()), it -> it));
    }
}
