package com.github.shk0da.goldendragon.config;

import com.github.shk0da.goldendragon.model.PortfolioPosition;
import com.github.shk0da.goldendragon.model.TickerInfo;
import com.github.shk0da.goldendragon.model.TickerType;
import com.github.shk0da.goldendragon.utils.PropertiesUtils;
import com.github.shk0da.goldendragon.utils.TickerTypeResolver;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.stream.Collectors;

public class RebalanceConfig {

    public static final String SERIALIZE_NAME = "rebalance.json";

    private final double positionPercent;
    private final List<PortfolioPosition> portfolioPositions = new ArrayList<>();

    public RebalanceConfig() throws Exception {
        final Properties properties = PropertiesUtils.loadProperties();

        double totalPercentage = 0;
        String portfolioRatio = properties.getProperty("rebalance.portfolio.ratio");
        for (String item : portfolioRatio.split(";")) {
            String[] values = item.split(":");
            String ticker;
            TickerType type;
            double percent;

            if (values.length == 3) {
                // Old format: TYPE:TICKER:PERCENT
                type = TickerType.valueOf(values[0]);
                ticker = values[1];
                percent = Double.parseDouble(values[2]);
            } else if (values.length == 2) {
                // New format: TICKER:PERCENT (type resolved automatically)
                ticker = values[0];
                percent = Double.parseDouble(values[1]);
                type = resolveTickerType(ticker);
            } else {
                throw new IllegalArgumentException("Invalid portfolio ratio format: " + item);
            }

            PortfolioPosition portfolioPosition = new PortfolioPosition(ticker, type, percent);
            this.portfolioPositions.add(portfolioPosition);
            totalPercentage += portfolioPosition.getPercent();
        }
        if (totalPercentage > 100.00) {
            throw new IllegalArgumentException("rebalance.portfolio > 100%!");
        }

        positionPercent = Double.parseDouble(properties.getProperty("rebalance.position.percent"));
    }

    /**
     * Resolves TickerType by ticker name using TickerTypeResolver utility. Searches through ticker
     * repository to find the instrument type.
     */
    private TickerType resolveTickerType(String ticker) {
        return TickerTypeResolver.resolve(ticker, TickerType.STOCK);
    }

    public Map<TickerInfo.Key, PortfolioPosition> getPortfolioPositions() {
        return portfolioPositions.stream()
                .collect(
                        Collectors.toMap(
                                it -> new TickerInfo.Key(it.getName(), it.getType()), it -> it));
    }

    public double getPositionPercent() {
        return positionPercent;
    }
}
