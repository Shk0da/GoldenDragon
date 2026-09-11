package com.github.shk0da.goldendragon.config;

import com.github.shk0da.goldendragon.utils.PropertiesUtils;

import java.io.IOException;
import java.util.Properties;

/**
 * Configuration for TradeCouncilStrategy.
 * Loads and provides access to TradeCouncil-specific properties.
 */
public class TradeCouncilConfig {

    private final Properties properties;

    // AI/LLM Configuration
    private final String openAiBaseUrl;
    private final String openAiApiKey;
    private final String debaterModel;
    private final String arbiterModel;

    // Debate Agent Prompts
    private final String analystPrompt;
    private final String traderPrompt;
    private final String riskManagerPrompt;
    private final String arbiterPrompt;
    private final String consensusPrompt;

    // Trading Parameters
    private final double proximityPercent;
    private final double riskPerTradePercent;
    private final int debateRounds;
    private final double entryCashPercent;

    public TradeCouncilConfig() throws IOException {
        this.properties = PropertiesUtils.loadProperties();

        this.openAiBaseUrl = properties.getProperty(
            "tradecouncil.openai.baseUrl",
            "http://localhost:4000/v1"
        );
        this.openAiApiKey = properties.getProperty(
            "tradecouncil.openai.apiKey",
            "12345"
        );
        this.debaterModel = properties.getProperty(
            "tradecouncil.debater.model",
            "shcoder"
        );
        this.arbiterModel = properties.getProperty(
            "tradecouncil.arbiter.model",
            "shcoder"
        );
        this.analystPrompt = properties.getProperty(
            "tradecouncil.prompt.analyst",
            "You are an analyst-trader. Analyze H1 and M15 candles, RSI (H1/M15), volume, key support/resistance levels and recent price patterns. Determine direction (LONG / SHORT / NO_TRADE). Consider: H1 and M15 trend direction, RSI overbought/oversold, level break/rebound."
        );
        this.traderPrompt = properties.getProperty(
            "tradecouncil.prompt.trader",
            "You are a trading strategist. Define: entry, stop-loss based on nearest support/resistance level, take-profit(s) based on targets and H1+M15 structure. Calculate R:R for each candidate (min 1:2 for profitable). Entry: at level, on retest or inside H1+M15 context. SL: beyond the nearest level, not inside the range."
        );
        this.riskManagerPrompt = properties.getProperty(
            "tradecouncil.prompt.risk",
            "You are a risk manager. Validate R:R (min 1:2 for profitable). Determine position sizing based on signal strength: strong trend + confirmed level = 'Full Capital'; medium signal = 'Half Capital'; weak signal / uncertain level = 'Small Position'. Consider RSI (overbought >70 for long, oversold <30 for short). Recommend NO_TRADE if R:R < 1:2."
        );
        this.arbiterPrompt = properties.getProperty(
            "tradecouncil.prompt.arbiter",
            "You are an arbitrator. Listen to all opinions, evaluate agreement and decide: CONSENSUS or CONTINUE. Output final decision as JSON: {\"decision\":\"LONG|SHORT|NO_TRADE\", \"entry\":number, \"stop\":number, \"take_profits\":[number], \"risk_reward\":number, \"position_size\":\"FullCapital|HalfCapital|SmallPosition\", \"confidence\":number (0-100), \"ttlMinutes\":number (10-60), \"reasoning\":\"text\"}. ttlMinutes: how long to wait for entry (10-60 min, shorter for volatile instruments, longer for stable trends)"
        );
        this.consensusPrompt = properties.getProperty(
            "tradecouncil.prompt.consensus",
            "You are a consensus judge. Compare all {N} debater outputs. If >=80% agree on direction (LONG/SHORT) AND similar entry/stop ranges, output CONSENSUS with merged decision. Otherwise output CONTINUE. JSON: {\"status\":\"CONSENSUS|CONTINUE\", \"decision\":\"LONG|SHORT|NO_TRADE\", \"entry\":number, \"stop\":number, \"take_profits\":[number], \"risk_reward\":number, \"position_size\":\"FullCapital|HalfCapital|SmallPosition\", \"confidence\":number (0-100), \"reasoning\":\"text\"}"
        );
        this.proximityPercent = Double.parseDouble(
            properties.getProperty("tradecouncil.proximity.percent", "2.0")
        );
        this.riskPerTradePercent = Double.parseDouble(
            properties.getProperty("tradecouncil.risk.percent", "1.0")
        );
        this.debateRounds = Integer.parseInt(
            properties.getProperty("tradecouncil.debate.rounds", "3")
        );
        this.entryCashPercent = Double.parseDouble(
            properties.getProperty("tradecouncil.entry.cashPercent", "95.0")
        );
    }

    public String getOpenAiBaseUrl() {
        return openAiBaseUrl;
    }

    public String getOpenAiApiKey() {
        return openAiApiKey;
    }

    public String getDebaterModel() {
        return debaterModel;
    }

    public String getArbiterModel() {
        return arbiterModel;
    }

    public String getAnalystPrompt() {
        return analystPrompt;
    }

    public String getTraderPrompt() {
        return traderPrompt;
    }

    public String getRiskManagerPrompt() {
        return riskManagerPrompt;
    }

    public String getArbiterPrompt() {
        return arbiterPrompt;
    }

    public String getConsensusPrompt() {
        return consensusPrompt;
    }

    public double getProximityPercent() {
        return proximityPercent;
    }

    public double getRiskPerTradePercent() {
        return riskPerTradePercent;
    }

    public int getDebateRounds() {
        return debateRounds;
    }

    public double getEntryCashPercent() {
        return entryCashPercent;
    }

    public Properties getProperties() {
        return properties;
    }

    @Override
    public String toString() {
        return "TradeCouncilConfig{" +
            "openAiBaseUrl='" + openAiBaseUrl + '\'' +
            ", debaterModel='" + debaterModel + '\'' +
            ", arbiterModel='" + arbiterModel + '\'' +
            ", proximityPercent=" + proximityPercent +
            ", riskPerTradePercent=" + riskPerTradePercent +
            ", debateRounds=" + debateRounds +
            ", entryCashPercent=" + entryCashPercent +
            '}';
    }
}
