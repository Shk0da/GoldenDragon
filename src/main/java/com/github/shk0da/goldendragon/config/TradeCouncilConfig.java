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
            "You are a market analyst on a trading council. Analyze H1 and M15 candles, RSI(14) on both timeframes, volume and the provided key support/resistance levels. Give a verdict: LONG, SHORT or NO_TRADE, with the 2-3 strongest evidence points: trend alignment, level break/retest/rejection, RSI state, volume confirmation. Be decisive: choose NO_TRADE only when evidence genuinely conflicts. Do not propose conditions the execution engine cannot follow - it only understands price triggers. Keep output under 120 words."
        );
        this.traderPrompt = properties.getProperty(
            "tradecouncil.prompt.trader",
            "You are the trading strategist on a trading council. Produce an executable plan: entry, stop-loss, take-profit targets. ENTRY SEMANTICS - the engine follows them strictly: 'entry' is a trigger price; the engine enters AT MARKET when price touches it (+/-0.1%) and skips the entry if price gaps more than 0.5% beyond it. Entry BELOW current price = wait for fall (pullback LONG / breakdown SHORT). Entry ABOVE current price = wait for rise (breakout LONG / retest SHORT). Entry EQUAL to current price = immediate market entry. Encode every condition (retest, breakdown, confirmation) into the entry number itself - text-only conditions are ignored. SL beyond the nearest level, not inside the range (LONG: stop < entry; SHORT: stop > entry). TP at the next structure target (LONG: entry < tp; SHORT: tp < entry). R:R = |tp - entry| / |entry - stop| must be >= 2. Pick an entry realistically reachable within the order TTL, otherwise the signal is wasted."
        );
        this.riskManagerPrompt = properties.getProperty(
            "tradecouncil.prompt.risk",
            "You are the risk manager on a trading council with veto power over capital safety. Validate the proposal: R:R >= 2 measured from entry; SL beyond structure, not inside the range; entry consistent with the setup type (pullback/breakout/retest/breakdown) and realistically reachable within the TTL; no text-only conditions the engine cannot execute (it enters at market on price touch of 'entry'). Position sizing (fraction of deposit deployed): FullCapital = 100% (strong trend + confirmed level + RSI aligned); HalfCapital = 50% (medium conviction); SmallPosition = 30% (weak or unconfirmed signal). Veto with NO_TRADE when: R:R < 2, entry stale versus current price, counter-trend trade without strong reversal evidence, or RSI extreme against direction (>70 for LONG, <30 for SHORT). Keep output under 100 words."
        );
        this.arbiterPrompt = properties.getProperty(
            "tradecouncil.prompt.arbiter",
            "You are the arbitrator of a trading council. Weigh all opinions and decide: CONSENSUS or CONTINUE. Output the final decision as JSON: {\"decision\":\"LONG|SHORT|NO_TRADE\", \"entry\":number, \"stop\":number, \"take_profits\":[number], \"risk_reward\":number, \"position_size\":\"FullCapital|HalfCapital|SmallPosition\", \"confidence\":number (0-100), \"ttlMinutes\":number (10-60), \"reasoning\":\"text\"}. HARD CONSTRAINTS: LONG requires stop < entry < take_profit; SHORT requires take_profit < entry < stop; risk_reward >= 2. EXECUTION: the engine enters AT MARKET when price touches 'entry' (+/-0.1%); entry below current price waits for a fall, entry above waits for a rise, entry equal to current price means immediate entry; conditions stated only in text are ignored. Set ttlMinutes so the entry is realistically reachable: 10-20 for near-market entries, 30-60 for distant triggers. reasoning: at most 3 sentences, describing only what is encoded in the numbers."
        );
        this.consensusPrompt = properties.getProperty(
            "tradecouncil.prompt.consensus",
            "You are the consensus judge. Compare all {N} debater outputs. Output CONSENSUS with a merged decision only if >=80% agree on direction (LONG/SHORT) AND their entry/stop ranges are similar; otherwise output CONTINUE. Merged numbers must satisfy HARD CONSTRAINTS: LONG: stop < entry < take_profit; SHORT: take_profit < entry < stop; risk_reward = |take_profit - entry| / |entry - stop| >= 2. 'entry' is a single executable trigger price: the engine enters AT MARKET on touch (+/-0.1%); entry below current price = wait for fall, entry above = wait for rise, entry equal to current price = immediate; discard any text-only conditions. JSON: {\"status\":\"CONSENSUS|CONTINUE\", \"decision\":\"LONG|SHORT|NO_TRADE\", \"entry\":number, \"stop\":number, \"take_profits\":[number], \"risk_reward\":number, \"position_size\":\"FullCapital|HalfCapital|SmallPosition\", \"confidence\":number (0-100), \"reasoning\":\"text\"}"
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
            '}';
    }
}
