package com.github.shk0da.goldendragon.utils;

import com.github.shk0da.goldendragon.config.MainConfig;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.Properties;

import static org.assertj.core.api.BDDAssertions.then;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@DisplayName("TinkoffApiUrlResolver")
class TinkoffApiUrlResolverTest {

    private MainConfig mainConfig;

    @BeforeEach
    void setUp() throws IOException {
        Properties props = new Properties();
        props.setProperty("tcs.accountId", "test-account");
        props.setProperty("tcs.apiKey", "test-key");
        mainConfig = mock(MainConfig.class);
        when(mainConfig.getTcsAccountId()).thenReturn("test-account");
        when(mainConfig.getTcsApiKey()).thenReturn("test-key");
    }

    @Nested
    @DisplayName("resolveBaseUrl")
    class ResolveBaseUrl {

        @Test
        @DisplayName("Should return sandbox URL when sandbox mode is enabled")
        void shouldReturnSandboxUrl() {
            when(mainConfig.isSandbox()).thenReturn(true);

            String url = TinkoffApiUrlResolver.resolveBaseUrl(mainConfig);

            then(url).isEqualTo("https://sandbox-invest-public-api.tbank.ru");
        }

        @Test
        @DisplayName("Should return production URL when sandbox mode is disabled")
        void shouldReturnProductionUrl() {
            when(mainConfig.isSandbox()).thenReturn(false);

            String url = TinkoffApiUrlResolver.resolveBaseUrl(mainConfig);

            then(url).isEqualTo("https://invest-public-api.tbank.ru");
        }
    }

    @Nested
    @DisplayName("buildRestUrl")
    class BuildRestUrl {

        @Test
        @DisplayName("Should build sandbox URL for GetFuturesMargin endpoint")
        void shouldBuildSandboxFuturesMarginUrl() {
            when(mainConfig.isSandbox()).thenReturn(true);

            String url = TinkoffApiUrlResolver.buildRestUrl(mainConfig, "InstrumentsService/GetFuturesMargin");

            then(url).isEqualTo(
                "https://sandbox-invest-public-api.tbank.ru/rest/tinkoff.public.invest.api.contract.v1.InstrumentsService/GetFuturesMargin");
        }

        @Test
        @DisplayName("Should build production URL for GetFuturesMargin endpoint")
        void shouldBuildProductionFuturesMarginUrl() {
            when(mainConfig.isSandbox()).thenReturn(false);

            String url = TinkoffApiUrlResolver.buildRestUrl(mainConfig, "InstrumentsService/GetFuturesMargin");

            then(url).isEqualTo(
                "https://invest-public-api.tbank.ru/rest/tinkoff.public.invest.api.contract.v1.InstrumentsService/GetFuturesMargin");
        }

        @Test
        @DisplayName("Should build URL for StopOrdersService endpoint")
        void shouldBuildStopOrdersUrl() {
            when(mainConfig.isSandbox()).thenReturn(false);

            String url = TinkoffApiUrlResolver.buildRestUrl(mainConfig, "StopOrdersService/PostStopOrder");

            then(url).isEqualTo(
                "https://invest-public-api.tbank.ru/rest/tinkoff.public.invest.api.contract.v1.StopOrdersService/PostStopOrder");
        }

        @Test
        @DisplayName("Should build URL for OperationsService endpoint")
        void shouldBuildOperationsUrl() {
            when(mainConfig.isSandbox()).thenReturn(false);

            String url = TinkoffApiUrlResolver.buildRestUrl(mainConfig, "OperationsService/GetOperationsByCursor");

            then(url).isEqualTo(
                "https://invest-public-api.tbank.ru/rest/tinkoff.public.invest.api.contract.v1.OperationsService/GetOperationsByCursor");
        }
    }
}
