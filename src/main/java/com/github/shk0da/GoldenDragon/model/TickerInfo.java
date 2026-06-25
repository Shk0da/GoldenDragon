package com.github.shk0da.goldendragon.model;

import com.fasterxml.jackson.annotation.JsonIgnore;
import java.time.Instant;
import java.util.Objects;

public class TickerInfo {

    public static class Key {

        private final String ticker;
        private final TickerType type;

        public Key(String tickerWithType) {
            String[] key = tickerWithType.split("/");
            this.ticker = key[1];
            this.type = TickerType.byName(key[0]);
        }

        public Key(String ticker, TickerType type) {
            this.ticker = ticker;
            this.type = type;
        }

        public String getTicker() {
            return ticker;
        }

        public TickerType getType() {
            return type;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) {
                return true;
            }
            if (o == null || getClass() != o.getClass()) {
                return false;
            }
            Key key = (Key) o;
            return Objects.equals(ticker, key.ticker) && type == key.type;
        }

        @Override
        public int hashCode() {
            return Objects.hash(ticker, type);
        }

        @Override
        public String toString() {
            return type + "/" + ticker;
        }
    }

    private String figi;
    private String ticker;
    private String isin;
    private Double minPriceIncrement;
    private Integer lot;
    private String currency;
    private String name;
    private TickerType type;
    private boolean forQualInvestorFlag;
    private boolean apiTradeAvailableFlag = true;
    private boolean normalTradingStatus = true;
    private String basicAsset;
    private String assetType;
    private Instant expirationDate;

    public TickerInfo() {}

    public TickerInfo(
            String figi,
            String ticker,
            String isin,
            Double minPriceIncrement,
            Integer lot,
            String currency,
            String name,
            String type) {
        this.figi = figi;
        this.ticker = ticker;
        this.isin = isin;
        this.minPriceIncrement = minPriceIncrement;
        this.lot = lot;
        this.currency = currency;
        this.name = name;
        this.type = TickerType.byName(type);
    }

    public String getFigi() {
        return figi;
    }

    public String getTicker() {
        return ticker;
    }

    public String getIsin() {
        return isin;
    }

    public Double getMinPriceIncrement() {
        return minPriceIncrement;
    }

    public Integer getLot() {
        return lot;
    }

    public String getCurrency() {
        return currency.toUpperCase();
    }

    public String getName() {
        return name;
    }

    public TickerType getType() {
        return type;
    }

    public boolean isForQualInvestorFlag() {
        return forQualInvestorFlag;
    }

    public void setForQualInvestorFlag(boolean forQualInvestorFlag) {
        this.forQualInvestorFlag = forQualInvestorFlag;
    }

    public boolean isApiTradeAvailableFlag() {
        return apiTradeAvailableFlag;
    }

    public void setApiTradeAvailableFlag(boolean apiTradeAvailableFlag) {
        this.apiTradeAvailableFlag = apiTradeAvailableFlag;
    }

    public boolean isNormalTradingStatus() {
        return normalTradingStatus;
    }

    public void setNormalTradingStatus(boolean normalTradingStatus) {
        this.normalTradingStatus = normalTradingStatus;
    }

    public String getBasicAsset() {
        return basicAsset;
    }

    public void setBasicAsset(String basicAsset) {
        this.basicAsset = basicAsset;
    }

    public String getAssetType() {
        return assetType;
    }

    public void setAssetType(String assetType) {
        this.assetType = assetType;
    }

    public Instant getExpirationDate() {
        return expirationDate;
    }

    public void setExpirationDate(Instant expirationDate) {
        this.expirationDate = expirationDate;
    }

    @JsonIgnore
    public TickerInfo.Key getKey() {
        return new TickerInfo.Key(ticker, type);
    }

    @Override
    public String toString() {
        return "TickerInfo{"
                + "figi='"
                + figi
                + '\''
                + ", ticker='"
                + ticker
                + '\''
                + ", isin='"
                + isin
                + '\''
                + ", minPriceIncrement="
                + minPriceIncrement
                + ", lot="
                + lot
                + ", currency='"
                + currency
                + '\''
                + ", name='"
                + name
                + '\''
                + ", type="
                + type
                + '}';
    }
}
