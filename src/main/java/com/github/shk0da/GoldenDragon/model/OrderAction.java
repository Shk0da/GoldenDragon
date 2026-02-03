package com.github.shk0da.GoldenDragon.model;

public class OrderAction {

    public enum Direct {CLOSE, BUY, SELL}

    private final String name;
    private final Direct direct;
    private final Double sl;
    private final Double tp;

    public OrderAction(String name, Direct direct) {
        this.name = name;
        this.direct = direct;
        this.sl = 0.0;
        this.tp = 0.0;
    }

    public OrderAction(String name, Direct direct, String sl, String tp) {
        this.name = name;
        this.direct = direct;
        this.sl = Double.parseDouble(sl);
        this.tp = Double.parseDouble(tp);
    }

    public String getName() {
        return name;
    }

    public Direct getDirect() {
        return direct;
    }

    public Double getSL() {
        return null == sl ? 0.0 : sl;
    }

    public Double getTP() {
        return null == tp ? 0.0 : tp;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        com.github.shk0da.GoldenDragon.model.OrderAction that = (com.github.shk0da.GoldenDragon.model.OrderAction) o;
        return java.util.Objects.equals(name, that.name) && java.util.Objects.equals(direct, that.direct);
    }

    @Override
    public int hashCode() {
        return java.util.Objects.hash(name, direct);
    }

    @Override
    public String toString() {
        return "OrderAction{" +
                "name='" + name + '\'' +
                ", direct='" + direct + '\'' +
                ", sl='" + sl + '\'' +
                ", tp='" + tp + '\'' +
                '}';
    }
}
