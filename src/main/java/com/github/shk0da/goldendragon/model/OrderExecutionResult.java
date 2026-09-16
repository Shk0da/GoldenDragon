package com.github.shk0da.goldendragon.model;

/**
 * Result of an order execution with details on price, count, commission, and the
 * protective position that was created as part of a bracket order.
 */
public class OrderExecutionResult {

    private final boolean success;
    private final Double executedPrice;
    private final int executedCount;
    private final double commission;
    private final Position protectivePosition;
    private final int errorCode;
    private final String errorMessage;

    private OrderExecutionResult(
            boolean success,
            Double executedPrice,
            int executedCount,
            double commission,
            Position protectivePosition) {
        this(success, executedPrice, executedCount, commission, protectivePosition, 0, null);
    }

    private OrderExecutionResult(
            boolean success,
            Double executedPrice,
            int executedCount,
            double commission,
            Position protectivePosition,
            int errorCode) {
        this(success, executedPrice, executedCount, commission, protectivePosition, errorCode, null);
    }

    private OrderExecutionResult(
            boolean success,
            Double executedPrice,
            int executedCount,
            double commission,
            Position protectivePosition,
            int errorCode,
            String errorMessage) {
        this.success = success;
        this.executedPrice = executedPrice;
        this.executedCount = executedCount;
        this.commission = commission;
        this.protectivePosition = protectivePosition;
        this.errorCode = errorCode;
        this.errorMessage = errorMessage;
    }

    public static OrderExecutionResult success(
            Double executedPrice,
            int executedCount,
            double commission,
            Position protectivePosition) {
        return new OrderExecutionResult(
                true, executedPrice, executedCount, commission, protectivePosition);
    }

    public static OrderExecutionResult testSuccess(Double executedPrice, int executedCount) {
        return new OrderExecutionResult(true, executedPrice, executedCount, 0.0, null);
    }

    public static OrderExecutionResult failed() {
        return new OrderExecutionResult(false, null, 0, 0.0, null);
    }

    public static OrderExecutionResult failed(String errorMessage) {
        return new OrderExecutionResult(false, null, 0, 0.0, null);
    }

    public static OrderExecutionResult failed(int errorCode, String errorMessage) {
        return new OrderExecutionResult(false, null, 0, 0.0, null, errorCode, errorMessage);
    }

    public boolean isSuccess() {
        return success;
    }

    public String getErrorMessage() {
        return errorMessage;
    }

    public Double getExecutedPrice() {
        return executedPrice;
    }

    public int getExecutedCount() {
        return executedCount;
    }

    public double getCommission() {
        return commission;
    }

    public Position getProtectivePosition() {
        return protectivePosition;
    }
}
