package com.github.shk0da.goldendragon.strategy.orderbook;

import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Handles partial fills for limit and market orders in the order book trading engine.
 *
 * <p>When an order is executed with fewer units than requested, this handler determines the
 * appropriate action based on the configured strategy: cancel the remaining quantity, re-submit
 * the unfilled portion, or wait for further fills. It also enforces a configurable timeout after
 * which any remaining unfilled quantity is cancelled.
 *
 * <p>Thread-safe: all mutable state is guarded by a {@link ConcurrentHashMap} and volatile fields.
 */
public final class PartialFillHandler {

  /**
   * Strategy for handling the unfilled portion of a partially filled order.
   */
  public enum Strategy {

    /** Cancel the remaining unfilled quantity immediately. */
    CANCEL_REMAINING,

    /** Re-submit the unfilled quantity as a new order. */
    RESUBMIT,

    /** Wait for the remaining quantity to fill within the timeout period. */
    WAIT
  }

  /**
   * Action to take for the unfilled portion of an order.
   */
  public enum Action {

    /** No action needed — order was fully filled. */
    NONE,

    /** Cancel the remaining unfilled quantity. */
    CANCEL,

    /** Re-submit the unfilled quantity as a new order. */
    RESUBMIT,

    /** Wait for more fills within the timeout. */
    WAIT
  }

  /**
   * Input DTO for reporting an order execution to the partial fill handler.
   */
  public static final class OrderReport {

    private final String ticker;
    private final String orderId;
    private final String direction;
    private final int orderedQuantity;
    private final int executedQuantity;
    private final double fillPrice;

    public OrderReport(
        String ticker,
        String orderId,
        String direction,
        int orderedQuantity,
        int executedQuantity,
        double fillPrice) {

      this.ticker = ticker;
      this.orderId = orderId;
      this.direction = direction;
      this.orderedQuantity = orderedQuantity;
      this.executedQuantity = executedQuantity;
      this.fillPrice = fillPrice;
    }

    public String getTicker() {
      return ticker;
    }

    public String getOrderId() {
      return orderId;
    }

    public String getDirection() {
      return direction;
    }

    public int getOrderedQuantity() {
      return orderedQuantity;
    }

    public int getExecutedQuantity() {
      return executedQuantity;
    }

    public double getFillPrice() {
      return fillPrice;
    }
  }

  /**
   * Tracks a pending order that has been partially filled and requires resolution.
   */
  public static final class PendingFill {

    private final String ticker;
    private final String orderId;
    private final String direction;
    private final int orderedQuantity;
    private volatile int filledQuantity;
    private volatile double averageFillPrice;
    private final long placedAtMs;
    private volatile long lastFillAtMs;
    private final Strategy strategy;
    private int resubmitAttempts;

    PendingFill(
        String ticker,
        String orderId,
        String direction,
        int orderedQuantity,
        int filledQuantity,
        double averageFillPrice,
        long placedAtMs,
        Strategy strategy) {

      this.ticker = ticker;
      this.orderId = orderId;
      this.direction = direction;
      this.orderedQuantity = orderedQuantity;
      this.filledQuantity = filledQuantity;
      this.averageFillPrice = averageFillPrice;
      this.placedAtMs = placedAtMs;
      this.lastFillAtMs = placedAtMs;
      this.strategy = strategy;
      this.resubmitAttempts = 0;
    }

    public String getTicker() {
      return ticker;
    }

    public String getOrderId() {
      return orderId;
    }

    public String getDirection() {
      return direction;
    }

    public int getOrderedQuantity() {
      return orderedQuantity;
    }

    public int getFilledQuantity() {
      return filledQuantity;
    }

    public double getAverageFillPrice() {
      return averageFillPrice;
    }

    public long getPlacedAtMs() {
      return placedAtMs;
    }

    public long getLastFillAtMs() {
      return lastFillAtMs;
    }

    public Strategy getStrategy() {
      return strategy;
    }

    public int getResubmitAttempts() {
      return resubmitAttempts;
    }

    public int getUnfilledQuantity() {
      return orderedQuantity - filledQuantity;
    }

    public double getFillRatio() {
      if (orderedQuantity == 0) {
        return 0.0;
      }
      return (double) filledQuantity / orderedQuantity;
    }
  }

  /**
   * Result of processing a partial fill event.
   */
  public static final class FillResult {

    private final boolean partialFill;
    private final int filledQuantity;
    private final int unfilledQuantity;
    private final double fillRatio;
    private final Action recommendedAction;

    FillResult(
        boolean partialFill,
        int filledQuantity,
        int unfilledQuantity,
        double fillRatio,
        Action recommendedAction) {

      this.partialFill = partialFill;
      this.filledQuantity = filledQuantity;
      this.unfilledQuantity = unfilledQuantity;
      this.fillRatio = fillRatio;
      this.recommendedAction = recommendedAction;
    }

    static FillResult fullFill(int quantity) {
      return new FillResult(false, quantity, 0, 1.0, Action.NONE);
    }

    public boolean isPartialFill() {
      return partialFill;
    }

    public int getFilledQuantity() {
      return filledQuantity;
    }

    public int getUnfilledQuantity() {
      return unfilledQuantity;
    }

    public double getFillRatio() {
      return fillRatio;
    }

    public Action getRecommendedAction() {
      return recommendedAction;
    }
  }

  private final long partialFillTimeoutMs;
  private final Strategy defaultStrategy;
  private final int maxResubmitAttempts;

  private final Map<String, PendingFill> pendingFillsByTicker = new ConcurrentHashMap<>();

  /**
   * Creates a new PartialFillHandler.
   *
   * @param partialFillTimeoutMs maximum time to wait for remaining fills before cancelling
   * @param defaultStrategy default strategy for handling unfilled quantities
   * @param maxResubmitAttempts maximum number of re-submit attempts for RESUBMIT strategy
   */
  public PartialFillHandler(
      long partialFillTimeoutMs,
      Strategy defaultStrategy,
      int maxResubmitAttempts) {

    if (partialFillTimeoutMs <= 0) {
      throw new IllegalArgumentException("partialFillTimeoutMs must be positive");
    }
    if (defaultStrategy == null) {
      throw new IllegalArgumentException("defaultStrategy must not be null");
    }
    if (maxResubmitAttempts < 0) {
      throw new IllegalArgumentException("maxResubmitAttempts must be non-negative");
    }
    this.partialFillTimeoutMs = partialFillTimeoutMs;
    this.defaultStrategy = defaultStrategy;
    this.maxResubmitAttempts = maxResubmitAttempts;
  }

  /**
   * Processes an order execution report and detects partial fills.
   *
   * <p>If the executed quantity is less than the ordered quantity, a partial fill is recorded and
   * the recommended action is determined based on the configured strategy.
   *
   * @param report order execution details
   * @return result describing the fill status and recommended action
   */
  public FillResult processExecution(OrderReport report) {
    int orderedQuantity = report.getOrderedQuantity();
    int executedQuantity = report.getExecutedQuantity();
    double fillPrice = report.getFillPrice();
    String ticker = report.getTicker();

    if (orderedQuantity <= 0) {
      throw new IllegalArgumentException("orderedQuantity must be positive");
    }
    if (executedQuantity < 0 || executedQuantity > orderedQuantity) {
      throw new IllegalArgumentException(
          "executedQuantity must be between 0 and orderedQuantity");
    }

    // Full fill — no partial handling needed
    if (executedQuantity == orderedQuantity) {
      pendingFillsByTicker.remove(ticker);
      return FillResult.fullFill(executedQuantity);
    }

    // Zero fill — treated as failed order, not a partial fill
    if (executedQuantity == 0) {
      pendingFillsByTicker.remove(ticker);
      return new FillResult(false, 0, 0, 0.0, Action.NONE);
    }

    int unfilled = orderedQuantity - executedQuantity;
    double fillRatio = (double) executedQuantity / orderedQuantity;
    long now = System.currentTimeMillis();

    PendingFill pending = buildOrUpdatePendingFill(report, unfilled, fillRatio, now);
    pendingFillsByTicker.put(ticker, pending);

    Action action = resolveAction(pending, now);
    logPartialFill(report, unfilled, fillRatio, action);

    return new FillResult(true, executedQuantity, unfilled, fillRatio, action);
  }

  /**
   * Checks all tracked pending fills for timeout expiry and returns tickers that need cancellation.
   *
   * <p>A pending fill times out when the elapsed time since the last fill exceeds the configured
   * timeout. This method should be called periodically from the engine's main loop.
   *
   * @return list of ticker symbols whose pending fills have timed out and should be cancelled
   */
  public List<String> checkTimeouts() {
    long now = System.currentTimeMillis();
    List<String> timedOut = new ArrayList<>();

    for (Map.Entry<String, PendingFill> entry : pendingFillsByTicker.entrySet()) {
      PendingFill pending = entry.getValue();
      if (isTimedOut(pending, now)) {
        timedOut.add(entry.getKey());
      }
    }

    return timedOut;
  }

  /**
   * Checks whether a specific pending fill has timed out.
   *
   * @param ticker ticker symbol to check
   * @return true if the pending fill for this ticker has exceeded the timeout
   */
  public boolean isTimedOut(String ticker) {
    PendingFill pending = pendingFillsByTicker.get(ticker);
    if (pending == null) {
      return false;
    }
    return isTimedOut(pending, System.currentTimeMillis());
  }

  /**
   * Returns the pending fill for a ticker, or null if none exists.
   *
   * @param ticker ticker symbol
   * @return pending fill state, or null
   */
  public PendingFill getPendingFill(String ticker) {
    return pendingFillsByTicker.get(ticker);
  }

  /**
   * Returns the unfilled quantity for a ticker, or 0 if no pending fill exists.
   *
   * @param ticker ticker symbol
   * @return unfilled quantity
   */
  public int getUnfilledQuantity(String ticker) {
    PendingFill pending = pendingFillsByTicker.get(ticker);
    if (pending == null) {
      return 0;
    }
    return pending.getUnfilledQuantity();
  }

  /**
   * Records that a resubmit attempt was made for a ticker.
   *
   * <p>Call this after re-submitting the unfilled portion so the handler can track the attempt
   * count and stop re-submitting after the configured maximum.
   *
   * @param ticker ticker symbol
   */
  public void recordResubmitAttempt(String ticker) {
    PendingFill pending = pendingFillsByTicker.get(ticker);
    if (pending != null) {
      pending.resubmitAttempts++;
      pending.lastFillAtMs = System.currentTimeMillis();
    }
  }

  /**
   * Removes the pending fill tracking for a ticker after the unfilled portion has been resolved
   * (cancelled or fully filled).
   *
   * @param ticker ticker symbol
   */
  public void clearPendingFill(String ticker) {
    pendingFillsByTicker.remove(ticker);
  }

  /**
   * Returns the number of tickers with active pending fills.
   *
   * @return count of tracked partial fills
   */
  public int getActivePendingCount() {
    return pendingFillsByTicker.size();
  }

  /**
   * Returns the configured partial fill timeout in milliseconds.
   *
   * @return timeout in milliseconds
   */
  public long getPartialFillTimeoutMs() {
    return partialFillTimeoutMs;
  }

  /**
   * Returns the default strategy for handling partial fills.
   *
   * @return default strategy
   */
  public Strategy getDefaultStrategy() {
    return defaultStrategy;
  }

  private PendingFill buildOrUpdatePendingFill(
      OrderReport report, int unfilled, double fillRatio, long now) {

    String ticker = report.getTicker();
    PendingFill existing = pendingFillsByTicker.get(ticker);

    if (existing != null) {
      // Accumulate into existing partial fill
      int totalFilled = existing.getFilledQuantity() + report.getExecutedQuantity();
      double weightedPrice = computeWeightedPrice(existing, report, totalFilled);
      PendingFill updated = new PendingFill(
          ticker,
          report.getOrderId(),
          report.getDirection(),
          report.getOrderedQuantity(),
          totalFilled,
          weightedPrice,
          existing.getPlacedAtMs(),
          existing.getStrategy());
      updated.lastFillAtMs = now;
      updated.resubmitAttempts = existing.getResubmitAttempts();
      return updated;
    }

    return new PendingFill(
        ticker,
        report.getOrderId(),
        report.getDirection(),
        report.getOrderedQuantity(),
        report.getExecutedQuantity(),
        report.getFillPrice(),
        now,
        defaultStrategy);
  }

  private double computeWeightedPrice(
      PendingFill existing, OrderReport report, int totalFilled) {

    return (existing.getAverageFillPrice() * existing.getFilledQuantity()
        + report.getFillPrice() * report.getExecutedQuantity())
        / totalFilled;
  }

  private Action resolveAction(PendingFill pending, long now) {
    if (isTimedOut(pending, now)) {
      return Action.CANCEL;
    }

    switch (pending.getStrategy()) {
      case CANCEL_REMAINING:
        return Action.CANCEL;
      case RESUBMIT:
        if (pending.getResubmitAttempts() >= maxResubmitAttempts) {
          return Action.CANCEL;
        }
        return Action.RESUBMIT;
      case WAIT:
        return Action.WAIT;
      default:
        return Action.CANCEL;
    }
  }

  private boolean isTimedOut(PendingFill pending, long now) {
    return now - pending.getLastFillAtMs() > partialFillTimeoutMs;
  }

  private void logPartialFill(
      OrderReport report, int unfilledQuantity, double fillRatio, Action action) {

    System.out.println(
        Instant.now().toString()
            + " PARTIAL_FILL " + report.getTicker() + " " + report.getDirection()
            + " ordered=" + report.getOrderedQuantity()
            + " filled=" + report.getExecutedQuantity()
            + " remaining=" + unfilledQuantity
            + " fillRatio=" + String.format("%.2f%%", fillRatio * 100.0)
            + " action=" + action);
  }
}
