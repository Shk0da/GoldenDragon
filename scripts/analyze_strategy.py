#!/usr/bin/env python3
"""
Order book strategy analytics script.

Parses orderbook-metrics.csv and orderbook-diagnostics-replay.log
to produce a comprehensive trading performance report.

Usage:
    python scripts/analyze_strategy.py
    python scripts/analyze_strategy.py --csv analytics/orderbook-metrics.csv --log analytics/orderbook-diagnostics-replay.log
"""

import csv
import re
import sys
import os
from collections import defaultdict
from dataclasses import dataclass, field
from typing import List, Optional


@dataclass
class Trade:
    ticker: str
    direction: str
    signal: str
    entry_price: float
    exit_price: Optional[float]
    units: int
    gross_pnl: float
    net_pnl: float
    fees: float
    hold_seconds: int
    reason: str
    quality: float
    entry_time: str
    exit_time: str


@dataclass
class SkipStats:
    reason: str
    count: int = 0
    tickers: dict = field(default_factory=lambda: defaultdict(int))


def parse_metrics_csv(path: str) -> tuple:
    """Parse orderbook-metrics.csv into trades and skip events."""
    trades = []
    skip_reasons = defaultdict(lambda: SkipStats(""))
    entry_events = []
    all_events = []

    with open(path, "r", encoding="utf-8") as f:
        reader = csv.DictReader(f)
        for row in reader:
            event_type = row.get("type", "")
            ticker = row.get("ticker", "")
            timestamp = row.get("timestamp", "")

            all_events.append(row)

            if event_type == "ENTRY_SKIPPED":
                reason = row.get("reason", "unknown")
                skip_reasons[reason].reason = reason
                skip_reasons[reason].count += 1
                skip_reasons[reason].tickers[ticker] += 1

            elif event_type == "ENTRY_OPENED":
                entry_events.append(row)

            elif event_type == "POSITION_CLOSED":
                entry_price = _float_or(row.get("entryPrice"), 0.0)
                exit_price = _float_or(row.get("exitPrice"), None)
                gross_pnl = _float_or(row.get("grossPnl"), 0.0)
                net_pnl = _float_or(row.get("netPnl"), 0.0)
                fees = _float_or(row.get("fees"), 0.0)
                hold_seconds = _int_or(row.get("holdSeconds"), 0)
                units = _int_or(row.get("units"), 0)
                reason = row.get("reason", "unknown")
                direction = row.get("direction", "")

                # Find matching entry for quality/signal
                quality = 0.0
                signal = ""
                entry_time = ""
                for entry in reversed(entry_events):
                    if entry.get("ticker") == ticker:
                        quality = _float_or(entry.get("quality"), 0.0)
                        signal = entry.get("reason", "")
                        entry_time = entry.get("timestamp", "")
                        break

                trade = Trade(
                    ticker=ticker,
                    direction=direction,
                    signal=signal,
                    entry_price=entry_price,
                    exit_price=exit_price,
                    units=units,
                    gross_pnl=gross_pnl,
                    net_pnl=net_pnl,
                    fees=fees,
                    hold_seconds=hold_seconds,
                    reason=reason,
                    quality=quality,
                    entry_time=entry_time,
                    exit_time=timestamp,
                )
                trades.append(trade)

    return trades, dict(skip_reasons), entry_events, all_events


def parse_diagnostics_log(path: str) -> dict:
    """Parse orderbook-diagnostics-replay.log for additional context."""
    stats = {
        "total_events": 0,
        "event_counts": defaultdict(int),
        "entry_flow_values": [],
        "skip_flow_values": [],
        "position_hold_times": [],
    }

    flow_pattern = re.compile(r"flow=(-?[\d.]+)")
    hold_pattern = re.compile(r"holdSeconds=(\d+)")

    with open(path, "r", encoding="utf-8") as f:
        for line in f:
            stats["total_events"] += 1
            parts = line.strip().split("|")
            if len(parts) >= 3:
                event_type = parts[1]
                stats["event_counts"][event_type] += 1

                if "flow=" in line:
                    m = flow_pattern.search(line)
                    if m:
                        flow_val = float(m.group(1))
                        if event_type == "ENTRY_SKIPPED":
                            stats["skip_flow_values"].append(flow_val)
                        else:
                            stats["entry_flow_values"].append(flow_val)

                if "holdSeconds=" in line:
                    m = hold_pattern.search(line)
                    if m:
                        stats["position_hold_times"].append(int(m.group(1)))

    return stats


def _float_or(val, default):
    if val is None or val == "" or val == "NaN":
        return default
    try:
        return float(val)
    except (ValueError, TypeError):
        return default


def _int_or(val, default):
    if val is None or val == "" or val == "NaN":
        return default
    try:
        return int(float(val))
    except (ValueError, TypeError):
        return default


def print_section(title: str):
    print(f"\n{'='*70}")
    print(f"  {title}")
    print(f"{'='*70}")


def print_subsection(title: str):
    print(f"\n  --- {title} ---")


def analyze(trades: list, skip_reasons: dict, entry_events: list, diag_stats: dict):
    """Print comprehensive analytics report."""

    # --- Header ---
    print_section("ORDER BOOK STRATEGY ANALYTICS REPORT")
    if trades:
        print(f"  Period: {trades[0].entry_time} -> {trades[-1].exit_time}")
    print(f"  Closed positions: {len(trades)}")
    print(f"  Total diagnostic events: {diag_stats['total_events']}")

    # --- Overall P&L ---
    print_section("1. OVERALL PERFORMANCE")
    total_net = sum(t.net_pnl for t in trades)
    total_gross = sum(t.gross_pnl for t in trades)
    total_fees = sum(t.fees for t in trades)
    wins = [t for t in trades if t.net_pnl > 0]
    losses = [t for t in trades if t.net_pnl <= 0]

    print(f"  Total net P&L:     {total_net:>12.2f} RUB")
    print(f"  Total gross P&L:   {total_gross:>12.2f} RUB")
    print(f"  Total fees:        {total_fees:>12.2f} RUB")
    print(f"  Trades:            {len(trades):>12}")
    print(f"  Wins:              {len(wins):>12} ({len(wins)/max(len(trades),1)*100:.1f}%)")
    print(f"  Losses:            {len(losses):>12} ({len(losses)/max(len(trades),1)*100:.1f}%)")

    if wins:
        avg_win = sum(t.net_pnl for t in wins) / len(wins)
        print(f"  Avg win:           {avg_win:>12.2f} RUB")
    if losses:
        avg_loss = sum(t.net_pnl for t in losses) / len(losses)
        print(f"  Avg loss:          {avg_loss:>12.2f} RUB")
    if wins and losses:
        profit_factor = abs(sum(t.net_pnl for t in wins)) / abs(sum(t.net_pnl for t in losses))
        print(f"  Profit factor:     {profit_factor:>12.2f}")
        expectancy = total_net / len(trades)
        print(f"  Expectancy:        {expectancy:>12.2f} RUB/trade")

    # --- P&L by Instrument ---
    print_section("2. P&L BY INSTRUMENT")
    by_ticker = defaultdict(list)
    for t in trades:
        by_ticker[t.ticker].append(t)

    print(f"  {'Ticker':<12} {'Trades':>7} {'Wins':>5} {'WinR%':>6} {'Net P&L':>12} {'Avg P&L':>10} {'Fees':>10}")
    print(f"  {'-'*12} {'-'*7} {'-'*5} {'-'*6} {'-'*12} {'-'*10} {'-'*10}")
    for ticker in sorted(by_ticker.keys()):
        tlist = by_ticker[ticker]
        tw = [t for t in tlist if t.net_pnl > 0]
        tnet = sum(t.net_pnl for t in tlist)
        tfees = sum(t.fees for t in tlist)
        wr = len(tw) / len(tlist) * 100 if tlist else 0
        avg = tnet / len(tlist) if tlist else 0
        print(f"  {ticker:<12} {len(tlist):>7} {len(tw):>5} {wr:>5.1f}% {tnet:>12.2f} {avg:>10.2f} {tfees:>10.2f}")

    # --- P&L by Signal ---
    print_section("3. P&L BY SIGNAL TYPE")
    by_signal = defaultdict(list)
    for t in trades:
        by_signal[t.signal or "unknown"].append(t)

    print(f"  {'Signal':<15} {'Trades':>7} {'Wins':>5} {'WinR%':>6} {'Net P&L':>12} {'Avg P&L':>10}")
    print(f"  {'-'*15} {'-'*7} {'-'*5} {'-'*6} {'-'*12} {'-'*10}")
    for signal in sorted(by_signal.keys()):
        slist = by_signal[signal]
        sw = [t for t in slist if t.net_pnl > 0]
        snet = sum(t.net_pnl for t in slist)
        wr = len(sw) / len(slist) * 100 if slist else 0
        avg = snet / len(slist) if slist else 0
        print(f"  {signal:<15} {len(slist):>7} {len(sw):>5} {wr:>5.1f}% {snet:>12.2f} {avg:>10.2f}")

    # --- P&L by Direction ---
    print_section("4. P&L BY DIRECTION")
    by_dir = defaultdict(list)
    for t in trades:
        by_dir[t.direction or "unknown"].append(t)

    for direction in sorted(by_dir.keys()):
        dlist = by_dir[direction]
        dw = [t for t in dlist if t.net_pnl > 0]
        dnet = sum(t.net_pnl for t in dlist)
        dwr = len(dw) / len(dlist) * 100 if dlist else 0
        print(f"  {direction:<8} trades={len(dlist)}  wins={len(dw)}  winR={dwr:.1f}%  netPnL={dnet:.2f}")

    # --- Exit Reason Analysis ---
    print_section("5. EXIT REASON ANALYSIS")
    by_reason = defaultdict(list)
    for t in trades:
        by_reason[t.reason].append(t)

    print(f"  {'Reason':<20} {'Count':>6} {'Net P&L':>12} {'Avg P&L':>10} {'Avg Hold':>10}")
    print(f"  {'-'*20} {'-'*6} {'-'*12} {'-'*10} {'-'*10}")
    for reason in sorted(by_reason.keys()):
        rlist = by_reason[reason]
        rnet = sum(t.net_pnl for t in rlist)
        ravg = rnet / len(rlist) if rlist else 0
        rhold = sum(t.hold_seconds for t in rlist) / len(rlist) if rlist else 0
        print(f"  {reason:<20} {len(rlist):>6} {rnet:>12.2f} {ravg:>10.2f} {rhold:>8.0f}s")

    # --- Position Sizing Analysis ---
    print_section("6. POSITION SIZING")
    print(f"  {'Ticker':<12} {'Units':>7} {'Entry':>10} {'Notional':>12} {'Margin25%':>10}")
    print(f"  {'-'*12} {'-'*7} {'-'*10} {'-'*12} {'-'*10}")
    for t in trades:
        notional = t.units * t.entry_price
        margin = notional * 0.25
        print(f"  {t.ticker:<12} {t.units:>7} {t.entry_price:>10.2f} {notional:>12.2f} {margin:>10.2f}")

    # --- Hold Time Analysis ---
    print_section("7. HOLD TIME ANALYSIS")
    hold_times = [t.hold_seconds for t in trades]
    if hold_times:
        avg_hold = sum(hold_times) / len(hold_times)
        min_hold = min(hold_times)
        max_hold = max(hold_times)
        print(f"  Avg hold: {avg_hold:.0f}s ({avg_hold/60:.1f}m)")
        print(f"  Min hold: {min_hold}s")
        print(f"  Max hold: {max_hold}s ({max_hold/60:.1f}m)")

        # Hold time buckets
        buckets = {"<30s": 0, "30s-2m": 0, "2m-5m": 0, "5m-10m": 0, ">10m": 0}
        for h in hold_times:
            if h < 30:
                buckets["<30s"] += 1
            elif h < 120:
                buckets["30s-2m"] += 1
            elif h < 300:
                buckets["2m-5m"] += 1
            elif h < 600:
                buckets["5m-10m"] += 1
            else:
                buckets[">10m"] += 1
        for bucket, count in buckets.items():
            bar = "#" * count
            print(f"    {bucket:>8}: {count:>3} {bar}")

    # --- Quality Score Analysis ---
    print_section("8. QUALITY SCORE ANALYSIS")
    for t in trades:
        marker = "WIN " if t.net_pnl > 0 else "LOSS"
        print(f"  [{marker}] {t.ticker:<8} quality={t.quality:.3f}  netPnL={t.net_pnl:>10.2f}  signal={t.signal}")

    win_qualities = [t.quality for t in trades if t.net_pnl > 0 and t.quality > 0]
    loss_qualities = [t.quality for t in trades if t.net_pnl <= 0 and t.quality > 0]
    if win_qualities:
        print(f"\n  Avg quality (wins):  {sum(win_qualities)/len(win_qualities):.3f}")
    if loss_qualities:
        print(f"  Avg quality (losses): {sum(loss_qualities)/len(loss_qualities):.3f}")

    # --- Fee Impact ---
    print_section("9. FEE IMPACT ANALYSIS")
    if trades:
        fee_ratio = total_fees / abs(total_gross) * 100 if total_gross != 0 else 0
        print(f"  Total fees:          {total_fees:>12.2f} RUB")
        print(f"  Gross P&L:           {total_gross:>12.2f} RUB")
        print(f"  Fee/Gross ratio:     {fee_ratio:>11.1f}%")
        print(f"  Fees ate {fee_ratio:.1f}% of gross {'profit' if total_gross > 0 else 'loss'}")

        # Per-instrument fee analysis
        print_subsection("Fee per instrument")
        for ticker in sorted(by_ticker.keys()):
            tlist = by_ticker[ticker]
            tf = sum(t.fees for t in tlist)
            tg = sum(t.gross_pnl for t in tlist)
            fr = tf / abs(tg) * 100 if tg != 0 else 0
            print(f"    {ticker:<12} fees={tf:>8.2f}  gross={tg:>10.2f}  feeRatio={fr:.1f}%")

    # --- Risk Metrics ---
    print_section("10. RISK METRICS")
    if trades:
        # Max drawdown
        cumulative = 0.0
        peak = 0.0
        max_dd = 0.0
        max_dd_pct = 0.0
        for t in trades:
            cumulative += t.net_pnl
            if cumulative > peak:
                peak = cumulative
            dd = peak - cumulative
            if dd > max_dd:
                max_dd = dd
                max_dd_pct = dd / abs(peak) * 100 if peak != 0 else 0

        print(f"  Max drawdown:   {max_dd:>12.2f} RUB ({max_dd_pct:.1f}%)")

        # Consecutive losses
        max_consec_loss = 0
        current_consec = 0
        for t in trades:
            if t.net_pnl <= 0:
                current_consec += 1
                max_consec_loss = max(max_consec_loss, current_consec)
            else:
                current_consec = 0
        print(f"  Max consecutive losses: {max_consec_loss}")

        # Largest win / loss
        if trades:
            largest_win = max(trades, key=lambda t: t.net_pnl)
            largest_loss = min(trades, key=lambda t: t.net_pnl)
            print(f"  Largest win:    {largest_win.ticker} {largest_win.net_pnl:>10.2f} RUB")
            print(f"  Largest loss:   {largest_loss.ticker} {largest_loss.net_pnl:>10.2f} RUB")

    # --- Skip Analysis ---
    print_section("11. ENTRY SKIP ANALYSIS (from diagnostics)")
    total_skips = sum(s.count for s in skip_reasons.values())
    print(f"  Total skipped entries: {total_skips}")
    print(f"\n  {'Reason':<35} {'Count':>6} {'%':>6}")
    print(f"  {'-'*35} {'-'*6} {'-'*6}")
    for reason in sorted(skip_reasons.keys(), key=lambda r: skip_reasons[r].count, reverse=True):
        s = skip_reasons[reason]
        pct = s.count / total_skips * 100 if total_skips > 0 else 0
        print(f"  {reason:<35} {s.count:>6} {pct:>5.1f}%")
        # Top tickers per reason
        top_tickers = sorted(s.tickers.items(), key=lambda x: x[1], reverse=True)[:3]
        ticker_str = ", ".join(f"{t}({c})" for t, c in top_tickers)
        print(f"    top tickers: {ticker_str}")

    # --- Flow Distribution ---
    if diag_stats.get("skip_flow_values"):
        print_section("12. TRADE FLOW DISTRIBUTION (skip events)")
        flows = diag_stats["skip_flow_values"]
        pos = sum(1 for f in flows if f > 0)
        neg = sum(1 for f in flows if f < 0)
        zero = sum(1 for f in flows if f == 0)
        print(f"  Total flow samples: {len(flows)}")
        print(f"  Positive: {pos}  Negative: {neg}  Zero: {zero}")
        if flows:
            print(f"  Min: {min(flows):.1f}  Max: {max(flows):.1f}  Avg: {sum(flows)/len(flows):.2f}")

        # Flow buckets
        flow_buckets = {"0": 0, "1-5": 0, "6-10": 0, "11-20": 0, "21-50": 0, "50+": 0}
        neg_buckets = {"-1 to -5": 0, "-6 to -10": 0, "-11 to -20": 0, "-21 to -50": 0, "-50+": 0}
        for f in flows:
            af = abs(f)
            if f == 0:
                flow_buckets["0"] += 1
            elif f > 0:
                if af <= 5:
                    flow_buckets["1-5"] += 1
                elif af <= 10:
                    flow_buckets["6-10"] += 1
                elif af <= 20:
                    flow_buckets["11-20"] += 1
                elif af <= 50:
                    flow_buckets["21-50"] += 1
                else:
                    flow_buckets["50+"] += 1
            else:
                if af <= 5:
                    neg_buckets["-1 to -5"] += 1
                elif af <= 10:
                    neg_buckets["-6 to -10"] += 1
                elif af <= 20:
                    neg_buckets["-11 to -20"] += 1
                elif af <= 50:
                    neg_buckets["-21 to -50"] += 1
                else:
                    neg_buckets["-50+"] += 1

        print(f"\n  Positive flow distribution:")
        for bucket, count in flow_buckets.items():
            bar = "#" * min(count, 60)
            print(f"    {bucket:>8}: {count:>4} {bar}")
        print(f"\n  Negative flow distribution:")
        for bucket, count in neg_buckets.items():
            bar = "#" * min(count, 60)
            print(f"    {bucket:>8}: {count:>4} {bar}")

    # --- Recommendations ---
    print_section("13. KEY OBSERVATIONS & RECOMMENDATIONS")
    observations = []

    if trades:
        if total_net < 0:
            observations.append(f"STRATEGY IS UNPROFITABLE: net P&L = {total_net:.2f} RUB")

        if len(wins) > 0 and len(losses) > 0:
            avg_w = sum(t.net_pnl for t in wins) / len(wins)
            avg_l = sum(t.net_pnl for t in losses) / len(losses)
            if abs(avg_l) > abs(avg_w):
                observations.append(
                    f"Risk/reward imbalance: avg loss ({avg_l:.2f}) > avg win ({avg_w:.2f})"
                )

        if total_fees > 0 and total_gross != 0:
            fee_pct = total_fees / abs(total_gross) * 100
            if fee_pct > 30:
                observations.append(f"High fee drag: fees are {fee_pct:.1f}% of gross P&L")

        # Check for oversized positions
        for t in trades:
            notional = t.units * t.entry_price
            margin = notional * 0.25
            if margin > 100000:
                observations.append(
                    f"Oversized position: {t.ticker} {t.units} units, "
                    f"margin={margin:.0f} RUB (entry={t.entry_price})"
                )

        # Check win rate
        wr = len(wins) / len(trades) * 100
        if wr < 40:
            observations.append(f"Low win rate: {wr:.1f}% (need >50% for scalping)")

        # Check hold times
        avg_h = sum(t.hold_seconds for t in trades) / len(trades)
        if avg_h > 300:
            observations.append(f"Avg hold time {avg_h:.0f}s is high for scalping (target <120s)")

        # Per-instrument issues
        for ticker, tlist in by_ticker.items():
            tnet = sum(t.net_pnl for t in tlist)
            if tnet < -1000:
                observations.append(f"{ticker} is a big loser: {tnet:.2f} RUB from {len(tlist)} trades")

    if not observations:
        observations.append("No critical issues detected")

    for i, obs in enumerate(observations, 1):
        print(f"  {i}. {obs}")

    print(f"\n{'='*70}")
    print(f"  END OF REPORT")
    print(f"{'='*70}")


def main():
    import argparse

    parser = argparse.ArgumentParser(description="Order book strategy analytics")
    parser.add_argument(
        "--csv",
        default="analytics/orderbook-metrics.csv",
        help="Path to orderbook-metrics.csv",
    )
    parser.add_argument(
        "--log",
        default="analytics/orderbook-diagnostics-replay.log",
        help="Path to orderbook-diagnostics-replay.log",
    )
    args = parser.parse_args()

    # Resolve paths relative to project root
    project_root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    csv_path = os.path.join(project_root, args.csv)
    log_path = os.path.join(project_root, args.log)

    if not os.path.exists(csv_path):
        print(f"ERROR: CSV file not found: {csv_path}")
        sys.exit(1)
    if not os.path.exists(log_path):
        print(f"ERROR: Log file not found: {log_path}")
        sys.exit(1)

    print(f"Loading CSV:  {csv_path}")
    print(f"Loading LOG:  {log_path}")

    trades, skip_reasons, entry_events, all_events = parse_metrics_csv(csv_path)
    diag_stats = parse_diagnostics_log(log_path)

    analyze(trades, skip_reasons, entry_events, diag_stats)


if __name__ == "__main__":
    main()
