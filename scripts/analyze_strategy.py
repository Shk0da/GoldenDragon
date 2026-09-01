#!/usr/bin/env python3
"""
Order book strategy analytics script.

Parses orderbook-metrics.csv and orderbook-diagnostics-replay.log
to produce a comprehensive trading performance report.

Usage:
    python scripts/analyze_strategy.py
    python scripts/analyze_strategy.py --csv analytics/orderbook-metrics.csv --log analytics/orderbook-diagnostics-replay.log
"""

from __future__ import annotations

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
    # Enhanced metrics
    regime: str = ""
    atr: float = 0.0
    adx: float = 0.0
    vpin: float = 0.0
    vwap: float = 0.0
    poc: float = 0.0
    block_trade_count: int = 0
    avg_fill_rate: float = 0.0
    eaten_ratio: float = 0.0
    avg_slippage: float = 0.0
    max_slippage: float = 0.0
    signal_win_rate: float = 0.0
    adjusted_min_delta: float = 0.0
    dynamic_tp_price: float = 0.0


@dataclass
class SkipStats:
    reason: str
    count: int = 0
    tickers: dict = field(default_factory=lambda: defaultdict(int))


@dataclass
class DensityScalpSkip:
    """DensityScalp signal skip event with detailed metrics."""
    timestamp: str
    ticker: str
    skip_reason: str
    trend: str = ""
    level_strength: float = 0.0
    compression_strength: float = 0.0
    impulse_strength: float = 0.0
    cluster_count: int = 0
    spread_bps: float = 0.0
    obi: float = 0.0


def parse_metrics_csv(path: str) -> tuple:
    """Parse orderbook-metrics.csv into trades and skip events."""
    trades = []
    skip_reasons = defaultdict(lambda: SkipStats(""))
    entry_events = []
    all_events = []
    density_scalp_skips = []

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
                
                # Parse densityScalp specific metrics
                signal_id = row.get("signalId", "")
                if signal_id == "densityScalp" or reason.startswith("densityScalp_"):
                    skip = DensityScalpSkip(
                        timestamp=timestamp,
                        ticker=ticker,
                        skip_reason=row.get("skipReason", reason.replace("densityScalp_", "")),
                        trend=row.get("trend", ""),
                        level_strength=_float_or(row.get("levelStrength"), 0.0),
                        compression_strength=_float_or(row.get("compressionStrength"), 0.0),
                        impulse_strength=_float_or(row.get("impulseStrength"), 0.0),
                        cluster_count=_int_or(row.get("clusterCount"), 0),
                        spread_bps=_float_or(row.get("spreadBps"), 0.0),
                        obi=_float_or(row.get("obi"), 0.0),
                    )
                    density_scalp_skips.append(skip)

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

                # Find matching entry for quality/signal and enhanced metrics
                quality = 0.0
                signal = ""
                entry_time = ""
                regime = ""
                atr = 0.0
                adx = 0.0
                vpin = 0.0
                vwap = 0.0
                poc = 0.0
                block_trade_count = 0
                avg_fill_rate = 0.0
                eaten_ratio = 0.0
                avg_slippage = 0.0
                max_slippage = 0.0
                signal_win_rate = 0.0
                adjusted_min_delta = 0.0
                dynamic_tp_price = 0.0
                for entry in reversed(entry_events):
                    if entry.get("ticker") == ticker:
                        quality = _float_or(entry.get("quality"), 0.0)
                        signal = entry.get("reason", "")
                        entry_time = entry.get("timestamp", "")
                        regime = entry.get("regime", "")
                        atr = _float_or(entry.get("atr"), 0.0)
                        adx = _float_or(entry.get("adx"), 0.0)
                        vpin = _float_or(entry.get("vpin"), 0.0)
                        vwap = _float_or(entry.get("vwap"), 0.0)
                        poc = _float_or(entry.get("poc"), 0.0)
                        block_trade_count = _int_or(entry.get("blockTradeCount"), 0)
                        avg_fill_rate = _float_or(entry.get("avgFillRate"), 0.0)
                        eaten_ratio = _float_or(entry.get("eatenRatio"), 0.0)
                        avg_slippage = _float_or(entry.get("avgSlippage"), 0.0)
                        max_slippage = _float_or(entry.get("maxSlippage"), 0.0)
                        signal_win_rate = _float_or(entry.get("signalWinRate"), 0.0)
                        adjusted_min_delta = _float_or(entry.get("adjustedMinDelta"), 0.0)
                        dynamic_tp_price = _float_or(entry.get("dynamicTpPrice"), 0.0)
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
                    regime=regime,
                    atr=atr,
                    adx=adx,
                    vpin=vpin,
                    vwap=vwap,
                    poc=poc,
                    block_trade_count=block_trade_count,
                    avg_fill_rate=avg_fill_rate,
                    eaten_ratio=eaten_ratio,
                    avg_slippage=avg_slippage,
                    max_slippage=max_slippage,
                    signal_win_rate=signal_win_rate,
                    adjusted_min_delta=adjusted_min_delta,
                    dynamic_tp_price=dynamic_tp_price,
                )
                trades.append(trade)

    return trades, dict(skip_reasons), entry_events, all_events, density_scalp_skips


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


def analyze(trades: list, skip_reasons: dict, entry_events: list, diag_stats: dict, density_scalp_skips: list = None, args=None):
    """Print comprehensive analytics report."""
    if density_scalp_skips is None:
        density_scalp_skips = []
    
    # For hourly analysis access
    all_events_for_hourly = entry_events  # Reuse entry_events as placeholder

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

    # --- DensityScalp Signal Analysis ---
    if density_scalp_skips:
        print_section("13. DENSITYSCALP SIGNAL ANALYSIS")
        print(f"  Total densityScalp skip events: {len(density_scalp_skips)}")
        
        # Skip reasons breakdown
        skip_reason_counts = defaultdict(int)
        skip_reason_by_ticker = defaultdict(lambda: defaultdict(int))
        for skip in density_scalp_skips:
            skip_reason_counts[skip.skip_reason] += 1
            skip_reason_by_ticker[skip.skip_reason][skip.ticker] += 1
        
        print_subsection("Skip reasons")
        print(f"  {'Reason':<25} {'Count':>6}  %")
        print(f"  {'-'*25} {'-'*6}  {'-'*5}")
        total_skips = len(density_scalp_skips)
        for reason, count in sorted(skip_reason_counts.items(), key=lambda x: -x[1]):
            pct = count / total_skips * 100
            print(f"  {reason:<25} {count:>6}  {pct:>5.1f}%")
            # Top tickers for this reason
            ticker_counts = skip_reason_by_ticker[reason]
            top_tickers = sorted(ticker_counts.items(), key=lambda x: -x[1])[:3]
            if top_tickers:
                ticker_str = ", ".join(f"{t}({c})" for t, c in top_tickers)
                print(f"    top tickers: {ticker_str}")
        
        # Average metrics by skip reason
        print_subsection("Average metrics by skip reason")
        metrics_by_reason = defaultdict(lambda: {
            "level_strength": [], "compression_strength": [], 
            "impulse_strength": [], "cluster_count": [], "spread_bps": []
        })
        for skip in density_scalp_skips:
            metrics_by_reason[skip.skip_reason]["level_strength"].append(skip.level_strength)
            metrics_by_reason[skip.skip_reason]["compression_strength"].append(skip.compression_strength)
            metrics_by_reason[skip.skip_reason]["impulse_strength"].append(skip.impulse_strength)
            metrics_by_reason[skip.skip_reason]["cluster_count"].append(skip.cluster_count)
            metrics_by_reason[skip.skip_reason]["spread_bps"].append(skip.spread_bps)
        
        print(f"  {'Reason':<25} {'AvgLvl':>7} {'AvgCmpr':>8} {'AvgImp':>7} {'AvgClust':>9} {'AvgSpread':>10}")
        print(f"  {'-'*25} {'-'*7} {'-'*8} {'-'*7} {'-'*9} {'-'*10}")
        for reason, metrics in sorted(metrics_by_reason.items()):
            avg_lvl = sum(metrics["level_strength"]) / len(metrics["level_strength"]) if metrics["level_strength"] else 0
            avg_cmpr = sum(metrics["compression_strength"]) / len(metrics["compression_strength"]) if metrics["compression_strength"] else 0
            avg_imp = sum(metrics["impulse_strength"]) / len(metrics["impulse_strength"]) if metrics["impulse_strength"] else 0
            avg_clust = sum(metrics["cluster_count"]) / len(metrics["cluster_count"]) if metrics["cluster_count"] else 0
            avg_spread = sum(metrics["spread_bps"]) / len(metrics["spread_bps"]) if metrics["spread_bps"] else 0
            print(f"  {reason:<25} {avg_lvl:>7.2f} {avg_cmpr:>8.2f} {avg_imp:>7.2f} {avg_clust:>9.1f} {avg_spread:>9.1f}bps")
        
        # Trend distribution
        print_subsection("Trend distribution")
        trend_counts = defaultdict(int)
        for skip in density_scalp_skips:
            trend_counts[skip.trend] += 1
        for trend, count in sorted(trend_counts.items(), key=lambda x: -x[1]):
            pct = count / total_skips * 100
            print(f"  {trend:<15} {count:>6}  {pct:>5.1f}%")

    # --- Enhanced Metrics Analysis ---
    section_num = 14 if density_scalp_skips else 13
    print_section(f"{section_num}. ENHANCED METRICS ANALYSIS")

    # Market regime distribution
    print_subsection("Market Regime Distribution")
    regime_counts = defaultdict(lambda: {"count": 0, "pnl": 0.0, "atr_sum": 0.0, "adx_sum": 0.0})
    for t in trades:
        if t.regime:
            regime_counts[t.regime]["count"] += 1
            regime_counts[t.regime]["pnl"] += t.net_pnl
            regime_counts[t.regime]["atr_sum"] += t.atr
            regime_counts[t.regime]["adx_sum"] += t.adx
    if regime_counts:
        print(f"  {'Regime':<12} {'Trades':>7} {'Net P&L':>12} {'Avg P&L':>10} {'Avg ATR':>8} {'Avg ADX':>8}")
        print(f"  {'-'*12} {'-'*7} {'-'*12} {'-'*10} {'-'*8} {'-'*8}")
        for regime, data in sorted(regime_counts.items(), key=lambda x: -x[1]["count"]):
            avg = data["pnl"] / data["count"] if data["count"] else 0
            avg_atr = data["atr_sum"] / data["count"] if data["count"] else 0
            avg_adx = data["adx_sum"] / data["count"] if data["count"] else 0
            print(f"  {regime:<12} {data['count']:>7} {data['pnl']:>12.2f} {avg:>10.2f} {avg_atr:>8.4f} {avg_adx:>8.1f}")
    else:
        print("  No regime data available")

    # VPIN (toxicity) analysis
    print_subsection("VPIN (Order Flow Toxicity)")
    vpin_values = [t.vpin for t in trades if t.vpin > 0]
    if vpin_values:
        avg_vpin = sum(vpin_values) / len(vpin_values)
        max_vpin = max(vpin_values)
        # Correlate VPIN with P&L
        high_toxicity = [t for t in trades if t.vpin > 0.6]
        low_toxicity = [t for t in trades if t.vpin <= 0.6 and t.vpin > 0]
        print(f"  Avg VPIN: {avg_vpin:.3f}")
        print(f"  Max VPIN: {max_vpin:.3f}")
        print(f"  High toxicity (>0.6): {len(high_toxicity)} trades, P&L={sum(t.net_pnl for t in high_toxicity):.2f}")
        print(f"  Low toxicity (<=0.6): {len(low_toxicity)} trades, P&L={sum(t.net_pnl for t in low_toxicity):.2f}")
    else:
        print("  No VPIN data available")

    # Volume profile analysis
    print_subsection("Volume Profile (VWAP/POC)")
    vwap_trades = [t for t in trades if t.vwap > 0]
    if vwap_trades:
        # Check if entries are near VWAP
        near_vwap = sum(1 for t in vwap_trades if abs(t.entry_price - t.vwap) / t.vwap < 0.005)
        print(f"  Trades with VWAP data: {len(vwap_trades)}")
        print(f"  Entries within 0.5% of VWAP: {near_vwap} ({near_vwap/len(vwap_trades)*100:.1f}%)")
        # POC analysis
        poc_trades = [t for t in trades if t.poc > 0]
        if poc_trades:
            near_poc = sum(1 for t in poc_trades if abs(t.entry_price - t.poc) / t.poc < 0.005)
            print(f"  Trades with POC data: {len(poc_trades)}")
            print(f"  Entries within 0.5% of POC: {near_poc} ({near_poc/len(poc_trades)*100:.1f}%)")
    else:
        print("  No volume profile data available")

    # Block trades analysis
    print_subsection("Block Trades (Tape Reading)")
    block_trades = [t for t in trades if t.block_trade_count > 0]
    if block_trades:
        avg_blocks = sum(t.block_trade_count for t in block_trades) / len(block_trades)
        block_pnl = sum(t.net_pnl for t in block_trades)
        print(f"  Trades with block activity: {len(block_trades)}")
        print(f"  Avg block trades per entry: {avg_blocks:.1f}")
        print(f"  P&L from block trade entries: {block_pnl:.2f}")
    else:
        print("  No block trade data available")

    # Queue dynamics analysis
    print_subsection("Queue Dynamics (Fill Rate)")
    fill_rate_trades = [t for t in trades if t.avg_fill_rate > 0]
    if fill_rate_trades:
        avg_fill = sum(t.avg_fill_rate for t in fill_rate_trades) / len(fill_rate_trades)
        avg_eaten = sum(t.eaten_ratio for t in fill_rate_trades) / len(fill_rate_trades)
        print(f"  Trades with fill rate data: {len(fill_rate_trades)}")
        print(f"  Avg fill rate: {avg_fill:.2f} units/sec")
        print(f"  Avg eaten ratio: {avg_eaten:.2%}")
    else:
        print("  No queue dynamics data available")

    # Slippage analysis
    print_subsection("Execution Quality (Slippage)")
    slippage_trades = [t for t in trades if t.avg_slippage != 0]
    if slippage_trades:
        avg_slip = sum(abs(t.avg_slippage) for t in slippage_trades) / len(slippage_trades)
        max_slip = max(abs(t.max_slippage) for t in slippage_trades)
        print(f"  Trades with slippage data: {len(slippage_trades)}")
        print(f"  Avg absolute slippage: {avg_slip:.4f}")
        print(f"  Max absolute slippage: {max_slip:.4f}")
        # Slippage impact on P&L
        high_slip = [t for t in slippage_trades if abs(t.avg_slippage) > 0.01]
        if high_slip:
            print(f"  High slippage (>0.01) trades: {len(high_slip)}, P&L={sum(t.net_pnl for t in high_slip):.2f}")
    else:
        print("  No slippage data available")

    # Signal performance tracking
    print_subsection("Signal Performance (Adaptive)")
    signal_perf = [t for t in trades if t.signal_win_rate > 0]
    if signal_perf:
        print(f"  {'Signal':<15} {'Trades':>7} {'Avg WR':>7} {'Net P&L':>12}")
        print(f"  {'-'*15} {'-'*7} {'-'*7} {'-'*12}")
        by_signal_wr = defaultdict(lambda: {"count": 0, "wr_sum": 0.0, "pnl": 0.0})
        for t in signal_perf:
            by_signal_wr[t.signal]["count"] += 1
            by_signal_wr[t.signal]["wr_sum"] += t.signal_win_rate
            by_signal_wr[t.signal]["pnl"] += t.net_pnl
        for signal, data in sorted(by_signal_wr.items(), key=lambda x: -x[1]["pnl"]):
            # signalWinRate is stored as a percentage (0-100), not a fraction
            avg_wr = data["wr_sum"] / data["count"] if data["count"] else 0
            print(f"  {signal:<15} {data['count']:>7} {avg_wr:>6.1f}% {data['pnl']:>12.2f}")
    else:
        print("  No signal performance data available")

    # Dynamic TP analysis
    print_subsection("Dynamic Take Profit")
    dynamic_tp_trades = [t for t in trades if t.dynamic_tp_price > 0]
    if dynamic_tp_trades:
        # Analyze if exits hit dynamic TP
        hit_dynamic_tp = sum(1 for t in dynamic_tp_trades 
                            if t.exit_price and abs(t.exit_price - t.dynamic_tp_price) / t.dynamic_tp_price < 0.002)
        print(f"  Trades with dynamic TP: {len(dynamic_tp_trades)}")
        print(f"  Exits near dynamic TP (within 0.2%): {hit_dynamic_tp} ({hit_dynamic_tp/len(dynamic_tp_trades)*100:.1f}%)")
        # P&L comparison
        dynamic_pnl = sum(t.net_pnl for t in dynamic_tp_trades)
        print(f"  P&L from dynamic TP trades: {dynamic_pnl:.2f}")
    else:
        print("  No dynamic TP data available")

    # --- Recommendations ---
    section_num += 1
    print_section(f"{section_num}. KEY OBSERVATIONS & RECOMMENDATIONS")
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

    # --- Hourly Analysis (if requested) ---
    if args and args.by_hour:
        print_hourly_analysis(trades, all_events_for_hourly)


def print_hourly_analysis(trades: list, skip_events: list):
    """Print hourly performance breakdown."""
    print_section("HOURLY PERFORMANCE ANALYSIS")
    
    from collections import defaultdict
    from datetime import datetime
    
    hourly_stats = defaultdict(lambda: {"trades": 0, "pnl": 0.0, "wins": 0, "losses": 0, "skips": 0})
    
    # Parse trades by hour
    for t in trades:
        try:
            dt = datetime.strptime(t.entry_time, "%Y-%m-%d %H:%M:%S")
            hour = dt.hour
            hourly_stats[hour]["trades"] += 1
            hourly_stats[hour]["pnl"] += t.net_pnl
            if t.net_pnl > 0:
                hourly_stats[hour]["wins"] += 1
            else:
                hourly_stats[hour]["losses"] += 1
        except (ValueError, TypeError):
            pass
    
    # Parse skips by hour (skip_events is actually all_events list)
    for event in skip_events:
        if isinstance(event, dict) and event.get("type") == "ENTRY_SKIPPED":
            try:
                timestamp = event.get("timestamp", "")
                dt = datetime.strptime(timestamp, "%Y-%m-%d %H:%M:%S")
                hour = dt.hour
                hourly_stats[hour]["skips"] += 1
            except (ValueError, TypeError):
                pass
    
    if not hourly_stats:
        print("  No hourly data available")
        return
    
    print(f"\n  {'Hour':<8} {'Trades':>7} {'Wins':>5} {'Losses':>8} {'WinR%':>7} {'Net P&L':>12} {'Avg P&L':>10} {'Skips':>8}")
    print(f"  {'-'*8} {'-'*7} {'-'*5} {'-'*8} {'-'*7} {'-'*12} {'-'*10} {'-'*8}")
    
    for hour in sorted(hourly_stats.keys()):
        stats = hourly_stats[hour]
        win_rate = stats["wins"] / stats["trades"] * 100 if stats["trades"] > 0 else 0
        avg_pnl = stats["pnl"] / stats["trades"] if stats["trades"] > 0 else 0
        print(f"  {hour:02d}:00    {stats['trades']:>7} {stats['wins']:>5} {stats['losses']:>8} {win_rate:>6.1f}% {stats['pnl']:>12.2f} {avg_pnl:>10.2f} {stats['skips']:>8}")
    
    # Find best/worst hours
    if hourly_stats:
        best_hour = max(hourly_stats.items(), key=lambda x: x[1]["pnl"])
        worst_hour = min(hourly_stats.items(), key=lambda x: x[1]["pnl"])
        
        print_subsection("Key Insights")
        print(f"  Best hour:  {best_hour[0]:02d}:00 ({best_hour[1]['pnl']:.2f} RUB, {best_hour[1]['trades']} trades)")
        print(f"  Worst hour: {worst_hour[0]:02d}:00 ({worst_hour[1]['pnl']:.2f} RUB, {worst_hour[1]['trades']} trades)")
        
        # Recommendation
        profitable_hours = [h for h, s in hourly_stats.items() if s["pnl"] > 0]
        unprofitable_hours = [h for h, s in hourly_stats.items() if s["pnl"] <= 0 and s["trades"] > 0]
        
        if profitable_hours:
            print(f"\n  Recommendation: Trade during hours {min(profitable_hours):02d}:00-{max(profitable_hours):02d}:00")
        if unprofitable_hours:
            print(f"  Avoid: {', '.join(f'{h:02d}:00' for h in sorted(unprofitable_hours))}")


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
    parser.add_argument(
        "--by-hour",
        action="store_true",
        help="Show hourly performance breakdown",
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

    trades, skip_reasons, entry_events, all_events, density_scalp_skips = parse_metrics_csv(csv_path)
    diag_stats = parse_diagnostics_log(log_path)

    analyze(trades, skip_reasons, entry_events, diag_stats, density_scalp_skips, args)
    
    # Hourly analysis if requested
    if args and args.by_hour:
        print_hourly_analysis(trades, all_events)


if __name__ == "__main__":
    main()
