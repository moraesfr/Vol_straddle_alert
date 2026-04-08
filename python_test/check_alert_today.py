#!/usr/bin/env python3
"""
Equivalent of check_alert_today.R
Calculates 20-day rolling volatility for 5 Brazilian stocks,
detects if vol crossed below 30th-percentile threshold today,
and saves the result as CSV.
"""

import csv
import math
import sys
from datetime import date, timedelta
from pathlib import Path

import numpy as np
import pandas as pd
import yfinance as yf

SYMBOLS = ["PETR4.SA", "VALE3.SA", "ITUB4.SA", "BBDC4.SA", "ABEV3.SA"]
START_DATE = "2023-01-02"
ROLLING_WINDOW = 20
THRESHOLD_PERCENTILE = 30
WORKDIR = Path(__file__).resolve().parent


def calc_rolling_vol(close: pd.Series, window: int = ROLLING_WINDOW) -> pd.Series:
    """Annualised-free daily rolling std of log-returns, exactly as in the R script."""
    log_ret = np.log(close).diff()
    return log_ret.rolling(window=window, min_periods=window).std()


def check_symbol(sym: str) -> dict:
    base = dict(
        symbol=sym,
        last_date="",
        last_vol=float("nan"),
        threshold=float("nan"),
        is_alert_today=False,
        below_threshold=False,
        status="ok",
    )

    try:
        raw = yf.download(sym, start=START_DATE, auto_adjust=True, progress=False)
    except Exception as e:
        base["status"] = f"download_error: {e}"
        return base

    if raw is None or len(raw) < 25:
        base["status"] = "insufficient_data"
        return base

    close = raw["Close"].squeeze()
    vol = calc_rolling_vol(close)
    df = pd.DataFrame({"date": close.index, "volatility": vol.values})
    df = df.dropna(subset=["volatility"]).reset_index(drop=True)

    if len(df) < 2:
        base["status"] = "insufficient_data"
        return base

    thr = float(np.percentile(df["volatility"], THRESHOLD_PERCENTILE))
    df["prev_vol"] = df["volatility"].shift(1)
    df["is_alert"] = (df["prev_vol"] > thr) & (df["volatility"] <= thr)

    last = df.iloc[-1]
    last_date = pd.Timestamp(last["date"]).date()
    today = date.today()

    base["last_date"] = str(last_date)
    base["last_vol"] = round(float(last["volatility"]), 6)
    base["threshold"] = round(thr, 6)
    base["is_alert_today"] = bool(last_date == today and last["is_alert"])
    base["below_threshold"] = bool(float(last["volatility"]) <= thr)
    return base


def main():
    rows = [check_symbol(sym) for sym in SYMBOLS]

    # Sort: alerts first, then alphabetical
    rows.sort(key=lambda r: (not r["is_alert_today"], r["symbol"]))

    today_str = date.today().isoformat()
    fields = ["symbol", "last_date", "last_vol", "threshold",
              "is_alert_today", "below_threshold", "status"]

    file_today = WORKDIR / f"daily_alert_check_{today_str}.csv"
    file_latest = WORKDIR / "daily_alert_check_latest.csv"

    for path in (file_today, file_latest):
        with path.open("w", newline="", encoding="utf-8") as f:
            w = csv.DictWriter(f, fieldnames=fields)
            w.writeheader()
            w.writerows(rows)

    # Print summary (same format as R script)
    print(f"\n=== DAILY ALERT CHECK ===")
    print(f"Date: {today_str}\n")

    header = f"{'symbol':<12} {'last_date':<12} {'last_vol':<10} {'threshold':<12} {'is_alert_today':<16} {'below_threshold':<17} status"
    print(header)
    print("-" * len(header))
    for r in rows:
        print(f"{r['symbol']:<12} {r['last_date']:<12} {str(r['last_vol']):<10} "
              f"{str(r['threshold']):<12} {str(r['is_alert_today']):<16} "
              f"{str(r['below_threshold']):<17} {r['status']}")

    alerts = [r for r in rows if r["is_alert_today"]]
    below = [r for r in rows if r["below_threshold"]]

    print("\nAlerts today (strict crossing on today):")
    print("  None" if not alerts else "\n".join(
        f"  {r['symbol']} em {r['last_date']} (vol={r['last_vol']}, thr={r['threshold']})"
        for r in alerts
    ))

    print("\nNames currently below threshold (state):")
    print("  None" if not below else "\n".join(
        f"  {r['symbol']} (vol={r['last_vol']}, thr={r['threshold']})" for r in below
    ))

    print(f"\nSaved files:")
    print(f"  - {file_today.name}")
    print(f"  - {file_latest.name}")


if __name__ == "__main__":
    main()
