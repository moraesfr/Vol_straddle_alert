#!/usr/bin/env python3
"""
Analyze how long each stock stays below the volatility threshold.

For each stock the script:
  - Downloads the full historical data from START_DATE to today
  - Calculates the 20-day rolling volatility (same logic as run_daily_alert.py)
  - Computes the stock-specific 30th-percentile threshold
  - Identifies every continuous period when volatility is below the threshold
  - Reports per-stock statistics: avg / shortest / longest period length,
    total number of periods and overall % of trading days below threshold
  - Exports a detailed table of every period (start date, end date, duration)
    to threshold_periods_<date>.csv / threshold_periods_latest.csv and a
    summary to threshold_summary_<date>.csv / threshold_summary_latest.csv

Usage:
    python analyze_threshold_periods.py
"""

import socket
import time
from datetime import date
from pathlib import Path

import numpy as np
import pandas as pd
import yfinance as yf

# ── constants (same as run_daily_alert.py) ───────────────────────────────────
SYMBOLS = ["PETR4.SA", "VALE3.SA", "ITUB4.SA", "BBDC4.SA", "ABEV3.SA"]
START_DATE = "2023-01-02"
ROLLING_WINDOW = 20
THRESHOLD_PERCENTILE = 30
WORKDIR = Path(__file__).resolve().parent


# ── helpers ──────────────────────────────────────────────────────────────────

def calc_rolling_vol(close: pd.Series, window: int = ROLLING_WINDOW) -> pd.Series:
    """Annualised-free daily rolling std of log-returns (same as run_daily_alert.py)."""
    log_ret = np.log(close).diff()
    return log_ret.rolling(window=window, min_periods=window).std()


def download_symbol(sym: str, max_retries: int = 3) -> pd.DataFrame | None:
    """Download OHLCV data for *sym* from START_DATE to today with retries."""
    raw = None
    for attempt in range(max_retries):
        try:
            print(f"Downloading {sym} (attempt {attempt + 1}/{max_retries})...")
            raw = yf.download(sym, start=START_DATE, auto_adjust=True, progress=False)
            break
        except (socket.gaierror, OSError) as e:
            wait = min(2 ** attempt, 60)
            print(
                f"Network error downloading {sym}: {e}. "
                + (f"Retrying in {wait}s..." if attempt < max_retries - 1 else "Giving up.")
            )
            if attempt < max_retries - 1:
                time.sleep(wait)
            else:
                return None
        except Exception as e:
            print(f"Error downloading {sym}: {e}")
            return None

    if raw is None or raw.empty or len(raw) < ROLLING_WINDOW + 5:
        print(f"Insufficient data for {sym}.")
        return None

    close = raw["Close"].squeeze()
    vol = calc_rolling_vol(close)
    df = pd.DataFrame({"date": close.index, "volatility": vol.values})
    df = df.dropna(subset=["volatility"]).reset_index(drop=True)
    df["date"] = pd.to_datetime(df["date"]).dt.date
    return df


def find_below_threshold_periods(df: pd.DataFrame, thr: float) -> pd.DataFrame:
    """
    Return a DataFrame of continuous periods where volatility <= threshold.

    Columns: symbol (added by caller), start_date, end_date, duration_days
    """
    below = df["volatility"] <= thr
    periods = []
    in_period = False
    start_idx = None

    for i, flag in enumerate(below):
        if flag and not in_period:
            in_period = True
            start_idx = i
        elif not flag and in_period:
            in_period = False
            end_idx = i - 1
            periods.append(
                {
                    "start_date": df.at[start_idx, "date"],
                    "end_date": df.at[end_idx, "date"],
                    "duration_days": end_idx - start_idx + 1,
                }
            )

    # Close an open period at the last row
    if in_period:
        end_idx = len(df) - 1
        periods.append(
            {
                "start_date": df.at[start_idx, "date"],
                "end_date": df.at[end_idx, "date"],
                "duration_days": end_idx - start_idx + 1,
            }
        )

    return pd.DataFrame(periods)


def analyze_symbol(sym: str) -> tuple[dict, pd.DataFrame]:
    """
    Analyse threshold-below periods for *sym*.

    Returns
    -------
    stats : dict  – summary statistics for the stock
    periods_df : pd.DataFrame – detail of each below-threshold period
    """
    df = download_symbol(sym)
    if df is None:
        stats = {
            "symbol": sym,
            "threshold": float("nan"),
            "total_periods": 0,
            "avg_days": float("nan"),
            "min_days": float("nan"),
            "max_days": float("nan"),
            "pct_below": float("nan"),
            "status": "error",
        }
        return stats, pd.DataFrame()

    thr = float(np.percentile(df["volatility"], THRESHOLD_PERCENTILE))
    periods_df = find_below_threshold_periods(df, thr)

    total_below_days = int((df["volatility"] <= thr).sum())
    pct_below = 100.0 * total_below_days / len(df)

    if periods_df.empty:
        stats = {
            "symbol": sym,
            "threshold": round(thr, 6),
            "total_periods": 0,
            "avg_days": 0.0,
            "min_days": 0,
            "max_days": 0,
            "pct_below": round(pct_below, 2),
            "status": "ok",
        }
        return stats, pd.DataFrame()

    periods_df["symbol"] = sym
    periods_df = periods_df[["symbol", "start_date", "end_date", "duration_days"]]

    durations = periods_df["duration_days"]
    stats = {
        "symbol": sym,
        "threshold": round(thr, 6),
        "total_periods": len(durations),
        "avg_days": round(float(durations.mean()), 2),
        "min_days": int(durations.min()),
        "max_days": int(durations.max()),
        "pct_below": round(pct_below, 2),
        "status": "ok",
    }
    return stats, periods_df


# ── reporting ─────────────────────────────────────────────────────────────────

def print_summary(summary_rows: list[dict]):
    today_str = date.today().isoformat()
    print(f"\n=== BELOW-THRESHOLD PERIOD ANALYSIS ===")
    print(f"Analysis date : {today_str}")
    print(f"Start date    : {START_DATE}")
    print(f"Rolling window: {ROLLING_WINDOW} days")
    print(f"Threshold     : {THRESHOLD_PERCENTILE}th percentile (per stock)\n")

    header = (
        f"{'symbol':<12} {'threshold':>10} {'periods':>8} "
        f"{'avg_days':>9} {'min_days':>9} {'max_days':>9} {'pct_below':>10} {'status'}"
    )
    print(header)
    print("-" * len(header))
    for r in summary_rows:
        print(
            f"{r['symbol']:<12} {str(r['threshold']):>10} {str(r['total_periods']):>8} "
            f"{str(r['avg_days']):>9} {str(r['min_days']):>9} {str(r['max_days']):>9} "
            f"{str(r['pct_below']) + '%':>10} {r['status']}"
        )


def save_outputs(summary_rows: list[dict], all_periods: pd.DataFrame):
    today_str = date.today().isoformat()

    # Summary CSV
    summary_path = WORKDIR / f"threshold_summary_{today_str}.csv"
    summary_latest = WORKDIR / "threshold_summary_latest.csv"
    summary_df = pd.DataFrame(summary_rows)
    for path in (summary_path, summary_latest):
        summary_df.to_csv(path, index=False)

    # Periods detail CSV
    periods_path = WORKDIR / f"threshold_periods_{today_str}.csv"
    periods_latest = WORKDIR / "threshold_periods_latest.csv"
    for path in (periods_path, periods_latest):
        all_periods.to_csv(path, index=False)

    print(f"\nSaved files:")
    print(f"  Summary  : {summary_path.name}")
    print(f"  Summary  : {summary_latest.name}")
    print(f"  Periods  : {periods_path.name}")
    print(f"  Periods  : {periods_latest.name}")


# ── main ─────────────────────────────────────────────────────────────────────

def main():
    summary_rows = []
    all_periods_list = []

    for sym in SYMBOLS:
        stats, periods_df = analyze_symbol(sym)
        summary_rows.append(stats)
        if not periods_df.empty:
            all_periods_list.append(periods_df)

    all_periods = (
        pd.concat(all_periods_list, ignore_index=True)
        if all_periods_list
        else pd.DataFrame(columns=["symbol", "start_date", "end_date", "duration_days"])
    )

    print_summary(summary_rows)

    if not all_periods.empty:
        print("\n--- Below-Threshold Periods (all stocks) ---")
        print(all_periods.to_string(index=False))

    save_outputs(summary_rows, all_periods)


if __name__ == "__main__":
    main()
