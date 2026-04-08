#!/usr/bin/env python3
"""
Full pipeline: check alerts and send email.
Equivalent of run_daily_alert_and_email.sh + check_alert_today.R + send_daily_alert_email.py
in a single Python script with no R dependency.
"""

import csv
import math
import os
import smtplib
import sys
from datetime import date
from email.message import EmailMessage
from pathlib import Path

import numpy as np
import pandas as pd
import yfinance as yf

SYMBOLS = ["PETR4.SA", "VALE3.SA", "ITUB4.SA", "BBDC4.SA", "ABEV3.SA"]
START_DATE = "2023-01-02"
ROLLING_WINDOW = 20
THRESHOLD_PERCENTILE = 30
WORKDIR = Path(__file__).resolve().parent


# ── helpers ──────────────────────────────────────────────────────────────────

def calc_rolling_vol(close: pd.Series, window: int = ROLLING_WINDOW) -> pd.Series:
    log_ret = np.log(close).diff()
    return log_ret.rolling(window=window, min_periods=window).std()


def check_symbol(sym: str) -> dict:
    base = dict(symbol=sym, last_date="", last_vol=float("nan"),
                threshold=float("nan"), is_alert_today=False,
                below_threshold=False, status="ok")
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

    base["last_date"] = str(last_date)
    base["last_vol"] = round(float(last["volatility"]), 6)
    base["threshold"] = round(thr, 6)
    base["is_alert_today"] = bool(last_date == date.today() and last["is_alert"])
    base["below_threshold"] = bool(float(last["volatility"]) <= thr)
    return base


def save_csv(rows: list, fields: list) -> Path:
    today_str = date.today().isoformat()
    file_today = WORKDIR / f"daily_alert_check_{today_str}.csv"
    file_latest = WORKDIR / "daily_alert_check_latest.csv"
    for path in (file_today, file_latest):
        with path.open("w", newline="", encoding="utf-8") as f:
            w = csv.DictWriter(f, fieldnames=fields)
            w.writeheader()
            w.writerows(rows)
    return file_latest


def print_summary(rows: list, today_str: str):
    print(f"\n=== DAILY ALERT CHECK ===\nDate: {today_str}\n")
    hdr = f"{'symbol':<12} {'last_date':<12} {'last_vol':<10} {'threshold':<12} {'is_alert_today':<16} {'below_threshold':<17} status"
    print(hdr)
    print("-" * len(hdr))
    for r in rows:
        print(f"{r['symbol']:<12} {r['last_date']:<12} {str(r['last_vol']):<10} "
              f"{str(r['threshold']):<12} {str(r['is_alert_today']):<16} "
              f"{str(r['below_threshold']):<17} {r['status']}")

    alerts = [r for r in rows if r["is_alert_today"]]
    below = [r for r in rows if r["below_threshold"]]

    print("\nAlerts today (strict crossing on today):")
    print("  None" if not alerts else "\n".join(
        f"  {r['symbol']} em {r['last_date']} (vol={r['last_vol']}, thr={r['threshold']})"
        for r in alerts))

    print("\nNames currently below threshold (state):")
    print("  None" if not below else "\n".join(
        f"  {r['symbol']} (vol={r['last_vol']}, thr={r['threshold']})"
        for r in below))


def build_email_body(rows: list, today_str: str) -> tuple[str, str]:
    alerts = [r for r in rows if r["is_alert_today"]]
    below = [r for r in rows if r["below_threshold"]]
    lines = [
        f"Relatorio diario de alertas - {today_str}", "",
        "Resumo geral:",
        f"- Acoes monitoradas: {len(rows)}",
        f"- Alertas hoje (cruzamento estrito): {len(alerts)}",
        f"- Acoes abaixo do threshold (estado): {len(below)}", "",
        "Tabela completa:",
        "symbol | last_date | last_vol | threshold | is_alert_today | below_threshold | status",
        "-" * 80,
    ]
    for r in rows:
        lines.append(
            f"{r['symbol']} | {r['last_date']} | {r['last_vol']} | "
            f"{r['threshold']} | {r['is_alert_today']} | {r['below_threshold']} | {r['status']}"
        )
    lines += [""]
    if alerts:
        lines.append("Alertas hoje:")
        for r in alerts:
            lines.append(f"- {r['symbol']} em {r['last_date']} (vol={r['last_vol']}, thr={r['threshold']})")
    else:
        lines.append("Alertas hoje: nenhum")

    prefix = "ALERTA" if alerts else "SEM ALERTA"
    subject = f"[{prefix}] Check diario de volatilidade - {today_str}"
    return subject, "\n".join(lines)


def get_env(name: str, default: str = None) -> str:
    val = os.getenv(name, default)
    if val is None:
        raise RuntimeError(f"Variavel de ambiente obrigatoria ausente: {name}")
    return val


def send_email(subject: str, body: str, attachment: Path):
    smtp_host = get_env("ALERT_SMTP_HOST")
    smtp_port = int(get_env("ALERT_SMTP_PORT", "587"))
    smtp_user = get_env("ALERT_SMTP_USER")
    smtp_pass = get_env("ALERT_SMTP_PASS")
    smtp_from = get_env("ALERT_EMAIL_FROM")
    smtp_to = get_env("ALERT_EMAIL_TO")
    use_tls = get_env("ALERT_SMTP_USE_TLS", "true").lower() == "true"

    recipients = [x.strip() for x in smtp_to.split(",") if x.strip()]
    msg = EmailMessage()
    msg["Subject"] = subject
    msg["From"] = smtp_from
    msg["To"] = ", ".join(recipients)
    msg.set_content(body)
    msg.add_attachment(attachment.read_bytes(), maintype="text", subtype="csv",
                       filename=attachment.name)

    with smtplib.SMTP(smtp_host, smtp_port, timeout=30) as server:
        if use_tls:
            server.starttls()
        server.login(smtp_user, smtp_pass)
        server.send_message(msg)


# ── main ─────────────────────────────────────────────────────────────────────

def main():
    dry_run = "--dry-run" in sys.argv
    today_str = date.today().isoformat()
    fields = ["symbol", "last_date", "last_vol", "threshold",
              "is_alert_today", "below_threshold", "status"]

    rows = [check_symbol(sym) for sym in SYMBOLS]
    rows.sort(key=lambda r: (not r["is_alert_today"], r["symbol"]))

    print_summary(rows, today_str)

    latest_csv = save_csv(rows, fields)
    print(f"\nSaved files:\n  - daily_alert_check_{today_str}.csv\n  - daily_alert_check_latest.csv")

    subject, body = build_email_body(rows, today_str)

    if dry_run:
        print("\n--- DRY RUN ---")
        print(subject)
        print(body)
        return

    send_email(subject, body, latest_csv)
    print("Email enviado com sucesso.")


if __name__ == "__main__":
    main()
