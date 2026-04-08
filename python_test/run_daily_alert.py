#!/usr/bin/env python3
"""
Full pipeline: check alerts and send email.
Equivalent of run_daily_alert_and_email.sh + check_alert_today.R + send_daily_alert_email.py
in a single Python script with no R dependency.
"""

import base64
import csv
import math
import os
import smtplib
import socket
import sys
import time
from datetime import date
from email.message import EmailMessage
from io import BytesIO
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.dates as mdates
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import yfinance as yf

SYMBOLS = ["PETR4.SA", "VALE3.SA", "ITUB4.SA", "BBDC4.SA", "ABEV3.SA"]
START_DATE = "2023-01-02"
ROLLING_WINDOW = 20
THRESHOLD_PERCENTILE = 30
PLOT_DAYS = 90
WORKDIR = Path(__file__).resolve().parent


# ── helpers ──────────────────────────────────────────────────────────────────

def calc_rolling_vol(close: pd.Series, window: int = ROLLING_WINDOW) -> pd.Series:
    log_ret = np.log(close).diff()
    return log_ret.rolling(window=window, min_periods=window).std()


def check_symbol(sym: str, max_retries: int = 3) -> dict:
    base = dict(symbol=sym, last_date="", last_vol=float("nan"),
                threshold=float("nan"), is_alert_today=False,
                below_threshold=False, status="ok", vol_history=None)
    raw = None
    for attempt in range(max_retries):
        try:
            print(f"Downloading {sym} (attempt {attempt + 1}/{max_retries})...")
            raw = yf.download(sym, start=START_DATE, auto_adjust=True, progress=False)
            break
        except (socket.gaierror, OSError) as e:
            wait = min(2 ** attempt, 60)
            print(f"Network error downloading {sym}: {e}. "
                  f"{'Retrying in ' + str(wait) + 's...' if attempt < max_retries - 1 else 'Giving up.'}")
            if attempt < max_retries - 1:
                time.sleep(wait)
            else:
                base["status"] = f"download_error: {e}"
                return base
        except Exception as e:
            base["status"] = f"download_error: {e}"
            return base

    if raw is None or raw.empty or len(raw) < 25:
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
    base["vol_history"] = df[["date", "volatility"]].copy()
    return base


def save_csv(rows: list, fields: list) -> Path:
    today_str = date.today().isoformat()
    file_today = WORKDIR / f"daily_alert_check_{today_str}.csv"
    file_latest = WORKDIR / "daily_alert_check_latest.csv"
    for path in (file_today, file_latest):
        with path.open("w", newline="", encoding="utf-8") as f:
            w = csv.DictWriter(f, fieldnames=fields, extrasaction="ignore")
            w.writeheader()
            w.writerows(rows)
    return file_latest


def generate_volatility_plot(sym: str, vol_df: pd.DataFrame, thr: float) -> str:
    """Generate a 90-day rolling volatility plot and return as a base64-encoded PNG string."""
    df90 = vol_df.tail(PLOT_DAYS).copy()
    dates = pd.to_datetime(df90["date"])
    vols = df90["volatility"].values

    fig, ax = plt.subplots(figsize=(10, 4))
    ax.plot(dates, vols, color="steelblue", linewidth=1.5, label=f"Volatilidade ({ROLLING_WINDOW}d)")
    ax.axhline(y=thr, color="red", linestyle="--", linewidth=1.2,
               label=f"Threshold (p{THRESHOLD_PERCENTILE} = {thr:.4f})")
    ax.plot(dates.iloc[-1], vols[-1], marker="*", color="crimson", markersize=14,
            zorder=5, label=f"Atual: {vols[-1]:.4f}")
    ax.fill_between(dates, 0, thr, alpha=0.07, color="red")
    ax.xaxis.set_major_formatter(mdates.DateFormatter("%d/%b"))
    ax.xaxis.set_major_locator(mdates.WeekdayLocator(interval=2))
    fig.autofmt_xdate()
    ax.set_title(f"{sym} — Volatilidade Rolling {ROLLING_WINDOW}d (últimos {PLOT_DAYS} dias)")
    ax.set_xlabel("Data")
    ax.set_ylabel("Volatilidade")
    ax.legend(fontsize=9)
    ax.grid(True, alpha=0.3)
    plt.tight_layout()

    buf = BytesIO()
    fig.savefig(buf, format="png", dpi=100, bbox_inches="tight")
    plt.close(fig)
    buf.seek(0)
    return base64.b64encode(buf.read()).decode("ascii")


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


def build_email_body(rows: list, today_str: str) -> tuple[str, str, str]:
    """Return (subject, plain_text, html_body)."""
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
    plain_text = "\n".join(lines)

    # ── Build HTML body ──────────────────────────────────────────────────────
    table_rows_html = ""
    for r in rows:
        row_style = ""
        if r["is_alert_today"]:
            row_style = ' style="background:#fff3cd"'
        elif r["below_threshold"]:
            row_style = ' style="background:#d4edda"'
        table_rows_html += (
            f"<tr{row_style}>"
            f"<td>{r['symbol']}</td>"
            f"<td>{r['last_date']}</td>"
            f"<td>{r['last_vol']}</td>"
            f"<td>{r['threshold']}</td>"
            f"<td>{'&#10003;' if r['is_alert_today'] else '&mdash;'}</td>"
            f"<td>{'&#10003;' if r['below_threshold'] else '&mdash;'}</td>"
            f"<td>{r['status']}</td>"
            f"</tr>\n"
        )

    plots_html = ""
    for r in rows:
        vol_df = r.get("vol_history")
        thr = r.get("threshold", float("nan"))
        if vol_df is not None and not math.isnan(thr) and len(vol_df) > 0:
            try:
                b64 = generate_volatility_plot(r["symbol"], vol_df, thr)
                plots_html += (
                    f'<div style="margin:20px 0">'
                    f'<img src="data:image/png;base64,{b64}" '
                    f'style="max-width:100%;border:1px solid #ddd;border-radius:4px" '
                    f'alt="{r["symbol"]} volatility plot">'
                    f'</div>\n'
                )
            except Exception as exc:
                plots_html += f"<p><em>Erro ao gerar gráfico para {r['symbol']}: {exc}</em></p>\n"

    alert_summary_html = ""
    if alerts:
        alert_summary_html = "<ul>\n" + "".join(
            f"<li><strong>{r['symbol']}</strong> em {r['last_date']} "
            f"(vol={r['last_vol']}, thr={r['threshold']})</li>\n"
            for r in alerts
        ) + "</ul>"
    else:
        alert_summary_html = "<p>Nenhum alerta hoje.</p>"

    html_body = f"""<!DOCTYPE html>
<html>
<head>
<meta charset="utf-8">
<style>
  body {{font-family: Arial, sans-serif; max-width: 960px; margin: 0 auto; padding: 16px; color: #333;}}
  h1 {{color: #2c3e50;}}
  h2 {{color: #34495e; border-bottom: 1px solid #eee; padding-bottom: 4px;}}
  table {{border-collapse: collapse; width: 100%; margin-bottom: 16px;}}
  th, td {{border: 1px solid #ddd; padding: 8px 10px; text-align: left; font-size: 13px;}}
  th {{background: #f2f2f2; font-weight: bold;}}
</style>
</head>
<body>
<h1>Relatório Diário de Alertas &mdash; {today_str}</h1>
<p>
  <strong>Ações monitoradas:</strong> {len(rows)} &nbsp;|&nbsp;
  <strong>Alertas hoje:</strong> {len(alerts)} &nbsp;|&nbsp;
  <strong>Abaixo do threshold:</strong> {len(below)}
</p>

<h2>Alertas de Hoje</h2>
{alert_summary_html}

<h2>Tabela de Status</h2>
<table>
  <tr>
    <th>Symbol</th><th>Data</th><th>Vol Atual</th><th>Threshold</th>
    <th>Alerta Hoje</th><th>Abaixo Thr</th><th>Status</th>
  </tr>
  {table_rows_html}
</table>

<h2>Gráficos de Volatilidade (últimos {PLOT_DAYS} dias)</h2>
{plots_html}
</body>
</html>"""

    return subject, plain_text, html_body


def get_env(name: str, default: str = None) -> str:
    val = os.getenv(name, default)
    if val is None:
        raise RuntimeError(f"Variavel de ambiente obrigatoria ausente: {name}")
    return val


def send_email(subject: str, body: str, html_body: str, attachment: Path, max_retries: int = 3):
    smtp_host = get_env("ALERT_SMTP_HOST")
    smtp_port = int(get_env("ALERT_SMTP_PORT", "587"))
    smtp_user = get_env("ALERT_SMTP_USER")
    smtp_pass = get_env("ALERT_SMTP_PASS")
    smtp_from = get_env("ALERT_EMAIL_FROM")
    smtp_to = get_env("ALERT_EMAIL_TO")
    use_tls = get_env("ALERT_SMTP_USE_TLS", "true").lower() == "true"

    if not smtp_host:
        raise RuntimeError("ALERT_SMTP_HOST is empty; cannot send email.")

    print(f"Connecting to SMTP host: {smtp_host}:{smtp_port} (TLS={use_tls})")

    recipients = [x.strip() for x in smtp_to.split(",") if x.strip()]
    msg = EmailMessage()
    msg["Subject"] = subject
    msg["From"] = smtp_from
    msg["To"] = ", ".join(recipients)
    # Plain-text fallback
    msg.set_content(body)
    # HTML alternative with embedded plots
    msg.add_alternative(html_body, subtype="html")
    # Promote to multipart/mixed so the CSV attachment can be added alongside
    msg.make_mixed()
    msg.add_attachment(attachment.read_bytes(), maintype="text", subtype="csv",
                       filename=attachment.name)

    last_error = None
    for attempt in range(max_retries):
        try:
            print(f"Sending email (attempt {attempt + 1}/{max_retries})...")
            with smtplib.SMTP(smtp_host, smtp_port, timeout=30) as server:
                if use_tls:
                    server.starttls()
                server.login(smtp_user, smtp_pass)
                server.send_message(msg)
            return
        except (socket.gaierror, OSError, smtplib.SMTPException) as e:
            last_error = e
            wait = min(2 ** attempt, 60)
            print(f"Email send error (attempt {attempt + 1}/{max_retries}): {e}. "
                  f"{'Retrying in ' + str(wait) + 's...' if attempt < max_retries - 1 else 'Giving up.'}")
            if attempt < max_retries - 1:
                time.sleep(wait)

    raise RuntimeError(f"Failed to send email after {max_retries} attempts: {last_error}")


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

    subject, body, html_body = build_email_body(rows, today_str)

    if dry_run:
        print("\n--- DRY RUN ---")
        print(subject)
        print(body)
        return

    send_email(subject, body, html_body, latest_csv)
    print("Email enviado com sucesso.")


if __name__ == "__main__":
    main()
