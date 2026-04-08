#!/usr/bin/env python3
import csv
import os
import sys
import smtplib
from datetime import date
from email.message import EmailMessage
from pathlib import Path

WORKDIR = Path(__file__).resolve().parent
LATEST_CSV = WORKDIR / "daily_alert_check_latest.csv"


def read_rows(csv_path: Path):
    if not csv_path.exists():
        raise FileNotFoundError(f"Arquivo nao encontrado: {csv_path}")
    with csv_path.open(newline="", encoding="utf-8") as f:
        return list(csv.DictReader(f))


def build_summary(rows):
    alerts_today = [r for r in rows if str(r.get("is_alert_today", "")).strip().lower() == "true"]
    below_threshold = [r for r in rows if str(r.get("below_threshold", "")).strip().lower() == "true"]

    lines = []
    lines.append(f"Relatorio diario de alertas - {date.today().isoformat()}")
    lines.append("")
    lines.append("Resumo geral:")
    lines.append(f"- Acoes monitoradas: {len(rows)}")
    lines.append(f"- Alertas hoje (cruzamento estrito): {len(alerts_today)}")
    lines.append(f"- Acoes abaixo do threshold (estado): {len(below_threshold)}")
    lines.append("")
    lines.append("Tabela completa:")

    header = "symbol | last_date | last_vol | threshold | is_alert_today | below_threshold | status"
    lines.append(header)
    lines.append("-" * len(header))

    for r in rows:
        lines.append(
            f"{r.get('symbol','')} | {r.get('last_date','')} | {r.get('last_vol','')} | "
            f"{r.get('threshold','')} | {r.get('is_alert_today','')} | {r.get('below_threshold','')} | {r.get('status','')}"
        )

    lines.append("")
    if alerts_today:
        lines.append("Alertas hoje:")
        for r in alerts_today:
            lines.append(f"- {r.get('symbol')} em {r.get('last_date')} (vol={r.get('last_vol')}, thr={r.get('threshold')})")
    else:
        lines.append("Alertas hoje: nenhum")

    subject_prefix = "ALERTA" if alerts_today else "SEM ALERTA"
    subject = f"[{subject_prefix}] Check diario de volatilidade - {date.today().isoformat()}"
    body = "\n".join(lines)
    return subject, body


def get_env(name, required=True, default=None):
    value = os.getenv(name, default)
    if required and (value is None or value == ""):
        raise RuntimeError(f"Variavel de ambiente obrigatoria ausente: {name}")
    return value


def send_email(subject, body, attachment_path: Path):
    smtp_host = get_env("ALERT_SMTP_HOST")
    smtp_port = int(get_env("ALERT_SMTP_PORT", required=False, default="587"))
    smtp_user = get_env("ALERT_SMTP_USER")
    smtp_pass = get_env("ALERT_SMTP_PASS")
    smtp_from = get_env("ALERT_EMAIL_FROM")
    smtp_to = get_env("ALERT_EMAIL_TO")
    use_tls = get_env("ALERT_SMTP_USE_TLS", required=False, default="true").lower() == "true"

    recipients = [x.strip() for x in smtp_to.split(",") if x.strip()]
    if not recipients:
        raise RuntimeError("ALERT_EMAIL_TO precisa conter ao menos 1 destinatario")

    msg = EmailMessage()
    msg["Subject"] = subject
    msg["From"] = smtp_from
    msg["To"] = ", ".join(recipients)
    msg.set_content(body)

    with attachment_path.open("rb") as f:
        data = f.read()
    msg.add_attachment(data, maintype="text", subtype="csv", filename=attachment_path.name)

    with smtplib.SMTP(smtp_host, smtp_port, timeout=30) as server:
        if use_tls:
            server.starttls()
        server.login(smtp_user, smtp_pass)
        server.send_message(msg)


def main():
    dry_run = "--dry-run" in sys.argv

    rows = read_rows(LATEST_CSV)
    subject, body = build_summary(rows)

    if dry_run:
        print(subject)
        print()
        print(body)
        return

    send_email(subject, body, LATEST_CSV)
    print("Email enviado com sucesso.")


if __name__ == "__main__":
    main()
