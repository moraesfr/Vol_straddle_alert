#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

if [[ -f ".env" ]]; then
	set -a
	# shellcheck disable=SC1091
	source .env
	set +a
fi

Rscript check_alert_today.R
python3 send_daily_alert_email.py
