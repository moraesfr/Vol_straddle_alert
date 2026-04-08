library(quantmod)
library(dplyr)
library(readr)

options(warn = -1)

symbols <- c("PETR4.SA", "VALE3.SA", "ITUB4.SA", "BBDC4.SA", "ABEV3.SA")
start_date <- as.Date("2023-01-02")
end_date <- Sys.Date()

calc_roll <- function(x, w = 20) {
  n <- length(x)
  out <- rep(NA_real_, n)
  if (n >= w) {
    for (i in w:n) {
      out[i] <- sd(x[(i - w + 1):i], na.rm = TRUE)
    }
  }
  out
}

check_symbol <- function(sym) {
  x <- tryCatch(
    getSymbols(sym, from = start_date, to = end_date, auto.assign = FALSE),
    error = function(e) NULL
  )

  if (is.null(x)) {
    return(data.frame(
      symbol = sym,
      last_date = NA_character_,
      last_vol = NA_real_,
      threshold = NA_real_,
      is_alert_today = FALSE,
      below_threshold = FALSE,
      status = "download_error",
      stringsAsFactors = FALSE
    ))
  }

  close <- Cl(x)
  ret <- diff(log(close), lag = 1)
  ret <- ret[!is.na(ret)]

  if (length(ret) < 25) {
    return(data.frame(
      symbol = sym,
      last_date = NA_character_,
      last_vol = NA_real_,
      threshold = NA_real_,
      is_alert_today = FALSE,
      below_threshold = FALSE,
      status = "insufficient_data",
      stringsAsFactors = FALSE
    ))
  }

  vol <- calc_roll(as.numeric(ret), 20)
  df <- data.frame(
    date = as.Date(index(ret)),
    volatility = vol,
    stringsAsFactors = FALSE
  )
  df <- df[!is.na(df$volatility), ]

  thr <- as.numeric(quantile(df$volatility, 0.30, na.rm = TRUE))
  df <- df %>%
    arrange(date) %>%
    mutate(
      prev_vol = dplyr::lag(volatility),
      is_alert = !is.na(prev_vol) & prev_vol > thr & volatility <= thr
    )

  last <- df[nrow(df), ]
  data.frame(
    symbol = sym,
    last_date = as.character(last$date),
    last_vol = round(last$volatility, 6),
    threshold = round(thr, 6),
    is_alert_today = (last$date == as.Date(Sys.Date())) && isTRUE(last$is_alert),
    below_threshold = isTRUE(last$volatility <= thr),
    status = "ok",
    stringsAsFactors = FALSE
  )
}

rows <- lapply(symbols, check_symbol)
out <- do.call(rbind, rows)
out <- out %>% arrange(desc(is_alert_today), symbol)

daily_stamp <- format(Sys.Date(), "%Y-%m-%d")
file_today <- paste0("daily_alert_check_", daily_stamp, ".csv")

write_csv(out, file_today)
write_csv(out, "daily_alert_check_latest.csv")

cat("\n=== DAILY ALERT CHECK ===\n")
cat("Date:", daily_stamp, "\n\n")
print(out)

alerts_today <- out %>% filter(is_alert_today)
below_now <- out %>% filter(below_threshold)

cat("\nAlerts today (strict crossing on today):\n")
if (nrow(alerts_today) == 0) {
  cat("  None\n")
} else {
  print(alerts_today[, c("symbol", "last_date", "last_vol", "threshold")])
}

cat("\nNames currently below threshold (state):\n")
if (nrow(below_now) == 0) {
  cat("  None\n")
} else {
  print(below_now[, c("symbol", "last_date", "last_vol", "threshold")])
}

cat("\nSaved files:\n")
cat("  -", file_today, "\n")
cat("  - daily_alert_check_latest.csv\n")
