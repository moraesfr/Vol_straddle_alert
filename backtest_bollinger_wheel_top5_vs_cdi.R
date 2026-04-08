required_packages <- c("quantmod", "TTR", "tibble", "dplyr", "readr", "ggplot2", "jsonlite")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

# ---- Parameters ----
symbols <- c("PETR4.SA", "VALE3.SA", "ITUB4.SA", "BBDC4.SA", "ABEV3.SA")
from_date <- as.Date("2023-01-01")
to_date <- Sys.Date()
initial_capital_per_stock <- 1000
bb_n <- 20
bb_sd <- 2
vol_lookback <- 60
risk_free <- 0.105

# ---- Helpers ----
third_friday <- function(year, month) {
  d <- as.Date(sprintf("%04d-%02d-01", year, month))
  first_wday <- as.POSIXlt(d)$wday
  friday_offset <- (5 - first_wday) %% 7
  first_friday <- d + friday_offset
  first_friday + 14
}

next_monthly_expiry <- function(d) {
  y <- as.integer(format(d, "%Y"))
  m <- as.integer(format(d, "%m"))
  e <- third_friday(y, m)
  if (e <= d) {
    if (m == 12) {
      y <- y + 1
      m <- 1
    } else {
      m <- m + 1
    }
    e <- third_friday(y, m)
  }
  e
}

align_expiry_to_trade_day <- function(expiry, trade_dates) {
  idx <- max(which(trade_dates <= expiry))
  if (!is.finite(idx)) return(NA_integer_)
  idx
}

bs_price <- function(S, K, r, sigma, T, type = c("call", "put")) {
  type <- match.arg(type)
  if (!is.finite(S) || !is.finite(K) || !is.finite(r) || !is.finite(sigma) || !is.finite(T) || S <= 0 || K <= 0) return(NA_real_)
  if (T <= 0) {
    if (type == "call") return(max(S - K, 0))
    return(max(K - S, 0))
  }
  sigma <- max(sigma, 1e-6)
  d1 <- (log(S / K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  if (type == "call") return(S * pnorm(d1) - K * exp(-r * T) * pnorm(d2))
  K * exp(-r * T) * pnorm(-d2) - S * pnorm(-d1)
}

get_cdi_curve <- function(from_date, to_date, initial_value) {
  url <- sprintf(
    "https://api.bcb.gov.br/dados/serie/bcdata.sgs.12/dados?formato=json&dataInicial=%s&dataFinal=%s",
    format(from_date, "%d/%m/%Y"), format(to_date, "%d/%m/%Y")
  )

  x <- tryCatch(jsonlite::fromJSON(url, simplifyVector = TRUE), error = function(e) NULL)
  if (is.null(x)) return(NULL)

  if (is.data.frame(x) && all(c("date", "valor") %in% names(x))) {
    raw <- x
  } else if (is.list(x) && length(x) > 0 && is.list(x[[1]]) && all(c("data", "valor") %in% names(x[[1]]))) {
    raw <- data.frame(
      date = vapply(x, function(r) as.character(r$data[1]), character(1)),
      valor = vapply(x, function(r) as.character(r$valor[1]), character(1)),
      stringsAsFactors = FALSE
    )
  } else if (is.list(x) && length(x) > 0 && is.data.frame(x[[1]]) && all(c("data", "valor") %in% names(x[[1]]))) {
    raw <- x[[1]]
    names(raw)[names(raw) == "data"] <- "date"
  } else if (is.list(x) && all(c("date", "valor") %in% names(x))) {
    raw <- data.frame(date = unlist(x$date), valor = unlist(x$valor), stringsAsFactors = FALSE)
  } else {
    return(NULL)
  }

  if (nrow(raw) == 0) return(NULL)

  cdi <- tibble(
    date = as.Date(raw$date, format = "%d/%m/%Y"),
    value = as.numeric(raw$valor)
  ) %>%
    filter(is.finite(value)) %>%
    arrange(date)

  # SGS-12 is usually annualized percent; if values are too small, treat as daily percent.
  if (median(cdi$value, na.rm = TRUE) > 1) {
    cdi <- cdi %>% mutate(daily_rate = (1 + value / 100)^(1 / 252) - 1)
  } else {
    cdi <- cdi %>% mutate(daily_rate = value / 100)
  }

  cdi <- cdi %>% mutate(cdi_value = initial_value * cumprod(1 + daily_rate))
  cdi
}

run_wheel_single <- function(symbol) {
  px <- tryCatch(getSymbols(symbol, from = from_date, to = to_date, auto.assign = FALSE), error = function(e) NULL)
  if (is.null(px) || nrow(px) < (bb_n + vol_lookback + 10)) return(NULL)

  close_px <- as.numeric(Cl(px))
  dates <- as.Date(index(px))
  bb <- TTR::BBands(Cl(px), n = bb_n, sd = bb_sd)
  up <- as.numeric(bb$up)
  dn <- as.numeric(bb$dn)

  ret <- diff(log(close_px))
  vol_annual <- rep(NA_real_, length(close_px))
  for (i in seq_len(length(close_px))) {
    if (i >= vol_lookback) {
      vol_annual[i] <- sd(ret[(i - vol_lookback + 1):i], na.rm = TRUE) * sqrt(252)
    }
  }

  cash <- initial_capital_per_stock
  stock_qty <- 0L
  open_put <- NULL
  open_call <- NULL
  rows <- vector("list", length(close_px))

  for (i in seq_along(close_px)) {
    S <- close_px[i]
    d <- dates[i]

    if (!is.null(open_put) && i == open_put$expiry_idx) {
      if (S < open_put$strike) {
        cash <- cash - open_put$strike * open_put$qty
        stock_qty <- stock_qty + open_put$qty
      }
      open_put <- NULL
    }

    if (!is.null(open_call) && i == open_call$expiry_idx) {
      if (S > open_call$strike) {
        cash <- cash + open_call$strike * open_call$qty
        stock_qty <- stock_qty - open_call$qty
      }
      open_call <- NULL
    }

    if (is.null(open_put) && is.null(open_call) && is.finite(S) && is.finite(up[i]) && is.finite(dn[i])) {
      sigma <- vol_annual[i]
      if (!is.finite(sigma)) sigma <- 0.35
      sigma <- min(1.20, max(0.15, sigma + 0.03))

      expiry_cal <- next_monthly_expiry(d)
      expiry_idx <- align_expiry_to_trade_day(expiry_cal, dates)

      if (is.finite(expiry_idx) && expiry_idx > i) {
        T_exp <- as.numeric(dates[expiry_idx] - d) / 365.25

        if (stock_qty == 0L && S <= dn[i]) {
          qty <- floor(cash / S)
          if (is.finite(qty) && qty >= 1) {
            prem <- bs_price(S, S, risk_free, sigma, T_exp, "put")
            if (is.finite(prem) && prem > 0) {
              cash <- cash + qty * prem
              open_put <- list(expiry_idx = expiry_idx, strike = S, qty = qty)
            }
          }
        }

        if (stock_qty > 0L && S >= up[i] && is.null(open_put)) {
          qty <- stock_qty
          prem <- bs_price(S, S, risk_free, sigma, T_exp, "call")
          if (is.finite(prem) && prem > 0) {
            cash <- cash + qty * prem
            open_call <- list(expiry_idx = expiry_idx, strike = S, qty = qty)
          }
        }
      }
    }

    rows[[i]] <- tibble(
      symbol = symbol,
      date = d,
      equity = cash + stock_qty * S
    )
  }

  eq <- bind_rows(rows)
  tibble(
    symbol = symbol,
    start_date = min(eq$date),
    end_date = max(eq$date),
    initial_capital = initial_capital_per_stock,
    final_equity = tail(eq$equity, 1),
    roi_pct = 100 * (tail(eq$equity, 1) / initial_capital_per_stock - 1)
  ) -> summary

  list(equity = eq, summary = summary)
}

# ---- Run all symbols ----
runs <- lapply(symbols, run_wheel_single)
valid <- !sapply(runs, is.null)
runs <- runs[valid]
if (length(runs) == 0) stop("No symbols produced valid runs.")

summary_by_symbol <- bind_rows(lapply(runs, function(x) x$summary))
equity_df <- bind_rows(lapply(runs, function(x) x$equity))

# ---- CDI benchmark ----
cdi <- get_cdi_curve(from_date, to_date, initial_capital_per_stock)
if (is.null(cdi)) {
  warning("Could not download CDI series from BCB API; using fixed annual CDI proxy.")
  biz_dates <- sort(unique(equity_df$date))
  daily_rate_proxy <- (1 + risk_free)^(1 / 252) - 1
  cdi <- tibble(
    date = biz_dates,
    value = daily_rate_proxy * 100,
    daily_rate = daily_rate_proxy,
    cdi_value = initial_capital_per_stock * cumprod(rep(1 + daily_rate_proxy, length(biz_dates))),
    source = "proxy_fixed_annual_rate"
  )
  cdi_final <- tail(cdi$cdi_value, 1)
} else {
  cdi <- cdi %>% mutate(source = "bcb_sgs_12")
  cdi_final <- tail(cdi$cdi_value, 1)
}

summary_by_symbol <- summary_by_symbol %>%
  mutate(
    cdi_final_per_1000 = cdi_final,
    excess_vs_cdi = final_equity - cdi_final_per_1000,
    excess_vs_cdi_pct = 100 * (final_equity / cdi_final_per_1000 - 1)
  ) %>%
  arrange(desc(roi_pct))

portfolio_summary <- tibble(
  symbols_used = nrow(summary_by_symbol),
  initial_total = initial_capital_per_stock * nrow(summary_by_symbol),
  final_total = sum(summary_by_symbol$final_equity),
  roi_total_pct = 100 * (final_total / initial_total - 1),
  equivalent_per_1000 = mean(summary_by_symbol$final_equity),
  cdi_final_per_1000 = cdi_final,
  cdi_total_equivalent = cdi_final * nrow(summary_by_symbol),
  excess_total_vs_cdi = final_total - cdi_total_equivalent,
  cdi_source = unique(cdi$source)[1]
)

# ---- Plot: one chart with all stocks + CDI ----
plot_df <- equity_df %>%
  mutate(series = symbol) %>%
  select(date, series, equity)

if (!is.null(cdi)) {
  cdi_plot <- cdi %>%
    transmute(date = date, series = "CDI", equity = cdi_value)
  plot_df <- bind_rows(plot_df, cdi_plot)
}

p <- ggplot(plot_df, aes(x = date, y = equity, color = series, linewidth = series == "CDI")) +
  geom_line(alpha = 0.9) +
  scale_linewidth_manual(values = c("TRUE" = 1.2, "FALSE" = 0.8), guide = "none") +
  labs(
    title = "Bollinger Wheel by Stock (R$1,000 each) vs CDI",
    subtitle = paste0(format(from_date), " to ", format(to_date)),
    x = NULL,
    y = "Value (R$)",
    color = "Series"
  ) +
  theme_bw(base_size = 11)

ggsave("bollinger_wheel_top5_vs_cdi.png", p, width = 11, height = 6, dpi = 150)

# ---- Save ----
write_csv(summary_by_symbol, "bollinger_wheel_top5_summary_by_symbol.csv")
write_csv(portfolio_summary, "bollinger_wheel_top5_portfolio_summary.csv")
write_csv(equity_df, "bollinger_wheel_top5_equity_by_symbol.csv")
if (!is.null(cdi)) write_csv(cdi, "cdi_series_2023_to_now.csv")

cat("Done.\n\n")
cat("Per-stock summary:\n")
print(summary_by_symbol)
cat("\nPortfolio summary:\n")
print(portfolio_summary)
