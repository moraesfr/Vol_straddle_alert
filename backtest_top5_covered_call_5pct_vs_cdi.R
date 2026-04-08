required_packages <- c("quantmod", "tibble", "dplyr", "readr", "ggplot2", "jsonlite")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

# -----------------------------------------------------------------------------
# Strategy:
# - Buy ~R$5,000 of each stock at day 1, respecting round lot size (100 shares).
# - Sell monthly covered CALL with strike = +5% over purchase price (cost basis).
# - If called away at expiry, re-buy stock next trading day with available cash,
#   update cost basis, and repeat.
# - If not called away, keep stock and sell new covered CALL again with strike at
#   +5% over the same purchase cost basis.
# -----------------------------------------------------------------------------

symbols <- c("PETR4.SA", "VALE3.SA", "ITUB4.SA", "BBDC4.SA", "ABEV3.SA")
from_date <- as.Date("2023-01-01")
to_date <- Sys.Date()
initial_capital_per_stock <- 5000
lot_size <- 100
strike_buffer <- 0.05
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

  if (nrow(cdi) == 0) return(NULL)

  if (median(cdi$value, na.rm = TRUE) > 1) {
    cdi <- cdi %>% mutate(daily_rate = (1 + value / 100)^(1 / 252) - 1)
  } else {
    cdi <- cdi %>% mutate(daily_rate = value / 100)
  }

  cdi <- cdi %>% mutate(cdi_value = initial_value * cumprod(1 + daily_rate), source = "bcb_sgs_12")
  cdi
}

run_symbol <- function(symbol) {
  px <- tryCatch(getSymbols(symbol, from = from_date, to = to_date, auto.assign = FALSE), error = function(e) NULL)
  if (is.null(px) || nrow(px) < (vol_lookback + 30)) return(NULL)

  dates <- as.Date(index(px))
  close_px <- as.numeric(Cl(px))

  ret <- diff(log(close_px))
  vol_annual <- rep(NA_real_, length(close_px))
  for (i in seq_along(close_px)) {
    if (i >= vol_lookback) vol_annual[i] <- sd(ret[(i - vol_lookback + 1):i], na.rm = TRUE) * sqrt(252)
  }

  # Initial buy (day 1) with round lot
  S0 <- close_px[1]
  qty <- floor((initial_capital_per_stock / S0) / lot_size) * lot_size
  if (!is.finite(qty) || qty < lot_size) return(NULL)

  cash <- initial_capital_per_stock - qty * S0
  stock_qty <- qty
  cost_basis <- S0

  open_call <- NULL
  pending_rebuy <- FALSE

  trades <- list()
  equity_rows <- vector("list", length(close_px))

  for (i in seq_along(close_px)) {
    S <- close_px[i]
    d <- dates[i]

    # Rebuy after prior call assignment
    if (pending_rebuy) {
      qty_new <- floor((cash / S) / lot_size) * lot_size
      if (is.finite(qty_new) && qty_new >= lot_size) {
        cash <- cash - qty_new * S
        stock_qty <- qty_new
        cost_basis <- S
        pending_rebuy <- FALSE
        trades[[length(trades) + 1]] <- tibble(
          symbol = symbol,
          event = "REBUY",
          date = d,
          price = S,
          qty = qty_new,
          cash_after = cash,
          stock_after = stock_qty
        )
      }
    }

    # Option expiry
    if (!is.null(open_call) && i == open_call$expiry_idx) {
      called <- S >= open_call$strike
      if (called) {
        cash <- cash + open_call$strike * open_call$qty
        stock_qty <- 0L
        pending_rebuy <- TRUE
      }
      trades[[length(trades) + 1]] <- tibble(
        symbol = symbol,
        event = "CALL_EXPIRY",
        date = d,
        price = S,
        strike = open_call$strike,
        qty = open_call$qty,
        called = called,
        premium_per_share = open_call$premium,
        cash_after = cash,
        stock_after = stock_qty
      )
      open_call <- NULL
    }

    # Sell new covered call if we hold stock and no open call
    if (stock_qty > 0L && is.null(open_call)) {
      expiry_cal <- next_monthly_expiry(d)
      expiry_idx <- align_expiry_to_trade_day(expiry_cal, dates)

      if (is.finite(expiry_idx) && expiry_idx > i) {
        T_exp <- as.numeric(dates[expiry_idx] - d) / 365.25
        sigma <- vol_annual[i]
        if (!is.finite(sigma)) sigma <- 0.35
        sigma <- min(1.20, max(0.15, sigma + 0.03))

        strike <- cost_basis * (1 + strike_buffer)
        prem <- bs_price(S, strike, risk_free, sigma, T_exp, type = "call")

        if (is.finite(prem) && prem > 0) {
          cash <- cash + stock_qty * prem
          open_call <- list(
            sell_date = d,
            expiry_idx = expiry_idx,
            strike = strike,
            qty = stock_qty,
            premium = prem
          )
          trades[[length(trades) + 1]] <- tibble(
            symbol = symbol,
            event = "SELL_CALL",
            date = d,
            price = S,
            strike = strike,
            qty = stock_qty,
            premium_per_share = prem,
            cash_after = cash,
            stock_after = stock_qty
          )
        }
      }
    }

    equity_rows[[i]] <- tibble(
      symbol = symbol,
      date = d,
      close = S,
      cash = cash,
      stock_qty = stock_qty,
      cost_basis = cost_basis,
      equity = cash + stock_qty * S,
      open_call = !is.null(open_call)
    )
  }

  eq <- bind_rows(equity_rows)
  tr <- if (length(trades) > 0) bind_rows(trades) else tibble()

  summary <- tibble(
    symbol = symbol,
    start_date = min(eq$date),
    end_date = max(eq$date),
    initial_capital = initial_capital_per_stock,
    initial_qty = qty,
    final_equity = tail(eq$equity, 1),
    roi_pct = 100 * (tail(eq$equity, 1) / initial_capital_per_stock - 1),
    call_sales = sum(tr$event == "SELL_CALL", na.rm = TRUE),
    call_assignments = sum(tr$event == "CALL_EXPIRY" & tr$called, na.rm = TRUE),
    rebuys = sum(tr$event == "REBUY", na.rm = TRUE)
  )

  list(equity = eq, trades = tr, summary = summary)
}

# ---- Run all symbols ----
runs <- lapply(symbols, run_symbol)
valid <- !sapply(runs, is.null)
runs <- runs[valid]
if (length(runs) == 0) stop("No symbol produced a valid run.")

summary_by_symbol <- bind_rows(lapply(runs, function(x) x$summary))
equity_df <- bind_rows(lapply(runs, function(x) x$equity))
trades_df <- bind_rows(lapply(runs, function(x) x$trades))

# ---- CDI benchmark ----
cdi <- get_cdi_curve(from_date, to_date, initial_capital_per_stock)
if (is.null(cdi)) {
  warning("Could not download CDI from BCB API; using fixed annual CDI proxy.")
  all_dates <- sort(unique(equity_df$date))
  daily_rate_proxy <- (1 + risk_free)^(1 / 252) - 1
  cdi <- tibble(
    date = all_dates,
    value = daily_rate_proxy * 100,
    daily_rate = daily_rate_proxy,
    cdi_value = initial_capital_per_stock * cumprod(rep(1 + daily_rate_proxy, length(all_dates))),
    source = "proxy_fixed_annual_rate"
  )
}

cdi_final <- tail(cdi$cdi_value, 1)

summary_by_symbol <- summary_by_symbol %>%
  mutate(
    cdi_final_per_5000 = cdi_final,
    excess_vs_cdi = final_equity - cdi_final_per_5000,
    excess_vs_cdi_pct = 100 * (final_equity / cdi_final_per_5000 - 1)
  ) %>%
  arrange(desc(roi_pct))

portfolio_summary <- tibble(
  symbols_used = nrow(summary_by_symbol),
  initial_total = initial_capital_per_stock * nrow(summary_by_symbol),
  final_total = sum(summary_by_symbol$final_equity),
  roi_total_pct = 100 * (final_total / initial_total - 1),
  cdi_final_per_5000 = cdi_final,
  cdi_total_equivalent = cdi_final * nrow(summary_by_symbol),
  excess_total_vs_cdi = final_total - cdi_total_equivalent,
  cdi_source = unique(cdi$source)[1]
)

# ---- Plot (individual stocks + CDI on one chart) ----
plot_df <- equity_df %>% transmute(date, series = symbol, equity)
cdi_plot <- cdi %>% transmute(date, series = "CDI", equity = cdi_value)
plot_df <- bind_rows(plot_df, cdi_plot)

p <- ggplot(plot_df, aes(x = date, y = equity, color = series, linewidth = series == "CDI")) +
  geom_line(alpha = 0.9) +
  scale_linewidth_manual(values = c("TRUE" = 1.2, "FALSE" = 0.8), guide = "none") +
  labs(
    title = "Top-5 Covered Call (+5%) Strategy vs CDI",
    subtitle = paste0("R$5,000 por acao | ", format(from_date), " a ", format(to_date)),
    x = NULL,
    y = "Valor (R$)",
    color = "Serie"
  ) +
  theme_bw(base_size = 11)

ggsave("top5_covered_call_5pct_vs_cdi.png", p, width = 11, height = 6, dpi = 150)

# ---- Save outputs ----
write_csv(summary_by_symbol, "top5_covered_call_5pct_summary_by_symbol.csv")
write_csv(portfolio_summary, "top5_covered_call_5pct_portfolio_summary.csv")
write_csv(equity_df, "top5_covered_call_5pct_equity_by_symbol.csv")
write_csv(trades_df, "top5_covered_call_5pct_trades.csv")
write_csv(cdi, "top5_covered_call_5pct_cdi_series.csv")

cat("Done.\n\n")
cat("Per-stock summary:\n")
print(summary_by_symbol)
cat("\nPortfolio summary:\n")
print(portfolio_summary)
