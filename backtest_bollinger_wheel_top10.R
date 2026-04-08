required_packages <- c("quantmod", "TTR", "tibble", "dplyr", "readr", "ggplot2")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

# ---- Parameters ----
symbols <- c(
  "PETR4.SA", "VALE3.SA", "ITUB4.SA", "BBDC4.SA", "ABEV3.SA",
  "BBAS3.SA", "B3SA3.SA", "WEGE3.SA", "RENT3.SA", "SUZB3.SA"
)
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
      window_r <- ret[(i - vol_lookback + 1):i]
      vol_annual[i] <- sd(window_r, na.rm = TRUE) * sqrt(252)
    }
  }

  cash <- initial_capital_per_stock
  stock_qty <- 0L
  open_put <- NULL
  open_call <- NULL

  rows <- vector("list", length(close_px))
  trades <- list()

  for (i in seq_along(close_px)) {
    S <- close_px[i]
    d <- dates[i]

    if (!is.null(open_put) && i == open_put$expiry_idx) {
      assigned <- S < open_put$strike
      if (assigned) {
        cash <- cash - open_put$strike * open_put$qty
        stock_qty <- stock_qty + open_put$qty
      }
      trades[[length(trades) + 1]] <- tibble(
        symbol = symbol,
        type = "PUT",
        sell_date = open_put$sell_date,
        expiry_date = d,
        strike = open_put$strike,
        qty = open_put$qty,
        premium_per_share = open_put$premium,
        assigned = assigned,
        spot_expiry = S,
        cash_after = cash,
        stock_after = stock_qty
      )
      open_put <- NULL
    }

    if (!is.null(open_call) && i == open_call$expiry_idx) {
      called <- S > open_call$strike
      if (called) {
        cash <- cash + open_call$strike * open_call$qty
        stock_qty <- stock_qty - open_call$qty
      }
      trades[[length(trades) + 1]] <- tibble(
        symbol = symbol,
        type = "CALL",
        sell_date = open_call$sell_date,
        expiry_date = d,
        strike = open_call$strike,
        qty = open_call$qty,
        premium_per_share = open_call$premium,
        assigned = called,
        spot_expiry = S,
        cash_after = cash,
        stock_after = stock_qty
      )
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
              open_put <- list(sell_date = d, expiry_idx = expiry_idx, strike = S, qty = qty, premium = prem)
            }
          }
        }

        if (stock_qty > 0L && S >= up[i] && is.null(open_put)) {
          qty <- stock_qty
          prem <- bs_price(S, S, risk_free, sigma, T_exp, "call")
          if (is.finite(prem) && prem > 0) {
            cash <- cash + qty * prem
            open_call <- list(sell_date = d, expiry_idx = expiry_idx, strike = S, qty = qty, premium = prem)
          }
        }
      }
    }

    rows[[i]] <- tibble(
      symbol = symbol,
      date = d,
      close = S,
      lower_band = dn[i],
      upper_band = up[i],
      cash = cash,
      stock_qty = stock_qty,
      equity = cash + stock_qty * S
    )
  }

  eq <- bind_rows(rows)
  tr <- if (length(trades) > 0) bind_rows(trades) else tibble()

  final_equity <- tail(eq$equity, 1)
  summary <- tibble(
    symbol = symbol,
    start_date = min(eq$date),
    end_date = max(eq$date),
    initial_capital = initial_capital_per_stock,
    final_equity = final_equity,
    roi_pct = 100 * (final_equity / initial_capital_per_stock - 1),
    put_sales = sum(tr$type == "PUT", na.rm = TRUE),
    call_sales = sum(tr$type == "CALL", na.rm = TRUE),
    put_assignments = sum(tr$type == "PUT" & tr$assigned, na.rm = TRUE),
    call_assignments = sum(tr$type == "CALL" & tr$assigned, na.rm = TRUE)
  )

  list(equity = eq, trades = tr, summary = summary)
}

# ---- Run portfolio ----
all_runs <- lapply(symbols, run_wheel_single)
valid <- !sapply(all_runs, is.null)
all_runs <- all_runs[valid]
symbols_used <- symbols[valid]

if (length(all_runs) == 0) stop("No symbols produced valid data.")

equity_df <- bind_rows(lapply(all_runs, function(x) x$equity))
trade_df <- bind_rows(lapply(all_runs, function(x) x$trades))
summary_by_symbol <- bind_rows(lapply(all_runs, function(x) x$summary)) %>% arrange(desc(roi_pct))

# Aggregate portfolio equity by date (sum over symbols)
portfolio_equity <- equity_df %>%
  group_by(date) %>%
  summarise(equity = sum(equity, na.rm = TRUE), .groups = "drop") %>%
  arrange(date)

initial_total <- initial_capital_per_stock * length(symbols_used)
final_total <- tail(portfolio_equity$equity, 1)
roi_total <- 100 * (final_total / initial_total - 1)
n_used <- length(symbols_used)

portfolio_summary <- tibble(
  symbols_tested = length(symbols),
  symbols_used = n_used,
  initial_capital_per_stock = initial_capital_per_stock,
  initial_total = initial_total,
  final_total = final_total,
  roi_total_pct = roi_total,
  equivalent_per_1000 = final_total / n_used
)

# ---- Save outputs ----
write_csv(summary_by_symbol, "bollinger_wheel_top10_summary_by_symbol.csv")
write_csv(portfolio_summary, "bollinger_wheel_top10_portfolio_summary.csv")
write_csv(trade_df, "bollinger_wheel_top10_trades.csv")
write_csv(portfolio_equity, "bollinger_wheel_top10_portfolio_equity.csv")

p <- ggplot(portfolio_equity, aes(x = date, y = equity)) +
  geom_line(color = "#2b8cbe", linewidth = 0.8) +
  geom_hline(yintercept = initial_total, linetype = "dashed", color = "gray40") +
  labs(
    title = "Bollinger Wheel Portfolio (10 Liquid Stocks)",
    subtitle = paste0("R$", initial_capital_per_stock, " per stock from ", from_date, " to ", to_date),
    x = NULL,
    y = "Portfolio Equity (R$)"
  ) +
  theme_bw(base_size = 11)

ggsave("bollinger_wheel_top10_portfolio_equity.png", p, width = 10, height = 5, dpi = 150)

cat("Done. Symbols used:\n")
print(symbols_used)
cat("\nPortfolio summary:\n")
print(portfolio_summary)
cat("\nTop and bottom symbols by ROI:\n")
print(summary_by_symbol %>% select(symbol, roi_pct, final_equity) %>% head(3))
print(summary_by_symbol %>% select(symbol, roi_pct, final_equity) %>% tail(3))
