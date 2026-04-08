required_packages <- c("quantmod", "TTR", "tibble", "dplyr", "readr", "ggplot2")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

# ---- Parameters ----
symbol <- "PETR4.SA"
from_date <- as.Date("2023-01-01")
to_date <- Sys.Date()
initial_capital <- 1000
bb_n <- 20
bb_sd <- 2
vol_lookback <- 60
risk_free <- 0.105

# ---- Helpers ----
third_friday <- function(year, month) {
  d <- as.Date(sprintf("%04d-%02d-01", year, month))
  first_wday <- as.POSIXlt(d)$wday # 0=Sunday
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

# ---- Data ----
px <- getSymbols(symbol, from = from_date, to = to_date, auto.assign = FALSE)
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

# ---- Strategy state ----
cash <- initial_capital
stock_qty <- 0L
open_put <- NULL
open_call <- NULL

# Track daily equity
rows <- vector("list", length(close_px))
trades <- list()

for (i in seq_along(close_px)) {
  S <- close_px[i]
  d <- dates[i]

  # Expiry processing first
  if (!is.null(open_put) && i == open_put$expiry_idx) {
    assigned <- S < open_put$strike
    if (assigned) {
      cash <- cash - open_put$strike * open_put$qty
      stock_qty <- stock_qty + open_put$qty
    }
    trades[[length(trades) + 1]] <- tibble(
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

  # New trade only if no option currently open
  if (is.null(open_put) && is.null(open_call) && is.finite(S) && is.finite(up[i]) && is.finite(dn[i])) {
    sigma <- vol_annual[i]
    if (!is.finite(sigma)) sigma <- 0.35
    sigma <- min(1.20, max(0.15, sigma + 0.03))

    expiry_cal <- next_monthly_expiry(d)
    expiry_idx <- align_expiry_to_trade_day(expiry_cal, dates)

    if (is.finite(expiry_idx) && expiry_idx > i) {
      T_exp <- as.numeric(dates[expiry_idx] - d) / 365.25

      # Cash state: sell cash-secured put when touching lower band
      if (stock_qty == 0L && S <= dn[i]) {
        qty <- floor(cash / S)
        if (is.finite(qty) && qty >= 1) {
          prem <- bs_price(S, S, risk_free, sigma, T_exp, "put")
          if (is.finite(prem) && prem > 0) {
            cash <- cash + qty * prem
            open_put <- list(
              sell_date = d,
              expiry_idx = expiry_idx,
              strike = S,
              qty = qty,
              premium = prem
            )
          }
        }
      }

      # Stock state: sell covered call when touching upper band
      if (stock_qty > 0L && S >= up[i] && is.null(open_put)) {
        qty <- stock_qty
        prem <- bs_price(S, S, risk_free, sigma, T_exp, "call")
        if (is.finite(prem) && prem > 0) {
          cash <- cash + qty * prem
          open_call <- list(
            sell_date = d,
            expiry_idx = expiry_idx,
            strike = S,
            qty = qty,
            premium = prem
          )
        }
      }
    }
  }

  rows[[i]] <- tibble(
    date = d,
    close = S,
    lower_band = dn[i],
    upper_band = up[i],
    cash = cash,
    stock_qty = stock_qty,
    equity = cash + stock_qty * S,
    open_put = !is.null(open_put),
    open_call = !is.null(open_call)
  )
}

equity_df <- bind_rows(rows)
trade_df <- if (length(trades) > 0) bind_rows(trades) else tibble()

final_equity <- tail(equity_df$equity, 1)
roi_pct <- 100 * (final_equity / initial_capital - 1)

summary_df <- tibble(
  symbol = symbol,
  start_date = min(equity_df$date),
  end_date = max(equity_df$date),
  initial_capital = initial_capital,
  final_equity = final_equity,
  roi_pct = roi_pct,
  put_sales = sum(trade_df$type == "PUT", na.rm = TRUE),
  call_sales = sum(trade_df$type == "CALL", na.rm = TRUE),
  put_assignments = sum(trade_df$type == "PUT" & trade_df$assigned, na.rm = TRUE),
  call_assignments = sum(trade_df$type == "CALL" & trade_df$assigned, na.rm = TRUE)
)

write_csv(equity_df, "bollinger_wheel_equity.csv")
write_csv(trade_df, "bollinger_wheel_trades.csv")
write_csv(summary_df, "bollinger_wheel_summary.csv")

p_eq <- ggplot(equity_df, aes(x = date, y = equity)) +
  geom_line(color = "#2b8cbe", linewidth = 0.8) +
  geom_hline(yintercept = initial_capital, linetype = "dashed", color = "gray40") +
  labs(
    title = "Bollinger Wheel Strategy Equity",
    subtitle = paste0(symbol, " | Sell put at lower band, sell call at upper band"),
    x = NULL,
    y = "Equity (R$)"
  ) +
  theme_bw(base_size = 11)

ggsave("bollinger_wheel_equity.png", p_eq, width = 10, height = 5, dpi = 150)

cat("Done. Files created:\n")
cat("- bollinger_wheel_equity.csv\n")
cat("- bollinger_wheel_trades.csv\n")
cat("- bollinger_wheel_summary.csv\n")
cat("- bollinger_wheel_equity.png\n\n")
print(summary_df)
