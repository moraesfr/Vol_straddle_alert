library(quantmod)
library(dplyr)
library(tibble)
library(readr)
library(ggplot2)

symbols <- c("PETR4.SA", "VALE3.SA", "ITUB4.SA", "BBDC4.SA", "ABEV3.SA")
start_date <- as.Date("2023-01-02")
end_date <- as.Date("2026-04-02")
r_annual <- 0.08
target_pct <- 20

black_scholes <- function(S, K, T, r, sigma, type = "call") {
  if (T <= 0 || sigma <= 0) {
    return(0)
  }

  d1 <- (log(S / K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)

  if (type == "call") {
    value <- S * pnorm(d1) - K * exp(-r * T) * pnorm(d2)
  } else {
    value <- K * exp(-r * T) * pnorm(-d2) - S * pnorm(-d1)
  }

  max(value, 0)
}

straddle_price <- function(S, K, T, r, sigma) {
  black_scholes(S, K, T, r, sigma, "call") + black_scholes(S, K, T, r, sigma, "put")
}

get_next_monthly_expiry <- function(current_date) {
  for (day_offset in 1:90) {
    test_date <- current_date + day_offset
    month_now <- as.numeric(format(current_date, "%m"))
    month_test <- as.numeric(format(test_date, "%m"))
    year_now <- as.numeric(format(current_date, "%Y"))
    year_test <- as.numeric(format(test_date, "%Y"))

    if (month_test > month_now || year_test > year_now) {
      year_val <- as.numeric(format(test_date, "%Y"))
      month_val <- as.numeric(format(test_date, "%m"))
      first_day <- as.Date(paste(year_val, month_val, "01", sep = "-"))

      friday_count <- 0
      for (d in 1:31) {
        day_check <- first_day + (d - 1)
        month_check <- as.numeric(format(day_check, "%m"))
        if (month_check != month_val) {
          break
        }

        if (as.numeric(format(day_check, "%w")) == 5) {
          friday_count <- friday_count + 1
          if (friday_count == 3) {
            return(day_check)
          }
        }
      }
    }
  }

  current_date + 30
}

calc_rolling_vol <- function(returns_vec, window = 20) {
  n <- length(returns_vec)
  rolling_vol <- rep(NA_real_, n)

  for (i in window:n) {
    rolling_vol[i] <- sd(returns_vec[(i - window + 1):i], na.rm = TRUE)
  }

  rolling_vol
}

build_volatility_frame <- function(symbol) {
  xts_data <- getSymbols(symbol, from = start_date, to = end_date, auto.assign = FALSE)
  close_prices <- Cl(xts_data)
  log_returns <- diff(log(close_prices), lag = 1)
  log_returns <- log_returns[!is.na(log_returns)]

  vol_df <- tibble(
    date = as.Date(index(log_returns)),
    returns = as.numeric(log_returns),
    volatility = calc_rolling_vol(as.numeric(log_returns), window = 20),
    symbol = symbol
  )

  close_df <- tibble(
    date = as.Date(index(xts_data)),
    close = as.numeric(Cl(xts_data))
  )

  vol_df %>%
    left_join(close_df, by = "date") %>%
    arrange(date)
}

simulate_symbol <- function(symbol) {
  cat("Processing", symbol, "...\n")

  vol_df <- build_volatility_frame(symbol)
  vol_threshold <- quantile(vol_df$volatility, 0.30, na.rm = TRUE)

  signal_df <- vol_df %>%
    mutate(
      prev_vol = lag(volatility),
      is_alert = !is.na(prev_vol) & prev_vol > vol_threshold & volatility <= vol_threshold
    )

  alerts <- signal_df %>%
    filter(is_alert) %>%
    select(date, close, volatility)

  if (nrow(alerts) == 0) {
    return(list(
      summary = tibble(
        symbol = symbol,
        alerts = 0,
        trades = 0,
        capital = 0,
        total_pnl = 0,
        total_return_pct = 0,
        win_rate_pct = 0,
        avg_hold_days = NA_real_,
        avg_pnl_trade = NA_real_,
        cdi_return_pct = 0,
        excess_vs_cdi = 0,
        outperf_x = NA_real_
      ),
      trades = tibble(),
      equity = tibble(),
      volatility = signal_df
    ))
  }

  trades <- list()
  equity_marks <- list()
  trade_counter <- 0

  for (i in seq_len(nrow(alerts))) {
    alert_date <- alerts$date[i]
    alert_price <- alerts$close[i]
    alert_vol <- alerts$volatility[i]
    expiry_date <- get_next_monthly_expiry(alert_date)

    remaining_data <- signal_df %>%
      filter(date >= alert_date & date <= expiry_date)

    if (nrow(remaining_data) < 15) {
      next
    }

    trade_counter <- trade_counter + 1
    T <- as.numeric(expiry_date - alert_date) / 365
    strike <- round(alert_price, 2)
    cost <- straddle_price(alert_price, strike, T, r_annual, alert_vol)

    if (is.na(cost) || cost <= 0) {
      next
    }

    position_data <- remaining_data %>%
      mutate(
        T_remaining = as.numeric(expiry_date - date) / 365,
        straddle_value = pmax(
          0,
          mapply(
            straddle_price,
            S = close,
            K = strike,
            T = T_remaining,
            r = r_annual,
            sigma = volatility
          )
        ),
        pnl = straddle_value - cost,
        pnl_pct = pnl / cost * 100
      ) %>%
      filter(!is.na(straddle_value))

    if (nrow(position_data) == 0) {
      next
    }

    exit_hit <- position_data %>% filter(pnl_pct >= target_pct) %>% slice(1)
    exit_row <- if (nrow(exit_hit) > 0) exit_hit else position_data %>% slice(n())
    exit_reason <- if (nrow(exit_hit) > 0) paste0(target_pct, "% target") else "Expiry"

    trades[[length(trades) + 1]] <- tibble(
      symbol = symbol,
      trade_id = trade_counter,
      alert_date = alert_date,
      alert_price = round(alert_price, 2),
      alert_vol = round(alert_vol, 4),
      strike = strike,
      expiry_date = expiry_date,
      days_to_expiry = as.numeric(expiry_date - alert_date),
      straddle_cost = round(cost, 2),
      exit_date = exit_row$date[1],
      exit_price = round(exit_row$straddle_value[1], 2),
      exit_pnl = round(exit_row$pnl[1], 2),
      exit_pnl_pct = round(exit_row$pnl_pct[1], 2),
      exit_reason = exit_reason,
      days_held = as.numeric(exit_row$date[1] - alert_date)
    )

    equity_marks[[length(equity_marks) + 1]] <- tibble(
      symbol = symbol,
      alert_date = alert_date,
      exit_date = exit_row$date[1],
      scaled_pnl = (exit_row$straddle_value[1] / cost * 1000) - 1000
    )
  }

  trades_df <- bind_rows(trades)
  equity_df <- bind_rows(equity_marks)

  if (nrow(trades_df) == 0) {
    return(list(
      summary = tibble(
        symbol = symbol,
        alerts = nrow(alerts),
        trades = 0,
        capital = 0,
        total_pnl = 0,
        total_return_pct = 0,
        win_rate_pct = 0,
        avg_hold_days = NA_real_,
        avg_pnl_trade = NA_real_,
        cdi_return_pct = 0,
        excess_vs_cdi = 0,
        outperf_x = NA_real_
      ),
      trades = tibble(),
      equity = tibble(),
      volatility = signal_df
    ))
  }

  trades_df <- trades_df %>%
    mutate(
      scale_factor = 1000 / straddle_cost,
      scaled_cost = 1000,
      scaled_exit = exit_price * scale_factor,
      scaled_pnl = scaled_exit - scaled_cost
    )

  capital <- nrow(trades_df) * 1000
  total_pnl <- sum(trades_df$scaled_pnl, na.rm = TRUE)
  total_return_pct <- 100 * total_pnl / capital
  win_rate_pct <- 100 * mean(trades_df$scaled_pnl > 0, na.rm = TRUE)
  avg_hold_days <- mean(trades_df$days_held, na.rm = TRUE)
  avg_pnl_trade <- mean(trades_df$scaled_pnl, na.rm = TRUE)
  period_days <- as.numeric(max(trades_df$exit_date) - min(trades_df$alert_date))
  cdi_pnl <- capital * ((1 + r_annual)^(period_days / 365) - 1)
  cdi_return_pct <- 100 * cdi_pnl / capital
  excess_vs_cdi <- total_pnl - cdi_pnl
  outperf_x <- ifelse(cdi_pnl > 0, total_pnl / cdi_pnl, NA_real_)

  summary_df <- tibble(
    symbol = symbol,
    alerts = nrow(alerts),
    trades = nrow(trades_df),
    capital = capital,
    total_pnl = total_pnl,
    total_return_pct = total_return_pct,
    win_rate_pct = win_rate_pct,
    avg_hold_days = avg_hold_days,
    avg_pnl_trade = avg_pnl_trade,
    cdi_return_pct = cdi_return_pct,
    excess_vs_cdi = excess_vs_cdi,
    outperf_x = outperf_x
  )

  list(
    summary = summary_df,
    trades = trades_df,
    equity = equity_df,
    volatility = signal_df
  )
}

results <- lapply(symbols, simulate_symbol)

summary_by_symbol <- bind_rows(lapply(results, function(x) x$summary))
trades_by_symbol <- bind_rows(lapply(results, function(x) x$trades))
equity_by_symbol <- bind_rows(lapply(results, function(x) x$equity))
volatility_all <- bind_rows(lapply(results, function(x) x$volatility))

portfolio_capital <- sum(summary_by_symbol$capital, na.rm = TRUE)
portfolio_pnl <- sum(summary_by_symbol$total_pnl, na.rm = TRUE)
portfolio_return_pct <- ifelse(portfolio_capital > 0, 100 * portfolio_pnl / portfolio_capital, 0)
portfolio_period_start <- min(trades_by_symbol$alert_date, na.rm = TRUE)
portfolio_period_end <- max(trades_by_symbol$exit_date, na.rm = TRUE)
portfolio_period_days <- as.numeric(portfolio_period_end - portfolio_period_start)
portfolio_cdi_pnl <- portfolio_capital * ((1 + r_annual)^(portfolio_period_days / 365) - 1)

portfolio_summary <- tibble(
  symbols_used = nrow(summary_by_symbol %>% filter(trades > 0)),
  total_trades = nrow(trades_by_symbol),
  capital = portfolio_capital,
  total_pnl = portfolio_pnl,
  total_return_pct = portfolio_return_pct,
  avg_hold_days = mean(trades_by_symbol$days_held, na.rm = TRUE),
  win_rate_pct = 100 * mean(trades_by_symbol$scaled_pnl > 0, na.rm = TRUE),
  cdi_pnl = portfolio_cdi_pnl,
  cdi_return_pct = ifelse(portfolio_capital > 0, 100 * portfolio_cdi_pnl / portfolio_capital, 0),
  excess_vs_cdi = portfolio_pnl - portfolio_cdi_pnl,
  outperf_x = ifelse(portfolio_cdi_pnl > 0, portfolio_pnl / portfolio_cdi_pnl, NA_real_)
)

write_csv(summary_by_symbol, "top5_straddle_summary_by_symbol.csv")
write_csv(trades_by_symbol, "top5_straddle_trades.csv")
write_csv(equity_by_symbol, "top5_straddle_equity_marks.csv")
write_csv(portfolio_summary, "top5_straddle_portfolio_summary.csv")
write_csv(volatility_all, "top5_straddle_volatility_frames.csv")

summary_plot <- ggplot(summary_by_symbol, aes(x = reorder(symbol, total_return_pct), y = total_return_pct, fill = symbol)) +
  geom_col(alpha = 0.85) +
  geom_text(aes(label = paste0(round(total_return_pct, 1), "%")), hjust = -0.1, size = 3.5) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(face = "bold")) +
  labs(
    title = "Top 5 Stocks - Straddle Return by Symbol",
    subtitle = "20% target | R$ 1,000 per alert",
    x = "Symbol",
    y = "Return on deployed capital (%)"
  )

ggsave("top5_straddle_return_by_symbol.png", summary_plot, width = 10, height = 6, dpi = 120)

equity_curve <- trades_by_symbol %>%
  arrange(alert_date) %>%
  mutate(cumulative_pnl = cumsum(scaled_pnl))

equity_plot <- ggplot(equity_curve, aes(x = alert_date, y = cumulative_pnl, color = symbol)) +
  geom_line(linewidth = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.4) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom") +
  labs(
    title = "Top 5 Stocks - Cumulative Straddle P&L",
    subtitle = "Chronological aggregation of all alerts",
    x = "Alert date",
    y = "Cumulative P&L (R$)",
    color = "Symbol"
  )

ggsave("top5_straddle_equity_curve.png", equity_plot, width = 12, height = 6, dpi = 120)

cat("\n=== SUMMARY BY SYMBOL ===\n")
print(summary_by_symbol)
cat("\n=== PORTFOLIO SUMMARY ===\n")
print(portfolio_summary)