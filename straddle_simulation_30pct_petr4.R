library(quantmod)
library(zoo)
library(dplyr)
library(tibble)
library(readr)
library(ggplot2)

# Black-Scholes option pricing
black_scholes <- function(S, K, T, r, sigma, type = "call") {
  if (T <= 0 | sigma <= 0) return(0)
  
  d1 <- (log(S / K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  
  if (type == "call") {
    value <- S * pnorm(d1) - K * exp(-r * T) * pnorm(d2)
  } else if (type == "put") {
    value <- K * exp(-r * T) * pnorm(-d2) - S * pnorm(-d1)
  } else {
    value <- 0
  }
  
  return(max(value, 0))
}

# Calculate straddle price
straddle_price <- function(S, K, T, r, sigma) {
  call_price <- black_scholes(S, K, T, r, sigma, type = "call")
  put_price <- black_scholes(S, K, T, r, sigma, type = "put")
  return(call_price + put_price)
}

# Get next trading day that is the 3rd Friday (monthly expiry)
get_next_monthly_expiry <- function(current_date) {
  # Find the next 3rd Friday
  test_date <- current_date + 1
  
  for (day_offset in 1:90) {
    test_date <- current_date + day_offset
    month_now <- as.numeric(format(current_date, "%m"))
    month_test <- as.numeric(format(test_date, "%m"))
    year_now <- as.numeric(format(current_date, "%Y"))
    year_test <- as.numeric(format(test_date, "%Y"))
    
    # If month or year changed, we're in next month
    if (month_test > month_now || (year_test > year_now)) {
      year_val <- as.numeric(format(test_date, "%Y"))
      month_val <- as.numeric(format(test_date, "%m"))
      
      # Start from first of the month
      first_day <- as.Date(paste(year_val, month_val, "01", sep = "-"))
      
      # Find 3rd Friday
      friday_count <- 0
      for (d in 1:31) {
        day_check <- first_day + (d - 1)
        month_check <- as.numeric(format(day_check, "%m"))
        if (month_check != month_val) break
        
        if (as.numeric(format(day_check, "%w")) == 5) {
          friday_count <- friday_count + 1
          if (friday_count == 3) {
            return(day_check)
          }
        }
      }
    }
  }
  
  return(current_date + 30)  # Fallback
}

cat("=== PETR4 STRADDLE STRATEGY SIMULATION (V230% TARGET) ===\n\n")

# Load corrected volatility data
petr4_vol <- read_csv("petr4_volatility_rolling_fixed.csv")
petr4_vol$date <- as.Date(petr4_vol$date)

# Define alert threshold (30th percentile)
vol_threshold <- quantile(petr4_vol$volatility, 0.30, na.rm = TRUE)

cat("Volatility threshold (30th percentile):", round(vol_threshold, 4), "\n")
cat("Min vol in data:", round(min(petr4_vol$volatility, na.rm = TRUE), 4), "\n")
cat("Max vol in data:", round(max(petr4_vol$volatility, na.rm = TRUE), 4), "\n\n")

# Generate alerts: when volatility crosses below threshold
petr4_vol <- petr4_vol %>%
  arrange(date) %>%
  mutate(
    prev_vol = lag(volatility),
    is_alert = (!is.na(prev_vol) & prev_vol > vol_threshold & volatility <= vol_threshold)
  )

alerts <- petr4_vol %>%
  filter(is_alert == TRUE) %>%
  select(date, close, volatility)

cat("Total alerts generated:", nrow(alerts), "\n\n")

if (nrow(alerts) == 0) {
  cat("No alerts found. Exiting.\n")
  quit(status = 1)
}

# Risk-free rate (annual)
r_annual <- 0.08

# Simulate straddles
straddle_trades <- list()
trade_counter <- 0

for (i in 1:nrow(alerts)) {
  alert_date <- alerts$date[i]
  alert_price <- alerts$close[i]
  alert_vol <- alerts$volatility[i]
  
  # Get expiry date
  expiry_date <- get_next_monthly_expiry(alert_date)
  
  # Check remaining trading days
  remaining_data <- petr4_vol %>%
    filter(date >= alert_date & date <= expiry_date)
  
  remaining_days <- nrow(remaining_data)
  
  if (remaining_days < 15) next  # Skip if less than 15 days
  
  trade_counter <- trade_counter + 1
  
  # Time to expiry
  T <- as.numeric(expiry_date - alert_date) / 365
  
  # Straddle setup
  K <- round(alert_price, 2)
  cost <- straddle_price(alert_price, K, T, r_annual, alert_vol)
  
  if (is.na(cost) | cost <= 0) next
  
  # Simulate through expiry
  position_data <- petr4_vol %>%
    filter(date >= alert_date & date <= expiry_date) %>%
    mutate(
      T_remaining = as.numeric(expiry_date - date) / 365,
      straddle_value = pmax(0, mapply(straddle_price, 
                                S = close, 
                                K = K, 
                                T = T_remaining, 
                                r = r_annual, 
                                sigma = volatility)),
      pnl = straddle_value - cost,
      pnl_pct = (pnl / cost) * 100,
      days_remaining = as.numeric(expiry_date - date)
    ) %>%
    na.omit()
  
  if (nrow(position_data) == 0) next
  
  # Find exit points
  
  
  exits_30 <- position_data %>% filter(pnl_pct >= 30) %>% slice(1)
  exits_exp <- position_data %>% slice(n())
  
  # Determine actual exit
  exit_date <- NULL
  exit_price <- NULL
  exit_pnl <- NULL
  exit_pnl_pct <- NULL
  exit_reason <- NULL
  
  if (nrow(exits_30) > 0) {
    exit_date <- exits_30$date[1]
    exit_price <- exits_30$straddle_value[1]
    exit_pnl <- exits_30$pnl[1]
    exit_pnl_pct <- exits_30$pnl_pct[1]
    exit_reason <- "30% target"
  } else {
    exit_date <- exits_exp$date[1]
    exit_price <- exits_exp$straddle_value[1]
    exit_pnl <- exits_exp$pnl[1]
    exit_pnl_pct <- exits_exp$pnl_pct[1]
    exit_reason <- "Expiry"
  }
  
  straddle_trades[[trade_counter]] <- tibble(
    trade_id = trade_counter,
    alert_date = alert_date,
    alert_price = round(alert_price, 2),
    alert_vol = round(alert_vol, 4),
    strike = K,
    expiry_date = expiry_date,
    days_to_expiry = as.numeric(expiry_date - alert_date),
    straddle_cost = round(cost, 2),
    exit_date = exit_date,
    exit_price = round(exit_price, 2),
    exit_pnl = round(exit_pnl, 2),
    exit_pnl_pct = round(exit_pnl_pct, 2),
    exit_reason = exit_reason,
    days_held = as.numeric(exit_date - alert_date)
  )
}

cat("Straddles executed (15+ days to expiry):", length(straddle_trades), "\n\n")

if (length(straddle_trades) == 0) {
  cat("No valid straddles to execute. Exiting.\n")
  quit(status = 1)
}

# Combine results
straddle_results <- bind_rows(straddle_trades)

# Save raw results
write_csv(straddle_results, "petr4_straddle_results_30pct.csv")

cat("=== STRADDLE PERFORMANCE SUMMARY ===\n\n")

winning_trades <- sum(straddle_results$exit_pnl > 0)
losing_trades <- sum(straddle_results$exit_pnl <= 0)
win_rate <- winning_trades / nrow(straddle_results) * 100

cat("Winning trades (PnL > 0):", winning_trades, "\n")
cat("Losing trades (PnL <= 0):", losing_trades, "\n")
cat("Win rate:", round(win_rate, 2), "%\n\n")

cat("Average PnL per trade: R$", round(mean(straddle_results$exit_pnl), 2), "\n")
cat("Median PnL per trade: R$", round(median(straddle_results$exit_pnl), 2), "\n")
cat("Average return:  ", round(mean(straddle_results$exit_pnl_pct), 2), "%\n")
cat("Average hold time:", round(mean(straddle_results$days_held)), "days\n\n")

# Exit reasons distribution
cat("Exit reasons:\n")
exit_summary <- straddle_results %>%
  group_by(exit_reason) %>%
  summarise(
    count = n(),
    avg_pnl = mean(exit_pnl),
    avg_pnl_pct = mean(exit_pnl_pct)
  )
print(as.data.frame(exit_summary))

# Calculate for R$1.000 per trade
cat("\n=== R$1.000 PER ALERT SIMULATION ===\n\n")

straddle_results <- straddle_results %>%
  mutate(
    scale_factor = 1000 / straddle_cost,
    scaled_cost = 1000,
    scaled_exit = exit_price * scale_factor,
    scaled_pnl = scaled_exit - scaled_cost
  )

total_capital <- nrow(straddle_results) * 1000
total_pnl <- sum(straddle_results$scaled_pnl)
total_return <- total_pnl / total_capital * 100

cat("Total trades:", nrow(straddle_results), "\n")
cat("Total capital deployed: R$", format(total_capital, big.mark = ".", decimal.mark = ","), "\n")
cat("Average PnL per R$1.000:", round(mean(straddle_results$scaled_pnl), 2), "\n")
cat("Total net PnL: R$", format(round(total_pnl, 2), big.mark = ".", decimal.mark = ","), "\n")
cat("Overall return on capital:", round(total_return, 2), "%\n\n")

cat("Best trade (R$1.000 basis): R$", round(max(straddle_results$scaled_pnl), 2), "\n")
cat("Worst trade (R$1.000 basis): R$", round(min(straddle_results$scaled_pnl), 2), "\n")

# Save final results with scaling
write_csv(straddle_results, "petr4_straddle_results_scaled_30pct.csv")

# Plot equity curve
straddle_sorted <- straddle_results %>%
  arrange(alert_date) %>%
  mutate(cumulative_pnl = cumsum(scaled_pnl))

p <- ggplot(straddle_sorted, aes(x = alert_date, y = cumulative_pnl, color = exit_reason)) +
  geom_line(color = "steelblue", size = 1, linewidth = 1) +
  geom_point(size = 2.5, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "PETR4 Straddle Strategy - Cumulative P&L (R$1.000 per trade)",
    x = "Trade Entry Date",
    y = "Cumulative P&L (R$)",
    color = "Exit Reason"
  ) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months")

ggsave("petr4_straddle_equity_curve_30pct.png", p, width = 14, height = 6, dpi = 100)
cat("\n✓ Saved: petr4_straddle_equity_curve_30pct.png\n")
cat("✓ Saved: petr4_straddle_results_scaled_30pct.csv\n")
