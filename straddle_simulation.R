library(quantmod)
library(zoo)
library(dplyr)
library(tibble)
library(readr)
library(ggplot2)

# Black-Scholes option pricing
black_scholes <- function(S, K, T, r, sigma, type = "call") {
  d1 <- (log(S / K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  
  if (type == "call") {
    value <- S * pnorm(d1) - K * exp(-r * T) * pnorm(d2)
  } else if (type == "put") {
    value <- K * exp(-r * T) * pnorm(-d2) - S * pnorm(-d1)
  }
  
  return(value)
}

# Calculate straddle price
straddle_price <- function(S, K, T, r, sigma) {
  call_price <- black_scholes(S, K, T, r, sigma, type = "call")
  put_price <- black_scholes(S, K, T, r, sigma, type = "put")
  return(call_price + put_price)
}

# Get next trading day that is the 3rd Friday (monthly expiry)
get_next_monthly_expiry <- function(current_date) {
  # Start from current date and look forward
  test_date <- current_date + 1
  
  for (day_offset in 1:90) {
    test_date <- current_date + day_offset
    month_now <- as.numeric(format(current_date, "%m"))
    month_test <- as.numeric(format(test_date, "%m"))
    
    # If month changed, we're in next month
    if (month_test > month_now || (month_now == 12 && month_test == 1)) {
      # Found next month, now backtrack to find 3rd Friday
      year_val <- as.numeric(format(test_date, "%Y"))
      month_val <- as.numeric(format(test_date, "%m"))
      
      # Start from first of the month
      first_day <- as.Date(paste(year_val, month_val, "01", sep = "-"))
      
      # Find Fridays (weekday 5 = Friday)
      friday_count <- 0
      for (d in 1:28) {
        day_check <- first_day + (d - 1)
        if (as.numeric(format(day_check, "%w")) == 5) {
          friday_count <- friday_count + 1
          if (friday_count == 3) {
            return(day_check)
          }
        }
      }
    }
  }
  
  return(current_date + 30)  # Fallback to 30 days
}

cat("=== PETR4 STRADDLE STRATEGY SIMULATION ===\n\n")

# Load data
petr4_vol <- read_csv("petr4_volatility_analysis.csv")
petr4_vol$date <- as.Date(petr4_vol$date)  # Ensure date is proper Date class

data_petr4 <- getSymbols("PETR4.SA", from = "2023-01-02", to = "2026-04-02", auto.assign = FALSE)

# Extract prices - ensure proper date conversion
prices_df <- data.frame(
  date = as.Date(index(data_petr4)),
  close = as.numeric(Cl(data_petr4)),
  stringsAsFactors = FALSE
)

# Merge with volatility data
petr4_full <- petr4_vol %>%
  left_join(prices_df, by = "date") %>%
  arrange(date) %>%
  filter(!is.na(close))

# Define alert threshold
vol_threshold <- quantile(petr4_vol$volatility, 0.30, na.rm = TRUE)

cat("Volatility threshold: ", round(vol_threshold, 4), "\n")
cat("Min vol in data: ", round(min(petr4_full$volatility, na.rm=TRUE), 4), "\n")
cat("Max vol in data: ", round(max(petr4_full$volatility, na.rm=TRUE), 4), "\n")

# Generate alerts: when volatility crosses below threshold
petr4_full <- petr4_full %>%
  mutate(
    prev_vol = lag(volatility),
    is_alert = (!is.na(prev_vol) & prev_vol > vol_threshold & volatility <= vol_threshold)
  )

# Also add manual check for days where vol <= threshold
petr4_full <- petr4_full %>%
  mutate(
    is_alert = is_alert | (is.na(is_alert) & volatility <= vol_threshold & row_number() == 1),
    days_at_low = 0
  )

alerts <- petr4_full %>%
  filter(is_alert == TRUE) %>%
  select(date, close, volatility, is_alert)

cat("Alerts found: ", nrow(alerts), "\n")

# Risk-free rate assumption (annual)
r_annual <- 0.08  # 8% annual

# Simulate straddles
straddle_trades <- list()
trade_counter <- 0

for (i in 1:nrow(alerts)) {
  alert_date <- alerts$date[i]
  alert_price <- alerts$close[i]
  alert_vol <- alerts$volatility[i]
  
  # Get expiry date
  expiry_date <- get_next_monthly_expiry(alert_date)
  
  # Check remaining dates - we need at least 15 days
  remaining_dates <- petr4_full %>%
    filter(date >= alert_date & date <= expiry_date) %>%
    nrow()
  
  if (remaining_dates < 15) next  # Skip if less than 15 days
  
  trade_counter <- trade_counter + 1
  
  # Time to expiry in years
  T <- as.numeric(expiry_date - alert_date) / 365
  
  # Straddle setup
  K <- round(alert_price, 2)  # Strike = current price (ATM)
  cost <- straddle_price(alert_price, K, T, r_annual, alert_vol)
  
  # Simulate the position through expiry
  position_data <- petr4_full %>%
    filter(date >= alert_date & date <= expiry_date) %>%
    mutate(
      T_remaining = as.numeric(expiry_date - date) / 365,
      straddle_value = mapply(straddle_price, 
                              S = close, 
                              K = K, 
                              T = T_remaining, 
                              r = r_annual, 
                              sigma = volatility),
      pnl = straddle_value - cost,
      pnl_pct = (pnl / cost) * 100,
      days_remaining = as.numeric(expiry_date - date)
    )
  
  # Find exit point (10%, 15%, or 20% profit, whichever comes first)
  exits <- list(
    exit_10pct = position_data %>% filter(pnl_pct >= 10) %>% slice(1),
    exit_15pct = position_data %>% filter(pnl_pct >= 15) %>% slice(1),
    exit_20pct = position_data %>% filter(pnl_pct >= 20) %>% slice(1),
    exit_expiry = position_data %>% slice(n())
  )
  
  # Determine actual exit
  exit_date <- NULL
  exit_price <- NULL
  exit_pnl <- NULL
  exit_pnl_pct <- NULL
  exit_reason <- NULL
  
  if (nrow(exits$exit_20pct) > 0) {
    exit_date <- exits$exit_20pct$date[1]
    exit_price <- exits$exit_20pct$straddle_value[1]
    exit_pnl <- exits$exit_20pct$pnl[1]
    exit_pnl_pct <- exits$exit_20pct$pnl_pct[1]
    exit_reason <- "20% target"
  } else if (nrow(exits$exit_15pct) > 0) {
    exit_date <- exits$exit_15pct$date[1]
    exit_price <- exits$exit_15pct$straddle_value[1]
    exit_pnl <- exits$exit_15pct$pnl[1]
    exit_pnl_pct <- exits$exit_15pct$pnl_pct[1]
    exit_reason <- "15% target"
  } else if (nrow(exits$exit_10pct) > 0) {
    exit_date <- exits$exit_10pct$date[1]
    exit_price <- exits$exit_10pct$straddle_value[1]
    exit_pnl <- exits$exit_10pct$pnl[1]
    exit_pnl_pct <- exits$exit_10pct$pnl_pct[1]
    exit_reason <- "10% target"
  } else {
    exit_date <- exits$exit_expiry$date[1]
    exit_price <- exits$exit_expiry$straddle_value[1]
    exit_pnl <- exits$exit_expiry$pnl[1]
    exit_pnl_pct <- exits$exit_expiry$pnl_pct[1]
    exit_reason <- "Expiry"
  }
  
  straddle_trades[[trade_counter]] <- tibble(
    trade_id = trade_counter,
    alert_date = alert_date,
    alert_price = alert_price,
    alert_vol = alert_vol,
    strike = K,
    expiry_date = expiry_date,
    days_to_expiry = as.numeric(expiry_date - alert_date),
    straddle_cost = cost,
    exit_date = exit_date,
    exit_price = exit_price,
    exit_pnl = exit_pnl,
    exit_pnl_pct = exit_pnl_pct,
    exit_reason = exit_reason,
    days_held = as.numeric(exit_date - alert_date)
  )
}

# Combine all trades
straddle_results <- bind_rows(straddle_trades)

cat("Straddles with 15+ days to expiry:", nrow(straddle_results), "\n\n")

# Summary statistics
cat("=== STRADDLE PERFORMANCE SUMMARY ===\n\n")
cat("Winning trades (PnL > 0):", sum(straddle_results$exit_pnl > 0), "\n")
cat("Losing trades (PnL <= 0):", sum(straddle_results$exit_pnl <= 0), "\n")
cat("Win rate:", round(sum(straddle_results$exit_pnl > 0) / nrow(straddle_results) * 100, 2), "%\n\n")

cat("Average PnL per trade: R$", round(mean(straddle_results$exit_pnl), 2), "\n")
cat("Median PnL per trade: R$", round(median(straddle_results$exit_pnl), 2), "\n")
cat("Total PnL:", sum(straddle_results$exit_pnl > 0) - sum(straddle_results$exit_pnl <= 0), "trades\n")

cat("Average hold time:", round(mean(straddle_results$days_held)), "days\n")
cat("Average return:", round(mean(straddle_results$exit_pnl_pct), 2), "%\n\n")

# Exit reasons distribution
cat("Exit reasons:\n")
exit_summary <- straddle_results %>%
  group_by(exit_reason) %>%
  summarise(count = n(), avg_pnl_pct = mean(exit_pnl_pct))
print(exit_summary)

# Calculate for R$1.000 per trade
cat("\n=== R$1.000 PER ALERT SIMULATION ===\n\n")

# Scale each trade cost to R$1.000
straddle_results <- straddle_results %>%
  mutate(
    scale_factor = 1000 / straddle_cost,
    scaled_cost = 1000,
    scaled_exit = exit_price * scale_factor,
    scaled_pnl = scaled_exit - scaled_cost,
    scaled_pnl_total = scaled_pnl
  )

cat("Total trades analyzed:", nrow(straddle_results), "\n")
cat("Total capital deployed:", nrow(straddle_results) * 1000, "\n")
cat("Average PnL per R$1.000:", round(mean(straddle_results$scaled_pnl), 2), "\n")
cat("Total net PnL:", round(sum(straddle_results$scaled_pnl), 2), "\n")
cat("Return on total capital:", round(sum(straddle_results$scaled_pnl) / (nrow(straddle_results) * 1000) * 100, 2), "%\n\n")

# Best and worst trades
cat("Best trade (R$1.000 basis):", round(max(straddle_results$scaled_pnl), 2), "\n")
cat("Worst trade (R$1.000 basis):", round(min(straddle_results$scaled_pnl), 2), "\n")

# Save results
write_csv(straddle_results %>% select(-scale_factor), "petr4_straddle_results.csv")

# Plot equity curve
straddle_results_sorted <- straddle_results %>%
  arrange(alert_date) %>%
  mutate(cumulative_pnl = cumsum(scaled_pnl))

p <- ggplot(straddle_results_sorted, aes(x = alert_date, y = cumulative_pnl)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(aes(color = exit_reason), size = 2, alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom"
  ) +
  labs(
    title = "PETR4 Straddle Strategy - Cumulative P&L (R$1.000 per trade)",
    x = "Trade Entry Date",
    y = "Cumulative P&L (R$)",
    color = "Exit Reason"
  )

ggsave("petr4_straddle_equity_curve.png", p, width = 14, height = 6, dpi = 100)
cat("\n✓ Saved: petr4_straddle_equity_curve.png\n")
