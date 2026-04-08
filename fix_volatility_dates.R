library(quantmod)
library(zoo)
library(dplyr)
library(tibble)
library(readr)
library(ggplot2)

cat("=== PETR4 VOLATILITY ROLLING CALCULATION (FIXED) ===\n\n")

# Download price data
data_petr4 <- getSymbols("PETR4.SA", from = "2023-01-02", to = "2026-04-02", auto.assign = FALSE)

# Extract closing prices with date
close_prices <- Cl(data_petr4)

# Calculate log returns
log_returns <- diff(log(close_prices), lag = 1)
log_returns <- log_returns[!is.na(log_returns)]

# Create a data frame with dates for rolling volatility
dates <- as.Date(index(log_returns))
returns <- as.numeric(log_returns)

# Function to calculate rolling volatility properly
calc_rolling_vol <- function(returns_vec, window = 20) {
  n <- length(returns_vec)
  rolling_vol <- rep(NA, n)
  
  for (i in window:n) {
    rolling_vol[i] <- sd(returns_vec[(i-window+1):i], na.rm = TRUE)
  }
  
  return(rolling_vol)
}

# Calculate rolling volatility
rolling_vol <- calc_rolling_vol(returns, window = 20)

# Create data frame with proper dates
petr4_vol <- data.frame(
  date = dates,
  volatility = rolling_vol,
  returns = returns,
  symbol = "PETR4.SA",
  stringsAsFactors = FALSE
)

petr4_vol$date <- as.Date(petr4_vol$date)

# Add close prices
close_df <- data.frame(
  date = as.Date(index(data_petr4)),
  close = as.numeric(Cl(data_petr4)),
  stringsAsFactors = FALSE
)

petr4_vol <- petr4_vol %>%
  left_join(close_df, by = "date")

cat("Date range check:\n")
cat("First date:", as.character(min(petr4_vol$date)), "\n")
cat("Last date:", as.character(max(petr4_vol$date)), "\n")
cat("Total rows:", nrow(petr4_vol), "\n")
cat("Rows with NA volatility:", sum(is.na(petr4_vol$volatility)), "\n\n")

# Save this for later use
write_csv(petr4_vol, "petr4_volatility_rolling_fixed.csv")

cat("✓ Saved: petr4_volatility_rolling_fixed.csv\n")
