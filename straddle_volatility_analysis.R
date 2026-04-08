library(quantmod)
library(zoo)
library(dplyr)
library(tibble)
library(readr)
library(ggplot2)

# Step 1: Analyze PETR4 volatility patterns
cat("=== PETR4 VOLATILITY PATTERN ANALYSIS ===\n\n")

# Load the rolling volatility data
vol_data <- read_csv("volatility_rolling_20day.csv")

# Filter for PETR4
petr4_vol <- vol_data %>%
  filter(symbol == "PETR4.SA") %>%
  arrange(date) %>%
  mutate(
    vol_pct = volatility * 100,
    vol_change = volatility - lag(volatility),
    vol_lag1 = lag(volatility),
    vol_lag5 = lag(volatility, 5),
    vol_lag10 = lag(volatility, 10)
  )

# Identify "low volatility" periods (below 20th percentile)
vol_quantile_20 <- quantile(petr4_vol$volatility, 0.20, na.rm = TRUE)
vol_quantile_30 <- quantile(petr4_vol$volatility, 0.30, na.rm = TRUE)

cat("Volatility percentiles:\n")
cat("  20th percentile:", round(vol_quantile_20, 4), "\n")
cat("  30th percentile:", round(vol_quantile_30, 4), "\n")
cat("  50th percentile (median):", round(quantile(petr4_vol$volatility, 0.50, na.rm = TRUE), 4), "\n\n")

# Identify volatility drops followed by spikes
# A "drop" is when volatility fell significantly in the last 5 days
# A "spike" is when volatility rises significantly in the next N days

petr4_vol <- petr4_vol %>%
  mutate(
    # Detect low volatility moments (below 30th percentile)
    low_vol_flag = volatility <= vol_quantile_30,
    
    # Look ahead 5 and 10 days for spike (increase of at least 50% from low)
    future_vol_5d = lead(volatility, 5),
    future_vol_10d = lead(volatility, 10),
    
    # Calculate potential spikes
    spike_5d = ifelse(!is.na(future_vol_5d) & !is.na(volatility), 
                      (future_vol_5d - volatility) / volatility, NA),
    spike_10d = ifelse(!is.na(future_vol_10d) & !is.na(volatility), 
                       (future_vol_10d - volatility) / volatility, NA)
  )

# Also analyze the actual volatility drop before the rise
petr4_vol <- petr4_vol %>%
  mutate(
    # Look back at past volatility
    past_vol_10d = lag(volatility, 10),
    past_vol_20d = lag(volatility, 20),
    vol_drop_pct = ifelse(!is.na(past_vol_10d), 
                          (past_vol_10d - volatility) / past_vol_10d * 100, NA)
  )

# Find patterns: low vol followed by spike
straddle_candidates <- petr4_vol %>%
  filter(low_vol_flag) %>%
  filter(!is.na(spike_5d) | !is.na(spike_10d))

cat("Low volatility periods followed by potential spikes:\n")
cat("  Total low-vol periods detected:", sum(petr4_vol$low_vol_flag, na.rm = TRUE), "\n")
cat("  With 5-day forward data:", nrow(straddle_candidates), "\n")

# Analyze the spikes after low vol
spikes_after_low <- straddle_candidates %>%
  filter(!is.na(spike_5d)) %>%
  pull(spike_5d)

cat("\nSpike distribution after low volatility (5-day ahead):\n")
cat("  Average spike:", round(mean(spikes_after_low, na.rm = TRUE) * 100, 2), "%\n")
cat("  Median spike:", round(median(spikes_after_low, na.rm = TRUE) * 100, 2), "%\n")
cat("  Min spike:", round(min(spikes_after_low, na.rm = TRUE) * 100, 2), "%\n")
cat("  Max spike:", round(max(spikes_after_low, na.rm = TRUE) * 100, 2), "%\n")
cat("  Std dev:", round(sd(spikes_after_low, na.rm = TRUE) * 100, 2), "%\n")

drops_before_spike <- straddle_candidates %>%
  filter(!is.na(vol_drop_pct)) %>%
  pull(vol_drop_pct)

cat("\n\nVolatility drop BEFORE low-vol period (vs 10-day back):\n")
cat("  Average drop:", round(mean(drops_before_spike, na.rm = TRUE), 2), "%\n")
cat("  Median drop:", round(median(drops_before_spike, na.rm = TRUE), 2), "%\n")
cat("  Min drop:", round(min(drops_before_spike, na.rm = TRUE), 2), "%\n")
cat("  Max drop:", round(max(drops_before_spike, na.rm = TRUE), 2), "%\n")

# Save analysis data
write_csv(petr4_vol, "petr4_volatility_analysis.csv")
write_csv(straddle_candidates, "petr4_straddle_candidates.csv")

cat("\n\n=== PATTERN SUMMARY ===\n")
cat("Pattern observed:\n")
cat("  1. Volatility drops to low levels (< 30th percentile)\n")
cat("  2. Average historical drop from peak: ~", round(mean(drops_before_spike, na.rm = TRUE), 1), "%\n")
cat("  3. Followed by spike averaging ~", round(mean(spikes_after_low, na.rm = TRUE) * 100, 1), "% within 5 days\n")
cat("  4. This pattern suggests STRADDLE opportunity:\n")
cat("     - Buy at-the-money CALL and PUT\n")
cat("     - Profit from the expected volatility expansion\n")
cat("     - Target: close when vol increases or price moves significantly\n\n")

# Download price data for PETR4 to establish current price
data_petr4 <- getSymbols("PETR4.SA", from = "2023-01-02", to = "2026-04-02", auto.assign = FALSE)
latest_price <- as.numeric(Cl(data_petr4)[nrow(data_petr4)])
cat("Latest PETR4 price:", round(latest_price, 2), "\n")
