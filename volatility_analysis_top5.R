library(quantmod)
library(zoo)
library(dplyr)
library(tibble)
library(readr)
library(ggplot2)

# Define the 5 main stocks
symbols <- c("PETR4.SA", "VALE3.SA", "ITUB4.SA", "BBDC4.SA", "ABEV3.SA")

# Date range from the strategy backtest
start_date <- as.Date("2023-01-02")
end_date <- as.Date("2026-04-02")

# Download price data
cat("Downloading price data for", length(symbols), "stocks...\n")
prices_list <- list()

for (symbol in symbols) {
  tryCatch({
    data <- getSymbols(symbol, from = start_date, to = end_date, auto.assign = FALSE)
    prices_list[[symbol]] <- data
    cat(symbol, ": OK\n")
  }, error = function(e) {
    cat("Error downloading", symbol, "\n")
  })
}

# Calculate rolling volatility (20-day rolling standard deviation of log returns)
# Also calculate overall volatility
cat("\nCalculating volatility...\n")

volatility_data <- data.frame()
volatility_summary <- data.frame()

for (symbol in names(prices_list)) {
  data <- prices_list[[symbol]]
  
  # Extract closing prices
  close <- Cl(data)
  
  # Calculate log returns
  log_returns <- diff(log(close), lag = 1)
  log_returns <- log_returns[!is.na(log_returns)]
  
  # Calculate rolling volatility (20-day)
  rolling_vol <- zoo::rollapply(log_returns, width = 20, FUN = sd, by.column = TRUE)
  rolling_vol <- as.data.frame(rolling_vol)
  rolling_vol$date <- index(rolling_vol)
  rolling_vol$symbol <- symbol
  colnames(rolling_vol)[1] <- "volatility"
  rolling_vol <- rolling_vol[, c("date", "symbol", "volatility")]
  
  volatility_data <- rbind(volatility_data, as.data.frame(rolling_vol))
  
  # Summary statistics
  overall_vol <- sd(as.numeric(log_returns), na.rm = TRUE)
  annualized_vol <- overall_vol * sqrt(252)  # 252 trading days per year
  
  volatility_summary <- rbind(volatility_summary, data.frame(
    symbol = symbol,
    daily_volatility = overall_vol,
    annualized_volatility = annualized_vol,
    min_rolling_vol = min(rolling_vol$volatility, na.rm = TRUE),
    max_rolling_vol = max(rolling_vol$volatility, na.rm = TRUE),
    mean_rolling_vol = mean(rolling_vol$volatility, na.rm = TRUE)
  ))
}

# Clean data
volatility_data <- na.omit(volatility_data)
volatility_data$date <- as.Date(volatility_data$date)

cat("\nVolatility Summary:\n")
print(volatility_summary)

# Save summary
write_csv(volatility_summary, "volatility_summary.csv")
write_csv(volatility_data, "volatility_rolling_20day.csv")

# Plot 1: Rolling volatility over time
p1 <- ggplot(volatility_data, aes(x = date, y = volatility, color = symbol)) +
  geom_line(alpha = 0.7, size = 0.8) +
  facet_wrap(~symbol, ncol = 2, scales = "free_y") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom"
  ) +
  labs(
    title = "Rolling 20-Day Volatility (Daily Log Returns)",
    subtitle = paste("Period:", start_date, "to", end_date),
    x = "Date",
    y = "Volatility (σ)",
    color = "Stock"
  )

ggsave("volatility_rolling_by_symbol.png", p1, width = 14, height = 8, dpi = 100)
cat("Saved: volatility_rolling_by_symbol.png\n")

# Plot 2: Volatility comparison (bar chart of annualized)
volatility_summary_sorted <- volatility_summary %>% arrange(desc(annualized_volatility))

p2 <- ggplot(volatility_summary_sorted, aes(x = reorder(symbol, -annualized_volatility), y = annualized_volatility)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  geom_text(aes(label = paste0(round(annualized_volatility * 100, 1), "%")), 
            vjust = -0.5, size = 4, fontface = "bold") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    title = "Annualized Volatility Comparison",
    subtitle = paste("Period:", start_date, "to", end_date),
    x = "Stock",
    y = "Annualized Volatility (%)"
  ) +
  scale_y_continuous(labels = function(x) paste0(x * 100, "%"))

ggsave("volatility_comparison_annualized.png", p2, width = 10, height = 6, dpi = 100)
cat("Saved: volatility_comparison_annualized.png\n")

# Plot 3: Box plot of rolling volatility distributions
p3 <- ggplot(volatility_data, aes(x = symbol, y = volatility, fill = symbol)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    title = "Distribution of 20-Day Rolling Volatility",
    subtitle = paste("Period:", start_date, "to", end_date),
    x = "Stock",
    y = "Volatility (σ)"
  )

ggsave("volatility_distribution_boxplot.png", p3, width = 10, height = 6, dpi = 100)
cat("Saved: volatility_distribution_boxplot.png\n")

cat("\n✓ Volatility analysis complete!\n")
cat("Generated files:\n")
cat("  - volatility_summary.csv\n")
cat("  - volatility_rolling_20day.csv\n")
cat("  - volatility_rolling_by_symbol.png\n")
cat("  - volatility_comparison_annualized.png\n")
cat("  - volatility_distribution_boxplot.png\n")
