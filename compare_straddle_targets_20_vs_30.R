library(readr)
library(dplyr)
library(ggplot2)

analyze_target <- function(file_path, target_label) {
  df <- read_csv(file_path, show_col_types = FALSE)
  df$alert_date <- as.Date(df$alert_date)
  df$exit_date <- as.Date(df$exit_date)

  trades <- nrow(df)
  capital <- trades * 1000
  total_pnl <- sum(df$scaled_pnl, na.rm = TRUE)
  total_return_pct <- 100 * total_pnl / capital
  win_rate_pct <- 100 * mean(df$scaled_pnl > 0, na.rm = TRUE)
  avg_hold_days <- mean(df$days_held, na.rm = TRUE)
  median_hold_days <- median(df$days_held, na.rm = TRUE)
  avg_pnl_trade <- mean(df$scaled_pnl, na.rm = TRUE)

  start_date <- min(df$alert_date, na.rm = TRUE)
  end_date <- max(df$exit_date, na.rm = TRUE)
  period_days <- as.numeric(end_date - start_date)

  cdi_annual <- 0.08
  cdi_pnl <- capital * ((1 + cdi_annual)^(period_days / 365) - 1)
  cdi_return_pct <- 100 * cdi_pnl / capital

  excess_vs_cdi <- total_pnl - cdi_pnl
  outperf_x <- ifelse(cdi_pnl > 0, total_pnl / cdi_pnl, NA_real_)

  tibble(
    target = target_label,
    trades = trades,
    capital = capital,
    total_pnl = total_pnl,
    total_return_pct = total_return_pct,
    win_rate_pct = win_rate_pct,
    avg_hold_days = avg_hold_days,
    median_hold_days = median_hold_days,
    avg_pnl_trade = avg_pnl_trade,
    start_date = start_date,
    end_date = end_date,
    period_days = period_days,
    cdi_pnl = cdi_pnl,
    cdi_return_pct = cdi_return_pct,
    excess_vs_cdi = excess_vs_cdi,
    outperf_x = outperf_x
  )
}

res_20 <- analyze_target("petr4_straddle_results_scaled.csv", "20%")
res_30 <- analyze_target("petr4_straddle_results_scaled_30pct.csv", "30%")
comparison <- bind_rows(res_20, res_30)

write_csv(comparison, "target_comparison_20_vs_30.csv")

# Long format for visualization without tidyr
plot_df <- bind_rows(
  comparison %>% transmute(target, metric = "Retorno da Estrategia (%)", value = total_return_pct),
  comparison %>% transmute(target, metric = "Retorno CDI (%)", value = cdi_return_pct),
  comparison %>% transmute(target, metric = "Win Rate (%)", value = win_rate_pct),
  comparison %>% transmute(target, metric = "Hold Medio (dias)", value = avg_hold_days)
)

p <- ggplot(plot_df, aes(x = target, y = value, fill = target)) +
  geom_col(alpha = 0.85, width = 0.65) +
  facet_wrap(~metric, scales = "free_y", ncol = 2) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "none",
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "Comparativo PETR4: Alvo 20% vs 30%",
    x = "Alvo de Saida",
    y = "Valor"
  )

ggsave("target_comparison_20_vs_30.png", p, width = 12, height = 8, dpi = 120)

cat("Comparativo salvo em target_comparison_20_vs_30.csv\n")
cat("Grafico salvo em target_comparison_20_vs_30.png\n\n")
print(comparison)

best <- comparison %>% arrange(desc(total_return_pct)) %>% slice(1)
cat("\nMelhor alvo por retorno total: ", best$target, " (", round(best$total_return_pct, 2), "%)\n", sep = "")
