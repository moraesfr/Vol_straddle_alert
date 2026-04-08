library(readr)
library(dplyr)
library(ggplot2)

vol_df <- read_csv("petr4_volatility_rolling_fixed.csv", show_col_types = FALSE) %>%
  mutate(date = as.Date(date)) %>%
  filter(!is.na(volatility)) %>%
  arrange(date)

threshold <- quantile(vol_df$volatility, 0.30, na.rm = TRUE)

plot_df <- vol_df %>%
  mutate(
    prev_vol = lag(volatility),
    is_alert = !is.na(prev_vol) & prev_vol > threshold & volatility <= threshold
  )

p <- ggplot(plot_df, aes(x = date, y = volatility)) +
  geom_line(color = "#1f77b4", linewidth = 0.8) +
  geom_hline(
    yintercept = threshold,
    color = "#d62728",
    linetype = "dashed",
    linewidth = 0.8
  ) +
  geom_point(
    data = plot_df %>% filter(is_alert),
    aes(x = date, y = volatility),
    color = "#ff7f0e",
    size = 2.2,
    alpha = 0.9
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "PETR4 - Volatilidade Rolling com Limiar de Alerta",
    subtitle = paste0(
      "Linha tracejada = 30o percentil (",
      round(threshold, 4),
      ") | Pontos laranja = alertas"
    ),
    x = "Data",
    y = "Volatilidade rolling (20 dias)"
  )

ggsave("petr4_volatility_threshold_alerts.png", p, width = 12, height = 6, dpi = 140)

cat("Arquivo gerado: petr4_volatility_threshold_alerts.png\n")
cat("Threshold (30o percentil):", round(threshold, 4), "\n")
cat("Total de alertas marcados:", sum(plot_df$is_alert, na.rm = TRUE), "\n")
