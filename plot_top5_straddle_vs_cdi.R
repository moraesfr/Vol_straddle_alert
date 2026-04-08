library(readr)
library(dplyr)
library(ggplot2)

summary_by_symbol <- read_csv("top5_straddle_summary_by_symbol.csv", show_col_types = FALSE)
portfolio_summary <- read_csv("top5_straddle_portfolio_summary.csv", show_col_types = FALSE)

comparison_long <- bind_rows(
  summary_by_symbol %>%
    transmute(symbol, series = "Strategy", return_pct = total_return_pct),
  summary_by_symbol %>%
    transmute(symbol, series = "CDI", return_pct = cdi_return_pct)
)

p_symbol <- ggplot(comparison_long, aes(x = reorder(symbol, return_pct), y = return_pct, fill = series)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.68) +
  coord_flip() +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "Top 5 Straddle Study - Return vs CDI by Symbol",
    subtitle = "20% target | R$ 1,000 per alert",
    x = "Symbol",
    y = "Return on deployed capital (%)",
    fill = "Series"
  )

ggsave("top5_straddle_vs_cdi_by_symbol.png", p_symbol, width = 10, height = 6, dpi = 120)

portfolio_df <- tibble(
  series = c("Strategy", "CDI"),
  return_pct = c(portfolio_summary$total_return_pct[1], portfolio_summary$cdi_return_pct[1]),
  pnl = c(portfolio_summary$total_pnl[1], portfolio_summary$cdi_pnl[1])
)

p_portfolio <- ggplot(portfolio_df, aes(x = series, y = return_pct, fill = series)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(round(return_pct, 1), "%")), vjust = -0.35, size = 4) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none",
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "Top 5 Portfolio - Strategy vs CDI",
    subtitle = paste0("Capital: R$ ", format(round(portfolio_summary$capital[1], 0), big.mark = ".", decimal.mark = ",")),
    x = "Series",
    y = "Return (%)"
  )

ggsave("top5_portfolio_vs_cdi.png", p_portfolio, width = 8, height = 5, dpi = 120)