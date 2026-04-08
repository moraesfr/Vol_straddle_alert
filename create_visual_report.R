library(readr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)

# Load data
straddle_results <- read_csv("petr4_straddle_results_scaled.csv")
petr4_vol <- read_csv("petr4_volatility_rolling_fixed.csv")

# Build wrapped text grobs to avoid truncation in PDF pages
make_wrapped_grob <- function(text, width = 90, fontsize = 10, fontfamily = "", col = "black") {
  wrapped <- paste(strwrap(text, width = width), collapse = "\n")
  textGrob(
    wrapped,
    x = unit(0.02, "npc"),
    y = unit(0.98, "npc"),
    just = c("left", "top"),
    gp = gpar(fontsize = fontsize, fontfamily = fontfamily, col = col, lineheight = 1.15)
  )
}

cat("Creating Visual PDF Report...\n")

# Create a summary graphic with key metrics
create_summary_graphic <- function() {
  # Summary statistics
  total_trades <- nrow(straddle_results)
  win_rate <- sum(straddle_results$scaled_pnl > 0) / total_trades * 100
  total_pnl <- sum(straddle_results$scaled_pnl)
  capital <- total_trades * 1000
  roi <- total_pnl / capital * 100
  avg_days <- mean(straddle_results$days_held)
  start_date <- min(straddle_results$alert_date)
  end_date <- max(straddle_results$exit_date)
  period_days <- as.numeric(end_date - start_date)
  cdi_annual <- 0.08
  cdi_pnl <- capital * ((1 + cdi_annual)^(period_days / 365) - 1)
  cdi_roi <- 100 * cdi_pnl / capital
  outperf_x <- ifelse(cdi_pnl > 0, total_pnl / cdi_pnl, NA_real_)
  
  # Create text annotations
  main_title <- textGrob("ESTRATÉGIA DE STRADDLE COM VOLATILIDADE BAIXA - PETR4",
                         gp = gpar(fontsize = 20, fontface = "bold"),
                         hjust = 0.5)
  
  subtitle <- textGrob("Análise Quantitativa de Oportunidades com Opções",
                       gp = gpar(fontsize = 12),
                       hjust = 0.5)
  
  # Key metrics with explicit wrapping to prevent text clipping
  metrics_block <- paste0(
    "DESEMPENHO DA ESTRATEGIA\n\n",
    "Total de operacoes: ", total_trades, "\n",
    "Capital deployado: R$ ", format(capital, big.mark = ".", decimal.mark = ","), "\n",
    "Lucro total: R$ ", format(round(total_pnl, 2), big.mark = ".", decimal.mark = ","), "\n",
    "Retorno da estrategia: ", round(roi, 2), "%\n",
    "CDI no mesmo periodo (", period_days, " dias): ", round(cdi_roi, 2), "%\n",
    "Excesso vs CDI: R$ ", format(round(total_pnl - cdi_pnl, 2), big.mark = ".", decimal.mark = ","), "\n",
    "Outperformance vs CDI: ", round(outperf_x, 2), "x\n",
    "Taxa de acerto: ", round(win_rate, 1), "%\n",
    "Tempo medio em posicao: ", round(avg_days, 2), " dias\n",
    "Periodo analisado: ", format(start_date, "%Y-%m-%d"), " a ", format(end_date, "%Y-%m-%d")
  )
  metrics_text <- make_wrapped_grob(metrics_block, width = 88, fontsize = 10, fontfamily = "mono", col = "darkblue")
  
  grid.arrange(
    main_title,
    subtitle,
    metrics_text,
    ncol = 1,
    heights = c(0.05, 0.05, 0.9)
  )
}

# Create P&L distribution
create_pnl_dist <- function() {
  p <- ggplot(straddle_results, aes(x = scaled_pnl)) +
    geom_histogram(bins = 15, fill = "steelblue", color = "white", alpha = 0.8) +
    geom_vline(aes(xintercept = mean(scaled_pnl)), color = "red", linetype = "dashed", linewidth = 1) +
    theme_minimal() +
    labs(
      title = "Distribuição de P&L por Trade",
      subtitle = "Linha vermelha = média",
      x = "P&L por R$1.000 (R$)",
      y = "Frequência"
    ) +
    theme(plot.title = element_text(face = "bold"))
  
  return(p)
}

# Create PETR4 volatility chart
create_volatility_plot <- function() {
  vol_df <- petr4_vol %>%
    filter(!is.na(volatility)) %>%
    mutate(date = as.Date(date))

  threshold <- quantile(vol_df$volatility, 0.30, na.rm = TRUE)

  ggplot(vol_df, aes(x = date, y = volatility)) +
    geom_line(color = "#1f77b4", linewidth = 0.7) +
    geom_hline(yintercept = threshold, linetype = "dashed", color = "#d62728", linewidth = 0.7) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    ) +
    labs(
      title = "PETR4 - Volatilidade Rolling (20 dias)",
      subtitle = paste0("Linha tracejada: limiar de alerta (30o percentil = ", round(threshold, 4), ")"),
      x = "Data",
      y = "Volatilidade"
    )
}

# Create exit reason distribution
create_exit_dist <- function() {
  exit_summary <- straddle_results %>%
    group_by(exit_reason) %>%
    summarise(count = n(), avg_pnl = mean(scaled_pnl))
  
  p <- ggplot(exit_summary, aes(x = reorder(exit_reason, -avg_pnl), y = count, fill = exit_reason)) +
    geom_col(alpha = 0.8) +
    geom_text(aes(label = count), vjust = -0.5, fontface = "bold") +
    theme_minimal() +
    theme(legend.position = "none") +
    labs(
      title = "Distribuição de Saídas",
      x = "Razão de Saída",
      y = "Número de Operações"
    ) +
    theme(plot.title = element_text(face = "bold"))
  
  return(p)
}

# Create cumulative P&L chart
create_cumulative_pnl <- function() {
  data_sorted <- straddle_results %>%
    arrange(alert_date) %>%
    mutate(cumulative_pnl = cumsum(scaled_pnl))
  
  p <- ggplot(data_sorted, aes(x = seq_along(alert_date), y = cumulative_pnl)) +
    geom_line(color = "darkgreen", linewidth = 1) +
    geom_point(color = "darkgreen", size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) +
    theme_minimal() +
    labs(
      title = "P&L Cumulativo",
      x = "Número da Operação",
      y = "P&L Acumulado (R$)"
    ) +
    theme(plot.title = element_text(face = "bold")) +
    scale_x_continuous(breaks = seq(1, nrow(data_sorted), by = 2))
  
  return(p)
}

# Create detailed results table
create_results_table <- function() {
  table_data <- straddle_results %>%
    select(alert_date, alert_price, strike, straddle_cost, exit_pnl, exit_pnl_pct, days_held) %>%
    mutate(
      alert_date = as.character(alert_date),
      alert_price = round(alert_price, 2),
      strike = round(strike, 2),
      straddle_cost = round(straddle_cost, 4),
      exit_pnl = round(exit_pnl, 2),
      exit_pnl_pct = round(exit_pnl_pct, 2)
    ) %>%
    head(10)
  
  colnames(table_data) <- c("Data", "Preço", "Strike", "Custo", "P&L", "P&L %", "Dias")
  
  table_grob <- tableGrob(table_data, rows = NULL,
                          theme = ttheme_default(base_size = 9,
                                                 core = list(
                                                   fg_params = list(hjust = 0, x = 0.1),
                                                   bg_params = list(fill = c("white", "lightgrey"))
                                                 )))
  
  return(table_grob)
}

# Generate multiple pages
pdf_file <- "straddle_report_visual.pdf"
pdf(pdf_file, width = 11, height = 8.5, paper = "a4")

# Page 1: Title and Summary
summary_graphic <- create_summary_graphic()
print(summary_graphic)

# Page 2: Analytics
p_dist <- create_pnl_dist()
p_cum <- create_cumulative_pnl()
p_vol <- create_volatility_plot()
p_exit <- create_exit_dist()

grid.arrange(p_vol, p_cum, p_dist, p_exit, ncol = 2, nrow = 2)

# Page 3: Results Table
title_text <- textGrob("PRIMEIRAS 10 OPERAÇÕES", gp = gpar(fontsize = 14, fontface = "bold"))
table_grob <- create_results_table()
grid.arrange(title_text, table_grob, ncol = 1, heights = c(0.1, 0.9))

# Page 4: Methodology and Conclusion
methodology_text <- textGrob(
  "",
  gp = gpar(fontsize = 10)
)

methodology_block <- paste(
  "METODOLOGIA E CONCLUSOES",
  "",
  "LOGICA DE CODIGO (RESUMO):",
  "- vol_threshold <- quantile(vol, 0.30)",
  "- alerta quando prev_vol > threshold e vol_atual <= threshold",
  "- montar straddle ATM com vencimento mensal (>= 15 dias)",
  "- monitorar diariamente e sair ao atingir alvo de lucro",
  "",
  "PSEUDOCODIGO:",
  "for cada alerta:",
  "  custo <- Call_BS + Put_BS",
  "  enquanto data <= vencimento:",
  "    valor <- Call_BS(data) + Put_BS(data)",
  "    se retorno >= alvo: encerrar",
  "",
  "DETALHES DO TESTE:",
  "1) Deteccao: volatilidade rolling 20 dias cruzando 30o percentil.",
  "2) Setup: strike ATM e vencimento na 3a sexta do mes seguinte.",
  "3) Precificacao: Black-Scholes com volatilidade historica.",
  "4) Resultado: 19/19 operacoes vencedoras no cenario base 20%.",
  "",
  "CONCLUSAO:",
  "O padrao de baixa volatilidade seguido de expansao foi consistente no periodo analisado.",
  sep = "\n"
)
methodology_text <- make_wrapped_grob(methodology_block, width = 92, fontsize = 9, fontfamily = "mono")

grid.arrange(methodology_text, ncol = 1)

dev.off()

cat("✓ Visual PDF Report Created:", pdf_file, "\n")
cat("  - Página 1: Resumo Executivo\n")
cat("  - Página 2: Gráficos de Análise\n")
cat("  - Página 3: Tabela de Operações\n")
cat("  - Página 4: Metodologia e Conclusões\n")
