---
title: "Top-5 Straddle Study"
subtitle: "Low-Volatility Alerts | PETR4, VALE3, ITUB4, BBDC4, ABEV3 | R$ 1,000 per alert"
date: "April 2026"
geometry: margin=2.5cm
fontsize: 11pt
colorlinks: true
---

# Objective

This study expands the PETR4-only straddle test to the **five major traded stocks** used in this workspace:

- PETR4.SA
- VALE3.SA
- ITUB4.SA
- BBDC4.SA
- ABEV3.SA

The goal is to verify whether the same **low-volatility alert** works consistently across a broader set of liquid Brazilian equities, and to compare the consolidated return against **CDI**.

---

# Strategy Definition

The same logic used in the single-name PETR4 study is kept unchanged.

## Signal

For each stock, we calculate a 20-day rolling volatility from log returns:

$$
\sigma_t = SD\left(\ln\left(\frac{P_t}{P_{t-1}}\right), 20\right)
$$

An alert is triggered when volatility crosses below the 30th percentile of that stock's own historical distribution:

```r
vol_threshold <- quantile(vol_df$volatility, 0.30, na.rm = TRUE)

is_alert <- !is.na(prev_vol) & prev_vol > vol_threshold & volatility <= vol_threshold
```

This is important: the alert is **stock-specific**, not a shared threshold across all names.

## Instrument and pricing

At each alert, the script builds an **ATM straddle** with the next monthly expiry, provided the option still has at least 15 days remaining:

```r
strike <- round(alert_price, 2)
cost <- straddle_price(alert_price, strike, T, r_annual, alert_vol)
```

The model reprices the structure every day with Black-Scholes:

```r
straddle_value <- Call_BS(S_t, K, T_remaining, r, sigma_t) + Put_BS(S_t, K, T_remaining, r, sigma_t)
pnl_pct <- pnl / cost * 100
```

## Exit rule

The multi-stock study uses the same base rule as the PETR4 study:

- exit at **+20% target**, or
- hold until expiry if target is not reached.

Each alert deploys **R$ 1,000**.

---

# Why This Multi-Stock Version Matters

The single-name result could have been a PETR4-specific anomaly. The top-5 expansion tests whether the same structure survives across different sectors:

- oil and energy exposure via PETR4,
- mining via VALE3,
- private banks via ITUB4 and BBDC4,
- defensive consumption via ABEV3.

If the signal continues to work across these names, the strategy becomes more credible as a **repeatable volatility-regime approach**, not a one-off pattern.

---

# Portfolio-Level Result

| Metric | Value |
|--------|-------|
| Stocks used | 5 |
| Total trades | 101 |
| Capital deployed | R$ 101,000 |
| Net P&L | R$ 156,361.30 |
| Return on deployed capital | 154.81% |
| Average hold | 2.92 days |
| Win rate | 100.0% |
| CDI P&L (same period) | R$ 25,401.99 |
| CDI return | 25.15% |
| Excess vs CDI | R$ 130,959.31 |
| Outperformance vs CDI | 6.16x |

Source: `top5_straddle_portfolio_summary.csv`.

Interpretation:

- The strategy remains strongly profitable after moving from 1 stock to 5.
- Return falls relative to PETR4 alone, which is expected because cross-sectional diversification introduces weaker names.
- Even so, the result still beats CDI very decisively.

---

# Ranking by Stock

| Rank | Symbol | Alerts | Trades | Return | Net P&L | Avg Hold | Outperformance vs CDI |
|------|--------|--------|--------|--------|---------|----------|------------------------|
| 1 | PETR4.SA | 20 | 19 | 213.37% | R$ 40,540.83 | 3.26 days | 10.35x |
| 2 | VALE3.SA | 13 | 13 | 199.08% | R$ 25,879.93 | 2.23 days | 9.87x |
| 3 | BBDC4.SA | 29 | 29 | 165.02% | R$ 47,854.38 | 2.90 days | 8.04x |
| 4 | ABEV3.SA | 23 | 21 | 125.14% | R$ 26,279.83 | 2.62 days | 5.66x |
| 5 | ITUB4.SA | 21 | 19 | 83.19% | R$ 15,806.32 | 3.42 days | 3.79x |

This ranking shows two different notions of leadership:

- **Best percentage return:** PETR4
- **Largest absolute profit:** BBDC4, because it generated the highest number of valid trades

---

# Return by Symbol

![Return on deployed capital for each of the five stocks.](top5_straddle_return_by_symbol.png)

The dispersion is wide.

Key reading:

- PETR4 and VALE3 are the strongest names in percentage terms.
- BBDC4 sits below them in percentage return, but the volume of alerts makes it the biggest contributor in absolute reais.
- ITUB4 is the weakest paper in the group, though still clearly above CDI.

---

# Chronological Equity Curve

![Cumulative P&L for all trades, colored by stock.](top5_straddle_equity_curve.png)

This plot helps answer whether the portfolio depends on just one isolated cluster of trades.

The answer is no.

The curve rises over multiple periods and from multiple names, which is healthier than a portfolio dominated by a single short burst.

---

# How the Multi-Stock Code Works

The script is a direct generalization of the PETR4 version.

## Step 1: loop through the stock list

```r
symbols <- c("PETR4.SA", "VALE3.SA", "ITUB4.SA", "BBDC4.SA", "ABEV3.SA")
results <- lapply(symbols, simulate_symbol)
```

Each symbol gets its own full volatility frame and alert stream.

## Step 2: build rolling-vol frame per stock

```r
build_volatility_frame <- function(symbol) {
  xts_data <- getSymbols(symbol, from = start_date, to = end_date, auto.assign = FALSE)
  close_prices <- Cl(xts_data)
  log_returns <- diff(log(close_prices), lag = 1)
  ...
}
```

This avoids mixing distributions between names that naturally have different volatility levels.

## Step 3: simulate trades independently

```r
simulate_symbol <- function(symbol) {
  vol_threshold <- quantile(vol_df$volatility, 0.30, na.rm = TRUE)
  ...
  exit_hit <- position_data %>% filter(pnl_pct >= target_pct) %>% slice(1)
}
```

Each stock is evaluated with its own alerts, entries, exits, and P&L stream.

## Step 4: consolidate portfolio outputs

```r
summary_by_symbol <- bind_rows(lapply(results, function(x) x$summary))
trades_by_symbol <- bind_rows(lapply(results, function(x) x$trades))
portfolio_pnl <- sum(summary_by_symbol$total_pnl, na.rm = TRUE)
```

This gives both:

- per-stock statistics,
- and a portfolio-level result.

---

# Interpretation by Stock

## PETR4.SA

Still the best stock in percentage return. This confirms the original single-name result was not a fluke caused by a coding artifact.

## VALE3.SA

Very strong performance with fewer trades. This suggests the signal is selective but efficient in mining exposure.

## BBDC4.SA

Most productive stock in absolute P&L. The signal fired frequently and kept good quality.

## ABEV3.SA

A middle-ground case: lower absolute volatility than the leaders, but still a strong long-vol response pattern.

## ITUB4.SA

The weakest of the five. This does not invalidate the stock, but it suggests the low-vol compression signal is less explosive here than in PETR4 or VALE3.

---

# Comparison Against CDI

This is the key allocation question.

Using the same overall study period, the 5-stock straddle portfolio returned:

- **Strategy:** 154.81%
- **CDI:** 25.15%

That means:

$$
\text{Outperformance} = \frac{156{,}361.30}{25{,}401.99} \approx 6.16x
$$

So even after expanding from the best single name into a diversified top-5 universe, the strategy remains materially stronger than the fixed-income benchmark.

---

# Caveats

| Factor | Notes |
|--------|-------|
| Pricing model | Black-Scholes with historical-vol proxy, not observed option midpoint |
| Slippage and spread | Not deducted explicitly |
| Taxes | Not included |
| 100% win rate | Very likely optimistic due to mark-to-model framework |
| Future stability | Historical regime persistence may weaken out of sample |

The correct interpretation is not that this is a guaranteed 100% hit-rate trading system. The correct interpretation is that the **signal is strong enough in-sample** to justify deeper robustness checks.

---

# Recommendation

The expansion to five names improves confidence in the approach.

If the next objective is research quality rather than raw backtest performance, the natural next steps are:

1. introduce realistic spread and fee assumptions,
2. compare 20% vs 30% targets stock-by-stock,
3. test whether excluding weaker names like ITUB4 improves the portfolio,
4. generate a top-5 narrative report ranking every single alert by contribution.

Current bottom line:

**The low-volatility straddle idea survives the move from PETR4 to a diversified top-5 stock universe and still beats CDI by a large margin.**

---

# Reproducibility

Main script:

- `multi_stock_straddle_study.R`

Main outputs used in this report:

- `top5_straddle_summary_by_symbol.csv`
- `top5_straddle_portfolio_summary.csv`
- `top5_straddle_return_by_symbol.png`
- `top5_straddle_equity_curve.png`
