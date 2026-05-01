# 03_regression.R
# Regression models for report.Rmd
# Requires: 00_load_data.R sourced first

library(tidyverse)
library(fixest)

# ---- Run Models ----

# M1: Baseline - pre-trend only
model1 <- feols(
  auc ~ pre_trend_annual | month_year,
  cluster = ~month_year,
  data    = df
)

# M2: Storm severity
model2 <- feols(
  auc ~ pre_trend_annual +
  log_damage | month_year,
  cluster = ~month_year,
  data    = df
)

# M3: Event Type
model3 <- feols(
  auc ~ pre_trend_annual +
    log_damage + log_risk_value + log_damage*log_risk_value | month_year,
  cluster = ~month_year,
  data    = df
)
  

# M4: + NRI Risk
model4 <- feols(
  auc ~ pre_trend_annual +
    log_damage + log_eal_valt + log_eal_valt*log_damage| month_year,
  cluster = ~month_year,
  data    = df
)

# M5: NRI components
model5 <- feols(
  auc ~ pre_trend_annual +
    log_damage + log_eal_valt + log_eal_valt*log_damage + resl_value + sovi_score | month_year,
  cluster = ~month_year,
  data    = df
)


models <- list(
  "M1: Baseline" = model1,
  "M2: Storm severity"  = model2,
  "M3: Event Type" = model3,
  "M4: NRI Risks" = model4,
  "M5: NRI components"= model5
)

etable(
  models,
  tex   = TRUE,
  file  = here::here("paper", "output", "tables","regression_table.tex"),
  dict  = c(
    pre_trend_annual       = "Pre-Storm Trend (Annual)",
    episode_count            = "Episode Count",
    log_damage             = "Log Property Damage",
    log_eal_valt           = "Log Expected Annual Loss",
    log_risk_value         = "NRI Risk Value (log)",
    resl_value             = "Resilience Score",
    sovi_score             = "Social Vulnerability",
    evt_wind               = "Wind Event",
    evt_tornado            = "Tornado Event",
    evt_hail               = "Hail Event",
    evt_flood              = "Flood Event",
    auc                    = "Cumulative Impulse Response (CIR)",
    resl_quartileLow = "Resiliency Low (Q1)",
    resl_quartileHigh = "Resiliency High (Q4)"
  ),
  title = "Two-Way Fixed Effects Estimates of Post-Storm CIR"
)


