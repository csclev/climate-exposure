# 03_regression.R
# Regression models for report.Rmd
# Requires: 00_load_data.R sourced first

library(tidyverse)
library(fixest)

# ---- Run Models ----

# M1: Baseline - pre-trend only
model1 <- feols(
  auc ~ pre_trend_annual | month_year + stcofips,
  cluster = ~stcofips + month_year,
  data    = df
)

# M2: + Storm severity
model2 <- feols(
  auc ~ pre_trend_annual + event_count + log_damage | month_year + stcofips,
  cluster = ~stcofips + month_year,
  data    = df
)


# M3: + NRI components
model3 <- feols(
  auc ~ pre_trend_annual + event_count + log_damage +
    resl_score | month_year + stcofips,
  cluster = ~stcofips + month_year,
  data    = df
)

# M4: + Resilience x Damage
model4 <- feols(
  auc ~ pre_trend_annual + event_count + log_damage +
    resl_score + resl_score*log_damage | month_year + stcofips,
  cluster = ~stcofips + month_year,
  data    = df
)

# M5: + Resilience x Damage
model5 <- feols(
  auc ~ pre_trend_annual + event_count + log_damage +

    resl_quartile + log_damage:resl_quartile | month_year + stcofips,
  cluster = ~stcofips + month_year,
  data = df
)

# M6: Storm Event Type
model6 <- feols(
  auc ~ pre_trend_annual + event_count + log_damage +
  +evt_wind + evt_tornado + evt_hail + evt_flood | month_year + stcofips,
  cluster = ~stcofips + month_year,
  data    = df
)




models <- list(
  "M1: Baseline"     = model1,
  "M2: + Severity"   = model2,
  "M3: + Resilience"        = model3,
  "M4: + Resilience x Damage"= model4,
  "M5: + Quartile x Damage"= model5
)

etable(
  models,
  tex   = TRUE,
  file  = here::here("paper", "output", "tables","regression_table.tex"),
  dict  = c(
    pre_trend_annual       = "Pre-Storm Trend (Annual)",
    event_count            = "Event Count",
    log_damage             = "Log Property Damage",
    log_eal_valt           = "Log Expected Annual Loss",
    resl_score             = "Resilience Score",
    evt_wind               = "Wind Event",
    evt_tornado            = "Tornado Event",
    evt_hail               = "Hail Event",
    evt_flood              = "Flood Event",
    "resl_score:log_eal_valt" = "Resilience × Log EAL",
    auc                    = "Cumulative Impulse Response (CIR)",
    resl_quartileLow = "Resiliency Low (Q1)",
    resl_quartileHigh = "Resiliency High (Q4)"
  ),
  title = "Two-Way Fixed Effects Estimates of Post-Storm CIR"
)


