# 03_regression.R
library(tidyverse)
library(fixest)

# "M1: Baseline" 
model1 <- feols(
  auc ~ pre_trend_annual | month_year,
  cluster = ~stcofips + month_year,
  data    = df
)

# "M2: + Tier"
model2 <- feols(
  auc ~ pre_trend_annual + tier | month_year,
  cluster = ~stcofips + month_year,
  data    = df
)

# "M3: + Episode"
model3 <- feols(
  auc ~ pre_trend_annual + tier + episode_count + log_damage | month_year,
  cluster = ~stcofips + month_year,
  data    = df
)

# "M4: + NRI"
model4 <- feols(
  auc ~ pre_trend_annual + tier + episode_count + log_damage  + log_risk_value | month_year,
  cluster = ~stcofips + month_year,
  data    = df
)



models <- list(
  "M1: Baseline"       = model1,
  "M2: + Tier"         = model2,
  "M3: + Damage"       = model3,
  "M4: Damage × Tier"  = model4
)

etable(
  models,
  tex  = TRUE,
  file = here::here("paper", "output", "tables", "regression_table.tex"),
  dict = c(
    pre_trend_annual        = "Pre-Storm Trend (Annual)",
    episode_count              = "Episode Count",
    tierbottom              = "Bottom Tier",
    tiertop                 = "Top Tier",
    "episode_count:tierbottom" = "Episode Count × Bottom",
    "episode_count:tiertop"    = "Episode Count × Top",
    auc                     = "Cumulative Impulse Response (CIR)",
    log_risk_value = "NRI Risk Value (log)"
  ),
  title = "Two-Way Fixed Effects Estimates of Post-Storm CIR by Market Tier"
)