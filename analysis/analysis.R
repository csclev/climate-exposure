# ==============================================================================
# Climate Resilience
# Author: David Schaaf
# ==============================================================================
library(dplyr)
library(tidyverse)
library(sandwich)
library(lmtest)
library(fixest)


df <- read_csv("./data/processed/analysis_dataset.csv") |>
  mutate(
    month_year = factor(paste0(month, year)),
    log_eal_valt = log(eal_valt),
    log_risk_value = log(risk_value)
  )

# Model 1: Past performance of pre-storm trend
model1 <- feols(auc ~ pre_trend_annual | month_year + stcofips, data=df)
model1

# Model 2: Pre-trend + Storm metrics
model2 <- feols(auc ~ pre_trend_annual + event_count + log_damage | month_year + stcofips, data=df)
model2

# Model 3: Pre-trend + Storm metrics + Resiliency + EAL + SOVI
model3 <- feols(auc ~ pre_trend_annual + event_count + log_damage + log_eal_valt  | month_year + stcofips, data=df)
model3

# Model 4: Pre-trend + Storm metrics + Resiliency
model4 <- feols(auc ~ pre_trend_annual + event_count + log_damage + log_eal_valt + resl_score  | month_year + stcofips, data=df)
model4

# Model 5: Pre-trend + Storm metrics + Risk Score
model5 <- feols(auc ~ pre_trend_annual + event_count + log_damage + log_eal_valt + sovi_score | month_year + stcofips, data=df)
model5

