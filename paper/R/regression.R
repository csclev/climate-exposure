# regression.R
# Section 4.2: Regression tables
# Table 1: Model buildup (mid tier, L2)
# Table 2: Baseline sensitivity (mid tier, preferred spec, all 3 baselines)
# Table 3: Tier comparison (L2, preferred spec, all 3 tiers)

library(tidyverse)
library(fixest)
library(here)

source(here::here("paper", "R", "theme.R"))

run_regression <- function(baselines, output_dir) {
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  dict <- c(
    pre_trend_annual = "Pre-Storm Trend (Annual)",
    coastal          = "Coastal",
    log_damage       = "Log Property Damage",
    episode_count    = "Episode Count",
    log_risk_value   = "Log NRI Risk Value",
    log_eal_valt     = "Log Expected Annual Loss",
    resl_value       = "Resilience Value",
    sovi_score       = "Social Vulnerability",
    auc              = "Cumulative Impulse Response (CIR)"
  )
  
  # ---- Table 1: Model buildup (mid tier, L2 baseline) ----
  df_l2_mid <- baselines$level2$df |> filter(tier == "mid")
  
  m1 <- feols(auc ~ pre_trend_annual | month_year,
              cluster = ~stcofips + month_year, data = df_l2_mid)
  
  m2 <- feols(auc ~ pre_trend_annual + coastal + log_damage + episode_count | month_year,
              cluster = ~stcofips + month_year, data = df_l2_mid)
  
  m3 <- feols(auc ~ pre_trend_annual + coastal + log_damage + episode_count +
                log_risk_value | month_year,
              cluster = ~stcofips + month_year, data = df_l2_mid)
  
  m4 <- feols(auc ~ pre_trend_annual + coastal + log_damage + episode_count +
                log_eal_valt + resl_value + sovi_score | month_year,
              cluster = ~stcofips + month_year, data = df_l2_mid)

  
  models_buildup <- list(
    "M1: Baseline"     = m1,
    "M2: + Storm"      = m2,
    "M3: + Risk"       = m3,
    "M4: + Components" = m4
  )
  
  etable(models_buildup, tex = TRUE, dict = dict,
         file = file.path(output_dir, "table_buildup.tex"))
  
  # ---- Table 2: Baseline sensitivity (mid tier, M4 spec) ----
  baseline_models <- list()
  
  for (key in names(baselines)) {
    bl     <- baselines[[key]]
    df_mid <- bl$df |> filter(tier == "mid")
    
    baseline_models[[bl$baseline]] <- feols(
      auc ~ pre_trend_annual + coastal + log_damage + episode_count +
        log_eal_valt + resl_value + sovi_score | month_year,
      cluster = ~stcofips + month_year,
      data    = df_mid
    )
  }
  
  # Reorder: Regional, L2, L1
  baseline_models <- baseline_models[c("Regional", "Second Ring (L2)", "Adjacent (L1)")]
  
  etable(baseline_models, tex = TRUE, dict = dict,
         headers = c("Regional", "Second Ring (L2)", "Adjacent (L1)"),
         file = file.path(output_dir, "table_baseline_sensitivity.tex"))
  
  # ---- Table 3: Tier comparison (L2 baseline, M4 spec) ----
  tier_models <- list()
  
  for (t in c("bottom", "mid", "top")) {
    df_tier <- baselines$level2$df |> filter(tier == t)
    
    tier_label <- case_when(
      t == "bottom" ~ "Bottom",
      t == "mid"    ~ "Mid",
      t == "top"    ~ "Top"
    )
    
    tier_models[[tier_label]] <- feols(
      auc ~ pre_trend_annual + coastal + log_damage + episode_count +
        log_eal_valt + resl_value + sovi_score | month_year,
      cluster = ~stcofips + month_year,
      data    = df_tier
    )
  }
  
  etable(tier_models, tex = TRUE, dict = dict,
         headers = c("Bottom", "Mid", "Top"),
         file = file.path(output_dir, "table_tier_comparison.tex"))
  
  cat(sprintf("Regression tables saved to %s\n", output_dir))
  
  list(
    models_buildup    = models_buildup,
    baseline_models   = baseline_models,
    tier_models       = tier_models
  )
}