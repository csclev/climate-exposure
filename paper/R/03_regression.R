# 03_regression.R
# Regression models for report.Rmd
# Requires: 00_load_data.R sourced first

library(tidyverse)
library(fixest)

# ---- Run Models ----

# Model 1: Baseline - past performance predicts future performance
model1 <- feols(
  auc ~ pre_trend_annual | month_year + stcofips,
  cluster = ~stcofips,
  data    = df
)

# Model 2: Add storm severity controls
model2 <- feols(
  auc ~ pre_trend_annual + event_count + log_damage | month_year + stcofips,
  cluster = ~stcofips,
  data    = df
)

# Model 3: Add hazard exposure
model3 <- feols(
  auc ~ pre_trend_annual + event_count + log_damage + log_eal_valt |
    month_year + stcofips,
  cluster = ~stcofips,
  data    = df
)

# Model 4: Add resilience score (key model)
model4 <- feols(
  auc ~ pre_trend_annual + event_count + log_damage + log_eal_valt +
    resl_score | month_year + stcofips,
  cluster = ~stcofips,
  data    = df
)

# Model 5: Swap resilience for social vulnerability
model5 <- feols(
  auc ~ pre_trend_annual + event_count + log_damage + log_eal_valt +
    sovi_score | month_year + stcofips,
  cluster = ~stcofips,
  data    = df
)

# Model 6: Full spec - note corr(resl_score, sovi_score) = -0.53
model6 <- feols(
  auc ~ pre_trend_annual + event_count + log_damage + log_eal_valt +
    resl_score + sovi_score | month_year + stcofips,
  cluster = ~stcofips,
  data    = df
)

# Model 7: BRIC sub-components - decomposes composite resl_score into
# physical infrastructure vs economic vs social channels
# Note: BRIC 2020 data only, treated as time-invariant
model7 <- feols(
  auc ~ pre_trend_annual + event_count + log_damage + log_eal_valt +
    bric_housing + bric_econ + bric_social | month_year ,
  cluster = ~stcofips,
  data    = df
)


# Model 8: Heterogeneous treatment effects for resiliency
df <- df |>
  mutate(resl_zone = case_when(
    resl_score < 48  ~ "Low",
    resl_score > 62  ~ "High",
    TRUE             ~ "Middle"
  )) |>
  mutate(resl_zone = relevel(factor(resl_zone), ref = "Middle"))

model8 <- feols(
  auc ~ pre_trend_annual + event_count + log_damage + log_eal_valt +
   sovi_score + resl_zone | month_year + stcofips,
  cluster = ~stcofips,
  data    = df
)
model8


models <- list(
  #"M1: Baseline"         = model1,
  "M2: + Severity"       = model2,
  #"M3: + Storms "       = model3,
  "M4: + Resilience"     = model4,
  #"M5: + Vulnerability"  = model5,
  "M6: Full"             = model6,
  "M7: Full with HTE"    = model8
)

# ---- Coefficient plot for Model 4 (key model) ----
coef_df <- broom::tidy(model6, conf.int = TRUE) |>
  filter(term != "(Intercept)", term != "log_eal_valt") |>
  mutate(
    term = recode(term,
                  "pre_trend_annual" = "Pre-Storm Trend (Annual)",
                  "event_count"      = "Event Count",
                  "log_damage"       = "Log Property Damage",
                  "resl_score"       = "Resilience Score",
                  "sovi_score"       = "Social Vulnerability Score"
    ),
    significant = p.value < 0.05
  )

fig_coef_plot <- coef_df |>
  ggplot(aes(x = estimate, y = reorder(term, estimate), color = significant)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_point(size = 3) +
  annotate("text", x = Inf, y = -Inf,
           label = "Note: log(EAL) excluded - control variable, different scale",
           hjust = 1.05, vjust = -0.5, size = 3, color = "grey40", fontface = "italic") +
  scale_color_manual(values = c("TRUE" = "steelblue", "FALSE" = "grey60"),
                     labels = c("TRUE" = "p < 0.05", "FALSE" = "p >= 0.05")) +
  labs(
    title    = "Coefficient Plot: Model 6 (Preferred Specification)",
    subtitle = "Two-way FE (county + month-year). Clustered SE by county. Bars = 95% CI.",
    x        = "Estimate (index points)",
    y        = NULL,
    color    = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# ---- Regression table via etable ----
dir.create("./output/tables", recursive = TRUE, showWarnings = FALSE)

etable(
  models,
  vcov = ~stcofips,
  tex   = TRUE,
  file  = here::here("output/tables/regression_table.tex"),
  dict  = c(
    pre_trend_annual = "Pre-Storm Trend (Annual)",
    event_count      = "Event Count",
    log_damage       = "Log Property Damage",
    log_eal_valt     = "Log Expected Annual Loss",
    resl_score       = "Resilience Score",
    sovi_score       = "Social Vulnerability Score",
    bric_housing     = "BRIC: Housing/Infrastructure",
    bric_econ        = "BRIC: Economic Capacity",
    bric_social      = "BRIC: Social Capital",
    auc              = "Cumulative Impulse Response (CIR)",
    resl_zoneHigh    = "High Resiliency (pctl >62%)",
    resl_zoneLow     = "Low Resiliency (pctl <48%)"
  ),
  title = "Two-Way Fixed Effects Estimates of Post-Storm CIR"
)

# ---- Save coefficient plot ----
dir.create("./output/figures", recursive = TRUE, showWarnings = FALSE)
ggsave("./output/figures/fig_coef_plot.pdf", fig_coef_plot, width = 7, height = 5)

cat("Regression models estimated\n")
cat("Table saved to ./output/tables/regression_table.tex\n")
cat("Coefficient plot saved to ./output/figures/fig_coef_plot.pdf\n")

# ---- Key scalars for inline R blocks in report.Rmd ----
resl_coef    <- coef(model6)["resl_score"]
resl_se      <- sqrt(diag(vcov(model6)))["resl_score"]
resl_pval    <- summary(model6)$coeftable["resl_score", "Pr(>|t|)"]
within_r2_m1 <- fitstat(model1, "war2")$war2
within_r2_m6 <- fitstat(model6, "war2")$war2
r2_gain      <- within_r2_m6 - within_r2_m1

# BRIC component coefficients from Model 7
bric_housing_coef <- coef(model7)["bric_housing"]
bric_econ_coef    <- coef(model7)["bric_econ"]
bric_social_coef  <- coef(model7)["bric_social"]
bric_housing_pval <- summary(model7)$coeftable["bric_housing", "Pr(>|t|)"]
bric_econ_pval    <- summary(model7)$coeftable["bric_econ",    "Pr(>|t|)"]
bric_social_pval  <- summary(model7)$coeftable["bric_social",  "Pr(>|t|)"]
