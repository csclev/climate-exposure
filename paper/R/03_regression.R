# 03_regression.R
# Regression models for report.Rmd
# Requires: 00_load_data.R sourced first

library(tidyverse)
library(fixest)
library(modelsummary)
library(stargazer)


# ---- Run Models ----

# Model 1: Baseline — past performance predicts future performance
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

# Model 6: Full spec — note corr(resl_score, sovi_score) = -0.53
model6 <- feols(
  auc ~ pre_trend_annual + event_count + log_damage + log_eal_valt +
    resl_score + sovi_score | month_year + stcofips,
  cluster = ~stcofips,
  data    = df
)

models <- list(
  "M1: Baseline"    = model1,
  "M2: + Severity"  = model2,
  "M3: + Exposure"  = model3,
  "M4: + Resilience"= model4,
  "M5: + Vulnerability" = model5,
  "M6: Full"        = model6
)

# ---- Coefficient plot for Model 4 (key model) ----
coef_df <- broom::tidy(model4, conf.int = TRUE) |>
  filter(term != "(Intercept)", term != "log_eal_valt") |>
  mutate(
    term = recode(term,
      "pre_trend_annual" = "Pre-Storm Trend (Annual)",
      "event_count"      = "Event Count",
      "log_damage"       = "Log Damage",
      "log_eal_valt"     = "Log Expected Annual Loss",
      "resl_score"       = "Resilience Score"
    ),
    significant = p.value < 0.05
  )

fig_coef_plot <- coef_df |>
  ggplot(aes(x = estimate, y = reorder(term, estimate), color = significant)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_point(size = 3) +
  annotate("text", x = Inf, y = -Inf, 
           label = "Note: log(EAL) excluded",
           hjust = 1.3, vjust = -0.5, size = 3, color = "grey40", fontface = "italic") +
  scale_color_manual(values = c("TRUE" = "steelblue", "FALSE" = "grey60"),
                     labels = c("TRUE" = "p < 0.05", "FALSE" = "p >= 0.05")) +
  labs(
    title    = "Coefficient Plot: Model 4 (Key Model)",
    subtitle = "Two-way FE (county + month-year). Clustered SE by county. Bars = 95% CI.",
    x        = "Estimate (USD)",
    y        = NULL,
    color    = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# ---- Regression table via modelsummary ----
dir.create("./output/tables", recursive = TRUE, showWarnings = FALSE)


etable(models,
       se       = "cluster",
       tex      = TRUE,
       file     = here::here("output/tables/regression_table.tex"),
       dict     = c(
         pre_trend_annual = "Pre-Storm Trend (Annual)",
         event_count      = "Event Count",
         log_damage       = "Log Property Damage",
         log_eal_valt     = "Log Expected Annual Loss",
         resl_score       = "Resilience Score",
         sovi_score       = "Social Vulnerability Score",
         auc              = "Area Under Curve (AUC)"
       ),
       title    = "Two-Way Fixed Effects Estimates of Post-Storm ZHVI Deviation (AUC)"
)

# Save coefficient plot
ggsave("./output/figures/fig_coef_plot.pdf", fig_coef_plot, width = 7, height = 5)

cat("Regression models estimated\n")
cat("Table saved to ./output/tables/regression_table.tex\n")
cat("Coefficient plot saved to ./output/figures/fig_coef_plot.pdf\n")

# ---- Key scalars for inline R in report.Rmd ----
resl_coef  <- coef(model4)["resl_score"]
resl_se    <- sqrt(diag(vcov(model4)))["resl_score"]
resl_pval  <- summary(model4)$coeftable["resl_score", "Pr(>|t|)"]
within_r2_m1 <- fitstat(model1, "war2")$war
within_r2_m4 <- fitstat(model4, "war2")$war
r2_gain    <- within_r2_m4 - within_r2_m1
