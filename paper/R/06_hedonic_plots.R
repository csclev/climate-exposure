# 02_event_study_plots.R
# Event study visualizations for report.Rmd
# Requires: 00_load_data.R, 05_hedonic sourced first

library(tidyverse)
library(scales)
library(broom)
library(fixest)
library(ggplot2)
library(dplyr)
library(tigris)
library(sf)
library(binsreg)

# Coefficient Plot (horizontal forest)
# Pull coefficients and SEs from fixest object directly
coef_df <- data.frame(
  term      = names(coef(mB)),
  estimate  = coef(mB),
  std.error = se(mB)
) %>%
  mutate(
    conf.low  = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    p.value   = 2 * (1 - pnorm(abs(estimate / std.error))),
    label = case_when(
      term == "log_eal"         ~ "Expected Annual Loss (log)",
      term == "resl_value_z"    ~ "Resilience (1-SD)",
      term == "sovi_score_z"    ~ "Social Vulnerability (1-SD)",
      term == "log_income"      ~ "Median income (log)",
      term == "log_pop_density" ~ "Pop density (log)",
      term == "coastal"         ~ "Coastal county",
      TRUE                      ~ term
    ),
    sig = ifelse(p.value < 0.05, "Significant", "Not significant")
  )

coefficient_plot <- ggplot(coef_df, aes(x = estimate, y = reorder(label, estimate), color = sig)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), size = 0.6) +
  scale_color_manual(values = c("Significant" = "#1a5490", "Not significant" = "grey60")) +
  labs(
    title    = "Hedonic regression coefficients on log(ZHVI)",
    subtitle = "County-month panel, Nov 2020 – Dec 2025; state + month FE; county-clustered SEs",
    x        = "Coefficient (% change in ZHVI)",
    y        = NULL,
    color    = NULL
  ) +
  theme_minimal()

# Income Effect
get_coefs <- function(model, model_label) {
  data.frame(
    term      = names(coef(model)),
    estimate  = coef(model),
    std.error = se(model),
    model     = model_label
  ) %>%
    mutate(
      conf.low  = estimate - 1.96 * std.error,
      conf.high = estimate + 1.96 * std.error
    )
}

coef_compare <- bind_rows(
  get_coefs(mE, "Without income"),
  get_coefs(mB, "With income")
) %>%
  filter(term %in% c("log_eal", "resl_value_z", "sovi_score_z", "coastal"))

income_comparison_plot <- ggplot(coef_compare, aes(x = estimate, y = term, color = model)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high),
                  position = position_dodge(0.4), size = 0.5) +
  labs(
    title    = "Coefficients shift dramatically when income is removed",
    subtitle = "Income effects absorb much of the apparent NRI variance",
    x        = "Coefficient",
    y        = NULL,
    color    = NULL
  ) +
  theme_minimal()

# Urban/Ruralness ruled out
rucc_summary <- df %>%
  distinct(stcofips, .keep_all = TRUE) %>%
  group_by(rucc_2023) %>%
  summarise(
    Resilience           = mean(resl_value, na.rm = TRUE),
    `Median ZHVI`        = mean(zhvi, na.rm = TRUE),
    `Median Income`      = mean(median_hh_income, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(-rucc_2023, names_to = "metric", values_to = "value") %>%
  group_by(metric) %>%
  mutate(indexed = 100 * value / value[rucc_2023 == 1]) %>%
  ungroup()

urban_rural_plot <- ggplot(rucc_summary, aes(x = rucc_2023, y = indexed, color = metric)) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "grey60") +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = 1:9) +
  scale_color_manual(values = c(
    "Resilience"     = "#1a5490",
    "Median ZHVI"    = "#b2182b",
    "Median Income"  = "#4d9221"
  )) +
  labs(
    title    = "Resilience barely moves across the urban-rural continuum; income and prices halve",
    subtitle = "All series indexed to RUCC 1 (large metro) = 100",
    x        = "RUCC code  (1 = large metro, 9 = most rural)",
    y        = "Index (RUCC 1 = 100)",
    color    = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Scatter/Partial-residual plot (binned)
# Residualize ZHVI and EAL on controls + FE
df$zhvi_resid <- residuals(feols(log_zhvi ~ log_income + log_pop_density + coastal | state + month_id, data = df))
df$eal_resid  <- residuals(feols(log_eal  ~ log_income + log_pop_density + coastal | state + month_id, data = df))

partial_residual_bins_plot <- binsreg(y = df$zhvi_resid, x = df$eal_resid, nbins = 30, line = c(3, 3))

# Map of Residuals
# State Map
# Pull observations that fixest actually used (handles dropped rows)
df_used <- df[obs(mB), ]   # if this errors, df is already aligned to mB

df_used$.resid <- residuals(mB)

county_resid <- df_used %>%
  group_by(stcofips) %>%
  summarise(
    mean_resid = mean(.resid, na.rm = TRUE),
    n_obs      = n(),
    .groups    = "drop"
  ) %>%
  filter(n_obs >= 12)  # require at least a year of obs for a stable mean

# Force types match exactly
county_resid$stcofips <- as.character(trimws(county_resid$stcofips))
counties_sf$GEOID     <- as.character(trimws(counties_sf$GEOID))

map_df <- counties_sf %>%
  left_join(county_resid, by = c("GEOID" = "stcofips"))

hedonic_residual_state_map <- ggplot(map_df) +
  geom_sf(aes(fill = mean_resid), color = NA) +
  scale_fill_gradient2(
    low = "#b2182b", mid = "white", high = "#2166ac",
    midpoint = 0, limits = c(-0.5, 0.5),
    oob = scales::squish, labels = scales::percent_format(),
    na.value = "grey85", name = "Mean residual"
  ) +
  labs(
    title    = "Where the hedonic over- and under-predicts ZHVI vs State Avg",
    subtitle = "Positive (blue) = market price exceeds NRI-and-controls prediction relative to rest of State"
  ) +
  theme_void()

# National Map
mB_noFE <- feols(
  log_zhvi ~ log_eal + resl_value_z + sovi_score_z +
    log_income + log_pop_density + coastal | month_id,
  data = df, cluster = ~ stcofips
)

county_resid_noFE <- df[obs(mB_noFE), ] %>%
  mutate(.resid = residuals(mB_noFE)) %>%
  group_by(stcofips) %>%
  summarise(mean_resid = mean(.resid, na.rm = TRUE), .groups = "drop")
# Force types match exactly
county_resid$stcofips <- as.character(trimws(county_resid$stcofips))
counties_sf$GEOID     <- as.character(trimws(counties_sf$GEOID))

natl_map_df <- counties_sf %>%
  left_join(county_resid, by = c("GEOID" = "stcofips"))

hedonic_residual_national_map <- ggplot(natl_map_df) +
  geom_sf(aes(fill = mean_resid), color = NA) +
  scale_fill_gradient2(
    low = "#b2182b", mid = "white", high = "#2166ac",
    midpoint = 0, limits = c(-0.5, 0.5),
    oob = scales::squish, labels = scales::percent_format(),
    na.value = "grey85", name = "Mean residual"
  ) +
  labs(
    title    = "Where the hedonic over- and under-predicts ZHVI vs National Avg",
    subtitle = "Positive (blue) = market price exceeds NRI-and-controls prediction relative to rest of Nation"
  ) +
  theme_void()

# ---- Save figures ----
ggsave("./paper/output/figures/hedonic_coefficient_plot.pdf",     coefficient_plot,
       width = 8, height = 5)
ggsave("./paper/output/figures/hedonic_income_comparison.pdf", income_comparison_plot,
       width = 8, height = 5)
ggsave("./paper/output/figures/hedonic_urban_rural.pdf", urban_rural_plot,
       width = 10, height = 7)
ggsave("./paper/output/figures/hedonic_residual_bins.pdf", partial_residual_bins_plot$bins_plot,
       width = 8, height = 5)
ggsave("./paper/output/figures/hedonic_residual_state_map.pdf", hedonic_residual_state_map,
       width = 10, height = 7)
ggsave("./paper/output/figures/hedonic_residual_national_map.pdf", hedonic_residual_national_map,
       width = 10, height = 7)
cat("Event study figures saved to ./paper/output/figures/\n")