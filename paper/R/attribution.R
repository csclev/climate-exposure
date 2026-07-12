# attribution.R
# Market cap and storm attribution analysis by tier
# Computes per-county totals summed across the study, not median approximations

library(tidyverse)
library(tidycensus)
library(here)

source(here::here("paper", "R", "theme.R"))

run_attribution <- function(baselines, reg, output_dir) {
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # ---- Load ZHVI at T=0 per tier (one row per event) ----
  en_bottom <- read_csv(here::here("data", "processed", "nlevel2_nmin2_pre3_post9", "event_neighbors_bottom.csv"),
                        col_types = cols(target_fips = col_character()), show_col_types = FALSE)
  en_mid    <- read_csv(here::here("data", "processed", "nlevel2_nmin2_pre3_post9", "event_neighbors_mid.csv"),
                        col_types = cols(target_fips = col_character()), show_col_types = FALSE)
  en_top    <- read_csv(here::here("data", "processed", "nlevel2_nmin2_pre3_post9", "event_neighbors_top.csv"),
                        col_types = cols(target_fips = col_character()), show_col_types = FALSE)
  
  # One ZHVI per county (median across events if county appears multiple times)
  zhvi_by_county <- tibble(stcofips = en_mid$target_fips, zhvi_mid = en_mid$target_zhvi_t0) |>
    group_by(stcofips) |> summarise(zhvi_mid = median(zhvi_mid, na.rm = TRUE), .groups = "drop") |>
    left_join(
      tibble(stcofips = en_bottom$target_fips, zhvi_bottom = en_bottom$target_zhvi_t0) |>
        group_by(stcofips) |> summarise(zhvi_bottom = median(zhvi_bottom, na.rm = TRUE), .groups = "drop"),
      by = "stcofips"
    ) |>
    left_join(
      tibble(stcofips = en_top$target_fips, zhvi_top = en_top$target_zhvi_t0) |>
        group_by(stcofips) |> summarise(zhvi_top = median(zhvi_top, na.rm = TRUE), .groups = "drop"),
      by = "stcofips"
    )
  
  # ---- Housing units from ACS ----
  acs_units <- get_acs(
    geography = "county",
    variables = c(total_units = "B25001_001"),
    year = 2023, survey = "acs5", output = "wide",
    cache_table = TRUE
  ) |>
    transmute(stcofips = GEOID, total_units = total_unitsE)
  
  # ---- Per-county market cap (summed, not approximated) ----
  study_counties <- unique(baselines$level2$df |> filter(tier == "mid") |> pull(stcofips))
  county_cap <- zhvi_by_county |>
    filter(stcofips %in% study_counties) |>
    left_join(acs_units, by = "stcofips") |>
    filter(!is.na(total_units)) |>
    mutate(
      cap_bottom = coalesce(zhvi_bottom, 0) * total_units / 3,
      cap_mid    = coalesce(zhvi_mid, 0) * total_units / 3,
      cap_top    = coalesce(zhvi_top, 0) * total_units / 3,
      cap_total  = cap_bottom + cap_mid + cap_top
    )
  n_study_counties    <- nrow(county_cap)
  total_market_cap    <- sum(county_cap$cap_total, na.rm = TRUE)
  total_cap_bottom    <- sum(county_cap$cap_bottom, na.rm = TRUE)
  total_cap_mid       <- sum(county_cap$cap_mid, na.rm = TRUE)
  total_cap_top       <- sum(county_cap$cap_top, na.rm = TRUE)
  mean_county_cap     <- total_market_cap / n_study_counties
  mean_units          <- mean(county_cap$total_units, na.rm = TRUE)
  
  # ---- Per-tier regression coefficients ----
  tc <- reg$tier_models
  beta_bottom <- as.numeric(coef(tc[["Bottom"]])["pre_trend_annual"])
  beta_mid    <- as.numeric(coef(tc[["Mid"]])["pre_trend_annual"])
  beta_top    <- as.numeric(coef(tc[["Top"]])["pre_trend_annual"])
  
  # ---- Per-tier CIR and pre-trend (medians for typical county) ----
  df_l2 <- baselines$level2$df
  
  tier_stats <- tibble(
    tier = c("Bottom", "Mid", "Top"),
    median_zhvi_t0 = c(
      median(en_bottom$target_zhvi_t0, na.rm = TRUE),
      median(en_mid$target_zhvi_t0, na.rm = TRUE),
      median(en_top$target_zhvi_t0, na.rm = TRUE)
    ),
    median_cir = c(
      median(df_l2 |> filter(tier == "bottom") |> pull(auc), na.rm = TRUE),
      median(df_l2 |> filter(tier == "mid") |> pull(auc), na.rm = TRUE),
      median(df_l2 |> filter(tier == "top") |> pull(auc), na.rm = TRUE)
    ),
    median_pre_trend = c(
      median(df_l2 |> filter(tier == "bottom") |> pull(pre_trend_annual), na.rm = TRUE),
      median(df_l2 |> filter(tier == "mid") |> pull(pre_trend_annual), na.rm = TRUE),
      median(df_l2 |> filter(tier == "top") |> pull(pre_trend_annual), na.rm = TRUE)
    ),
    pre_trend_beta = c(beta_bottom, beta_mid, beta_top),
    total_tier_cap = c(total_cap_bottom, total_cap_mid, total_cap_top)
  ) |>
    mutate(
      tier = factor(tier, levels = c("Bottom", "Mid", "Top")),
      # CIR decomposition for typical county
      pre_trend_cir = pre_trend_beta * median_pre_trend,
      residual_cir  = median_cir - pre_trend_cir,
      # Dollar impact scaled to total study market
      dollar_total_cir   = total_tier_cap * (median_cir / 100),
      dollar_pre_trend   = total_tier_cap * (pre_trend_cir / 100),
      dollar_residual    = total_tier_cap * (residual_cir / 100)
    )
  
  # ---- Study-wide totals ----
  study_dollar_cir      <- sum(tier_stats$dollar_total_cir)
  study_dollar_pretrend <- sum(tier_stats$dollar_pre_trend)
  study_dollar_residual <- sum(tier_stats$dollar_residual)
  
  # ---- Chart: per-tier decomposition (study-wide dollars) ----
  fig_attribution <- tier_stats |>
    select(tier, 
           `Pre-Trend Momentum` = dollar_pre_trend, 
           `Unexplained Residual` = dollar_residual) |>
    pivot_longer(cols = -tier, names_to = "component", values_to = "dollars") |>
    mutate(
      dollars_b = dollars / 1e9,
      component = factor(component, levels = c("Pre-Trend Momentum", "Unexplained Residual"))
    ) |>
    ggplot(aes(x = tier, y = dollars_b, fill = component)) +
    geom_col(position = "stack", width = 0.6) +
    geom_hline(yintercept = 0, linewidth = 0.3) +
    scale_fill_manual(values = c("Pre-Trend Momentum" = "grey70", "Unexplained Residual" = "#e41a1c")) +
    scale_y_continuous(labels = scales::dollar_format(suffix = "B")) +
    labs(
      title    = "Estimated Market Impact Decomposition by Tier",
      subtitle = sprintf("Across %s study counties (R2), Total housing exposure: $%sT.",
                         format(n_study_counties, big.mark = ","),
                         format(round(total_market_cap / 1e12, 1))),
      x        = "Market Tier",
      y        = "Implied Market Movement",
      fill     = NULL
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  ggsave(file.path(output_dir, "fig_attribution.pdf"),
         fig_attribution, width = 7, height = 5)
  
  write_csv(tier_stats, file.path(output_dir, "attribution_summary.csv"))
  
  cat(sprintf("Attribution analysis saved to %s\n", output_dir))
  cat(sprintf("  Study counties: %s\n", format(n_study_counties, big.mark = ",")))
  cat(sprintf("  Total market cap: $%sT\n", format(round(total_market_cap / 1e12, 1))))
  cat(sprintf("  Total CIR movement: $%sB\n", format(round(study_dollar_cir / 1e9, 1))))
  cat(sprintf("  Pre-trend explained: $%sB\n", format(round(study_dollar_pretrend / 1e9, 1))))
  cat(sprintf("  Unexplained residual: $%sB\n", format(round(study_dollar_residual / 1e9, 1))))
  
  list(
    fig_attribution       = fig_attribution,
    tier_stats            = tier_stats,
    county_cap            = county_cap,
    n_study_counties      = n_study_counties,
    mean_units            = mean_units,
    total_market_cap      = total_market_cap,
    study_dollar_cir      = study_dollar_cir,
    study_dollar_pretrend = study_dollar_pretrend,
    study_dollar_residual = study_dollar_residual
  )
}