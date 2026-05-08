# eda_zhvi.R
# Section 2.2: ZHVI Tier EDA
# Produces tier distributions, trajectories, and summary stats

library(tidyverse)
library(scales)
library(here)

source(here::here("paper", "R", "theme.R"))

run_zhvi_eda <- function(output_dir) {
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # ---- Load raw Zillow tier CSVs ----
  META_COLS <- c('RegionID', 'SizeRank', 'RegionName', 'RegionType',
                 'StateName', 'State', 'Metro', 'StateCodeFIPS', 'MunicipalCodeFIPS')
  
  load_tier <- function(path, tier_name) {
    raw <- read_csv(path, show_col_types = FALSE)
    date_cols <- setdiff(names(raw), META_COLS)
    
    raw |>
      filter(RegionType == "county") |>
      mutate(
        state_fips  = str_pad(as.character(StateCodeFIPS), 2, pad = "0"),
        county_fips = str_pad(as.character(MunicipalCodeFIPS), 3, pad = "0"),
        stcofips    = paste0(state_fips, county_fips)
      ) |>
      filter(!state_fips %in% c("02", "15", "72")) |>
      pivot_longer(
        cols      = all_of(date_cols),
        names_to  = "date",
        values_to = "zhvi"
      ) |>
      mutate(
        date = as.Date(date),
        tier = tier_name
      ) |>
      filter(!is.na(zhvi), date >= "2020-01-01") |>
      select(stcofips, date, zhvi, tier)
  }
  
  zhvi_mid    <- load_tier(here::here("data", "raw", "zillow_county_mid_zhvi.csv"),    "Mid")
  zhvi_top    <- load_tier(here::here("data", "raw", "zillow_county_top_zhvi.csv"),    "Top")
  zhvi_bottom <- load_tier(here::here("data", "raw", "zillow_county_bottom_zhvi.csv"), "Bottom")
  
  zhvi_all <- bind_rows(zhvi_mid, zhvi_top, zhvi_bottom) |>
    mutate(tier = factor(tier, levels = c("Bottom", "Mid", "Top")))
  
  # ---- 1. Tier distributions at a point in time ----
  snapshot_date <- as.Date("2023-01-31")
  
  fig_tier_dist <- zhvi_all |>
    filter(date == snapshot_date) |>
    ggplot(aes(x = zhvi / 1000, fill = tier)) +
    geom_density(alpha = 0.4, color = NA) +
    geom_vline(
      data = zhvi_all |>
        filter(date == snapshot_date) |>
        group_by(tier) |>
        summarise(med = median(zhvi / 1000), .groups = "drop"),
      aes(xintercept = med, color = tier),
      linetype = "dashed", linewidth = 0.8
    ) +
    scale_x_continuous(labels = dollar_format(suffix = "K")) +
    scale_color_manual(values = TIER_COLORS, labels = TIER_LABELS) +
    scale_fill_manual(values = TIER_COLORS, labels = TIER_LABELS) +
    coord_cartesian(xlim = c(0, 800)) +
    labs(
      title    = "ZHVI Distribution by Market Tier",
      subtitle = sprintf("County-level home values as of %s. Dashed = median.", format(snapshot_date, "%B %Y")),
      x        = "ZHVI ($K)",
      y        = "Density",
      fill     = "Tier",
      color    = "Tier"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # ---- 2. Tier trajectories over time ----
  fig_tier_trajectory <- zhvi_all |>
    group_by(tier, date) |>
    summarise(
      median_zhvi = median(zhvi, na.rm = TRUE),
      .groups     = "drop"
    ) |>
    ggplot(aes(x = date, y = median_zhvi / 1000, color = tier)) +
    geom_line(linewidth = 0.9) +
    scale_y_continuous(labels = dollar_format(suffix = "K")) +
    scale_color_manual(values = TIER_COLORS) +
    labs(
      title    = "National Median ZHVI by Market Tier (2020-2025)",
      subtitle = "Monthly median across all counties with ZHVI coverage.",
      x        = NULL,
      y        = "Median ZHVI ($K)",
      color    = "Tier"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # ---- 3. Summary stats by tier ----
  tier_summary <- zhvi_all |>
    filter(date == snapshot_date) |>
    group_by(tier) |>
    summarise(
      counties    = n_distinct(stcofips),
      median_zhvi = median(zhvi, na.rm = TRUE),
      mean_zhvi   = mean(zhvi, na.rm = TRUE),
      sd_zhvi     = sd(zhvi, na.rm = TRUE),
      min_zhvi    = min(zhvi, na.rm = TRUE),
      max_zhvi    = max(zhvi, na.rm = TRUE),
      .groups     = "drop"
    )
  
  # Coverage stats across full date range
  coverage_summary <- zhvi_all |>
    group_by(tier) |>
    summarise(
      counties    = n_distinct(stcofips),
      date_min    = min(date),
      date_max    = max(date),
      total_obs   = n(),
      .groups     = "drop"
    )
  
  write_csv(tier_summary, file.path(output_dir, "zhvi_tier_summary.csv"))
  write_csv(coverage_summary, file.path(output_dir, "zhvi_coverage_summary.csv"))
  
  # ---- Save ----
  ggsave(file.path(output_dir, "fig_tier_dist.pdf"),
         fig_tier_dist, width = 8, height = 5)
  ggsave(file.path(output_dir, "fig_tier_trajectory.pdf"),
         fig_tier_trajectory, width = 8, height = 5)
  
  cat(sprintf("ZHVI EDA saved to %s\n", output_dir))
  cat(sprintf("  Snapshot date: %s\n", format(snapshot_date)))
  cat("\nTier summary:\n")
  print(tier_summary)
  
  list(
    fig_tier_dist       = fig_tier_dist,
    fig_tier_trajectory = fig_tier_trajectory,
    tier_summary        = tier_summary,
    coverage_summary    = coverage_summary
  )
}