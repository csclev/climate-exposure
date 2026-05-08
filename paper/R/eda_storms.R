# eda_storms.R
# Section 2.1: Storm Events EDA
# Produces heatmap, episodes by year/type, and summary stats

library(tidyverse)
library(tigris)
library(sf)
library(scales)
library(here)

source(here::here("paper", "R", "theme.R"))

options(tigris_use_cache = TRUE)

run_storm_eda <- function(output_dir) {
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  storms <- read_csv(here::here("data", "processed", "storm_events.csv"),
                     col_types = cols(stcofips = col_character()),
                     show_col_types = FALSE)
  
  # ---- Event type groupings ----
  storms <- storms |>
    mutate(
      evt_wind    = as.integer(str_detect(event_type, "Thunderstorm Wind|Funnel Cloud")),
      evt_tornado = as.integer(str_detect(event_type, "Tornado")),
      evt_hail    = as.integer(str_detect(event_type, "Hail")),
      evt_flood   = as.integer(str_detect(event_type, "Flash Flood|Flood|Heavy Rain")),
      evt_other   = as.integer(str_detect(event_type, "Lightning|Dust Devil|Debris Flow"))
    )
  
  # ---- 1. Storm heatmap ----
  county_events <- storms |>
    group_by(stcofips) |>
    summarise(total_events = sum(event_count), .groups = "drop") |>
    mutate(
      state_fips  = str_sub(stcofips, 1, 2),
      county_fips = str_sub(stcofips, 3, 5)
    )
  
  counties_sf <- counties(cb = TRUE, resolution = "20m", year = 2021) |>
    filter(!STATEFP %in% c("02", "15", "60", "66", "69", "72", "78")) |>
    left_join(county_events, by = c("STATEFP" = "state_fips", "COUNTYFP" = "county_fips"))
  
  fig_storm_heatmap <- ggplot(counties_sf) +
    geom_sf(aes(fill = total_events), color = NA) +
    scale_fill_viridis_c(
      option = "magma", direction = -1, na.value = "grey90",
      name = "Storm Events", labels = comma, trans = "log1p",
      breaks = c(1, 5, 20, 100, 500)
    ) +
    labs(
      title    = "County-Level Storm Event Exposure (2020-2025)",
      subtitle = "Total county-month storm observations. Grey = no recorded events.",
      caption  = "Source: NOAA Storm Events Database"
    ) +
    theme_void() +
    theme(
      legend.position  = "bottom",
      legend.key.width = unit(2, "cm"),
      plot.title       = element_text(size = 13, face = "bold"),
      plot.subtitle    = element_text(size = 9, color = "grey40")
    )
  
  # ---- 2. Episodes by year and event type ----
  fig_events_by_type <- storms |>
    pivot_longer(
      cols      = c(evt_wind, evt_tornado, evt_hail, evt_flood, evt_other),
      names_to  = "event_category",
      values_to = "flag"
    ) |>
    filter(flag == 1) |>
    mutate(event_category = recode(event_category,
                                   "evt_wind"    = "Wind",
                                   "evt_tornado" = "Tornado",
                                   "evt_hail"    = "Hail",
                                   "evt_flood"   = "Flood",
                                   "evt_other"   = "Other"
    )) |>
    count(year, event_category) |>
    ggplot(aes(x = year, y = n, fill = event_category)) +
    geom_col(position = "dodge") +
    scale_fill_brewer(palette = "Paired") +
    scale_y_continuous(labels = comma) +
    labs(
      title = "Storm Events by Year and Type",
      x     = NULL,
      y     = "County-Month Storm Observations",
      fill  = "Event Type"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # ---- 3. Summary stats ----
  summary_stats <- tibble(
    total_county_months = nrow(storms),
    total_events        = sum(storms$event_count),
    total_episodes      = sum(storms$episode_count),
    unique_counties     = n_distinct(storms$stcofips),
    year_min            = min(storms$year),
    year_max            = max(storms$year),
    total_damage        = sum(storms$total_damage, na.rm = TRUE),
    median_damage       = median(storms$total_damage[storms$total_damage > 0], na.rm = TRUE),
    pct_zero_damage     = mean(storms$total_damage == 0, na.rm = TRUE)
  )
  
  write_csv(summary_stats, file.path(output_dir, "storm_summary_stats.csv"))
  
  # ---- Save ----
  ggsave(file.path(output_dir, "fig_storm_heatmap.pdf"),
         fig_storm_heatmap, width = 10, height = 6)
  ggsave(file.path(output_dir, "fig_events_by_type.pdf"),
         fig_events_by_type, width = 8, height = 5)
  
  cat(sprintf("Storm EDA saved to %s\n", output_dir))
  cat(sprintf("  County-months: %s\n", format(summary_stats$total_county_months, big.mark = ",")))
  cat(sprintf("  Counties: %s\n", format(summary_stats$unique_counties, big.mark = ",")))
  cat(sprintf("  Total damage: $%s\n", format(summary_stats$total_damage, big.mark = ",")))
  
  list(
    fig_storm_heatmap  = fig_storm_heatmap,
    fig_events_by_type = fig_events_by_type,
    summary_stats      = summary_stats,
    storms             = storms
  )
}