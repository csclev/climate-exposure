# 01_eda.R
# EDA figures and tables for report.Rmd
# Requires: 00_load_data.R sourced first

library(tidyverse)
library(tigris)
library(sf)
library(scales)

options(tigris_use_cache = TRUE)

df <- df |>
  mutate(climate_region_display = recode(
    climate_region,"West North Central" = "W. North Central")
    )
df |> 
  filter(climate_region == "West") |> 
  count(year)
# ---- 1. Storm events by year and climate region ----
fig_events_by_region <- df |>
  count(year, climate_region_display) |>
  ggplot(aes(x = year, y = n, fill = climate_region_display)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Storm Events by Year and Climate Region",
    x        = NULL,
    y        = "County-Month Storm Observations",
    fill     = "Climate Region"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.4, "cm")) +
  guides(fill = guide_legend(nrow = 2))

# ---- 2. Distribution of resl_score ----
fig_resl_dist <- df |>
  distinct(stcofips, resl_score) |>
  ggplot(aes(x = resl_score)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white", linewidth = 0.2) +
  geom_vline(aes(xintercept = median(resl_score)), color = "firebrick",
             linetype = "dashed", linewidth = 0.8) +
  labs(
    title    = "Distribution of Community Resilience Score (FEMA NRI)",
    subtitle = "Dashed line = median",
    x        = "Resilience Score (National Percentile)",
    y        = "County Count"
  ) +
  theme_minimal()

# ---- 3. Distribution of AUC ----
fig_auc_dist <- df |>
  ggplot(aes(x = auc / 1000)) +
  geom_histogram(bins = 80, fill = "steelblue", color = "white", linewidth = 0.2) +
  geom_vline(aes(xintercept = median(auc / 1000)), color = "firebrick",
             linetype = "dashed", linewidth = 0.8) +
  scale_x_continuous(labels = dollar_format(suffix = "K")) +
  labs(
    title    = "Distribution of Post-Storm CIR",
    subtitle = "Cumulative 12-month ZHVI deviation from regional baseline. Dashed line = median.",
    x        = "CIR (index points)",
    y        = "Event Count"
  ) +
  theme_minimal()

# ---- 4. resl_score vs sovi_score correlation ----
fig_resl_sovi <- df |>
  distinct(stcofips, resl_score, sovi_score) |>
  ggplot(aes(x = sovi_score, y = resl_score)) +
  geom_point(alpha = 0.2, size = 0.8, color = "steelblue") +
  geom_smooth(method = "lm", color = "firebrick", se = TRUE, linewidth = 0.8) +
  labs(
    title    = "Resilience Score vs. Social Vulnerability Score",
    subtitle = sprintf("Pearson r = %.2f", cor(df$resl_score, df$sovi_score, use = "complete.obs")),
    x        = "Social Vulnerability Score (National Percentile)",
    y        = "Resilience Score (National Percentile)"
  ) +
  theme_minimal()

# ---- 5. County-level storm event heatmap (CONUS) ----

# Aggregate total events per county
county_events <- df |>
  group_by(stcofips) |>
  summarise(total_events = sum(event_count), .groups = "drop") |>
  mutate(
    state_fips  = str_sub(stcofips, 1, 2),
    county_fips = str_sub(stcofips, 3, 5)
  )

# Load Census county shapefile — cached after first run
counties_sf <- counties(cb = TRUE, resolution = "20m", year = 2021) |>
  filter(!STATEFP %in% c("02", "15", "60", "66", "69", "72", "78")) |>  # drop non-CONUS
  left_join(county_events, by = c("STATEFP" = "state_fips", "COUNTYFP" = "county_fips"))

fig_storm_heatmap <- ggplot(counties_sf) +
  geom_sf(aes(fill = total_events), color = NA) +
  scale_fill_viridis_c(
    option    = "magma",
    direction = -1,
    na.value  = "grey90",
    name      = "Storm Events",
    labels    = comma,
    trans     = "log1p",
    breaks    = c(1, 5, 20, 100, 500)
  ) +
  labs(
    title = "County-Level Storm Event Exposure (2020-2025)",
    subtitle = "Total county-month storm observations. Counties in grey had no recorded events.",
    caption  = "Source: NOAA Storm Events Database"
  ) +
  theme_void() +
  theme(
    legend.position  = "bottom",
    legend.key.width = unit(2, "cm"),
    plot.title       = element_text(size = 13, face = "bold"),
    plot.subtitle    = element_text(size = 9, color = "grey40")
  )

# Correlation of resilience and social vulnerability
resl_sovi_cor <- round(cor(df$resl_score, df$sovi_score, use="complete.obs"), 2)

# ---- Save all figures ----
dir.create("./output/figures", recursive = TRUE, showWarnings = FALSE)

ggsave("./output/figures/fig_events_by_region.pdf", fig_events_by_region,
       width = 8, height = 5)
ggsave("./output/figures/fig_resl_dist.pdf",        fig_resl_dist,
       width = 7, height = 4)
ggsave("./output/figures/fig_auc_dist.pdf",         fig_auc_dist,
       width = 7, height = 4)
ggsave("./output/figures/fig_resl_sovi.pdf",        fig_resl_sovi,
       width = 6, height = 5)
ggsave("./output/figures/fig_storm_heatmap.pdf",    fig_storm_heatmap,
       width = 10, height = 6)

cat("EDA figures saved to ./output/figures/\n")
